#' Hydro-process a DEM
#'
#' @author Ghislain de Laplante (gdela069@uottawa.ca or ghislain.delaplante@yukon.ca)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Takes a digital elevation model and prepares it for hydrological analyses, such as basin delineation. Modifies the input DEM by breaching single cell pits/depressions and then breaching remaining depressions using a least cost algorithm (where cost is a function of distance plus elevation change to the DEM).
#'
#' If a streams layer is specified, a depression will be "burned-in" to the DEM along the stream path (after converting the vector file to a raster). This is very useful when trying to delineate basins with a poor resolution DEM. You can control the depth of this depression with parameter 'burn_dist'.
#'
#' @details
#' Relies on two WhiteboxTools functions: [whitebox::wbt_fill_single_cell_pits()] and [whitebox::wbt_breach_depressions_least_cost()]. If the parameter `streams` is specified, a depression is burned into the DEM after running fill_single_cell_pits and before breaching depressions.
#'
#'
#' @param DEM The path to a digital elevation model file with .tif extension, or a terra spatRaster object.
#' @param breach_dist The max radius (in raster cells) in which to search for a path to breach depressions, passed to [whitebox::wbt_breach_depressions_least_cost()]. This value should be high to ensure all depressions are breached, keeping in mind that greater distance = greater computing time. Note that the DEM is *not* breached in order of lowest elevation to greatest, nor is it breached sequentially (order is unknown, but the raster is presumably searched in some grid pattern for depressions). This means that flow paths may need to cross multiple depressions, especially in low relief areas.
#' @param streams Optionally, the path to the polylines shapefile or geopackage file containing streams, which can be used to improve hydrological accuracy when using poor quality DEMs but decent accuracy stream networks.
#' @param burn_dist The number of units (in DEM units) to use for burning-in the stream network.
#' @param save_path An optional path in which to save the processed DEM. If left NULL will save it in the same directory as the provided DEM or, if the DEM is a terra object, return only terra objects.
#' @param force_update_wbt Whitebox Tools is by default only downloaded if it cannot be found on the computer, and no check are performed to ensure the local version is current. Set to TRUE if you know that there is a new version and you would like to use it.
#'
#' @return A hydro-processed DEM returned as a terra object and saved to disk if `save_path` is not null.
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' \donttest{
#'
#' # Running with terra objects:
#' res <- hydroProcess(DEM = elev,
#'   breach_dist = 500,
#'   streams = streams)
#'
#' terra::plot(res)
#'
#' # Running with file paths:
#' res <- hydroProcess(DEM = system.file("extdata/dem.tif", package = "SAiVE"),
#'   breach_dist = 500,
#'   streams = system.file("extdata/streams.gpkg", package = "SAiVE")
#'   )
#'
#' terra::plot(res)
#' }

hydroProcess <- function(DEM, breach_dist, streams = NULL, burn_dist = 10, save_path = NULL, force_update_wbt = FALSE)
{

  #initial checks
  rlang::check_installed("whitebox", reason = "required to use function drainageBasins") #This is here because whitebox is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
  wbtCheck(force = force_update_wbt)  #Check whitebox binaries existence and version, install if necessary or if force_update_wbt = TRUE.

  if (inherits(DEM, "SpatRaster")){
    temp_dir <- paste0(tempdir(), "/hydroProcess")
    suppressWarnings(dir.create(temp_dir))
    suppressWarnings(unlink(list.files(temp_dir, full.names=TRUE), recursive = TRUE, force = TRUE))
    terra::writeRaster(DEM, paste0(temp_dir, "/rast.tif"))
    dem_path <- paste0(temp_dir, "/rast.tif")
    directory <- temp_dir
  } else if (inherits(DEM, "character")){
    directory <- if (is.null(save_path)) dirname(DEM) else save_path
    temp_dir <- paste0(tempdir(), "/hydroProcess")
    suppressWarnings(dir.create(temp_dir))
    suppressWarnings(unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE, force = TRUE))
    dem_path <- DEM
    DEM <- terra::rast(dem_path)
  } else {
    stop("Parameter DEM must be either a terra SpatRaster or a path to a raster.")
  }

  #Fill single cell pits
  whitebox::wbt_fill_single_cell_pits(dem = dem_path,
                                      output = paste0(temp_dir, "/filled_single_cells.tif"))
  filled_single_cells <- terra::rast(paste0(temp_dir, "/filled_single_cells.tif"))

  if (!is.null(streams)){ #load streams, process to raster, and burn-in the DEM
    if (inherits(streams, "character")){
      streams <- suppressWarnings(terra::vect(streams))
    } else if (!inherits(streams, "SpatVector")){
      stop("Parameter 'streams' must be either a path to a vector file (shapefile or geopackage file) or a terra SpatVector.")
    }
    streams <- terra::project(streams, DEM)
    streams <- terra::rasterize(streams, DEM, touches = TRUE, filename = paste0(temp_dir, "/streams_rasterized.tif"), overwrite=TRUE) #Make raster stream network. Background has values NA. Write to disk to avoid memory restrictions.
    streams <- (streams/streams) * burn_dist #Make each cell value = 20 to later burn in a 20 unit depression
    streams <- terra::subst(streams, NA, 0) #replace background NAs with 0 so that it subtracts (nothing) from the DEM later; subtracting NA results in NA cells.
    message("Creating depressions in the DEM where streams should be...")
    DEM_burned <- filled_single_cells - streams #burn-in the DEM
    terra::writeRaster(DEM_burned, paste0(temp_dir, "/DEM_burned.tif"), overwrite = TRUE)
  }

  message("Breaching depressions in the DEM to ensure continuous flow paths...")
  whitebox::wbt_breach_depressions_least_cost(
    dem = if (!is.null(streams)) paste0(temp_dir, "/DEM_burned.tif") else paste0 (temp_dir, "/filled_single_cells.tif"),
    output = paste0(directory, "/FilledDEM.tif"),
    dist = breach_dist,
    fill = TRUE,
    flat_increment = 0.0001)

  filledDEM <- terra::rast(paste0(directory, "/FilledDEM.tif"))
  return(filledDEM)
}
