#' Hydro-process a DEM
#'
#' @description
#' Takes a digital elevation model and prepares it for hydrological analyses, such as basin delineation. Modifies the input DEM by breaching single cell pits/depressions and then breaching remaining depressions using a least cost algorithm (where cost is a function of distance plus elevation change to the DEM).
#'
#' If a streams layer is specified, a depression will be "burned-in" to the DEM along the stream path (after converting the vector file to a raster). This is very useful when trying to delineate basins with a poor resolution DEM. You can control the depth of this depression with parameter 'burn_dist'.
#'
#' @param DEM The path to a DEM including extension from which to delineate watersheds/catchments. Must be in .tif format. Derived layers such as flow accumulation, flow direction, and streams will inherit the DEM coordinate reference system.
#' @param breach_dist The max radius (in raster cells) in which to search for a path to breach depressions, passed to [whitebox::wbt_breach_depressions_least_cost()]. This value should be high to ensure all depressions are breached, keeping in mind that greater distance = greater computing time. Note that the DEM is *not* breached in order of lowest elevation to greatest, nor is it breached sequentially (order is unknown, but the raster is presumably searched in some grid pattern for depressions). This means that flow paths may need to cross multiple depressions, especially in low relief areas.
#' @param streams Optionally, the path to the polylines shapefile or geopackage file containing streams, which can be used to improve hydrological accuracy when using poor quality DEMs and decent accuracy stream networks.
#' @param save_path An optional path in which to save the processed DEM. If left NULL will save it in the same directory as the provided DEM.
#'
#' @return A hydro-processed DEM as a {terra} object and a .tif file saved to disk.
#' @export


hydroProcess <- function(DEM, breach_dist, streams = NULL, burn_dist = 10, save_path = NULL)
  {

  directory <- if (is.null(save_path)) dirname(DEM) else save_path
  temp_dir <- paste0(tempdir(), "/hyroProcess")
  suppressWarningsdir.create(paste0(tempdir(), "/hyroProcess"))
  suppressWarnings(unlink(paste0(tempdir(), "/hydroProcess"), recursive = TRUE, force = TRUE))
  dem_path <- DEM
  DEM <- terra::rast(dem_path)

  #Fill single cell pits
  whitebox::wbt_fill_single_cell_pits(dem = dem_path,
                                      output = paste0(temp_dir, "/filled_single_cells.tif"))
  filled_single_cells <- terra::rast(paste0(temp_dir, "/filled_single_cells.tif"))

  if (!is.null(streams)){ #load streams, process to raster, and burn-in the DEM
    print("Creating a stream raster from the provided stream file...")
    streams_input <- terra::vect(streams)
    streams_input <- terra::project(streams_input, DEM)
    streams_input <- terra::rasterize(streams_input, DEM, touches = TRUE, filename = paste0(temp_dir, "/streams_input_rasterized.tif"), overwrite=TRUE) #Make raster stream network. Background has values NA. Write to disk to avoid memory restrictions.
    streams_input <- (streams_input/streams_input) * 20 #Make each cell value = 20 to later burn in a 20 unit depression
    streams_input <- terra::subst(streams_input, NA, 0) #replace background NAs with 0 so that it subtracts (nothing) from the DEM later; subtracting NA results in NA cells.
    print("Creating depressions in the DEM where streams should be...")
    DEM_burned <- filled_single_cells - streams_input #burn-in the DEM
    terra::writeRaster(DEM_burned, paste0(temp_dir, "/DEM_burned.tif"), overwrite = TRUE)
  }

  print("Breaching depressions in the DEM to ensure continuous flow paths...")
  whitebox::wbt_breach_depressions_least_cost(
    dem = if (!is.null(streams)) paste0(temp_dir, "/DEM_burned.tif") else paste0 (temp_dir, "/filled_single_cells.tif"),
    output = paste0(directory, "/FilledDEM.tif"),
    dist = breach_dist,
    fill = TRUE,
    flat_increment = 0.0001)

  filledDEM <- terra::rast(paste0(directory, "/DEM_burned.tif"))
  return(filledDEM)
}
