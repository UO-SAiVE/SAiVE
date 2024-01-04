#' Create stream network from DEM
#'
#' @param DEM The path to a digital elevation model file, with .tif extension.
#' @param threshold The accumulation threshold in DEM cells necessary to start defining a stream.
#' @param vector Output file specifications. NULL for no vector file, "gpkg" for a geopackage file, "shp" for a shapefile.
#' @param save_path An optional path in which to save the newly created stream network. If left NULL will save it in the same directory as the provided DEM.
#'
#' @return A raster representation of streams and, if requested, a vector representation of streams. Returned as {terra} objects and saved to disk.
#' @export
#'

createStreams <- function(DEM, threshold, vector = NULL, save_path = NULL){

  directory <- if (is.null(save_path)) dirname(DEM) else save_path
  temp_dir <- paste0(tempdir(), "/createStreams")
  suppressWarnings(dir.create(temp_dir))
  suppressWarnings(unlink(temp_dir, recursive = TRUE, force = TRUE))
  dem_path <- DEM
  DEM <- terra::rast(dem_path)

  print("Calculating a flow accumulation raster...")
  whitebox::wbt_d8_flow_accumulation(input = dem_path,
                                     output = paste0(directory, "/D8fac.tif"))
  d8fac <- terra::rast(paste0(directory, "/D8fac.tif"))

  print("Calculating a flow directions raster...")
  whitebox::wbt_d8_pointer(dem = dem_path,
                           output = paste0(directory, "/D8pointer.tif"))
  d8pntr <- terra::rast(paste0(directory, "/D8pointer.tif"))

  # Make a raster of streams only from the DEM, with a threshold (in cells) for flow accumulation
  print("Creating a raster of streams based on the flow accumulation raster...")
  whitebox::wbt_extract_streams(flow_accum = paste0(directory, "/D8fac.tif"),
                                output = paste0(directory, "/streams_derived.tif"),
                                threshold = threshold)
  streams_derived <- terra::rast(paste0(directory, "/streams_derived.tif"))

  return(list(flow_accum=d8fac, flow_dir=d8pntr, streams_derived=streams_derived))
}

