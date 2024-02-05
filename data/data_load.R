# Storing and loading package spatial data is tricky! For spatial data, the sf or terra package is required but loading internal data can't be done with a specific package (loading a .rda file doesn't allow that). Requiring a package to be loaded just to load internal data doesn't make much sense. Instead, delayedAssign below waits until the object is called and then uses the code to load the file as a terra object.

delayedAssign("permafrost_polygons", local({
  try(
    terra::vect(
      system.file("extdata/pmfst.gpkg", package = "SAiVE")  #This lives in the /inst folder, but because of how /inst works the path does not include /inst.
    ),
    silent = TRUE
  )
}))

delayedAssign("basin_pts", local({
  try(
    terra::vect(
      system.file("extdata/basin_pts.gpkg", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("streams", local({
  try(
    terra::vect(
      system.file("extdata/streams.gpkg", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("aspect", local({
  try(
    terra::rast(
      system.file("extdata/asp.tif", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("basin_dem", local({
  try(
    terra::rast(
      system.file("extdata/basin_rast.tif", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("elev", local({
  try(
    terra::rast(
      system.file("extdata/dem.tif", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("solrad", local({
  try(
    terra::rast(
      system.file("extdata/rad.tif", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("slope", local({
  try(
    terra::rast(
      system.file("extdata/slp.tif", package = "SAiVE")
    ),
    silent = TRUE
  )
}))

delayedAssign("veg", local({
  try(
    terra::rast(
      system.file("extdata/veg.tif", package = "SAiVE")
    ),
    silent = TRUE
  )
}))
