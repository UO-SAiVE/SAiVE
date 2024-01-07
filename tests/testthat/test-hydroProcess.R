#WARNING: this will likely fail at each run since the hydro-processed DEM changes very slightly.
test_that("DEM is hydro-processed as expected", {
  rast <- terra::rast(test_path("fixtures", "basin_test_rast.tif"))
  shp <- suppressWarnings(terra::vect(test_path("fixtures", "water_flow.shp")))
  res <- hydroProcess(rast, 100, shp, 10)
  vdiffr::expect_doppelganger("correct DEM", terra::plot(res))
})
