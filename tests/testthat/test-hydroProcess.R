#WARNING: this may fail regularly the hydro-processed DEM changes very slightly.
#Cannot be tested on GitHub or CRAN because WhiteboxTools also needs to be installed.

test_that("DEM is hydro-processed as expected", {
  skip_on_ci()
  skip_on_cran()
  rast <- terra::rast(test_path("fixtures/hydroProcess_createStreams_data", "basin_test_rast.tif"))
  shp <- suppressWarnings(terra::vect(test_path("fixtures/hydroProcess_createStreams_data", "water_flow.shp")))
  res <- hydroProcess(rast, 500, shp, 200) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
  expect_snapshot(res, cran=FALSE)
  vdiffr::expect_doppelganger("correct DEM", terra::plot(res))
})
