#Cannot be tested on GitHub or CRAN because WhiteboxTools also needs to be installed.
test_that("correct stream network is created", {
  skip_on_ci()
  skip_on_cran()
  rast <- terra::rast(test_path("fixtures", "filled_DEM.tif"))
  res <- createStreams(rast, 100)
  vdiffr::expect_doppelganger("stream network", terra::plot(res$streams_derived))
})
