#Cannot be tested on GitHub or CRAN because WhiteboxTools also needs to be installed.
test_that("correct stream network is created", {
  if (whitebox::check_whitebox_binary()){
    dem <- hydroProcess(basin_dem, 500, streams, 200) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
    res <- createStreams(dem, 100)
    vdiffr::expect_doppelganger("stream network", terra::plot(res$streams_derived))
  }
})
