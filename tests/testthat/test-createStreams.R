#Cannot be tested on GitHub or CRAN because WhiteboxTools also needs to be installed.

# Note about package data usage: refering simply to the object ('dem', 'streams') doesn't work if the package isn't built yet, but pointing right to the file does.
test_that("correct stream network is created", {
  if (whitebox::check_whitebox_binary()){
    dem <- hydroProcess(system.file("extdata/basin_rast.tif", package = "SAiVE"), 500, system.file("extdata/streams.gpkg", package = "SAiVE"), 200, n.cores = 2) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
    res <- createStreams(dem, 100)
    vdiffr::expect_doppelganger("stream network", terra::plot(res$streams_derived))
  }
})
