#WARNING: this may fail regularly as the hydro-processed DEM changes very slightly.

# Note about package data usage: refering simply to the object ('dem', 'streams') doesn't work if the package isn't built yet, but pointing right to the file does.

test_that("DEM is hydro-processed as expected", {
  if (whitebox::check_whitebox_binary()) {
    res <- hydroProcess(system.file("extdata/basin_rast.tif", package = "SAiVE"), 500, system.file("extdata/streams.gpkg", package = "SAiVE"), 200, n.cores = 2) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
    expect_snapshot(res, cran=FALSE)
    vdiffr::expect_doppelganger("correct DEM", terra::plot(res))
  }
})
