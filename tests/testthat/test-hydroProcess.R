#WARNING: this may fail regularly as the hydro-processed DEM changes very slightly.

test_that("DEM is hydro-processed as expected", {
  if (whitebox::check_whitebox_binary()){
    res <- hydroProcess(basin_dem, 500, streams, 200) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
    expect_snapshot(res, cran=FALSE)
    vdiffr::expect_doppelganger("correct DEM", terra::plot(res))
  }
})
