test_that("correct stream network is created", {
  rast <- terra::rast(test_path("fixtures", "filled_DEM.tif"))
  res <- createStreams(rast, 100)
  vdiffr::expect_doppelganger("stream network", terra::plot(res$streams_derived))
})
