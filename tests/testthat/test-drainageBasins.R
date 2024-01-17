#Cannot be tested on GitHub or CRAN because WhiteboxTools also needs to be installed.

test_that("Basins are created", {
  skip_on_ci()
  skip_on_cran()
  unlink(list.files(paste0(tempdir(), "/drainageBasins_test"), full.names = TRUE), recursive = TRUE)
  suppressWarnings(dir.create(paste0(tempdir(), "/drainageBasins_test")))
  suppressWarnings(dir.create(paste0(tempdir(), "/drainageBasins_test/output")))
  unlink(list.files(paste0(tempdir(), "/drainageBasins_test/output"), full.names = TRUE), recursive = TRUE)
  file.copy(test_path("fixtures", "basin_test_rast.tif"), paste0(tempdir(), "/drainageBasins_test/basin_test_rast.tif"))
  res <- drainageBasins(DEM = paste0(tempdir(), "/drainageBasins_test/basin_test_rast.tif"), streams = test_path("fixtures", "water_flow.shp"), breach_dist = 500, threshold = 200, points = test_path("fixtures", "basin_pts.shp"), points_name_col = "ID", save_path = paste0(tempdir(), "/drainageBasins_test/output"), overwrite=TRUE) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
  vdiffr::expect_doppelganger("drainage basins", terra::plot(res$delineated_basins))
})
