#Cannot be tested on GitHub or CRAN because WhiteboxTools also needs to be installed.
test_that("Basins are created", {
  if (whitebox::check_whitebox_binary()){
    unlink(list.files(paste0(tempdir(), "/drainageBasins_test"), full.names = TRUE), recursive = TRUE)
    unlink(list.files(paste0(tempdir(), "/drainageBasins_test/output"), full.names = TRUE), recursive = TRUE)
    suppressWarnings(dir.create(paste0(tempdir(), "/drainageBasins_test")))
    suppressWarnings(dir.create(paste0(tempdir(), "/drainageBasins_test/output")))
    file.copy(system.file("extdata/basin_rast.tif", package = "SAiVE"), paste0(tempdir(), "/drainageBasins_test/basin_test_rast.tif"))

    res <- drainageBasins(DEM = paste0(tempdir(), "/drainageBasins_test/basin_test_rast.tif"), streams = system.file("extdata/streams.gpkg", package = "SAiVE"), breach_dist = 500, threshold = 200, points = system.file("extdata/basin_pts.gpkg", package = "SAiVE"), points_name_col = "ID", save_path = paste0(tempdir(), "/drainageBasins_test/output"), overwrite=TRUE) #These are relatively high values for the size of the DEM, but necessary for reproducibility.
    vdiffr::expect_doppelganger("drainage basins", terra::plot(res$delineated_basins))
  }

})
