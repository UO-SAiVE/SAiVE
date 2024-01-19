test_that("thinFeatures works", {
  skip_on_ci()
  skip_on_cran()
  res <- thinFeatures(readRDS(test_path("fixtures/thinFeatures_data", "data.frame.rds")), "Type")
  expect_snapshot(res$VSURF_outcome[,1]) #The other columns might be different because of the random nature of random forests...
})
