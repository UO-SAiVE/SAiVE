test_that("thinFeatures works", {
  skip_on_ci()
  skip_on_cran()
  data(permafrost)
  res <- thinFeatures(permafrost, "Type")
  expect_snapshot(res$VSURF_outcome[,1]) #The other columns might be different because of the random nature of random forests...
})
