# Predicting surfaces using random forests means that the results will likely be slightly different each time. This precludes testing on CI or CRAN.

test_that("single model spatPredict works", {
  skip_on_ci()
  skip_on_cran()
  features <- c(terra::rast(system.file("extdata/asp.tif", package = "SAiVE")), terra::rast(system.file("extdata/dem.tif", package = "SAiVE")), terra::rast(system.file("extdata/rad.tif", package = "SAiVE")), terra::rast(system.file("extdata/slp.tif", package = "SAiVE")), terra::rast(system.file("extdata/veg.tif", package = "SAiVE")))
  outcome <- terra::vect(system.file("extdata/pmfst.gpkg", package = "SAiVE"))
  outcome$Type <- as.factor(outcome$Type)
  trainControl <- caret::trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    verboseIter = FALSE,
    returnResamp = "final",
    savePredictions = "all",
    allowParallel = TRUE)

  res <- spatPredict(features, outcome, 100, trainControl, methods = "ranger", thinFeatures = FALSE, predict = TRUE, n.cores = 2)
  expect_named(res, c("training", "testing", "selected_method", "best_model", "selected_model_performance", "prediction"))
  vdiffr::expect_doppelganger("permafrost prediction single model", terra::plot(res$prediction))
})

test_that("multi-model spatPredict works", {
  skip_on_ci()
  skip_on_cran()
  features <- c(terra::rast(system.file("extdata/asp.tif", package = "SAiVE")), terra::rast(system.file("extdata/dem.tif", package = "SAiVE")), terra::rast(system.file("extdata/rad.tif", package = "SAiVE")), terra::rast(system.file("extdata/slp.tif", package = "SAiVE")), terra::rast(system.file("extdata/veg.tif", package = "SAiVE")))
  outcome <- terra::vect(system.file("extdata/pmfst.gpkg", package = "SAiVE"))
  outcome$Type <- as.factor(outcome$Type)
  trainControl <- list("ranger" = caret::trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    verboseIter = FALSE,
    returnResamp = "final",
    savePredictions = "all",
    allowParallel = TRUE),
    "Rborist" = caret::trainControl(
      method = "boot",
      number = 5,
      verboseIter = FALSE,
      returnResamp = "final",
      savePredictions = "all",
      allowParallel = TRUE)
  )

  res <- suppressWarnings(spatPredict(features, outcome, 100, trainControl, methods = c("ranger", "Rborist"), thinFeatures = FALSE, predict = TRUE, n.cores = 2))
  expect_named(res, c("training", "testing", "failed_methods", "warned_methods", "warned_methods", "error_messages", "warn_messages", "trained_models_performance", "best_model", "selected_model_performance", "prediction"))
  vdiffr::expect_doppelganger("permafrost prediction two models", terra::plot(res$prediction))
})
