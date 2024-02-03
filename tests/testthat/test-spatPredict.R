# Predicting surfaces using random forests means that the results will likely be slightly different each time. This precludes testing on CI or CRAN.

test_that("single model spatPredict works", {
  skip_on_ci()
  skip_on_cran()
  features <- c(aspect, elev, solrad, slope, veg)
  outcome <- permafrost_polygons
  outcome$Type <- as.factor(outcome$Type)
  trainControl <- caret::trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    verboseIter = FALSE,
    returnResamp = "final",
    savePredictions = "all",
    allowParallel = TRUE)

  res <- spatPredict(features, outcome, 100, trainControl, methods = "ranger", thinFeatures = FALSE, predict = TRUE)
  expect_named(res, c("training_df", "testing_df", "failed_methods", "selected_model", "selected_model_performance", "prediction"))
  vdiffr::expect_doppelganger("permafrost prediction single model", terra::plot(res$prediction))
})

test_that("multi-model spatPredict works", {
  skip_on_ci()
  skip_on_cran()
  features <- c(aspect, elev, solrad, slope, veg)
  outcome <- permafrost_polygons
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

  res <- suppressWarnings(spatPredict(features, outcome, 100, trainControl, methods = c("ranger", "Rborist"), thinFeatures = FALSE, predict = TRUE))
  expect_named(res, c("training_df", "testing_df", "failed_methods", "trained_models_performance", "selected_model", "selected_model_performance", "prediction"))
  vdiffr::expect_doppelganger("permafrost prediction two models", terra::plot(res$prediction))
})
