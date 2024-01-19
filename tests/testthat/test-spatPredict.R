

test_that("single model spatPredict works", {
  skip_on_ci()
  skip_on_cran()
  asp <- terra::rast(test_path("fixtures/spatPredict_data", "asp.tif"))
  curv <- terra::rast(test_path("fixtures/spatPredict_data", "curv.tif"))
  dem <- terra::rast(test_path("fixtures/spatPredict_data", "dem.tif"))
  rad <- terra::rast(test_path("fixtures/spatPredict_data", "rad.tif"))
  slp <- terra::rast(test_path("fixtures/spatPredict_data", "slp.tif"))
  veg <- terra::rast(test_path("fixtures/spatPredict_data", "veg.tif"))
  features <- c(asp, curv, dem, rad, slp, veg)
  outcome <- terra::vect(test_path("fixtures/spatPredict_data", "pmfst.shp"))
  outcome <- outcome[ ,2]
  outcome$Type <- as.factor(outcome$Type)
  trainControl <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    verboseIter = FALSE,
    returnResamp = "final",
    savePredictions = "all",
    allowParallel = TRUE)

  res <- spatPredict(features, outcome, 100, trainControl, methods = "ranger", thinFeatures = FALSE, predict = FALSE)
  expect_named(res, c("training_df", "testing_df", "failed_methods", "selected_model", "selected_model_performance"))
})

test_that("multi-model spatPredict works", {
  skip_on_ci()
  skip_on_cran()
  asp <- terra::rast(test_path("fixtures/spatPredict_data", "asp.tif"))
  curv <- terra::rast(test_path("fixtures/spatPredict_data", "curv.tif"))
  dem <- terra::rast(test_path("fixtures/spatPredict_data", "dem.tif"))
  rad <- terra::rast(test_path("fixtures/spatPredict_data", "rad.tif"))
  slp <- terra::rast(test_path("fixtures/spatPredict_data", "slp.tif"))
  veg <- terra::rast(test_path("fixtures/spatPredict_data", "veg.tif"))
  features <- c(asp, curv, dem, rad, slp, veg)
  outcome <- terra::vect(test_path("fixtures/spatPredict_data", "pmfst.shp"))
  outcome <- outcome[ ,2]
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

  res <- spatPredict(features, outcome, 100, trainControl, methods = c("ranger", "Rborist"), thinFeatures = FALSE)
})
