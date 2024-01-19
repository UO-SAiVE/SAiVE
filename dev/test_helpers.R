#Helpers for testing spatPredict
# dem <- rast(test_path("fixtures/spatPredict_data", "dem.tif"))
# curv <- rast(test_path("fixtures/spatPredict_data", "curv.tif"))
# asp <- rast(test_path("fixtures/spatPredict_data", "asp.tif"))
# veg <- rast(test_path("fixtures/spatPredict_data", "veg.tif"))
# rad <- rast(test_path("fixtures/spatPredict_data", "rad.tif"))
# slp <- rast(test_path("fixtures/spatPredict_data", "slp.tif"))
#
# features <- c(dem, curv, asp, veg, rad, slp)
# outcome <- vect(test_path("fixtures/spatPredict_data", "pmfst.shp"))[,2]
# outcome$Type <- as.factor(outcome$Type)
# trainControl <- caret::trainControl(
#   method = "repeatedcv",
#   number = 10, # 10-fold Cross-validation
#   repeats = 10, # repeated ten times
#   verboseIter=FALSE ,
#   returnResamp="final",
#   savePredictions="all",
#   allowParallel=TRUE)
# methods <- "ranger"
# fastCompare = TRUE
# thinFeatures <- TRUE
# predict <- FALSE
# save_path <- tempdir()
