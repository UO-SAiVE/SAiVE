#' Predict spatial variables using machine learning
#'
#' @author Ghislain de Laplante (gdela069@uottawa.ca or ghislain.delaplante@yukon.ca)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Function to facilitate the prediction of spatial variables using machine learning, including the selection of a particular model and/or model parameters from several user-defined options. Both classification and regression is supported, though please ensure that the models passed to the parameter `methods` are suitable.
#'
#' Note that you may need to acquiesce to installing supplementary packages, depending on the model types chosen and whether or not these have been run before; this function may not be 'set and forget'.
#'
#' It is possible to specify multiple model types (the `methods` argument) as well as model-specific parameters (the `trainControl` parameter) if you wish to test multiple options and select the best one. To facilitate model type selection, refer to function [modelMatch()].
#'
#' @details
#' This function partly operates as a convenient means of passing various parameters to the [caret::train()] function, enabling the user to rapidly trial different model types and parameter sets. In addition, pre-processing of data can optionally be done using [VSURF::VSURF()] (parameter `thinFeatures`) which can decrease the time to run models by removing superfluous parameters.
#'
#' # Balancing classes in outcome (dependent) variable
#' Models can be biased if they are given significantly more points in one outcome class vs others, and best practice is to even out the number of points in each class. If extracting point values from a vector or raster object, a simple way to do that is by using the "strata" parameter if using [terra::spatSample()]. If working directly from points, [caret::downSample()] and [caret::upSample()] can be used. See [this link](https://topepo.github.io/caret/subsampling-for-class-imbalances.html) for more information.
#'
#' # Classification or regression
#' Whether this function treats your inputs as a classification or regression problem depends on the class attached to the outcome variable. A class `factor` will be treated as a classification problem while all other classes will be treated as regression problems.
#'
#' # Method for selecting the best model:
#' When specifying multiple model types in`methods`, each model type and `trainControl` pair (if `trainControl` is a list of length equal to `methods`) is run using [caret::train()]. To speed things up you can use `fastCompare` = TRUE. Models are then compared on their 'accuracy' metric as output by [caret::resamples()], and the highest-performing model is selected. If `fastCompare` is TRUE, this model is then run on the complete data set provided in `outcome`. Model statistics are returned upon function completion, which allows the user to select their own 'best performing' model based on other criteria.
#'
#' @param features Independent variables. Must be either a NAMED list of terra spatRasters or a multi-layer (stacked) spatRaster (c(rast1, rast2). All layers must all have the same cell size, alignment, extent, and crs. These rasters should include the training extent (that covered by the spatVector in `outcome`) as well as the desired extrapolation extent.
#' @param outcome Dependent variable, as a terra spatVector of points or polygons with a single attribute table column (of class integer, numeric or factor). The class of this column dictates whether the problem is approached as a classification or regression problem; see details. If specifying polygons, stratified random sampling will be done with `poly_sample` number of points per unique polygon value.
#' @param poly_sample If passing a polygon SpatVector to `outcome`, the number of points to generate from the polygons for each unique polygon value.
#' @param trainControl Parameters used to control training of the machine learning model, created with [caret::trainControl()]. Passed to the `trControl` parameter of [caret::train()]. If specifying multiple model types in `methods` you can use a single `trainControl` which will apply to all `methods`, or pass multiple variations to this argument as a list with names matching the names of `methods` (one element for each model specified in methods).
#' @param methods A string specifying one or more classification/regression model(s) to use. Passed to the `method` parameter of [caret::train()]. If specifying more than one method they will all be passed to [caret::resamples()] to compare model performance. Then, if `predict = TRUE`, the model with the highest accuracy will be selected to predict the raster surface across the exent of `features`. A different `trainControl` parameter can be used for each model in `methods`.
#' @param fastCompare If specifying multiple model types in `methods` or one model with multiple different `trainControl` objects, should the points in `outcome` be sub-sampled for the model comparison step? The selected model will be trained on the full `outcome` data set after selection. TRUE/FALSE. This only applies if `methods` is length > 3 and if `outcome` has more than 4000 rows.
#' @param thinFeatures Should random forest selection using [VSURF::VSURF()] be used in an attempt to remove irrelevant variables?
#' @param predict TRUE will apply the selected model to the full extent of `features` and return a raster saved to `save_path`.
#' @param save_path The path (folder) to which you wish to save the predicted raster. Not used unless `predict = TRUE`. If you don't want to save anywhere permanent point to the temp directory using tempdir().
#'
#' @return A list with three to five elements: the outcome of the VSURF variable selection process, details of the fitted model, model performance statistics, model performance comparison (if methods includes more than one model), and the final predicted raster (if predict = TRUE). If applicable, the predicted raster is written to disk.
#' @export
#' @examples
#' # These examples can take a while to run!
#'
#' # Single model, single trainControl
#'
#' trainControl <- caret::trainControl(
#'                 method = "repeatedcv",
#'                 number = 10, # 10-fold Cross-validation
#'                 repeats = 10, # repeated ten times
#'                 verboseIter = FALSE,
#'                 returnResamp = "final",
#'                 savePredictions = "all",
#'                 allowParallel = TRUE)
#'
#' spatPredict(c(aspect, solrad, slope), permafrost_polygons, 10000, trainControl, "ranger")
#'
#'
#' # Multiple models, multiple trainControl
#'
#' trainControl <- list("ranger" = caret::trainControl(
#'                                   method = "repeatedcv",
#'                                   number = 10,
#'                                   repeats = 10,
#'                                   verboseIter = FALSE,
#'                                   returnResamp = "final",
#'                                   savePredictions = "all",
#'                                   allowParallel = TRUE),
#'                      "Rborist" = caret::trainControl(
#'                                    method = "boot",
#'                                    number = 10,
#'                                    repeats = 10,
#'                                    verboseIter = FALSE,
#'                                    returnResamp = "final",
#'                                    savePredictions = "all",
#'                                    allowParallel = TRUE)
#'                                    )
#'
#' spatPredict(c(aspect, solrad, slope), permafrost_polygons, 10000, trainControl, c("ranger", "Rborist"))
#'

spatPredict <- function(features, outcome, poly_sample = 1000, trainControl, methods, fastCompare = TRUE, thinFeatures = TRUE, predict = FALSE, save_path = tempdir())
{

  results <- list() #This will hold model performance measures and a terra pointer to the created spatRaster

  if (predict){
    if (!dir.exists(save_path)){
      stop("The specified directory does not exist. Please create it before pointing to it.")
    }
  }

  #If multiple models and multiple trainControls passed as arguments, check that names of 'trainControl' list matches those of 'methods'
  if (length(methods) > 1){
    if (!identical(names(trainControl), names(caret::trainControl()))){ #if this is true then multiple trainControls are passed or attempted to be passed to trainControl
      if (!all(names(trainControl) %in% methods)){ #names in both need to match
        stop("It looks like you are specifying multiple model types in 'methods' along with multiple versions of trainControl, but the names of both don't match. Please review the function help and try again.")
      } else {
        multi_trainControl <- TRUE
      }
    } else { #only one trainControl despite multiple model types passed to 'methods'
      multi_trainControl <- FALSE
    }
  } else {
    if (!identical(names(trainControl), names(caret::trainControl()))){
      stop("It looks like you're specifying multiple trainControl objects under parameter trainControl, but only a single model in 'methods'. Please review your inputs.")
    } else {
      multi_trainControl <- FALSE
    }
  }

  if (is.null(names(features))){
    stop("Looks like you're giving me an unnamed list for 'features'. Names please.")
  }
  if (!inherits(outcome, "SpatVector")){
    stop("The parameter 'outcome' must be a terra SpatVector (points of polygons).")
  }
  if (ncol(as.data.frame(outcome)) != 1){
    stop("Looks like the attribute table for the outcome does not contain exactly one column. See the help file and try again.")
  }

  outcome_class <- class(as.data.frame(outcome)[,1])

  if (!(outcome_class %in% c("factor", "numeric", "integer"))){
    stop("The outcome variable should be factor, numeric, or integer class.")
  }


  if (inherits(features, "list")){
    features <- terra::rast(features) #In case they were input as a list of raster objects and not a stacked spatRaster
  } else if (!inherits(features, "SpatRaster")){
    stop("'features' must be specified as a stacked SpatRaster object (c(raster1, raster2)) or as a list of SpatRasters (list(raster1, raster2).")
  }
  crs.identical <- sapply(features, function(x) terra::same.crs(x, features[[1]]))
  ext.identical <- sapply(features, function(x) terra::compareGeom(x, features[[1]]))
  if (FALSE %in% crs.identical){
    stop("The features you specified do not have the same coordinate reference system. Please check and try again.")
  }
  if (FALSE %in% ext.identical){
    stop("The features you specified do not have the exact same extents. Please check and try again.")
  }
  outcome <- terra::project(outcome, features[[1]]) #Make sure the points have the same crs as the features.

  if (terra::geomtype(outcome) == "polygons"){
    message("Sampling the polygons...")
    col_name <- names(outcome)[1]
    outcome <- terra::spatSample(outcome, size = poly_sample, strata = col_name)
  } else if (terra::geomtype(outcome) == "points"){
    #Nothing being done with points right now
  } else {
    stop("'outcome' can only be a terra SpatVector of points or polygons.")
  }

  message("Extracting raster values for each point in 'outcome'...")
  featureValues <- terra::extract(features, outcome, ID=FALSE) #get point values as a matrix
  TrainingData <- cbind(outcome, featureValues)

  TrainingDataFrame <- as.data.frame(TrainingData) #Create data.frame of TrainingData with type as factor as VSURF can't use the terra object
  #select columns with predictor variables. VSURF selects relevant variables based on random forest classification in a three step process. Step one (thresholding) eliminates irrelevant variables from the dataset, then steps 2 and 3 further refine the selection. Variables are then assigned a measure of relative importance that can be viewed.

  # Remove irrelevant features ######
  if (thinFeatures){
    message("Running VSURF algorithm to select only relevant variables...")
    tryCatch({
      res <- thinFeatures(TrainingDataFrame, names(TrainingDataFrame)[1])
      results$VSURF_outcome <- res$VSURF_outcome
      TrainingDataFrame <- TrainingDataFrame[ , names(res$subset_data)]
    }, error = function(e){
      warning("Failed to run VSURF algorithm to thin features. Proceeding to model training step with whole data set.")
      results$VSURF_outcome <<- "Failed to run."
    })
  }

  #Split the data into a training and testing data set
  split <- as.vector(caret::createDataPartition(TrainingDataFrame[,1], p=0.8, list=FALSE))
  Training <- as.data.frame(TrainingDataFrame[split, ])
  Testing <- as.data.frame(TrainingDataFrame[-split, ])
  results$training_df <- Training
  results$testing_df <- Testing

  #Train the model(s) using parallel computing
  results$failed_methods <- character()
  cluster <- parallel::makePSOCKcluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cluster)
  if (length(methods) > 1){
    models <- list()
    if (length(methods) > 3 & nrow(Training > 4000) & fastCompare){
      Training.sub <- Training[sample(nrow(Training), 1500), ]
      Testing.sub <- Testing[sample(nrow(Testing), 500)]
      if (!identical(as.vector(unique(Training[,1]))[order(as.vector(unique(Training[,1])))], as.vector(unique(Training.sub[,1]))[order(as.vector(unique(Training.sub[,1])))])){ #Checks if every factor in Training is present in Training.sub, which is theoretically possible!
        Training.sub <- Training[sample(nrow(Training), 2500), ]
      }
      if (!identical(as.vector(unique(Testing[,1]))[order(as.vector(unique(Testing[,1])))], as.vector(unique(Testing.sub[,1]))[order(as.vector(unique(Testing.sub[,1])))])){ #Checks if every factor in Testing is present in Testing.sub, which is theoretically possible!
        Testing.sub <- Testing[sample(nrow(Testing), 1000), ]
      }
      message("Training multiple models (on down-sampled training data for speed)...")
      redo_best <- TRUE
      for (i in methods){
        tryCatch({
          message(paste0("Working on model '", i, "'"))
          models[[i]] <- caret::train(x = Training.sub[,-1,], #predictor variables
                                      y = as.factor(Training.sub[,1]), #outcome variable
                                      method = i,
                                      trControl = if (multi_trainControl) trainControl[[i]] else trainControl)
          message("Done")
        }, error = function(e){
          warning(paste0("Failed to run model ", i))
          results$failed_methods <<- c(results$failed_methods, i)
        })
      }
      results$trained_model_performance <- list()
      for (i in 1:length(models)){
        test <- stats::predict(models[[i]], newdata = Testing.sub)
        results$trained_models_performance[[names(models[i])]] <- caret::confusionMatrix(data = test, as.factor(Testing$Type))
      }
    } else {
      redo_best <- FALSE
      message("Training multiple models and finding the best one...")
      for (i in methods){
        tryCatch({
          message(paste0("Working on model '", i, "'"))
          models[[i]] <- caret::train(x = Training[,-1,], #predictor variables
                                      y = as.factor(Training[,1]), #outcome variable
                                      method = i,
                                      trControl = if (multi_trainControl) trainControl[[i]] else trainControl)
          message("Done")
        }, error = function(e){
          warning(paste0("Failed to run model ", i))
          results$failed_methods <<- c(results$failed_methods, i)
        })
      }
      results$trained_models_performance <- list()
      for (i in 1:length(models)){
        test <- stats::predict(models[[i]], newdata = Testing)
        results$trained_models_performance[[names(models[i])]] <- caret::confusionMatrix(data = test, as.factor(Testing$Type))
      }
    }

    accuracy <- numeric()
    for (i in 1:length(results$trained_models_performance)){
      accuracy[[names(models)[i]]] <- results$trained_models_performance[[i]]$overall[1]
    }
    name <- names(which(accuracy == max(accuracy)))
    model <- models[[name]]

    message(paste0("Model selection and training complete. Selected model '", name, "' based on accuracy."))

    if (redo_best){
      message("Re-training the best model on the full training data set...")
      model <- caret::train(x = Training[,-1,], #predictor variables
                   y = as.factor(Training[,1]), #outcome variable
                   method = name,
                   trControl = if (multi_trainControl) trainControl[[name]] else trainControl)
       message("Model training complete. ")
    }
  } else { #There is only a single method specified and only one trainControl
    tryCatch({
      message("Training the model...")
      model <- caret::train(x = Training[,-1,], #predictor variables
                            y = as.factor(Training[,1]), #outcome variable
                            method = methods,
                            trControl = trainControl)
      message("Model training complete. ")
    }, error = function(e){
      parallel::stopCluster(cluster)
      stop(paste0("Failed to run model ", methods, " with the dataset and parameters specified."))
    })
  }
  parallel::stopCluster(cluster) #Actual prediction using terra has its own parallel method inherent to the function, so parallel cluster is discarded here
  results$selected_model <- model
  message("Model-specific hyperparameters were adjusted automatically; refer to returned object results$selected_model to see the result.")

  #Test the selected model and save statistics
  PermTest <- stats::predict(model, newdata = Testing)
  results$selected_model_performance <- caret::confusionMatrix(data = PermTest, as.factor(Testing$Type))

  if (predict){
    features <- terra::subset(features, names(TrainingDataFrame)[-1]) #remove layers from the raster stack using the pruned TrainingData (post-VSURF, if thinFeatures was set to TRUE)
    message("Running the model on the full extent of 'features' and saving to disk...")
    results$prediction <- terra::predict(object=features, model=model, na.rm=TRUE, progress='text', filename=paste0(save_path, "/Prediction_", Sys.Date(), ".tif"), overwrite = TRUE, cores = parallel::detectCores() - 1)
  }
  return(results)
}
