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
#' Method for selecting the best model:
#' TO BE FILLED IN
#'
#' @param features Independent variables. Must be either a NAMED list of terra spatRasters or a multi-layer (stacked) spatRaster. All layers must all have the same cell size, alignment, extent, and crs. These rasters can include the training extent as well as the desired extrapolation extent.
#' @param outcome Dependent variable, as a terra spatVector of points with a single attribute table column (of class integer, numeric or factor). The class of this column dictates whether the problem is approached as a classification or regression problem.
#' @param trainControl Parameters used to control training of the machine learning model, created with [caret::trainControl()]. Passed to the `trControl` parameter of [caret::train()]. If specifying multiple model types in `methods` you can use a single `trainControl` which will apply to all `methods`, or pass multiple variations to this argument as a list with names matching the names of `methods` (one element for each model specified in methods).
#' @param methods A string specifying one or more classification/regression model(s) to use. Passed to the `method` parameter of [caret::train()]. If specifying more than one method they will all be passed to [caret::resamples()] to compare model performance. Then, if `predict = TRUE`, the model with the highest accuracy will be selected to predict the raster surface across the exent of `features`. A different `trainControl` parameter can be used for each model in `methods`.
#' @param fastCompare If specifying multiple model types in `methods` or one model with multiple different `trainControl` objects, should the points in `outcome` be sub-sampled for the model comparison step? The selected model will be trained on the full `outcome` data set after selection. TRUE/FALSE.
#' @param thinFeatures Should random forest selection using [VSURF::VSURF()] be used in an attempt to remove irrelevant variables?
#' @param predict TRUE will apply the selected model to the full extent of `features` and return a raster saved to `save_path`.
#' @param save_path The path (folder) to which you wish to save the predicted raster. Not used unless `predict = TRUE`. If you don't want to save anywhere permanent point to the temp directory using tempdir().
#'
#' @return A list with three to five elements: the outcome of the VSURF variable selection process, details of the fitted model, model performance statistics, model performance comparison (if methods includes more than one model), and the final predicted raster (if predict = TRUE). If applicable, the predicted raster is written to disk.
#' @export
#' @examples
#' \dontrun{
#'
#' }
#'

spatPredict <- function(features, outcome, trainControl, methods, fastCompare = TRUE, thinFeatures = TRUE, predict = FALSE, save_path = tempdir())
{

  results <- list() #This will hold model performance measures and a terra pointer to the created spatRaster
  set.seed(123) #set seed so that results will be reproducible later
  if (predict){
    if (!dir.exists(save_path)){
      stop("The specified directory does not exist. Please create it before pointing to it.")
    }
  }

  if (is.null(names(features))){
    stop("Looks like you're giving me an unnamed list. Names please.")
  }
  if (ncol(as.data.frame(outcome)) != 1){
    stop("Looks like the attribute table for the outcome does not contain exactly one column. See the help file and try again.")
  }
  if (!(class(outcome[[1]]) %in% c("factor", "numeric", "integer"))){
    stop("The outcome variable should be a factor, numeric, or integer.")
  }

  features <- terra::rast(features) #In case they were input as a list of raster objects and not a stacked spatRaster
  crs.identical <- sapply(features, function(x) terra::same.crs(x, features[[1]]))
  ext.identical <- sapply(features, function(x) terra::compareGeom(x, features[[1]]))
  if (FALSE %in% crs.identical){
    stop("The features you specified do not have the same coordinate reference system. Please check and try again.")
  }
  if (FALSE %in% ext.identical){
    stop("The features you specified do not have the exact same extents. Please check and try again.")
  }
  outcome <- terra::project(outcome, features[[1]]) #Make sure the points have the same crs as the features.

  featureValues <- terra::extract(features, outcome, ID=FALSE) #get point values as a matrix
  TrainingData <- cbind(outcome, featureValues)

  #If multiple models and multiple trainControls passed as arguments, check that names of 'trainControl' list matches those of 'methods'
  if (length(methods) > 1){
    if (!identical(names(trainControl), names(caret::trainControl()))){ #if this is true then multiple trainControls are passed or attempted to be passed to trainControl
      if (names(trainControl) != methods){ #names in both need to match
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

  # Remove irrelevant features ######
  if (thinFeatures){
    message("Running VSURF algorithm to select only relevant variables...")
    TrainingDataFrame <- as.data.frame(TrainingData) #Create data.frame of TrainingData with type as factor as VSURF can't use the spdf object
    #select columns with predictor variables. VSURF selects relevant variables based on random forest classification in a three step process. Step one (thresholding) eliminates irrelevant variables from the dataset, then steps 2 and 3 further refine the selection. Variables are then assigned a measure of relative importance that can be viewed.
    set.seed(123)
    #check and install if package ranger is not installed
    rlang::check_installed("ranger", reason = "Package ranger is required to select + retain only relevant variables")
    tryCatch({
      VSURF.result <- VSURF::VSURF(TrainingDataFrame[,2:ncol(TrainingDataFrame)], TrainingDataFrame[,1], RFimplem = "ranger", parallel = TRUE, ncores = parallel::detectCores() - 1, clusterType = "PSOCK") #This takes a while, get comfortable or find something else to do
      ordered.selected <- names(TrainingDataFrame[,2:ncol(TrainingDataFrame)])[VSURF.result$varselect.thres]
      ordered.importance <- VSURF.result$imp.varselect.thres
      results$VSURF_outcome <- data.frame(Feature = ordered.selected,
                                          Importance = ordered.importance)
      retained <- names(TrainingDataFrame[,2:ncol(TrainingDataFrame)])[VSURF.result$varselect.pred]
      results$VSURF_outcome$Retained <- NA
      for (i in 1:nrow(results$VSURF_outcome)){
        if (results$VSURF_outcome$Feature[i] %in% retained) {
          results$VSURF_outcome$Retained[i] <- TRUE
        } else {
          results$VSURF_outcome$Retained[i] <- FALSE
        }
      }
      TrainingData <- TrainingData[, c(1,(VSURF.result$varselect.pred) + 1)] #Subset using the final VSURF results
    }, warning = function(w){
      ordered.selected <- names(TrainingDataFrame[,2:ncol(TrainingDataFrame)])[VSURF.result$varselect.thres]
      ordered.importance <- VSURF.result$imp.varselect.thres
      results$VSURF_outcome <<- data.frame(Feature = ordered.selected,
                                           Importance = ordered.importance,
                                           Retained = TRUE)
      TrainingData <<- TrainingData[, c(1,(VSURF.result$varselect.interp) + 1)] #Subset using the outcome of the interpretation step. Prediction step did not run since interpretation did not eliminate any variables.
    }, error = function(e){
      warning("Failed to run VSURF algorithm to thin features. Proceeding to model training step with whole data set.")
    })
  }

  #Split the data into a training and testing data set
  split <- as.vector(caret::createDataPartition(TrainingData$Type, p=0.8, list=FALSE))
  Training <- as.data.frame(TrainingData[split, ])
  Testing <- as.data.frame(TrainingData[-split, ])
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
      if (!identical(as.vector(unique(Training[,1]))[order(as.vector(unique(Training[,1])))], as.vector(unique(Training.sub[,1]))[order(as.vector(unique(Training.sub[,1])))])){ #Checks if every factor in Training is present in Training.sub, which is theoretically possible!
        Training.sub <- Training[sample(nrow(Training), 3000), ]
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
    }
    resamples <- caret::resamples(models)
    results$models_perform_summary <- summary(resamples)
    results$models_perform_boxplot <- lattice::bwplot(resamples)
    performance <- resamples$values[, grep("Accuracy", names(resamples$values))]#Find and use the best performing model
    performance <- colMeans(performance)
    model <- names(performance[which(performance == max(performance))])
    model <- sub("~Accuracy", "", model)
    name <- model
    model <- models[[model]]
    message(paste0("Model selection and training complete. Selected model ", name, " based on accuracy."))
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
      stop(paste0("Failed to run model ", methods," with the dataset and parameters specified."))
      parallel::stopCluster(cluster)
    })
  }
  parallel::stopCluster(cluster) #Actual prediction using terra has its own parallel method inherent to the function, so parallel cluster is discarded here
  results$selected_model <- model
  message("Model-specific hyperparameters were adjusted automatically; refer to returned object results$selected_model to see the result.")

  #Test the selected model and print statistics
  PermTest <- stats::predict(model, newdata = Testing)
  TestStats <- caret::confusionMatrix(data = PermTest, as.factor(Testing$Type))
  results$selected_model_performance <- TestStats

  if (predict){
    features <- terra::subset(features, names(TrainingData)[-1]) #remove layers from the raster stack using the pruned TrainingData (post-VSURF, if thinFeatures was set to TRUE)
    message("Running the model and saving to disk...")
    results$prediction <- terra::predict(object=features, model=model, na.rm=TRUE, progress='text', filename=paste0(save_path, "/Prediction_", Sys.Date(), ".tif"), overwrite = TRUE, cores = parallel::detectCores() - 1)
  }

  return(results)
}
