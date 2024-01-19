#' Remove irrelevant predictor variables
#'
#' Uses [VSURF::VSURF()] to build random forests and remove irrelevant predictor variables from a data.frame containing an outcome variable and 2 or more predictor variables.
#'
#' @param data A data.frame containing a column for the outcome variable and *n* columns for predictor variables.
#' @param outcome_col The name of the outcome variable column.
#'
#' @return A list of two data.frames: the outcome of the VSURF algorithm and the data after applying the VSURF results (rows removed if applicable).,
#' @export
#'
#' @examples
#' # thinFeatures on 'permafrost' data set
#' \dontrun{
#' data(permafrost)
#' res <- thinFeatures(permafrost, "Type")
#' # Results will vary due to inherent radomness of random forests!
#' }


thinFeatures <- function(data, outcome_col) {

  if (!inherits(data, "data.frame")){
    stop("'data' can only be a data.frame.")
  }
  if (!(outcome_col %in% names(data))){
    stop("The name you specified for the outcome column is not present in the data.frame. Try again.")
  }

  set.seed(123)
  #check and install if package ranger is not installed
  rlang::check_installed("ranger", reason = "Package ranger is required to select + retain only relevant variables")

  col <- which(names(data) == outcome_col)
  if (col != 1){
    data <- data[, c(col, setdiff(seq_along(data), col))]
  }

  results <- list()
  tryCatch({
    VSURF.result <- VSURF::VSURF(data[,2:ncol(data)], data[,1], RFimplem = "ranger", parallel = TRUE, ncores = parallel::detectCores() - 1, clusterType = "PSOCK") #This takes a while, get comfortable or find something else to do
    ordered.selected <- names(data[,2:ncol(data)])[VSURF.result$varselect.thres]
    ordered.importance <- VSURF.result$imp.varselect.thres
    results$VSURF_outcome <- data.frame(Feature = ordered.selected,
                                        Importance = ordered.importance)
    retained <- names(data[,2:ncol(data)])[VSURF.result$varselect.pred]
    results$VSURF_outcome$Retained <- NA
    for (i in 1:nrow(results$VSURF_outcome)){
      if (results$VSURF_outcome$Feature[i] %in% retained) {
        results$VSURF_outcome$Retained[i] <- TRUE
      } else {
        results$VSURF_outcome$Retained[i] <- FALSE
      }
    }
    results$subset_data <- data[, c(1,(VSURF.result$varselect.pred) + 1)] #Subset using the final VSURF results
  }, warning = function(w){
    ordered.selected <- names(data[,2:ncol(data)])[VSURF.result$varselect.thres]
    ordered.importance <- VSURF.result$imp.varselect.thres
    results$VSURF_outcome <<- data.frame(Feature = ordered.selected,
                                         Importance = ordered.importance,
                                         Retained = TRUE)
    results$subset_data <<- data #Subset using the outcome of the interpretation step. Prediction step did not run since interpretation did not eliminate any variables.
    warning("Prediction step did not run since interpretation step did not eliminate any variables. Returning all variables.")
  }, error = function(e){
    stop("Failed to run VSURF algorithm.")
  })

  return(results)
}
