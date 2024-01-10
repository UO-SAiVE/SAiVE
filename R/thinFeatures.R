thinFeatures <- function(predictors, response) {

  tryCatch({
    VSURF.result <- VSURF::VSURF(predictors, response, RFimplem = "ranger", parallel = TRUE, ncores = parallel::detectCores() - 1, clusterType = "PSOCK") #This takes a while, get comfortable or find something else to do
    ordered.selected <- names(predictors)[VSURF.result$varselect.thres]
    ordered.importance <- VSURF.result$imp.varselect.thres
    results$VSURF_outcome <- data.frame(Feature = ordered.selected,
                                        Importance = ordered.importance)
    retained <- names(predictors)[VSURF.result$varselect.pred]
    results$VSURF_outcome$Retained <- NA
    for (i in 1:nrow(results$VSURF_outcome)){
      if (results$VSURF_outcome$Feature[i] %in% retained) {
        results$VSURF_outcome$Retained[i] <- TRUE
      } else {
        results$VSURF_outcome$Retained[i] <- FALSE
      }
    }
    predictors <- predictors[, c(1, (VSURF.result$varselect.pred) + 1)] #Subset using the final VSURF results
  })
}
