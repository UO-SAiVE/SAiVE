#' Find machine learning models for use in caret
#'
#' @author Ghislain de Laplante (gdela069@uottawa.ca or ghislain.delaplante@yukon.ca)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' As of 2023-06-15, there are 238 different machine learning models which can be used with the CARET package. As evaluating model performance is time consuming, selecting a subset of models to test prior to deciding on which model to use is essential. This function aims to facilitate this process by matching models according to their Jaccard similarity, in a process inspired by [this section](https://topepo.github.io/caret/models-clustered-by-tag-similarity.html) in the CARET e-book. Model data is fetched from [here](https://topepo.github.io/caret/tag_data.csv). The result of this function can then be passed to [spatPredict()] to further refine model selection.
#'
#' @details
#' This function requires internet access to get an up-to-date list of models.
#'
#' @param model The abbreviation or short name of the model you'd like to match, taken from [here](https://topepo.github.io/caret/available-models.html).
#' @param type The type of model. You can match the input `model` type with "match", or select from dual-purpose models ("dual"), regression models only ("regression"), or classification models only ("classification").
#' @param similarity The similarity threshold to use as a numeric value from 0 to 1. Models with a similarity score greater than this will be returned.
#'
#' @return A data.frame of models meeting the requested similarity threshold along with the model abbreviations that can be passed to [caret::train()] or to function [spatPredict()].
#' @export
#' @examples
#'
#' # Find models similar to 'ranger'
#' modelMatch("ranger")
#'
#' # Find only models with a similarity > 0.8 to 'ranger'
#' modelMatch("ranger", similarity = 0.8)
#'

modelMatch <- function(model, type = "match", similarity = 0.7)
{

  tag_data <- utils::read.csv("https://topepo.github.io/caret/tag_data.csv", row.names = 1)
  model_names <- data.frame(names = rownames(tag_data),
                            abbreviations = gsub(".*\\((.*)\\).*", "\\1", rownames(tag_data)))
  if (!(model %in% model_names$abbreviations)) {
    stop("The model abbreviation does not appear to be in the list of models I've got. See the list of models at https://topepo.github.io/caret/tag_data.csv.")
  }
  model_match <- model_names[model_names$abbreviations == model , ]$names
  model_match <- tag_data[rownames(tag_data) == model_match ,]

  if (tolower(type) == "match") {
    type <- if (model_match$Classification == 1 & model_match$Regression == 1) "Dual" else if (model_match$Classification == 1) "Classification" else if (model_match$Regression == 1) "Regression"
  }

  if (tolower(type) == "classification") {
    tag_data <- tag_data[tag_data$Classification == 1 , ]
  } else if (tolower(type) == "regression") {
    tag_data <- tag_data[tag_data$Regression == 1 , ]
  } else if (tolower(type) == "dual") {
    tag_data <- tag_data[tag_data$Regression == 1 & tag_data$Classification == 1 , ]
  } else {
    stop("Options for parameter 'type' are 'Regression', 'Classification', or 'Dual'.")
  }

  Dist <- proxy::dist(tag_data, method = "Jaccard")
  Dm <- as.matrix(Dist)
  sim <- 1 - Dm #convet the distance to a similarity
  sim <- as.data.frame(sim)
  sim <- sim[rownames(model_match)]
  sim$model <- rownames(sim)
  selected <- sim[sim[[1]] > similarity,]
  names(selected)[1] <- paste0("Similarity to ", names(selected)[1])
  selected <- selected[1]
  selected$`Model Abbreviation` <- gsub(".*\\((.*)\\).*", "\\1", rownames(selected))

  return(selected)
}

