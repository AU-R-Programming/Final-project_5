#' Predict Probabilities for Logistic Regression
#'
#' This function predicts the probabilities for each observation given the beta coefficients and predictor matrix.
#'
#' @param beta Numeric vector of coefficients.
#' @param X Numeric matrix of predictors.
#' @return A numeric vector of predicted probabilities.
#' @export
predicted_prob <- function(beta, X) {
  return(1 / (1 + exp(-X %*% beta)))
}
