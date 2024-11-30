#' Predict Labels for Logistic Regression
#'
#' This function predicts binary labels for each observation given the beta coefficients, predictors, and a cutoff value.
#'
#' @param beta Numeric vector of coefficients.
#' @param X Numeric matrix of predictors.
#' @param cutoff Cutoff value to classify probabilities as 0 or 1 (default is 0.5).
#' @return A numeric vector of predicted labels (0 or 1).
#' @export
predict <- function(beta, X, cutoff = 0.5) {
  prob <- predicted_prob(beta, X)
  return(ifelse(prob > cutoff, 1, 0))
}