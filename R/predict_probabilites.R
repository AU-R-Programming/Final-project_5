#' Predict Probabilities for Logistic Regression
#'
#' This function predicts the probabilities for each observation given the beta coefficients and predictor matrix.
#'
#' @param beta Numeric vector of coefficients.
#' @param X Numeric matrix of predictors.
#' @return A numeric vector of predicted probabilities.
#' @export
#' @examples
#' n <- 100
#' X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
#' y <- rbinom(n, 1, prob = 0.5)
#' beta_est <- estimate_beta(X, y)
#' pred_probs <- predicted_prob(beta_est, X)
#' print(pred_probs)
predicted_prob <- function(beta, X) {
  return(1 / (1 + exp(-X %*% beta)))
}
