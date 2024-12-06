#' Predict Labels for Logistic Regression
#'
#' This function predicts binary labels for each observation given the beta coefficients, predictors, and a cutoff value.
#'
#' @param beta Numeric vector of coefficients.
#' @param X Numeric matrix of predictors.
#' @param cutoff Cutoff value to classify probabilities as 0 or 1 (default is 0.5).
#' @return A numeric vector of predicted labels (0 or 1).
#' @export
#' @examples
#' n <- 100
#' X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
#' y <- rbinom(n, 1, prob = 0.5)
#' beta_est <- estimate_beta(X, y)
#' y_pred <- predictlabels(beta_est, X)
#' print(y_pred)
predictlabels <- function(beta, X, cutoff = 0.5) {
  prob <- predicted_prob(beta, X)
  return(ifelse(prob > cutoff, 1, 0))
}
