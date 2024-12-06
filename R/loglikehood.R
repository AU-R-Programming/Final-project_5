#' Calculate Log-Likelihood for Logistic Regression
#'
#' This function calculates the log-likelihood for a logistic regression model given the coefficients.
#'
#' @param beta Numeric vector of coefficients.
#' @param X Numeric matrix of predictors.
#' @param y Numeric vector of binary response values (0 or 1).
#' @return Negative log-likelihood value (as optim minimizes by default).
#' @export
log_likelihood <- function(beta, X, y) {
  eta <- X %*% beta
  pr <- 1 / (1 + exp(-eta))
  epsilon <- 1e-15
  pr <- pmax(pmin(pr, 1 - epsilon), epsilon)
  log_li <- sum(y * log(pr) + (1 - y) * log(1 - pr))
  return(-log_li)
}