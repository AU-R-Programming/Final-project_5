#' Estimate Beta Coefficients for Logistic Regression
#'
#' This function estimates the beta coefficients for a logistic regression model using numerical optimization.
#'
#' @param X Numeric matrix of predictors.
#' @param y Numeric vector of binary response values (0 or 1).
#' @return A numeric vector of estimated beta coefficients.
#' @importFrom stats optim
#' @export
#' @examples
#' n <- 100
#' X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
#' y <- rbinom(n, 1, prob = 0.5)
#' beta_est <- estimate_beta(X, y)
#' print(beta_est)
estimate_beta <- function(X, y) {
  initial_beta <- solve(t(X) %*% X) %*% t(X) %*% y
  result <- optim(par = initial_beta, fn = log_likelihood, X = X, y = y, method = "BFGS")
  return(result$par)
}
