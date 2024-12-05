
#' Bootstrap Confidence Intervals for Coefficients
#'
#' This function performs bootstrapping to calculate confidence intervals for the logistic regression coefficients.
#'
#' @param X Numeric matrix of predictors.
#' @param y Numeric vector of binary response values (0 or 1).
#' @param alpha Significance level for confidence intervals (default is 0.05).
#' @param no_of_bootstraps Number of bootstrap iterations (default is 20).

#' @return A list containing the lower and upper confidence intervals for each coefficient.
#' @importFrom stats quantile
#' @export
#' @examples
#' n <- 100
#' X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
#' y <- rbinom(n, 1, prob = 0.5)
#' ci <- bootstrap_co_int(X, y, alpha = 0.05, no_of_bootstraps = 20)
#' print(ci)
bootstrap_co_int <- function(X, y, alpha = 0.05, no_of_bootstraps = 20) {
  n <- nrow(X)
  pr <- ncol(X)
  bootstrap_beta <- matrix(0, nrow = no_of_bootstraps, ncol = pr)

  for (i in 1:no_of_bootstraps) {
    indices <- sample(1:n, size = n, replace = TRUE)
    X_boot <- X[indices, ]
    y_boot <- y[indices]
    bootstrap_beta[i, ] <- estimate_beta(X_boot, y_boot)
  }

  lower <- apply(bootstrap_beta, 2, function(b) quantile(b, alpha / 2))
  upper <- apply(bootstrap_beta, 2, function(b) quantile(b, 1 - alpha / 2))

  return(list(lower = lower, upper = upper))
}
