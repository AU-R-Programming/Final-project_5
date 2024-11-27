### LOGISTIC REGRESSION ESTIMATOR USING NUMERICAL OPTIMIZATION.
# Lets start with implementation of logistic regression using optimization.
#using r optim() function to perform numerical optimization

# creating function to calculate loglikelihood
log_likelihood <- function(beta, X, y) {
  pr <- 1 / (1 + exp(-X %*% beta))
  log_li <- sum(y * log(pr) + (1 - y) * log(1 - pr))
  #using negate because optim performs minimization
  return(-log_li)
}


#' @title LS through optimization
#'
#' @description XXX
#' @param y A \code{vector} of response.
#' @param X A \code{matrix} of predictors.
#' @return A \code{list} containing the following attributes:
#' \describe{XX}
#' @author Shibani,Ifesinachi,Nikhil
#' @importFrom stats runif
#' @export
#' @examples
#' set.seed(50)
n <- 100
X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
y <- rbinom(n, 1, prob = 0.5)
# creating a function to estimate beta by using optimization
estimate_beta <- function(X, y) {
  #inital values for optimization obtained from least squares
  initial_beta <- solve(t(X) %*% X) %*% t(X) %*% y

  #optim function is used for minimizing negative log likelihood
  result <- optim(par = initial_beta, fn = log_likelihood, X = X, y = y, method = "BFGS")

  return(result$par)
}
