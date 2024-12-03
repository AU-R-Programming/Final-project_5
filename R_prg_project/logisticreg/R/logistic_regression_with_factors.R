#' Perform Logistic Regression with Mixed Data Types
#'
#' This function performs logistic regression on a dataset containing both numerical and categorical
#' variables. It preprocesses the data, estimates coefficients, calculates bootstrap confidence
#' intervals, makes predictions, and provides confusion matrix metrics.
#'
#' @param data A data frame containing the dataset with both numeric and categorical variables.
#' @param target_var A string specifying the name of the target (dependent) variable.
#'
#' @return Prints the following outputs:
#' \item{Estimated Coefficients}{The logistic regression coefficients.}
#' \item{Bootstrap Confidence Intervals}{Confidence intervals for the coefficients calculated using bootstrapping.}
#' \item{Confusion Matrix Metrics}{Performance metrics including accuracy, sensitivity, and specificity.}
#'
#' @examples
#' data <- data.frame(
#'   age = c(25, 32, 47),
#'   income = c(50000, 60000, 120000),
#'   gender = factor(c("male", "female", "female")),
#'   target = c(1, 0, 1)
#' )
#' logistic_regression_with_factors(data, "target")
#'
#' @export
logistic_regression_with_factors <- function(data, target_var) {
  processed <- preprocess_data(data, target_var)
  X <- processed$X
  y <- processed$y
  
  # Estimate coefficients
  beta <- estimate_beta(X, y)
  print("Estimated Coefficients:")
  print(beta)
  
  # Bootstrap confidence intervals
  bootstrap_results <- bootstrap_co_int(X, y)
  print("Bootstrap Confidence Intervals:")
  print(bootstrap_results)
  
  # Predictions
  predictions <- predict(beta, X)
  
  # Confusion matrix and metrics
  metrics <- confusion_matrix_metrics(y, predictions)
  print("Confusion Matrix Metrics:")
  print(metrics)
}
