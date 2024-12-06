#' Logistic Regression Analysis Using Numerical Optimization
#'
#' @description
#' Fits a logistic regression model to a binary response, computes bootstrap confidence intervals, 
#' and provides predictions and performance metrics.
#'
#' @param data A data frame containing predictors and response.
#' @param x_vars Character vector of predictor variable names.
#' @param y_var Character string of the binary response variable name.
#' @param no_of_bootstraps Integer number of bootstrap samples for confidence intervals.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Removes rows with missing values.
#'   \item Converts character predictors to factors and creates dummy variables.
#'   \item Ensures the response is binary (0/1).
#'   \item Uses \code{optim()} for logistic regression.
#'   \item Uses a fixed \eqn{\alpha=0.05} and cutoff = 0.5.
#' }
#'
#' @return A list with:
#' \describe{
#'   \item{beta}{Estimated coefficients (including intercept).}
#'   \item{bootstrap_conf_int}{Confidence intervals for coefficients.}
#'   \item{predicted_probabilities}{Predicted probabilities.}
#'   \item{predictions}{Predicted classes (0/1).}
#'   \item{confusion_matrix_metrics}{Performance metrics and confusion matrix.}
#' }
#'
#' @examples
#' n <- 100
#' X_mat <- matrix(rnorm(n * 2), ncol = 2)  
#'y <- rbinom(n, 1, prob = 0.5) 
#'
#'data <- data.frame(y = y,x1 = X_mat[, 1],x2 = X_mat[, 2])
#'x_vars <- c("x1", "x2")
#'y_var <- "y"

#' results <- logistic_regression_analysis(data = data,x_vars = x_vars,y_var = y_var,no_of_bootstraps = 20)
#' @export
logistic_regression_analysis <- function(data, x_vars, y_var, no_of_bootstraps) {
  # Internal fixed parameters
  alpha <- 0.05
  cutoff <- 0.5
  
  # Extract predictors and response from the data
  X <- data[, x_vars, drop = FALSE]
  y <- data[[y_var]]
  
  # Data Preprocessing
  
  ## Handle missing values
  data_combined <- data.frame(X, y)
  data_combined <- na.omit(data_combined)
  
  # Separate predictors and response after cleaning
  X <- data_combined[, x_vars, drop = FALSE]
  y <- data_combined$y
  
  ## Convert character variables to factors
  X <- as.data.frame(X)  # Ensure X is a data frame
  char_vars <- sapply(X, is.character)
  X[char_vars] <- lapply(X[char_vars], as.factor)
  
  ## Ensure y is binary
  if (is.factor(y) || is.character(y)) {
    y <- as.factor(y)
    if (length(levels(y)) != 2) {
      stop("The response variable 'y' should be binary with two levels.")
    }
    y <- as.numeric(y) - 1
  } else if (is.numeric(y)) {
    if (!all(y %in% c(0, 1))) {
      stop("The numeric response variable 'y' should contain only 0 and 1.")
    }
  } else {
    stop("The response variable 'y' should be numeric or factor.")
  }
  
  # Convert predictors to a design matrix (including intercept)
  X_design <- model.matrix(~ ., data = X)
  
  
  # Estimate coefficients using numerical optimization
  beta <- estimate_beta(X_design, y)
  
  # Assign names to beta coefficients
  names(beta) <- colnames(X_design)
  
  # Display estimated coefficients
  cat("Estimated Coefficients:\n")
  print(beta)
  
  # Calculate bootstrap confidence intervals
  cat("\nBootstrap Confidence Intervals:\n")
  bootstrap_results <- bootstrap_co_int(X_design, y, alpha = alpha, no_of_bootstraps = no_of_bootstraps)
  print(bootstrap_results)
  
  # Compute predicted probabilities
  pred_probs <- predicted_prob(beta, X_design)
  cat("\nPredicted Probabilities (first 6):\n")
  print(head(pred_probs))
  
  # Generate predictions based on cutoff
  predictions <- predictlabels(beta, X_design, cutoff = cutoff)
  cat("\nPredictions (first 6):\n")
  print(head(predictions))
  
  # Calculate confusion matrix and metrics
  metrics <- confusion_matrix_metrics(y, predictions)
  cat("\nConfusion Matrix Metrics:\n")
  print(metrics)
  
  # Return results as a list
  return(list(
    beta = beta,
    bootstrap_conf_int = bootstrap_results,
    predicted_probabilities = pred_probs,
    predictions = predictions,
    confusion_matrix_metrics = metrics
  ))
}
