#' Calculate Confusion Matrix and Metrics
#'
#' This function calculates the confusion matrix and other performance metrics for logistic regression predictions.
#'
#' @param y_true Numeric vector of true labels (0 or 1).
#' @param y_pred Numeric vector of predicted labels (0 or 1).
#' @return A list containing prevalence, accuracy, sensitivity, specificity, false discovery rate, and diagnostic odds ratio.
#' @export
#' @examples
#' n <- 100
#' X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
#' y <- rbinom(n, 1, prob = 0.5)
#' beta_est <- estimate_beta(X, y)
#' y_pred <- predict(beta_est, X)
#' metrics <- confusion_matrix_metrics(y, y_pred)
#' print(metrics)
confusion_matrix_metrics <- function(y_true, y_pred) {
  TP <- sum(y_true == 1 & y_pred == 1)
  TN <- sum(y_true == 0 & y_pred == 0)
  FP <- sum(y_true == 0 & y_pred == 1)
  FN <- sum(y_true == 1 & y_pred == 0)
  
  prevalence <- mean(y_true)
  accuracy <- (TP + TN) / length(y_true)
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  false_discovery_rate <- ifelse((FP + TP) > 0, FP / (FP + TP), NA)
  diagnostic_odds_ratio <- ifelse((TP * TN) > 0 && (FP * FN) > 0, (TP / FN) / (FP / TN), NA)
  
  return(list(
    Prevalence = prevalence,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    False_Discovery_Rate = false_discovery_rate,
    Diagnostic_Odds_Ratio = diagnostic_odds_ratio
  ))
}
