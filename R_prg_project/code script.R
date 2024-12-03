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

# creating a function to estimate beta by using optimization
estimate_beta <- function(X, y) {
  #inital values for optimization obtained from least squares
  initial_beta <- solve(t(X) %*% X) %*% t(X) %*% y

  #optim function is used for minimizing negative log likelihood
  result <- optim(par = initial_beta, fn = log_likelihood, X = X, y = y, method = "BFGS")

  return(result$par)
}
### 2. bootsrap confidence intervals
#implementing a function for calculating bootstrap  confidence intervals


# function for performing bootstrap
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

 # calculating confidence intervals
  lower <- apply(bootstrap_beta, 2, function(b) quantile(b, alpha / 2))
  upper <- apply(bootstrap_beta, 2, function(b) quantile(b, 1 - alpha / 2))

  return(list(lower = lower, upper = upper))
}

### 3. confusion matrix and metrics

# creating function to predict probabilities
predicted_prob <- function(beta, X) {
  return(1 / (1 + exp(-X %*% beta)))
}

#predict labels
predict <- function(beta, X, cutoff = 0.5) {
  prob <- predicted_prob(beta, X)
  return(ifelse(prob > cutoff, 1, 0))
}

# create afunction to calculate confusion marix and metrics
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

# creating a function to preprocess data
preprocess_data <- function(data, target_var) {
  # converting target variable to numeric if its a factor
  y <- as.numeric(data[[target_var]]) - 1  # substract 1 to make it binary
  
  # converting predictors to model matrix
  X <- model.matrix(as.formula(paste(target_var, "~ .")), data = data)[, -1]
  
  return(list(X = X, y = y))
}

# function to perform logistic regression with factor
logistic_regression_with_factors <- function(data, target_var) {
  processed <- preprocess_data(data, target_var)
  X <- processed$X
  y <- processed$y
  
  # estimate coefficients
  beta <- estimate_beta(X, y)
  print("Estimated Coefficients:")
  print(beta)
  
  # calculating bootstrap confidence intervals
  bootstrap_results <- bootstrap_co_int(X, y)
  print("Bootstrap Confidence Intervals:")
  print(bootstrap_results)
  
  #predictions
  predictions <- predict(beta, X)
  
  # calculating confusion matrix and metrics
  metrics <- confusion_matrix_metrics(y, predictions)
  print("Confusion Matrix Metrics:")
  print(metrics)
}




