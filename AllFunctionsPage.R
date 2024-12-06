### LOGISTIC REGRESSION ESTIMATOR USING NUMERICAL OPTIMIZATION.
# Lets start with implementation of logistic regression using optimization.
#using r optim() function to perform numerical optimization

# creating function to calculate loglikelihood
log_likelihood <- function(beta, X, y) {
  eta <- X %*% beta
  pr <- 1 / (1 + exp(-eta))
  epsilon <- 1e-15
  pr <- pmax(pmin(pr, 1 - epsilon), epsilon)
  log_li <- sum(y * log(pr) + (1 - y) * log(1 - pr))
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
### 2. bootstrap confidence intervals
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
predictlabels <- function(beta, X, cutoff = 0.5) {
  prob <- predicted_prob(beta, X)
  return(ifelse(prob > cutoff, 1, 0))
}

# create a function to calculate confusion marix and metrics
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

#Logistic regression analysis wrapper function
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
  predictions <- predict(beta, X_design, cutoff = cutoff)
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



