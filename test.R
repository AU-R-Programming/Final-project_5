# Testing the functions on a dataset provided on canvas
# Load the dataset - We used the Bank Marketing data set provided
data <- read.csv("~/Downloads/bank.csv", sep = ";")

# Convert target variable to binary (0 = "no", 1 = "yes")
data$y <- ifelse(data$y == "yes", 1, 0)

# Select columns
numerical_columns <- c("age", "balance", "duration", "previous")
data_subset <- data[, c(numerical_columns, "y")]

# Standardize numerical features (mean = 0, sd = 1)
X_numeric <- scale(data_subset[, numerical_columns])

# Add intercept column to the design matrix
X <- as.matrix(cbind(1, X_numeric))

# Ensure the response variable (y) is numeric
y <- as.numeric(data_subset$y)

# estimate coefficients
beta <- estimate_beta(X, y)
print("Estimated Coefficients:")
print(beta)

# calculating bootstrap confidence intervals
bootstrap_results <- bootstrap_co_int(X, y)
print("Bootstrap Confidence Intervals:")
print(bootstrap_results)

#Predicted Probablities
pred_probs <- predicted_prob(beta, X)
head(pred_probs)

#predictions
predictions <- predict(beta, X)
head(predictions)


# calculating confusion matrix and metrics
metrics <- confusion_matrix_metrics(y, predictions)
print("Confusion Matrix Metrics:")
print(metrics)



# Testing the function with simulated data
set.seed(5) # Group name
n <- 100
X <- cbind(1, matrix(rnorm(n * 2), ncol = 2))
y <- rbinom(n, 1, prob = 0.5)

# estimate beta coefficient
beta_est <- estimate_beta(X, y)

#bootstrap confidence interval
ci <- bootstrap_co_int(X, y, alpha = 0.05, no_of_bootstraps = 100)
print(ci)

pred_probs <- predicted_prob(beta_est, X)
head(pred_probs)

# predict lables
y_pred <- predict(beta_est, X)
head(y_pred)

# calculate confusion matrix and metric
metrics <- confusion_matrix_metrics(y, y_pred)
print(metrics)