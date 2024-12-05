# Testing the code
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