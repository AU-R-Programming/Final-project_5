#Testing the functions on a dataset provided on canvas
# Load the dataset - We used the Bank Marketing data set provided
data <- read.csv("~/Downloads/bank.csv", sep = ";")

x_vars <- c("age",  "marital", "education", "default", "balance","duration", "previous")  # Example predictors
y_var <- "y"  # Response variable

results <- logistic_regression_analysis(
  data = data,
  x_vars = x_vars,
  y_var = y_var,
  no_of_bootstraps = 20  
)
