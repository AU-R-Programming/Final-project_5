#' Preprocess Data for Logistic Regression
#'
#' Converts a dataset containing both numerical and categorical variables into a format suitable
#' for logistic regression. Factors are converted into dummy variables, and the target variable
#' is transformed into binary numeric values.
#'
#' @param data A data frame containing the dataset with both numeric and categorical variables.
#' @param target_var A string specifying the name of the target (dependent) variable.
#'
#' @return A list with the following elements:
#' \item{X}{A matrix of predictors (dummy variables for factors).}
#' \item{y}{A binary numeric vector for the target variable (0 or 1).}
#'
#' @examples
#' data <- data.frame(
#'   age = c(25, 32, 47),
#'   income = c(50000, 60000, 120000),
#'   gender = factor(c("male", "female", "female")),
#'   target = c(1, 0, 1)
#' )
#' preprocess_data(data, "target")
#'
#' @export
preprocess_data <- function(data, target_var) {
  y <- as.numeric(data[[target_var]]) - 1  # Subtract 1 to make it binary (0 or 1)
  X <- model.matrix(as.formula(paste(target_var, "~ .")), data = data)[, -1]
  return(list(X = X, y = y))
}
