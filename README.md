# Logisticreg Package
  
---
title: "logisticreg"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{logisticreg}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Introduction

Logistic regression is a widely used statistical method for modeling binary outcomes based on one or more predictor variables. The `logisticreg` package provides a streamlined and robust framework for performing logistic regression using numerical optimization. This package is designed to help users estimate model parameters, quantify uncertainty through bootstrap confidence intervals, and evaluate model performance using comprehensive metrics.

### Key Features of `logisticreg`

- **Parameter Estimation**: Uses numerical optimization (via the `optim` function) to estimate logistic regression coefficients efficiently.

- **Bootstrap Confidence Intervals**: Provides a non-parametric approach to assess the uncertainty of estimated coefficients.

- **Prediction**: Includes functions for predicting probabilities and binary outcomes based on fitted models.

- **Performance Evaluation**: Offers tools to calculate confusion matrices and key performance metrics, such as accuracy, sensitivity, specificity, and diagnostic odds ratios.

### Why Use `logisticreg`?

- **Customizable Workflows**: Users can easily integrate their own data and apply the package to various binary classification tasks.

- **Lightweight and Flexible**: Built for R users who need a quick and effective solution for logistic regression without relying on large external dependencies.

- **Educational Value**: Ideal for understanding logistic regression concepts and applying them to real-world datasets.

This vignette demonstrates the step-by-step use of the `logisticreg` package, from estimating coefficients to evaluating model performance, using a dataset.

### How to Use the Pacakge and Functions

This vignette provides:

1. A **workflow example** showing the usage of the package functions on a dataset.

2. Explanations of the key functions included in the package.

3. Insights into interpreting the outputs and evaluating model performance.

By the end of this vignette, you’ll have a comprehensive understanding of how to:

1. Fit a logistic regression model.

2. Interpret the results.

3. Evaluate the model's predictive accuracy and reliability.

## Installation

To get started with the `logisticreg` package, you first need to install it from its GitHub repository. The installation process requires the `devtools` package, which allows you to install packages directly from GitHub. If you do not already have `devtools` installed, you can install it easily using the following command:

```
install.packages("devtools")
```


Once `devtools` is installed, you can proceed to install the `logisticreg` package. Simply run the following command in your R console:

```
devtools::install_github("AU-R-Programming/Final-project_5")
```


After successfully installing the `logisticreg` package, the next step is to load it into your R environment. Loading the package ensures that all its functions and features are available for use in your session. To do this, use the `library()` function as shown below:

```
library(logisticreg)
```

This command activates the package, making its functions accessible so you can begin your analysis. Once the package is loaded, you can start exploring its capabilities, such as fitting logistic regression models, generating predictions, evaluating performance metrics, and more. Loading the package is a crucial step before running any of its functionalities.


## Internal Workings of `logistic_regression_analysis()` Which is the Main Wrapper Function

**Description:**
`logistic_regression_analysis()` orchestrates the entire logistic regression process:
data cleaning, model fitting, bootstrap-based inference, prediction, and performance evaluation. The user only needs to provide the data, the predictor names, the response variable name, and the number of bootstrap samples.

**Step-by-Step Process:**
1. **Parameter Initialization:**
   - Internally sets the significance level (\eqn{\alpha = 0.05}) and classification cutoff (0.5).
   
2. **Data Extraction and Preprocessing:**
   - Subsets the provided data frame to the specified predictors (`x_vars`) and response (`y_var`).
   - Removes rows with missing values using `na.omit()` to ensure a clean dataset for modeling.
   - Converts character predictors into factors, ensuring categorical variables are appropriately handled.
   - Validates and converts the response variable into a binary (0/1) numeric format.

3. **Design Matrix Creation:**
   - Calls `model.matrix()` to construct a design matrix that includes an intercept and dummy variables for categorical predictors.
   
4. **Coefficient Estimation:**
   - Invokes `estimate_beta()` to fit the logistic model by numerically optimizing the log-likelihood function.
   - Assigns meaningful column names to the estimated coefficients.

5. **Bootstrap Confidence Intervals:**
   - Uses `bootstrap_co_int()` to draw multiple bootstrap samples, refit the model, and compute empirical confidence intervals for each coefficient.

6. **Predictions and Probabilities:**
   - Computes predicted probabilities for each observation using `predicted_prob()`.
   - Converts these probabilities into binary class predictions (0/1) based on the fixed cutoff (0.5).

7. **Model Performance Evaluation:**
   - Calls `confusion_matrix_metrics()` to produce a confusion matrix and performance metrics (accuracy, sensitivity, specificity, etc.).
   - Prints key results (coefficients, confidence intervals, predictions, metrics) to the console for immediate inspection.

8. **Return Value:**
   - Returns a list containing:
     - Estimated coefficients
     - Bootstrap confidence intervals
     - Predicted probabilities
     - Predicted classes
     - Confusion matrix metrics
     
**In Summary:**
The function takes care of all the heavy lifting: from cleaning data and transforming inputs into a suitable design matrix, to fitting the model, quantifying uncertainty with bootstrapping, predicting outcomes, and finally assessing how well the model performed.



### **Example Workflow**

We will demonstrate the functionality of the package with an example workflow using the Bank dataset provided.

#### Information about dataset

The data is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed. The classification goal is to predict if the client will subscribe a term deposit (variable y).

You can access the data from the [UC Irvine Machine Learning Repository](https://archive.ics.uci.edu/dataset/222/bank+marketing)


**Load the dataset, you can change the path to where you have the data**

```
data <- read.csv("~/Downloads/bank.csv", sep = ";")
```


Select the desired columns/predictors you wish to use and create `x_vars` and `y_var`

* ***age***: clients age
* ***marital***: marital status
* ***education***: education Level
* ***default***: has credit in default?
* ***balance***: average yearly balance
* ***duration***: last contact duration, in seconds (numeric).
* ***previous***: number of contacts performed before this campaign and for this client

```
x_vars <- c("age",  "marital", "education", "default", "balance","duration", "previous")  # Example predictors
y_var <- "y"  # Response variable
```


Call the `logistic_regression_analysis` function, setting the arguments `x_vars`, `y_var` and `no_of_bootstraps`.

* data: A data frame containing the predictors and the response variable.
* x_vars: A character vector specifying the names of the predictor variables in data.These can be both numeric and categorical variables. Categorical variables will be internally converted into dummy variables.
* y_var: A character scalar indicating the name of the binary response variable in data.The response should be coded as 0/1 or as a factor with two levels.
* no_of_bootstraps: An integer specifying the number of bootstrap samples to use for estimating the confidence intervals of the coefficients.

```

results <- logistic_regression_analysis(
  data = data,
  x_vars = x_vars,
  y_var = y_var,
  no_of_bootstraps = 20  
)
```
We got the output below
```
Estimated Coefficients:
                            [,1]
(Intercept)        -3.9861824509
age                 0.0231161059
maritalmarried     -0.6953138026
maritalsingle      -0.3026683861
educationsecondary  0.5488431414
educationtertiary   1.1881491302
educationunknown    1.1227889068
defaultyes         -1.4411324852
balance            -0.0004205648
duration            0.0037823035
previous            0.1566433251
attr(,"names")
 [1] "(Intercept)"        "age"                "maritalmarried"     "maritalsingle"      "educationsecondary"
 [6] "educationtertiary"  "educationunknown"   "defaultyes"         "balance"            "duration"          
[11] "previous"          

Bootstrap Confidence Intervals:
$lower
 [1] -7.5744829420  0.0159053832 -0.8884010069 -0.6229484469 -0.2025349751  0.4097801020 -0.7458008780 -2.6589661629
 [9] -0.0005747595  0.0035375024  0.1131326647

$upper
 [1] -3.5861304447  0.0661107185  0.1768237163  1.2436650904  0.9692055472  1.6645150323  1.3628661729  0.4583972579
 [9] -0.0001234456  0.0046602936  0.2327465873


Predicted Probabilities (first 6):
        [,1]
1 0.01164966
2 0.01935563
3 0.11888505
4 0.06489191
5 0.12853189
6 0.16762340

Predictions (first 6):
  [,1]
1    0
2    0
3    0
4    0
5    0
6    0

Confusion Matrix Metrics:
$Prevalence
[1] 0.11524

$Accuracy
[1] 0.8863083

$Sensitivity
[1] 0.2341651

$Specificity
[1] 0.97125

$False_Discovery_Rate
[1] 0.4852321

$Diagnostic_Odds_Ratio
[1] 10.32952

```

## Interpretation of Results

**Model Coefficients:**
- The intercept is negative and large in magnitude, indicating a low baseline probability of a positive outcome when all predictors are at their reference levels.
- Positive coefficients (e.g., for `educationtertiary`) suggest that having that characteristic increases the likelihood of a positive outcome.
- Negative coefficients (e.g., for `defaultyes`) indicate that having that trait decreases the likelihood of a positive outcome.

**Bootstrap Confidence Intervals:**
- The provided intervals show the range of plausible values for each coefficient.
- Many intervals do not include zero, suggesting that those predictors have a meaningful association with the outcome.

**Predicted Probabilities and Predictions:**
- The first six predicted probabilities are all relatively low (<0.17), leading to predicted class 0 for these observations.
- This suggests the model is conservative about predicting positive outcomes.

**Performance Metrics:**

- **Prevalence (~0.115):** About 11.5% of observations are positive in the dataset. 
- **Accuracy (~0.886):** The model correctly classifies nearly 89% of observations overall. 
- **Sensitivity (~0.234):** It correctly identifies about 23% of actual positives, indicating it often misses positives. 
- **Specificity (~0.971):** It correctly identifies about 97% of actual negatives, showing strong performance in ruling out negatives. 
- **False Discovery Rate (~0.485):** Almost half of the predicted positives are actually false positives. 
- **Diagnostic Odds Ratio (10.33):** The model has moderate ability to distinguish between positive and negative cases. 



## Documentation of Helper Functions

Below is a description of the main helper functions included in the package. These functions work together to perform logistic regression estimation using numerical optimization, bootstrap confidence intervals, and model performance evaluation.

### 1. `log_likelihood(beta, X, y)`
**Description:**  
Calculates the negative log-likelihood for a given set of parameters (`beta`), predictors (`X`), and binary response (`y`). This function is internally used by the optimization routine to find the parameter values that minimize the negative log-likelihood.

**Key Details:**  
- `beta`: Coefficient vector for the logistic regression model.  
- `X`: Design matrix of predictors (including intercept if applicable).  
- `y`: Binary response variable (0/1).  
- Ensures numerical stability by clipping probabilities away from 0 and 1 with a small `epsilon`.

### 2. `estimate_beta(X, y)`
**Description:**  
Estimates the logistic regression coefficients by minimizing the negative log-likelihood using the `optim()` function. It starts from an initial guess derived from a least-squares approximation and iteratively updates the parameters.

**Key Details:**  
- `X`: Design matrix of predictors.  
- `y`: Binary response variable.  
- Returns a vector of estimated coefficients that best fit the data under a logistic model.

### 3. `bootstrap_co_int(X, y, alpha = 0.05, no_of_bootstraps = 20)`
**Description:**  
Computes bootstrap confidence intervals for the estimated coefficients. It repeatedly samples the data with replacement, re-estimates the coefficients, and then derives empirical percentile-based confidence intervals.

**Key Details:**  
- `X`: Design matrix of predictors.  
- `y`: Binary response variable.  
- `alpha`: Significance level (default 0.05).  
- `no_of_bootstraps`: Number of bootstrap samples (default 20).  
- Returns a list with `lower` and `upper` confidence limits for each coefficient.

### 4. `predicted_prob(beta, X)`
**Description:**  
Generates the predicted probabilities of the positive class (1) for each observation given the current coefficient estimates.

**Key Details:**  
- `beta`: Coefficient vector.  
- `X`: Design matrix.  
- Returns a vector of probabilities (values between 0 and 1).

### 5. `predictlabels(beta, X, cutoff = 0.5)`
**Description:**  
Converts predicted probabilities into class predictions (0 or 1) based on a specified cutoff. Observations with predicted probabilities above the cutoff are classified as 1; otherwise, 0.

**Key Details:**  
- `beta`: Coefficient vector.  
- `X`: Design matrix.  
- `cutoff`: Classification threshold (default 0.5).  
- Returns a vector of predicted classes (0/1).

### 6. `confusion_matrix_metrics(y_true, y_pred)`
**Description:**  
Calculates various performance metrics derived from the confusion matrix. This includes accuracy, sensitivity, specificity, false discovery rate, and diagnostic odds ratio, among others.

**Key Details:**  
- `y_true`: True binary labels.  
- `y_pred`: Predicted binary labels.  
- Returns a list of performance metrics and summary statistics.

---

**How These Functions Work Together:**
- `log_likelihood()` and `estimate_beta()` handle the core parameter estimation for logistic regression.
- `bootstrap_co_int()` refines the understanding of parameter uncertainty through resampling.
- `predicted_prob()` and `predictlabels()` translate model parameters into interpretable predictions.
- `confusion_matrix_metrics()` provides a snapshot of how well the model’s predictions align with the actual outcomes, guiding model evaluation and comparison.

You will not need to call these helper functions directly if you use the main wrapper function (ie, `logistic_regression_analysis()`). However, understanding these components can help you diagnose issues, customize analyses, or extend the functionality.

**The `logisticreg` package simplifies the process of logistic regression modeling and evaluation.**


## Logistic Regression Shiny App

This Shiny app provides an interactive interface for performing logistic regression analysis. Upload your dataset, select the response and predictor variables, and view the results instantly.

To launch the app, simply run the following command in your R console:

```
run_logisticreg_app()
```


## Github Repository
Thank you for exploring the logisticreg package! This package aims to simplify logistic regression modeling and provide robust tools for data analysis. For detailed documentation, examples, and the source code, please visit the [GitHub repository](https://github.com/AU-R-Programming/Final-project_5) Your feedback and contributions are always welcome!



### Dedicated Website 
You can also find everything in this document on our dedicated website, where it’s conveniently organized and easy to navigate. [Visit our website for a seamless experience and additional resources!](https://au-r-programming.github.io/Final-project_5/)


### References
We would like to acknowledge the contributions of generative language models, which played a role in enhancing the clarity and quality of this project and documentation. These tools helped in refining our explanations and improving the overall presentation of the package's features. You can find our conversations [here](https://chatgpt.com/share/6753730b-3d58-8005-9a30-ac98f3060ce2) and [here](https://chatgpt.com/share/6753674f-aa38-800a-b218-c6ebf5d03349).


