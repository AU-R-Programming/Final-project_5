% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_beta.R
\name{estimate_beta}
\alias{estimate_beta}
\title{Estimate Beta Coefficients for Logistic Regression}
\usage{
estimate_beta(X, y)
}
\arguments{
\item{X}{Numeric matrix of predictors.}

\item{y}{Numeric vector of binary response values (0 or 1).}
}
\value{
A numeric vector of estimated beta coefficients.
}
\description{
This function estimates the beta coefficients for a logistic regression model using numerical optimization.
}
