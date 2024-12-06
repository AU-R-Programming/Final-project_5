#' Run the Logistic Regression Shiny App
#'
#' This function launches the Shiny app for logistic regression.
#' The app allows users to upload datasets, select features,
#' and compute logistic regression results.
#'
#' @export
run_logisticreg_app <- function() {
  # Locate the app directory inside the package
  appDir <- system.file("logisticreg_shiny", package = "logisticreg")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  
  # Run the Shiny app
  shiny::runApp(appDir, display.mode = "normal")
}
