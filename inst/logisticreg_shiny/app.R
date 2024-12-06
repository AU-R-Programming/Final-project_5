library(shiny)

ui <- fluidPage(
  titlePanel("Logistic Regression Analysis (Numerical Optimization)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV File", accept = ".csv"),
      uiOutput("y_var_ui"),
      uiOutput("x_vars_ui"),
      numericInput("no_bootstrap", "Number of Bootstraps:", value = 20, min = 1, step = 1),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      h3("Results"),
      verbatimTextOutput("model_output")
    )
  )
)

server <- function(input, output, session) {
  
  # Attempt to read file with comma, if only one column found, try semicolon
  data_reactive <- reactive({
    req(input$datafile)
    # First try comma
    d <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE, sep = ",")
    
    # If there's only one column, the file might be semicolon-separated
    if (ncol(d) == 1) {
      # Check if there's a semicolon in the first line, if so, try semicolon
      first_line <- readLines(input$datafile$datapath, n = 1)
      if (length(grep(";", first_line)) > 0) {
        d_semicolon <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE, sep = ";")
        # If semicolon parsing gives more columns, use that
        if (ncol(d_semicolon) > 1) {
          d <- d_semicolon
        }
      }
    }
    d
  })
  
  # Dynamically generate UI for y_var and x_vars after data is uploaded
  output$y_var_ui <- renderUI({
    req(data_reactive())
    selectInput("y_var", "Select Response Variable:", choices = names(data_reactive()))
  })
  
  output$x_vars_ui <- renderUI({
    req(data_reactive())
    selectInput("x_vars", "Select Predictor Variables:", 
                choices = names(data_reactive()), multiple = TRUE)
  })
  
  # Run analysis when button is clicked
  results <- eventReactive(input$run_analysis, {
    req(data_reactive(), input$y_var, input$x_vars)
    if (length(input$x_vars) == 0) {
      return("Please select at least one predictor variable.")
    }
    logistic_regression_analysis(
      data = data_reactive(),
      x_vars = input$x_vars,
      y_var = input$y_var,
      no_of_bootstraps = input$no_bootstrap
    )
  })
  
  # Display results
  output$model_output <- renderPrint({
    res <- results()
    if (is.character(res)) {
      cat(res)
    } else {
      cat("Estimated Coefficients:\n")
      print(res$beta)
      
      cat("\nBootstrap Confidence Intervals:\n")
      print(res$bootstrap_conf_int)
      
      cat("\nPredicted Probabilities (first 6):\n")
      print(head(res$predicted_probabilities))
      
      cat("\nPredictions (first 6):\n")
      print(head(res$predictions))
      
      cat("\nConfusion Matrix Metrics:\n")
      print(res$confusion_matrix_metrics)
    }
  })
}

shinyApp(ui = ui, server = server)




