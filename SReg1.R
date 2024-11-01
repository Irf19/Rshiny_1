#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
rsconnect::writeManifest()

library(shiny)
library(bslib)
library(ggplot2)

ui <- page_sidebar(
  title = "Analisis Regresi",
  sidebar = sidebar(
    sliderInput("n", "Jumlah Data:", min = 20, max = 1234, value = 100),
    sliderInput("error_sd", "Error term standar deviasi:", min = 0.1, max = 5, value = 1, step = 0.1),
    sliderInput("num_vars", "Jumlah variabel penjelas:", min = 1, max = 5, value = 2),
    actionButton("generate", "Generate")
  ),
  card(
    card_header("Plotting Regresi"),
    plotOutput("regression_plot")
  ),
  card(
    card_header("Ringkasan Hasil"),
    verbatimTextOutput("regression_summary")
  ),
  card(
    card_header("R-squared dan signifikansi"),
    verbatimTextOutput("rsquared_significance")
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    n <- input$n
    error_sd <- input$error_sd
    num_vars <- input$num_vars
    
    X <- matrix(runif(n * num_vars, -5, 5), ncol = num_vars)
    colnames(X) <- paste0("X", 1:num_vars)
    
    true_betas <- runif(num_vars, -2, 2)
    y <- X %*% true_betas + rnorm(n, 0, error_sd)
    
    data(data.frame(y = y, X))
  })
  
  model <- reactive({
    req(data())
    lm(y ~ ., data = data())
  })
  
  output$regression_plot <- renderPlot({
    req(model())
    
    df <- data()
    ggplot(df, aes(x = X1, y = y)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
      theme_minimal() +
      labs(title = "Plot Regresi (X1 vs. Y)",
           x = "X1", y = "Y")
  })
  
  output$regression_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$rsquared_significance <- renderPrint({
    req(model())
    
    summary_model <- summary(model())
    cat("R-squared:", round(summary_model$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(summary_model$adj.r.squared, 4), "\n\n")
    
    cat("Significant variables (p < 0.05):\n")
    significant_vars <- summary_model$coefficients[, 4] < 0.05
    print(names(significant_vars)[significant_vars])
  })
  
  # Initialize the app with data
  observeEvent(0, {
    input$generate
  }, once = TRUE)
}

shinyApp(ui, server)
