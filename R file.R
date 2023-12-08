# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(randomForest)

# Load data from Excel file
excel_file <- "C:\\Users\\dharm\\Downloads\\New folder\\icc\\Book1.xlsx"
data <- read_excel(excel_file)

# Define the UI
ui <- fluidPage(
  titlePanel("Cricket Team Data and Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_variable", "Select X-Axis Variable:", choices = names(data)),
      selectInput("y_variable", "Select Y-Axis Variable:", choices = names(data)),
      selectInput("plot_type", "Select Plot Type:", choices = c("Bar Chart", "Scatterplot")),
      actionButton("predict_button", "Predict Team Ranking")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("team_table"),  # Add table output
      verbatimTextOutput("prediction_output")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive values for storing predictions
  predictions <- reactiveValues(data = NULL)
  
  # Function to perform random forest prediction
  observeEvent(input$predict_button, {
    # Assuming 'Team_ranking' is the target variable
    target_variable <- "Team_ranking"
    
    # Features for prediction
    features <- setdiff(names(data), target_variable)
    
    # Train the Random Forest model
    rf_model <- randomForest(formula(paste(target_variable, "~ .")), data = data)
    
    # Make predictions
    predictions$data <- data.frame(Team_name = data$Team_name, Predicted = predict(rf_model, data))
  })
  
  # Plot output
  output$plot <- renderPlot({
    x_var <- data[[input$x_variable]]
    y_var <- data[[input$y_variable]]
    
    if (input$plot_type == "Bar Chart") {
      ggplot(data, aes(x = x_var, y = y_var)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Bar Chart", x = input$x_variable, y = input$y_variable)
    } else if (input$plot_type == "Scatterplot") {
      ggplot(data, aes(x = x_var, y = y_var, label = data$Team_name)) +
        geom_point() +
        geom_text(hjust = -0.2, vjust = -0.5) +
        labs(title = "Scatterplot", x = input$x_variable, y = input$y_variable)
    }
  })
  
  # Display predictions
  output$prediction_output <- renderPrint({
    if (!is.null(predictions$data)) {
      head(predictions$data)
    } else {
      "Click 'Predict Team Ranking' to see predictions."
    }
  })
  
  # Display team table
  output$team_table <- renderTable({
    if (!is.null(predictions$data)) {
      predictions$data[, c("Team_name", "Predicted")]
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
