#Required library
library(tidyverse)
library(ggplot2)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Cost Effectiveness Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("uploadData", "Upload Data (CSV format)"),
      # Add any other input elements here if needed
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cost Analysis", plotOutput("cost_plot")),
        tabPanel("Outcome Analysis", plotOutput("outcome_plot")),
        tabPanel("Cost-Effectiveness Analysis", verbatimTextOutput("icer_output"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create a reactive data frame to hold the uploaded data
  uploaded_data <- reactive({
    req(input$uploadData)
    inFile <- input$uploadData
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = TRUE)
  })
  
  output$cost_plot <- renderPlot({
    # Your cost analysis code here using uploaded_data()
    ggplot(data = uploaded_data(), aes(x = Treatment, y = Costs, fill = Treatment)) +
      geom_bar(stat = "identity") +
      labs(title = "Mean Costs by Treatment",
           x = "Treatment",
           y = "Mean Cost")
  })
  
  output$outcome_plot <- renderPlot({
    # Your outcome analysis code here using uploaded_data()
    ggplot(data = uploaded_data(), aes(x = Treatment, y = Outcomes, fill = Treatment)) +
      geom_bar(stat = "identity") +
      labs(title = "Mean Outcomes by Treatment",
           x = "Treatment",
           y = "Mean Outcome")
  })
  
  output$icer_output <- renderText({
    # Your cost-effectiveness analysis code here using uploaded_data()
    data <- uploaded_data()  # Rename for convenience
    baseline <- filter(data, Treatment == "Standard Treatment")
    intervention <- filter(data, Treatment == "Drug X")
    
    cost_diff <- mean(intervention$Costs) - mean(baseline$Costs)
    outcome_diff <- mean(intervention$Outcomes) - mean(baseline$Outcomes)
    icer <- cost_diff / outcome_diff
    paste("Incremental Cost-Effectiveness Ratio (ICER): $", round(icer, 2), "per unit of outcome improvement")
  })
}

# Run the Shiny app
shinyApp(ui, server)

