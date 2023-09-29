#Step 1: Load Required Libraries
# Load required libraries
library(tidyverse)
library(ggplot2)

#Step 2: Generate Dummy Data
# Set a random seed for reproducibility
set.seed(123)

# Create a dummy dataset
n_patients <- 100
treatment <- sample(c("Drug X", "Standard Treatment"), n_patients, replace = TRUE)
costs <- rnorm(n_patients, mean = c(1000, 800), sd = c(200, 100))
outcomes <- rnorm(n_patients, mean = c(0.9, 0.8), sd = c(0.1, 0.1))

# Create a data frame
data <- data.frame(Treatment = treatment, Costs = costs, Outcomes = outcomes)
write.csv(data,"C:/Users/Anup Kumar/Downloads/data.csv")
#Step 3: Data Summary
# Summary statistics
summary(data)

#Step 4: Cost Analysis
# Calculate mean costs by treatment
cost_summary <- data %>%
  group_by(Treatment) %>%
  summarize(Mean_Cost = mean(Costs))

# Plot cost comparison
ggplot(data = cost_summary, aes(x = Treatment, y = Mean_Cost, fill = Treatment)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Costs by Treatment",
       x = "Treatment",
       y = "Mean Cost")

#Step 5: Outcome Analysis
# Calculate mean outcomes by treatment
outcome_summary <- data %>%
  group_by(Treatment) %>%
  summarize(Mean_Outcome = mean(Outcomes))

# Plot outcome comparison
ggplot(data = outcome_summary, aes(x = Treatment, y = Mean_Outcome, fill = Treatment)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Outcomes by Treatment",
       x = "Treatment",
       y = "Mean Outcome")

#Step 6: Cost-Effectiveness Analysis
# Calculate incremental cost-effectiveness ratio (ICER)
baseline <- filter(data, Treatment == "Standard Treatment")
intervention <- filter(data, Treatment == "Drug X")

cost_diff <- mean(intervention$Costs) - mean(baseline$Costs)
outcome_diff <- mean(intervention$Outcomes) - mean(baseline$Outcomes)
icer <- cost_diff / outcome_diff

# Print ICER
cat("Incremental Cost-Effectiveness Ratio (ICER): $", round(icer, 2), "per unit of outcome improvement\n")




#CEA Shiny application
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Pharmaceutical Treatment Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Add any input elements here if needed
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
  
  # Create a dummy dataset
  set.seed(123)
  n_patients <- 100
  treatment <- sample(c("Drug X", "Standard Treatment"), n_patients, replace = TRUE)
  costs <- rnorm(n_patients, mean = c(1000, 800), sd = c(200, 100))
  outcomes <- rnorm(n_patients, mean = c(0.9, 0.8), sd = c(0.1, 0.1))
  
  dummy_data <- data.frame(Treatment = treatment, Costs = costs, Outcomes = outcomes)
  
  output$cost_plot <- renderPlot({
    # Your cost analysis code here using dummy_data
    ggplot(data = dummy_data, aes(x = Treatment, y = Costs, fill = Treatment)) +
      geom_bar(stat = "identity") +
      labs(title = "Mean Costs by Treatment",
           x = "Treatment",
           y = "Mean Cost")
  })
  
  output$outcome_plot <- renderPlot({
    # Your outcome analysis code here using dummy_data
    ggplot(data = dummy_data, aes(x = Treatment, y = Outcomes, fill = Treatment)) +
      geom_bar(stat = "identity") +
      labs(title = "Mean Outcomes by Treatment",
           x = "Treatment",
           y = "Mean Outcome")
  })
  
  output$icer_output <- renderText({
    # Your cost-effectiveness analysis code here using dummy_data
    baseline <- filter(dummy_data, Treatment == "Standard Treatment")
    intervention <- filter(dummy_data, Treatment == "Drug X")
    
    cost_diff <- mean(intervention$Costs) - mean(baseline$Costs)
    outcome_diff <- mean(intervention$Outcomes) - mean(baseline$Outcomes)
    icer <- cost_diff / outcome_diff
    paste("Incremental Cost-Effectiveness Ratio (ICER): $", round(icer, 2), "per unit of outcome improvement")
  })
}

# Run the Shiny app
shinyApp(ui, server)

























# Define UI
ui <- fluidPage(
  titlePanel("Pharmaceutical Treatment Analysis"),
  
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

