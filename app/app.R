library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv('mp_batteries.csv')

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Density Plot"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for column selection
      selectInput(
        inputId = "selected_column",
        label = "Choose a Column:",
        choices = names(data %>% select(where(is.numeric)))  # Numeric columns only
      )
    ),
    
    mainPanel(
      plotOutput("densityPlot") # Output the density plot
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive data for the selected column
  reactive_data <- reactive({
    req(input$selected_column)  # Ensure a column is selected
    data %>%
      select(input$selected_column) %>%
      pivot_longer(cols = everything(), names_to = "name", values_to = "value")
  })
  
  # Render Density Plot
  output$densityPlot <- renderPlot({
    reactive_data() %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(
        title = paste("Density Plot for", input$selected_column),
        x = "Value",
        y = "Density"
      ) +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui, server)
