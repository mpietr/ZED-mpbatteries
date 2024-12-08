library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv('mp_batteries.csv')

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_column",
        label = "Choose an attribute:",
        choices = names(data %>% select(where(is.numeric))),
        selected = "Average.Voltage"
      )
    ),
    mainPanel(
      plotOutput("densityPlot")
    )
  )
)

server <- function(input, output) {
  
  reactive_data <- reactive({
    req(input$selected_column)
    data %>%
      select(input$selected_column) %>%
      pivot_longer(cols = everything(), names_to = "name", values_to = "value")
  })
  
  output$densityPlot <- renderPlot({
    reactive_data() %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "darkblue", alpha = 0.25) +
      labs(
        title = paste("Density Plot for", input$selected_column),
        x = "Value",
        y = "Density"
      ) + theme_minimal()
  })
}

shinyApp(ui, server)
