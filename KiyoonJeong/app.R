library(shiny)
library(ggplot2)
library(dplyr)

df_select <- read.csv("AnnualIncome_WeeklyWorkHour.csv")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("TimeInput", "Time", min = 2000, max = 2014, value = c(2000, 2014)),
      selectInput("nameInput", "Choose a country:", choices = df_select$Country),
      mainPanel(
        plotOutput(outputId = "main_plot", width = "500%"),
        tableOutput("results")
      )
    ),
    titlePanel("Working Hour versus Income")
  ))
server <- function(input, output, session) {
  reduced_df <- reactive({
    filter(
      df_select, 
      Country == input$nameInput, 
      Time >= input$TimeInput[1] & Time <= input$TimeInput[2]
    )
  })
  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(WorkHour, Income, colour = Country)) + 
      geom_line() + ggtitle(input$nameInput)
    
  })
  output$results <- renderTable({ 
    reduced_df()
  })
}

shinyApp(ui = ui, server = server)

