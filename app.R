library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(openxlsx)
library(readxl)

data_A<-read.xlsx("sarurday data from Arjun.xlsx")

# Define UI
ui <- fluidPage(
  titlePanel("Canton schools : Lunch menu entrées and their carbon footprint"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("school", "Select School:", 
                  choices = unique(data_A$School_Name), 
                  selected = unique(data_A$School_Name)[1]),
      
      selectInput("weekday", "Select Weekday:", 
                  choices = unique(data_A$Week_days), 
                  selected = "Monday"),
      
      uiOutput("entreeInput"),
      
      actionButton("selectAll", "Select All Entrées"),
      actionButton("clearAll", "Clear All Entrées")
      
    ),
    mainPanel(
      tableOutput("filteredData"),
      verbatimTextOutput("totalEmission"),
      plotOutput("emissionPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Ensure data_A exists before filtering
  req(exists("data_A"), is.data.frame(data_A))
  
  # Reactive expression to filter data by selected school and weekday
  filteredBySchoolAndWeekday <- reactive({
    subset(data_A, School_Name == input$school & Week_days == input$weekday)
  })
  
  # Dynamic UI for entrée selection
  output$entreeInput <- renderUI({
    available_entrees <- unique(filteredBySchoolAndWeekday()$Entrée)
    selectInput("entrees", "Select Entrées:", 
                choices = available_entrees, 
                multiple = TRUE, 
                selected = ifelse(length(available_entrees) > 0, available_entrees[1], NULL))
  })
  
  # Observe "Select All" button
  observeEvent(input$selectAll, {
    updateSelectInput(session, "entrees",
                      selected = unique(filteredBySchoolAndWeekday()$Entrée))
  })
  
  # Observe "Clear All" button
  observeEvent(input$clearAll, {
    updateSelectInput(session, "entrees", selected = character(0))
  })
  
  # Reactive expression to filter data
  filteredData <- reactive({
    req(input$entrees)
    data <- filteredBySchoolAndWeekday()
    filtered <- subset(data, Entrée %in% input$entrees)
    filtered[order(-filtered$Footprint), ]
  })
  
  # Render sorted filtered data table
  output$filteredData <- renderTable({
    data <- filteredData()
    if (nrow(data) > 0) data else data.frame(Message = "No data available")
  })
  
  # Calculate and render total emissions
  output$totalEmission <- renderText({
    data <- filteredData()
    if (nrow(data) > 0) {
      total <- sum(data$Emission_per_day, na.rm = TRUE)  # Handle potential NA values
      paste("Total Carbon Emission for selected entrees on", input$weekday, "at", input$school, ":", total, "kg CO₂e")
    } else {
      "No data available for the selected filters."
    }
  })
  
  # Render emission plot with "Total" bar
  output$emissionPlot <- renderPlot({
    data <- filteredData()
    req(nrow(data) > 0)
    
    total_emission <- sum(data$Footprint, na.rm = TRUE)  # Handle NA values
    
    # Ensure `"Total"` row has all matching columns
    total_row <- data.frame(
      Entrée = "Total",
      Footprint = total_emission
    )
    
    # Add `"Total"` row
    data_with_total <- rbind(data[, c("Entrée", "Footprint")], total_row)
    
    # Convert `Entrée` into a factor to keep "Total" at the end
    data_with_total$Entrée <- factor(data_with_total$Entrée, levels = c(as.character(data$Entrée), "Total"))
    
    ggplot(data_with_total, aes(x = Entrée, y = Footprint, fill = Entrée)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(rep("steelblue", nrow(data)), "red")) +
      theme_minimal() +
      labs(title = paste("Carbon Footprint for Selected Entrées on", input$weekday, "at", input$school),
           x = "Entrées",
           y = "Carbon Emission (kg CO₂e)") +
      geom_text(aes(label = round(Footprint, 2)), vjust = -0.5, color = "black", size = 5) +
      theme(text = element_text(size = 16),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_cartesian(ylim = c(0, max(data_with_total$Footprint) * 1.2))
  })
}

# Run the app
shinyApp(ui = ui, server = server)