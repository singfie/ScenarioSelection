#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("scenarioSelection.R")

#Read in data
# feed in data
plane_crash <- read.csv("plane_crash_v2.csv", header=TRUE)
flight_time <- read.csv("flight_time.csv", header=TRUE)
# data transformations
plane_crash$date <- as.POSIXct(plane_crash$date, format = "%Y-%m-%d")

#Valid flight phases
validFlightPhases <- c("APR", "ENR", "ICL", "LDG", "TOF")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$dateSelector <- renderUI({
    dateRangeInput("dateRange", "Date range:",
                   start = min(plane_crash$date, na.rm = TRUE),
                   end   = max(plane_crash$date, na.rm = TRUE),
                   format="yyyy-mm-dd")
  })
  
  output$scenarioSelector <- renderUI({
    selectizeInput("scenarioSelected", "Select scenarios to analyze", choices = levels(plane_crash$group), multiple=TRUE)
  })
  
  buttonText = eventReactive(input$ssButton, {
    #validate(input$dateRange[2] > input$dateRange[1], "End date is earlier than the start date")
    plane_crash_reduced <- years_of_interest(data = plane_crash,start_date = input$dateRange[1], end_date = input$dateRange[2])
    plane_crash_reduced <- manuf_of_interest(data = plane_crash_reduced, manuf = paste(input$manufacturerSelected, collapse = "|"))
    plane_crash_reduced <- phase_of_interest(data = plane_crash_reduced, interest = paste(input$flightPhaseSelected, collapse = "|"))
    plane_crash_reduced <- scenario_of_interest(data = plane_crash_reduced,interest = paste(input$scenarioSelected, collapse = "|"))
    score_frame <- sim_score(plane_crash_reduced, flight_time, input$flightPhaseSelected)
    score_frame
  })
  output$ssTable <- renderTable({
    buttonText()
  })
})
