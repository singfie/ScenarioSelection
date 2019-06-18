#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

manuChoices = c('Airbus','Boeing','Bombardier','Comac','Douglas','Embraer','Dornier','Fokker','Gulfstream','Ilyushin','Saab','Sukhoi','Tupolev')

#Valid flight phases
validFlightPhases <- c("Approach" = "APR","Enroute" = "ENR","Initial Climb" = "ICL","Landing" = "LDG","Takeoff" = "TOF")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Scenario Score Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("dateSelector"),
       selectizeInput("manufacturerSelected", "Select manufacturers to analyze", choices = manuChoices, multiple=TRUE),
       selectizeInput("flightPhaseSelected", "Select flight phases to analyze", choices = validFlightPhases , multiple=TRUE),
       uiOutput("scenarioSelector"),
      actionButton("ssButton", "Calculate Scenario Score")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput('ssTable'),
      plotOutput('plot1')
    )
  )
))
