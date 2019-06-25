#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(jpeg)
library(leaflet)
library(leaflet.extras)


typeChoices <- c('Passenger', 'Cargo', 'Military', 'Private', 'Training', 'Executive', 'Other')
#Valid flight phases
validFlightPhases <- c("Takeoff" = "TOF","Initial Climb" = "ICL","Enroute" = "ENR","Approach" = "APR","Landing" = "LDG")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(skin="blue",
                dashboardHeader(title = "Flight Scenario Selector", titleWidth = 250),
                dashboardSidebar(
                  width = 250,
                  sidebarMenu(
                    menuItem("Home Page", tabName = "homePage", icon = icon("dashboard")),
                    menuItem("Scenario Score Calculation", tabName = "SSc", icon = icon("bar-chart-o"))
                    )),
                      
                    ## Body content
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "homePage",
                                fluidRow(column(width = 12), column(12, align ='center', h1("Overview on Flight Scenario Selector"))),
                                column(width=12,
                                       box(
                                         title = "Background", width = NULL, solidHeader = TRUE, status = "primary",
                                         "This tool is supposed to deliver a systematic method on how to assess the suitability
                                         of different problem scenarios for studies conducted in flight simulators. This should
                                         give aviation authorities, pilot training schools and researchers a tool to select scenarios
                                         based on the occurence frequency of a certain scenario in aviation and the level of preparedness
                                         of pilots through training (the latter one is not included in this tool due to limited data availability). The motivation behind this idea is the high cost of flight simulator hours.
                                         Hence, keeping the amount of scenarios small while covering a wide range of potential extremes is desirable.
                                         This method satisfies this need."
                                       ),
                                       box(
                                         title = "Method", width = NULL, solidHeader = TRUE, status = "primary",
                                         fluidRow(column(width = 12, "The method uses the following formula to evaluate the developed scenario score:"),
                                                  column(width = 1, height = 1, align = "center", img(src = "formula.png")),
                                                  column(width = 12, "where i = in-flight phase (Take-off, initial climb, etc., j = scenario group, 
                                                         and Sij = frequency of scenario j at in-flight phase i. Note, that this score is a comparative score and hence calculated relative to 
                                                         the selected flight phases and therefore varies, based on the flight phases that are selected."))
                                         
                                       ),
                                       box(
                                         title = "Funding", width = NULL, solidHeader = TRUE, status = "primary",
                                         "This method was developed in the scope of a research project in the Human Factors & Statistical Modeling
                                         Laboratory at the University of Washington, Seattle, USA, to investigate peak workload in modern aicraft cockpits.
                                         The corresponding research paper will be published in the conference proceedings of the Human Factors & Ergonomics
                                         Society Annual Conference of 2019. This research was sponsored by the Joint Center for Aerospace Technology Innovation (JCATI), 
                                         in collaboration with Esterline."
                                       )
                                )
                                
                        ),
                        tabItem(tabName = "SSc",
                                fluidRow(column(width = 12), column(12, align ='center', h1("Scenario Score Calculator"))),
                                fluidRow(column(width = 12), column(12, align ='center', p("Note that all scenario scores are dependent on the selected flight phases!"))),
                                fluidRow(column(width =12), column(12, align = 'left', box(
                                  title = "Instructions", width = NULL, solidHeader = TRUE, status = "primary",
                                  "To calculate the scenarios scores, please choose a date range of interest, select aircraft models that you want to analyze or choose entire manufacturers, 
                                  select the aviation type you want to look at and select flight phases and scenarios of interest. Recall, that the scores are dependent on the selected scenarios and flight phases. 
                                  Further, if you do not find an entry in a list for a certain combination of values, no occurences have been found."
                                ))),
                                fluidRow(column(width =12), column(12, align = 'left', box(
                                  title = "Legend Entries", width = NULL, solidHeader = TRUE, status = "primary",
                                  "TOF = Take-off, ICL = Initial climb, ENR = En-route, APR = Approach, LDG = Landing"
                                ))),
                                fluidPage(
  
  # Application title
  #titlePanel("Scenario Score Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("dateSelector"),
      selectizeInput("typeSelected", "Select aviation types to analyze", choices = typeChoices, multiple=TRUE),
      uiOutput("modelSelector"),
      selectInput("flightPhaseSelected", "Select flight phases to analyze", choices = validFlightPhases , multiple=TRUE),
      uiOutput("scenarioSelector"),
      actionButton("ssButton", "Calculate Scenario Score")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput('ssTable'),
      plotOutput('plot1'),
      plotOutput('plot2')
    )
  )))))
))
