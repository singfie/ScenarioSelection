#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggrepel)
library(ggplot2)
source("scenarioSelection.R")

setwd("/Users/fietekrutein/Documents/University/University of Washington/RA positions/HFSM/ScenarioSelection-master/")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  #Valid flight phases
  validFlightPhases <- c("TOF", "ICL", "ENR", "APR","LDG")
  
  output$dateSelector <- renderUI({
    dateRangeInput("dateRange", "Date range:",
                   start = min(plane_crash$date, na.rm = TRUE),
                   end   = max(plane_crash$date, na.rm = TRUE),
                   format="yyyy-mm-dd")
  })
  
  output$modelSelector <- renderUI({
    selectInput("modelSelected", "Select models to analyze", choices = plane_choices, multiple=TRUE)
  })
  
  output$scenarioSelector <- renderUI({
    selectInput("scenarioSelected", "Select scenarios to analyze", choices = levels(plane_crash$group), multiple=TRUE)
  })
  
  buttonText = eventReactive(input$ssButton, {
    plane_crash_reduced <- years_of_interest(data = plane_crash, start_date = input$dateRange[1], end_date = input$dateRange[2])
    plane_crash_reduced <- type_of_interest(data = plane_crash_reduced, type = paste(input$typeSelected, collapse = "|"))
    plane_crash_reduced <- manuf_of_interest(data = plane_crash_reduced, manuf = paste(input$manufacturerSelected, collapse = "|"))
    plane_crash_reduced <- phase_of_interest(data = plane_crash_reduced, interest = paste(input$flightPhaseSelected, collapse = "|"))
    plane_crash_reduced <- scenario_of_interest(data = plane_crash_reduced,interest = paste(input$scenarioSelected, collapse = "|"))
    score_frame <- sim_score(plane_crash_reduced, flight_time, input$flightPhaseSelected)
  })
  output$ssTable <- renderTable({
    df2 <- buttonText()
    df2 <- cbind(df2$group, df2$phase, df2$value, df2$score)
    colnames(df2) <- c("Incident group", "In-flight phase", "Number of occurences", "Scenario Score")
    df2
  })
  output$plot1 <- renderPlot({
    df <- buttonText()
    ggplot(data=df, aes(x=score, y=phase, size=value, colour=phase)) + geom_point(show.legend=TRUE) + ggtitle("Scoring of scenarios") + guides(size=FALSE) + ylab("Flight Phase") + xlab("Scenario Score") + xlim(0,1) + theme_light() + theme(text = element_text(size=20), legend.position="none", legend.title=element_text(size=20), legend.text=element_text(size=16)) + geom_text_repel(aes(label=value), nudge_y = 0.1, nudge_x=0.025, direction = "x", angle = 0, vjust = 0, segment.size = 0.2, size=6, show.legend=FALSE) + scale_y_discrete(limits=validFlightPhases)+ facet_wrap(~group, ncol=1, scales="free") 
  })
  output$plot2 <- renderPlot({
    df <- buttonText()
    ggplot(df, aes(x = phase, y = value,  fill = phase)) + facet_wrap(~ group) + geom_bar(stat="identity", show.legend=FALSE) + ggtitle("Overview on count values") + ylab("No. of incidents in selected time frame") + xlab("Flight phase") + theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
})
