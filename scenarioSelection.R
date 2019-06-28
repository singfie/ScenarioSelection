# preamble
library(plyr)
library(dplyr)
library(tidyverse)
library(reshape2)

# Read in data
# feed in data
plane_crash <- read.csv("plane_crash_v2.csv", header=TRUE)
flight_time <- read.csv("flight_time.csv", header=TRUE)
# plane models
plane_choices <- read.csv("plane_models.csv", header=TRUE)
plane_choices <- plane_choices[,-1]
# plane models
aviation_types <- read.csv("aviation_types.csv", header=TRUE)
aviation_types <- aviation_types[,-1]
# data transformations
plane_crash$date <- as.POSIXct(plane_crash$date, format = "%Y-%m-%d")

############## DATE RANGE OF INTEREST ##############
# function to reduce the dataset to the date-range of interest
years_of_interest <- function(data, start_date, end_date){
  reduced_data <- subset(data, date >= start_date & date <= end_date)
  return(reduced_data)
}

# testing
# plane_crash_reduced <- years_of_interest(data = plane_crash, start_date = "1981-09-26", end_date = "2019-05-21")
# head(plane_crash_reduced)


############## AVIATION TYPE OF INTEREST ##############
# function to reduce the dataset to the aviation type of interest
# NOTE: this function uses fractional information, it looks for all values containing the passed type
# for multiple selections pass a type list in the format 'type a | type b | type c'
type_of_interest <- function(data, type){ 
  if(grepl('Other', type, fixed = T)){
    others <- paste(c('|-', 'Aerial Work', 'Agricultural', 'Ambulance', 'Demonstration', 'Ferry', 
                      'Fire', 'Illegal', 'Nature', 'state', 'Parachuting', 'research', 'Test', 'Unknown'), collapse="|")
    type <- paste0(type, others, collapse = "|")
  }
  reduced_data <- data %>% filter(str_detect(nature, type))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- type_of_interest(data = plane_crash, type = 'Passenger|Military')
# head(plane_crash_reduced)


############## MANUFACTURER OF INTEREST ##############
# function to reduce the dataset to manufacturer(s) of interest
# same structure as "type of interest"; pass list using "|" as seperator for multiple entries
manuf_of_interest <- function(data, manuf){ 
  reduced_data <- data %>% filter(str_detect(plane_type, manuf))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- manuf_of_interest(data = plane_crash, manuf = 'Airbus|Boeing|Bombardier|Comac|Douglas|Embraer')
# head(plane_crash_reduced)


############## FLIGHT PHASE OF INTEREST ##############
# function to reduce the dataset to flight phases of interest
# same structure as "type of interest"; pass list using "|" as seperator for multiple entries
phase_of_interest <- function(data, interest){ 
  reduced_data <- data %>% filter(str_detect(phase, interest))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- phase_of_interest(data = plane_crash, interest = 'TOF|ICL|ENR|APR|LDG')
# head(plane_crash_reduced)


############## SCENARIO GROUP OF INTEREST ##############
# function to reduce the dataset to scenario groups of interest
# same structure as "type of interest"; pass list using "|" as seperator for multiple entries
scenario_of_interest <- function(data, interest){ 
  reduced_data <- data %>% filter(str_detect(group, interest))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- scenario_of_interest(data = plane_crash, interest = 'Cargo|Instruments')
# head(plane_crash_reduced)


#Simulation score calculation
sim_score <- function(overview_data, flight_times, flight_phases_of_interest){
  overview_data$phase = mapvalues(overview_data$phase, 
                                from = levels(overview_data$phase), 
                                to = c("APR", "ENR", "ICL", "LDG", "MNV", "PBT", "STD", "TOF", "TXI", "UNK"))
  overview_data = overview_data %>% group_by(group, phase) %>% summarise(value = n()) %>% filter(phase %in% flight_phases_of_interest)
  
  # calculate scenario score
  for (i in unique(overview_data$group)){
    for (j in unique(overview_data$phase)){
      overview_data$norm[overview_data$phase==j] <- overview_data$value[overview_data$phase==j]*(flight_time$percentage[flight_time$flight_phase==j])^(-1)
    }
    overview_data$total[overview_data$group==i] <- sum(overview_data$norm[overview_data$group==i])
  }
  overview_data$score <- overview_data$norm/overview_data$total
  return(overview_data)
}

# testing
# myvars <- c("APR", "ENR", "ICL", "LDG", "TOF")
# score_frame <- sim_score(test, flight_time, myvars)


############## TRAINING DATA ##############
# training information not included in this script!
# in the next step the frame obtained from the sim_score() function
# needs to be used to evaluate every single scenario from training material


############## TESTING VALIDATION ##############
# the test case resembles the data analysis performed in the scope of the submitted paper
# plane_crash_reduced <- years_of_interest(data = plane_crash,
#                                          start_date = "1981-09-26",
#                                          end_date = "2019-05-21")
# plane_crash_reduced <- manuf_of_interest(data = plane_crash_reduced,
#                                          manuf = 'Airbus|Boeing|Bombardier|Comac|Douglas|Embraer|Dornier|Fokker|Gulfstream|Ilyushin|Saab|Sukhoi|Tupolev')
# plane_crash_reduced <- phase_of_interest(data = plane_crash_reduced, interest = 'TOF|ICL|ENR|APR|LDG')
# plane_crash_reduced <- scenario_of_interest(data = plane_crash_reduced,
#                                             interest = 'Engines|Undercarriage|Fire|Systems|Cargo|Flight control surfaces|External factors|Instruments|Pressurization')
# stats_overview <- generate_frequency_overview(plane_crash_reduced)
# myvars <- c("APR", "ENR", "ICL", "LDG", "TOF")
# score_frame <- sim_score(plane_crash_reduced, flight_time, myvars)
