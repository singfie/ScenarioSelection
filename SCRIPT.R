# preamble
library(plyr)
library(dplyr)
library(tidyverse)
library(reshape2)
# feed in data
setwd("")
plane_crash <- read.csv("plane_crash_v2.csv", header=TRUE)
flight_time <- read.csv("flight_time.csv", header=TRUE)
# data transformations
plane_crash$date <- as.POSIXct(plane_crash$date, format = "%d-%b-%Y")

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
  reduced_data <- data %>%
    filter(str_detect(nature, type))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- type_of_interest(data = plane_crash, type = 'Passenger|Military')
# head(plane_crash_reduced)


############## MANUFACTURER OF INTEREST ##############
# function to reduce the dataset to manufacturer(s) of interest
# same structure as "type of interest"; pass list using "|" as seperator for multiple entries
manuf_of_interest <- function(data, manuf){ 
  reduced_data <- data %>%
    filter(str_detect(plane_type, manuf))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- manuf_of_interest(data = plane_crash, manuf = 'Airbus|Boeing|Bombardier|Comac|Douglas|Embraer')
# head(plane_crash_reduced)


############## FLIGHT PHASE OF INTEREST ##############
# function to reduce the dataset to flight phases of interest
# same structure as "type of interest"; pass list using "|" as seperator for multiple entries
phase_of_interest <- function(data, interest){ 
  reduced_data <- data %>%
    filter(str_detect(phase, interest))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- phase_of_interest(data = plane_crash, interest = 'TOF|ICL|ENR|APR|LDG')
# head(plane_crash_reduced)


############## SCENARIO GROUP OF INTEREST ##############
# function to reduce the dataset to scenario groups of interest
# same structure as "type of interest"; pass list using "|" as seperator for multiple entries
scenario_of_interest <- function(data, interest){ 
  reduced_data <- data %>%
    filter(str_detect(group, interest))
  return(reduced_data)
}

# testing
# plane_crash_reduced <- scenario_of_interest(data = plane_crash, interest = 'Cargo|Instruments')
# head(plane_crash_reduced)


############## OVERVIEW ON INCIDENTS FRAME ##############
# this is a function to generate the frequency overview per flight phase per scenario group
# take note, that for this method to work as desired, 
# the data needs to be manipulated according to the functions above.
generate_frequency_overview <- function(data){
  overview <- data.frame(NA)
  overview[,1:length(summary(data$phase))] <- NA
  colnames(overview) <- names(summary(data$phase)) # name the column
  for (i in unique(data$group))
  {
    overview[i,1:10] <- summary(data$phase[data$group == i])
  }
  overview <- overview[-1,] # reduce by empty generated row
  colnames(overview) <- c("APR", "ENR", "ICL", "LDG", "MNV", "PBT", "STD", "TOF", "TXI", "UNK")
  return(overview)
}

# testing
# test <- generate_frequency_overview(plane_crash)
# head(test)


############## SIM-SCORE CALCULATION ##############
# function to calculate the simulator score
# note that this function only works for flight phases in flight, 
# as no data is available for time spend in on-ground phases
# Hence this method trims the dataset by on-ground phases, if not done before
# flight phases of interest need to be passed in the form of a vector
sim_score <- function(overview_data, flight_times, flight_phases_of_interest){
  # trim to available data
  trimvars <- c("APR", "ENR", "ICL", "LDG", "TOF")
  overview_data <- overview_data[trimvars]
  # individual trim
  myvars <- flight_phases_of_interest
  overview_data <- overview_data[myvars]
  # reshape
  overview_data <- rownames_to_column(overview_data)
  m <- melt(overview_data)
  
  # calculate scenario score
  for (i in unique(m$rowname)){
    for (j in unique(m$variable)){
      m$norm[m$variable==j] <- m$value[m$variable==j]*
        (flight_time$percentage[flight_time$flight_phase==j])^(-1)
    }
    m$total[m$rowname==i] <- sum(m$norm[m$rowname==i])
  }
  m$score <- m$norm/m$total
  return(m)
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
plane_crash_reduced <- years_of_interest(data = plane_crash, 
                                         start_date = "1981-09-26", 
                                         end_date = "2019-05-21")
plane_crash_reduced <- manuf_of_interest(data = plane_crash_reduced, 
                                         manuf = 'Airbus|Boeing|Bombardier|Comac|Douglas|Embraer|Dornier|Fokker|Gulfstream|Ilyushin|Saab|Sukhoi|Tupolev')
plane_crash_reduced <- phase_of_interest(data = plane_crash_reduced, interest = 'TOF|ICL|ENR|APR|LDG')
plane_crash_reduced <- scenario_of_interest(data = plane_crash_reduced, 
                                            interest = 'Engines|Undercarriage|Fire|Systems|Cargo|Flight control surfaces|External factors|Instruments|Pressurization') 
stats_overview <- generate_frequency_overview(plane_crash_reduced)
myvars <- c("APR", "ENR", "ICL", "LDG", "TOF")
score_frame <- sim_score(stats_overview, flight_time, myvars)
