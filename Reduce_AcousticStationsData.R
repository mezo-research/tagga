## Date: 4 October 2016
## Creator: Christina Smith
## Purpose: Reduce data in AcousticStations_reduced_2hr.csv to contain the min
## data required for the mapping and time series plots and save in 
## AcousticStations_maptime.csv
##
## NOTES: Date is not read in as an R date

library(dplyr)

## Read in reduced Acoustic Station Data.
station_data <- read.csv("AcousticStations_reduced_2hr.csv")


## transform data: add extra column with unit count, group by date, life stage
## and station (keeping station region and coordinates), and finally add 
## all fish of each life stage recorded at each station on a particular day
min_data <- station_data %>%
    mutate(number = 1) %>%
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
    group_by(Date, Life_stage, StationID, Station_Region, Latitude, Longitude) %>%
    summarise(Number_fish = sum(number))



## Write data to comma separated file
write.table(min_data, file = "AcousticStations_maptime.csv", sep = ",", 
            row.names = FALSE)