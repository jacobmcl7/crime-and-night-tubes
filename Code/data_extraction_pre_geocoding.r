# this extracts the data to create one large dataset of crime locations that can be geocoded, and also saves the individual crime data for later processing

library(readxl)
library(writexl)
library(tidyverse)

setwd("~/Economics/Papers (WIP)")


# load in the first month of 2015 data
crime_data <- read.csv("Crime and night tubes/Data/2015-01/2015-01-metropolitan-street.csv", stringsAsFactors = FALSE)

# merge with the rest of the data
for (i in 2:12) {
    # get the correct month format
    month <- ifelse(i < 10, paste0("0", i), i)
    # read in the data
    temp_data <- read.csv(paste0("Crime and night tubes/Data/2015-", month, "/2015-", month, "-metropolitan-street.csv"), stringsAsFactors = FALSE)
    # merge with the main dataset
    crime_data <- rbind(crime_data, temp_data)
}  

for (j in 2016:2017) {
    for (i in 1:12) {
        # get the correct month format
        month <- ifelse(i < 10, paste0("0", i), i)
        temp_data <- read.csv(paste0("Crime and night tubes/Data/", j, "-", month, "/", j, "-", month, "-metropolitan-street.csv"), stringsAsFactors = FALSE)
        crime_data <- rbind(crime_data, temp_data)
    }  
}


# now do some processing
crime_data <- as.data.frame(crime_data) %>%
    # drop irrelevant columns
    select(-c(Crime.ID, Reported.by, Falls.within, Context)) %>%

    # drop observations with no coordinates
    filter(!is.na(Longitude) & !is.na(Latitude)) %>%

    # generate a unique identifier for each crime, given by 'year'-'row number' (have to do this since the crime.id in the data is occasionally missing)
    mutate(crime_id = paste0(Month, "-", row_number()))


# now generate the data we will need for the geoprocessing - specifically, every unique location (i.e. longitude-latitude pair) in the data
geocoding_data <- crime_data %>%
    select(c(Longitude, Latitude)) %>%
    distinct()

# there are 63294 distinct locations of crime - these will be geocoded and related to tube station locations in the gis_processing file

# split into three different files, so that ArcGIS can calculate distances to further away tube stations without crashing

# export this as an excel file, for the geocoding
# in fact, export as three different excel files, so that ArcGIS can calculate distances to further away tube stations without crashing
write_xlsx(geocoding_data[1:floor(nrow(geocoding_data) / 3), ], "Crime and night tubes EXTRA DATA/london_crime_locations_1.xlsx")
write_xlsx(geocoding_data[(floor(nrow(geocoding_data) / 3) + 1):(floor(2 * nrow(geocoding_data) / 3)), ], "Crime and night tubes EXTRA DATA/london_crime_locations_2.xlsx")
write_xlsx(geocoding_data[(floor(2 * nrow(geocoding_data) / 3) + 1):nrow(geocoding_data), ], "Crime and night tubes EXTRA DATA/london_crime_locations_3.xlsx")


# save the crime data as an R datafile so that we can merge back in the geolocated data later
save(crime_data, file = "Crime and night tubes EXTRA DATA/individual_crime_data.RData")