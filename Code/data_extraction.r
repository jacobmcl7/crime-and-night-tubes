# this extracts the data to create one large dataset of dated crimes that can be geocoded

library(writexl)
library(tidyverse)

setwd("~/Economics/Papers (WIP)")

# make a different datafile for each year, so the files aren't too big

for (year in 2015:2017) {

    # load in the first month of the year data
    crime_data <- as.data.frame(read.csv(paste0("Crime and night tubes/Data/", year, "-01/", year, "-01-metropolitan-street.csv"), stringsAsFactors = FALSE))
    
    # merge with the rest of the data
    for (i in 2:12) {
        # get the correct month format
        month <- ifelse(i < 10, paste0("0", i), i)
        # read in the data
        temp_data <- as.data.frame(read.csv(paste0("Crime and night tubes/Data/", year, "-", month, "/", year, "-", month, "-metropolitan-street.csv"), stringsAsFactors = FALSE))
        # merge with the main dataset
        crime_data <- rbind(crime_data, temp_data)
    }
    
    # drop irrelevant columns
    crime_data <- as.data.frame(crime_data) %>%
        select(-c(Crime.ID, Reported.by, Falls.within, Context))
    
    # drop observations with no coordinates
    crime_data <- crime_data %>%
        filter(!is.na(Longitude) & !is.na(Latitude))

    # generate a unique identifier for each crime, given by 'year'-'row number' (have to do this since the crime.id in the data is occasionally missing)
    crime_data <- crime_data %>%
        mutate(crime_id = paste0(year, "-", row_number()))

    # export this as an excel file to be read into ArcGIS
    write_xlsx(crime_data, paste0("Crime and night tubes EXTRA DATA/london_crime_data-", year, ".xlsx"))
}