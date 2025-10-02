# this extracts the data to create one large dataset of dated crimes that can be geocoded

library(openxlsx)
library(tidyverse)

setwd("~/Economics/Papers (WIP)/Crime and night tubes")

# load in the first month of 2015 data
crime_data <- read.csv("Data/2015-01/2015-01-metropolitan-street.csv", stringsAsFactors = FALSE)

# merge with the rest of the data
for (i in 2:12) {
    # get the correct month format
    month <- ifelse(i < 10, paste0("0", i), i)
    # read in the data
    temp_data <- read.csv(paste0("Data/2015-", month, "/2015-", month, "-metropolitan-street.csv"), stringsAsFactors = FALSE)
    # merge with the main dataset
    crime_data <- rbind(crime_data, temp_data)
}  

for (j in 2016:2017) {
    for (i in 1:12) {
        # get the correct month format
        month <- ifelse(i < 10, paste0("0", i), i)
        temp_data <- read.csv(paste0("Data/", j, "-", month, "/", j, "-", month, "-metropolitan-street.csv"), stringsAsFactors = FALSE)
        crime_data <- rbind(crime_data, temp_data)
    }  
}

# drop irrelevant columns
crime_data <- as.data.frame(crime_data) %>%
    select(-c(Crime.ID, Reported.by, Falls.within, Location, Context))

# drop observations with no coordinates
crime_data <- crime_data %>%
    filter(!is.na(Longitude) & !is.na(Latitude))

# export this as an excel file to be read into ArcGIS
write.xlsx(crime_data, 'Data/london_crime_data.xlsx')

# note: this exports strangely. We have to open it in excel and re-save it to allow ArcGIS to read it in properly.