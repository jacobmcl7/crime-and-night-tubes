# this extracts the data to create one large dataset of dated crimes that can be geocoded

library(readxl)
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




#####################################################################
#####################################################################
# THE ARCGIS PROCESSING HAPPENS HERE
#####################################################################
#####################################################################



# now merge together the files produced by ArcGIS

# load in the 2015 crime-station pairs
cs_pairs <- as.data.frame(read_excel("Crime and night tubes EXTRA DATA/crime_station_pairs_2015.xlsx"))

# append them with the pairs from the later years
for (year in 2016:2017) {
    temp <- as.data.frame(read_excel(paste0("Crime and night tubes EXTRA DATA/crime_station_pairs_", year, ".xlsx")))
    cs_pairs <- rbind(cs_pairs, temp)
}

# clear the extras from the environment
rm(c(temp, year))

# drop the id column inserted by ArcGIS, and save the dataset
cs_pairs <- cs_pairs %>%
    select(-c(OBJECTID)) %>%
    save("Crime and night tubes EXTRA DATA/cs_pairs.Rda")

# this is now ready to be merged in



# now deal with the station data, coming from crime_data

# first read it in and combine it in the same way
crime_data <- as.data.frame(read_excel("Crime and night tubes EXTRA DATA/london_crime_data-2015.xlsx"))

# append it with the other years, as before
for (year in 2016:2017) {
    temp <- as.data.frame(read_excel(paste0("Crime and night tubes EXTRA DATA/london_crime_data-", year, ".xlsx")))
    crime_data <- rbind(crime_data, temp)
    rm(temp)
}


# now merge the cs_pairs in, on the variable 'crime_id', in a one-to-many way
full_data <- merge(cs_pairs, crime_data, by = "crime_id", all.x = TRUE)