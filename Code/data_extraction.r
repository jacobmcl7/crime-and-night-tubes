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
rm(temp)
rm(year)

# drop the id column inserted by ArcGIS
cs_pairs <- cs_pairs %>%
    select(-c(OBJECTID))

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

# keep environment clean
rm(year)


# now merge the cs_pairs in, on the variable 'crime_id', in a one-to-many way
full_data <- merge(crime_data, cs_pairs, by = "crime_id", all.x = TRUE)



#################################################################
#################################################################
# full_data contains everything we want: we can now work with it to create the dataset for analysis
#################################################################
#################################################################


# crimes are recorded at finely grouped locations: reshape to get the number of crimes in each longitude-latitude pair by month
monthly_counts <- full_data %>%
    group_by(Month, Longitude, Latitude) %>%
    summarise(num_crimes = n()) %>%
    ungroup()

# now we need to add 0s for all months in which no crimes were recorded in given months for any observation

# first concatenate the longitude and latitude to make it just one variable
monthly_counts <- monthly_counts %>%
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude)) %>%

    # now for every combination of location and month that isn't in the data, add a row with num_crimes = 0
    complete(Month, location, fill = list(num_crimes = 0))


# now we need to get information for each location as to whether it is near a given tube station, and which one

# get all longitude-latitude-station_info combinations in the full_data
location_info <- full_data %>%
    select(c(Longitude, Latitude, NEAR_DIST, NAME, LINES)) %>%
    distinct() %>%

    # join longitude and latitude into one location variable, as before
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Longitude, Latitude))

    # note that lon+lat uniquely define the other three variables, since they are obtained through the geocoding which depends on lon+lat
    # thus no duplicate locations, unless they are within 500m of multiple stations (in which case, one for each station)

# now reshape the data to give one observation per location
# if an location has multiple observations with different names, near_dist and lines, make new variables name2, near_dist2 and lines2 that hold these new values
location_info <- location_info %>%
    # number each observation for every location, to give the suffixes when reshaping 
    group_by(location) %>%
    mutate(station_count = row_number()) %>%
    ungroup() %>%
    # now do the reshaping
    pivot_wider(names_from = station_count, values_from = c(NAME, NEAR_DIST, LINES), names_sep = "")


# now merge the monthly counts with the location info, on the location variable
final_data <- merge(monthly_counts, location_info, by = "location", all.x = TRUE) %>%
    arrange(location, Month) %>%

    # now concatenate all lines together, to then sort through to determine treatment
    unite("all_lines", starts_with("LINES"), sep = ", ", na.rm = TRUE)

    # now make a variable for whether within 500m of a station
    mutate(near_station == !is.na(NAME1)) %>%

    # now make one for whether within 500m of a station on each of the night tube lines (Central, Jubilee, Northern, Piccadilly, Victoria)
    mutate(near_station_central == str_detect(all_lines, "Central")) %>%
    mutate(near_station_jubilee == str_detect(all_lines, "Jubilee")) %>%
    mutate(near_station_northern == str_detect(all_lines, "Northern")) %>%
    mutate(near_station_piccadilly == str_detect(all_lines, "Piccadilly")) %>%
    mutate(near_station_victoria == str_detect(all_lines, "Victoria"))