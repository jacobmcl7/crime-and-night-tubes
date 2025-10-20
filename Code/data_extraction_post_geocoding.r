# now clean the location data we have just generated, and merge it into a suitably processed version of the crime data from before

library(readxl)
library(writexl)
library(tidyverse)

setwd("~/Economics/Papers (WIP)")

#######################################################################################


# first deal with the geocoded location information

# load in the first file of location-station pairs
ls_pairs <- as.data.frame(read_excel("Crime and night tubes EXTRA DATA/location_station_pairs_1.xlsx"))

# append them with the pairs from the next files
for (i in 2:3) {
    temp <- as.data.frame(read_excel(paste0("Crime and night tubes EXTRA DATA/location_station_pairs_", i, ".xlsx")))
    ls_pairs <- rbind(ls_pairs, temp)
}

# do some cleaning of the data
ls_pairs <- ls_pairs %>%
    # drop the id column inserted by ArcGIS
    select(-c(OBJECTID)) %>%

    # join longitude and latitude into one location variable, as before
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Longitude, Latitude))

# now reshape it to give one observation per location, with all the relevant station info in it
# if an location has multiple observations with different names, near_dist and lines, make new variables name2, near_dist2 and lines2 that hold these new values
location_info <- ls_pairs %>%
    # number each observation for every location, to give the suffixes when reshaping 
    group_by(location) %>%
    mutate(station_count = row_number()) %>%
    ungroup() %>%
    # now do the reshaping
    pivot_wider(names_from = station_count, values_from = c(NAME, NEAR_DIST, LINES), names_sep = "")




###############################################################




# now we need to clean the crime data to get monthly crime counts, then merge with the location info we just processed

# first load it in
load("Crime and night tubes EXTRA DATA/individual_crime_data.RData")

# first get the monthly crime count in each location
monthly_counts <- crime_data %>%
    group_by(Month, Longitude, Latitude) %>%
    summarise(num_crimes = n()) %>%
    ungroup()

# now clean it up some more
monthly_counts <- monthly_counts %>%
    
    # first concatenate the longitude and latitude to make it just one variable
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude)) %>%

    # now for every combination of location and month that isn't in the data, add a row with num_crimes = 0
    complete(Month, location, fill = list(num_crimes = 0))


# now merge the monthly counts with the location info, on the location variable
final_data <- merge(monthly_counts, location_info, by = "location", all.x = TRUE) %>%

    # arrange the data to make it look nicer
    arrange(location, Month) %>%
    
    # now adjust months from 2015-01 to 1, and increase in units of 1, and call this the period
    mutate(period = as.numeric(substr(Month, 6, 7)) + 12 * (as.numeric(substr(Month, 1, 4)) - 2015)) %>%

    # get the location, the month, the period and the number of crimes as the first four columns
    relocate(location, Month, period, num_crimes) %>%

    # save the data
    save(file = "Crime and night tubes EXTRA DATA/final_data_new.RData")

# this is our final dataset with all the info we need - it has:
# - crime count in each location with recorded crimes 
# - names and lines served by stations within 2km of each points
# - distance from each of the points to each of the stations within 2km of it

# from this we can define treatment, as we need to, and then run the appropriate regressions
# this is the key decision, which will be made (and varied) in the next R script



# we can also do the same with the types of crime specified as well, in exactly the same way but also grouping crimes on the type as well as the location and month

monthly_counts_crime_specific <- crime_data %>%
    group_by(Month, Longitude, Latitude, Crime.type) %>%
    summarise(num_crimes = n()) %>%
    ungroup() %>%
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude)) %>%
    complete(Month, location, fill = list(num_crimes = 0))


final_data_crime_specific <- merge(monthly_counts_crime_specific, location_info, by = "location", all.x = TRUE) %>%
    arrange(location, Month) %>%
    mutate(period = as.numeric(substr(Month, 6, 7)) + 12 * (as.numeric(substr(Month, 1, 4)) - 2015)) %>%
    relocate(location, Month, period, Crime.type, num_crimes) %>%
    save(file = "Crime and night tubes EXTRA DATA/final_data_crime_specific_new.RData")