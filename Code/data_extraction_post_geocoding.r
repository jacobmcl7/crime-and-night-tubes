# now clean the location data we have just generated, and merge it into a suitably processed version of the crime data from before

# in more detail, this script does the following:
# - cleans the location-station pair data to give, for each location, the names, lines served and distances of all stations within 2km of it
# - cleans the ward data to give, for each location, the ward code and name
# - processes the crime data to give monthly crime counts in each location
# - merges all of this together to give a final dataset with monthly crime counts and location information for each location
# - from this final dataset, calculates the minimum distance of each location to a station on each of the treated lines, and to any station at all
# - saves this final dataset for analysis in the next script

#######################################################################################

# load libraries
library(readxl)
library(writexl)
library(tidyverse)

# set working directory
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
    # give each location-station pair a number from 1 to the number of stations for that location
    mutate(station_count = row_number()) %>%
    ungroup() %>%
    # now do the reshaping, using these values
    pivot_wider(names_from = station_count, values_from = c(NAME, NEAR_DIST, LINES), names_sep = "")



# we now want to prepare the ward information for each location as well

# first load in the ward info we generated in the geocoding script
ward_info <- as.data.frame(read_excel("Crime and night tubes EXTRA DATA/ward_info_1.xlsx"))
for (i in 2:3) {
    temp <- as.data.frame(read_excel(paste0("Crime and night tubes EXTRA DATA/ward_info_", i, ".xlsx")))
    ward_info <- rbind(ward_info, temp)
}

# combine latitude and longitude, and drop the id column, as before
ward_info <- ward_info %>%
    # drop the id column inserted by ArcGIS
    select(-c(OBJECTID)) %>%

    # join longitude and latitude into one location variable, as before
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Longitude, Latitude))

# this data will be merged in later




###############################################################



# now we need to clean the crime data to get monthly crime counts

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



# we can also get counts of specific types of crime, in the same way as above

# make a new dataset giving crime counts in each location-month for each of a specific subset of crimes
monthly_counts_type <- crime_data %>%

    # choose the crimes of interest
    filter(Crime.type %in% c("Theft from the person", "Burglary", "Shoplifting")) %>%

    # clean as before
    group_by(Month, Longitude, Latitude, Crime.type) %>%
    summarise(num_crimes = n()) %>%
    ungroup() %>%

    # reshape wide to get variables for each crime type
    pivot_wider(names_from = Crime.type, values_from = num_crimes, values_fill = list(num_crimes = 0)) %>%
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude))







##############################################################

# now merge all the data together, and do some cleaning to make the final dataset


# first merge the monthly counts with the location info, on the location variable
final_data <- merge(monthly_counts, location_info, by = "location", all.x = TRUE)

# also merge in the ward info from above too, in the same way
final_data <- merge(final_data, ward_info, by = "location", all.x = TRUE)

# now merge in the counts of each crime type as well - now on both location and month
final_data <- merge(final_data, monthly_counts_type, by = c("location", "Month"), all.x = TRUE)

# now do some final cleaning
final_data <- final_data %>%

    # arrange the data to make it look nicer
    arrange(location, Month) %>%
    
    # now adjust months from 2015-01 to 1, and increase in units of 1, and call this the period
    mutate(period = as.numeric(substr(Month, 6, 7)) + 12 * (as.numeric(substr(Month, 1, 4)) - 2015)) %>%

    # get the location, the month, the period, the number of crimes, and the ward info, as the first columns
    relocate(location, Month, period, num_crimes, WD24CD, WD24NM, Burglary, `Shoplifting`, `Theft from the person`) %>%

    # replace NAs in the crime type counts with 0s
    mutate(Burglary = ifelse(is.na(Burglary), 0, Burglary),
           `Shoplifting` = ifelse(is.na(`Shoplifting`), 0, `Shoplifting`),
           `Theft from the person` = ifelse(is.na(`Theft from the person`), 0, `Theft from the person`)) %>%

    # include a log of crime count + 1, as the count data is heavily rightward skewed but has zeros (as done in e.g. Christensen et al (2024))
    mutate(log_num_crimes = log(1 + num_crimes))



# now get the minimum distance of each station from a tube station on each of the lines of interest, if they are within 2km
# we will do this for the 5 treated lines, and then for any line at all

# first reshape the data to make it easier to work with: get observations corresponding to each location-station pair
min_dist_determination <- final_data %>%
  filter(Month == "2015-01") %>%  # just need one month's observations
  select(location, starts_with("LINES"), starts_with("NEAR_DIST")) %>%
  pivot_longer(
    cols = -location,
    names_to = c(".value", "n"),
    names_pattern = "(LINES|NEAR_DIST)(\\d+)"
  )


# now get the min distance for the central line
min_dist_central <- min_dist_determination %>%
  # filter only rows where LINES is not NA and contains "Central"
  filter(!is.na(LINES), grepl("Central", LINES, fixed = TRUE)) %>%
  # now get the minimum distance for each location
  group_by(location) %>%
  summarise(min_central_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

# this now gives the minimum distance to a central line station for each location within 2km of a central line station

# merge back into the data
final_data <- final_data %>%
  left_join(min_dist_central, by = "location")

# the locations that aren't within 2km of a central line station will have NA in this variable - no problem


# do the same for all other treated lines: first Jubilee
min_dist_jubilee <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Jubilee", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  summarise(min_jubilee_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
  left_join(min_dist_jubilee, by = "location")

# now Piccadilly
min_dist_piccadilly <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Piccadilly", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  summarise(min_piccadilly_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
  left_join(min_dist_piccadilly, by = "location")

# now Victoria
min_dist_victoria <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Victoria", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  summarise(min_victoria_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
  left_join(min_dist_victoria, by = "location")

# now Northern
min_dist_northern <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Northern", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  summarise(min_northern_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
  left_join(min_dist_northern, by = "location")

# now get the min distance to any station
min_dist_any <- min_dist_determination %>%
  filter(!is.na(LINES)) %>% # may not need to filter
  group_by(location) %>%
  summarise(min_any_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
  left_join(min_dist_any, by = "location")


# save the data
save(final_data, file = "Crime and night tubes EXTRA DATA/final_data_new.RData")

# this is our final dataset with all the info we need - it has:
# - crime count in each location with recorded crimes 
# - names and lines served by stations within 2km of each points
# - distance from each of the points to each of the stations within 2km of it

# from this we can define treatment, as we need to, and then run the appropriate regressions
# this is the key decision, which will be made (and varied) in the next R script