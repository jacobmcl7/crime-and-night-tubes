# this file does the post-processing of the house data after geocoding

# load libraries
library(tidyverse)

# set working directory
setwd("~/Economics/Papers (WIP)")

# import the geocoded house data
house_data_geocoded <- read_csv("Crime and night tubes EXTRA DATA/house_data_geocoded.csv")
# note: four observations give weird warnings - nothing major, but work out why!

# first some preliminary cleaning
house_data_geocoded <- house_data_geocoded %>%

    # keep only those for which the match was perfect
    filter(Score == 100) %>%

    # keep only relevant columns
    select(c(IN_FID, NEAR_FID, NEAR_DIST, USER_V2, USER_V3, USER_V4, NAME, LINES)) %>%

    # generate a month variable
    mutate(month = paste0(substr(USER_V3, 4, 5), "-", substr(USER_V3, 7, 10))) %>%

    # generate a period variable
    mutate(period = (as.numeric(substr(USER_V3, 7, 10)) - 2015) * 12 + as.numeric(substr(USER_V3, 4, 5))) %>%

    # generate a postcode sector variable
    mutate(pcsect = substr(USER_V4, 1, nchar(USER_V4) - 3)) %>%

    # generate a log price variable
    mutate(log_price = log(as.numeric(USER_V2)))



# now extract the fundamental characteristics of each house sale
distinct_sales <- house_data_geocoded %>%

    # keep only relevant columns
    select(c(IN_FID, USER_V2, USER_V3, USER_V4, month, period, log_price, pcsect)) %>%

    # keep only distinct observations
    distinct(IN_FID, .keep_all = TRUE)


# now extract, for each house sale, the info for each nearby station that was matched, all in one observation
station_data <- house_data_geocoded %>%

    # keep only relevant columns
    select(c(IN_FID, NEAR_DIST, NAME, LINES)) %>%

    # number each observation for every house sale, to give the suffixes when reshaping 
    group_by(IN_FID) %>%

    # give each sale-station pair a number from 1 to the number of stations for that location
    mutate(station_count = row_number()) %>%
    ungroup() %>%

    # now do the reshaping, using these values
    pivot_wider(names_from = station_count, values_from = c(NAME, NEAR_DIST, LINES), names_sep = "")


# now merge the two datasets together
house_data <- distinct_sales %>%

    # merge in the station data
    left_join(station_data, by = "IN_FID")



# now do the following:
# - get the lines of each of the stations processed as in the original processing script


# find the minimum distances, as in the previous script

# first reshape the data to make it easier to work with: get observations corresponding to each location-station pair
min_dist_determination <- house_data %>%
  select(IN_FID, starts_with("LINES"), starts_with("NEAR_DIST")) %>%
  pivot_longer(
    cols = -IN_FID,
    names_to = c(".value", "n"),
    names_pattern = "(LINES|NEAR_DIST)(\\d+)"
  )


# now get the min distance for the central line
min_dist_central <- min_dist_determination %>%
  # filter only rows where LINES is not NA and contains "Central"
  filter(!is.na(LINES), grepl("Central", LINES, fixed = TRUE)) %>%
  # now get the minimum distance for each location
  group_by(IN_FID) %>%
  summarise(min_central_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

# this now gives the minimum distance to a central line station for each location within 2km of a central line station

# merge back into the data
house_data <- house_data %>%
  left_join(min_dist_central, by = "IN_FID")

# the locations that aren't within 2km of a central line station will have NA in this variable - no problem

# do the same for all other treated lines: first Jubilee
min_dist_jubilee <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Jubilee", LINES, fixed = TRUE)) %>%
  group_by(IN_FID) %>%
  summarise(min_jubilee_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

house_data <- house_data %>%
  left_join(min_dist_jubilee, by = "IN_FID")

# now Piccadilly
min_dist_piccadilly <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Piccadilly", LINES, fixed = TRUE)) %>%
  group_by(IN_FID) %>%
  summarise(min_piccadilly_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

house_data <- house_data %>%
  left_join(min_dist_piccadilly, by = "IN_FID")

# now Victoria
min_dist_victoria <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Victoria", LINES, fixed = TRUE)) %>%
  group_by(IN_FID) %>%
  summarise(min_victoria_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

house_data <- house_data %>%
  left_join(min_dist_victoria, by = "IN_FID")

# now Northern
min_dist_northern <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Northern", LINES, fixed = TRUE)) %>%
  group_by(IN_FID) %>%
  summarise(min_northern_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

house_data <- house_data %>%
  left_join(min_dist_northern, by = "IN_FID")

# now get the min distance to any station
min_dist_any <- min_dist_determination %>%
  filter(!is.na(LINES)) %>% # may not need to filter
  group_by(IN_FID) %>%
  summarise(min_any_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

house_data <- house_data %>%
  left_join(min_dist_any, by = "IN_FID")

# save the cleaned dataset
save(house_data, file = "Crime and night tubes EXTRA DATA/house_data_cleaned.RData")