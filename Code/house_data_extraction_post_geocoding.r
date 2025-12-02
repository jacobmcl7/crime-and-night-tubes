# this file does the post-processing of the house data after geocoding

# load libraries
library(tidyverse)

# set working directory
setwd("~/Economics/Papers (WIP)")

# import the geocoded house data
house_data_geocoded <- read_csv("Crime and night tubes EXTRA DATA/house_data_geocoded.csv")

# first some preliminary cleaning
house_data <- house_data_geocoded %>%

    # keep only those for which the match was perfect
    filter(Score == 100) %>%

    # keep only relevant columns
    select(c(IN_FID, NEAR_FID, NEAR_DIST, USER_V2, USER_V3, USER_V4, NAME, LINES))


# now extract the fundamental characteristics of each house sale
distinct_sales <- house_data_geocoded %>%

    # keep only relevant columns
    select(c(IN_FID, USER_V2, USER_V3, USER_V4)) %>%

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
# - get a month variable, for comparability with crime data
# - get the lines of each of the stations processed as in the original processing script



# save the cleaned dataset
save(house_data, file = "Crime and night tubes EXTRA DATA/house_data_cleaned.RData")