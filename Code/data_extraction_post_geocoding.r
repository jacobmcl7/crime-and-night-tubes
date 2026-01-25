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


# now extract the relevant station information about each location
# do this as follows: get the minimum distance of each location from a tube station on each of the lines of interest, if they are within 2km
# we will do this for the 5 treated lines, and then for any line at all

# first get the min distance for each location from a stop on the central line
min_dist_central <- ls_pairs %>%
  # filter only rows where LINES contains "Central"
  filter(grepl("Central", LINES, fixed = TRUE)) %>%
  # now for each location, get the minimum distance and the corresponding station
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_central_dist = NEAR_DIST,
         central_station = NAME) %>%
  select(location, min_central_dist, central_station)

# this data will be combined with the remainder of the lines later on

# now do the same for all other treated lines: first Jubilee
min_dist_jubilee <- ls_pairs %>%
  filter(grepl("Jubilee", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_jubilee_dist = NEAR_DIST,
         jubilee_station = NAME) %>%
  select(location, min_jubilee_dist, jubilee_station)

# now Piccadilly
min_dist_piccadilly <- ls_pairs %>%
  filter(grepl("Piccadilly", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_piccadilly_dist = NEAR_DIST,
         piccadilly_station = NAME) %>%
  select(location, min_piccadilly_dist, piccadilly_station)

# now Victoria
min_dist_victoria <- ls_pairs %>%
  filter(grepl("Victoria", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_victoria_dist = NEAR_DIST,
         victoria_station = NAME) %>%
  select(location, min_victoria_dist, victoria_station)

# now Northern
min_dist_northern <- ls_pairs %>%
  filter(grepl("Northern", LINES, fixed = TRUE)) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_northern_dist = NEAR_DIST,
         northern_station = NAME) %>%
  select(location, min_northern_dist, northern_station)

# now do it for any station at all
min_dist_any <- ls_pairs %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  # we will also keep the line on which the closest station lies
  rename(min_any_dist = NEAR_DIST,
         closest_station = NAME,
         closest_line = LINES) %>%
  select(location, min_any_dist, closest_station, closest_line)

# now merge all of these minimum distance datasets together
location_info <- min_dist_any %>%
  left_join(min_dist_central, by = "location") %>%
  left_join(min_dist_jubilee, by = "location") %>%
  left_join(min_dist_piccadilly, by = "location") %>%
  left_join(min_dist_victoria, by = "location") %>%
  left_join(min_dist_northern, by = "location")


# it is also important to note that not all stations on these lines are night tube stations
# so we will also create variables giving the minimum distance to a night tube station on each line

# first, create vectors of the stations on each night tube line not served by the night tube
central_line_no_night_tube <- c("West Ruislip", "Ruislip Gardens", "South Ruislip", "Northolt", "Greenford", "Perivale", "Hanger Lane", "Debden", "Theydon Bois", "Epping", "Grange Hill", "Chigwell", "Roding Valley")
northern_line_no_night_tube <- c("Mill Hill East", "Nine Elms", "Battersea Power Station", "King's Cross St. Pancras", "Angel", "Old Street", "Moorgate", "Bank", "London Bridge", "Borough", "Elephant & Castle")
piccadilly_line_no_night_tube <- c("Heathrow Terminal 4", "Uxbridge", "Hillingdon", "Ickenham", "Ruislip", "Ruislip Manor", "Eastcote", "Rayners Lane", "South Harrow", "Sudbury Hill", "Sudbury Town", "Alperton", "Park Royal", "North Ealing", "Ealing Common")

# now adjust the ls_pairs dataset to exclude these stations when calculating minimum distances
# first copy over a new variable that keeps the observation only if the line is one of the night tube lines and the station isn't in the no night tube list
ls_pairs <- ls_pairs %>%
    mutate(night_tube = case_when(
        grepl("Jubilee", LINES, fixed = TRUE) ~ TRUE,
        grepl("Victoria", LINES, fixed = TRUE) ~ TRUE,
        grepl("Central", LINES, fixed = TRUE) & !(NAME %in% central_line_no_night_tube) ~ TRUE,
        grepl("Northern", LINES, fixed = TRUE) & !(NAME %in% northern_line_no_night_tube) ~ TRUE,
        grepl("Piccadilly", LINES, fixed = TRUE) & !(NAME %in% piccadilly_line_no_night_tube) ~ TRUE,
        TRUE ~ FALSE
    ))

# now repeat the minimum distance calculations, but only for night tube stations

# first Central
min_nt_dist_central <- ls_pairs %>%
  # filter only rows where LINES contains "Central" and night_tube is TRUE
  filter(grepl("Central", LINES, fixed = TRUE) & night_tube) %>%
  # now for each location, get the minimum distance from a night tube station and the corresponding station
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_nt_central_dist = NEAR_DIST,
          central_nt_station = NAME) %>%
  select(location, min_nt_central_dist, central_nt_station)

# now Jubilee
min_nt_dist_jubilee <- ls_pairs %>%
  filter(grepl("Jubilee", LINES, fixed = TRUE) & night_tube) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_nt_jubilee_dist = NEAR_DIST,
         jubilee_nt_station = NAME) %>%
  select(location, min_nt_jubilee_dist, jubilee_nt_station)

# now Piccadilly
min_nt_dist_piccadilly <- ls_pairs %>%
  filter(grepl("Piccadilly", LINES, fixed = TRUE) & night_tube) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_nt_piccadilly_dist = NEAR_DIST,
         piccadilly_nt_station = NAME) %>%
  select(location, min_nt_piccadilly_dist, piccadilly_nt_station)

# now Victoria
min_nt_dist_victoria <- ls_pairs %>%
  filter(grepl("Victoria", LINES, fixed = TRUE) & night_tube) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_nt_victoria_dist = NEAR_DIST,
         victoria_nt_station = NAME) %>%
  select(location, min_nt_victoria_dist, victoria_nt_station)

# now Northern
min_nt_dist_northern <- ls_pairs %>%
  filter(grepl("Northern", LINES, fixed = TRUE) & night_tube) %>%
  group_by(location) %>%
  filter(NEAR_DIST == min(NEAR_DIST, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(min_nt_northern_dist = NEAR_DIST,
         northern_nt_station = NAME) %>%
  select(location, min_nt_northern_dist, northern_nt_station)

# now merge all of these minimum distance datasets together with location info
location_info <- location_info %>%
  left_join(min_nt_dist_central, by = "location") %>%
  left_join(min_nt_dist_jubilee, by = "location") %>%
  left_join(min_nt_dist_piccadilly, by = "location") %>%
  left_join(min_nt_dist_victoria, by = "location") %>%
  left_join(min_nt_dist_northern, by = "location")

###############################################################



# now we need to clean the crime data to get monthly crime counts

# first load it in
load("Crime and night tubes EXTRA DATA/individual_crime_data.RData")

# do some cleaning
monthly_counts <- crime_data %>%

    # first get the monthly crime count in each location
    group_by(Month, Longitude, Latitude) %>%
    summarise(num_crimes = n()) %>%
    ungroup() %>%
    
    # now concatenate the longitude and latitude to make it just one variable
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude)) %>%

    # now for every combination of location and month that isn't in the data, add a row with num_crimes = 0
    complete(Month, location, fill = list(num_crimes = 0))



# we can also get counts of specific types of crime, in the same way as above

# make a new dataset giving crime counts in each location-month for each of a specific subset of crimes
monthly_counts_type <- crime_data %>%

    # # choose the crimes of interest
    # filter(Crime.type %in% c("Theft from the person", "Burglary", "Shoplifting", "Robbery")) %>%

    # clean as before
    group_by(Month, Longitude, Latitude, Crime.type) %>%
    summarise(num_crimes = n()) %>%
    ungroup() %>%

    # reshape wide to get variables for each crime type
    pivot_wider(names_from = Crime.type, values_from = num_crimes, values_fill = list(num_crimes = 0)) %>%
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude)) %>%

    # make the crime column names lower case and underscores instead of spaces
    rename_with(~ gsub(" ", "_", .x), -c(Month, location)) %>%
    rename_with(tolower, -c(Month, location))




# also now get a set of outcomes for each location - first reload in the data
load("Crime and night tubes EXTRA DATA/individual_crime_data.RData")

# rename the outcome variable
crime_data <- crime_data %>%
    rename(outcome = Last.outcome.category)

# process the outcome variable - was the offender identified or not?
# note - this variable is sometimes missing, exactly when the crime is antisocial behaviour
# since we aren't interested in this, it doesn't matter how we deal with this variable - it just goes into 'unclear'
crime_data <- crime_data %>%
    mutate(offender_identified = case_when(
        outcome %in% c("Investigation complete; no suspect identified", "Unable to prosecute suspect") ~ "No",
        outcome %in% c("", "Formal action is not in the public interest", "Further investigation is not in the public interest", "Status update unavailable", "Under investigation") ~ "Unclear",
        TRUE ~ "Yes"
    )) %>%

    # also change the location variable, as before
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Latitude, Longitude))

# now get the counts of specific crimes and their outcomes in each location-month
monthly_counts_outcome_all <- crime_data %>%

    # clean as before
    group_by(Month, location, offender_identified) %>%
    summarise(num_crimes = n()) %>%
    ungroup() %>%

    # reshape wide to get variables for each outcome type
    pivot_wider(names_from = offender_identified, values_from = num_crimes, values_fill = list(num_crimes = 0)) %>%

    # rename variables
    rename(outcome_no_all = No,
           outcome_yes_all = Yes,
           outcome_unclear_all = Unclear)

# now get the counts of specific crimes and their outcomes in each location-month
monthly_counts_outcome_type <- crime_data %>%

    # filter only the crimes of interest
    filter(Crime.type %in% c("Theft from the person", "Robbery")) %>%

    # now exactly as before, but splitting by crime type as well

    # clean as before
    group_by(Month, location, Crime.type, offender_identified) %>%
    summarise(num_crimes = n()) %>%
    ungroup() %>%

    # reshape wide to get variables for each outcome type
    pivot_wider(names_from = c(offender_identified, Crime.type), values_from = num_crimes, values_fill = list(num_crimes = 0)) %>%

    # rename variables
    rename(outcome_no_robbery = No_Robbery,
           outcome_yes_robbery = Yes_Robbery,
           outcome_unclear_robbery = Unclear_Robbery,
           outcome_no_theft_from_the_person = `No_Theft from the person`,
           outcome_yes_theft_from_the_person = `Yes_Theft from the person`,
           outcome_unclear_theft_from_the_person = `Unclear_Theft from the person`)

##############################################################

# now merge all the data together, and do some cleaning to make the final dataset


# first merge the monthly counts with the location info, on the location variable
final_data <- merge(monthly_counts, location_info, by = "location", all.x = TRUE)

# now merge in the counts of each crime type as well - now on both location and month
final_data <- merge(final_data, monthly_counts_type, by = c("location", "Month"), all.x = TRUE)

# also merge in the counts of outcomes
final_data <- merge(final_data, monthly_counts_outcome_all, by = c("location", "Month"), all.x = TRUE)
final_data <- merge(final_data, monthly_counts_outcome_type, by = c("location", "Month"), all.x = TRUE)

# now do some final cleaning
final_data <- final_data %>%

    # arrange the data to make it look nicer
    arrange(location, Month) %>%
    
    # now adjust months from 2015-01 to 1, and increase in units of 1, and call this the period
    mutate(period = as.numeric(substr(Month, 6, 7)) + 12 * (as.numeric(substr(Month, 1, 4)) - 2015))

# now deal with the crime counts - first make a vector of each crime type, to make this easier
crime_types <- colnames(monthly_counts_type)[!(colnames(monthly_counts_type) %in% c("Month", "location"))]
    
# now replace NAs in the crime type counts with 0s
final_data <- final_data %>%

    # for each crime type, replace NAs with 0s
    mutate(across(all_of(crime_types), ~ ifelse(is.na(.), 0, .))) %>%

    # do the same for the outcome counts
    mutate(across(starts_with("outcome_"), ~ ifelse(is.na(.), 0, .))) %>%

    # now include a log of crime count + 1, for each crime type specifically
    mutate(across(all_of(crime_types), ~ log(1 + .), .names = "log_{.col}")) %>%

    # same for the overall crime count
    mutate(log_num_crimes = log(1 + num_crimes)) %>%

    # finally get the location, the month, the period, and the number of crimes as the first columns
    relocate(location, Month, period, num_crimes, log_num_crimes, all_of(crime_types), starts_with("log_"))




##############################################################

# finally, get the controls in

# first load in the ward and msoa data
locations <- as.data.frame(read_excel("Crime and night tubes EXTRA DATA/lsoa_msoa_ward_info_1.xlsx"))

# append them with the pairs from the next files
for (i in 2:3) {
    temp <- as.data.frame(read_excel(paste0("Crime and night tubes EXTRA DATA/lsoa_msoa_ward_info_", i, ".xlsx")))
    locations <- rbind(locations, temp)
}

# do some cleaning of the data
locations <- locations %>%
    # drop the id column inserted by ArcGIS
    select(-c(OBJECTID)) %>%

    # join longitude and latitude into one location variable, as before
    mutate(location = paste0(Latitude, ", ", Longitude)) %>%
    select(-c(Longitude, Latitude))


# now import the IMD data
imd_data <- as.data.frame(read_excel("Crime and night tubes EXTRA DATA/IMD data/File_2_ID_2015_Domains_of_deprivation.xlsx", sheet = 2))

# rename some columns for ease of use (column 5)
colnames(imd_data)[5] <- "IMD"
colnames(imd_data)[6] <- "IMD_decile"

# merge this with the locations data, on the LSOA code
locations <- locations %>%
    left_join(imd_data %>% select(`LSOA code (2011)`, `LSOA name (2011)`, IMD, IMD_decile), by = c("LSOA11CD" = "LSOA code (2011)"))

# some NAs - poor geocoding, so maybe drop them


# merge with the final data, on location
final_data <- final_data %>%
    left_join(locations %>% select(location, WD24NM, MSOA21NM, LSOA11NM, IMD, IMD_decile), by = "location")

##############################################################

# the cleaning is done - now save the final dataset
save(final_data, file = "Crime and night tubes EXTRA DATA/final_data.RData")

# this is our final dataset with all the info we need - it has:
# - crime count in each location with recorded crimes 
# - names and lines served by stations within 2km of each points, appropriately processed for analysis

# from this we can define treatment, as we need to, and then run the appropriate regressions
# this is the key decision, which will be made (and varied) in the next R script


##############################################################

# note our controls:
# distance to nearest station - min_any_dist