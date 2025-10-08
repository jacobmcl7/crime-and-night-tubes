# this file does the analysis

library(tidyverse)

setwd("~/Economics/Papers (WIP)")

load("Crime and night tubes EXTRA DATA/final_data_new.RData")


# we need to get the minimum distance of each station from a tube station on each of the lines

# to do this, first create a reshaped dataset with an ID for later merging

final_data <- final_data %>%
  mutate(location_id = row_number())  # Create ID to rejoin later

min_dist_determination <- final_data %>%
  select(location_id, starts_with("LINES"), starts_with("NEAR_DIST")) %>%
  pivot_longer(
    cols = -location_id,
    names_to = c(".value", "n"),
    names_pattern = "(LINES|NEAR_DIST)(\\d+)"
  )


# now get the min distance for the central line
min_dist_central <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Central", LINES, fixed = TRUE)) %>%
  group_by(location_id) %>%
  summarise(min_central_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

# merge back into the data
final_data <- final_data %>%
  left_join(min_dist_central, by = "location_id")


# do the same for all other treated lines
min_dist_jubilee <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Jubilee", LINES, fixed = TRUE)) %>%
  group_by(location_id) %>%
  summarise(min_jubilee_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
left_join(min_dist_jubilee, by = "location_id")


min_dist_piccadilly <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Piccadilly", LINES, fixed = TRUE)) %>%
  group_by(location_id) %>%
  summarise(min_piccadilly_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
left_join(min_dist_piccadilly, by = "location_id")


min_dist_victoria <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Victoria", LINES, fixed = TRUE)) %>%
  group_by(location_id) %>%
  summarise(min_victoria_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
left_join(min_dist_victoria, by = "location_id")


min_dist_northern <- min_dist_determination %>%
  filter(!is.na(LINES), grepl("Northern", LINES, fixed = TRUE)) %>%
  group_by(location_id) %>%
  summarise(min_northern_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
left_join(min_dist_northern, by = "location_id")

# now get the min distance to any station
min_dist_any <- min_dist_determination %>%
  filter(!is.na(LINES)) %>% # may not need to filter
  group_by(location_id) %>%
  summarise(min_any_dist = min(NEAR_DIST, na.rm = TRUE), .groups = "drop")

final_data <- final_data %>%
left_join(min_dist_any, by = "location_id")



# check this all works!!!!!!!! THIS WAS DONE IN A RUSH


# save the data
save(final_data, file = "Crime and night tubes EXTRA DATA/final_data__for_analysis.RData")








############################################################
############################################################
# Now do some analysis
############################################################
############################################################


# import relevant packages
library(fixest)


# keep only the observations within 2km of a tube station
final_data <- final_data %>%
    filter(!is.na(min_any_dist))


# define treatment: for now, call a region treated if it is within 2km of an active night tube station
final_data <- final_data %>%
     mutate(treatment = ifelse((!is.na(min_central_dist) & period >= 20) |
                                  (!is.na(min_jubilee_dist) == 1 & period >= 22) |
                                  (!is.na(min_northern_dist) == 1 & period >= 23) |
                                  (!is.na(min_piccadilly_dist) == 1 & period >= 24) |
                                  (!is.na(min_victoria_dist) == 1 & period >= 20), 
                                  1,
                                  0))
    # note that the first treatment months of each station are:
    # Central: 19 Aug 2016 (first treatment month = 12 + 8 = 20)
    # Victoria: 19 Aug 2016 (ftm = 20)
    # Jubilee: 7 Oct 2016 (ftm = 22)
    # Northern: 18 Nov 2016 (ftm = 23)
    # Piccadilly: 16 Dec 2016 (ftm = 24)


# do a basic regression
simple_did <- feols(num_crimes ~ treatment | location + Month, data = final_data)
summary(simple_did)

# crimes increase!