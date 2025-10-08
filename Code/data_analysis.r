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


##############################################################################

# now do it dynamically, in the standard TWFE way

final_data <- final_data %>%
    
    # first get a variable giving the period of first treatment
    mutate(first_treatment = 1000) %>%
    mutate(first_treatment = ifelse(!is.na(min_piccadilly_dist), 24, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_northern_dist), 23, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_jubilee_dist), 22, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_victoria_dist), 20, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_central_dist), 20, first_treatment)) %>%

    # now get the event-times
    mutate(event_time = case_when(
        first_treatment < 1000 ~ period - first_treatment,
        first_treatment == 1000 ~ -1
    ))

# now do the regression
TWFE <- feols(num_crimes ~ i(event_time, ref = -1) | location + Month, data = final_data)


# plot the coefficients in ggplot
event_time_coefs <- coef(TWFE)[grep("event_time::", names(coef(TWFE)))]
event_time_se <- se(TWFE)[grep("event_time::", names(se(TWFE)))]
event_time_df <- data.frame(
    event_time = as.numeric(gsub("event_time::", "", names(event_time_coefs))),
    coef = event_time_coefs,
    se = event_time_se
)
# Add event_time = -1 with coef = 0 and se = 0
event_time_df <- rbind(event_time_df, data.frame(event_time = -1, coef = 0, se = 0))
event_time_df <- event_time_df[order(event_time_df$event_time), ]

# plot the graph
ggplot(event_time_df, aes(x = event_time, y = coef)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se), 
                alpha = 0.1, fill = "blue", color = scales::alpha("blue", 0.3)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(-20, 15, 5)) +
    labs(title = "Event Study",
         x = "Event Time (Months Since Treatment)",
         y = "Coefficient on Event Time") +
    theme_minimal()

# no evidence for PT