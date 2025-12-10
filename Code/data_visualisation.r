# this script does some data visualisation and other descriptive statistics

# import relevant packages
library(fixest)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(vtable)

# set working directory
setwd("~/Economics/Papers (WIP)")

# load in the data
load("Crime and night tubes EXTRA DATA/final_data.RData")
load("Crime and night tubes EXTRA DATA/house_data_cleaned.RData")


########################################################
# define some functions
# NOTE - MOVE THESE ALL TO POST-GEOCODING SCRIPT?
########################################################

# first a function that prepares the regression results for plotting of event-study coefficients
# the input to this function will be the output of a regression done using the 'feols' package
plot_prepare <- function(results, substring) {
  event_time_coefs <- coef(results)[grep(paste0(substring, "::"), names(coef(results)))]
  event_time_se <- se(results)[grep(paste0(substring, "::"), names(se(results)))]
  event_time_df <- data.frame(
    event_time = as.numeric(gsub(paste0(substring, "::"), "", names(event_time_coefs))),
    coef = event_time_coefs,
    se = event_time_se
  )
  # Add event_time = -1 with coef = 0 and se = 0
  event_time_df <- rbind(event_time_df, data.frame(event_time = -1, coef = 0, se = 0))
  event_time_df <- event_time_df[order(event_time_df$event_time), ]

  return(event_time_df)

}


# another one:
plot_prepare2 <- function(results, omitted_pd) {
  # get the coefficients from the regression
  event_time_df <- as.data.frame(results$coeftable[, 1:2])

  # make a new column that gives the characters after the :: part of the row name
  event_time_df$event_time <- as.numeric(sub(".*::", "", rownames(event_time_df)))

  # edit column names
  colnames(event_time_df) <- c("coef", "se", "event_time")

  # insert a row for the reference period
  event_time_df <- rbind(event_time_df, data.frame(event_time = omitted_pd, coef = 0, se = 0))
  event_time_df <- event_time_df[order(event_time_df$event_time), ]

  return(event_time_df)

}


# now create a function that plots the graph, using ggplot
plot <- function(coefs, xsequence, ymax, ymin, title, note = "") {
  ggplot(coefs, aes(x = event_time, y = coef)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se), 
                alpha = 0.1, fill = "blue", color = scales::alpha("blue", 0.3)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = xsequence) +
    ylim(ymin, ymax) +
    labs(title = title,
          x = "Event Time (Months Since Treatment)",
          y = "Coefficient on Event Time",
          caption = note) +
    theme_minimal()
}


# now define a function that defines treatment and event time variables, given a distance threshold
# specifically, we define a location as treated if it is within 'dist' km of an active night tube station
define_treatment_event_time <- function(distance, data) {

  # first create dynamic column names including the distance
  treatment_col <- paste0("treatment_", distance)
  first_treatment_col <- paste0("first_treatment_", distance)
  event_time_col <- paste0("event_time_", distance)

  # now create the required variables
  data <- data %>%

    # note that the first treatment months of each station are:
    # Central: 19 Aug 2016 (first treatment month = 12 + 8 = 20)
    # Victoria: 19 Aug 2016 (ftm = 20)
    # Jubilee: 7 Oct 2016 (ftm = 22)
    # Northern: 18 Nov 2016 (ftm = 23)
    # Piccadilly: 16 Dec 2016 (ftm = 24)
    # we therefore define the current treatment status variable as follows
    mutate(!!treatment_col := ifelse(
      (!is.na(min_central_dist) & min_central_dist < distance & period >= 20) |
      (!is.na(min_jubilee_dist) & min_jubilee_dist < distance & period >= 22) |
      (!is.na(min_northern_dist) & min_northern_dist < distance & period >= 23) |
      (!is.na(min_piccadilly_dist) & min_piccadilly_dist < distance & period >= 24) |
      (!is.na(min_victoria_dist) & min_victoria_dist < distance & period >= 20),
      1,
      0
    )) %>%

    # create first_treatment variable, assigning it infninity to all untreated locations
    mutate(!!first_treatment_col := Inf) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_piccadilly_dist) & min_piccadilly_dist < distance, 24, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_northern_dist) & min_northern_dist < distance, 23, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_jubilee_dist) & min_jubilee_dist < distance, 22, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_victoria_dist) & min_victoria_dist < distance, 20, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_central_dist) & min_central_dist < distance, 20, !!sym(first_treatment_col))) %>%
    

    # finally, create a set of event time dummies
    mutate(!!event_time_col := case_when(
      !!sym(first_treatment_col) < Inf ~ period - !!sym(first_treatment_col),
      !!sym(first_treatment_col) == Inf ~ -1
    ))
    
  return(data)

}


# now create a function that takes in distance thresholds and the overall treatment distance, and creates distance band dummies and their interactions with event time
define_distance_bands <- function(thresholds, distance, data) {

  # use the overall treatment distance to get the first treatment and event time column names
  first_treatment_col <- paste0("first_treatment_", distance)
  event_time_col <- paste0("event_time_", distance)

  data <- data %>%

    # first create a variable giving distace to closest active night tube station
    mutate(min_active_dist := case_when(
      (!!sym(first_treatment_col) == 20) ~ pmin(min_central_dist, min_victoria_dist, na.rm = TRUE),
      (!!sym(first_treatment_col) == 22) ~ min_jubilee_dist,
      (!!sym(first_treatment_col) == 23) ~ min_northern_dist,
      (!!sym(first_treatment_col) == 24) ~ min_piccadilly_dist,
      TRUE ~ Inf
    ))
    # min_active_dist is dependent on treatment!! Either add a _dist, or make it independent of treatment

  # now loop over the thresholds to create distance band dummies and their interactions with event time
  for (i in 1:(length(thresholds) - 1)) {
    lower <- thresholds[i]
    upper <- thresholds[i + 1]

    dist_band_col <- paste0("dist_band_", lower, "_", upper)
    event_time_dist_col <- paste0("event_time_dist_", lower, "_", upper)

    data <- data %>%
      mutate(!!dist_band_col := !is.na(min_active_dist) & min_active_dist >= lower & min_active_dist < upper) %>%
      mutate(!!event_time_dist_col := ifelse(!!sym(dist_band_col), !!sym(event_time_col), -1))
  }

  return(data)

}

# NEED TO CHECK THIS ONE ABOVE


############################################################
# prepare the data for analysis
############################################################


# first we define treatment and event time variables
# in the baseline case, we call a location treated if it is at most 1km from an active night tube station

# use the functions above to create treatment and event time variables, and distance band variables
final_data <- define_treatment_event_time(distance = 1, data = final_data)
final_data <- define_distance_bands(thresholds = c(0, 0.25, 0.5, 0.75, 1), distance = 1, data = final_data)
# note that the distance bands are: 0-0.25km, 0.25-0.5km, 0.5-0.75km, 0.75-1km


# now do the exact same for the house price data
house_data <- define_treatment_event_time(distance = 1, data = house_data)
house_data <- define_distance_bands(thresholds = c(0, 0.25, 0.5, 0.75, 1), distance = 1, data = house_data)

# CHECK THESE WORKED! graphs are the same, so it seems to be fine, but check the functions

# also, filter only for locations within 2km of a station, for representativeness
final_data <- final_data %>%
  filter(min_any_dist < 2)

# filter now the house data for only for locations within 2km of a station (NOTE - I think it already satisfies this. Check the post-geocoding processing)
house_data <- house_data %>%
  filter(min_any_dist < 2)



############################################################
# now begin the analysis
############################################################


# first plot means over time for treated and untreated areas, by the 'first treatment' variable

# calculate monthly means of log number of crimes, by first treatment period
mean_data <- final_data %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_num_crimes = mean(log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean in periods 1 to 19, by subtracting the difference
baseline_means <- mean_data %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data <- mean_data %>%
  left_join(baseline_means, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_num_crimes = mean_log_num_crimes - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data, aes(x = period, y = adjusted_mean_log_num_crimes, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log number of crimes over time by first treatment period",
       x = "Month",
       y = "Mean log number of crimes",
       color = "First Treatment Period") +
  theme_minimal()


# now do it after grouping into three month bins
mean_data_binned <- final_data %>%
  mutate(period_bin = case_when(
    period %in% c(1, 2, 3) ~ 1,
    period %in% c(4, 5, 6) ~ 4,
    period %in% c(7, 8, 9) ~ 7,
    period %in% c(10, 11, 12) ~ 10,
    period %in% c(13, 14, 15) ~ 13,
    period %in% c(16, 17, 18) ~ 16,
    period %in% c(19, 20, 21) ~ 19,
    period %in% c(22, 23, 24) ~ 22,
    period %in% c(25, 26, 27) ~ 25,
    period %in% c(28, 29, 30) ~ 28,
    period %in% c(31, 32, 33) ~ 31,
    period %in% c(34, 35, 36) ~ 34
  )) %>%
  group_by(period_bin, first_treatment_1) %>%
  summarise(mean_log_num_crimes = mean(log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period_bin 1 and 18, by subtracting the difference
baseline_means_binned <- mean_data_binned %>%
  filter(period_bin >= 1 & period_bin <= 18) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_binned <- mean_data_binned %>%
  left_join(baseline_means_binned, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_num_crimes = mean_log_num_crimes - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_binned, aes(x = period_bin, y = adjusted_mean_log_num_crimes, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log number of crimes over time (binned) by first treatment period",
       x = "Month (binned)",
       y = "Mean log number of crimes",
       color = "First Treatment Period") +
  theme_minimal()


############################################################

# now do the same for thefts
mean_data_theft <- final_data %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_theft_from_person = mean(log_theft_from_person, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_theft <- mean_data_theft %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_theft_from_person, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_theft <- mean_data_theft %>%
  left_join(baseline_means_theft, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_theft_from_person = mean_log_theft_from_person - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_theft, aes(x = period, y = adjusted_mean_log_theft_from_person, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log theft from the person over time by first treatment period",
       x = "Month",
       y = "Mean log theft from the person",
       color = "First Treatment Period") +
  theme_minimal()

############################################################

# now get a summary table

sumtable(data = final_data, 
        vars = c("num_crimes", "theft_from_the_person", "robbery"),
        out = "latex",
        file = "Crime and night tubes/Output/Figures/summary_table.tex")

# MAKE THIS MUCH BETTER

############################################################

# plot a histogram of the crime counts

ggplot(final_data, aes(x = num_crimes)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "Histogram of Number of Crimes per Location-Month",
       x = "Number of Crimes",
       y = "Frequency") +
  xlim(-1, 50) +
  labs(caption = "Note: Values above 50 are not shown") +
  theme_minimal()

# save
ggsave("Crime and night tubes/Output/Figures/histogram_num_crimes.png", width = 8, height = 6)

# MAKE THIS MUCH BETTER

############################################################