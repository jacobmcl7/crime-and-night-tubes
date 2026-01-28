# this script does some data visualisation and other descriptive statistics

# import relevant packages
library(fixest)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(vtable)
library(openxlsx)

# set working directory
setwd("~/Economics/Papers (WIP)")

# load in the data
load("Crime and night tubes EXTRA DATA/final_data.RData")
# load("Crime and night tubes EXTRA DATA/house_data_cleaned.RData")


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
      (!is.na(min_nt_central_dist) & min_nt_central_dist < distance & period >= 20) |
      (!is.na(min_nt_jubilee_dist) & min_nt_jubilee_dist < distance & period >= 22) |
      (!is.na(min_nt_northern_dist) & min_nt_northern_dist < distance & period >= 23) |
      (!is.na(min_nt_piccadilly_dist) & min_nt_piccadilly_dist < distance & period >= 24) |
      (!is.na(min_nt_victoria_dist) & min_nt_victoria_dist < distance & period >= 20),
      1,
      0
    )) %>%

    # create first_treatment variable, assigning it infninity to all untreated locations
    mutate(!!first_treatment_col := Inf) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_nt_piccadilly_dist) & min_nt_piccadilly_dist < distance, 24, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_nt_northern_dist) & min_nt_northern_dist < distance, 23, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_nt_jubilee_dist) & min_nt_jubilee_dist < distance, 22, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_nt_victoria_dist) & min_nt_victoria_dist < distance, 20, !!sym(first_treatment_col))) %>%
    mutate(!!first_treatment_col := ifelse(!is.na(min_nt_central_dist) & min_nt_central_dist < distance, 20, !!sym(first_treatment_col))) %>%
    

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
      (!!sym(first_treatment_col) == 20) ~ pmin(min_nt_central_dist, min_nt_victoria_dist, na.rm = TRUE),
      (!!sym(first_treatment_col) == 22) ~ min_nt_jubilee_dist,
      (!!sym(first_treatment_col) == 23) ~ min_nt_northern_dist,
      (!!sym(first_treatment_col) == 24) ~ min_nt_piccadilly_dist,
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
# house_data <- define_treatment_event_time(distance = 1, data = house_data)
# house_data <- define_distance_bands(thresholds = c(0, 0.25, 0.5, 0.75, 1), distance = 1, data = house_data)

# CHECK THESE WORKED! graphs are the same, so it seems to be fine, but check the functions

# also, filter only for locations within 2km of a station, for representativeness
final_data <- final_data %>%
  filter(min_any_dist < 2)

# filter now the house data for only for locations within 2km of a station (NOTE - I think it already satisfies this. Check the post-geocoding processing)
# house_data <- house_data %>%
#   filter(min_any_dist < 2)



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

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_num_crimes_over_time.png", width = 8, height = 6)


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

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_num_crimes_over_time_binned.png", width = 8, height = 6)


############################################################

# now do the same for thefts
mean_data_theft <- final_data %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_theft_from_the_person = mean(log_theft_from_the_person, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_theft <- mean_data_theft %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_theft_from_the_person, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_theft <- mean_data_theft %>%
  left_join(baseline_means_theft, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_theft_from_the_person = mean_log_theft_from_the_person - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_theft, aes(x = period, y = adjusted_mean_log_theft_from_the_person, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log theft from the person over time by first treatment period",
       x = "Month",
       y = "Mean log theft from the person",
       color = "First Treatment Period") +
  theme_minimal()

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_theft_from_the_person_over_time.png", width = 8, height = 6)


############################################################

# now do the same thing for different control groups - first just locations within 1km of any station

mean_data_near <- final_data %>%
  filter(min_any_dist < 1) %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_num_crimes = mean(log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_near <- mean_data_near %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_near <- mean_data_near %>%
  left_join(baseline_means_near, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_num_crimes = mean_log_num_crimes - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_near, aes(x = period, y = adjusted_mean_log_num_crimes, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log number of crimes over time by first treatment period (within 1km of any station)",
       x = "Month",
       y = "Mean log number of crimes",
       color = "First Treatment Period",
       note = "Modified control group - regions within 1km of any station") +
  theme_minimal()

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_num_crimes_over_time_within_1km.png", width = 8, height = 6)


# same but for thefts
mean_data_near_theft <- final_data %>%
  filter(min_any_dist < 1) %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_theft_from_the_person = mean(log_theft_from_the_person, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_near_theft <- mean_data_near_theft %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_theft_from_the_person, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_near_theft <- mean_data_near_theft %>%
  left_join(baseline_means_near_theft, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_theft_from_the_person = mean_log_theft_from_the_person - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_near_theft, aes(x = period, y = adjusted_mean_log_theft_from_the_person, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log theft from the person over time by first treatment period (within 1km of any station)",
       x = "Month",
       y = "Mean log theft from the person",
       color = "First Treatment Period",
       note = "Modified control group - regions within 1km of any station") +
  theme_minimal()

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_theft_from_the_person_over_time_within_1km.png", width = 8, height = 6)

############################################################

# now do the same, but using only regions within 2km of a night tube station

# keep only those observations where any of min_nt_*_dist < 2
final_data_nt_near <- final_data %>%
  filter(
    !is.na(min_nt_central_dist) |
    !is.na(min_nt_jubilee_dist) |
    !is.na(min_nt_northern_dist) |
    !is.na(min_nt_piccadilly_dist) |
    !is.na(min_nt_victoria_dist)
  )

mean_data_nt_near <- final_data_nt_near %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_num_crimes = mean(log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_nt_near <- mean_data_nt_near %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_nt_near <- mean_data_nt_near %>%
  left_join(baseline_means_nt_near, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_num_crimes = mean_log_num_crimes - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_nt_near, aes(x = period, y = adjusted_mean_log_num_crimes, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log number of crimes over time by first treatment period (within 2km of night tube station)",
       x = "Month",
       y = "Mean log number of crimes",
       color = "First Treatment Period",
       note = "Modified control group - regions between 1-2km of night tube station") +
  theme_minimal()

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_num_crimes_over_time_within_2km_nt.png", width = 8, height = 6)


# same but for thefts
mean_data_nt_near_theft <- final_data_nt_near %>%
  group_by(period, first_treatment_1) %>%
  summarise(mean_log_theft_from_the_person = mean(log_theft_from_the_person, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_nt_near_theft <- mean_data_nt_near_theft %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment_1) %>%
  summarise(baseline_mean = mean(mean_log_theft_from_the_person, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_nt_near_theft <- mean_data_nt_near_theft %>%
  left_join(baseline_means_nt_near_theft, by = "first_treatment_1") %>%
  mutate(adjusted_mean_log_theft_from_the_person = mean_log_theft_from_the_person - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_nt_near_theft, aes(x = period, y = adjusted_mean_log_theft_from_the_person, color = as.factor(first_treatment_1), group = as.factor(first_treatment_1))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log theft from the person over time by first treatment period (within 2km of night tube station)",
       x = "Month",
       y = "Mean log theft from the person",
       color = "First Treatment Period",
       note = "Modified control group - regions between 1-2km of night tube station") +
  theme_minimal()

# save
ggsave("Crime and night tubes/Output/Figures/mean_log_theft_from_the_person_over_time_within_2km_nt.png", width = 8, height = 6)




############################################################

# now get a summary table for the crime stats

# collect up relevant variables
crime_types <- c(
  "num_crimes",
  "robbery",
  "theft_from_the_person", 
  "violence_and_sexual_offences",
  "public_order",
  "burglary",
  "bicycle_theft",
  "shoplifting",
  "criminal_damage_and_arson",
  "drugs",
  "other_theft",
  "vehicle_crime",
  "possession_of_weapons",
  "other_crime",
  "anti-social_behaviour"
)

# create a summary function that gets the stats we want
calc_summary <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Median = median(x),
    P75 = quantile(x, 0.75),
    P90 = quantile(x, 0.90),
    P95 = quantile(x, 0.95),
    P99 = quantile(x, 0.99),
    Max = max(x),
    Total_Count = sum(x),
    Nonzero = sum(x > 0),
    PCT_Nonzero = mean(x > 0) * 100
  )
}

# get the summary stats
panel <- map_dfr(crime_types, ~{
  calc_summary(final_data[[.x]]) %>%
    mutate(Variable = .x, .before = 1)
})

# make the names nicer
format_varname <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_replace("num crimes", "Total crimes") %>%
    str_to_title() %>%
    str_replace("And", "and")
}

panel$Variable <- format_varname(panel$Variable)

# make the table
full_table <- panel

# round relevant columns
full_table <- full_table %>%
  mutate(across(c(Mean, SD), ~round(., 3)))

# order the table by total number of crimes
full_table <- full_table %>%
  arrange(desc(Nonzero))

# prepare table for LaTeX
latex_table <- full_table %>%
  select(Variable, N, Mean, SD, Median, P75, P90, P95, P99, Max, Total_Count, Nonzero, PCT_Nonzero) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary Statistics",
    label = "tab:summary_stats",
    col.names = c("Variable", "N", "Mean", "SD", "Median", "P75", "P90", "P95", "P99", "Max", "Total Count", "Nonzero", "\\% Nonzero"),
    align = c("l", rep("c", 12)),
    digits = 3,
    escape = FALSE  # needed if using LaTeX symbols in column names
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 10
  ) %>%
  row_spec(0, bold = TRUE)  # bold header row

# Save to .tex file
writeLines(latex_table, "Crime and night tubes/Output/Figures/crimes_summary_stats.tex")

############################################################

# now make a similar table for distance stats

# collect up relevant distance variables
distance_vars <- c(
  "min_any_dist",
  "min_nt_central_dist",
  "min_nt_jubilee_dist",
  "min_nt_northern_dist",
  "min_nt_piccadilly_dist",
  "min_nt_victoria_dist"
)

# make a summary function that gets the stats we want
nrows <- length(final_data$min_any_dist)
calc_summary_distance <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    N_within_2km = length(x),
    PCT_within_2km = sum(x <= 2) * 100 / nrows,
    N_within_1km = sum(x <= 1),
    PCT_within_1km = sum(x <= 1) * 100 / nrows,
    N_within_0.5km = sum(x <= 0.5),
    PCT_within_0.5km = sum(x <= 0.5) * 100 / nrows
  )
}

# get the summary stats
panel_distance <- map_dfr(distance_vars, ~{
  calc_summary_distance(final_data[[.x]]) %>%
    mutate(Variable = .x, .before = 1)
})

# make the names nicer
panel_distance$Variable <- panel_distance$Variable %>%
  str_replace_all("min_nt_", "Min distance to night tube ") %>%
  str_replace_all("min_any_dist", "Min distance to any night tube") %>%
  str_replace_all("_dist", " station") %>%
  str_replace_all("_", " ") %>%
  str_to_title()

# make the table
full_table_distance <- panel_distance

# prepare table for LaTeX
latex_table_distance <- full_table_distance %>%
  select(Variable, N_within_2km, PCT_within_2km, N_within_1km, PCT_within_1km, N_within_0.5km, PCT_within_0.5km) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Distance Statistics",
    label = "tab:distance_stats",
    col.names = c("Variable", "N within 2km", "\\% within 2km", "N within 1km", "\\% within 1km", "N within 0.5km", "\\% within 0.5km"),
    align = c("l", rep("c", 6)),
    digits = 0
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 10
  ) %>%
    row_spec(0, bold = TRUE)  # bold header row

# Save to .tex file
writeLines(latex_table_distance, "Crime and night tubes/Output/Figures/distance_summary_stats.tex")

############################################################

# plot a binned bar chart of the crime counts

# first define the bins
crime_bins <- final_data %>%
  mutate(crime_category = cut(
    num_crimes,
    breaks = c(-Inf, 0, 1, 2, 5, 10, 20, 50, Inf),
    labels = c("0", "1", "2", "3-5", "6-10", "11-20", "21-50", "50+")
  )) %>%
  count(crime_category) %>%
  mutate(pct = n / sum(n) * 100)

# now plot it
ggplot(crime_bins, aes(x = crime_category, y = pct)) +
  geom_col(fill = "blue", color = "black", alpha = 0.5) +
  geom_text(aes(label = sprintf("%.2f%%", pct)), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Crimes per Location-Month",
    subtitle = "Grouped into categories",
    x = "Number of Crimes",
    y = "Percentage of Observations"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

# save
ggsave("Crime and night tubes/Output/Figures/bar_chart_num_crimes.png", width = 8, height = 6)


# now do the same for thefts and robberies

# first preprocess the data into long format in order to calculate the correct counts
crime_bins_long <- final_data %>%
  select(robbery, theft_from_the_person) %>%
  pivot_longer(
    cols = everything(),
    names_to = "crime_type",
    values_to = "count"
  ) %>%
  mutate(
    crime_category = cut(
      count,
      breaks = c(-Inf, 0, 1, 2, 5, 10, Inf),
      labels = c("0", "1", "2", "3-5", "6-10", "11+")
    ),
    crime_type = case_when(
      crime_type == "robbery" ~ "Robbery",
      crime_type == "theft_from_the_person" ~ "Theft from Person"
    )
  ) %>%
  count(crime_type, crime_category) %>%
  group_by(crime_type) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# now plot it
ggplot(crime_bins_long, aes(x = crime_category, y = pct)) +
  geom_col(fill = "blue", color = "black", alpha = 0.5) +
  geom_text(aes(label = sprintf("%.2f%%", pct)), vjust = -0.5, size = 2.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~crime_type) +
  labs(
    x = "Number of Crimes per Location-Month",
    y = "Percentage of Observations"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

# save
ggsave("Crime and night tubes/Output/Figures/bar_chart_theft_robbery.png", width = 8, height = 5)

############################################################

# get the theft and robbery count in the last six months before first treatment, and last six months of the sample

# do this for the whole of the sample, not just the subset within 2km of a station
load("Crime and night tubes EXTRA DATA/final_data.RData")
# final_data is reloaded here

# create the summary table
theft_robbery_summary <- final_data %>%
  mutate(
    pre_treatment_period = ifelse(period >= 13 & period <= 18, 1, 0),
    post_treatment_period = ifelse(period >= 31 & period <= 36, 1, 0)
  ) %>%
  group_by(location) %>%
  summarise(
    thefts_pre_treatment = sum(theft_from_the_person * pre_treatment_period, na.rm = TRUE),
    robberies_pre_treatment = sum(robbery * pre_treatment_period, na.rm = TRUE),
    thefts_post_treatment = sum(theft_from_the_person * post_treatment_period, na.rm = TRUE),
    robberies_post_treatment = sum(robbery * post_treatment_period, na.rm = TRUE)
  ) %>%
  ungroup()

# get the proportional difference of the sum
theft_robbery_summary <- theft_robbery_summary %>%
  mutate(
    thefts_robberies_pre = thefts_pre_treatment + robberies_pre_treatment,
    thefts_robberies_post = thefts_post_treatment + robberies_post_treatment,
    thefts_robberies_prop_diff = ifelse(thefts_robberies_pre > 0, (thefts_robberies_post - thefts_robberies_pre) / thefts_robberies_pre, NA)
  )

# split location into lat and long separately, by taking values before and after the comma
theft_robbery_summary <- theft_robbery_summary %>%
  separate(location, into = c("latitude", "longitude"), sep = ", ") %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )


# export it to excel to be visualised in ArcGIS
write.xlsx(theft_robbery_summary, "Crime and night tubes EXTRA DATA/theft_robbery_summary.xlsx")



############################################################

# plot the evolution of mean number of thefts and solved number of thefts at red stations over the sample period

# ISSUE - TO GET UPDATED OUTCOMES WE NEED THE NEWER VERSION OF THE DATASET. IGNORE FOR NOW

# start by summing thefts and robberies over all observations that share the same nearest_station and period, and have min_any_dist < 0.25
police_response_data <- final_data %>%
  filter(min_any_dist < 0.25) %>%
  mutate(thefts_robberies = theft_from_the_person + robbery,
         outcome_yes_thefts_robberies = outcome_yes_theft_from_the_person + outcome_yes_robbery,
         outcome_done_thefts_robberies = outcome_no_theft_from_the_person + outcome_no_robbery + outcome_yes_thefts_robberies) %>%
  group_by(closest_station, period) %>%
  summarise(
    total_thefts_robberies = sum(thefts_robberies, na.rm = TRUE),
    solved_thefts_robberies = sum(outcome_yes_thefts_robberies, na.rm = TRUE),
    done_thefts_robberies = sum(outcome_done_thefts_robberies, na.rm = TRUE)
  ) %>%
  ungroup()

# keep if the station is a red station, then get the monthly total number and solved number of thefts and robberies on average
red_stations <- c("Camden Town", "London Bridge", "North Greenwich", "Vauxhall", "Brixton", "Waterloo", "Oxford Circus", "Leicester Square", "Piccadilly Circus", "Charing Cross", "Victoria", "Hammersmith", "Walthamstow Central", "Stratford")

police_response_data <- police_response_data %>%
  filter(closest_station %in% red_stations)

# now sum over all stations to get the total number and solved number of thefts and robberies per month
police_response_data <- police_response_data %>%
  group_by(period) %>%
  summarise(
    total_thefts_robberies = sum(total_thefts_robberies, na.rm = TRUE),
    solved_thefts_robberies = sum(solved_thefts_robberies, na.rm = TRUE),
    done_thefts_robberies = sum(done_thefts_robberies, na.rm = TRUE)
  ) %>%
  ungroup()

# plot the number of total and solved thefts and robberies over time for each station
ggplot(police_response_data) +
  geom_line(aes(x = period, y = total_thefts_robberies, color = "blue"), size = 1) +
  labs(title = "Total Thefts and Robberies Over Time by Station",
       x = "Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(police_response_data) +
  geom_line(aes(x = period, y = solved_thefts_robberies, color = "red"), size = 1) +
  labs(title = "Proportion Solved Over Time by Station",
       x = "Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(police_response_data) +
  geom_line(aes(x = period, y = solved_thefts_robberies/done_thefts_robberies, color = "red"), size = 1) +
  labs(title = "Proportion Solved Over Time by Station, of those with an outcome",
       x = "Period") +
  theme_minimal() +
  theme(legend.position = "bottom")



# now plot the solve rate over time in the whole sample for the total crime count

police_response_data <- final_data %>%
  group_by(period) %>%
  summarise(
    total_crimes = sum(num_crimes, na.rm = TRUE),
    solved_crimes = sum(outcome_yes_all, na.rm = TRUE),
    solve_rate = solved_crimes / total_crimes * 100
  ) %>%
  ungroup()

ggplot(police_response_data, aes(x = period, y = solve_rate)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Police Solve Rate Over Time for Total Crimes",
       x = "Period",
       y = "Solve Rate (%)") +
  theme_minimal()

# this shows the dataset needs updating