# this file does the analysis

# import relevant packages
library(fixest)
library(ggplot2)
library(tidyverse)
library(patchwork)
# library(didimputation)
library(KernSmooth) # for kernel regression

# set working directory
setwd("~/Economics/Papers (WIP)")

# load in the data
load("Crime and night tubes EXTRA DATA/final_data_new.RData")
load("Crime and night tubes EXTRA DATA/house_data_cleaned.RData")


########################################################
# define some functions
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


############################################################
# prepare the data for analysis
############################################################


# first under the baseline definition of treatment
# in particular, we call a region treated if it is at most 1km from an active night tube station - this will be the baseline definition

# create a variable giving whether a location is being currently treated according to the definition above

dist = 1

final_data <- final_data %>%
  mutate(treatment = ifelse(
    (!is.na(min_central_dist) & min_central_dist < dist & period >= 20) |
    (!is.na(min_jubilee_dist) & min_jubilee_dist < dist & period >= 22) |
    (!is.na(min_northern_dist) & min_northern_dist < dist & period >= 23) |
    (!is.na(min_piccadilly_dist) & min_piccadilly_dist < dist & period >= 24) |
    (!is.na(min_victoria_dist) & min_victoria_dist < dist & period >= 20),
    1,
    0
  ))
# note that the first treatment months of each station are:
# Central: 19 Aug 2016 (first treatment month = 12 + 8 = 20)
# Victoria: 19 Aug 2016 (ftm = 20)
# Jubilee: 7 Oct 2016 (ftm = 22)
# Northern: 18 Nov 2016 (ftm = 23)
# Piccadilly: 16 Dec 2016 (ftm = 24)


# now create event time variables
final_data <- final_data %>%
    
    # first get a variable giving the period of first treatment
    mutate(first_treatment = 1000) %>%
    mutate(first_treatment = ifelse(!is.na(min_piccadilly_dist) & min_piccadilly_dist < dist, 24, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_northern_dist) & min_northern_dist < dist, 23, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_jubilee_dist) & min_jubilee_dist < dist, 22, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_victoria_dist) & min_victoria_dist < dist, 20, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_central_dist) & min_central_dist < dist, 20, first_treatment)) %>%

    # now get the event-times
    mutate(event_time = case_when(
        first_treatment < 1000 ~ period - first_treatment,
        first_treatment == 1000 ~ -1
    ))


# now create distance bands, and their interactions with event time
final_data <- final_data %>%

  # first create a variable giving distace to closest active night tube station
  mutate(min_active_dist = case_when(
      (first_treatment == 20) ~ pmin(min_central_dist, min_victoria_dist, na.rm = TRUE),
      (first_treatment == 22) ~ min_jubilee_dist,
      (first_treatment == 23) ~ min_northern_dist,
      (first_treatment == 24) ~ min_piccadilly_dist,
      TRUE ~ NA_real_
  )) %>%

  # create another TEMPORARY version that ignores how we've defined treatment, and therefore extends to 2km
  mutate(min_treated_dist = pmin(min_central_dist, min_victoria_dist, min_jubilee_dist, min_northern_dist, min_piccadilly_dist, na.rm = TRUE)) %>%
  # this might actually be better for our purposes - it is invariant to current treatment status

  # now create a set of dummies for distance bands from 0 up to 1 in intervals of 0.25 (this will need changing when the distances change)
  mutate(dist_band_0_025 = !is.na(min_active_dist) & min_active_dist < 0.25) %>%
  mutate(dist_band_025_05 = !is.na(min_active_dist) & min_active_dist >= 0.25 & min_active_dist < 0.5) %>%
  mutate(dist_band_05_075 = !is.na(min_active_dist) & min_active_dist >= 0.5 & min_active_dist < 0.75) %>%
  mutate(dist_band_075_1 = !is.na(min_active_dist) & min_active_dist >= 0.75 & min_active_dist < 1) %>%

  # now interact these with the event-time dummies
  mutate(event_time_dist_0_025 = ifelse(dist_band_0_025, event_time, -1)) %>%
  mutate(event_time_dist_025_05 = ifelse(dist_band_025_05, event_time, -1)) %>%
  mutate(event_time_dist_05_075 = ifelse(dist_band_05_075, event_time, -1)) %>%
  mutate(event_time_dist_075_1 = ifelse(dist_band_075_1, event_time, -1))


# create log variable
final_data <- final_data %>%
  mutate(log_theft_from_person = log(1 + `Theft from the person`)) %>%
  mutate(log_shoplifting = log(1 + Shoplifting)) %>%
  mutate(log_burglary = log(1 + Burglary))
# note: may need to do this another way (e.g. logit/poisson) as there are a lot of 0 and 1 (harder to justify log(1 + x) here?)



# filter only for locations within 2km of a station
final_data <- final_data %>%
  filter(min_any_dist < 2)



############################################################
# repeat the above processing of the treatment variable for the house data
############################################################

# baseline treatment variable
house_data <- house_data %>%
  mutate(treatment = ifelse(
    (!is.na(min_central_dist) & min_central_dist < dist & period >= 20) |
    (!is.na(min_jubilee_dist) & min_jubilee_dist < dist & period >= 22) |
    (!is.na(min_northern_dist) & min_northern_dist < dist & period >= 23) |
    (!is.na(min_piccadilly_dist) & min_piccadilly_dist < dist & period >= 24) |
    (!is.na(min_victoria_dist) & min_victoria_dist < dist & period >= 20),
    1,
    0
  ))


# now event time variables
house_data <- house_data %>%
    
    # first get a variable giving the period of first treatment
    mutate(first_treatment = 1000) %>%
    mutate(first_treatment = ifelse(!is.na(min_piccadilly_dist) & min_piccadilly_dist < dist, 24, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_northern_dist) & min_northern_dist < dist, 23, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_jubilee_dist) & min_jubilee_dist < dist, 22, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_victoria_dist) & min_victoria_dist < dist, 20, first_treatment)) %>%
    mutate(first_treatment = ifelse(!is.na(min_central_dist) & min_central_dist < dist, 20, first_treatment)) %>%

    # now get the event-times
    mutate(event_time = case_when(
        first_treatment < 1000 ~ period - first_treatment,
        first_treatment == 1000 ~ -1
    ))


# now distance bands and their interactions with event time
house_data <- house_data %>%

  # first create a variable giving distace to closest active night tube station
  mutate(min_active_dist = case_when(
      (first_treatment == 20) ~ pmin(min_central_dist, min_victoria_dist, na.rm = TRUE),
      (first_treatment == 22) ~ min_jubilee_dist,
      (first_treatment == 23) ~ min_northern_dist,
      (first_treatment == 24) ~ min_piccadilly_dist,
      TRUE ~ NA_real_
  )) %>%

  # now create a set of dummies for distance bands from 0 up to 1 in intervals of 0.25 (this will need changing when the distances change)
  mutate(dist_band_0_025 = !is.na(min_active_dist) & min_active_dist < 0.25) %>%
  mutate(dist_band_025_05 = !is.na(min_active_dist) & min_active_dist >= 0.25 & min_active_dist < 0.5) %>%
  mutate(dist_band_05_075 = !is.na(min_active_dist) & min_active_dist >= 0.5 & min_active_dist < 0.75) %>%
  mutate(dist_band_075_1 = !is.na(min_active_dist) & min_active_dist >= 0.75 & min_active_dist < 1) %>%

  # now interact these with the event-time dummies
  mutate(event_time_dist_0_025 = ifelse(dist_band_0_025, event_time, -1)) %>%
  mutate(event_time_dist_025_05 = ifelse(dist_band_025_05, event_time, -1)) %>%
  mutate(event_time_dist_05_075 = ifelse(dist_band_05_075, event_time, -1)) %>%
  mutate(event_time_dist_075_1 = ifelse(dist_band_075_1, event_time, -1))

# filter only for locations within 2km of a station (NOTE - I think it already satisfies this. Check the post-geocoding processing)
house_data <- house_data %>%
  filter(min_any_dist < 2)



############################################################
# now do some analysis
############################################################

# first plot means over time for treated and untreated areas, by the 'first treatment' variable
mean_data <- final_data %>%
  group_by(period, first_treatment) %>%
  summarise(mean_log_num_crimes = mean(log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean in periods 1 to 19, by subtracting the difference
baseline_means <- mean_data %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment) %>%
  summarise(baseline_mean = mean(mean_log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data <- mean_data %>%
  left_join(baseline_means, by = "first_treatment") %>%
  mutate(adjusted_mean_log_num_crimes = mean_log_num_crimes - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data, aes(x = period, y = adjusted_mean_log_num_crimes, color = as.factor(first_treatment), group = as.factor(first_treatment))) +
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
  group_by(period_bin, first_treatment) %>%
  summarise(mean_log_num_crimes = mean(log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period_bin 1 and 18, by subtracting the difference
baseline_means_binned <- mean_data_binned %>%
  filter(period_bin >= 1 & period_bin <= 18) %>%
  group_by(first_treatment) %>%
  summarise(baseline_mean = mean(mean_log_num_crimes, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_binned <- mean_data_binned %>%
  left_join(baseline_means_binned, by = "first_treatment") %>%
  mutate(adjusted_mean_log_num_crimes = mean_log_num_crimes - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_binned, aes(x = period_bin, y = adjusted_mean_log_num_crimes, color = as.factor(first_treatment), group = as.factor(first_treatment))) +
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
  group_by(period, first_treatment) %>%
  summarise(mean_log_theft_from_person = mean(log_theft_from_person, na.rm = TRUE)) %>%
  ungroup()

# edit the data so all have the same average mean between period 1 and 19, by subtracting the difference
baseline_means_theft <- mean_data_theft %>%
  filter(period >= 1 & period <= 19) %>%
  group_by(first_treatment) %>%
  summarise(baseline_mean = mean(mean_log_theft_from_person, na.rm = TRUE)) %>%
  ungroup()

# join the baseline means back to the mean data
mean_data_theft <- mean_data_theft %>%
  left_join(baseline_means_theft, by = "first_treatment") %>%
  mutate(adjusted_mean_log_theft_from_person = mean_log_theft_from_person - baseline_mean + mean(baseline_mean, na.rm = TRUE))

# plot it as a line graph, by first treatment period
ggplot(mean_data_theft, aes(x = period, y = adjusted_mean_log_theft_from_person, color = as.factor(first_treatment), group = as.factor(first_treatment))) +
  geom_line() +
  geom_point() +
  labs(title = "Mean log theft from the person over time by first treatment period",
       x = "Month",
       y = "Mean log theft from the person",
       color = "First Treatment Period") +
  theme_minimal()




############################################################

# now do a simple dynamic TWFE regression

# now do the regression, saving it to then be plotted
TWFE_1km <- feols(log_num_crimes ~ i(event_time, ref = -1) | location + Month, data = final_data, cluster = "location")


# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km, substring = "event_time")


# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km.png", width = 8, height = 6)



####################################################################

# same for house prices

TWFE_1km_house <- feols(log_price ~ i(event_time, ref = -1) | pcsect + month, data = house_data, cluster = "pcsect")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_house, substring = "event_time")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.15,
    ymax = 0.15,
    title = "Dynamic TWFE results - House Prices", 
    note = "Simple treatment definition, theshold = 1km")




####################################################################

# now use Abraham and Sun (2021) method

# same as before, but we use the sunab command in fixest
# note that our cohort variable is first_treatment, and the large value of this variable for never treated units is what the command wants

sunab_1km <- feols(log_num_crimes ~ sunab(first_treatment, period) | location + Month, data = final_data, cluster = "location")

# prepare for plotting
coefs <- plot_prepare2(sunab_1km, omitted_pd = -1)
# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic Sun and Abraham (2021) results", 
    note = "Simple treatment definition, theshold = 1km") 

# save it
ggsave("Crime and night tubes/Output/Results/Sunab_1km.png", width = 8, height = 6)



####################################################################

# ADD CONTROLS! This goes here

####################################################################

# disaggregate the results by distance: interact each of the event time dummies with a distance variable

# now these can be used in a regression
TWFE_1km_disagg <- feols(log_num_crimes ~ i(event_time_dist_0_025, ref = -1) + i(event_time_dist_025_05, ref = -1) + i(event_time_dist_05_075, ref = -1) + i(event_time_dist_075_1, ref = -1) | location + Month, data = final_data, cluster = "location")


# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_0_025")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_025_05")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_05_075")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_075_1")

# plot them, in a 2x2 grid
# first create the plots
p1 <- plot(coefs = coefs_0_025, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0 to 0.25km")
p2 <- plot(coefs = coefs_025_05, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0.25 to 0.5km")
p3 <- plot(coefs = coefs_05_075,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.5 to 0.75km")
p4 <- plot(coefs = coefs_075_1,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.75 to 1km")

# now combine them into a grid
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km')

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg.png", width = 12, height = 8)


#####################################################################

# now with house prices

TWFE_1km_disagg_house <- feols(log_price ~ i(event_time_dist_0_025, ref = -1) + i(event_time_dist_025_05, ref = -1) + i(event_time_dist_05_075, ref = -1) + i(event_time_dist_075_1, ref = -1) | pcsect + month, data = house_data, cluster = "pcsect")


# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg_house, substring = "event_time_dist_0_025")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg_house, substring = "event_time_dist_025_05")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg_house, substring = "event_time_dist_05_075")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg_house, substring = "event_time_dist_075_1")

# plot them, in a 2x2 grid
# first create the plots
p1 <- plot(coefs = coefs_0_025, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.3,
           ymax = 0.3,
           title = "0 to 0.25km")
p2 <- plot(coefs = coefs_025_05, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.3,
           ymax = 0.3,
           title = "0.25 to 0.5km")
p3 <- plot(coefs = coefs_05_075,
            xsequence = seq(-20, 15, 5),
            ymin = -0.3,
            ymax = 0.3,
            title = "0.5 to 0.75km")
p4 <- plot(coefs = coefs_075_1,
            xsequence = seq(-20, 15, 5),
            ymin = -0.3,
            ymax = 0.3,
            title = "0.75 to 1km")

# now combine them into a grid
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km')





#####################################################################

# try to do these with A+S

# sunab_1km_disagg <- feols(log_num_crimes ~ sunab(first_treatment, period) : i(dist_band) | location + Month, data = final_data)

# work out how to do this!


#####################################################################

# do it for individual crimes

# first for theft from the person

# now do the regression, saving it to then be plotted
TWFE_1km_theft <- feols(log_theft_from_person ~ i(event_time, ref = -1) | location + Month, data = final_data, cluster = "location")


# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_theft, substring = "event_time")


# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results - Theft from the person", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_theft.png", width = 8, height = 6)


##########################################################

# now with A+S

# same as before, but we use the sunab command in fixest
sunab_1km_theft <- feols(log_theft_from_person ~ sunab(first_treatment, period) | location + Month, data = final_data, cluster = "location")
# prepare for plotting
coefs <- plot_prepare2(sunab_1km_theft, omitted_pd = -1)
# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic Sun and Abraham (2021) results - theft from the person",
    note = "Simple treatment definition, theshold = 1km")
# save it
ggsave("Crime and night tubes/Output/Results/Sunab_1km_theft.png", width = 8, height = 6)

##########################################################


# now do it with varying distance thresholds, again as before

# run the regression
TWFE_1km_disagg_theft <- feols(log_theft_from_person ~ i(event_time_dist_0_025, ref = -1) + i(event_time_dist_025_05, ref = -1) + i(event_time_dist_05_075, ref = -1) + i(event_time_dist_075_1, ref = -1) | location + Month, data = final_data, cluster = "location")


# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_0_025")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_025_05")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_05_075")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_075_1")

# plot them, in a 2x2 grid
# first create the plots
p1 <- plot(coefs = coefs_0_025, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0 to 0.25km")
p2 <- plot(coefs = coefs_025_05, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0.25 to 0.5km")
p3 <- plot(coefs = coefs_05_075,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.5 to 0.75km")
p4 <- plot(coefs = coefs_075_1,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.75 to 1km")

# now combine them into a grid
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results for theft, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km')

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_theft.png", width = 12, height = 8)




####################################################################

# now with a new crime: shoplifting

# first all together
TWFE_1km_shoplifting <- feols(log_shoplifting ~ i(event_time, ref = -1) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_shoplifting, substring = "event_time")
# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results - Shoplifting", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_shoplifting.png", width = 8, height = 6)


####################################################################

# now with A+S

# same as before, but we use the sunab command in fixest
sunab_1km_shoplifting <- feols(log_shoplifting ~ sunab(first_treatment, period) | location + Month, data = final_data, cluster = "location")
# prepare for plotting
coefs <- plot_prepare2(sunab_1km_shoplifting, omitted_pd = -1)
# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic Sun and Abraham (2021) results - shoplifting", 
    note = "Simple treatment definition, theshold = 1km")
# save it
ggsave("Crime and night tubes/Output/Results/Sunab_1km_shoplifting.png", width = 8, height = 6)


####################################################################

# now disaggregated by distance

# do the regression
TWFE_1km_disagg_shoplifting <- feols(log_shoplifting ~ i(event_time_dist_0_025, ref = -1) + i(event_time_dist_025_05, ref = -1) + i(event_time_dist_05_075, ref = -1) + i(event_time_dist_075_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg_shoplifting, substring = "event_time_dist_0_025")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg_shoplifting, substring = "event_time_dist_025_05")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg_shoplifting, substring = "event_time_dist_05_075")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg_shoplifting, substring = "event_time_dist_075_1")

# plot them, in a 2x2 grid
# first create the plots
p1 <- plot(coefs = coefs_0_025, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0 to 0.25km")
p2 <- plot(coefs = coefs_025_05, 
           xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.25 to 0.5km")
p3 <- plot(coefs = coefs_05_075,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.5 to 0.75km")
p4 <- plot(coefs = coefs_075_1,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.75 to 1km")

# now combine them into a grid
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results for shoplifting, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km')

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_shoplifting.png", width = 12, height = 8)



####################################################################

# now for burglary

# first all together
TWFE_1km_burglary <- feols(log_burglary ~ i(event_time, ref = -1) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_burglary, substring = "event_time")
# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results - Burglary", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_burglary.png", width = 8, height = 6)



####################################################################

# now with A+S

# same as before, but we use the sunab command in fixest

sunab_1km_burglary <- feols(log_burglary ~ sunab(first_treatment, period) | location + Month, data = final_data, cluster = "location")

# prepare for plotting
coefs <- plot_prepare2(sunab_1km_burglary, omitted_pd = -1)
# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic Sun and Abraham (2021) results - burglary", 
    note = "Simple treatment definition, theshold = 1km") 

# save it
ggsave("Crime and night tubes/Output/Results/Sunab_1km_burglary.png", width = 8, height = 6)



####################################################################

# now disaggregated by distance

# do the regression
TWFE_1km_disagg_burglary <- feols(log_burglary ~ i(event_time_dist_0_025, ref = -1) + i(event_time_dist_025_05, ref = -1) + i(event_time_dist_05_075, ref = -1) + i(event_time_dist_075_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg_burglary, substring = "event_time_dist_0_025")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg_burglary, substring = "event_time_dist_025_05")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg_burglary, substring = "event_time_dist_05_075")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg_burglary, substring = "event_time_dist_075_1")

# plot them, in a 2x2 grid
# first create the plots 
p1 <- plot(coefs = coefs_0_025, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0 to 0.25km")
p2 <- plot(coefs = coefs_025_05, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.125,
           ymax = 0.125,
           title = "0.25 to 0.5km")
p3 <- plot(coefs = coefs_05_075,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.5 to 0.75km")
p4 <- plot(coefs = coefs_075_1,
            xsequence = seq(-20, 15, 5),
            ymin = -0.125,
            ymax = 0.125,
            title = "0.75 to 1km")

# now combine them into a grid
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results for burglary, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km') 

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_burglary.png", width = 12, height = 8)



####################################################################

# non-parametric approach to distance decay: use kernel regression

# first collect the residuals from a basic regression on fixed effects, without event time dummies
TWFE_1km_theft <- feols(log_theft_from_person ~ 0 | location + Month, data = final_data)
final_data <- final_data %>%
  mutate(residuals = resid(TWFE_1km_theft))

# now run a sequence of kernel regressions of the residuals on distance to closest active night tube station, by event time

# plot a kernel regression estimate of the relationship between residuals and distance, for all post-treatment units
data_subset <- final_data %>%
  filter(event_time >= 0)

# do the kernel regression and save the results
model_kerns_all <- as.data.frame(locpoly(x = data_subset$min_active_dist,
                                 y = data_subset$residuals,
                                 bandwidth = dpill(data_subset$min_active_dist, data_subset$residuals),  # pilot bandwidth
                                 degree = 1,  # i.e. local linear
                                 gridsize = 100))

# plot the results
ggplot(model_kerns_all, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance (All Post-Treatment)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_all.png", width = 8, height = 6)




# now loop over six month periods from 0-5 to 12-17, doing a kernel regression for each and saving the predictions
for (t in seq(0, 15, by = 6)) {
  
  # subset the data to the relevant event times
  data_subset <- final_data %>%
    filter(event_time >= t & event_time < (t + 6))
  
  # do the kernel regression and save the results
  assign(paste0("model_kerns_", t), as.data.frame(locpoly(x = data_subset$min_active_dist,
                                       y = data_subset$residuals,
                                       bandwidth = dpill(data_subset$min_active_dist, data_subset$residuals),  # pilot bandwidth
                                       degree = 1,  # i.e. local linear
                                       gridsize = 100))
   )
}

# now plot the results, all on the same axes, with a colour gradient according to event time
ggplot() +
  geom_line(data = model_kerns_0, aes(x = x, y = y, color = "0-5 months"), size = 1) +
  geom_line(data = model_kerns_6, aes(x = x, y = y, color = "6-11 months"), size = 1) +
  geom_line(data = model_kerns_12, aes(x = x, y = y, color = "12-17 months"), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Treatment Effect Decay with Distance",
       x = "Distance from Station (km)",
       y = "Treatment Effect",
       color = "Event Time") +
  scale_color_manual(values = c("0-5 months" = "#3e18fa", "6-11 months" = "#29bdf3", "12-17 months" = "#00ffee")) +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_6_months.png", width = 8, height = 6)





# now do the same thing but in three month periods
for (t in seq(0, 15, by = 3)) {
  
  # subset the data to the relevant event times
  data_subset <- final_data %>%
    filter(event_time >= t & event_time < (t + 3))
  
  # do the kernel regression and save the results
  assign(paste0("model_kerns_", t), as.data.frame(locpoly(x = data_subset$min_active_dist,
                                                           y = data_subset$residuals,
                                                           bandwidth = dpill(data_subset$min_active_dist, data_subset$residuals),  # pilot bandwidth
                                                           degree = 1,  # i.e. local linear
                                                           gridsize = 100))
  )
}

# now plot the results, all on the same axes, with a colour gradient according to event time
ggplot() +
  geom_line(data = model_kerns_0, aes(x = x, y = y, color = "0-2 months"), size = 1) +
  geom_line(data = model_kerns_3, aes(x = x, y = y, color = "3-5 months"), size = 1) +
  geom_line(data = model_kerns_6, aes(x = x, y = y, color = "6-8 months"), size = 1) +
  geom_line(data = model_kerns_9, aes(x = x, y = y, color = "9-11 months"), size = 1) +
  geom_line(data = model_kerns_12, aes(x = x, y = y, color = "12-14 months"), size = 1) +
  geom_line(data = model_kerns_15, aes(x = x, y = y, color = "15-17 months"), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Treatment Effect Decay with Distance",
       x = "Distance from Station (km)",
       y = "Treatment Effect",
       color = "Event Time") +
  scale_color_manual(
    values = c(
      "0-2 months"   = "#3e18fa",
      "3-5 months"   = "#29bdf3",
      "6-8 months"   = "#00ffee",
      "9-11 months"  = "#00ff99",
      "12-14 months" = "#66ff33",
      "15-17 months" = "#ccff00"
    ),
    breaks = c("0-2 months", "3-5 months", "6-8 months", "9-11 months", "12-14 months", "15-17 months")
    # if you want the legend reversed (15 -> 0) add: , guide = guide_legend(reverse = TRUE)
  ) +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_3_months.png", width = 8, height = 6)



####################################################################

# now use Borusyak et al imputation-based estimator

# make the appropriate edits to the first_treated variable for this to work

# final_data <- final_data %>%
#   mutate(first_treatment = ifelse(first_treatment > 100, NA, first_treatment))


# # take a random sample of soze 1000 from the data to try this out on - full data is too large
# set.seed(123)  # for reproducibility
# final_data_sample <- final_data %>% sample_n(100000)

# # surely do the first step of this with ML methods?

# did_imputation(data = final_data_sample,
#             yname = "log_num_crimes",
#             gname = "first_treatment",
#             first_stage = ~ 0 | location + period,
#             tname = "period", 
#             idname = "location", 
#             pretrends = TRUE)

# "Error: std::bad_alloc"
# I don't think we have the memory for this

# maybe use RStata package and run it through Stata?







################################################################################################
################################################################################################






####################################################################
####################################################################
# notes
####################################################################
####################################################################


# notes:

# checking <- final_data[which(!is.na(final_data$NAME30)), ] is good to check things worked

# maybe should be using log crime count? as the distribution is heavily right skewed:
# to see this, plot num_crimes and log(1 + num_crimes)
#ggplot(final_data, aes(x = num_crimes)) + geom_histogram(binwidth = 1) + xlim(-1, 50)
#ggplot(final_data, aes(x = log(1 + num_crimes))) + geom_histogram(binwidth = 0.1) + xlim(-1, 5)


# we also want to interact the dummies with distance from the station, to determine the effect over distance - do this

# get controls in:
# - region x time (can't do unit x time as this would be collinear with treatment)
# - properties of the station/region (interacted with time)

# we also want to disaggregate by crime type - do this!

# we want to do the proper event study regression using new literature

# do a TWFE regression with inverse proximity weighting as our treatment, or other treatments, for robustness