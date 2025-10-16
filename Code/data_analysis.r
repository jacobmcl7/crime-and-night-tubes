# this file does the analysis

# import relevant packages
library(fixest)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(didimputation)

# set working directory
setwd("~/Economics/Papers (WIP)")

# load in the data
load("Crime and night tubes EXTRA DATA/final_data_new.RData")


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


########################################################
# define treatment!
########################################################


# we need to get the minimum distance of each station from a tube station on each of the lines

# to do this, first create a reshaped dataset with an ID for later merging

final_data <- final_data %>%
  mutate(location_id = row_number())  # create an ID to rejoin later

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


# note that the count data is heavily rightward skewed, but has some zeros: to address this, use log(1 + count), as done in e.g. Christensen et al (2024)
final_data <- final_data %>%
  mutate(log_num_crimes = log(1 + num_crimes))

# save the data
save(final_data, file = "Crime and night tubes EXTRA DATA/final_data__for_analysis.RData")


# check this all works!!!!!!!! THIS WAS DONE IN A RUSH






############################################################
############################################################
# Now do some analysis
############################################################
############################################################


# first under the baseline definition of treatment
# in particular, we call a region treated if it is at most 1km from an active night tube station - this will be the baseline definition

# load data
load("Crime and night tubes EXTRA DATA/final_data__for_analysis.RData")


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


##############################################################################


# first do a simple dynamic TWFE regression

# start by creating the required event-time dummies
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

# now do the regression, saving it to then be plotted
TWFE_1km <- feols(log_num_crimes ~ i(event_time, ref = -1) | location + Month, data = final_data)


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


# include region x time fixed effects and any other controls







####################################################################

# disaggregate the results by distance: interact each of the event time dummies with a distance variable

final_data <- final_data %>%

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

# now these can be used in a regression
TWFE_1km_disagg <- feols(log_num_crimes ~ i(event_time_dist_0_025, ref = -1) + i(event_time_dist_025_05, ref = -1) + i(event_time_dist_05_075, ref = -1) + i(event_time_dist_075_1, ref = -1) | location + Month, data = final_data)


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




####################################################################

# now use Abraham and Sun (2019) method

# same as before, but we use the sunab command in fixest
# note that our cohort variable is first_treatment, and the large value of this variable for never treated units is what the command wants

sunab_1km <- feols(log_num_crimes ~ sunab(first_treatment, period) | location + Month, data = final_data)

# plot, using feols plotting function
iplot(sunab_1km)

# incorporate this into my plotting function






####################################################################

# now use Borusyak et al imputation-based estimator

# make the appropriate edits to the first_treated variable for this to work

final_data <- final_data %>%
  mutate(first_treatment = ifelse(first_treatment > 100, NA, first_treatment))


# surely do the first step of this with ML methods?

did_imputation(data = final_data,
            yname = "log_num_crimes",
            gname = "first_treatment",
            first_stage = ~ 0 | location + period,
            tname = "period", 
            idname = "location", 
            pretrends = TRUE)

# "Error: std::bad_alloc"
# I don't think we have the memory for this



#####################################################################

# do it with controls

# we want:
# - properties of the station/region (interacted with time)










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
ggplot(final_data, aes(x = num_crimes)) + geom_histogram(binwidth = 1) + xlim(-1, 50)
ggplot(final_data, aes(x = log(1 + num_crimes))) + geom_histogram(binwidth = 0.1) + xlim(-1, 5)


# we also want to interact the dummies with distance from the station, to determine the effect over distance - do this

# get controls in:
# - region x time (can't do unit x time as this would be collinear with treatment)
# - properties of the station/region (interacted with time)

# we also want to disaggregate by crime type - do this!

# we want to do the proper event study regression using new literature

# do a TWFE regression with inverse proximity weighting as our treatment, or other treatments, for robustness