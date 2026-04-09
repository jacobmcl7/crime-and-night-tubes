########################################################
# outline
########################################################

# this file does the main data analysis for the paper
# the sequence of analysis is as follows:

# 1) TWFE, 1km treatment, on total crime count (log(+1))
#   1a) varied distances
# 2) A+S (2021), 1km treatment, on total crime count
#  2a) varied distances
# 3) BJS (2024), 1km treatment, on total crime count
# 4) TWFE with controls
# 5) TWFE disaggregated by distance bands
# 6) TWFE disaggregated by wealth of area
# 7) TWFE disaggregated by proximity to a red line station
# 8) TWFE for individual crime types - theft from the person
#   8a) A+S for theft
#   8b) TWFE disaggregation by distance bands
#   8c) BJS (2024) for theft
# 9) TWFE for individual crime types - robbery
#   9a) A+S for robbery
#   9b) TWFE disaggregation by distance bands
#   9c) BJS (2024) for robbery
# 10) TWFE for all crime types, plotted together
#   10a) same but with A+S
#   10b) same but with BJS
# 11) Poisson ETWFE regression for the main effect
#   11a) same but for theft from the person
#   11b) same but for robbery
# 12) non-parametric estimation of distance-decay of ATT
#   12a) same but split by six month periods
#   12b) same but split by three month periods
#   12c) now do it with the residuals from a Poisson regression
# 13) BJS (2024) for the difference in evolution in rich vs poor areas
#   13a) same but for all crime types, plotted together




########################################################
# setup
########################################################

# import relevant packages
library(fixest)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(didimputation)
library(KernSmooth) # for kernel regression
library(etwfe)
library(mgcv)
library(plm)


# set working directory
setwd("~/Economics/Papers (WIP)")

# load in the data
load("Crime and night tubes EXTRA DATA/final_data.RData")
# load("Crime and night tubes EXTRA DATA/house_data_cleaned.RData")


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
    theme_bw()
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


# finally, write a function that imports the csv results from estimation with Borusyak et al (2024) in Stata
load_bjs_results <- function(filepath, string) {

  # read the file in
  read.csv(filepath) %>%

    # remove the first row
    slice(-1) %>%

    # name the variables
    setNames(c("event_time", "coef", "se")) %>%

    # clean up the variables, using the string argument to remove prefixes for tau
    mutate(
      event_time = ifelse(grepl("^tau", event_time),
                          as.numeric(sub(string, "", event_time)),
                          -as.numeric(sub("pre", "", event_time))),
      coef = as.numeric(coef),
      se = as.numeric(se)
    )
}


############################################################
# prepare the data for analysis
############################################################


# first we define treatment and event time variables
# in the baseline case, we call a location treated if it is at most 1km from an active night tube station

# use the functions above to create treatment and event time variables, and distance band variables
final_data <- define_treatment_event_time(distance = 1, data = final_data)
final_data <- define_distance_bands(thresholds = c(0, 0.25, 0.5, 0.75, 1), distance = 1, data = final_data)
# note that the distance bands are: 0-0.25km, 0.25-0.5km, 0.5-0.75km, 0.75-1km

# do the same for other distances
final_data <- define_treatment_event_time(distance = 0.5, data = final_data)
final_data <- define_treatment_event_time(distance = 0.75, data = final_data)
final_data <- define_treatment_event_time(distance = 1.25, data = final_data)

# now do the exact same for the house price data
# house_data <- define_treatment_event_time(distance = 1, data = house_data)
# house_data <- define_distance_bands(thresholds = c(0, 0.25, 0.5, 0.75, 1), distance = 1, data = house_data)


# also, filter only for locations within 2km of a station, for representativeness
final_data <- final_data %>%
  filter(min_any_dist < 2)
  # move to the post-geocoding script?

# filter now the house data for only for locations within 2km of a station (NOTE - I think it already satisfies this. Check the post-geocoding processing)
# house_data <- house_data %>%
#   filter(min_any_dist < 2)



# write this to a csv to be loaded into stata
# write.csv(final_data, "Crime and night tubes EXTRA DATA/final_data_for_stata.csv", row.names = FALSE)




















############################################################
# now do some analysis
############################################################


# 1) TWFE event study regression, 1km treatment, on total crime count

# do the regression, saving it to then be plotted
TWFE_1km <- feols(log_num_crimes ~ i(event_time_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km, substring = "event_time_1")

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

# 1a) now do it again for different distances

for (dist in c(0.5, 0.75, 1.25)) {

  event_time_var <- paste0("event_time_", dist)
  
  formula_str <- paste0("log_num_crimes ~ i(", event_time_var, ", ref = -1) | location + Month")
  
  TWFE <- feols(as.formula(formula_str), data = final_data, cluster = "location")
  
  coefs <- plot_prepare(TWFE, substring = paste0("event_time_", dist))

  p <- plot(coefs = coefs,
      xsequence = seq(-20, 15, 5),
      ymin = -0.1,
      ymax = 0.1,
      title = paste0("Dynamic TWFE results - Distance threshold: ", dist, "km"), 
      note = paste0("Simple treatment definition, theshold = ", dist, "km"))

  # save it
  ggsave(paste0("Crime and night tubes/Output/Results/TWFE_", dist, "km.png"), plot = p, width = 8, height = 6)
}







####################################################################

# 2) now use Abraham and Sun (2021) method to do the above

# this works the same as before, but we use the sunab command in fixest
# note that our cohort variable is first_treatment_1, and the large value of this variable for never treated units is what the command wants

# do the regression
sunab_1km <- feols(log_num_crimes ~ sunab(first_treatment_1, period) | location + Month, data = final_data, cluster = "location")

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

# 2a) do it again for treatment defined at different distances, like above

for (dist in c(0.5, 0.75, 1.25)) {

  first_treatment_var <- paste0("first_treatment_", dist)
  
  formula_str <- paste0("log_num_crimes ~ sunab(", first_treatment_var, ", period) | location + Month")
  
  sunab <- feols(as.formula(formula_str), data = final_data, cluster = "location")
  
  coefs <- plot_prepare2(sunab, omitted_pd = -1)

  p <- plot(coefs = coefs,
      xsequence = seq(-20, 15, 5),
      ymin = -0.1,
      ymax = 0.1,
      title = paste0("Dynamic Sun and Abraham (2021) results - Distance threshold: ", dist, "km"), 
      note = paste0("Simple treatment definition, theshold = ", dist, "km"))

  # save it
  ggsave(paste0("Crime and night tubes/Output/Results/Sunab_", dist, "km.png"), plot = p, width = 8, height = 6)
}




####################################################################

# 3) now do it with the Borusyak, Jaravel and Spiess (2024) estimator

# load in the results from csv
coefs <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_all.csv", "tau")

# plot the results
plot(coefs = coefs, 
    xsequence = seq(-10, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "BJS (2024) - All Crimes", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_all_crimes.png", width = 8, height = 6)









####################################################################

# 4) now do regression 1) with some extra controls

# allow the effect of all the controls to vary by month
# don't include crime rank controls for now

TWFE_1km_controls <- feols(log_num_crimes ~ i(event_time_1, ref = -1) + i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = final_data, cluster = "location")

# no longer negative definite covariance matrix!!
# WORK OUT WHY
# maybe too little variation left? Use yearly interactions maybe
  # no - not helpful

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_controls, substring = "event_time_1")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5),
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results with controls",
    note = "Simple treatment definition, theshold = 1km, controls added")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_controls.png", width = 8, height = 6)

# note: adding time x region controls seems to restrict variation too much, leading to lost significance

####################################################################

# 4a) now with different thresholds

for (dist in c(0.5, 0.75, 1.25)) {

  event_time_var <- paste0("event_time_", dist)
  
  formula_str <- paste0("log_num_crimes ~ i(", event_time_var, ", ref = -1) + i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score)| location + Month")
  
  TWFE <- feols(as.formula(formula_str), data = final_data, cluster = "location")
  
  coefs <- plot_prepare(TWFE, substring = paste0("event_time_", dist))

  p <- plot(coefs = coefs,
      xsequence = seq(-20, 15, 5),
      ymin = -0.1,
      ymax = 0.1,
      title = paste0("Dynamic TWFE results - Distance threshold: ", dist, "km"), 
      note = paste0("Simple treatment definition, theshold = ", dist, "km, controls added"))

  # save it
  ggsave(paste0("Crime and night tubes/Output/Results/TWFE_", dist, "km_controls.png"), plot = p, width = 8, height = 6)
}


####################################################################

# 4b) now for thefts individually

TWFE_1km_controls_theft <- feols(log_theft_from_the_person ~ i(event_time_1, ref = -1) + i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_controls_theft, substring = "event_time_1")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5),
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results with controls: theft",
    note = "Simple treatment definition, theshold = 1km, controls added")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_controls_theft.png", width = 8, height = 6)


####################################################################

# 4c) now for robberies individually

TWFE_1km_controls_robbery <- feols(log_robbery ~ i(event_time_1, ref = -1) + i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_controls_robbery, substring = "event_time_1")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5),
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results with controls: robbery",
    note = "Simple treatment definition, theshold = 1km, controls added")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_controls_robbery.png", width = 8, height = 6)







####################################################################

# 5) now disaggregate the results from 1) by distance

# to do this, interact each of the event time dummies with a distance variable

# now these can be used in a regression
TWFE_1km_disagg <- feols(log_num_crimes ~ i(event_time_dist_0_0.25, ref = -1) + i(event_time_dist_0.25_0.5, ref = -1) + i(event_time_dist_0.5_0.75, ref = -1) + i(event_time_dist_0.75_1, ref = -1) | location + Month, data = final_data, cluster = "location")


# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_0_0.25")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_0.25_0.5")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_0.5_0.75")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg, substring = "event_time_dist_0.75_1")

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

# 6) disaggregate the effect from 1) by wealth of the region

# first use the IMD to create a high/low wealth dummy for all observations for which first_treatment_1 < Inf
# do this so the median is only calculated over treated locations
imd_median <- median(final_data$IMD[final_data$first_treatment_1 < Inf], na.rm = TRUE)

# now create the dummy
final_data <- final_data %>%
  mutate(high_wealth := ifelse(IMD < imd_median & first_treatment_1 < Inf, 1, 0))

# now create event-time variables for rich and poor treated regions
final_data <- final_data %>%
  mutate(event_time_rich := ifelse(high_wealth == 1, event_time_1, -1)) %>%
  mutate(event_time_poor := ifelse(high_wealth == 0 & first_treatment_1 < Inf, event_time_1, -1))

# now these can be used in a regression
TWFE_1km_disagg <- feols(log_theft_from_the_person ~ i(event_time_rich, ref = -1) + i(event_time_poor, ref = -1) | location + Month, data = final_data, cluster = "location")
# 468 NAs, presumably due to missing imd_median (i.e. missing LSOA)

# now prepare the coefficients for plotting
coefs_rich <- plot_prepare(TWFE_1km_disagg, substring = "event_time_rich")
coefs_poor <- plot_prepare(TWFE_1km_disagg, substring = "event_time_poor")

# plot them, side by side
p1 <- plot(coefs = coefs_rich, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.1,
           ymax = 0.1,
           title = "High Wealth Areas")

p2 <- plot(coefs = coefs_poor, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.1,
           ymax = 0.1,
           title = "Low Wealth Areas")

# now combine them into a grid
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results, disaggregated by area wealth',
  caption = 'Basic treatment definition, threshold = 1km')

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_wealth.png", width = 12, height = 6)







#####################################################################

# 7) now disaggregate the results from 1) according to proximity to a red station

# first create a dummy for being close to a red line station
final_data <- final_data %>%
  mutate(close_red = ifelse(!is.na(min_red_dist) & min_red_dist < 1 & first_treatment_1 < Inf, 1, 0))

# now create event-time variables for those close to red stations and those not
final_data <- final_data %>%
  mutate(event_time_red = ifelse(close_red == 1, event_time_1, -1)) %>%
  mutate(event_time_not_red = ifelse(close_red == 0 & first_treatment_1 < Inf, event_time_1, -1))

# now these can be used in a regression
TWFE_1km_disagg_theft <- feols(log_theft_from_the_person ~ i(event_time_red, ref = -1) + i(event_time_not_red, ref = -1) | location + Month, data = final_data, cluster = "location")

# now prepare the coefficients for plotting
coefs_red <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_red")
coefs_not_red <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_not_red")

# plot them, side by side
p1 <- plot(coefs = coefs_red, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.1,
           ymax = 0.1,
           title = "Close to Red Stations")
p2 <- plot(coefs = coefs_not_red, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.1,
           ymax = 0.1,
           title = "Not Close to Red Stations")

# now combine them into a grid
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results, disaggregated by proximity to Red stations',
  caption = 'Basic treatment definition, threshold = 1km, outcome = log(number of THEFTS)')

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_red_thefts.png", width = 12, height = 6)


#####################################################################

# 7a) do the same for robberies specifically 

# ADD TO THE CONTENTS, OR DELETE

TWFE_1km_disagg_robbery <- feols(log_robbery ~ i(event_time_red, ref = -1) + i(event_time_not_red, ref = -1) | location + Month, data = final_data, cluster = "location")

# now prepare the coefficients for plotting
coefs_red <- plot_prepare(TWFE_1km_disagg_robbery, substring = "event_time_red")
coefs_not_red <- plot_prepare(TWFE_1km_disagg_robbery, substring = "event_time_not_red")

# plot them, side by side
p1 <- plot(coefs = coefs_red, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.1,
           ymax = 0.1,
           title = "Close to Red Stations")
p2 <- plot(coefs = coefs_not_red, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.1,
           ymax = 0.1,
           title = "Not Close to Red Stations")

# now combine them into a grid
p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'TWFE results, disaggregated by proximity to Red stations',
  caption = 'Basic treatment definition, threshold = 1km, outcome = log(number of ROBBERIES)')

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_red_robberies.png", width = 12, height = 6)







#####################################################################

# 8) now do the analysis for individual crime types - first theft from the person

# start with the basic TWFE regression

# do the regression, saving it to then be plotted
TWFE_1km_theft <- feols(log_theft_from_the_person ~ i(event_time_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_theft, substring = "event_time_1")

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

# 8a) now do it with A+S, as in 2)

# same as before, but we use the sunab command in fixest
sunab_1km_theft <- feols(log_theft_from_the_person ~ sunab(first_treatment_1, period) | location + Month, data = final_data, cluster = "location")
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

# 8b) now do it with varying distance thresholds, as in 5)

# run the regression
TWFE_1km_disagg_theft <- feols(log_theft_from_the_person ~ i(event_time_dist_0_0.25, ref = -1) + i(event_time_dist_0.25_0.5, ref = -1) + i(event_time_dist_0.5_0.75, ref = -1) + i(event_time_dist_0.75_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_0_0.25")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_0.25_0.5")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_0.5_0.75")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg_theft, substring = "event_time_dist_0.75_1")

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

# 8c) now do it with BJS (2024)

# load in the results from csv
coefs <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_theft.csv", "tau")

# plot the results
plot(coefs = coefs, 
    xsequence = seq(-10, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "BJS (2024) - Theft from the Person", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_theft.png", width = 8, height = 6)








####################################################################

# 9) now do it for robbery - first TWFE

# do the regression, saving it to then be plotted
TWFE_1km_robbery <- feols(log_robbery ~ i(event_time_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_robbery, substring = "event_time_1")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results - Robbery", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_robbery.png", width = 8, height = 6)

####################################################################

# 9a) now with A+S

sunab_1km_robbery <- feols(log_robbery ~ sunab(first_treatment_1, period) | location + Month, data = final_data, cluster = "location")

# prepare for plotting
coefs <- plot_prepare2(sunab_1km_robbery, omitted_pd = -1)

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic Sun and Abraham (2021) results - Robbery", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/Sunab_1km_robbery.png", width = 8, height = 6)

####################################################################

# 9b) now do it with varied distance thresholds

# run the regression
TWFE_1km_disagg_robbery <- feols(log_robbery ~ i(event_time_dist_0_0.25, ref = -1) + i(event_time_dist_0.25_0.5, ref = -1) + i(event_time_dist_0.5_0.75, ref = -1) + i(event_time_dist_0.75_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# now prepare the coefficients for plotting
coefs_0_025 <- plot_prepare(TWFE_1km_disagg_robbery, substring = "event_time_dist_0_0.25")
coefs_025_05 <- plot_prepare(TWFE_1km_disagg_robbery, substring = "event_time_dist_0.25_0.5")
coefs_05_075 <- plot_prepare(TWFE_1km_disagg_robbery, substring = "event_time_dist_0.5_0.75")
coefs_075_1 <- plot_prepare(TWFE_1km_disagg_robbery, substring = "event_time_dist_0.75_1")

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
  title = 'TWFE results for robbery, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km')

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_disagg_robbery.png", width = 12, height = 8)


####################################################################

# 9c) now do it with BJS (2024)

# load in the results from csv
coefs <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_robbery.csv", "tau")

# plot the results
plot(coefs = coefs, 
    xsequence = seq(-10, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "BJS (2024) - Robbery", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_robbery.png", width = 8, height = 6)


####################################################################

# 9d) now do TWFE for the sum of thefts and robberies

# create the variable for the sum of thefts and robberies
final_data <- final_data %>%
  mutate(log_theft_and_robbery = log(theft_from_the_person + robbery + 1))

# run the regression
TWFE_1km_theft_and_robbery <- feols(log_theft_and_robbery ~ i(event_time_1, ref = -1) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_theft_and_robbery, substring = "event_time_1")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.1,
    ymax = 0.1,
    title = "Dynamic TWFE results - Theft and Robbery", 
    note = "Simple treatment definition, theshold = 1km")

# save it
ggsave("Crime and night tubes/Output/Results/TWFE_1km_theft_and_robbery.png", width = 8, height = 6)


#####################################################################

# 9e) now do BJS for the sum of thefts and robberies

# load in the results from csv
coefs <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_theft_and_robbery.csv", "tau")

# plot the results
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5),
    ymin = -0.1,
    ymax = 0.1,
    title = "BJS (2024) - Theft and Robbery", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_theft_and_robbery.png", width = 8, height = 6)


#####################################################################

# 9f) now do it disaggregated by distance for the sum of thefts and robberies, with BJS

# load in the results from csv
coefs_0_025 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_1.csv", "tau_W")
coefs_025_05 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_2.csv", "tau_W")
coefs_05_075 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_3.csv", "tau_W")
coefs_075_1 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_4.csv", "tau_W")

# plot them, in a 2x2 grid
# first create the plots
p1 <- plot(coefs = coefs_0_025, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.05,
           ymax = 0.15,
           title = "0 to 0.25km")
p2 <- plot(coefs = coefs_025_05, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.05,
           ymax = 0.15,
           title = "0.25 to 0.5km")
p3 <- plot(coefs = coefs_05_075, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.05,
           ymax = 0.15,
           title = "0.5 to 0.75km")
p4 <- plot(coefs = coefs_075_1, 
           xsequence = seq(-20, 15, 5),
           ymin = -0.05,
           ymax = 0.15,
           title = "0.75 to 1km")

# now combine them into a grid
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(
  title = 'BJS (2024) results for theft and robbery, disaggregated by distance',
  caption = 'Basic treatment definition, threshold = 1km')

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_disagg_theft_and_robbery.png", width = 12, height = 8)


# now visualise them all on the same set of axes, also removing pre-trends because they are all the same in BJS for different weight vectors

# keep all observations in coefs such that event_time >= 0
coefs_0_025 <- coefs_0_025 %>% filter(event_time >= 0)
coefs_025_05 <- coefs_025_05 %>% filter(event_time >= 0)
coefs_05_075 <- coefs_05_075 %>% filter(event_time >= 0)
coefs_075_1 <- coefs_075_1 %>% filter(event_time >= 0)

# stack them together into the same dataframe, merged by event_time
coefs_all <- coefs_0_025 %>%
  select(event_time, coef, se) %>%
  rename(coef_0_025 = coef, se_0_025 = se) %>%
  left_join(coefs_025_05 %>% select(event_time, coef, se) %>% rename(coef_025_05 = coef, se_025_05 = se), by = "event_time") %>%
  left_join(coefs_05_075 %>% select(event_time, coef, se) %>% rename(coef_05_075 = coef, se_05_075 = se), by = "event_time") %>%
  left_join(coefs_075_1 %>% select(event_time, coef, se) %>% rename(coef_075_1 = coef, se_075_1 = se), by = "event_time")

# now plot them all together, with different colours for each distance band, and error bars for the confidence intervals
band_colours <- c(
  "0 to 0.25km"    = "#1B3A6B",  # navy blue
  "0.25 to 0.5km"  = "#2E86AB",  # steel blue
  "0.5 to 0.75km"  = "#7DC88A",  # sage green
  "0.75 to 1km"    = "#A8C45A"   # medium yellow-green
)

ggplot(coefs_all, aes(x = event_time)) +
  # Reference line at 0
  geom_hline(yintercept = 0,, colour = "black", linewidth = 0.75) +
  # Ribbons (fill mapped to same labels as colour)
  geom_ribbon(aes(ymin = coef_0_025 - 1.96 * se_0_025,
                  ymax = coef_0_025 + 1.96 * se_0_025,
                  fill = "0 to 0.25km"), alpha = 0.15) +
  geom_ribbon(aes(ymin = coef_025_05 - 1.96 * se_025_05,
                  ymax = coef_025_05 + 1.96 * se_025_05,
                  fill = "0.25 to 0.5km"), alpha = 0.15) +
  geom_ribbon(aes(ymin = coef_05_075 - 1.96 * se_05_075,
                  ymax = coef_05_075 + 1.96 * se_05_075,
                  fill = "0.5 to 0.75km"), alpha = 0.15) +
  geom_ribbon(aes(ymin = coef_075_1 - 1.96 * se_075_1,
                  ymax = coef_075_1 + 1.96 * se_075_1,
                  fill = "0.75 to 1km"), alpha = 0.15) +
  # Lines
  geom_line(aes(y = coef_0_025,  colour = "0 to 0.25km"),   linewidth = 0.8) +
  geom_line(aes(y = coef_025_05, colour = "0.25 to 0.5km"), linewidth = 0.8) +
  geom_line(aes(y = coef_05_075, colour = "0.5 to 0.75km"), linewidth = 0.8) +
  geom_line(aes(y = coef_075_1,  colour = "0.75 to 1km"),   linewidth = 0.8) +
  # Points
  geom_point(aes(y = coef_0_025,  colour = "0 to 0.25km"),   size = 1.8) +
  geom_point(aes(y = coef_025_05, colour = "0.25 to 0.5km"), size = 1.8) +
  geom_point(aes(y = coef_05_075, colour = "0.5 to 0.75km"), size = 1.8) +
  geom_point(aes(y = coef_075_1,  colour = "0.75 to 1km"),   size = 1.8) +
  # Scales
  scale_colour_manual(name = "Distance Band", values = band_colours) +
  scale_fill_manual(name = "Distance Band", values = band_colours) +
  labs(
    title   = "BJS (2024) results for theft and robbery, disaggregated by distance",
    caption = "Basic treatment definition, threshold = 1km",
    x = "Event Time",
    y = "Coefficient"
  ) +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_disagg_theft_and_robbery_combined.png", width = 8, height = 6)


###################################################################

# 9g) now do it for thefts and robberies with controls

# first with BJS

# load in the results from csv
coefs <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_theft_and_robbery_controls.csv", "tau")

# plot the results
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "BJS (2024) - Theft and Robbery with Controls", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_theft_and_robbery_controls.png", width = 8, height = 6)


# now with TWFE

# create the variable
final_data <- final_data %>%
  mutate(log_theft_and_robbery = log(theft_from_the_person + robbery + 1)) # add 1 to avoid log(0)

# run the regression with all controls
TWFE_1km_theft_and_robbery_all_controls <- feols(log_theft_and_robbery ~ i(event_time_1, ref = -1) + i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs_all <- plot_prepare(TWFE_1km_theft_and_robbery_all_controls, substring = "event_time_1")

# run it with IMD controls
TWFE_1km_theft_and_robbery_IMD_controls <- feols(log_theft_and_robbery ~ i(event_time_1, ref = -1) + i(Month, IMD) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs_IMD <- plot_prepare(TWFE_1km_theft_and_robbery_IMD_controls, substring = "event_time_1")

# run it with constructed controls
TWFE_1km_theft_and_robbery_constructed_controls <- feols(log_theft_and_robbery ~ i(event_time_1, ref = -1) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs_constructed <- plot_prepare(TWFE_1km_theft_and_robbery_constructed_controls, substring = "event_time_1")

# plot the graphs
plot(coefs = coefs_all, 
     xsequence = seq(-20, 15, 5), 
     ymin = -0.1,
     ymax = 0.1,
     title = "TWFE - Theft and Robbery with All Controls", 
     note = "Simple treatment definition, theshold = 1km")
plot(coefs = coefs_IMD,
    xsequence = seq(-20, 15, 5), 
    ymin = -0.1,
    ymax = 0.1,
    title = "TWFE - Theft and Robbery with IMD Controls", 
    note = "Simple treatment definition, theshold = 1km")
plot(coefs = coefs_constructed,
    xsequence = seq(-20, 15, 5), 
    ymin = -0.1,
    ymax = 0.1,
    title = "TWFE - Theft and Robbery with Constructed Controls", 
    note = "Simple treatment definition, theshold = 1km")

# save it
# DO THIS ONCE DONE PROPERLY


########################################################################

# 9h) now do it with BJS by distance with controls

# load in the results from csv
coefs_0_025 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_1_controls.csv", "tau_W")
coefs_025_05 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_2_controls.csv", "tau_W")
coefs_05_075 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_3_controls.csv", "tau_W")
coefs_075_1 <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_distance_band_4_controls.csv", "tau_W")

# keep all observations in coefs such that event_time >= 0
coefs_0_025 <- coefs_0_025 %>% filter(event_time >= 0)
coefs_025_05 <- coefs_025_05 %>% filter(event_time >= 0)
coefs_05_075 <- coefs_05_075 %>% filter(event_time >= 0)
coefs_075_1 <- coefs_075_1 %>% filter(event_time >= 0)

# stack them together into the same dataframe, merged by event_time
coefs_all <- coefs_0_025 %>%
  select(event_time, coef, se) %>%
  rename(coef_0_025 = coef, se_0_025 = se) %>%
  left_join(coefs_025_05 %>% select(event_time, coef, se) %>% rename(coef_025_05 = coef, se_025_05 = se), by = "event_time") %>%
  left_join(coefs_05_075 %>% select(event_time, coef, se) %>% rename(coef_05_075 = coef, se_05_075 = se), by = "event_time") %>%
  left_join(coefs_075_1 %>% select(event_time, coef, se) %>% rename(coef_075_1 = coef, se_075_1 = se), by = "event_time")

# now plot them all together, with different colours for each distance band, and error bars for the confidence intervals
band_colours <- c(
  "0 to 0.25km"    = "#1B3A6B",  # navy blue
  "0.25 to 0.5km"  = "#2E86AB",  # steel blue
  "0.5 to 0.75km"  = "#7DC88A",  # sage green
  "0.75 to 1km"    = "#A8C45A"   # medium yellow-green
)

ggplot(coefs_all, aes(x = event_time)) +
  # Reference line at 0
  geom_hline(yintercept = 0,, colour = "black", linewidth = 0.75) +
  # Ribbons (fill mapped to same labels as colour)
  geom_ribbon(aes(ymin = coef_0_025 - 1.96 * se_0_025,
                  ymax = coef_0_025 + 1.96 * se_0_025,
                  fill = "0 to 0.25km"), alpha = 0.15) +
  geom_ribbon(aes(ymin = coef_025_05 - 1.96 * se_025_05,
                  ymax = coef_025_05 + 1.96 * se_025_05,
                  fill = "0.25 to 0.5km"), alpha = 0.15) +
  geom_ribbon(aes(ymin = coef_05_075 - 1.96 * se_05_075,
                  ymax = coef_05_075 + 1.96 * se_05_075,
                  fill = "0.5 to 0.75km"), alpha = 0.15) +
  geom_ribbon(aes(ymin = coef_075_1 - 1.96 * se_075_1,
                  ymax = coef_075_1 + 1.96 * se_075_1,
                  fill = "0.75 to 1km"), alpha = 0.15) +
  # Lines
  geom_line(aes(y = coef_0_025,  colour = "0 to 0.25km"),   linewidth = 0.8) +
  geom_line(aes(y = coef_025_05, colour = "0.25 to 0.5km"), linewidth = 0.8) +
  geom_line(aes(y = coef_05_075, colour = "0.5 to 0.75km"), linewidth = 0.8) +
  geom_line(aes(y = coef_075_1,  colour = "0.75 to 1km"),   linewidth = 0.8) +
  # Points
  geom_point(aes(y = coef_0_025,  colour = "0 to 0.25km"),   size = 1.8) +
  geom_point(aes(y = coef_025_05, colour = "0.25 to 0.5km"), size = 1.8) +
  geom_point(aes(y = coef_05_075, colour = "0.5 to 0.75km"), size = 1.8) +
  geom_point(aes(y = coef_075_1,  colour = "0.75 to 1km"),   size = 1.8) +
  # Scales
  scale_colour_manual(name = "Distance Band", values = band_colours) +
  scale_fill_manual(name = "Distance Band", values = band_colours) +
  labs(
    title   = "BJS (2024) results for theft and robbery, disaggregated by distance",
    caption = "Basic treatment definition, threshold = 1km",
    x = "Event Time",
    y = "Coefficient"
  ) +
  theme_bw()

# save it
ggsave("Crime and night tubes/Output/Results/BJS_1km_disagg_theft_and_robbery_controls_combined.png", width = 8, height = 6)



















####################################################################

# 10) now do the TWFE regression from 1) for all crimes separately, then plot them all together

crime_types <- c("violence_and_sexual_offences", "vehicle_crime", "other_theft", "burglary",                    
 "anti-social_behaviour", "shoplifting", "criminal_damage_and_arson", "other_crime",                 
 "possession_of_weapons", "bicycle_theft", "drugs", "public_order",                
"theft_from_the_person", "robbery")

for (crime in crime_types) {
  
  # create the formula
  formula <- as.formula(paste0("`log_", crime, "` ~ i(event_time_1, ref = -1) | location + Month"))
  
  # run the regression
  model <- feols(formula, data = final_data, cluster = "location")
  
  # prepare the coefficients for plotting
  coefs <- plot_prepare(model, substring = "event_time_1")
  
  # save and plot the graph
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-20, 15, 5), 
      ymin = -0.05,
      ymax = 0.05,
      title = paste0("TWFE results - ", gsub("_", " ", crime)), 
      note = "Simple treatment definition, theshold = 1km"))
  # now print the plot in the loop
  print(get(paste0("plot_", crime)))
  
  # print a message to indicate completion
  print(paste0("Done for ", crime))
  
}

# now plot them all in one big grid
(plot_violence_and_sexual_offences + plot_vehicle_crime + plot_other_theft + plot_burglary +
 `plot_anti-social_behaviour` + plot_shoplifting + plot_criminal_damage_and_arson + plot_other_crime +
 plot_possession_of_weapons + plot_bicycle_theft + plot_drugs + plot_public_order +
 plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 5)

# save this
ggsave("Crime and night tubes/Output/Results/TWFE_1km_crimes_grid.png", width = 22, height = 12)


####################################################################

# 10a) do this with A+S too instead of TWFE

for (crime in crime_types) {
  
  # create the formula
  formula <- as.formula(paste0("`log_", crime, "` ~ sunab(first_treatment_1, period) | location + Month"))
  
  # run the regression
  model <- feols(formula, data = final_data, cluster = "location")
  
  # prepare the coefficients for plotting
  coefs <- plot_prepare2(model, omitted_pd = -1)
  
  # save and plot the graph
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-20, 15, 5), 
      ymin = -0.05,
      ymax = 0.05,
      title = paste0("S&A (2021) results - ", gsub("_", " ", crime)), 
      note = "Simple treatment definition, theshold = 1km"))

  # now print the plot in the loop
  print(get(paste0("plot_", crime)))
  
  # save it
  # ggsave(paste0("Crime and night tubes/Output/Results/loop_Sunab_1km_", crime, ".png"), width = 8, height = 6)

  # print a message to indicate completion
  print(paste0("Done for ", crime))

}

# now plot them all in one big grid
(plot_violence_and_sexual_offences + plot_vehicle_crime + plot_other_theft + plot_burglary +
 `plot_anti-social_behaviour` + plot_shoplifting + plot_criminal_damage_and_arson + plot_other_crime +
 plot_possession_of_weapons + plot_bicycle_theft + plot_drugs + plot_public_order +
 plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 5)

# save this too
ggsave("Crime and night tubes/Output/Results/Sunab_1km_crimes_grid.png", width = 22, height = 12)

####################################################################

# 10b) do it with BJS (2024)

# edit crime_types to get the name for antisocial behaviour right for the csv
crime_types_bjs <- c("violence_and_sexual_offences", "vehicle_crime", "other_theft", "burglary",                    
 "antisocial_behaviour", "shoplifting", "criminal_damage_and_arson", "other_crime",                 
 "possession_of_weapons", "bicycle_theft", "drugs", "public_order", "theft_from_the_person", "robbery")

for (crime in crime_types_bjs) {

  coefs <- load_bjs_results(paste0("Crime and night tubes EXTRA DATA/BJS results/BJS_results_log_", crime, ".csv"), "tau")

  # plot the results
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-10, 15, 5), 
      ymin = -0.05,
      ymax = 0.05,
      title = paste0("BJS (2024) - ", gsub("_", " ", crime)), 
      note = "Simple treatment definition, theshold = 1km"))
  
  # print a message to indicate completion
  print(paste0("Done for ", crime))

}

# now plot them all in one big grid
(plot_violence_and_sexual_offences + plot_vehicle_crime + plot_other_theft + plot_burglary +
 plot_antisocial_behaviour + plot_shoplifting + plot_criminal_damage_and_arson + plot_other_crime +
 plot_possession_of_weapons + plot_bicycle_theft + plot_drugs + plot_public_order +
 plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 5)

# save this too
ggsave("Crime and night tubes/Output/Results/BJS_1km_crimes_grid.png", width = 22, height = 12)


####################################################################

# 10d) now do it with controls for all crimes with TWFE

crime_types <- c("violence_and_sexual_offences", "vehicle_crime", "other_theft", "burglary",                    
 "anti-social_behaviour", "shoplifting", "criminal_damage_and_arson", "other_crime",                 
 "possession_of_weapons", "bicycle_theft", "drugs", "public_order",                
"theft_from_the_person", "robbery")

for (crime in crime_types) {
  
  # create the formula
  formula_imd_constructed <- as.formula(paste0("`log_", crime, "` ~ i(event_time_1, ref = -1) + i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month"))
  
  # run the regression
  model <- feols(formula_imd_constructed, data = final_data, cluster = "location")
  
  # prepare the coefficients for plotting
  coefs <- plot_prepare(model, substring = "event_time_1")
  
  # save and plot the graph
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-20, 15, 5), 
      ymin = -0.05,
      ymax = 0.05,
      title = gsub("_", " ", crime),
      note = "Simple treatment definition, theshold = 1km"))
  # now print the plot in the loop
  print(get(paste0("plot_", crime)))
  
  # print a message to indicate completion
  print(paste0("Done for ", crime))
  
}

# now plot them all in one big grid
(plot_violence_and_sexual_offences + plot_vehicle_crime + plot_other_theft + plot_burglary +
 `plot_anti-social_behaviour` + plot_shoplifting + plot_criminal_damage_and_arson + plot_other_crime +
 plot_possession_of_weapons + plot_bicycle_theft + plot_drugs + plot_public_order +
 plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 5)

# save this
ggsave("Crime and night tubes/Output/Results/TWFE_1km_crimes_grid_controls_constructed_imd.png", width = 22, height = 12)


# now do it with all possible crimes

for (crime in crime_types) {
  
  # create the formula
  formula_imd_constructed <- as.formula(paste0("`log_", crime, "` ~ i(event_time_1, ref = -1) + i(Month, IMD) + i(Month, income_rank) + i(Month, education_rank) + i(Month, health_rank) + i(Month, barriers_rank) + i(Month, living_env_rank) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month"))
  
  # run the regression
  model <- feols(formula_imd_constructed, data = final_data, cluster = "location")
  
  # prepare the coefficients for plotting
  coefs <- plot_prepare(model, substring = "event_time_1")
  
  # save and plot the graph
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-20, 15, 5), 
      ymin = -0.05,
      ymax = 0.05,
      title = gsub("_", " ", crime),
      note = "Simple treatment definition, theshold = 1km"))
  # now print the plot in the loop
  print(get(paste0("plot_", crime)))
  
  # print a message to indicate completion
  print(paste0("Done for ", crime))
  
}

# now plot them all in one big grid
(plot_violence_and_sexual_offences + plot_vehicle_crime + plot_other_theft + plot_burglary +
 `plot_anti-social_behaviour` + plot_shoplifting + plot_criminal_damage_and_arson + plot_other_crime +
 plot_possession_of_weapons + plot_bicycle_theft + plot_drugs + plot_public_order +
 plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 5)

# save this
ggsave("Crime and night tubes/Output/Results/TWFE_1km_crimes_grid_controls_all.png", width = 22, height = 12)










####################################################################

# 11) estimate the effect with a Poisson regression

# to do this, we use the etwfe package (from Wooldridge, 2023)

# we must regress on a set of saturated controls, as implicitly done here
TWFE_1km_poisson <- etwfe(
  fml = num_crimes ~ 1,
  tvar = period,
  gvar = first_treatment_1,
  data = final_data,
  vcov = ~location, 
  family = "poisson",
  cgroup = "never"
)

# understand why dummies are being dropped! This happens in the etwfe guide too though, so maybe isn't a problem

# prepare the coefficients for plotting, using the emfx command from etwfe
coefs <- as.data.frame(emfx(TWFE_1km_poisson, type = "event")) %>%
  rename(event_time = event) %>%
  rename(coef = estimate) %>%
  rename(se = std.error) %>%
  # add one at event time -1, with coef = 0 and se = 0, to represent the omitted category
  add_row(event_time = -1, coef = 0, se = 0)

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.25,
    ymax = 0.25,
    title = "Dynamic TWFE results - Poisson Regression", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_poisson.png", width = 8, height = 6)


####################################################################

# 11a) do it for thefts specifically

# do the regression, saving it to then be plotted
TWFE_1km_poisson_theft <- etwfe(
  fml = theft_from_the_person ~ 1,
  tvar = period,
  gvar = first_treatment_1,
  data = final_data,
  vcov = ~location, 
  family = "poisson",
  cgroup = "never"
)

# prepare the coefficients for plotting, using the command from etwfe
coefs <- as.data.frame(emfx(TWFE_1km_poisson_theft, type = "event")) %>%
  rename(event_time = event) %>%
  rename(coef = estimate) %>%
  rename(se = std.error) %>%
  # add one at event time -1, with coef = 0 and se = 0, to represent the omitted category
  add_row(event_time = -1, coef = 0, se = 0)

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.125,
    ymax = 0.125,
    title = "Dynamic TWFE results - Poisson Regression - Theft from the Person", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_poisson_theft.png", width = 8, height = 6)


####################################################################

# 11b) do it for robberies too

# do the regression, saving it to then be plotted
TWFE_1km_poisson_robbery <- etwfe(
  fml = robbery ~ 1,
  tvar = period,
  gvar = first_treatment_1,
  data = final_data,
  vcov = ~location, 
  family = "poisson",
  cgroup = "never"
)

# prepare the coefficients for plotting, using the command from etwfe
coefs <- as.data.frame(emfx(TWFE_1km_poisson_robbery, type = "event")) %>%
  rename(event_time = event) %>%
  rename(coef = estimate) %>%
  rename(se = std.error) %>%
  # add one at event time -1, with coef = 0 and se = 0, to represent the omitted category
  add_row(event_time = -1, coef = 0, se = 0)

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5), 
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results - Poisson Regression - Robbery", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/TWFE_1km_poisson_robbery.png", width = 8, height = 6)








####################################################################

# 12) estimate the treatment effect decay with distance non-parametrically for thefts and robberies

# first collect the residuals from a basic regression on fixed effects, without event time dummies, on the not-yet-treated untreated units
# this is just first stage of BJS (2024)

# get the data ready
final_data <- final_data %>%
  mutate(log_theft_robbery = log(theft_from_the_person + robbery + 1)) # add 1 to avoid log(0)

# collect not (yet) treated units
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, log_theft_robbery)

# do the regression
TWFE_1km_theft_robbery <- feols(log_theft_robbery ~ 1 | location + Month, data = first_stage_data)

# get fitted values and residuals for the whole data
final_data$fitted_vals <- predict(TWFE_1km_theft_robbery, newdata = final_data)
final_data <- final_data %>%
  mutate(residuals = log_theft_robbery - fitted_vals)

# now run a sequence of kernel regressions of the residuals on distance to closest active night tube station, by event time

# plot a kernel regression estimate of the relationship between residuals and distance, for all post-treatment units
second_stage_data <- final_data %>%
  filter(event_time_1 >= 0)

# do the kernel regression and save the results
model_kerns_all <- as.data.frame(locpoly(x = second_stage_data$min_active_dist,
                                 y = second_stage_data$residuals,
                                 bandwidth = dpill(second_stage_data$min_active_dist, second_stage_data$residuals),  # pilot bandwidth
                                 degree = 1,  # i.e. local linear
                                 gridsize = 100))

# plot the results
ggplot(model_kerns_all, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance (T&R)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_theft_robbery.png", width = 8, height = 6)


# # alternative method - use the mgcv package to fit a generalised additive model (GAM)

# # Fit using bam() - optimized for large datasets
# model_gam <- bam(residuals ~ s(min_active_dist, k = 20),
#                  data = second_stage_data,
#                  discrete = TRUE,  # major speed boost for large N
#                  nthreads = 4)     # parallel processing

# # Create prediction grid with SEs
# pred_grid <- data.frame(min_active_dist = seq(min(second_stage_data$min_active_dist),
#                                                max(second_stage_data$min_active_dist),
#                                                length.out = 100))

# preds <- predict(model_gam, newdata = pred_grid, se.fit = TRUE)
# pred_grid$y <- preds$fit
# pred_grid$se <- preds$se.fit
# pred_grid$lower <- pred_grid$y - 1.96 * pred_grid$se
# pred_grid$upper <- pred_grid$y + 1.96 * pred_grid$se

# # Plot
# ggplot(pred_grid, aes(x = min_active_dist, y = y)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = "black", fill = "blue") +
#   geom_line(color = "blue") +
#   geom_hline(yintercept = 0, linetype = "solid", color = "black") +
#   labs(title = "Treatment Effect Decay with Distance (T&R)",
#        x = "Distance from Station (km)",
#        y = "Treatment Effect") +
#   theme_bw()

# # save the graph
# ggsave("Crime and night tubes/Output/Figures/TWFE_1km_gam_theft_robbery.png", width = 8, height = 6)


####################################################################

# 12a) now do the kernel regression by event time periods, first in six month periods

# now loop over six month periods from 0-5 to 12-17, doing a kernel regression for each and saving the predictions
for (t in seq(0, 15, by = 6)) {
  
  # subset the data to the relevant event times
  data_subset <- second_stage_data %>%
    filter(event_time_1 >= t & event_time_1 < (t + 6))
  
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
  labs(title = "Treatment Effect Decay with Distance (T&R)",
       x = "Distance from Station (km)",
       y = "Treatment Effect",
       color = "Event Time") +
  scale_color_manual(values = c(
        "0-5 months" = "#3e18fa",
        "6-11 months" = "#29bdf3",
        "12-17 months" = "#00ffee"), 
        breaks = c("0-5 months", "6-11 months", "12-17 months")) +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_6_months.png", width = 8, height = 6)

####################################################################

# 12b) now the same, but at three month periods

# now do the same thing but in three month periods
for (t in seq(0, 15, by = 3)) {
  
  # subset the data to the relevant event times
  data_subset <- second_stage_data %>%
    filter(event_time_1 >= t & event_time_1 < (t + 3))
  
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
  labs(title = "Treatment Effect Decay with Distance (T&R)",
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
    breaks = c("0-2 months", "3-5 months", "6-8 months", "9-11 months", "12-14 months", "15-17 months")) +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_3_months.png", width = 8, height = 6)


#####################################################################

# 12c) now do it with the residuals from a Poisson regression

# get the outcome variable ready
final_data <- final_data %>%
  mutate(theft_robbery = theft_from_the_person + robbery)

# subset the data to untreated and not-yet-treated units
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, theft_robbery)

# do the Poisson regression
first_stage_Poisson <- feglm(theft_robbery ~ 1 | location + Month, data = first_stage_data, family = poisson)

# subset the data to include only post-treatment observations
second_stage_data <- final_data %>%
  filter(event_time_1 >= 0)

# get the residuals
second_stage_data <- second_stage_data %>%
  mutate(fitted_poisson = predict(first_stage_Poisson, newdata = second_stage_data)) %>%
  mutate(residuals_poisson = theft_robbery - fitted_poisson)

# some are NA - why?? Because they didn't have a theft/robbery beforehand?
# drop these
second_stage_data <- second_stage_data %>%
  filter(!is.na(residuals_poisson))

# do the kernel regression and save the results
model_kerns_all <- as.data.frame(locpoly(x = second_stage_data$min_active_dist,
                                 y = second_stage_data$residuals_poisson,
                                 bandwidth = dpill(second_stage_data$min_active_dist, second_stage_data$residuals_poisson),  # pilot bandwidth
                                 degree = 1,  # i.e. local linear
                                 gridsize = 100))

# plot the results
ggplot(model_kerns_all, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance (T&R, Poisson residuals)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_theft_robbery_poisson.png", width = 8, height = 6)



# # now fit a GAM using bam()
# model_gam <- bam(residuals_poisson ~ s(min_active_dist, k = 20),
#                  data = second_stage_data,
#                  discrete = TRUE,  # major speed boost for large N
#                  nthreads = 4)     # parallel processing

# # Create prediction grid with SEs
# pred_grid <- data.frame(min_active_dist = seq(min(second_stage_data$min_active_dist),
#                                                max(second_stage_data$min_active_dist),
#                                                length.out = 100))

# preds <- predict(model_gam, newdata = pred_grid, se.fit = TRUE)
# pred_grid$y <- preds$fit
# pred_grid$se <- preds$se.fit
# pred_grid$lower <- pred_grid$y - 1.96 * pred_grid$se
# pred_grid$upper <- pred_grid$y + 1.96 * pred_grid$se

# # Plot
# ggplot(pred_grid, aes(x = min_active_dist, y = y)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = "black", fill = "blue") +
#   geom_line(color = "blue") +
#   geom_hline(yintercept = 0, linetype = "solid", color = "black") +
#   labs(title = "Treatment Effect Decay with Distance (T&R, Poisson residuals)",
#        x = "Distance from Station (km)",
#        y = "Treatment Effect") +
#   theme_bw()

# # save the graph
# ggsave("Crime and night tubes/Output/Figures/TWFE_1km_gam_theft_robbery_poisson.png", width = 8, height = 6)


####################################################################

# 12d) now do it with residuals from the total crime count

first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, log_num_crimes)

# do the regression
TWFE_1km_total <- feols(log_num_crimes ~ 1 | location + Month, data = first_stage_data)

# get fitted values and residuals for the whole data
final_data$fitted_vals <- predict(TWFE_1km_total, newdata = final_data)
final_data <- final_data %>%
  mutate(residuals = log_num_crimes - fitted_vals)

# now run a sequence of kernel regressions of the residuals on distance to closest active night tube station, by event time

# plot a kernel regression estimate of the relationship between residuals and distance, for all post-treatment units
second_stage_data <- final_data %>%
  filter(event_time_1 >= 0)

# do the kernel regression and save the results
model_kerns_all <- as.data.frame(locpoly(x = second_stage_data$min_active_dist,
                                 y = second_stage_data$residuals,
                                 bandwidth = dpill(second_stage_data$min_active_dist, second_stage_data$residuals),  # pilot bandwidth
                                 degree = 1,  # i.e. local linear
                                 gridsize = 100))

# plot the results
ggplot(model_kerns_all, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance (all crimes)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_total.png", width = 8, height = 6)


####################################################################

# 12e) now do it with the residuals from a Poisson regression on total crime counts

# subset the data to untreated and not-yet-treated units
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, num_crimes)

# do the Poisson regression
first_stage_Poisson <- feglm(num_crimes ~ 1 | location + Month, data = first_stage_data, family = poisson)

# subset the data to include only post-treatment observations
second_stage_data <- final_data %>%
  filter(event_time_1 >= 0)

# get the residuals
second_stage_data <- second_stage_data %>%
  mutate(fitted_poisson = predict(first_stage_Poisson, newdata = second_stage_data)) %>%
  mutate(residuals_poisson = num_crimes - fitted_poisson)

# some are NA - why?? Because they didn't have a theft/robbery beforehand?
# drop these
second_stage_data <- second_stage_data %>%
  filter(!is.na(residuals_poisson))

# do the kernel regression and save the results
model_kerns_all <- as.data.frame(locpoly(x = second_stage_data$min_active_dist,
                                 y = second_stage_data$residuals_poisson,
                                 bandwidth = dpill(second_stage_data$min_active_dist, second_stage_data$residuals_poisson),  # pilot bandwidth
                                 degree = 1,  # i.e. local linear
                                 gridsize = 100))

# plot the results
ggplot(model_kerns_all, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance (all crimes, Poisson residuals)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_total_poisson.png", width = 8, height = 6)


####################################################################

# 12f) spatial block boostrap the kernel regression for T&R count, by LSOA, to get confidence intervals

# NOTE: NEED TO DETERMINE WHETHER THIS IS VALID

# implement this manually

# sample by LSOA
set.seed(123) # for reproducibility

# pre-compute row indices per spatial block, the LSOA
block_indices <- split(seq_len(nrow(final_data)), final_data$LSOA11NM)
blocks <- names(block_indices)

# set the number of iterations
N = 100

# set up matrices to hold the results, and the distances
boot_kern_matrix <- matrix(NA, nrow = N, ncol = 100)
boot_kern_x <- NULL 

# loop over the number of bootstrap iterations
for (n in seq(1, N)) {

    # resample blocks with replacement, then build bootstrap dataset by pulling all obs within sampled blocks
    sampled_blocks <- sample(blocks, size = n_blocks, replace = TRUE)
    row_ids <- unlist(block_indices[sampled_blocks], use.names = FALSE)
    boot_data <- final_data[row_ids, ]

    # now do the same as 12, but on boot_data

    # collect not (yet) treated units
    first_stage_data <- boot_data %>%
      filter(event_time_1 < 0) %>%
      select(location, Month, log_theft_robbery)

    # do the regression
    TWFE_1km_boot <- feols(log_theft_robbery ~ 1 | location + Month, data = first_stage_data)

    # get fitted values and residuals for the whole data
    boot_data$fitted_vals <- predict(TWFE_1km_boot, newdata = boot_data)
    boot_data <- boot_data %>%
      mutate(residuals = log_theft_robbery - fitted_vals)

    # get a kernel regression estimate of the relationship between residuals and distance, for all post-treatment units
    second_stage_data <- boot_data %>%
      filter(event_time_1 >= 0)

    # do the kernel regression and save the results
    model_kerns <- as.data.frame(locpoly(x = second_stage_data$min_active_dist,
                                    y = second_stage_data$residuals,
                                    bandwidth = dpill(second_stage_data$min_active_dist, second_stage_data$residuals),  # pilot bandwidth
                                    degree = 1,  # i.e. local linear
                                    gridsize = 100))

    # store the y values as a row in the matrix
    boot_kern_matrix[n, ] <- model_kerns$y

    # save the x grid once
    if (is.null(boot_kern_x)) boot_kern_x <- model_kerns$x

    # print an update to track progress
    print(paste0("Completed bootstrap iteration ", n))
}

# convert the matrix of results to long format
boot_plot_df <- data.frame(
  x = rep(boot_kern_x, each = N),
  y = as.vector(boot_kern_matrix),
  iteration = rep(1:N, times = 100)
)

# get pointwise percentile CIs (95%)
# note - maybe switch to the percentile-t method
ci_lower <- apply(boot_kern_matrix, 2, quantile, probs = 0.025)
ci_upper <- apply(boot_kern_matrix, 2, quantile, probs = 0.975)
ci_mean  <- colMeans(boot_kern_matrix)
ci_df <- data.frame(x = boot_kern_x, mean = ci_mean, lower = ci_lower, upper = ci_upper)

# plot the results, with each bootstrap iteration as a thin line, and the pointwise CIs as a ribbon
ggplot() +
  geom_line(data = boot_plot_df, aes(x = x, y = y, group = iteration),
            color = "grey70", size = 0.3, alpha = 0.3) +
  geom_ribbon(data = ci_df, aes(x = x, ymin = lower, ymax = upper),
              fill = "blue", alpha = 0.2) +
  geom_line(data = ci_df, aes(x = x, y = mean), color = "blue", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Bootstrapped Treatment Effect Decay with Distance (T&R)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_theft_robbery_bootstrap.png", width = 8, height = 6)


# # pointwise CIs
# ci_lower <- apply(boot_kern_matrix, 2, quantile, probs = 0.025)
# ci_upper <- apply(boot_kern_matrix, 2, quantile, probs = 0.975)
# ci_mean  <- colMeans(boot_kern_matrix)

# ci_df <- data.frame(x = boot_kern_x, mean = ci_mean, lower = ci_lower, upper = ci_upper)

# ggplot(ci_df, aes(x = x)) +
# geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
# geom_line(aes(y = mean), color = "blue", size = 0.8) +
# geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
# labs(title = "Bootstrapped Treatment Effect Decay with Distance (T&R)",
#       x = "Distance from Station (km)",
#       y = "Treatment Effect") +
# theme_bw()

####################################################################

# 12g) now do it as at the start, but with controls

# get the data ready
final_data <- final_data %>%
  mutate(log_theft_robbery = log(theft_from_the_person + robbery + 1)) # add 1 to avoid log(0)

# collect not (yet) treated units
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, log_theft_robbery, IMD, pop_density, single_adult_hh_prop, avg_age, prop_same_eth_group, avg_health_score)

# do the regression
TWFE_1km_theft_robbery_controls <- feols(log_theft_robbery ~ i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = first_stage_data)

# get fitted values and residuals for the whole data
final_data$fitted_vals <- predict(TWFE_1km_theft_robbery_controls, newdata = final_data)
final_data <- final_data %>%
  mutate(residuals = log_theft_robbery - fitted_vals)

# now run a sequence of kernel regressions of the residuals on distance to closest active night tube station, by event time

# plot a kernel regression estimate of the relationship between residuals and distance, for all post-treatment units
second_stage_data <- final_data %>%
  filter(event_time_1 >= 0) %>%
  filter(!is.na(residuals)) # drop any with NA residuals, which are likely those with missing values in the controls

# do the kernel regression and save the results
model_kerns_all <- as.data.frame(locpoly(x = second_stage_data$min_active_dist,
                                 y = second_stage_data$residuals,
                                 bandwidth = dpill(second_stage_data$min_active_dist, second_stage_data$residuals),  # pilot bandwidth
                                 degree = 1,  # i.e. local linear
                                 gridsize = 100))

# plot the results
ggplot(model_kerns_all, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance (T&R)",
       x = "Distance from Station (km)",
       y = "Treatment Effect",
       caption = "Controls included in first-stage regression") +
  theme_bw()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_controls.png", width = 8, height = 6)





####################################################################

# 13) now use BJS (2024) to do a comparison of the ATT across rich vs poor areas - first for all crimes 

# load in the data
coefs <- load_bjs_results("Crime and night tubes EXTRA DATA/BJS results/BJS_results_wealth_all.csv", "tau_W")

# plot the results
plot(coefs = coefs, 
    xsequence = seq(-10, 15, 5), 
    ymin = -0.1,
    ymax = 0.1,
    title = "BJS (2024) - tau_rich - tau_poor - All Crimes", 
    note = "Simple treatment definition, theshold = 1km")

# save the graph
ggsave("Crime and night tubes/Output/Results/BJS_1km_wealth_diff_all.png", width = 8, height = 6)

####################################################################

# 13a) now for each crime individually, in a loop

# define the crime types
crime_types_bjs <- c("violence_and_sexual_offences", "vehicle_crime", "other_theft", "burglary",                    
 "antisocial_behaviour", "shoplifting", "criminal_damage_and_arson", "other_crime",                 
 "possession_of_weapons", "bicycle_theft", "drugs", "public_order", "theft_from_the_person", "robbery")

for (crime in crime_types_bjs) {

  coefs <- load_bjs_results(paste0("Crime and night tubes EXTRA DATA/BJS results/BJS_results_wealth_log_", crime, ".csv"), "tau_W")

  # plot the results
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-10, 15, 5), 
      ymin = -0.1,
      ymax = 0.1,
      title = paste0("BJS (2024), wealth difference, ", gsub("_", " ", crime)), 
      note = "Simple treatment definition, theshold = 1km"))
  
  # print a message to indicate completion
  print(paste0("Done for ", crime))

}

# now plot them all in one big grid
(plot_violence_and_sexual_offences + plot_vehicle_crime + plot_other_theft + plot_burglary +
 plot_antisocial_behaviour + plot_shoplifting + plot_criminal_damage_and_arson + plot_other_crime +
 plot_possession_of_weapons + plot_bicycle_theft + plot_drugs + plot_public_order +
 plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 5)

# save this too
ggsave("Crime and night tubes/Output/Results/BJS_1km_wealth_diff_grid.png", width = 22, height = 12)


###################################################################













# 14) now examine the ridership data and the aggregated station-level imputed crime effect

# NOTE - THIS COULD BE WRONG. THE SPECIFICATION SEEMS INCORRECT

# load in the ridership data
ridership_data <- read_csv("Crime and night tubes EXTRA DATA/TfL_station_monthly_ridership.csv")

# now get the imputed treatment effect for each location-month

# get the outcome variable ready
final_data <- final_data %>%
  mutate(theft_robbery = theft_from_the_person + robbery) %>%
  mutate(log_theft_robbery = log(theft_robbery + 1))

# subset the data to pre-treatment observations, as before
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, theft_robbery, log_theft_robbery)

# do the regression
first_stage_logs <- feols(log_theft_robbery ~ 1 | location + Month, data = first_stage_data)

# also do a Poisson regression (THOUGH ISSUES WITH THIS)
first_stage_Poisson <- feglm(theft_robbery ~ 1 | location + Month, data = first_stage_data, family = poisson)
# 17,154 locations (c. 500,000 observations) removed because of only 0 outcomes (no thefts or robberies ever)
# 587,489 observations left

# get the fitted values and residuals for the whole data, for both regressions
final_data <- final_data %>%
  mutate(fitted_vals = predict(first_stage_logs, newdata = final_data)) %>%
  mutate(residuals = log_theft_robbery - fitted_vals) %>%
  mutate(fitted_poisson = predict(first_stage_Poisson, newdata = final_data)) %>%
  mutate(residuals_poisson = theft_robbery - fitted_poisson) %>%
  mutate(residuals_imputed = theft_robbery - exp(fitted_vals) + 1)

# also impute levels TEs from log regression? Maybe subject to same issue as Poisson, but worth doing

# we do the following analysis on 2017 data, so first drop all earlier years and untreated observations
behaviour_data <- final_data %>%
  filter(period >= 25) %>%  # i.e. from 2017 onwards
  filter(event_time_1 >= 0)  # i.e. only treated observations

# now do the station-level aggregation

# split the data up by station, so that we have one row per station-month, with a column for the imputed treatment effect
# then aggregate to get the average imputed treatment effect for all observations within 0.5km of each station, by month
station_ATT_data <- behaviour_data %>%
  # Split the comma-separated station names into one row per station
  separate_rows(stations_within_0.5km, sep = ",\\s*") %>%  # now each row is a location-time-station combination
  # Group by station and time period
  group_by(stations_within_0.5km, period) %>%
  # Calculate sums of the imputed treatment effects (i.e. the residuals) for each station-month combo
  summarise(sum_TE_logs = sum(residuals), sum_TE_poisson = sum(residuals_poisson, na.rm = TRUE), sum_TE_imputed = sum(residuals_imputed)) %>% # we have to remove NAs here for Poisson!!
  ungroup() %>%
  rename(station = stations_within_0.5km)

# now we have two sets of station-level data, and we need to merge them

# first check the station naming in both datasets, and clean if necessary
# # get the unique station names in each dataset
# unique_stations_ridership <- unique(ridership_data$`station`)
# unique_stations_ATT <- unique(station_ATT_data$station)
# # print them out to check
# print(unique_stations_ridership)
# print(unique_stations_ATT)
# the ATT data has more, because it includes also untreated stations near treated ones
# these will be dropped as they aren't relevant for this
# the relevant cleaning is done in the TfL cleaning file

# now merge, dropping everything unmatched from the ATT data (i.e. untreated stations), and Heathrow T4 (which was in the ridership data but not in the final data - work out why)
# do this via an inner join, to drop unmatched stations on both sides (which will also drop Heathrow T4)
merged_data <- ridership_data %>%
  inner_join(
    station_ATT_data,
    by = c("station", "period")
  )

# correct number of observations in the merged dataset - only the 12 from Heathrow T4 were lost from the ridership data

# note: unclear what lag function we need to use below, but it isn't plm lag
detach("package:plm", unload = TRUE)
# the base one, whatever it is, works

# now create a variable giving the proportional and the level change in ridership compared to the previous period, for each station
merged_data <- merged_data %>%
  group_by(station) %>%
  arrange(station, period) %>%
  mutate(change_avg_taps = monthly_avg_taps - lag(monthly_avg_taps)) %>%
  ungroup()

# now do the same for proportional change in the imputed treatment effect (generating separate ones for each type of TE)
merged_data <- merged_data %>%
  group_by(station) %>%
  arrange(period) %>%
  mutate(change_sum_TE_logs = sum_TE_logs - lag(sum_TE_logs)) %>%
  mutate(change_sum_TE_poisson = sum_TE_poisson - lag(sum_TE_poisson)) %>%
  mutate(change_sum_TE_imputed = sum_TE_imputed - lag(sum_TE_imputed)) %>%
  ungroup()

# reattach plm
library(plm)

# finally, declare the merged data as a panel
merged_data <- panel(merged_data, ~ station + period)

# now ready for regressions

# Q: WHAT SEs TO USE?
  # without lagged dep.vars, just cluster by station
# also: CAREFUL WITH BIAS, SEs ETC!! (lags of dependent variables etc)
  # Arellano-Bond for this
  # presume we need to get some controls in too

# first analyse the sum of log TEs (without lagged dep vars)
behaviour_logs_1lag <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_logs_1lag)

behaviour_logs_2lag <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) + l(change_sum_TE_logs, 2) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_logs_2lag)

behaviour_logs_3lag <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) + l(change_sum_TE_logs, 2) + l(change_sum_TE_logs, 3) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_logs_3lag)

# now do it for the sum of imputed TEs from the log regression
behaviour_imputed_1lag <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_imputed_1lag)

behaviour_imputed_2lag <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) + l(change_sum_TE_imputed, 2) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_imputed_2lag)

behaviour_imputed_3lag <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) + l(change_sum_TE_imputed, 2) + l(change_sum_TE_imputed, 3) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_imputed_3lag)

# now do it for the sum of Poisson TEs (again without LDVs)
behaviour_poisson_1lag <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_poisson_1lag)

behaviour_poisson_2lag <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) + l(change_sum_TE_poisson, 2) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_poisson_2lag)

behaviour_poisson_3lag <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) + l(change_sum_TE_poisson, 2) + l(change_sum_TE_poisson, 3) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_poisson_3lag)

# I think this should be proportional change maybe? Or find controls? What is the issue here?

# do Anderson-Hsiao for log TEs
as_reg_logs <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) | station + period | 
                 l(change_avg_taps, 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)

summary(as_reg_logs, stage = 1:2)

# do Anderson-Hsiao for imputed TEs
as_reg_imputed <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) | station + period | 
                 l(change_avg_taps, 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)

summary(as_reg_imputed, stage = 1:2)

# do Anderson-Hsiao for Poisson TEs
as_reg_poisson <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) | station + period | 
                 l(change_avg_taps, 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)

summary(as_reg_poisson, stage = 1:2)


# make a regression table for these

# first make a dictionary for table labels
my_dict <- c(
  "l(change_sum_TE_logs, 1)"    = "$\\Delta$ Log TE$_{t-1}$",
  "l(change_sum_TE_logs, 2)"    = "$\\Delta$ Log TE$_{t-2}$",
  "l(change_sum_TE_logs, 3)"    = "$\\Delta$ Log TE$_{t-3}$",
  "l(change_sum_TE_imputed, 1)" = "$\\Delta$ Imputed TE$_{t-1}$",
  "l(change_sum_TE_imputed, 2)" = "$\\Delta$ Imputed TE$_{t-2}$",
  "l(change_sum_TE_imputed, 3)" = "$\\Delta$ Imputed TE$_{t-3}$",
  "fit_l(change_avg_taps, 1)"   = "$\\Delta$ Avg Taps$_{t-1}$ (fitted)",
  "change_avg_taps"             = "$\\Delta$ Avg Taps",
  "station"                     = "Station",
  "period"                      = "Period"
)

setFixest_dict(my_dict)

# make the table
etable(
  behaviour_logs_1lag, behaviour_logs_2lag, behaviour_logs_3lag, as_reg_logs,
  behaviour_imputed_1lag, behaviour_imputed_2lag, behaviour_imputed_3lag, as_reg_imputed,
  headers = c("Log", "Log", "Log", "AH: Log", "Imputed", "Imputed", "Imputed", "AH: Imputed"),
  order = c("TE_logs, 1", "TE_logs, 2", "TE_logs, 3",
          "TE_imputed, 1", "TE_imputed, 2", "TE_imputed, 3",
          "fit_l\\(change_avg_taps", "^change_avg_taps"),
  tex = TRUE,
  file = "Crime and Night Tubes/Output/Results/behaviour_regressions.tex",
  fitstat = ~ r2 + n + ivwald,
  title = "Effect of lagged treatment intensity on ridership changes",
  label = "tab:behaviour_results",
  fontsize = "footnotesize"
)




##### BELOW IS REDUNDANT UNLESS WE DO ARELLANO BOND


# do Arellano-Bond
# merged_pdata <- pdata.frame(merged_data, index = c("station", "period"))
# ab_reg_logs <- pgmm(
#   change_avg_taps ~ plm::lag(change_avg_taps, 1) + plm::lag(change_sum_TE_logs, 1) |
#     plm::lag(change_avg_taps, 2:5),  # instruments: deeper lags of the dependent variable
#   data = merged_pdata,
#   effect = "individual",  # two-way fixed effects doesn't work here!!
#   model = "twosteps",
#   transformation = "d"  # Arellano-Bond
# )
# summary(ab_reg_logs, robust = TRUE)

# why is the system computationally singular? it is the introduction of twoway FEs - the introduction of time dummies leads to multicollinearity
# there must be something else that is unit-invariant
# also even without TWFEs the inverses in first and second stage are singular - why?
# note that when we use just one lag, the AR(2) test is insignificant, as required - gets worse with more lags
# also adding levels of the dependent variable introduces singluarity too

# could we do Arellano-Bond in levels? I guess the levels effect and the differences effect are similar (at least same sign)

# merged_pdata <- pdata.frame(merged_data, index = c("station", "period"))
# ab_reg_logs <- pgmm(
#   avg_monthly_taps ~ plm::lag(avg_monthly_taps, 1) + plm::lag(sum_TE_logs, 1) |
#     plm::lag(avg_monthly_taps, 2:5),  # instruments: deeper lags of the dependent variable
#   data = merged_pdata,
#   effect = "twoways",  # two-way fixed effects
#   model = "twosteps",
#   transformation = "d"  # Arellano-Bond
# )
# summary(ab_reg_logs, robust = TRUE)


#########################################################################


# 14a) now do the same, but with residuals from a regression with controls

# load in the ridership data
ridership_data <- read_csv("Crime and night tubes EXTRA DATA/TfL_station_monthly_ridership.csv")

# now get the imputed treatment effect for each location-month

# get the outcome variable ready
final_data <- final_data %>%
  mutate(theft_robbery = theft_from_the_person + robbery) %>%
  mutate(log_theft_robbery = log(theft_robbery + 1))

# subset the data to pre-treatment observations, as before
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, theft_robbery, log_theft_robbery, IMD, income_rank, education_rank, health_rank, barriers_rank, living_env_rank, pop_density, prop_same_eth_group, single_adult_hh_prop, avg_health_score, avg_age)

# do the regressions
first_stage_logs <- feols(log_theft_robbery ~ i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = first_stage_data)
first_stage_Poisson <- feglm(theft_robbery ~ i(Month, IMD) + i(Month, pop_density) + i(Month, single_adult_hh_prop) + i(Month, avg_age) + i(Month, prop_same_eth_group) + i(Month, avg_health_score) | location + Month, data = first_stage_data, family = poisson)
# 17,154 locations (c. 500,000 observations) removed because of only 0 outcomes (no thefts or robberies ever)
# 587,489 observations left

# get the fitted values and residuals for the whole data
final_data <- final_data %>%
  mutate(fitted_vals = predict(first_stage_logs, newdata = final_data)) %>%
  mutate(residuals = log_theft_robbery - fitted_vals) %>%
  mutate(fitted_poisson = predict(first_stage_Poisson, newdata = final_data)) %>%
  mutate(residuals_poisson = theft_robbery - fitted_poisson) %>%
  mutate(residuals_imputed = theft_robbery - exp(fitted_vals) + 1)

# we do the following analysis on 2017 data, so first drop all earlier years and untreated observations
behaviour_data <- final_data %>%
  filter(period >= 25) %>%  # i.e. from 2017 onwards
  filter(event_time_1 >= 0)  # i.e. only treated observations

# now do the station-level aggregation
station_ATT_data <- behaviour_data %>%
  separate_rows(stations_within_0.5km, sep = ",\\s*") %>%  # now each row is a location-time-station combination
  group_by(stations_within_0.5km, period) %>%
  summarise(sum_TE_logs = sum(residuals), sum_TE_poisson = sum(residuals_poisson, na.rm = TRUE), sum_TE_imputed = sum(residuals_imputed)) %>% # we have to remove NAs here for Poisson!!
  ungroup() %>%
  rename(station = stations_within_0.5km)

# now merge with the station data from above
merged_data <- ridership_data %>%
  inner_join(
    station_ATT_data,
    by = c("station", "period")
  )


detach("package:plm", unload = TRUE)

# now create a variable giving the proportional and the level change in ridership compared to the previous period, for each station
merged_data <- merged_data %>%
  group_by(station) %>%
  arrange(station, period) %>%
  mutate(change_avg_taps = monthly_avg_taps - lag(monthly_avg_taps)) %>%
  ungroup()

# now do the same for proportional change in the imputed treatment effect (generating separate ones for each type of TE)
merged_data <- merged_data %>%
  group_by(station) %>%
  arrange(period) %>%
  mutate(change_sum_TE_logs = sum_TE_logs - lag(sum_TE_logs)) %>%
  mutate(change_sum_TE_poisson = sum_TE_poisson - lag(sum_TE_poisson)) %>%
  mutate(change_sum_TE_imputed = sum_TE_imputed - lag(sum_TE_imputed)) %>%
  ungroup()

# reattach plm
library(plm)

# finally, declare the merged data as a panel
merged_data <- panel(merged_data, ~ station + period)

# now ready for regressions

# Q: WHAT SEs TO USE?
  # without lagged dep.vars, just cluster by station
# also: CAREFUL WITH BIAS, SEs ETC!! (lags of dependent variables etc)
  # Arellano-Bond for this
  # presume we need to get some controls in too

# first analyse the sum of log TEs (without lagged dep vars)
behaviour_logs_1lag <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_logs_1lag)

behaviour_logs_2lag <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) + l(change_sum_TE_logs, 2) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_logs_2lag)

behaviour_logs_3lag <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) + l(change_sum_TE_logs, 2) + l(change_sum_TE_logs, 3) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_logs_3lag)

# now do it for the sum of imputed TEs from the log regression
behaviour_imputed_1lag <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_imputed_1lag)

behaviour_imputed_2lag <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) + l(change_sum_TE_imputed, 2) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_imputed_2lag)

behaviour_imputed_3lag <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) + l(change_sum_TE_imputed, 2) + l(change_sum_TE_imputed, 3) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_imputed_3lag)

# now do it for the sum of Poisson TEs (again without LDVs)
behaviour_poisson_1lag <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_poisson_1lag)

behaviour_poisson_2lag <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) + l(change_sum_TE_poisson, 2) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_poisson_2lag)

behaviour_poisson_3lag <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) + l(change_sum_TE_poisson, 2) + l(change_sum_TE_poisson, 3) | station + period, data = merged_data, cluster = ~ station)
summary(behaviour_poisson_3lag)

# I think this should be proportional change maybe? Or find controls? What is the issue here?

# do Anderson-Hsiao for log TEs
as_reg_logs <- feols(change_avg_taps ~ l(change_sum_TE_logs, 1) | station + period | 
                 l(change_avg_taps, 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)

summary(as_reg_logs, stage = 1:2)

# do Anderson-Hsiao for imputed TEs
as_reg_imputed <- feols(change_avg_taps ~ l(change_sum_TE_imputed, 1) | station + period | 
                 l(change_avg_taps, 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)

summary(as_reg_imputed, stage = 1:2)

# do Anderson-Hsiao for Poisson TEs
as_reg_poisson <- feols(change_avg_taps ~ l(change_sum_TE_poisson, 1) | station + period | 
                 l(change_avg_taps, 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)

summary(as_reg_poisson, stage = 1:2)


# make a regression table for these

# first make a dictionary for table labels
my_dict <- c(
  "l(change_sum_TE_logs, 1)"    = "$\\Delta$ Log TE$_{t-1}$",
  "l(change_sum_TE_logs, 2)"    = "$\\Delta$ Log TE$_{t-2}$",
  "l(change_sum_TE_logs, 3)"    = "$\\Delta$ Log TE$_{t-3}$",
  "l(change_sum_TE_imputed, 1)" = "$\\Delta$ Imputed TE$_{t-1}$",
  "l(change_sum_TE_imputed, 2)" = "$\\Delta$ Imputed TE$_{t-2}$",
  "l(change_sum_TE_imputed, 3)" = "$\\Delta$ Imputed TE$_{t-3}$",
  "fit_l(change_avg_taps, 1)"   = "$\\Delta$ Avg Taps$_{t-1}$ (fitted)",
  "change_avg_taps"             = "$\\Delta$ Avg Taps",
  "station"                     = "Station",
  "period"                      = "Period"
)

setFixest_dict(my_dict)

# make the table
etable(
  behaviour_logs_1lag, behaviour_logs_2lag, behaviour_logs_3lag, as_reg_logs,
  behaviour_imputed_1lag, behaviour_imputed_2lag, behaviour_imputed_3lag, as_reg_imputed,
  headers = c("Log", "Log", "Log", "AH: Log", "Imputed", "Imputed", "Imputed", "AH: Imputed"),
  order = c("TE_logs, 1", "TE_logs, 2", "TE_logs, 3",
          "TE_imputed, 1", "TE_imputed, 2", "TE_imputed, 3",
          "fit_l\\(change_avg_taps", "^change_avg_taps"),
  tex = TRUE,
  file = "Crime and Night Tubes/Output/Results/behaviour_regressions.tex",
  fitstat = ~ r2 + n + ivwald,
  title = "Effect of lagged treatment intensity on ridership changes",
  label = "tab:behaviour_results",
  fontsize = "footnotesize"
)


##################################################################

# 14new) do this all for the equation in levels

# load in the ridership data
ridership_data <- read_csv("Crime and night tubes EXTRA DATA/TfL_station_monthly_ridership.csv")

# now get the imputed treatment effect for each location-month

# get the outcome variable ready
final_data <- final_data %>%
  mutate(theft_robbery = theft_from_the_person + robbery) %>%
  mutate(log_theft_robbery = log(theft_robbery + 1))

# subset the data to pre-treatment observations, as before
first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, theft_robbery, log_theft_robbery)

# do the regression
first_stage_logs <- feols(log_theft_robbery ~ 1 | location + Month, data = first_stage_data)

# get the fitted values and residuals for the whole data, for both regressions
final_data <- final_data %>%
  mutate(fitted_vals = predict(first_stage_logs, newdata = final_data)) %>%
  mutate(residuals = log_theft_robbery - fitted_vals) %>%
  mutate(residuals_imputed = theft_robbery - exp(fitted_vals) + 1)

# we do the following analysis on 2017 data, so first drop all earlier years and untreated observations
behaviour_data <- final_data %>%
  filter(period >= 25) %>%  # i.e. from 2017 onwards
  filter(event_time_1 >= 0)  # i.e. only treated observations

# now do the station-level aggregation

# split the data up by station, so that we have one row per station-month, with a column for the imputed treatment effect
# then aggregate to get the average imputed treatment effect for all observations within 0.5km of each station, by month
station_ATT_data <- behaviour_data %>%
  # Split the comma-separated station names into one row per station
  separate_rows(stations_within_0.5km, sep = ",\\s*") %>%  # now each row is a location-time-station combination
  # Group by station and time period
  group_by(stations_within_0.5km, period) %>%
  # Calculate sums of the imputed treatment effects (i.e. the residuals) for each station-month combo
  summarise(sum_TE_logs = sum(residuals), sum_TE_imputed = sum(residuals_imputed)) %>% # we have to remove NAs here for Poisson!!
  ungroup() %>%
  rename(station = stations_within_0.5km)

# now we have two sets of station-level data, and we need to merge them
# now merge, dropping everything unmatched from the ATT data (i.e. untreated stations), and Heathrow T4 (which was in the ridership data but not in the final data - work out why)
# do this via an inner join, to drop unmatched stations on both sides (which will also drop Heathrow T4)
merged_data <- ridership_data %>%
  inner_join(
    station_ATT_data,
    by = c("station", "period")
  )

# finally, declare the merged data as a panel
merged_data <- panel(merged_data, ~ station + period)

# now ready for regressions

# start with just log TEs

# do Anderson-Hsiao for log TEs, starting with the simplest version
as_reg_logs <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_logs), 1) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_logs, stage = 1:2)

# use an instrument further back
as_reg_logs_3 <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_logs), 1) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 3),
               data = merged_data, cluster = ~ station)
summary(as_reg_logs_3, stage = 1:2)

# try instrumenting with levels
as_reg_logs_levels <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_logs), 1) | period |
                 l(d(monthly_avg_taps), 1) ~ l(monthly_avg_taps, 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_logs_levels, stage = 1:2)

# use more lags of the crime count
as_reg_logs_crime_1_3 <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_logs), 1:3) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_logs_crime_1_3, stage = 1:2)

as_reg_logs_crime_1_2 <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_logs), 1:2) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_logs_crime_1_2, stage = 1:2)

# EXTRAS:

# use Arellano-Bond
p_data <- pdata.frame(merged_data, index = c("station", "period"))

ab_model <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 2:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model, robust = TRUE)

# vary the number of instruments
ab_model_2_3 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 2:3),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_2_3, robust = TRUE)

ab_model_2_5 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 2:5),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_2_5, robust = TRUE)

# add in further lags
ab_model_3_4 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 3:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_3_4, robust = TRUE)

ab_model_3_5 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 3:5),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_3_5, robust = TRUE)

ab_model_3_6 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 3:6),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_3_6, robust = TRUE)

# add in instrumentation for the lagged crime count
ab_model_crime <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 2:4) + lag(sum_TE_logs, 2:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_crime, robust = TRUE)

ab_model_crime_3_4 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 3:4) + lag(sum_TE_logs, 3:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_crime_3_4, robust = TRUE)


# now do it for imputed TEs
as_reg_imputed <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_imputed), 1) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_imputed, stage = 1:2)

# use instruments further back
as_reg_imputed_3 <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_imputed), 1) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 3),
               data = merged_data, cluster = ~ station)
summary(as_reg_imputed_3, stage = 1:2)

# use more lags of the crime count
as_reg_imputed_crime_1_3 <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_imputed), 1:3) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_imputed_crime_1_3, stage = 1:2)

as_reg_imputed_crime_1_2 <- feols(d(monthly_avg_taps) ~ l(d(sum_TE_imputed), 1:2) | period |
                 l(d(monthly_avg_taps), 1) ~ l(d(monthly_avg_taps), 2),
               data = merged_data, cluster = ~ station)
summary(as_reg_imputed_crime_1_2, stage = 1:2)

# do Arellano-Bond for imputed TEs
ab_model_imputed <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 2:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed, robust = TRUE)

# vary the number of instruments
ab_model_imputed_2_3 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 2:3),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_2_3, robust = TRUE)

ab_model_imputed_2_5 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 2:5),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_2_5, robust = TRUE)

# add in further lags
ab_model_imputed_3_4 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 3:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_3_4, robust = TRUE)

ab_model_imputed_3_5 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 3:5),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_3_5, robust = TRUE)

ab_model_imputed_3_6 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 3:6),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_3_6, robust = TRUE)

# add in instrumentation for the lagged crime count
ab_model_imputed_crime <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 2:4) + lag(sum_TE_imputed, 2:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_crime, robust = TRUE)

ab_model_imputed_crime_3_4 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 3:4) + lag(sum_TE_imputed, 3:4),
  data = p_data,
  effect = "twoways",        # Includes the 'period' fixed effects
  model = "twosteps",     # Standard for optimal weighting matrix
  transformation = "d"    # Automatically takes the first-difference of the entire equation
)
summary(ab_model_imputed_crime_3_4, robust = TRUE)

############################

# AH - why use lagged differences and not lagged levels??

# AB - what about location clustering?

# now try system GMM
sys_gmm_model <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 3:4),
  data = p_data,
  effect = "twoways",      # Includes the time dummies
  model = "twosteps",      # Standard for optimal weighting matrix
  transformation = "ld",   # <-- THE CRITICAL CHANGE: "ld" stands for Level & Difference (System)
  collapse = TRUE          # Prevents the computationally singular error
)
summary(sys_gmm_model, robust = TRUE)

sys_gmm_model_4_5 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:3) + lag(sum_TE_logs, 1) | lag(monthly_avg_taps, 4:5),
  data = p_data,
  effect = "twoways",      # Includes the time dummies
  model = "twosteps",      # Standard for optimal weighting matrix
  transformation = "ld",   # <-- THE CRITICAL CHANGE: "ld" stands for Level & Difference (System)
  collapse = TRUE          # Prevents the computationally singular error
)
summary(sys_gmm_model_4_5, robust = TRUE)

sys_gmm_model_imputed <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:2) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 3:4),
  data = p_data,
  effect = "twoways",      # Includes the time dummies
  model = "twosteps",      # Standard for optimal weighting matrix
  transformation = "ld",   # <-- THE CRITICAL CHANGE: "ld" stands for Level & Difference (System)
  collapse = TRUE          # Prevents the computationally singular error
)
summary(sys_gmm_model_imputed, robust = TRUE)

sys_gmm_model_imputed_4_5 <- pgmm(
  monthly_avg_taps ~ lag(monthly_avg_taps, 1:3) + lag(sum_TE_imputed, 1) | lag(monthly_avg_taps, 4:5),
  data = p_data,
  effect = "twoways",      # Includes the time dummies
  model = "twosteps",      # Standard for optimal weighting matrix
  transformation = "ld",   # <-- THE CRITICAL CHANGE: "ld" stands for Level & Difference (System)
  collapse = TRUE          # Prevents the computationally singular error
)
summary(sys_gmm_model_imputed_4_5, robust = TRUE)

# do we need to instrument for crime count?

# make a summary table for AH estimation

# first make a dictionary for table labels
my_dict <- c(
  "l(d(sum_TE_logs), 1)"    = "$\\Delta$ Log TE$_{t-1}$",
  "l(d(sum_TE_logs), 2)"    = "$\\Delta$ Log TE$_{t-2}$",
  "l(d(sum_TE_logs), 3)"    = "$\\Delta$ Log TE$_{t-3}$",
  "l(d(sum_TE_imputed), 1)" = "$\\Delta$ Imputed TE$_{t-1}$",
  "l(d(sum_TE_imputed), 2)" = "$\\Delta$ Imputed TE$_{t-2}$",
  "l(d(sum_TE_imputed), 3)" = "$\\Delta$ Imputed TE$_{t-3}$",
  "fit_l(d(monthly_avg_taps), 1)"   = "$\\Delta$ Avg Taps$_{t-1}$ (fitted)",
  "d(monthly_avg_taps)"             = "$\\Delta$ Avg Taps",
  "station"                     = "Station",
  "period"                      = "Period"
)

setFixest_dict(my_dict)

# make the table
etable(
  as_reg_logs_levels, as_reg_logs, as_reg_logs_crime_1_2, as_reg_logs_crime_1_3, as_reg_logs_3,
  as_reg_imputed, as_reg_imputed_crime_1_2, as_reg_imputed_crime_1_3,
  headers = c("Log", "Log", "Log", "Log", "Log", "Imputed", "Imputed", "Imputed"),
  order = c("l(d(sum_TE_logs), 1)", "l(d(sum_TE_logs), 2)", "l(d(sum_TE_logs), 3)",
          "l(d(sum_TE_imputed), 1)", "l(d(sum_TE_imputed), 2)", "l(d(sum_TE_imputed), 3)",
          "fit_l(d(monthly_avg_taps, 1))"),
  tex = TRUE,
  file = "Crime and Night Tubes/Output/Results/behaviour_regressions_new.tex",
  fitstat = ~ r2 + n + ivwald,
  title = "Effect of lagged treatment intensity on ridership changes",
  label = "tab:behaviour_results",
  fontsize = "footnotesize"
)

# add a line that gives which instruments are used for each regression




# 15) now get some quick statistics to inform analysis (here until a better place is found)

# firstly, to inform about the magnitude of the treatment effect, get the mean number of pre-treatment crimes in each location (for the ever-treated locations)

# first for robbery

mean_robbery <- final_data %>%
  filter(event_time_1 < 0 & first_treatment_1 != Inf) %>%
  summarise(mean_robbery = mean(robbery))
# 0.0431

# calculate the rough treatment effect estimate

robbery_ATT = ( 1 + mean_robbery[1,1] ) * ( exp(0.0125) - 1 )
# 0.0131

# now for theft from the person

mean_theft_from_person <- final_data %>%
  filter(event_time_1 < 0 & first_treatment_1 != Inf) %>%
  summarise(mean_theft_from_person = mean(theft_from_the_person))
# 0.1016

# get rough TE estimate
theft_from_person_ATT = ( 1 + mean_theft_from_person[1,1] ) * ( exp(0.0275) - 1 )
# 0.0307


# count the number of treated locations, for aggregation purposes
num_treated_locations <- final_data %>%
  filter(first_treatment_1 != Inf) %>%
  summarise(num_treated = n_distinct(location))
# 16545


################################################################################################
################################################################################################