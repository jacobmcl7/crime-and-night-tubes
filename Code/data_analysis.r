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
# 11) Poisson ETWFE regression for the main effect
#   11a) same but for theft from the person
#   11b) same but for robbery
# 12) non-parametric estimation of distance-decay of ATT
#   12a) same but split by six month periods
#   12b) same but split by three month periods
#   12c) now do it with the residuals from a Poisson regression
# 13) BJS (2024) for the difference in evolution in rich vs poor areas
#   13a) same but for all crimes separately




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

# allow the effect of being close to a station, and the wealth of the region, to vary by month

TWFE_1km_controls <- feols(log_num_crimes ~ i(event_time_1, ref = -1) + i(Month, min_any_dist) + i(Month, IMD_decile) | location + Month, data = final_data, cluster = "location")

# prepare the coefficients for plotting
coefs <- plot_prepare(TWFE_1km_controls, substring = "event_time_1")

# plot the graph
plot(coefs = coefs, 
    xsequence = seq(-20, 15, 5),
    ymin = -0.05,
    ymax = 0.05,
    title = "Dynamic TWFE results with controls",
    note = "Simple treatment definition, theshold = 1km")

# note: adding time x region controls seems to restrict variation too much, leading to lost significance





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
  rename(se = std.error)

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
  rename(se = std.error)

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
  rename(se = std.error)

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
TWFE_1km_theft_robbery <- feols(log_theft_robbery ~ 0 | location + Month, data = first_stage_data)

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
  labs(title = "Treatment Effect Decay with Distance (All Post-Treatment)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_theft_robbery.png", width = 8, height = 6)


# alternative method - use the mgcv package to fit a generalised additive model (GAM)

# Fit using bam() - optimized for large datasets
model_gam <- bam(residuals ~ s(min_active_dist, k = 20),
                 data = second_stage_data,
                 discrete = TRUE,  # major speed boost for large N
                 nthreads = 4)     # parallel processing

# Create prediction grid with SEs
pred_grid <- data.frame(min_active_dist = seq(min(second_stage_data$min_active_dist),
                                               max(second_stage_data$min_active_dist),
                                               length.out = 100))

preds <- predict(model_gam, newdata = pred_grid, se.fit = TRUE)
pred_grid$y <- preds$fit
pred_grid$se <- preds$se.fit
pred_grid$lower <- pred_grid$y - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$y + 1.96 * pred_grid$se

# Plot
ggplot(pred_grid, aes(x = min_active_dist, y = y)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = "black", fill = "blue") +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_gam_theft_robbery.png", width = 8, height = 6)


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
  labs(title = "Treatment Effect Decay with Distance",
       x = "Distance from Station (km)",
       y = "Treatment Effect",
       color = "Event Time") +
  scale_color_manual(values = c(
        "0-5 months" = "#3e18fa",
        "6-11 months" = "#29bdf3",
        "12-17 months" = "#00ffee"), 
        breaks = c("0-5 months", "6-11 months", "12-17 months")) +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_6_months.png", width = 8, height = 6)

####################################################################

# 12b) now the same, but at three month periods

# now do the same thing but in three month periods
for (t in seq(0, 15, by = 3)) {
  
  # subset the data to the relevant event times
  data_subset <- final_data %>%
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
    breaks = c("0-2 months", "3-5 months", "6-8 months", "9-11 months", "12-14 months", "15-17 months")) +
  theme_minimal()

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
  labs(title = "Treatment Effect Decay with Distance (all crimes, Poisson residuals)",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_poisson.png", width = 8, height = 6)



# now fit a GAM using bam()
model_gam <- bam(residuals_poisson ~ s(min_active_dist, k = 20),
                 data = second_stage_data,
                 discrete = TRUE,  # major speed boost for large N
                 nthreads = 4)     # parallel processing

# Create prediction grid with SEs
pred_grid <- data.frame(min_active_dist = seq(min(second_stage_data$min_active_dist),
                                               max(second_stage_data$min_active_dist),
                                               length.out = 100))

preds <- predict(model_gam, newdata = pred_grid, se.fit = TRUE)
pred_grid$y <- preds$fit
pred_grid$se <- preds$se.fit
pred_grid$lower <- pred_grid$y - 1.96 * pred_grid$se
pred_grid$upper <- pred_grid$y + 1.96 * pred_grid$se

# Plot
ggplot(pred_grid, aes(x = min_active_dist, y = y)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = "black", fill = "blue") +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(title = "Treatment Effect Decay with Distance",
       x = "Distance from Station (km)",
       y = "Treatment Effect") +
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_gam_poisson.png", width = 8, height = 6)


####################################################################

# 12d) now do it with residuals from the total crime count

first_stage_data <- final_data %>%
  filter(event_time_1 < 0) %>%
  select(location, Month, log_num_crimes)

# do the regression
TWFE_1km_total <- feols(log_num_crimes ~ 0 | location + Month, data = first_stage_data)

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
  theme_minimal()

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
  theme_minimal()

# save the graph
ggsave("Crime and night tubes/Output/Figures/TWFE_1km_kernel_total_poisson.png", width = 8, height = 6)











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

# GET ANTI SOCIAL BEHAVIOUR IN TOO!!!

# define the crime types
crime_types <- c("burglary", "bicycle_theft", "violence_and_sexual_offences", "other_theft", "shoplifting", "theft_from_the_person", "robbery")

for (crime in crime_types) {

  # get the data in
  coefs <- load_bjs_results(paste0("Crime and night tubes EXTRA DATA/BJS results/BJS_results_wealth_log_", crime, ".csv"), "tau_W")

  # save a plot of the results
  assign(paste0("plot_", crime), plot(coefs = coefs, 
      xsequence = seq(-10, 15, 5), 
      ymin = -0.1,
      ymax = 0.1,
      title = paste0("BJS (2024) - tau_rich - tau_poor - ", gsub("_", " ", crime)), 
      note = "Simple treatment definition, theshold = 1km"))

  # print a message to indicate completion
  print(paste0("Done for ", crime))
  
}

# now plot them all in one big grid
(plot_burglary + plot_bicycle_theft + plot_violence_and_sexual_offences + plot_other_theft + plot_shoplifting + plot_theft_from_the_person + plot_robbery) +
  plot_layout(ncol = 4)

# save this
ggsave("Crime and night tubes/Output/Results/BJS_1km_wealth_diff_grid.png", width = 22, height = 12)


################################################################################################
################################################################################################