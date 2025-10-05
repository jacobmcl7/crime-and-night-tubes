# this file does the analysis

library(tidyverse)

setwd("~/Economics/Papers (WIP)")

load("Crime and night tubes EXTRA DATA/final_data.RData")


# keep only the observations within 500m of a tube station
final_data <- final_data %>%
    filter(!is.na(NAME1))


# start with a simple event study regression
simple_did <- lm(num_crimes ~  + factor(location) + factor(period), data = final_data)

# this doesn't work as there are too many factors
# what can I do about this?


# try alternative package
library(lfe)

simple_did <- felm(num_crimes ~ treatment | location + Month, data = final_data)
summary(simple_did)

# crimes increase!