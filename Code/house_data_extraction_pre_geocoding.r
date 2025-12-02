# this file imports the price paid data from HM land registry, and filters it to leave only relevant house sales from the period of the study (2015-2017)

library(tidyverse)
library(writexl)

setwd("~/Economics/Papers (WIP)")

# import the CSV
house_data <- read.csv("Crime and night tubes EXTRA DATA/pp-complete.csv", header = FALSE, stringsAsFactors = FALSE)

# begin the cleaning
house_data <- house_data %>%
    
    # include only relevant years
    mutate(V3 = as.Date(V3)) %>%
    filter(V3 >= "2015-01-01 00:00" & V3 <= "2017-12-31 00:00") %>%

    # keep only the standard Price Paid entries, for which we know the house type, not the additional ones
    filter(V15 == "A") %>%

    # drop houses in locations which are clearly not in and around London, such as Manchester and Birmingham
    filter(!grepl("YORK|WEST SUSSEX|WEST MIDLANDS|WEST YORKSHIRE|WILTSHIRE|WORCESTERSHIRE|WARWICKSHIRE|TYNE AND WEAR|SUFFOLK|SWINDON|SWANSEA|STOKE-ON-TRENT|STAFFORDSHIRE|SOUTH YORKSHIRE|SOMERSET|SHROPSHIRE|SOUTHAMPTON|RHONDDA CYNON TAFF|PORTSMOUTH|OXFORDSHIRE|NORTHUMBERLAND|NOTTINGHAMSHIRE|NORTHAMPTONSHIRE|NORTH YORKSHIRE|NORFOLK|MERSEYSIDE|LINCOLNSHIRE|LEICESTERSHIRE|LEICESTER|LANCASHIRE|ISLE OF WIGHT|GLOUCESTERSHIRE|GREATER MANCHESTER|EAST SUSSEX|EAST RIDING OF YORKSHIRE|DERBYSHIRE|DORSET|DEVON|CUMBRIA|CORNWALL|CENTRAL BEDFORDSHIRE|CITY OF BRISTOL|CITY OF PLYMOUTH|CITY OF PETERBOROUGH|CITY OF NOTTINGHAM|CITY OF DERBY|CARDIFF|CHESHIRE EAST|CHESHIRE WEST AND CHESTER|CAMBRIDGESHIRE|COUNTY DURHAM|BRIGHTON AND HOVE|BOURNEMOUTH", V14)) %>%

    # drop the high-storage ID variable
    select(-V1)


# save the dataset to be used in the geocoding, by writing it to an excel file
write_xlsx(house_data, "Crime and night tubes EXTRA DATA/house_data_pre_geocoding.xlsx")