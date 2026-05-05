*this do-file does the BJS (2024) estimation of the treatment effect - better in Stata than R
clear all
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)"

import delimited "Crime and night tubes EXTRA DATA\final_data_for_stata.csv"

*prepare the data for the estimator 
replace first_treatment_1 = "" if first_treatment_1 == "Inf"
destring first_treatment_1, replace
destring imd, replace force
destring pop_density, replace force
destring avg_age, replace force
destring prop_same_eth_group, replace force
destring single_adult_hh_prop, replace force
destring avg_health_score, replace force
destring income_rank, replace force
destring employment_rank, replace force
destring education_rank, replace force
destring health_rank, replace force
destring crime_rank, replace force
destring barriers_rank, replace force
destring living_env_rank, replace force



*1) do a basic BJS estimation for the grand total of crimes

*do the regression
did_imputation log_num_crimes location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm)

*save the coefficient vector and the SEs as a csv
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_all.csv", cells("b se") plain replace noobs

*1a) do it with controls

*do the regression
did_imputation log_num_crimes location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm) timec(imd pop_density avg_age prop_same_eth_group single_adult_hh_prop avg_health_score)

esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_all_controls.csv", cells("b se") plain replace noobs



*4) do it for the sum of thefts and robberies

*create the variable for the sum of thefts and robberies
gen log_theft_and_robbery = log(theft_from_the_person + robbery + 1)

*do the regression
did_imputation log_theft_and_robbery location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm)

*save the results
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_theft_and_robbery.csv", cells("b se") plain replace noobs


*4a) do it with controls

*first with just constructed controls

*do the regression
did_imputation log_theft_and_robbery location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm) timec(pop_density avg_age prop_same_eth_group single_adult_hh_prop avg_health_score)

*save the results
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_theft_and_robbery_controls_constructed.csv", cells("b se") plain replace noobs

*now with IMD and the other ranks as well

*do the regression
did_imputation log_theft_and_robbery location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm) timec(imd income_rank employment_rank education_rank health_rank crime_rank barriers_rank living_env_rank)

*save the results
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_theft_and_robbery_controls_IMD.csv", cells("b se") plain replace noobs

*now with all controls

*do the regression
did_imputation log_theft_and_robbery location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm) timec(imd income_rank employment_rank education_rank health_rank crime_rank barriers_rank living_env_rank pop_density avg_age prop_same_eth_group single_adult_hh_prop avg_health_score)

*save the results
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_theft_and_robbery_controls_all.csv", cells("b se") plain replace noobs
 *Convergence of standard errors is not achieved for coefs - try increasing the tolerance or number of iterations


*4) now do it for rich vs poor areas

*calculate median IMD only over treated locations

*first note 22 (792/36) locations have missing IMD because of a bad merge - drop these
drop if imd == "NA"
destring imd, replace
*then get the median
summarize imd if first_treatment_1 < ., detail
local imd_median = r(p50)

*create a variable giving whether the observation is high wealth or low wealth (for weighted averages)
gen wealth = 0
replace wealth = 1 if (imd > `imd_median' & first_treatment_1 < .)
replace wealth = -1 if (imd <= `imd_median' & first_treatment_1 < .)

*create weight vectors, for each post-period
forvalues t = 0/16 {
    qui gen W`t' = 0
    qui replace W`t' = wealth if event_time_1 == `t' & first_treatment_1 < .
    
    * Count observations where W equals 1 or -1
    qui count if W`t' == 1
    local sum_high = r(N)
    
    qui count if W`t' == -1
    local sum_low = r(N)
    
    * Normalize
    qui replace W`t' = W`t' / `sum_high' if first_treatment_1 < . & W`t' == 1
    qui replace W`t' = W`t' / `sum_low' if first_treatment_1 < . & W`t' == -1
}

*now use them in the BJS estimation
did_imputation log_num_crimes location period first_treatment_1, pre(20) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum cluster(msoa21nm)

*then save the coefficient vector
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_wealth_all.csv", cells("b se") plain replace noobs




*5) loop over all other crime types and do the BJS estimation for each - first the basic one

local crime_list "log_violence_and_sexual_offences log_antisocial_behaviour log_vehicle_crime log_other_theft log_burglary log_antisocial_behaviour log_shoplifting log_criminal_damage_and_arson log_other_crime log_possession_of_weapons log_bicycle_theft log_drugs log_public_order log_theft_from_the_person log_robbery"

*do the regressions in a loop for each crime type
foreach crime of local crime_list {
    di "Doing BJS estimation for `crime'"
    
    *basic BJS estimation
    did_imputation `crime' location period first_treatment_1, allhorizons pre(20) cluster(msoa21nm)
    
    *save the coefficient vector and the SEs as a csv
    esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_`crime'.csv", cells("b se") plain replace noobs
}




*6) now the wealth-differentiated one

foreach crime of local crime_list {
    di "Doing wealth-differentiated BJS estimation for `crime'"
    
    *wealth-differentiated BJS estimation
    did_imputation `crime' location period first_treatment_1, pre(20) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum cluster(msoa21nm)
    
    *save the coefficient vector and the SEs as a csv
    esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_wealth_`crime'.csv", cells("b se") plain replace noobs
}




*7) now do this for four distance bands between 0km and 1km, on log(thefts + robberies + 1)

*generate the outcome variable
gen log_theft_and_robbery = log(theft_from_the_person + robbery + 1)

*destring the min_active_dist variable
destring min_active_dist, replace force

*first create indicators for each distance band, for the weights
*the distance bands are 0m to 250m, 250m to 500m, 500m to 750m, and 750m to 1km
gen band_0_250m = (min_active_dist <= 0.25)
gen band_250_500m = (min_active_dist > 0.25 & min_active_dist <= 0.5)
gen band_500_750m = (min_active_dist > 0.5 & min_active_dist <= 0.75)
gen band_750_1000m = (min_active_dist > 0.75 & min_active_dist <= 1)

*now loop through each of these bins, create weights for each post-period, and do the BJS estimation
forvalues i = 1/4 {
    local bin_var = ""
    if `i' == 1 {
        local bin_var = "band_0_250m"
    }
    else if `i' == 2 {
        local bin_var = "band_250_500m"
    }
    else if `i' == 3 {
        local bin_var = "band_500_750m"
    }
    else if `i' == 4 {
        local bin_var = "band_750_1000m"
    }
    
    *create weight vectors for this distance band
    forvalues t = 0/16 {
        qui gen W`t' = 0
        qui replace W`t' = `bin_var' if event_time_1 == `t' & first_treatment_1 < .
        
        * Count observations where W equals 1
        qui count if W`t' == 1
        local sum_bin = r(N)
        
        * Normalize
        qui replace W`t' = W`t' / `sum_bin' if first_treatment_1 < . & W`t' == 1
    }
    
    *now do the BJS estimation for this distance band, for the thefts + robberies outcome
    did_imputation log_theft_and_robbery location period first_treatment_1, pre(20) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum cluster(msoa21nm)
    
    *save the coefficient vector and the SEs as a csv
    esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_distance_band_`i'.csv", cells("b se") plain replace noobs

    *report when done
    di "Done with distance band `i'"

    *drop the weight variables before the next iteration
    drop W0-W16
}


*7a) now the same but with controls
*now loop through each of these bins, create weights for each post-period, and do the BJS estimation
forvalues i = 1/4 {
    local bin_var = ""
    if `i' == 1 {
        local bin_var = "band_0_250m"
    }
    else if `i' == 2 {
        local bin_var = "band_250_500m"
    }
    else if `i' == 3 {
        local bin_var = "band_500_750m"
    }
    else if `i' == 4 {
        local bin_var = "band_750_1000m"
    }
    
    *create weight vectors for this distance band
    forvalues t = 0/16 {
        qui gen W`t' = 0
        qui replace W`t' = `bin_var' if event_time_1 == `t' & first_treatment_1 < .
        
        * Count observations where W equals 1
        qui count if W`t' == 1
        local sum_bin = r(N)
        
        * Normalize
        qui replace W`t' = W`t' / `sum_bin' if first_treatment_1 < . & W`t' == 1
    }
    
    *now do the BJS estimation for this distance band, for the thefts + robberies outcome
    did_imputation log_theft_and_robbery location period first_treatment_1, pre(20) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum cluster(msoa21nm) timec(imd pop_density avg_age prop_same_eth_group single_adult_hh_prop avg_health_score) nose
    
    *save the coefficient vector and the SEs as a csv
    esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_distance_band_`i'_controls.csv", cells("b se") plain replace noobs

    *report when done
    di "Done with distance band `i'"

    *drop the weight variables before the next iteration
    drop W0-W16
}




*8) add a triple difference estimation, for robustness, using a 1km cutoff around treated and untreated stations

*this calculates it on a subset of the data, losing 62,000 of the 250,000ish post-treatment observations, because it cannot impute
*UNDERSTAND WHY!!
*it stays the same for different outcomes, suggesting this is an issue with how we define groups vs how we define treatment
*'If you include group#period FEs, imputation is further impossible once all units in the group have been treated'
    * are all units in either within_1km (no) or closest_station treated at some point? maybe, in congested areas with lots of stations
*THIS NEEDS TO BE SORTED OUT - I THINK THERE ARE GROUPS (NEAREST STATIONS) WITH ALL UNITS WITHIN 1KM
* confirm this
*it only cannot impute for regions <1km from a station, but with loads of closest stations, some treated and some not
    *LOOK MORE INTO THIS

*now split units observed in a period in two ways: 
    *by whether they are within 1km of a station or not
    *by what their nearest station is

*make these variables
gen within_1km = (min_any_dist <= 1)

*convert station to numeric
encode closest_station, gen(closest_station_n)

*create interaction
egen id = group(closest_station_n within_1km)

*now do the regression
*have to use autosample to get it to work
did_imputation log_num_crimes id period first_treatment_1, fe(id within_1km#period closest_station_n#period) allhorizons pre(20) autosample cluster(msoa21nm)
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_triple_all.csv", cells("b se") plain replace noobs

*same for specific crimes: first robbery
did_imputation log_robbery id period first_treatment_1, fe(id within_1km#period closest_station_n#period) allhorizons pre(20) autosample cluster(msoa21nm)
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_triple_robbery.csv", cells("b se") plain replace noobs

*now theft
did_imputation log_theft_from_the_person id period first_treatment_1, fe(id within_1km#period closest_station_n#period) allhorizons pre(20) autosample cluster(msoa21nm)
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_triple_theft.csv", cells("b se") plain replace noobs







/* destring min_nt_central_dist min_nt_piccadilly_dist min_nt_victoria_dist min_nt_northern_dist min_nt_victoria_dist, replace force
egen min_nt_dist = rowmin(min_nt_central_dist min_nt_piccadilly_dist min_nt_victoria_dist min_nt_northern_dist min_nt_victoria_dist)
*count the number of observations for whom min_any_dist < min_nt_dist < 1
gen tag = (min_any_dist < min_nt_dist & min_nt_dist < 1) */