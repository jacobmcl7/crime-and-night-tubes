*this do-file does the BJS (2024) estimation of the treatment effect - better in Stata than R
clear all
cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)"

import delimited "Crime and night tubes EXTRA DATA\final_data_for_stata.csv"

*prepare the data for the estimator 
replace first_treatment_1 = "" if first_treatment_1 == "Inf"
destring first_treatment_1, replace


*1) do a basic BJS estimation for the grand total of crimes

*do the regression
did_imputation log_num_crimes location period first_treatment_1, allhorizons pre(10)

*save the coefficient vector and the SEs as a csv
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_all.csv", cells("b se") plain replace noobs


*2) do a BJS estimation for thefts only

*do the regression
did_imputation log_theft_from_the_person location period first_treatment_1, allhorizons pre(10)

*save the results
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_theft.csv", cells("b se") plain replace noobs


*3) do a BJS estimation for robberies only

*do the regression
did_imputation log_robbery location period first_treatment_1, allhorizons pre(10)

*save the results
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_robbery.csv", cells("b se") plain replace noobs



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
    qui replace W`t' = -W`t' / `sum_low' if first_treatment_1 < . & W`t' == -1
}

*now use them in the BJS estimation
did_imputation log_num_crimes location period first_treatment_1, pre(10) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum

*then save the coefficient vector
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_wealth_all.csv", cells("b se") plain replace noobs



*5) do it for burglaries

*do the regression
did_imputation log_burglary location period first_treatment_1, pre(10) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum

*then save the coefficient vector
esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_wealth_burglary.csv", cells("b se") plain replace noobs


*6) loop over all other crime types and do the BJS estimation for each - first the basic one

local crime_list "log_violence_and_sexual_offences log_vehicle_crime log_other_theft log_burglary log_anti-social_behaviour log_shoplifting log_criminal_damage_and_arson log_other_crime log_possession_of_weapons log_bicycle_theft log_drugs log_public_order log_theft_from_the_person log_robbery"

*do the regressions in a loop for each crime type
foreach crime of local crime_list {
    di "Doing BJS estimation for `crime'"
    
    *basic BJS estimation
    did_imputation `crime' location period first_treatment_1, allhorizons pre(10)
    
    *save the coefficient vector and the SEs as a csv
    esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_`crime'.csv", cells("b se") plain replace noobs
}

*7) now the wealth-differentiated one

foreach crime of local crime_list {
    di "Doing wealth-differentiated BJS estimation for `crime'"
    
    *wealth-differentiated BJS estimation
    did_imputation `crime' location period first_treatment_1, pre(10) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum
    
    *save the coefficient vector and the SEs as a csv
    esttab using "Crime and night tubes EXTRA DATA\BJS results\BJS_results_wealth_`crime'.csv", cells("b se") plain replace noobs
}