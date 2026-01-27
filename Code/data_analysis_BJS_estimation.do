*this do-file does the BJS (2024) estimation of the treatment effect - better in Stata than R

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
summarize IMD if first_treatment_1 < ., detail
local imd_median = r(p50)

*create a variable giving whether the observation is high wealth or low wealth (for weighted averages)
gen wealth = 0
replace wealth = 1 if (IMD > `imd_median' & first_treatment_1 < .)
replace wealth = -1 if (IMD >= `imd_median' & first_treatment_1 < .)

*create weight vectors, for each post-period
forvalues t = 0/16 {
    qui gen W`t' = 0
    qui replace W`t' = wealth if event_time == `t' & first_treatment_1 < .
    local sum_high = sum(W`t' == 1)
    local sum_low = sum(W`t' == -1)
    qui replace W`t' = W`t' / `sum_high' if first_treatment_1 < . & W`t' == 1
    qui replace W`t' = -W`t' / `sum_low' if first_treatment_1 < . & W`t' == -1
}

*now use them in the BJS estimation
did_imputation log_num_crimes location period first_treatment_1, allhorizons pre(10) wtr(W0 W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15 W16) sum

*then save the coefficient vector