*this do-file does the BJS (2024) estimation of the treatment effect - better in Stata than R

cd "C:\Users\jpmcl\OneDrive\Documents\Economics\Papers (WIP)"

import delimited "Crime and night tubes EXTRA DATA\final_data_for_stata.csv"

*prepare the data for the estimator 
replace first_treatment_1 = "" if first_treatment_1 == "Inf"
destring first_treatment_1, replace


*1) do a basic BJS estimation with 

*do the basic BJS estimation

*below works fast
did_imputation log_num_crimes location period first_treatment_1

*this works
did_imputation log_num_crimes location period first_treatment_1, allhorizons

*do 
did_imputation log_num_crimes location period first_treatment_1, allhorizons pre(10)