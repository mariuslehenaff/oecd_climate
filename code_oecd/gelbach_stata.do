// Prepare variables so that they can be used in R

// Transform a list of characters into a variables list
global controls_factor_vlist ""
foreach var in $controls_factor{
	tab `var', gen(`var'_factor)
	levelsof `var'
	local distinct `r(r)'
	di "`distinct'"
	forvalues j = 2/`distinct'{
		global controls_factor_vlist "$controls_factor_vlist `var'_factor`j'"
	}
}


foreach var in $var_to_decompose{

	// Obtain coefficient+SE from Partial regression
	reg ${var_to_decompose} $group_of_interest $controls $controls_factor_vlist $local_weight
	local b_`group_of_interest'_part_`var_to_decompose': display %4.2f _b[${group_of_interest}]
	local se_`group_of_interest'_`var_to_decompose': display %4.2f _se[${group_of_interest}]

	// Obtain coefficient+SE from Full regression
	reg ${var_to_decompose} $group_of_interest $controls $controls_factor_vlist $indices $local_weight
	local b_`group_of_interest'_full_`var_to_decompose': display %4.2f _b[${group_of_interest}]
	local se_`group_of_interest'_full_`var_to_decompose': display %4.2f _se[${group_of_interest}]

	// Run Gelbach decomposition
	b1x2 ${var_to_decompose} $local_weight, x1all($group_of_interest ${controls} ${controls_factor_vlist}) x2all(${indices}) x1only(${group_of_interest}) x2delta(${option_b1x2})

	// Save the coefficient+SE for each index from the Delta matrix
	forvalues i=1/$nbr_plus_one_indices{
		mat b = e(Delta)
		local bn_`var_to_decompose'_`i' =  b[1,`i']
		local b_`var_to_decompose'_`i': di %6.4f `bn_`var_to_decompose'_`i''

		mat se = r(table)
		local sen_`var_to_decompose'_`i' =  se[2,`i']
		local se_`var_to_decompose'_`i': di %6.4f `sen_`var_to_decompose'_`i''
	}
	.
} 
.

// Transform the data to output a matrix with the share explained by each index

clear
set obs $nbr_indices
egen n = seq()
gen shareExplained = .

// Compute share explained
forvalues i=1/$nbr_indices {
	replace shareExplained = `b_`var_to_decompose'_`i''/`b_`group_of_interest'_part_`var_to_decompose''*100  if n==`i'
}
.

// Compute unexplained share
local unexplained `b_`group_of_interest'_full_`var_to_decompose''/`b_`group_of_interest'_part_`var_to_decompose''

di "The unexplained share is " `unexplained'
