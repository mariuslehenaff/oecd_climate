foreach var in $var_to_decompose{
	reg ${var_to_decompose} $group_of_interest $controls
	local b_`group_of_interest'_part_`var_to_decompose': display %4.2f _b[${group_of_interest}]
	local se_`group_of_interest'_`var_to_decompose': display %4.2f _se[${group_of_interest}]


	reg ${var_to_decompose} $group_of_interest $controls $indices
	local b_`group_of_interest'_full_`var_to_decompose': display %4.2f _b[${group_of_interest}]
	local se_`group_of_interest'_full_`var_to_decompose': display %4.2f _se[${group_of_interest}]


	b1x2 ${var_to_decompose}, x1all($group_of_interest ${controls}) x2all(${indices}) x1only(${group_of_interest}) x2delta(${option_b1x2})

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

clear
set obs $nbr_indices
egen n = seq()
gen shareExplained = .

forvalues i=1/$nbr_indices {
	replace shareExplained = `b_`var_to_decompose'_`i''/`b_`group_of_interest'_part_`var_to_decompose''*100  if n==`i'
}
.

local unexplained `b_`group_of_interest'_full_`var_to_decompose''/`b_`group_of_interest'_part_`var_to_decompose''

di "The unexplained share is " `unexplained'

