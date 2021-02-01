clear all

local dir `c(pwd)'

	if substr("`dir'",1,10) == "/Users/Lio"{
		global user "/Users/Lio"
	}
	if substr("`dir'",1,11) == "/Users/Pier"{
	global user "/Users/Pier"
	}	
	if substr("`dir'",1,15) == "/Users/beatrice"{
	global user "/Users/beatrice"
	}	

global dir "$user/Dropbox/Teaching_Economics/Full Data/Respondi" // working folder
global trade "$dir/Trade" // working folder
global data_trade "$trade/Data"
*global output_trade "$trade/Output/Tables/Reg_tables/06 Mechanisms/interactions/political_x_video_treat"
global output_trade "$trade/Output/Tables/Reg_tables/Interactions/independent x video treat"



global date "2019_09_18"


use "$data_trade/Analytic/data_regression_$date.dta", replace

gen dummy_independents=(political_agg==3)

label define treatment_tables 1 "No Treatment" 2 "Redistribution T" 3 "Efficiency T" 4 "Economist T" 5 "Economist T - US" 
label val treatment treatment_tables


label var dummy_Q06001_1 "\begin{tabular}[c]{@{}c@{}} Large \\ corporations \end{tabular}" 
label var dummy_Q06001_2 "\begin{tabular}[c]{@{}c@{}} Small \\ businesses \end{tabular}" 
label var dummy_Q06001_3 "\begin{tabular}[c]{@{}c@{}} High-income \\ households \end{tabular}" 
label var dummy_Q06001_4 "\begin{tabular}[c]{@{}c@{}} Middle-class \\ households \end{tabular}" 
label var dummy_Q06001_5 "\begin{tabular}[c]{@{}c@{}} Low-income \\ households \end{tabular}" 

label var dummy_Q06002 "\begin{tabular}[c]{@{}c@{}} Both winners \\ and losers \end{tabular}" 
label var dummy_Q06003 "\begin{tabular}[c]{@{}c@{}} Most jobs \\ affected \end{tabular}" 
label var dummy_Q06004 "\begin{tabular}[c]{@{}c@{}} Hurt \\ overall \end{tabular}" 
label var dummy_Q06005 "\begin{tabular}[c]{@{}c@{}} Low-skilled \\ change sector \end{tabular}" 
label var dummy_Q06006 "\begin{tabular}[c]{@{}c@{}} High-skilled \\ change sector \end{tabular}" 

label var dummy_Q06007_1 "\begin{tabular}[c]{@{}c@{}} Unemployment in \\ some sectors \end{tabular}" 
label var dummy_Q06007_2 "\begin{tabular}[c]{@{}c@{}} Rise of \\ inequality \end{tabular}" 	

	* Efficiency

label var dummy_Q06008 "\begin{tabular}[c]{@{}c@{}} Decreased \\ prices \end{tabular}" 
label var dummy_Q06009 "\begin{tabular}[c]{@{}c@{}} Made firms \\ more competitive \end{tabular}" 
label var dummy_Q06010 "\begin{tabular}[c]{@{}c@{}} Increased \\ innovation \end{tabular}" 
label var dummy_Q06011 "\begin{tabular}[c]{@{}c@{}} Increased growth \\ of GDP \end{tabular}" 
label var dummy_Q06012 "\begin{tabular}[c]{@{}c@{}} Decrease value \\ dollar \end{tabular}" 

	
		* Case Study

label var dummy_Q06013 "\begin{tabular}[c]{@{}c@{}} Both countries \\ not better off \end{tabular}" 
label var dummy_Q06014 "\begin{tabular}[c]{@{}c@{}} Against inferior \\ imports \end{tabular}" 

label var dummy_Q06015 "\begin{tabular}[c]{@{}c@{}} Car prices \\ increase \end{tabular}" 
label var dummy_Q06016 "\begin{tabular}[c]{@{}c@{}} HH not \\ better off \end{tabular}" 
label var dummy_Q06017 "\begin{tabular}[c]{@{}c@{}} car wages (US) \\ decrease  \end{tabular}" 


label var dummy_Q06018 "\begin{tabular}[c]{@{}c@{}} No decrease \\ lap. prices (abroad) \end{tabular}" 
label var dummy_Q06019 "\begin{tabular}[c]{@{}c@{}} No increase \\ high-skill wages \end{tabular}" 
label var dummy_Q06020 "\begin{tabular}[c]{@{}c@{}} No increase \\ displ. low-skill wages \end{tabular}" 

label var dummy_Q06021 "\begin{tabular}[c]{@{}c@{}} Lap exp tax \\ increase prices abroad \end{tabular}"
label var dummy_Q06022 "\begin{tabular}[c]{@{}c@{}} Car imp tariff \\ increase prices \end{tabular}"

label var dummy_Q06023 "\begin{tabular}[c]{@{}c@{}} Export increase \\ No wage increase \end{tabular}"
label var dummy_Q06024 "\begin{tabular}[c]{@{}c@{}} Import increase \\ Wage decrease \end{tabular}"


label var dummy_Q07001 "\begin{tabular}[c]{@{}c@{}} US should not  \\ aim for trade \end{tabular}" 
label var dummy_Q07002 "Unfairness" 
label var dummy_Q07003 "Dissatisfaction"
label var dummy_Q07007 "Food security" 
label var dummy_Q07008 "\begin{tabular}[c]{@{}c@{}} Tariffs from \\ other countries?  \end{tabular}" 
label var dummy_Q07009 "\begin{tabular}[c]{@{}c@{}} Infant \\ industry \end{tabular}" 

label var dummy_Q07004 "\begin{tabular}[c]{@{}c@{}} Dummy indus. \\ to protect \end{tabular}" 
label var dummy_Q07005 "\begin{tabular}[c]{@{}c@{}} Dummy goods \\ to protect \end{tabular}" 
label var Q07004_nb "\begin{tabular}[c]{@{}c@{}} Number indus. \\ to protect \end{tabular}" 
label var Q07005_nb "\begin{tabular}[c]{@{}c@{}} Number goods \\ to protect \end{tabular}" 

label var dummy_Q07006_1 "Restrict imports" 
label var dummy_Q07006_2 "Direct assistance, retraining" 
label var dummy_Q07006_3 "Subsidize production" 

label var dummy_G08002 "HIgh incomes" 
label var dummy_G08002 "Middle Class" 
label var dummy_G08004_1 "Transfers to people out of work" 
label var dummy_G08004_2 "Better schools" 
label var dummy_G08004_3 "Retraining" 
label var dummy_G08004_4 "Healtcare subsidies" 
label var dummy_G08004_5 "Wage subsidies" 


label var dummy_G10001 "Trust" 
label var dummy_G10002 "Purposes" 
label var dummy_G10003 "Involvment" 
label var G10004_en "Cents wasted" 
label var dummy_G10005 "Satisfaction wasted" 


label var dummy_G10006_1 "\begin{tabular}[c]{@{}c@{}}  Reducing \\ income ineq. \end{tabular}" 
label var dummy_G10006_2 "\begin{tabular}[c]{@{}c@{}}  Reducing \\ wealth transmission \end{tabular}" 
label var dummy_G10006_3 " Health care" 
label var dummy_G10006_4 "\begin{tabular}[c]{@{}c@{}}  Reducing \\ opportunity differential \end{tabular}" 
label var dummy_G10006_5 "\begin{tabular}[c]{@{}c@{}}  Regulating \\ trade \end{tabular}" 
label var dummy_G10006_6 "\begin{tabular}[c]{@{}c@{}}  Finalcial syst. \\ stability \end{tabular}" 
label var dummy_G10006_7 "\begin{tabular}[c]{@{}c@{}}  Dollar \\ stability \end{tabular}" 
label var dummy_G10006_8 "\begin{tabular}[c]{@{}c@{}}  Minimum living \\ standard \end{tabular}" 


global controls "women kids i.race_agg i.agegroup_agg i.order_rando i.incomegroup_agg college econ i.employment_agg i.knowledge_econ_agg low_skilled"

***************
*** Block 6 ***
***************
foreach x in 1 2 3 4 5 {

eststo : reg dummy_Q06001_`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 2 3 4 5 6 {

eststo : reg dummy_Q0600`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
	
foreach x in 1 2 {

eststo : reg dummy_Q06007_`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 08 09 10 11 12 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear



foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 13 14 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 15 16 17 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 18 19 20 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 23 24 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
***************
*** Block 7 ***
***************	


foreach var in 1 2 3 7 8 9 {
eststo : reg dummy_Q0700`var' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd)

}
esttab using "$output_trade/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach var in 1 2 3 {

eststo : reg dummy_Q07006_`var' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls 
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
eststo : reg `var' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  
	
	su `var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


*--------------------------------------*
* TABLE: Specific Government Trade     *
*--------------------------------------*

label var dummy_Q09003 "most people lose, but a few gain a lot"
eststo clear
foreach var of varlist dummy_Q09003  {

	eststo: reg `var' i.mechanisms_rando i.treatment##dummy_independents i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
} 
.
esttab using "$output_trade/specgov_trade.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
		

*-------------------*
* GENERAL OUTCOMES  *
*-------------------*


eststo clear	
foreach var in  dummy_G08002 dummy_G08003  { 	
	eststo: reg `var' i.treatment##dummy_independents i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		
eststo clear	
foreach var in   dummy_G08004_1 dummy_G08004_2 dummy_G08004_3 dummy_G08004_4 dummy_G08004_5 { 	
	eststo: reg `var' i.treatment##dummy_independents i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*---------------------*
* GENERAL GOVERMENT 1 *
*---------------------*


*Regressions
eststo clear
foreach var in dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005 {
	eststo: reg `var' i.treatment##dummy_independents i.political_agg $controls    
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.

esttab using "$output_trade/gen_gov_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*Regressions
eststo clear
foreach var in dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4 dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8  {
	eststo: reg `var' i.treatment##dummy_independents i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.
esttab using "$output_trade/gen_gov_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
eststo clear

		
		
		
		
		
		
		
		
		
		
*-----------------------------*
* POLITICAL X VIDEO TREAT
*-----------------------------*		
global output_trade "$trade/Output/Tables/Reg_tables/Interactions/political x video treat"

***************
*** Block 6 ***
***************
foreach x in 1 2 3 4 5 {

eststo : reg dummy_Q06001_`x' i.mechanisms_rando i.treatment##i.political_agg $controls 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 2 3 4 5 6 {

eststo : reg dummy_Q0600`x' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
	
foreach x in 1 2 {

eststo : reg dummy_Q06007_`x' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 08 09 10 11 12 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear



foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 13 14 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 15 16 17 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 18 19 20 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 23 24 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
***************
*** Block 7 ***
***************	


foreach var in 1 2 3 7 8 9 {
eststo : reg dummy_Q0700`var' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd)

}
esttab using "$output_trade/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach var in 1 2 3 {

eststo : reg dummy_Q07006_`var' i.mechanisms_rando i.treatment##i.political_agg $controls 
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
eststo : reg `var' i.mechanisms_rando i.treatment##i.political_agg $controls  
	
	su `var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


*--------------------------------------*
* TABLE: Specific Government Trade     *
*--------------------------------------*

label var dummy_Q09003 "most people lose, but a few gain a lot"
eststo clear
foreach var of varlist dummy_Q09003  {

	eststo: reg `var' i.mechanisms_rando i.treatment##i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
} 
.
esttab using "$output_trade/specgov_trade.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
		

*-------------------*
* GENERAL OUTCOMES  *
*-------------------*


eststo clear	
foreach var in  dummy_G08002 dummy_G08003  { 	
	eststo: reg `var' i.treatment##i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		
eststo clear	
foreach var in   dummy_G08004_1 dummy_G08004_2 dummy_G08004_3 dummy_G08004_4 dummy_G08004_5 { 	
	eststo: reg `var' i.treatment##i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*---------------------*
* GENERAL GOVERMENT 1 *
*---------------------*


*Regressions
eststo clear
foreach var in dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005 {
	eststo: reg `var' i.treatment##i.political_agg $controls    
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.

esttab using "$output_trade/gen_gov_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*Regressions
eststo clear
foreach var in dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4 dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8  {
	eststo: reg `var' i.treatment##i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.
esttab using "$output_trade/gen_gov_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


		
		
		
		


		
		
*-----------------------------*
* POLITICAL X VIDEO TREAT
*-----------------------------*		
global output_trade "$trade/Output/Tables/Reg_tables/Interactions/women x you treat"

***************
*** Block 6 ***
***************
eststo clear
foreach x in 1 2 3 4 5 {

eststo : reg dummy_Q06001_`x' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 2 3 4 5 6 {

eststo : reg dummy_Q0600`x' i.mechanisms_rando##women i.treatment i.political_agg $controls  
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
	
foreach x in 1 2 {

eststo : reg dummy_Q06007_`x' i.mechanisms_rando##women i.treatment i.political_agg $controls  
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 08 09 10 11 12 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear



foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 13 14 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 15 16 17 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 18 19 20 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 23 24 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
***************
*** Block 7 ***
***************	


foreach var in 1 2 3 7 8 9 {
eststo : reg dummy_Q0700`var' i.mechanisms_rando##women i.treatment i.political_agg $controls  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd)

}
esttab using "$output_trade/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach var in 1 2 3 {

eststo : reg dummy_Q07006_`var' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
eststo : reg `var' i.mechanisms_rando##women i.treatment i.political_agg $controls  
	
	su `var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


*--------------------------------------*
* TABLE: Specific Government Trade     *
*--------------------------------------*

label var dummy_Q09003 "most people lose, but a few gain a lot"
eststo clear
foreach var of varlist dummy_Q09003  {

	eststo: reg `var' i.mechanisms_rando##women i.treatment i.political_agg $controls 

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
} 
.
esttab using "$output_trade/specgov_trade.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
		

*-------------------*
* GENERAL OUTCOMES  *
*-------------------*


eststo clear	
foreach var in  dummy_G08002 dummy_G08003  { 	
	eststo: reg `var' i.mechanisms_rando##women i.treatment i.political_agg $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		
eststo clear	
foreach var in   dummy_G08004_1 dummy_G08004_2 dummy_G08004_3 dummy_G08004_4 dummy_G08004_5 { 	
	eststo: reg `var' i.mechanisms_rando##women i.treatment i.political_agg $controls
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*---------------------*
* GENERAL GOVERMENT 1 *
*---------------------*


*Regressions
eststo clear
foreach var in dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005 {
	eststo: reg `var' i.mechanisms_rando##women i.treatment i.political_agg $controls 
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.

esttab using "$output_trade/gen_gov_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*Regressions
eststo clear
foreach var in dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4 dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8  {
	eststo: reg `var' i.mechanisms_rando##women i.treatment i.political_agg $controls

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.
esttab using "$output_trade/gen_gov_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


		
		
		
		
		
		
		
		
		
		
		
		
		
		

*-----------------------------*
* POLITICAL X you TREAT
*-----------------------------*		
global output_trade "$trade/Output/Tables/Reg_tables/Interactions/political x you treat"

***************
*** Block 6 ***
***************
eststo clear
foreach x in 1 2 3 4 5 {

eststo : reg dummy_Q06001_`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 2 3 4 5 6 {

eststo : reg dummy_Q0600`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
	
foreach x in 1 2 {

eststo : reg dummy_Q06007_`x' i.mechanisms_rando##i.political_agg i.treatment $controls   
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 08 09 10 11 12 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear



foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 13 14 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 15 16 17 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 18 19 20 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 23 24 {
eststo : reg dummy_Q060`x' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
***************
*** Block 7 ***
***************	


foreach var in 1 2 3 7 8 9 {
eststo : reg dummy_Q0700`var' i.mechanisms_rando##i.political_agg i.treatment $controls  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd)

}
esttab using "$output_trade/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach var in 1 2 3 {

eststo : reg dummy_Q07006_`var' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
eststo : reg `var' i.mechanisms_rando##i.political_agg i.treatment $controls   
	
	su `var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


*--------------------------------------*
* TABLE: Specific Government Trade     *
*--------------------------------------*

label var dummy_Q09003 "most people lose, but a few gain a lot"
eststo clear
foreach var of varlist dummy_Q09003  {

	eststo: reg `var' i.mechanisms_rando##i.political_agg i.treatment $controls 

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
} 
.
esttab using "$output_trade/specgov_trade.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
		

*-------------------*
* GENERAL OUTCOMES  *
*-------------------*


eststo clear	
foreach var in  dummy_G08002 dummy_G08003  { 	
	eststo: reg `var' i.mechanisms_rando##i.political_agg i.treatment $controls 

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		
eststo clear	
foreach var in   dummy_G08004_1 dummy_G08004_2 dummy_G08004_3 dummy_G08004_4 dummy_G08004_5 { 	
	eststo: reg `var' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*---------------------*
* GENERAL GOVERMENT 1 *
*---------------------*


*Regressions
eststo clear
foreach var in dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005 {
	eststo: reg `var' i.mechanisms_rando##i.political_agg i.treatment $controls 
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.

esttab using "$output_trade/gen_gov_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*Regressions
eststo clear
foreach var in dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4 dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8  {
	eststo: reg `var' i.mechanisms_rando##i.political_agg i.treatment $controls 

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.
esttab using "$output_trade/gen_gov_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		

		
		
		
		
		
		
		
	
		

		
		
*-----------------------------*
* POLITICAL X WOMEN TREAT
*-----------------------------*		
global output_trade "$trade/Output/Tables/Reg_tables/Interactions/women x video treat"
global controls "kids i.race_agg i.agegroup_agg i.order_rando i.incomegroup_agg college low_skilled econ i.employment_agg i.knowledge_econ_agg i.candidate_econ"

***************
*** Block 6 ***
***************
eststo clear
foreach x in 1 2 3 4 5 {

eststo : reg dummy_Q06001_`x' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 2 3 4 5 6 {

eststo : reg dummy_Q0600`x' i.mechanisms_rando i.treatment##women $controls   
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
	
foreach x in 1 2 {

eststo : reg dummy_Q06007_`x' i.mechanisms_rando i.treatment##women $controls  
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 08 09 10 11 12 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear



foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 13 14 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 15 16 17 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 18 19 20 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 21 22 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 23 24 {
eststo : reg dummy_Q060`x' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
***************
*** Block 7 ***
***************	


foreach var in 1 2 3 7 8 9 {
eststo : reg dummy_Q0700`var' i.mechanisms_rando i.treatment##women $controls 
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd)

}
esttab using "$output_trade/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach var in 1 2 3 {

eststo : reg dummy_Q07006_`var' i.mechanisms_rando i.treatment##women $controls  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
eststo : reg `var' i.mechanisms_rando i.treatment##women $controls 
	
	su `var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


*--------------------------------------*
* TABLE: Specific Government Trade     *
*--------------------------------------*

label var dummy_Q09003 "most people lose, but a few gain a lot"
eststo clear
foreach var of varlist dummy_Q09003  {

	eststo: reg `var' i.mechanisms_rando i.treatment##women $controls

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
} 
.
esttab using "$output_trade/specgov_trade.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
		

*-------------------*
* GENERAL OUTCOMES  *
*-------------------*


eststo clear	
foreach var in  dummy_G08002 dummy_G08003  { 	
	eststo: reg `var' i.mechanisms_rando i.treatment##women $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		
eststo clear	
foreach var in   dummy_G08004_1 dummy_G08004_2 dummy_G08004_3 dummy_G08004_4 dummy_G08004_5 { 	
	eststo: reg `var' i.mechanisms_rando i.treatment##women $controls
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*---------------------*
* GENERAL GOVERMENT 1 *
*---------------------*


*Regressions
eststo clear
foreach var in dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005 {
	eststo: reg `var' i.mechanisms_rando i.treatment##women $controls
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.

esttab using "$output_trade/gen_gov_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*Regressions
eststo clear
foreach var in dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4 dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8  {
	eststo: reg `var' i.mechanisms_rando i.treatment##women $controls

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.
esttab using "$output_trade/gen_gov_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


		



	
		
*-----------------------------*
* POLITICAL X VIDEO TREAT
*-----------------------------*		
global output_trade "$trade/Output/Tables/Reg_tables/Interactions/you x worse-better off"
global controls "women kids i.race_agg i.agegroup_agg i.order_rando i.incomegroup_agg college low_skilled econ i.employment_agg i.knowledge_econ_agg i.candidate_econ"
	
label var mechanisms_rando "YOU"

label define b 2 "Better off" 3 "Worse off"	
encode Q06201B, gen(Q06201B_en) label(b)
replace Q06201B_en=1 if mechanisms_rando==1

	
***************
*** Block 6 ***
***************
foreach x in 1 2 3 4 5 {

eststo : reg dummy_Q06001_`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 2 3 4 5 6 {

eststo : reg dummy_Q0600`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
	
foreach x in 1 2 {

eststo : reg dummy_Q06007_`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 

}
esttab using "$output_trade/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach x in 08 09 10 11 12 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear



foreach x in 21 22 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 13 14 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 15 16 17 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear	
	
foreach x in 18 19 20 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 21 22 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach x in 23 24 {
eststo : reg dummy_Q060`x'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear
	
***************
*** Block 7 ***
***************	


foreach var in 1 2 3 7 8 9 {
eststo : reg dummy_Q0700`var'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean)
	estadd scalar sd_control = r(sd)

}
esttab using "$output_trade/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear

foreach var in 1 2 3 {

eststo : reg dummy_Q07006_`var'  mechanisms_rando#i.Q06201B_en i.treatment $controls
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
eststo : reg `var'  mechanisms_rando#i.Q06201B_en i.treatment $controls  
	
	su `var' if(treatment==1 & mechanisms_rando==1) 
	estadd scalar mean_control = r(mean) 
	estadd scalar sd_control = r(sd) 
}
esttab using "$output_trade/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace nobase
eststo clear


*--------------------------------------*
* TABLE: Specific Government Trade     *
*--------------------------------------*

label var dummy_Q09003 "most people lose, but a few gain a lot"
eststo clear
foreach var of varlist dummy_Q09003  {

	eststo: reg `var'  mechanisms_rando#i.Q06201B_en i.treatment $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
} 
.
esttab using "$output_trade/specgov_trade.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 
		

*-------------------*
* GENERAL OUTCOMES  *
*-------------------*


eststo clear	
foreach var in  dummy_G08002 dummy_G08003  { 	
	eststo: reg `var'  mechanisms_rando#i.Q06201B_en i.treatment $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 

		
eststo clear	
foreach var in   dummy_G08004_1 dummy_G08004_2 dummy_G08004_3 dummy_G08004_4 dummy_G08004_5 { 	
	eststo: reg `var'  mechanisms_rando#i.Q06201B_en i.treatment $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)
	}
.
esttab using "$output_trade/gen_outcomes_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*---------------------*
* GENERAL GOVERMENT 1 *
*---------------------*


*Regressions
eststo clear
foreach var in dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005 {
	eststo: reg `var'  mechanisms_rando#i.Q06201B_en i.treatment $controls 
	
	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.

esttab using "$output_trade/gen_gov_1.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


*Regressions
eststo clear
foreach var in dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4 dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8  {
	eststo: reg `var'  mechanisms_rando#i.Q06201B_en i.treatment $controls  

	sum `var' if(treatment==1 & mechanisms_rando==1)
	estadd scalar mean_control = r(mean)

} 
.
esttab using "$output_trade/gen_gov_2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) nobase stats(mean_control N, label("Control Group Mean" "Observations")  fmt(2 0)) ///
		label not starlevels(* 0.1 ** 0.05 *** 0.01) nonum replace 


		
		

