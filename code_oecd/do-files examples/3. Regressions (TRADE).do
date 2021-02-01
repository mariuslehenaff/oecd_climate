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
	.

global dir "$user/Dropbox/Teaching_Economics/Full Data/Respondi" // working folder
global trade "$dir/Trade" // working folder
global data_trade "$trade/Data"
global output_trade "$data_trade/Output"

global date "2019_09_18"


use "$data_trade/Analytic/data_regression_$date.dta", clear


global controls "women kids i.race_agg i.agegroup_agg i.incomegroup_agg college econ i.employment_agg i.knowledge_econ_agg index_patriotism i.candidate_econ "



***************
*** Block 3 ***
***************


eststo : reg dummy_Q03001 $controls
	
sum dummy_Q03001 if(treatment==1 & mechanisms_rando==1) 
estadd scalar mean_control = r(mean) 

esttab using "$trade/Output/Tables/Reg_tables/03 Personal Exposure/03.pers_exposure.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear


***************
*** Block 6 ***
***************
label var dummy_Q06001_1 "\begin{tabular}[c]{@{}c@{}} Large \\ corporations \end{tabular}" 
label var dummy_Q06001_2 "\begin{tabular}[c]{@{}c@{}} SMall \\ businesses \end{tabular}" 
label var dummy_Q06001_3 "\begin{tabular}[c]{@{}c@{}} High-income \\ households \end{tabular}" 
label var dummy_Q06001_4 "\begin{tabular}[c]{@{}c@{}} Middle-class \\ households \end{tabular}" 
label var dummy_Q06001_5 "\begin{tabular}[c]{@{}c@{}} Low-income \\ households \end{tabular}"

foreach x in 1 2 3 4 5 {
#delimit ; 
eststo : reg dummy_Q06001_`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/01 Distributional/06.mechanisms_dist_01.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear


drop dummy_Q06001_1 dummy_Q06001_3
gen dummy_Q06001_1=(Q06001_1_en >3 & !mi(Q06001_1_en))
gen dummy_Q06001_3=(Q06001_3_en >3 & !mi(Q06001_3_en))

label var dummy_Q06001_1 "\begin{tabular}[c]{@{}c@{}} Large \\ corporations \end{tabular}"  
label var dummy_Q06001_3 "\begin{tabular}[c]{@{}c@{}} High-income \\ households \end{tabular}" 



foreach x in 1 2 3 4 5 {
#delimit ; 
eststo : reg dummy_Q06001_`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q06001_`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/01 Distributional/06.mechanisms_dist_01_v2.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

label var dummy_Q06002 "\begin{tabular}[c]{@{}c@{}} Both winners \\ and losers \end{tabular}" 
label var dummy_Q06003 "\begin{tabular}[c]{@{}c@{}} Most jobs \\ affected \end{tabular}" 
label var dummy_Q06004 "\begin{tabular}[c]{@{}c@{}} Hurt \\ overall \end{tabular}" 
label var dummy_Q06005 "\begin{tabular}[c]{@{}c@{}} Low-skilled \\ change sector \end{tabular}" 
label var dummy_Q06006 "\begin{tabular}[c]{@{}c@{}} High-skilled \\ change sector \end{tabular}" 

foreach x in 2 3 4 5 6 {
#delimit ; 
eststo : reg dummy_Q0600`x' incentives i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q0600`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/01 Distributional/06.mechanisms_dist_02_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear
	
label var dummy_Q06007_1 "\begin{tabular}[c]{@{}c@{}} Unemployment in \\ some sectors \end{tabular}" 
label var dummy_Q06007_2 "\begin{tabular}[c]{@{}c@{}} Rise of \\ inequality \end{tabular}" 	
	
foreach x in 1 2 {
#delimit ; 
eststo : reg dummy_Q06007_`x' incentives i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q06007_`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/01 Distributional/06.mechanisms_dist_07.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear
	
	
	
	* Efficiency

label var dummy_Q06008 "\begin{tabular}[c]{@{}c@{}} Decreased \\ prices \end{tabular}" 
label var dummy_Q06009 "\begin{tabular}[c]{@{}c@{}} Made firms \\ more competitive \end{tabular}" 
label var dummy_Q06010 "\begin{tabular}[c]{@{}c@{}} Increased \\ innovation \end{tabular}" 
label var dummy_Q06011 "\begin{tabular}[c]{@{}c@{}} Increased growth \\ of GDP \end{tabular}" 
label var dummy_Q06012 "\begin{tabular}[c]{@{}c@{}} Decrease value \\ dollar \end{tabular}" 
	
foreach x in 08 09 10 11 12 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/02 Efficiency/06.mechanisms_eff_08_12.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

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

foreach x in 21 22 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/03 Case Study/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

foreach x in 13 14 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/03 Case Study/06.mechanisms_case_13_14.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear	
	
foreach x in 15 16 17 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ;  
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/03 Case Study/06.mechanisms_case_15_17.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear	
	
foreach x in 18 19 20 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	estadd scalar mean_control = r(mean) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/03 Case Study/06.mechanisms_case_18_20.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

foreach x in 21 22 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/03 Case Study/06.mechanisms_case_21_22.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

foreach x in 23 24 {
#delimit ; 
eststo : reg dummy_Q060`x' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q060`x' if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/06 Mechanisms/03 Case Study/06.mechanisms_case_23_24.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear
	
***************
*** Block 7 ***
***************	

label var dummy_Q07001 "\begin{tabular}[c]{@{}c@{}} US should not \\ aim for trade \end{tabular}" 
label var dummy_Q07002 "Unfairness" 
label var dummy_Q07003 "Dissatisfaction"
label var dummy_Q07007 "Food security" 
label var dummy_Q07008 "\begin{tabular}[c]{@{}c@{}} Tariffs from \\ other countries?  \end{tabular}" 
label var dummy_Q07009 "\begin{tabular}[c]{@{}c@{}} Infant \\ industry \end{tabular}" 


foreach var in 1 2 3 7 8 9 {
#delimit ; 
eststo : reg dummy_Q0700`var' i.treatment i.mechanisms_rando i.order_rando
	$controls ;  
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/07 Specific Outcomes/07.outcomes.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

label var dummy_Q07006_1 "Restrict imports" 
label var dummy_Q07006_2 "Direct assistance, retraining" 
label var dummy_Q07006_3 "Subsidize production"

foreach var in 1 2 3 {
#delimit ; 
eststo : reg dummy_Q07006_`var' i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su dummy_Q0700`var' if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/07 Specific Outcomes/07.outcomes_06.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

label var dummy_Q07004 "\begin{tabular}[c]{@{}c@{}} Dummy indus. \\ to protect \end{tabular}" 
label var dummy_Q07005 "\begin{tabular}[c]{@{}c@{}} Dummy goods \\ to protect \end{tabular}" 
label var Q07004_nb "\begin{tabular}[c]{@{}c@{}} Number indus. \\ to protect \end{tabular}" 
label var Q07005_nb "\begin{tabular}[c]{@{}c@{}} Number goods \\ to protect \end{tabular}" 


foreach var in dummy_Q07004 dummy_Q07005 Q07004_nb Q07005_nb {
#delimit ; 
eststo : reg `var' incentives i.treatment i.mechanisms_rando i.order_rando
	$controls ; 
	
	su `var' if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr
}
esttab using "$trade/Output/Tables/Reg_tables/07 Specific Outcomes/07.outcomes_04_05.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

***************
*** Block 9 ***
***************

label var dummy_Q09003 "\begin{tabular}[c]{@{}c@{}} Most people \\ lose \end{tabular}" 

#delimit ; 
eststo : reg dummy_Q09003 incentives i.treatment i.mechanisms_rando i.order_rando
	$controls ;  
	
	su dummy_Q09003 if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr

esttab using "$trade/Output/Tables/Reg_tables/09 Specific Government/09.spec_gov.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear


		*******************************
		** Block 8: General Outcomes **
		*******************************	
		
		
foreach x in  2 3 {
#delimit ; 	
	eststo : reg dummy_G0800`x' incentives i.treatment i.mechanisms_rando i.order_rando
	women kids i.race_agg i.agegroup_agg 
	i.incomegroup_agg
	i.college_degree_area i.employment_agg i.knowledge_econ_agg 
	social_media index_patriotism
	i.candidate_econ
	if(complete==1), nobase; 
#delimit cr

	sum dummy_G0800`x' if treatment==1 & mechanisms_rando==1
estadd scalar mean_control=r(mean)
	}
esttab using "$trade/Output/Tables/Reg_tables/08 General Outcomes/08.general_outcomes_A.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0))  width(0.8\hsize) not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace 
eststo clear

		
		
foreach x in 1 2 3 4 5 {
#delimit ; 	
	eststo : reg dummy_G08004_`x' incentives i.treatment i.mechanisms_rando i.order_rando
	women kids i.race_agg i.agegroup_agg 
	i.incomegroup_agg
	i.college_degree_area i.employment_agg i.knowledge_econ_agg 
	social_media index_patriotism
	i.candidate_econ
	if(complete==1), base ; 
#delimit cr

	sum dummy_G08004_`x' if treatment==1 & mechanisms_rando==1
estadd scalar mean_control=r(mean)
	}
esttab using "$trade/Output/Tables/Reg_tables/08 General Outcomes/08.general_outcomes_B.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0))  width(1.4 \hsize) noconstant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace 
eststo clear



		**********************************
		** Block 10: General Government **
		**********************************
	
foreach var of varlist dummy_G10001 dummy_G10002 dummy_G10003 G10004_en dummy_G10005    {
#delimit ;
	eststo : reg `var' incentives i.treatment i.mechanisms_rando i.order_rando
		women kids i.race_agg i.agegroup_agg 
		i.incomegroup_agg 
		i.college_degree_area i.employment_agg i.knowledge_econ_agg 
		social_media index_patriotism
		i.candidate_econ
		if(complete==1), base ;
#delimit cr
sum `var' if treatment==1 & mechanisms_rando==1
estadd scalar mean_control=r(mean)
}

esttab using "$trade/Output/Tables/Reg_tables/10 General Government/10.general_gov_A.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0))  width(1 \hsize) noconstant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace 
eststo clear		


foreach var of varlist  dummy_G10006_1 dummy_G10006_2 dummy_G10006_3 dummy_G10006_4  {
#delimit ;
	eststo : reg `var' incentives i.treatment i.mechanisms_rando i.order_rando
		women kids i.race_agg i.agegroup_agg 
		i.incomegroup_agg 
		i.college_degree_area i.employment_agg i.knowledge_econ_agg 
		social_media index_patriotism
		i.candidate_econ
		if(complete==1), base;
#delimit cr
sum `var' if treatment==1 & mechanisms_rando==1
estadd scalar mean_control=r(mean)
}

esttab using "$trade/Output/Tables/Reg_tables/10 General Government/10.general_gov_C.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0))  width(1 \hsize) noconstant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear

foreach var of varlist   dummy_G10006_5 dummy_G10006_6 dummy_G10006_7 dummy_G10006_8 {
#delimit ;
	eststo : reg `var' incentives i.treatment i.mechanisms_rando i.order_rando
		women kids i.race_agg i.agegroup_agg 
		i.incomegroup_agg 
		i.college_degree_area i.employment_agg i.knowledge_econ_agg 
		social_media index_patriotism
		i.candidate_econ
		if(complete==1), base;
#delimit cr
sum `var' if treatment==1 & mechanisms_rando==1
estadd scalar mean_control=r(mean)
}

esttab using "$trade/Output/Tables/Reg_tables/10 General Government/10.general_gov_D.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0))  width(1 \hsize) noconstant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace 
eststo clear


	***************
	***** WTP *****
	***************
	
foreach i in wtp1 wtp2 wtp5 wtp10  {
gen `i'_dummy=0
replace `i'_dummy=1 if `i'!=.
}
.

foreach var in wtp_agg dummy_Q12002 {
#delimit ; 
eststo : reg `var' incentives i.treatment i.mechanisms_rando acc_dummy
	women kids i.race_agg i.agegroup_agg 
	i.incomegroup_agg 
	i.college_degree_area i.employment_agg i.knowledge_econ_agg 
	social_media index_patriotism
	i.candidate_econ
	wtp2_dummy wtp5_dummy wtp10_dummy , base ; 
	
	su wtp_agg if(treatment==1 & mechanisms_rando==1) ;
	return list ;
	estadd scalar mean_control = r(mean) ;
	estadd scalar sd_control = r(sd) ;
#delimit cr
}
.

esttab using "$trade/Output/Tables/Reg_tables/99 WTP/wtp.tex", cells(b(fmt(2) star) se(par fmt(2))) collabels(none) label  ///
		stats(mean_control N, fmt(2 0)) constant not starlevels(* 0.1 ** 0.05 *** 0.01) r2 replace
eststo clear
