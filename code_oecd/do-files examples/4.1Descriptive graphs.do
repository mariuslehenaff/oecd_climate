
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
		if substr("`dir'",1,12) == "/Users/Julia"{
	global user "/Users/Julia"
	}
	.

global dir "$user/Dropbox/Teaching_Economics/Full Data/Respondi" // working folder
global trade "$dir/Trade" // working folder
global data_trade "$trade/Data"
global output_trade "$data_trade/Output"

global date "2019_09_18"


use "$data_trade/Analytic/data_clean_$date.dta", clear




su Durationinminutes if complete==1 & Durationinminutes<120, det
return list
hist Durationinminutes if complete==1 & Durationinminutes <120 , xline(`r(mean)', lcolor(blue) lwidth(thick)) xline(`r(p50)', lcolor(red) lwidth(thick)) ///
	xaxis(1 2) xtitle("", axis(2)) xlabel(0(10)120) xlabel(`r(mean)' "Mean" `r(p50)' "Median", axis(2) angle(45) labsize(medium)) ///
		graphregion(color(white) style(none)) plotregion(color(white)) 
graph display, ysize(5) xsize(8)
		graph save  "$user/Dropbox/Teaching_Economics/Tables_Figures/Figures/Trade/trade_timedistribution.png", replace
		graph export "$user/Dropbox/Teaching_Economics/Tables_Figures/Figures/Trade/trade_timedistribution.png", as(png) replace

su Durationinminutes if complete==1 & Durationinminutes<120, det
return list
hist Durationinminutes if complete==1 & Durationinminutes <120 , xline(`r(mean)', lcolor(blue) lwidth(thick)) xline(`r(p50)', lcolor(red) lwidth(thick)) ///
	xaxis(1 2) xtitle("", axis(2)) xlabel(0(10)120) xlabel(`r(mean)' "Mean" `r(p50)' "Median", axis(2) angle(45) labsize(medium))  ///
	graphregion(color(white)) plotregion(color(white)) 
graph display, ysize(5) xsize(8)
		graph save "$dir/../../Paper_redistribution/figures/income_timedistribution.png", replace
		graph export "$dir/../../Paper_redistribution/figures/income_timedistribution.png", as(png) replace

		
		
		
use "$data_trade/Analytic/data_regression_$date.dta", clear

rename Q04002_en tariff
rename Q04004_en imp_quota
	
foreach x in tariff imp_quota    {
sum `x'
sca tot_`x'=r(N)
count if `x'==2
sca `x'_yes = (r(N)/tot_`x')*100
count if `x'==1
sca `x'_no = (r(N)/tot_`x')*100
}
.

preserve

	 clear
	 set obs 2
	 egen t = seq() 
	 label define t  1 "Tariff" 2 "Import Quota"
	 label values t t
	 gen know = .
	 gen dont_know = .

	 replace know=tariff_yes if t==1
	 replace know=imp_quota_yes if t==2
	 
	 replace dont_know=tariff_no if t==1
	 replace dont_know=imp_quota_no if t==2
	 

graph bar know dont_know, over(t) stack bargap(70)  outergap(100) asyvars bar(1, fcolor(navy*1.2)) bar(2, fcolor(red*1.1)) legend(order(1 2) size(small) symysize(small) symxsize(medium) col(2) lab(1 "Self-report to know what the policy is") lab(2 "Self-report not to know what the policy is")) ytitle("% of respondents") title("  ") subtitle(" ") ylabel(0[20]100, labsize(small) grid gmax) graphregion(color(white)) plotregion(color(white))
graph save "$draft_trade/know_tariff_quota.png", replace
graph export "$draft_trade/know_tariff_quota.png", as(png) replace

restore


*********************************

count if !mi(Q04009_en)
sca tot_Q04009 =r(N)
forvalues x=1/15 {
count if Q04009_en==`x'
scalar num_`x'_imp=r(N)
sca ratio_`x'_imp=(num_`x'_imp/tot_Q04009)*100 
}
.


count if !mi(Q04010_en)
sca tot_Q04010 =r(N)
forvalues x=1/15 {
count if Q04010_en==`x'
scalar num_`x'_exp=r(N)
sca ratio_`x'_exp=(num_`x'_exp/tot_Q04010)*100 
}
.




clear
set obs 44
egen t=seq()
gen var=.
label define countries 44 "Brazil" 41 "Canada" 38 "China" 35 "France" 32 "Germany" 29 "India" 26 "Ireland" 23 "Italy" 20 "Japan" 17 "Mexico" 14 "Netherlands" 11 "South Korea" 8 "Switzerland" 5 "Taiwan" 2 "United Kingdom"
label val t countries

		replace var=ratio_1_imp if t==44
	replace var=ratio_1_exp if t==43
		replace var=ratio_2_imp if t==41
	replace var=ratio_2_exp if t==40
		replace var=ratio_3_imp if t==38
	replace var=ratio_3_exp if t==37
		replace var=ratio_4_imp if t==35
	replace var=ratio_4_exp if t==34
		replace var=ratio_5_imp if t==32
	replace var=ratio_5_exp if t==31
		replace var=ratio_6_imp if t==29
	replace var=ratio_6_exp if t==28
		replace var=ratio_7_imp if t==26
	replace var=ratio_7_exp if t==25
		replace var=ratio_8_imp if t==23
	replace var=ratio_8_exp if t==22
		replace var=ratio_9_imp if t==20
	replace var=ratio_9_exp if t==19
		replace var=ratio_10_imp if t==17
	replace var=ratio_10_exp if t==16
		replace var=ratio_11_imp if t==14
	replace var=ratio_11_exp if t==13
		replace var=ratio_12_imp if t==11
	replace var=ratio_12_exp if t==10
		replace var=ratio_13_imp if t==8
	replace var=ratio_13_exp if t==7
		replace var=ratio_14_imp if t==5
	replace var=ratio_14_exp if t==4
		replace var=ratio_15_imp if t==2
	replace var=ratio_15_exp if t==1
				
				
				twoway  (bar var t if (t==43| t==40| t==37 |t==34| t==31 | t==28| t==25| t==22 |t==19 | t==16 | t==13| t==10 |t==7 | t==4 | t==1), horizontal color("0 114 178")) ///	
						(bar var t if (t==44| t==41| t==38 |t==35| t==32 | t==29| t==26| t==23 |t==20 | t==17 | t==14| t==11 |t==8 | t==5 | t==2), horizontal color("213 94 0")), ///
						ytitle("") xlabel(0[5]70, labsize(medium)) ylabel(44 41 38 35 32 29 26 23 20 17 14 11 8 5 2, value labsize(small) angle(0) glcolor(gs16) noticks)  ///
						xtitle("% Frequency ", size(small)) ///
						legend(order(1 2) col(2) lab(1 "Import") lab(2 "Export")) ///
					    graphregion(color(white)) plotregion(color(white))
						graph save "$draft_trade/dist_importexport.png", replace
						graph export "$draft_trade/dist_importexport.png", as(png) replace
						
				
