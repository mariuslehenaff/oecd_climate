************************************
********** Saving dataset **********
************************************
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

global date "2019_09_18"


import excel "$data_trade/Raw/trade_$date.xlsx", sheet("Questionnaire on Trade") firstrow clear

drop in 1/2

** Adding Pilot Data

*append using "/Users/Lio/Dropbox/Teaching_Economics/Full Data/Respondi/Income Taxation/Data/Analytic/data_pilot.dta"

* gen flag_pilot = 0
* 	replace flag_pilot = 1 if(strpos(StartDate, "6/4/2019"))



**********************************
****** Time spent on survey ******
**********************************

destring Durationinseconds, replace force
gen Durationinminutes=Durationinseconds/60 
label var Durationinminutes "Minutes spent on survey"

************************************
******* Adding city and state ******
************************************

* Adding State Name and state. 
destring G01005, gen(zip_code) force
joinby zip_code using "$dir/zip_statename_toptax.dta", unmatched(master)
rename _merge merge
drop TopTaxRate

************************************
********** Recoding timers *********
************************************
foreach var of varlist T0* T1* {
	destring `var', replace force
	}

/*
* There appears to be an issue with the way the timers have be saved onto the database. In certain cases, there is not decimal point.
* This should solve the issue. 

foreach var of varlist T0* T1* {

	gen `var'_= substr(`var', 1,1) + "."+substr(`var', -3,3) if(length(`var')==4 & !strpos(`var', ".")), after(`var')
	replace `var'_= substr(`var', 1,2) + "."+substr(`var', -3,3) if(length(`var')==5 & !strpos(`var', "."))
	replace `var'_= substr(`var', 1,3) + "."+substr(`var', -3,3) if(length(`var')==6 & !strpos(`var', "."))

	replace `var'_=`var' if missing(`var'_)
	destring `var'_, replace force
}
*/


************************************
*********** Save database **********
************************************

save "$data_trade/Analytic/data_$date.dta", replace





