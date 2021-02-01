

*TRADE

global date "2019_09_18"

use "$data_trade/Analytic/data_$date.dta", clear

append using "/Users/beatrice/Dropbox/Teaching_Economics/Full Data/Respondi/Trade/Data/Raw/Pilot/trade_pilot.dta", force

append using "/Users/beatrice/Dropbox/Teaching_Economics/Full Data/Respondi/Trade/Data/Raw/Pilot 2/trade_pilot2.dta", force

destring screen, replace


keep if screen==1

duplicates tag tic, gen(dup)
tab dup

sort tic
quietly by tic: gen dup2 = cond(_N==1,0,_n)
drop if dup2>1
keep tic
save "/Users/beatrice/Desktop/tics4.dta", replace




import excel "/Users/beatrice/Downloads/Matched IDs 09.12.2019 Link 4 (2370).xlsx", sheet("Sheet1") firstrow clear
keep if Status_respondi=="Complete"
keep tic
merge 1:1 tic using "/Users/beatrice/Desktop/tics4.dta", gen(_merge1)
drop if _merge1==2
drop _merge1

export excel using "/Users/beatrice/Desktop/tic_completes_link4.xls", replace




*HEALTH

global date "2019_07_12"

use "$data_health/Analytic/data_$date.dta", clear

append using  "/Users/beatrice/Dropbox/Teaching_Economics/Full Data/Respondi/Health Insurance/Data/Analytic/data_pilot.dta", force
destring screen, replace


keep if screen==1

duplicates tag tic, gen(dup)
tab dup

sort tic
quietly by tic: gen dup2 = cond(_N==1,0,_n)
drop if dup2>1
keep tic
export excel using "/Users/beatrice/Desktop/tic_completes_link3.xls", replace
