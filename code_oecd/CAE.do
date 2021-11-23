/********************************************************************************
					        OECD Climate - CAE Presentation

Goal: Create reg plots						
Date: 		Nov 2021
 
*********************************************************************************/
		 


/********************************************************************************

	0) Preface

********************************************************************************/

	* Set Environments 
		clear all 
		set more off 
		program drop _all 
		set type double 
		set matsize 10000
		
	* Set Date for version control
		global version	  "2021-11-23"
	
		
	* Set directory globals
	
		global gitpath 	"C:\Users\ans7406\Documents\GitHub\oecd_climate"
		global data		"${gitpath}//data/"
		global output 	"${gitpath}//figures//FR/"
		
	* Set Graph Settings
	set scheme opp_insights_contrast
	graph set window fontface "Arial"
		global graphsettings   "graphregion(fcolor(white) lcolor(white) margin( 1 1 1 1 ) ) plotregion( fcolor(none) lcolor(none) margin(1 1 1 1) ) ysize(12) xsize(12)"
		global labeloptions = ", labsize(small) glw(vvthin) glc(gs12) glp(shortdash) gmax angle(horizontal) tlw(vthin) tl(*.5) tlc(black)"
		global legendoptions = "size(3) region(col(white)) lstyle(none) row(1) position(6) symx(2) symy(2) "
		global xlabeloptions = ", labsize(small) glw(vvthin) glc(gs12) glp(shortdash) gmax angle(vertical) tlw(vthin) tl(*.5) tlc(black)"
		global legendoptions2 = "size(3) region(col(white)) lstyle(none) row(2) position(6) symx(2) symy(2) "
		
		global p1 = "41 182 164"
		global p2  ="250 165 35"
		global p3 ="0 58 79"
		global p4 ="127 72 146"
		global p5 ="164 206 78"
		global p6 ="43 143 67"
		global p7 ="0 115 162"
		global p8 ="229 64 96"
		global p9 ="255 212 0"
		global p10 ="107 189 69"	
		
	


/********************************************************************************

	1/ CAE Presentation 

********************************************************************************/

*--------------------------------------
* Open Data 
*--------------------------------------
* Limit to France
use "${data}all.dta" if country=="FR", clear 
	
* Choose outcomes 
	global outcomes1 ""
	
	foreach X in support fair {
	foreach var in standard  tax_transfers  investments  policies   {
		tab `var'_`X', m 
	gen binary_`var'_`X' = `var'_`X' > 0 & `var'_`X'!=. 
	replace binary_`var'_`X' = . if `var'_`X'==. 
	
	global outcomes1 $outcomes1 `var'_`X' 
	}
	}
	
	
	gen binary_limit_driving = willing_limit_driving > 0 
	gen binary_farm_ban = beef_ban_intensive_support > 0
	gen binary_insulation_support = insulation_support > 0 
	global outcomes2 "limit_driving farm_ban insulation_support"
	

	
* Choose sets 
	
* Set A 
	gen vote_agg0 = vote_agg == 0 
	gen vote_agg_left = vote_agg < 0 & vote_agg!=-.1
	gen vote_agg_right = vote_agg > 0 
	gen vote_agg_pnr = vote_agg ==-.1 
	
	gen age18_24 = age=="18-24"
	gen age25_34 = age=="25-34"
	gen age35_49 = age=="35-49"
	gen age50_64 = age=="50-64"
	gen age65plus = age=="65+"
	
	
	global setA ""
	foreach var in "female" "age25_34 age35_49 age50_64 age65plus"    "children"   "i.dominant_origin"       "i.income_factor"       "i.employment_agg"  "i.college"      "vote_agg_right"  "vote_agg0"  "vote_agg_pnr"     {
		global setA $setA `var'
	}
	

	
* Set B 
	gen binary_availability_transport = availability_transport > 0 
	assert availability_transport !=. 
	
	gen binary_gas_expenses = gas_expenses > 50 
	gen binary_heating_expenses = heating_expenses > 500 
	
	*TOCHECK: what does -.1 value means in heating expenses 
	recode heating_expenses (-.1 = 0 )
	
	gen flights_agg_1 = flights_agg > 1
	
	gen car_dependency = affected_transport >= 1 & affected_transport!=. 
	
	
	global setB ""
	foreach var in "i.urban"  "i.binary_availability_transport" "i.binary_gas_expenses"   "i.binary_heating_expenses" "i.car_dependency"  "i.flights_agg_1"   "i.polluting_sector"          "i.owner"   {
		global setB $setB `var'
	} 
	

	
*--------------------------------------
* Graphs: 
* 1/ Reg on SetA - Plot Set A
* 2/ Reg on SetAandB - Plot Set B 
*--------------------------------------
loc index_knowledget "Index knowledge"
loc title_index_knowledge "Knowledge about Climate Change"

loc index_main_policiest "Index main policies"
loc title_index_main_policies "Support policies"

	foreach var in index_knowledge index_main_policies {

	if strpos("`var'", "knowledge") loc i = 1 
	if strpos("`var'", "policies") loc i = 7 
	
	reg `var'  $setA  [w=weight], robust 
	estimates store `var'A

	coefplot ///
	 (`var'A,  drop(_cons ) keep( female age* *children *income_factor *employment_agg 1.dominant_origin  2.college vote*)  xline(0, lcolor(gs8) lpattern(dash)  lwidth(thin)) msize(1)  mlcolor("${p`i'}") mfcolor("${p`i'}") ciopts( lwidth(vthin)  color("${p`i'}") )  )	 , ///
	 $graphsettings xlabel( $labeloptions format(%4.1fc)) ylabel( $labeloptions nogrid ) ///
	 coeflabels(female = "Female" ///
				age25_34 = "25-34 years old" age35_49 = "35-49 years old" age50_64 = "50-64 years old" age65plus = "65+ years old" ///
				children = "Lives with child(ren)<14" /// 
				2.income_factor = "Q2" 3.income_factor = "Q3" 4.income_factor = "Q4" ///
				2.college = "Does not have college degree" ///
				1.dominant_origin = "Country's majority origin" ///
				vote_agg_pnr = "Vote Others or PNR" vote_agg_right = "Vote Right or Far Right" vote_agg0 = "Vote Center"  , labsize(vsmall)) 	///
	headings(female= "{bf: Demographics}" /// 
			 2.income_factor= "{bf: Income}" ///
			 2.employment_agg = "{bf: Employment Status}" ///
			 2.college  = "{bf: Education}" /// 
			 vote_agg_right = "{bf: Political Leaning}" /// 
			 , labsize(small)) ///
			/// title("`title_`var''", size(small) color(black)) ///
			legend(order(2) label(2 "``var't'") pos(12) size(small)) ///
			xtitle("Coefficients", col(gs8) size(small)) ///
			 name(gA, replace) 
			 graph export "${output}\\Coefplot_SetA_PlotA_`var'.png", replace 	
	
	reg `var'  $setA $setB [w=weight] , robust 
	estimates store `var'AB
	
	coefplot ///
	 (`var'AB,  drop(_cons ) keep( *urban *binary_availability_transport *binary_gas_expenses *binary_heating_expenses *car_dependency *flights_agg_1 *polluting_sector *owner)  xline(0, lcolor(gs8) lpattern(dash)  lwidth(thin)) msize(1)  mlcolor("${p`i'}") mfcolor("${p`i'}") ciopts( lwidth(vthin)  color("${p`i'}") )  )	 , ///
	 $graphsettings xlabel( $labeloptions format(%4.1fc)) ylabel( $labeloptions nogrid ) ///
	 coeflabels(1.urban = "Urban" ///		
				1.binary_gas_expenses = "High gas expenses" ///
				1.binary_heating_expenses = "High heating expenses" ///
				1.polluting_sector = "Works in polluting sector" ///
				1.binary_availability_transport = "Public transport available" ///
				1.car_dependency = "Uses car" ///
				1.owner = "Owner or landlord" ///
				1.flights_agg_1 = "Flies more than once a year" , labsize(vsmall)) 	///
	headings(		 1.urban = "{bf: Place Charac.}" ///
						 1.binary_gas_expenses = "{bf: Energy Usage}" ///
						 1.owner = "{bf: Personal Charac.}", labsize(small)) ///
			///title("`title_`var''", size(small) color(black)) ///
			legend(order(2) label(2 "``var't'") pos(12) size(small)) ///
			xtitle("Coefficients", col(gs8) size(small)) ///
			note("{it: Note}: Other covariates included: female, gender, children <14," "majority origin, income, employment status, education, voting.", size(vsmall) col(gs8)) /// 
			 name(gB, replace) 
			 graph export "${output}\\Coefplot_SetAB_PlotB_`var'.png", replace 
			 
	gr combine gA gB , xcommon title("`title_`var''", size(small) color(black)) 
	graph export "${output}\\Coefplot_SetAB_`var'.png", replace
	}
	
	
		 

/*

*--------------------------------------
* Exploration: Income / Wealth Only 
*--------------------------------------
global graphsettings   "graphregion(fcolor(white) lcolor(white) margin( 1 1 1 1 ) ) plotregion( fcolor(none) lcolor(none) margin(1 1 1 1) ) ysize(10) xsize(12)"


	global setAboth ""
	foreach var in "female" "age25_34 age35_49 age50_64 age65plus"    "children"   "i.dominant_origin"       "i.income_factor"  "i.wealth"     "i.employment_agg"  "i.college"      "vote_agg_right"  "vote_agg0"  "vote_agg_pnr"     {
		global setA $setA `var'
	}

	global setAinc ""
	foreach var in "female" "age25_34 age35_49 age50_64 age65plus"    "children"   "i.dominant_origin"       "i.income_factor"   "i.employment_agg"  "i.college"      "vote_agg_right"  "vote_agg0"  "vote_agg_pnr"     {
		global setAinc $setAinc `var'
	}
	
	global setAw ""
	foreach var in "female" "age25_34 age35_49 age50_64 age65plus"    "children"   "i.dominant_origin"       "i.wealth"   "i.employment_agg"  "i.college"      "vote_agg_right"  "vote_agg0"  "vote_agg_pnr"     {
		global setAw $setAw `var'
	}


loc index_knowledget "Index knowledge"
loc title_index_knowledge "Knowledge about Climate Change"

loc index_main_policiest "Index main policies"
loc title_index_main_policies "Support policies"

	foreach var in index_knowledge  index_main_policies   {

	if strpos("`var'", "knowledge") loc i = 1 
	if strpos("`var'", "policies") loc i = 7 
	
	
	reg `var'  $setAboth  [w=weight], robust 
	estimates store `var'A

	coefplot ///
	 (`var'A,  drop(_cons )  keep( female age* *children *income_factor *wealth *employment_agg 1.dominant_origin  2.college vote*)  xline(0, lcolor(gs8) lpattern(dash)  lwidth(thin)) msize(1)  mlcolor("${p`i'}") mfcolor("${p`i'}") ciopts( lwidth(vthin)  color("${p`i'}") )  )	 , ///
	 $graphsettings xlabel( $labeloptions format(%4.1fc)) ylabel( $labeloptions nogrid ) ///
	 coeflabels(female = "Female" ///
				age25_34 = "25-34 years old" age35_49 = "35-49 years old" age50_64 = "50-64 years old" age65plus = "65+ years old" ///
				children = "Lives with child(ren)<14" /// 
				2.income_factor = "Q2" 3.income_factor = "Q3" 4.income_factor = "Q4" ///
				2.wealth = "2nd quintile" 3.wealth = "3rd quintile" 4.wealth = "4th quintile" 5.wealth = "5th quintile" ///
				2.college = "Does not have college degree" ///
				1.dominant_origin = "Country's majority origin" ///
				vote_agg_pnr = "Vote Others or PNR" vote_agg_right = "Vote Right or Far Right" vote_agg0 = "Vote Center"  , labsize(vsmall)) 	///
	headings(female= "{bf: Demographics}" /// 
			 2.income_factor= "{bf: Income}" ///
			 2.wealth = "{bf: Wealth}" /// 
			 2.employment_agg = "{bf: Employment Status}" ///
			 2.college  = "{bf: Education}" /// 
			 vote_agg_right = "{bf: Political Leaning}" /// 
			 , labsize(small)) ///
			/// title("`title_`var''", size(small) color(black)) ///
			legend(order(2) label(2 "``var't'") pos(12) size(small)) ///
			xtitle("Coefficients", col(gs8) size(small)) ///
			 name(gA, replace) 
			 *graph export "${output}\\Coefplot_SetAinc_PlotA_`var'.png", replace 		
	
	
	reg `var'  $setAinc  [w=weight], robust 
	estimates store `var'Ainc

	coefplot ///
	 (`var'Ainc,  drop(_cons )  keep( female age* *children *income_factor *wealth *employment_agg 1.dominant_origin  2.college vote*)  xline(0, lcolor(gs8) lpattern(dash)  lwidth(thin)) msize(1)  mlcolor("${p`i'}") mfcolor("${p`i'}") ciopts( lwidth(vthin)  color("${p`i'}") )  )	 , ///
	 $graphsettings xlabel( $labeloptions format(%4.1fc)) ylabel( $labeloptions nogrid ) ///
	 coeflabels(female = "Female" ///
				age25_34 = "25-34 years old" age35_49 = "35-49 years old" age50_64 = "50-64 years old" age65plus = "65+ years old" ///
				children = "Lives with child(ren)<14" /// 
				2.income_factor = "Q2" 3.income_factor = "Q3" 4.income_factor = "Q4" ///
				2.wealth = "2nd quintile" 3.wealth = "3rd quintile" 4.wealth = "4th quintile" 5.wealth = "5th quintile" ///
				2.college = "Does not have college degree" ///
				1.dominant_origin = "Country's majority origin" ///
				vote_agg_pnr = "Vote Others or PNR" vote_agg_right = "Vote Right or Far Right" vote_agg0 = "Vote Center"  , labsize(vsmall)) 	///
	headings(female= "{bf: Demographics}" /// 
			 2.income_factor= "{bf: Income only}" ///
			 2.wealth = "{bf: Wealth}" /// 
			 2.employment_agg = "{bf: Employment Status}" ///
			 2.college  = "{bf: Education}" /// 
			 vote_agg_right = "{bf: Political Leaning}" /// 
			 , labsize(small)) ///
			/// title("`title_`var''", size(small) color(black)) ///
			legend(order(2) label(2 "``var't'") pos(12) size(small)) ///
			xtitle("Coefficients", col(gs8) size(small)) ///
			 name(gAinc, replace) 
			 *graph export "${output}\\Coefplot_SetAinc_PlotA_`var'.png", replace 	
			 
	reg `var'  $setAw  [w=weight], robust 
	estimates store `var'Aw			 
	coefplot ///
	 (`var'Aw,  drop(_cons )  keep( female age* *children *income_factor *wealth *employment_agg 1.dominant_origin  2.college vote*) xline(0, lcolor(gs8) lpattern(dash)  lwidth(thin)) msize(1)  mlcolor("${p`i'}") mfcolor("${p`i'}") ciopts( lwidth(vthin)  color("${p`i'}") )  )	 , ///
	 $graphsettings xlabel( $labeloptions format(%4.1fc)) ylabel( $labeloptions nogrid ) ///
	 coeflabels(female = "Female" ///
				age25_34 = "25-34 years old" age35_49 = "35-49 years old" age50_64 = "50-64 years old" age65plus = "65+ years old" ///
				children = "Lives with child(ren)<14" /// 
				2.income_factor = "Q2" 3.income_factor = "Q3" 4.income_factor = "Q4" ///
				2.wealth = "2nd quintile" 3.wealth = "3rd quintile" 4.wealth = "4th quintile" 5.wealth = "5th quintile" ///
				2.college = "Does not have college degree" ///
				1.dominant_origin = "Country's majority origin" ///
				vote_agg_pnr = "Vote Others or PNR" vote_agg_right = "Vote Right or Far Right" vote_agg0 = "Vote Center"  , labsize(vsmall)) 	///
	headings(female= "{bf: Demographics}" /// 
			 2.income_factor= "{bf: Income}" ///
			 2.wealth = "{bf: Wealth only}" /// 
			 2.employment_agg = "{bf: Employment Status}" ///
			 2.college  = "{bf: Education}" /// 
			 vote_agg_right = "{bf: Political Leaning}" /// 
			 , labsize(small)) ///
			/// title("`title_`var''", size(small) color(black)) ///
			legend(order(2) label(2 "``var't'") pos(12) size(small)) ///
			xtitle("Coefficients", col(gs8) size(small)) ///
			 name(gAw, replace) 
			 *graph export "${output}\\Coefplot_SetAw_PlotA_`var'.png", replace 	
			 
	gr combine gA gAinc gAw , xcommon title("`title_`var''", size(small) color(black)) col(3) 
	/// note("{it: Note}: Other covariates included in all specifications: female, gender, children <14, majority origin, employment status, education, voting.", size(vsmall) col(gs8)) /// 
	graph export "${output}\\Coefplot_SetAincw_`var'_all.png", replace
	
	}







	
	
	