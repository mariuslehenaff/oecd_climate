*********************************************
************** Cleaning Dataset *************
*********************************************
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


use "$data_trade/Analytic/data_clean_$date.dta", clear

set more off

***********************
*** Screening Level ***
*********************** 

drop if flag_time == 1
drop if screen != 1

***************************
** Demographic Variables **
***************************

***Gender
*Gender dummies
gen women=.
replace women=0 if complete==1
replace women=1 if gender==1


***Age
*Age group (coarser partition aggregating groups)
gen 			agegroup_agg=.
replace 		agegroup_agg=1 if agegroup==1 
replace 		agegroup_agg=2 if agegroup==2 | agegroup==3
replace 		agegroup_agg=3 if agegroup==4 | agegroup==5
label define 	agegroup_agg 1 "18-29" 2 "30-49" 3 "50-69"
label val 		agegroup_agg agegroup_agg

*Age dummies (fine partition)
gen 	age_1829=.
replace age_1829=0 if complete == 1
replace age_1829=1 if agegroup == 1
  
gen 	age_3039=.  
replace age_3039=0 if complete == 1
replace age_3039=1 if agegroup == 2
  
gen 	age_4049=.  
replace age_4049=0 if complete == 1
replace age_4049=1 if agegroup == 3
  
gen 	age_5059=.  
replace age_5059=0 if complete == 1
replace age_5059=1 if agegroup == 4
  
gen 	age_6069=.  
replace age_6069=0 if complete == 1
replace age_6069=1 if agegroup == 5

*Age dummies (young/old)
gen 	young =.
replace young = 0 if complete == 1
replace young = 1 if agegroup == 1 

gen old=.
replace old=0 if complete==1
replace old=1 if agegroup==4 | agegroup==5


***Income 
*Income group (coarser partition aggregating groups)
gen 			incomegroup_agg =.
replace 		incomegroup_agg = 1 if incomegroup == 1 | incomegroup == 2
replace 		incomegroup_agg = 2 if incomegroup == 3
replace 		incomegroup_agg = 3 if incomegroup == 4 | incomegroup == 5
label define 	incomegroup_agg 1 "0k-39k" 2 "40k-69k" 3 "70k+"
label val 		incomegroup_agg incomegroup_agg

gen 	rich = 0
replace rich = 1 if incomegroup_agg >= 3





***Race
*Race dummies 
gen 	race_white=.
replace race_white=0 if complete==1
replace race_white=1 if race==1

gen 	race_black=.
replace race_black=0 if complete==1
replace race_black=1 if race==2

gen 	race_hisp=.
replace race_hisp=0 if complete==1
replace race_hisp=1 if race==3

gen 	race_asian=.
replace race_asian=0 if complete==1
replace race_asian=1 if race==4

gen 	race_mixed=.
replace race_mixed=0 if complete==1
replace race_mixed=1 if race==5

gen 	race_other=.
replace race_other=0 if complete==1
replace race_other=1 if race==6

gen 			race_group = "White" 	if complete==1
replace 		race_group = "Black" 	if(race==2 & complete==1)
replace 		race_group = "Hispanic" if(race==3 & complete==1)
replace 		race_group = "Other" 	if(race>3 & complete==1)
label define 	race_agg 0 "White" 1 "Black" 2 "Hispanic" 3 "Other"
encode 			race_group, gen(race_agg) label(race_agg)



***Education
**Education group (coarser partition aggregating groups)
gen 	education_agg=.
replace education_agg=1 if education_level==1 | education_level==2 
replace education_agg=2 if education_level==3 | education_level==4 | education_level==5
replace education_agg=3 if education_level==6 | education_level==7 
replace education_agg=4 if education_level==8 | education_level==9 
label define education_agg 1 "Less than HS" 2 "Less than 4-year college" 3 "4-year college/Master's" 4 "Professional degree"
label val education_agg education_agg

*Education dummies

gen 	educ_low=0 if complete==1
replace educ_low=1 if education_level==1 | education_level==2 

gen 	educ_mid=0 if complete==1
replace educ_mid=1 if education_level==3 | education_level==4 | education_level==5

gen		educ_high=0 if complete==1
replace educ_high=1 if education_level==6 | education_level==7 

gen 	educ_veryhigh = 0 if complete==1
replace educ_veryhigh = 1 if education_level==8 | education_level==9

	* college defined as having a 4-year college degree. 
gen 			college=.
replace 		college=0 if complete==1
replace 		college=1 if education_level>5 
label define 	college_degree 0 "No College" 1 "4-year College Degree"
label val 		college college_degree
label var 		college "College Degree"

gen no_college = 1 if college==0
	replace no_college = 0 if college==1
label var no_college "No College Degree"


gen study_area=.
replace study_area=1 if field_study==4 | field_study==5 | field_study==9 | field_study==10 | field_study==12 | field_study==16 | field_study==20 | field_study==25 | field_study==31 | field_study==32 | field_study==34 | field_study==35 | field_study==42 | field_study==43 | field_study==50 | field_study==51 | field_study==52 | field_study==58 | field_study==61 | field_study==69 | field_study==74 | field_study==75
replace study_area=2 if field_study==2 | field_study==3 | field_study==13 | field_study==14 | field_study==15 | field_study==17 | field_study==18 | field_study==22 | field_study==23 | field_study==29 | field_study==33 | field_study==36 | field_study==38 | field_study==39 | field_study==41 | field_study==44 | field_study==45 | field_study==46 | field_study==48 | field_study==49 | field_study==54 | field_study==55 | field_study==56 | field_study==57 | field_study==60 | field_study==62 | field_study==63 | field_study==64 | field_study==65 | field_study==66 | field_study==67 | field_study==68 | field_study==71
replace study_area=3 if field_study==6 | field_study==7 | field_study==8 | field_study==6 | field_study==11 | field_study==13 | field_study==15 | field_study==19 | field_study==24 | field_study==26 | field_study==28 | field_study==30 | field_study==37 | field_study==40 | field_study==47 | field_study==53 | field_study==59 | field_study==70 | field_study==72 | field_study==73 | field_study==76
replace study_area=4 if field_study==1 | field_study==11 | field_study==21 | field_study==27
label define study_area 1 "Sciences" 2 "Professional" 3 "Arts" 4 "Econ"
label val study_area study_area

gen econ=0
replace econ=1 if study_area==4 & college ==1 
label var econ "Economics related major"

	* Interacting variable college X area of study

gen college_degree_area = college * study_area 
	replace college_degree_area = 0 if(missing(college_degree_area) & complete==1)
label define college_degree_area 0 "No 4-Year College Degree" 1 "Sciences" 2 "Professional" 3 "Arts" 4 "Econ"
label val college_degree_area college_degree_area 


*Study area dummies
gen arts=.
replace arts=0 if complete==1
replace arts=1 if study_area==1

gen sciences=.
replace sciences=0 if complete==1
replace sciences=1 if study_area==2

gen soc_sciences=.
replace soc_sciences=0 if complete==1
replace soc_sciences=1 if study_area==3

***Economic policy affiliation
*Pol affiliation (coarser partition aggregating groups)
gen econpol_affiliation=.
replace econpol_affiliation=1 if econ_ideology==1 | econ_ideology==2
replace econpol_affiliation=2 if econ_ideology==3
replace econpol_affiliation=3 if econ_ideology==4 | econ_ideology==5
label define econpol_affiliation 1 "Liberal" 2 "Moderate" 3 "Conservative"
label val econpol_affiliation econpol_affiliation

*Liberal/conservative dummies
gen liberal=.
replace liberal=0 if complete==1
replace liberal=1 if econ_ideology==1 | econ_ideology==2

gen moderate=.
replace moderate=0 if complete==1
replace moderate=1 if econ_ideology==3

gen conservative=.
replace conservative=0 if complete==1
replace conservative=1 if econ_ideology==4 | econ_ideology==5


***Political affiliation
*Republican-Democrat dummies
gen republican=.
replace republican=0 if complete==1
replace republican=1 if political_party==1

gen democrat=.
replace democrat=0 if complete==1
replace democrat=1 if political_party==2

gen political_agg = 1 if(complete==1 & political_party==2)
	replace political_agg = 2 if(complete==1 & political_party==1)
	replace political_agg = 3 if(complete==1 & political_party>2)
label define political_agg 1 "Democrat" 2 "Republican" 3 "Independent and others"
label val political_agg political_agg

gen candidate2016_agg = 1 if(complete==1 & candidate_2016==3)
	replace candidate2016_agg = 2 if(complete ==1 & candidate_2016==1)
	replace candidate2016_agg = 3 if(complete ==1 & candidate_2016==2 | complete ==1 & candidate_2016>3)
label define candidate2016_agg 1 "Clinton" 2 "Trump" 3 "Other" 
label val candidate2016_agg candidate2016_agg

gen trump=0
replace trump=1 if candidate_2016==1

gen candidate_econ = 1 if(candidate2016_agg==1 & econpol_affiliation==1)
	replace candidate_econ = 2 if(candidate2016_agg==1 & econpol_affiliation >1)
	replace candidate_econ = 3 if(candidate2016_agg==2 & econpol_affiliation <3)
	replace candidate_econ = 4 if(candidate2016_agg==2 & econpol_affiliation ==3)
	replace candidate_econ = 5 if(candidate2016_agg==3)
label define candidate_econ 1 "Clinton Liberals" 2 "Clinton Moderates" 3 "Trump Moderates" 4 "Trump Conservatives" 5 "Others"
label val candidate_econ candidate_econ


*** Media variables and dummies

* Amount of exposure to media

label define media_exposure 1 "A fair amount" 2 "A great deal" 3 "Not at all" 4 "Only a little"
encode G01043 , gen(media_exposure) label(media_exposure)

* dummies

gen internet=0
replace internet=1 if news_media==1

gen tvradio=0
replace tvradio=1 if news_media==2 | news_media==3

gen newspaper=0
replace newspaper=1 if news_media==4 
 
gen nonews=0
replace nonews=1 if news_media==8

* social media dummy created in do.file 1

gen media_agg=0
replace media_agg=1 if nonews==1
replace media_agg=2 if newspaper==1
replace media_agg=3 if tvradio==1
replace media_agg=4 if social_media==1 | G01038=="Mostly through social media"
replace media_agg=5 if internet==1 & G01035=="News websites and online newspapers" & G01038=="Mostly through the newspaper's website"
replace media_agg=6 if news_media==5 | news_media==6 | news_media==7
label define media_agg 1 "No news" 2 "Newspaper (Online and print)" 3 "TV and radio" 4 "Social media" 5 "Internet (not social media)" 6 "Others" 0 "Residual"
label val media_agg media_agg


*** Employment status

gen employment_agg = 1 if complete==1
	replace employment_agg = 2 if(complete==1 & employment_status<4)
	replace employment_agg = 3 if(complete==1 & employment_status==4 |complete==1 & employment_status==6)
	replace employment_agg = 4 if(complete==1 & employment_status==7)
label define employment_agg 1 "Student" 2 "Working" 3 "Not working" 4 "Retiree"
label val employment_agg employment_agg


gen employment_agg3 = 1 if complete==1
	replace employment_agg3 = 2 if(complete==1 & employment_status<4 & low_skilled==1)
	replace employment_agg3 = 3 if(complete==1 & employment_status<4 & low_skilled==0 )
	replace employment_agg3 = 4 if(complete==1 & employment_status==4 |complete==1 & employment_status==6)
	replace employment_agg3 = 5 if(complete==1 & employment_status==7)
label define employment_agg3 1 "Student" 2 "Working in low skilled" 3 "Working in high skilled" 4 "Not working" 5 "Retiree"
label val employment_agg3 employment_agg3

gen employment_agg2 = 1 if complete==1
	replace employment_agg2 = 2 if(complete==1 & employment_status==5)
	replace employment_agg2 = 3 if(complete==1 & employment_status<4)
	replace employment_agg2 = 4 if(complete==1 & employment_status==4)
	replace employment_agg2 = 5 if(complete==1 & employment_status==7)
label define employment_agg2 1 "Not working/seeking" 2 "Student" 3 "Working" 4 "Unemployed" 5 "Retiree"
label val employment_agg2 employment_agg2


*** Knowledge about economic policy

gen knowledge_econ_agg=.
replace knowledge_econ_agg=1 if knowledge_econ==2 | knowledge_econ==1
replace knowledge_econ_agg=2 if knowledge_econ==3 | knowledge_econ==4
label define knowledge_econ_agg 1 "Little knowledgeable" 2 "Knowledgeable"
label val knowledge_econ_agg knowledge_econ_agg

*** Social class

gen socialclass_agg = 1 if complete==1
replace socialclass_agg = 2 if(complete==1 & social_class == 3)
replace socialclass_agg = 3 if(complete==1 & social_class>3)
label define socialclass 1 "Lower or Working Class" 2 "Middle Class" 3 "Upper-middle and Upper Class"
label values socialclass_agg socialclass

gen upperclass_dummmy=(social_class==4 | social_class==5) if !mi(social_class)
label var upperclass_dummmy "Upper Class (self-reported)"


*** Children dummies
gen kids=.
replace kids=0 if complete==1
replace kids=1 if nb_child>0
label define kids 0 "No kids" 1 "Has kids"
label val kids kids


*patriotism index
foreach var in Q01037_en Q01038_en Q01039_en {
	sum `var' 
	local `var'mean = r(mean)
	local `var'sd = r(sd)	
gen `var'_ind=(`var'-``var'mean')/``var'sd' 
}
.
gen index_patriotism = (Q01037_en_ind + Q01038_en_ind + Q01039_en_ind )/3


****************************************
*        DUMMIES FOR GRAPHS            *
****************************************

*Women vs Men
gen women_gr=.
replace women_gr=1 if (gender==1 & complete==1)
replace women_gr=0 if (gender==2 & complete==1)

*Young(18-29) vs Old(50-69)
gen young_gr=.
replace young_gr=1 if (agegroup_agg==1 & complete==1)
replace young_gr=0 if (agegroup_agg==3 & complete==1)

*Poor(0-39k) vs Rich (+70k)
gen poor_gr=.
replace poor_gr=1 if (incomegroup_agg==1 & complete==1)
replace poor_gr=0 if (incomegroup_agg==3 & complete==1)

*Republican vs Democrat
gen republican_gr=.
replace republican_gr=1 if (political_party==1 & complete==1)
replace republican_gr=0 if (political_party==2 & complete==1)

*Lower and working social class vs Upper-middle and Upper Class
gen low_social_class=.
replace low_social_class=1 if (socialclass_agg==1 & complete==1)
replace low_social_class=0 if (socialclass_agg==3 & complete==1)

*Knowledgeable vs not knowledgeable
gen knowledgeable=.
replace knowledgeable=1 if (knowledge_econ_agg==2 & complete==1)
replace knowledgeable=0 if (knowledge_econ_agg==1 & complete==1)


*Economic degree vs No 4-year college
gen econ_gr=.
replace econ_gr=1 if (college_degree_area==4 & complete==1)
replace econ_gr=0 if (college_degree_area==0 & complete==1)

*White vs Black
gen white=.
replace white=1 if (race==1 & complete==1)
replace white=0 if (race==2 & complete==1)

*Trump conservatives vs Trump moderates
gen trump_gr=.
replace trump_gr=1 if (candidate_econ==4 & complete==1)
replace trump_gr=0 if (candidate_econ==3 & complete==1)

*Clinton moderates vs Clinton liberal
gen clinton_gr=.
replace clinton_gr=1 if (candidate_econ==2 & complete==1)
replace clinton_gr=0 if (candidate_econ==1 & complete==1)

gen media_gr=.
replace media_gr=1 if (media_agg==2 & complete==1 ) | (media_agg==5 & complete==1 )
replace media_gr=0 if (media_agg==4 & complete==1 )

***********************************************
*** Randomization and interacting variables ***
***********************************************

gen		rando_you 				= 0
replace	rando_you 				= 1 if mechanisms_rando == 2

gen		rando_gender 			= 0
replace rando_gender 			= 1 if mechanisms_rando == 2

gen		video_distributional 	= 0 
replace	video_distributional 	= 1 if treatment 		== 2
		
gen 	video_efficiency		= 0		
replace	video_efficiency		= 1 if treatment 		== 3
			
gen		video_econ_gen			= 0		
replace	video_econ_gen			= 1 if treatment 		== 4
		
gen		video_econ_us			= 0		
replace	video_econ_us			= 1 if treatment 		== 5

*********************
*** Actual Values ***
*********************

gen actual_Q04006 = 4
gen actual_Q04007 = 50
gen actual_Q04009 = "Canada"
gen actual_Q04010 = "China"

**********************
*** MISPERCEPTIONS ***
**********************

foreach i in Q04006 Q04007 {
	gen dev_`i' = `i'_en - actual_`i'
	gen absdev_`i'=abs(dev_`i')
	egen p50_`i'=pctile(absdev_`i'), p(50)
	gen acc_dummy_`i'=(absdev_`i'<=p50_`i') if !missing(absdev_`i')

}
.

gen acc_dummy_Q04009=(Q04009=="Canada") if !mi(Q04009)
gen acc_dummy_Q04010=(Q04010=="China") if !mi(Q04010)
gen acc_dummy_Q04011=(Q04011_en==3) if !mi(Q04011_en)

egen acc_dummy_sum = rowtotal(acc_dummy*)
gen acc_dummy=acc_dummy_sum/4

************************
*** QUESTION DUMMIES ***
************************

* Block 3 *

gen 	dummy_Q03001 = Q03001_en - 1 if !mi(Q03001_en)

* Block 6 *
	
	* Distributional questions 
	
gen 	Q06001_mean = (Q06001_1_en + Q06001_2_en + Q06001_3_en + Q06001_4_en + Q06001_5_en)/5
gen 	dummy_Q06001 = 1 if Q06001_mean < 3
replace dummy_Q06001 = 0 if(Q06001_mean >= 3 & Q06001_mean <= 5) 

gen 	dummy_Q06001_1 = 0 if !missing(Q06001_1_en)	
gen 	dummy_Q06001_2 = 0 if !missing(Q06001_2_en)	
gen 	dummy_Q06001_3 = 0 if !missing(Q06001_3_en)
gen 	dummy_Q06001_4 = 0 if !missing(Q06001_4_en)
gen 	dummy_Q06001_5 = 0 if !missing(Q06001_5_en)
replace dummy_Q06001_1 = 1 if(Q06001_1_en > 2 & Q06001_1_en < 6)
replace dummy_Q06001_2 = 1 if(Q06001_2_en > 2 & Q06001_2_en < 6)
replace dummy_Q06001_3 = 1 if(Q06001_3_en > 2 & Q06001_3_en < 6)
replace dummy_Q06001_4 = 1 if(Q06001_4_en > 2 & Q06001_4_en < 6)
replace dummy_Q06001_5 = 1 if(Q06001_5_en > 2 & Q06001_5_en < 6)


gen 	  dummy_Q06002 = (Q06002_en == 1) if !mi(Q06002_en)
label var dummy_Q06002 "More international trade can make everyone better off by compensating those who lose with transfers"

gen 	  dummy_Q06003 = (Q06003_en == 2) if !mi(Q06003_en)
label var dummy_Q06003 "Most jobs affected by US trade policy"

gen 	  dummy_Q06004 = (Q06004_en == 1) if !mi(Q06003_en)
label var dummy_Q06004 "Overall international trade has helped US workers"

gen 	  dummy_Q06005 = (Q06005_en == 1) if !mi(Q06005_en) 
label var dummy_Q06005 "Easy to find job in different sector for low skilled"

gen 	dummy_Q06006 = (Q06006_en == 1) if !mi(Q06006_en) 
label var dummy_Q06006 "Easy to find job in different sector for high skilled"

gen       dummy_Q06007_1 = (Q06007_1_en == 4 | Q06007_1_en == 5) if !mi(Q06007_1)
label var dummy_Q06007_1 "Trade major reason for unemployment in some sectors"

gen       dummy_Q06007_2 = (Q06007_2_en == 4 | Q06007_2_en == 5) if !mi(Q06007_2)
label var dummy_Q06007_2 "Trade major reason for rise in inequality" 


	* Efficiency

gen 	dummy_Q06008 = Q06008_en - 1
gen 	dummy_Q06009 = Q06009_en - 1
gen 	dummy_Q06010 = Q06010_en - 1
gen 	dummy_Q06011 = Q06011_en - 1

gen 	dummy_Q06012 = 0 if !missing(Q06012_en)
replace dummy_Q06012 = 1 if(Q06012_en == 2 | Q06012_en == 3)

	* Case Study

gen 	dummy_Q06013 = (Q06013_en == 3) if !mi(Q06013_en)
label var dummy_Q06013 "Both countries better off if they trade"

gen 	dummy_Q06014 = (Q06013_en == 2) if !mi(Q06014_en)
label var dummy_Q06014 "It makes sense to import from Germany under some circumstances"
	
gen 	dummy_Q06015 = (Q06015_en == 1) if !mi(Q06015_en)
gen 	dummy_Q06016 = (Q06016_en == 2) if !mi(Q06016_en)
gen 	dummy_Q06017 = (Q06017_en == 1) if !mi(Q06017_en)
gen 	dummy_Q06018 = (Q06018_en == 1) if !mi(Q06018_en)
gen 	dummy_Q06019 = (Q06019_en == 3) if !mi(Q06019_en)
gen 	dummy_Q06020 = (Q06020_en == 3) if !mi(Q06020_en)
gen 	dummy_Q06021 = (Q06021_en == 3) if !mi(Q06021_en)
gen 	dummy_Q06022 = (Q06022_en == 3) if !mi(Q06022_en)
gen 	dummy_Q06023 = (Q06023_en == 3) if !mi(Q06023_en)
gen 	dummy_Q06024 = (Q06024_en == 1) if !mi(Q06024_en)


* Block 7 *

foreach y in 4 5 {
gen diff_Q0700`y' = Q0700`y'
foreach x in "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"{
replace diff_Q0700`y' = subinstr(diff_Q0700`y', ",`x'", ",", .)
}

gen Q0700`y'_nb = 0
	replace Q0700`y'_nb = 1 if !missing(Q0700`y') 
	replace Q0700`y'_nb = Q0700`y'_nb + length(Q0700`y') - length(diff_Q0700`y')
}
.

gen dummy_Q07001 = (Q07001_en == 3 |Q07001_en == 4 |Q07001_en == 5 ) if !missing(Q07001_en)
	

gen dummy_Q07002 = (Q07002_en == 3 | Q07002_en == 4) if !missing(Q07002_en)
	

gen dummy_Q07003 = (Q07003_en == 3 | Q07003_en == 4) if !missing(Q07003_en)

gen 	dummy_Q07004 = 0 if !missing(Q07004_nb)
replace dummy_Q07004 = 1 if Q07004_nb > 2
gen 	dummy_Q07005 = 0 if !missing(Q07005_nb)
replace dummy_Q07005 = 1 if Q07005_nb > 3

gen     dummy_Q07006_1=(Q07006_1_en==1) if !mi(Q07006_1_en)
gen     dummy_Q07006_2=(Q07006_2_en==1) if !mi(Q07006_2_en)
gen     dummy_Q07006_3=(Q07006_3_en==1) if !mi(Q07006_3_en)

gen 	dummy_Q07007 = Q07007_en - 1
gen 	dummy_Q07008 = (Q07008_en == 1 | Q07008_en == 2) if !mi(Q07008_en)
gen 	dummy_Q07009 = Q07009_en - 1	


* Block 9

gen		dummy_Q09003 = Q09003_en - 1


* Block 08

gen 	dummy_G08002   = 1 if(G08002_en   >  2 & G08002_en < 6)
replace dummy_G08002   = 0 if(G08002_en   <  4)
gen 	dummy_G08003   = 1 if(G08003_en   >  2 & G08003_en < 6)
replace dummy_G08003   = 0 if(G08003_en   <  3)
gen		dummy_G08004_1 = 1 if(G08004_1_en == 3)
replace dummy_G08004_1 = 0 if(G08004_1_en <  3)
gen 	dummy_G08004_2 = 1 if(G08004_2_en == 3)
replace dummy_G08004_2 = 0 if(G08004_2_en <  3)
gen 	dummy_G08004_3 = 1 if(G08004_3_en == 3)
replace dummy_G08004_3 = 0 if(G08004_3_en <  3)	
gen 	dummy_G08004_4 = 1 if(G08004_4_en == 3)
replace dummy_G08004_4 = 0 if(G08004_4_en <  3)
gen 	dummy_G08004_5 = 1 if(G08004_5_en == 3)
replace dummy_G08004_5 = 0 if(G08004_5_en <  3)


label var dummy_G08002   "High-income HH"
label var dummy_G08003   "Middle-class HH"
label var dummy_G08004_1 "Income support"
label var dummy_G08004_2 "Better schools"
label var dummy_G08004_3 "Retraining"
label var dummy_G08004_4 "Subsidies for low-income"
label var dummy_G08004_5 "Wage subsidies"

* Block 10

gen 	dummy_G10001 = 1 if(G10001_en >  2 & G10001_en <  5)
replace dummy_G10001 = 0 if(G10001_en == 1 | G10001_en == 2)
gen 	dummy_G10002 = 1 if(G10002_en == 3)
replace dummy_G10002 = 0 if(G10002_en <  3)
gen 	dummy_G10003 = 1 if(G10003_en >  3 & G10003_en <  6)
replace dummy_G10003 = 0 if(G10003_en <  4)
gen 	dummy_G10005 = 1 if(G10005_en >  2 & G10005_en <  5)
replace dummy_G10005 = 0 if(G10005_en == 1 | G10005_en == 2)
	
label var dummy_G10001 	"Trust"
label var dummy_G10002 	"Purposes"
label var dummy_G10003 	"Involvment"
label var G10004_en 	"Cents wasted"
label var dummy_G10005 	"Satisfaction"	

forvalue y = 1/8 {
gen dummy_G10006_`y' = (G10006_`y'_en == 4 | G10006_`y'_en == 5) if !mi(G10006_`y'_en)
}
.


label var dummy_G10006_1 "Reduce income diff."
label var dummy_G10006_2 "Reduce wealth transmission"
label var dummy_G10006_3 "Ensuring health care"
label var dummy_G10006_4 "Reducing opportunity diff."
label var dummy_G10006_5 "Regulating trade"
label var dummy_G10006_6 "Stabilizing financial syst."
label var dummy_G10006_7 "Stabilizing dollar"
label var dummy_G10006_8 "Providing min. living"


*moved to the general question dataset

* WTP 
/*foreach i in wtp1 wtp2 wtp5 wtp10  {
gen `i'_dummy=0
replace `i'_dummy=1 if `i'!=.
}	
.*/

gen 	dummy_Q12002 = Q12002_en - 1 if !mi(Q12002_en)
label var dummy_Q12002 "Surprised by the numbers"


foreach i in  1 3 {
gen Q06001_`i'_invert = .
replace Q06001_`i'_invert = 1 if Q06001_`i'_en == 5
replace Q06001_`i'_invert = 2 if Q06001_`i'_en == 4
replace Q06001_`i'_invert = 3 if Q06001_`i'_en == 3
replace Q06001_`i'_invert = 4 if Q06001_`i'_en == 2
replace Q06001_`i'_invert = 5 if Q06001_`i'_en == 1
}
.

gen Q06002_invert = .
replace Q06002_invert = 1 if Q06002_en == 2 
replace Q06002_invert = 2 if Q06002_en == 1 

gen Q06004_invert = .
replace Q06004_invert = 1 if Q06004_en == 2 
replace Q06004_invert = 2 if Q06004_en == 1 

foreach i in  1 2 {
gen Q06007_`i'_invert = .
replace Q06007_`i'_invert = 1 if Q06007_`i'_en == 5
replace Q06007_`i'_invert = 2 if Q06007_`i'_en == 4
replace Q06007_`i'_invert = 3 if Q06007_`i'_en == 3
replace Q06007_`i'_invert = 4 if Q06007_`i'_en == 2
replace Q06007_`i'_invert = 5 if Q06007_`i'_en == 1
}
.


foreach i in  15 17 18 24 {
gen Q060`i'_invert = .
replace Q060`i'_invert = 1 if Q060`i'_en == 5
replace Q060`i'_invert = 2 if Q060`i'_en == 4
replace Q060`i'_invert = 3 if Q060`i'_en == 3
replace Q060`i'_invert = 4 if Q060`i'_en == 2
replace Q060`i'_invert = 5 if Q060`i'_en == 1
}
.

gen Q07008_invert = .
replace Q07008_invert = 4 if Q07008_en == 1
replace Q07008_invert = 3 if Q07008_en == 2
replace Q07008_invert = 2 if Q07008_en == 3
replace Q07008_invert = 1 if Q07008_en == 4


global X "women kids i.race_agg i.agegroup_agg i.incomegroup_agg college econ i.employment_agg i.knowledge_econ_agg upperclass_dummmy low_skilled"


foreach var in   Q06001_1_invert Q06001_2_en Q06001_3_invert Q06001_4_en Q06001_5_en Q06002_invert Q06004_invert Q06005_en Q06007_1_invert Q06007_2_invert ///
Q06008_en Q06009_en Q06010_en Q06011_en Q06013_en Q07008_invert /// 
Q06014_en Q06015_invert Q06016_en Q06017_invert Q06018_invert Q06019_en Q06020_en Q06021_en Q06022_en Q06023_en Q06024_invert ///
G10001_en G10002_en G10003_en   {
			 
xi: reg `var' $X
su `var' if e(sample)==1 & control==1
local `var'mc: display %5.3f `r(mean)'
su `var' if e(sample)==1 & control==1
local `var'sdc: display %5.3f `r(sd)'
su `var' if e(sample)==1 & treatment==2
local `var'mt1: display %5.3f `r(mean)'
su `var' if e(sample)==1 & treatment==3
local `var'mt2: display %5.3f `r(mean)'
su `var' if e(sample)==1 & treatment==4
local `var'mt4: display %5.3f `r(mean)'
su `var' if e(sample)==1 & treatment==5
local `var'mt3: display %5.3f `r(mean)'
su `var' if e(sample)==1 & mechanisms_rando==2
local `var'mtyou: display %5.3f `r(mean)'


gen `var'_ind=(`var'-``var'mc')/``var'sdc'
replace `var'_ind=(``var'mc'-``var'mc')/``var'sdc' if `var'==. & control==1
replace `var'_ind=(``var'mt1'-``var'mc')/``var'sdc' if `var'==. & treatment==2
replace `var'_ind=(``var'mt2'-``var'mc')/``var'sdc' if `var'==. & treatment==3
replace `var'_ind=(``var'mt3'-``var'mc')/``var'sdc' if `var'==. & treatment==4
replace `var'_ind=(``var'mt4'-``var'mc')/``var'sdc' if `var'==. & treatment==5
replace `var'_ind=(``var'mtyou'-``var'mc')/``var'sdc' if `var'==. & mechanisms_rando==2

}
.

gen index_redistribution = (Q06001_1_invert_ind + Q06001_2_en_ind + Q06001_3_invert_ind + Q06001_4_en_ind + Q06001_5_en_ind + Q06002_invert_ind + Q06004_invert_ind + Q06005_en_ind + Q06007_1_invert_ind + Q06007_2_invert_ind) / 10
gen index_efficiency = (Q06008_en_ind + Q06009_en_ind + Q06010_en_ind + Q06011_en_ind + Q06013_en_ind + Q07008_invert_ind  )/6
gen index_knowledge = (Q06014_en_ind + Q06015_invert_ind + Q06016_en_ind + Q06017_invert_ind + Q06018_invert_ind + Q06019_en_ind + Q06020_en_ind + Q06021_en_ind + Q06022_en_ind + Q06023_en_ind + Q06024_invert_ind)/11
gen index_government = (G10001_en_ind + G10002_en_ind + G10003_en_ind) / 3

label var index_efficiency "Efficiency index"
label var index_redistribution "Redistribution index"
label var index_knowledge "Knowledge index"
label var index_government "Government trust index"



save "$data_trade/Analytic/data_regression_$date.dta", replace
