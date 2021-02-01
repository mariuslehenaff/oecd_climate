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

global dir "$user/Dropbox/Teaching_Economics/Full Data/Respondi" // working folder
global trade "$dir/Trade" // working folder
global data_trade "$trade/Data"
global output_trade "$data_trade/Output"

global date "2019_09_18"


use "$data_trade/Analytic/data_$date.dta"

set more off

************************************
********* Survey completed? ********
************************************

* Proxy: did they answer the WTP question? (last forced question)

gen complete=1 if(!missing(WTP1)& Progress=="100" |!missing(WTP2)& Progress=="100"|!missing(WTP5)& Progress=="100"|!missing(WTP10)& Progress=="100")
replace complete=0 if missing(complete)

* flag_time
destring screen, replace

su Durationinminutes if(screen==1), detail
return list
gen flag_time=(Durationinminutes<r(p5))

*******************************************************
********** Variables describing randomization *********
*******************************************************

* Did the respondent receive incentives? 

gen 		incentives=1 if(FL_57_DO!="FL_66" & !missing(FL_57_DO))
replace 	incentives=0 if(FL_57_DO=="FL_66")
label var 	incentives "Received incentive"

label define order_rando 1 "Control Group (Outcomes First)" 2 "Distributional First" 3 "Efficiency First"
gen order_rando = 1 if(FL_57_DO=="FL_135" | FL_57_DO=="FL_145" | FL_57_DO=="FL_155" | FL_57_DO=="FL_165" | FL_57_DO=="FL_84")
	replace order_rando = 2 if( FL_57_DO=="FL_58" | FL_57_DO=="FL_93" | FL_57_DO=="FL_66")
	replace order_rando = 3 if( FL_57_DO=="FL_65")
label val order_rando order_rando

label define mechanisms_rando 1 "Neutral" 2 "You"
gen mechanisms_rando = 2 if(FL_57_DO=="FL_93" | FL_57_DO=="FL_66")
	replace mechanisms_rando = 1 if(missing(mechanisms_rando) & !missing(FL_57_DO))
label val mechanisms_rando mechanisms_rando

label define treatment 1 "No Video" 2 "Distributional Video" 3 "Efficiency Video" 4 "Economist (Generic)" 5 "Economist (US Specific)"
gen 		treatment = 1 if(!missing(FL_57_DO))
replace 	treatment = 2 if(FL_57_DO=="FL_135")
replace 	treatment = 3 if(FL_57_DO=="FL_145")
replace 	treatment = 4 if(FL_57_DO=="FL_155")
replace 	treatment = 5 if(FL_57_DO=="FL_165")
label val 	treatment treatment


* nine branches of randomization
gen group=.
replace group=0 if incentives==1 & treatment==1 & order_rando==1 & mechanisms_rando==1
replace group=1 if incentives==1 & treatment==1 & order_rando==2 & mechanisms_rando==1
replace group=2 if incentives==1 & treatment==1 & order_rando==2 & mechanisms_rando==2
replace group=3 if incentives==0 & treatment==1 & order_rando==2 & mechanisms_rando==2
replace group=4 if incentives==1 & treatment==1 & order_rando==3 & mechanisms_rando==1
replace group=5 if incentives==1 & treatment==2 & order_rando==1 & mechanisms_rando==1
replace group=6 if incentives==1 & treatment==3 & order_rando==1 & mechanisms_rando==1
replace group=7 if incentives==1 & treatment==4 & order_rando==1 & mechanisms_rando==1
replace group=8 if incentives==1 & treatment==5 & order_rando==1 & mechanisms_rando==1



gen control = (mechanisms_rando == 1 & treatment == 1)
****************************************
******** Encoding all questions ********
****************************************


	************************************
	****** Quota variables (01.0) ******
	************************************

encode		G00001, gen(G00001_)
order		G00001_, after(G00001)
drop		G00001
rename 		G00001_ G00001
label var 	G00001 "Participation question"

encode 		G01001, gen(gender)
label var 	G01001 "Gender"

*** Age and Age group
gen 		age = G01002_4
destring	age, replace

gen 			age_group="." 
replace 		age_group="18-29" if(age>17 & age<30)
replace 		age_group="30-39" if(age>29 & age<40)
replace 		age_group="40-49" if(age>39 & age<50)
replace 		age_group="50-59" if(age>49 & age<60)
replace 		age_group="60-69" if(age>59 & age<70)
label define 	agegroup 1 "18-29" 2 "30-39" 3 "40-49" 4 "50-59" 5 "60-69"
encode			age_group, gen(agegroup) label(agegroup)
drop 			age_group

*** Income and Income group 
label define income ///
	1 "$0-$9999" 		2 "$10000-$14999" 		3 "$15000-$19999" 		4 "$20000-$29999" ///
	5 "$30000-$39999" 	6 "$40000-$49999" 		7 "$50000-$69999" 		8 "$70000-$89999" ///
	9 "$90000-$109999" 	10 "$110000-$149999" 	11 "$150000-$199999" 	12 "$200000+"
encode G01003, gen(income) label(income)
label var G01003 "Income"

gen income_group="."
replace income_group="$0-$19999" if(G01003=="$0-$9999"| G01003== "$10000-$14999"|G01003=="$15000-$19999")
replace income_group="$20000-$39999"	if(G01003=="$20000-$29999"  | G01003== "$30000-$39999")
replace income_group="$40000-$69999" 	if(G01003=="$40000-$49999"  | G01003== "$50000-$69999")
replace income_group="$70000-$109999" 	if(G01003=="$70000-$89999"  | G01003== "$90000-$109999")
replace income_group="$110000+" 		if(G01003=="$110000-$149999"| G01003== "$150000-$199999"|G01003=="$200000+")

label define incomegroup 1 "$0-$19999" 2 "$20000-$39999" 3 "$40000-$69999" 4 "$70000-$109999" 5 "$110000+"
encode income_group, gen(incomegroup) label(incomegroup)
drop income_group

	************************************
	******* Characteristics (01.1) *****
	************************************

* Were you born in the US? 
label define born_us 1 "Yes" 2 "No"
encode G01004, gen(born_us) label(born_us)

* In what state were you born?
sort BEN01
encode BEN01, gen(state_birth)

* Marital status
label define marital_status 1 "Single" 2 "Married" 3 "Legally separated or divorced" 4 "Widowed"
encode G01006, gen(marital_status) label(marital_status)

* Number of children
label define nb_child 0 "I do not have children" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5 or more"
encode G01007, gen(nb_child) label(nb_child)

* Ethnicity/Race
label define race 1 "European American/White" 2 "African American/Black" 3 "Hispanic/Latino" 4 "Asian/Asian American" 5 "Mixed race" 6 "Other (please specify)"
encode G01008, gen(race) label(race)

* Highest level of education
#delimit ;
label define education_level 1 "Eighth Grade or lower" 2 "Some High School"
	3 "High School degree/ GED" 4 "Some College" 5 "2-year College Degree"
	6 "4-year College Degree" 7 "Master's Degree" 8 "Doctoral Degree"
	9 "Professional Degree (JD, MD, MBA)" ; 
#delimit cr
encode G01009, gen(education_level) label(education_level)

* Field of study
encode G01010, gen(field_study)

* Current employment status
#delimit ; 
label define employment_status 1 "Full-time employee" 2 "Part-time employee"
	3 "Self-employed or small business owner" 4 "Unemployed and looking for work"
	5 "Student" 6 "Not currently working and not looking for work" 7 "Retiree" ;
#delimit cr
encode G01012, gen(employment_status) label(employment_status)
	
* Main occupation 
gen 	occupation = G01013
replace occupation = G01014 if missing(occupation)
replace occupation = substr(occupation, 1, 10)

replace occupation = "Managers" 					if(occupation == "Managers (")
replace occupation = "Professionals" 				if(occupation == "Profession")
replace occupation = "Technicians" 					if(occupation == "Technician")
replace occupation = "Clericals" 					if(occupation == "Clerical s")
replace occupation = "Service and sales"			if(occupation == "Service an")
replace occupation = "Agriculture" 					if(occupation == "Agricultur")
replace occupation = "Craft and trade" 				if(occupation == "Craft and ")
replace occupation = "Plant and machine operators" 	if(occupation == "Plant and ")
replace occupation = "Elementary occupations" 		if(occupation == "Elementary")
replace occupation = "Armed forces" 				if(occupation == "Armed forc")

encode occupation, gen(occupation_)
drop occupation
rename occupation_ occupation

gen high_skilled=.
replace high_skilled=1 if (occupation==6|occupation==8|occupation==10)
replace high_skilled=0 if (occupation==1|occupation==2|occupation==3|occupation==4|occupation==5|occupation==7|occupation==9)
label var high_skilled "High-skill occupation"


gen low_skilled=.
replace low_skilled=0 if (occupation==6|occupation==8|occupation==10)
replace low_skilled=1 if (occupation==1|occupation==2|occupation==3|occupation==4|occupation==5|occupation==7|occupation==9)
label var low_skilled "Low-skill occupation"

* Industrial sector

replace G01043 = G01044 if missing(G01043)
encode G01043, gen(sector)


* Social class
label define social_class 1 "Lower Class or Poor" 2 "Working Class" 3 "Middle Class" 4 "Upper-middle Class" 5 "Upper Class"
encode G01018, gen(social_class) label(social_class)
label var social_class "G01018"

* Economic policy matters: liberal/conservative spectrum
label define economic_ideology 1 "Very liberal" 2 "Liberal" 3 "Moderate" 4 "Conservative" 5 "Very conservative"
gen economic_ideology = G01019
encode economic_ideology, gen(econ_ideology) label(economic_ideology)
drop economic_ideology

* Political affiliation
label define political_party 1 "Republican" 2 "Democrat" 3 "Independent" 4 "Other" 5 "Non-Affiliated"
gen political_party_ = G01020
encode political_party_, gen(political_party) label(political_party)
drop political_party_

* Vote in 2016 presidential election?
label define Yes_No 0 "No" 1 "Yes"
encode G01022, gen(vote_2016) label(Yes_No)

* 2016 preferred candidate (whether they voted or not)
gen candidate2016 = G01023
replace candidate2016 =G01024 if missing(candidate2016)
encode candidate2016, gen(candidate_2016)
drop candidate2016

* Registered to vote at current address? 
encode G01025, gen(registered_vote) label(Yes_No)

* Vote at 2018 midterms? 
encode G01028, gen(vote_midterms) label(Yes_No)

* Preferred party in 2018 midterms (whether they voted or not)
gen party_midterms_=G01029
replace party_midterms_=G01030 if missing(party_midterms_)
label define party 1 "Republican Party" 2 "Democratic Party"
encode party_midterms_, gen(party_midterms) label(party)
drop party_midterms_

* Main media for news
label define news_media 1 "Internet" 2 "TV" 3 "Radio" 4 "Newspaper (print)" 5 "Word of mouth" 6 "Magazine" 7 "Other" 8 "None, I don't follow the news"
encode G01031, gen(news_media) label(news_media)

gen dummy_foxnews =(G01032=="Fox News")
label var dummy_foxnews "Fox News main source of news"

gen social_media=0
replace social_media=1 if G01035=="Facebook"
replace social_media=1 if G01035=="Instagram"
replace social_media=1 if G01035=="Other social media"
replace social_media=1 if G01035=="Reddit"
replace social_media=1 if G01035=="Snapchat"
replace social_media=1 if G01035=="Twitter"
replace social_media=1 if G01035=="Youtube"


* Have one variable with the primary source of news (whether its radio, tv, internet etc)
gen main_source_news = G01032
replace main_source_news = G01033 if missing(main_source_news)
replace main_source_news = G01036 if missing(main_source_news)
replace main_source_news = G01037 if missing(main_source_news)
replace main_source_news = G01038 if missing(main_source_news)
replace main_source_news = G01034 if !missing(G01034)

* How knowledgeable do you consider yourself on economic policies and issues?
label define knowledge_econ 4 "Highly knowledgeable" 3 "Somewhat knowledgeable" 2 "Not very knowledgeable" 1 "Not knowledgeable at all"
encode G01041, gen(knowledge_econ) label(knowledge_econ)





	********************************
	** Background Specific (01.3) **
	********************************
	
label define Q01037 1 "Not at all proud" 2 "Only a little proud" 3 "Moderately proud" 4 "Very proud" 5 "Extremely proud"
encode Q01037, gen(Q01037_en) label(Q01037)
label var Q01037_en "Proud to be American"

label define Q01038 1 "Not important at all" 2 "Somewhat important" 3 "Very important"
encode Q01038, gen(Q01038_en) label(Q01038)
label var Q01038_en "Born in US to be American?"

label define Q01039 1 "Completely disagree" 2 "Mostly disagree" 3 "Mostly agree" 4 "Completely agree"
encode Q01039, gen(Q01039_en) label(Q01039)
label var Q01039_en "Culture superior to others"
	
	************************************
	******* Personal exposure (03) *****
	************************************

label define yes_no 1 "No" 2 "Yes"
encode Q03001, gen(Q03001_en) label(yes_no)
label var Q03001_en "Trade policy direct effect on life"

	************************************
	******* Knowledge (04.A/04.B) ******
	************************************

	* Combining open-ended questions

gen Q04003 = Q04103
	replace Q04003 = Q04203 if missing(Q04003)
gen Q04005 = Q04105
	replace Q04005= Q04205 if missing(Q04005)

	
	* Non numerical questions
foreach y in "02" "04" "09" "10" "11" "08_2" "08_3" "08_4" "08_5" "08_6" "08_7"{
	gen Q040`y' = Q042`y'
	replace Q040`y'=Q041`y' if missing(Q040`y')
}
.

encode Q04002, gen(Q04002_en) label(yes_no)
encode Q04004, gen(Q04004_en) label(yes_no)
label var Q04002_en "Know what import tariff"
label var Q04004_en "Know what import quota"


	* Numerical questions
foreach x in "06_1" "07_1" {
	gen Q040`x' = Q042`x'
	replace Q040`x' = Q041`x' if missing(Q040`x')
	destring Q040`x', gen(Q040`x'_en) force
}
.
rename Q04006_1_en Q04006_en
rename Q04007_1_en Q04007_en
label var Q04006_en "Share of imports that are agricultural"
label var Q04007_en "Share of imports subject to tariff"

label define countries 1 "Brazil" 2 "Canada" 3 "China" 4 "France" 5 "Germany" 6 "India" 7 "Ireland" 8 "Italy" 9 "Japan" 10 "Mexico" 11 "Netherlands" 12 "South Korea" 13 "Switzerland" 14 "Taiwan" 15 "United Kingdom"
encode Q04009, gen(Q04009_en) label(countries)
encode Q04010, gen(Q04010_en) label(countries)

replace Q04011="It has increased" if Q04011=="It has increased " 
label define Q04011 1 "It has decreased" 2 "It has stayed more or less the same" 3 "It has increased"
encode Q04011, gen(Q04011_en) label(Q04011)
label var Q04011_en "Has trade increased, decresed or stayed the same?"

	************************
	**** Mechanisms (06) ***
	************************

		******************
		* Distributional *
		******************

* Q06001 

foreach x in 1 2 3 4 5 {

gen			Q06001_`x' = Q06101_`x'
replace		Q06001_`x' = Q06201C_1 if missing(Q06001_`x')
destring 	Q06001_`x', gen(Q06001_`x'_en) force
}	
.


label var Q06001_1_en "Large corporations"
label var Q06001_2_en "Small businesses"
label var Q06001_3_en "High-income households"
label var Q06001_4_en "Middle-class households"
label var Q06001_5_en "Low-income households"

* Q06002 

#delimit ; 
label define Q06102 
1 "More international trade can make everyone in the U.S. better off. Even if some people lose from it, it creates sufficient gains so that even those who lose from it can be compensated through appropriate policies."
2 "Free trade will entail winners and losers and it will be impossible to compensate those who lose from it." ;
label define Q06202A 
1 "More international trade can make everyone in the U.S. better off. Even if people like me may lose from it, it creates sufficient gains so that even we who lose from it can be compensated through appropriate policies."
2 "Free trade will entail winners and losers and it will be impossible to compensate people like me who may lose from it." ;
label define Q06202B 
1 "More international trade can make everyone in the U.S. better off. Even if certain people may lose from it, appropriate policies can enable them to be compensated through the sufficient gains made by people like me."
2 "Free trade will entail winners and losers and it will be impossible to compensate people who, unlike me, may lose from it." ;
#delimit cr


encode Q06102	, gen(Q06102_en ) label(Q06102 )
encode Q06202A	, gen(Q06202A_en) label(Q06202A) 
encode Q06202B	, gen(Q06202B_en) label(Q06202B) 
gen Q06002_en = Q06102_en
	replace Q06002_en = Q06202A_en if missing(Q06002_en)
	replace Q06002_en = Q06202B_en if missing(Q06002_en)
label define Q06002 1 "More international trade can make everyone better off (through transfers)" 2 "Impossible to compensate losers of international trade"
label val Q06002_en Q06002

* Q06003

label define Q06003 1 "No" 2 "Yes"

encode Q06103, gen(Q06103_en) label(Q06003)
encode Q06203, gen(Q06203_en) label(Q06003)

gen Q06003_en = Q06103_en 
	replace Q06003_en = Q06203_en if missing(Q06003_en)
label val Q06003_en Q06003 
label var Q06003_en "Most jobs affected by US trade policy?"
	
* Q06004

label define Q06004 1 "Helped U.S. workers" 2 "Hurt U.S. workers"
gen Q06004 = Q06104
	replace Q06004 = Q06204 if missing(Q06004)
encode Q06004, gen(Q06004_en) label(Q06004)	
label var Q06004_en "Overall international trade has helped or hurt US workers?"
	
* Q06005 and Q06006 (two questions in Neutral, only one in You)

label define Q06005 1 "Yes" 2 "No"
gen Q06005 = Q06105 
	replace Q06005 = Q06205 if missing(Q06005) 
encode Q06005, gen(Q06005_en) label(Q06005) /* low skilled */
label var Q06005_en "Would it be easy for low skilled workers to find job in a different sector?"

gen Q06006 = Q06106 
	replace Q06006 = Q06205 if missing(Q06006)
encode Q06006, gen(Q06006_en) label(Q06005) /* high skilled */
label var Q06006_en "Would it be easy for high skilled workers to find job in a different sector?"

* Q06007

gen Q06007_1 = Q06107_1
	replace Q06007_1 = Q06207_1 if missing(Q06007_1)
gen Q06007_2 = Q06107_2
	replace Q06007_2 = Q06207_2 if missing(Q06007_2)
label define Q06007 1 "None at all" 2 "A little" 3 "A moderate amount" 4 "A lot" 5 "A great deal"
encode Q06007_1, gen(Q06007_1_en) label(Q06007)
encode Q06007_2, gen(Q06007_2_en) label(Q06007)
label var Q06007_1_en "Trade major reason for unemployment in some sectors"
label var Q06007_2_en "Trade major reason for rise of inequality"


	**************
	* Efficiency *
	**************
	
* Q06008 & Q06009 & Q06010 & Q06011

label define Q06008 1 "No" 2 "Yes" 
gen Q06008 = Q06108
	replace Q06008 = Q06208 if missing(Q06008)
encode Q06008, gen(Q06008_en) label(Q06008)
label var Q06008_en "Overall, has international trade decreased the prices of goods sold in the U.S.?"



gen Q06009 = Q06109 
	replace Q06009 = Q06209 if missing(Q06009)
encode Q06009, gen(Q06009_en) label(Q06008)
label var Q06009_en "Do you think that international trade has made firms in the U.S. more competitive and improved their productivity?"



gen Q06010 = Q06110
	replace Q06010 = Q06210 if missing(Q06010)
encode Q06010, gen(Q06010_en) label(Q06008)
label var Q06010_en "Do you think that the competitive pressure from international trade has increased innovation in the U.S.?"



gen Q06011 = Q06111 
	replace Q06011 = Q06211 if missing(Q06011)
encode Q06011, gen(Q06011_en) label(Q06008)
label var Q06011_en "Do you think that international trade has increased the growth of the GDP in the U.S.? "



label define Q06012 1 "It will increase" 2 "It will not change" 3 "It will decrease"
gen Q06012 = Q06112 
	replace Q06012 = Q06212 if missing(Q06012)
encode Q06012, gen(Q06012_en) label(Q06012)
label var Q06012_en "What will happen to the value of dollar if US exports more?"

	**************
	* Case Study * 
	**************

label define Q06013 3 "In general, both countries are better off" 2 "In general, one country gains, the other one loses" 1 "In general, both countries lose"
label define Q06014 2 "Yes, it makes sense for the U.S. to import cars from Germany under some circumstances" 1 "No, the U.S. should not import cars from Germany if it's better at producing them"

label define Q06015 1 "It will decrease" 2 "It will remain the same" 3 "It will increase"
label define Q06016 2 "Yes" 1 "No"
label define Q06017 1 "Wages will decrease" 2 "Wages will stay the same" 3 "Wages will increase"
label define Q06018 1 "It will decrease"    2 "It will remain the same"  3 "It will increase"
label define Q06019 1 "Wages will decrease" 2 "Wages will stay the same" 3 "Wages will increase"
label define Q06020 1 "Wages will decrease" 2 "Wages will stay the same" 3 "Wages will increase"
label define Q06021 1 "It will decrease"    2 "It will remain the same"  3 "It will increase"
label define Q06022 1 "It will decrease"    2 "It will remain the same"  3 "It will increase"
label define Q06023 1 "It will decrease"    2 "It will remain the same"  3 "It will increase"
label define Q06024 1 "It will decrease"    2 "It will remain the same"  3 "It will increase"

foreach x in 13 14 15 16 17 18 19 20 21 22 23 24 {
encode Q060`x', gen(Q060`x'_en) label(Q060`x')
}
	
	*******************************
	**** Specific Outcomes (07) ***
	*******************************

* smal error
replace Q07107 = "Restrict food imports from abroad" if Q07107 == "Restrict food imports form abroad"
replace Q07207 = "Restrict food imports from abroad" if Q07207 == "Restrict food imports form abroad"	
*
	
label define Q07001 1 "Strongly agree" 	2 "Agree" 				3 "Neither agree nor disagree" 	4 "Disagree" 		5 "Strongly disagree"
label define Q07002 1 "Very fair" 		2 "Somewhat fair" 		3 "Somewhat unfair" 			4 "Very unfair" 
label define Q07003 1 "Very satisfied" 	2 "Somewhat satisfied" 	3 "Somewhat dissatisfied" 		4 "Very dissatisfied"	
label define Q07008 1 "Very likely" 	2 "Likely" 				3 "Unlikely" 					4 "Very unlikely"
label define Q07007 1 "Provide more production subsidies in the food sector" 2 "Restrict food imports from abroad"

	* 
	
foreach x in 01 02 03 07 08 {
gen Q070`x' = Q071`x'
	replace Q070`x' = Q072`x' if missing(Q070`x')
encode Q070`x', gen(Q070`x'_en) label(Q070`x')
}	

	* Q07009 
	
label define Q07109	1 "It makes sense to let these firms face foreign competition to become more competitive" ///
					2 "It makes sense to protect for a while"
label define Q07209	1 "It makes sense to let the firms in my sector face foreign competition to become more competitive" ///
					2 "It makes sense to protect for a while"
encode Q07109, gen(Q07109_en) label(Q07109)
encode Q07209, gen(Q07209_en) label(Q07209)

gen		Q07009_en = Q07109_en
replace	Q07009_en = Q07209_en if missing(Q07009_en)
label values Q07009_en Q07109



gen Q07004 = Q07104
	replace Q07004 = Q07204 if missing(Q07004)
gen Q07005 = Q07105 
	replace Q07005 = Q07205 if missing(Q07005)

gen Q07006_1 = Q07106_1
	replace Q07006_1 = Q07206_1 if missing(Q07006_1)
gen Q07006_2 = Q07106_2
	replace Q07006_2 = Q07206_2 if missing(Q07006_2)
gen Q07006_3 = Q07106_3
	replace Q07006_3 = Q07206_3 if missing(Q07006_3)
destring Q07006_1, gen(Q07006_1_en)
destring Q07006_2, gen(Q07006_2_en)	
destring Q07006_3, gen(Q07006_3_en)	
	
	*******************************************
	********** General Outcomes (G08) *********
	*******************************************

label define G08002 5 "... pay much less than their fair share in income taxes" 4 "... pay less than their fair share in income taxes" 3 "... pay their fair share in income taxes" 2 "... pay more than their fair share in income taxes" 1 "... pay much more than their fair share in income taxes"
encode G08002, gen(G08002_en) label(G08002)
label var G08002_en "High-income households"
encode G08003, gen(G08003_en) label(G08002)
label var G08003_en "Middle-class households"	
	
label define G08004 3 "More of this service, more taxes" 2 "Service and taxes as now" 1 "Less of this service, reduced taxes"

foreach x in "1" "2" "3" "4" "5" {
	encode G08004_`x', gen(G08004_`x'_en) label(G08004)
	}

label var G08004_1_en "Transfers, income support"
label var G08004_2_en "Better schools"
label var G08004_3_en "Income support, retraining"
label var G08004_4_en "Subsidies for low-income"
label var G08004_5_en "Wage subsidies"


	*******************************************
	******** Specific Government (Q09) ********
	*******************************************	
	
*replace tags = subinstr(tags, `"""',  "", .)
replace Q09003 = "... most people gain, but a few people lose a lot." if Q09003 == `"... most people gain, but a few people lose a lot.""'
replace Q09003 = "... most people lose, but a few gain a lot." if Q09003 == `"... most people lose, but a few gain a lot.""'

label define Q09003 1 "... most people gain, but a few people lose a lot." 2 "... most people lose, but a few gain a lot."
encode Q09003, gen(Q09003_en) label(Q09003) 

	*******************************************
	******** General Government (G10) *********
	*******************************************

label define G10001 4 "Almost always" 3 "A lot of the time" 2 "Not very often" 1 "Almost never"
encode G10001, gen(G10001_en) label(G10001)

label define G10002 1 "Government is doing too much" 2 "Government is doing just the right amount" 3 "Government should do more"
encode G10002, gen(G10002_en) label(G10002)	

destring G10003, gen(G10003_en)

gen G10003_invert_en = 1 if(G10003_en==5)
	replace G10003_invert_en = 2 if(G10003_en==4)
	replace G10003_invert_en = 3 if(G10003_en==3)
	replace G10003_invert_en = 4 if(G10003_en==2)
	replace G10003_invert_en = 5 if(G10003_en==1)

rename G10004_1 G10004	
destring G10004, gen(G10004_en)

label define G10005 4 "Very satisfied" 3 "Somewhat satisfied" 2 "Somewhat dissatisfied" 1 "Very dissatisfied"
encode G10005, gen(G10005_en) label(G10005)

foreach x in "1" "2" "3" "4" "5" "6" "7" "8" {
	destring G10006_`x', gen(G10006_`x'_en)
	}

foreach x in 1 2 3 4 5 6 7 8 {
	gen G10006_`x'_invert_en = 1 if(G10006_`x'_en==5)
		replace G10006_`x'_invert_en = 2 if(G10006_`x'_en==4)
		replace G10006_`x'_invert_en = 3 if(G10006_`x'_en==3)
		replace G10006_`x'_invert_en = 4 if(G10006_`x'_en==2)
		replace G10006_`x'_invert_en = 5 if(G10006_`x'_en==1)
}
.
	
************************************
******* Screening questions ********
************************************
/*
gen S1_correct=1 if S1=="Strongly agree,Strongly disagree"
replace S1_correct=0 if(!missing(S1) & S1 !="Strongly agree,Strongly disagree")
tab S1_correct

gen S2_correct=1 if(S22_1=="98"|S21_1=="98")
replace S2_correct=0 if(missing(S2_correct))
replace S2_correct=. if(missing(S22_1)&missing(S21_1))
tab S2_correct

gen S3_correct=1 if(S3_1=="ABC News,The Drudge Report"|S3_2=="ABC News,The Drudge Report")
replace S3_correct=0 if(missing(S3_correct) & !missing(S3_1) | missing(S3_correct) & !missing(S3_2))
tab S3_correct

* Total correct answers at screening questions. 
gen screening_success = S1_correct+S2_correct+S3_correct
gen flag_screen=1 if(screening_success==0)
*/

gen S1_correct=(S1=="Strongly agree,Strongly disagree")
tab S1_correct, mi

gen S2_correct=(S22_1=="98"|S21_1=="98")
tab S2_correct, mi

gen S3_correct=(S3_1=="ABC News,The Drudge Report"|S3_2=="ABC News,The Drudge Report")
tab S3_correct, mi

* Total correct answers at screening questions. 
gen screening_success = S1_correct+S2_correct+S3_correct
gen flag_screen=1 if(screening_success==0)
tab flag_screen, mi

******************************
******** WTP questions *******
******************************

*encode the willingness to pay questions leaving the text as it is now - =1 if No; =2 if Yes
label define wtp 0 "No, I am not willing to pay" 1 "Yes, I am willing to pay"
foreach i in 1 2 5 10 {
	encode WTP`i', gen (wtp`i')
	replace wtp`i'=0 if wtp`i'==1
	replace wtp`i'=1 if wtp`i'==2
	label var wtp`i' "Willing to pay `i' dollars"
	label val wtp`i' wtp
}
.

*synthetic variable for WTP
gen wtp_agg=.
	foreach i in 1 2 5 10 {
	replace wtp_agg=wtp`i' if wtp`i'!=.
	label var wtp_agg "Willing to pay some amount"
	label val wtp_agg wtp
}
.

*Are you surprised by these numbers?
encode Q12002, gen(Q12002_en)
label val Q12002_en "yes_no"

************************************
***** Self-reported questions ******
************************************

label define order_self_1 1 "I put forth almost no effort" 2 "I put forth very little effort" 3 "I put forth some effort" 4 "I put forth quite a bit of effort" 5 "I put forth a lot of effort"
encode G11001, gen(G11001_) label(order_self_1)
drop G11001
rename G11001_ G11001

label define order_self_2 1 "I gave this study almost no attention" 2 "I gave this study very little attention" 3 "I gave this study some of my attention" 4 "I gave this study most of my attention" 5 "I gave this study my full attention"
encode G11002, gen(G11002_) label(order_self_2)
drop G11002
rename G11002_ G11002



save "$data_trade/Analytic/data_clean_$date.dta", replace
