# Examples
# Be careful with the arguments (df, miss...)

# e <- readRDS("../data/US_pilot_reg_210115.rds")

#source(".Rprofile")

# In case you want to redefine the labels, it need not be automatized, e.g.:
# labels_responsible <- c("Each of us", "The rich", "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "CC doesn't exist")
# Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F 

# (CC_exists_US <- barres(vars = "CC_exists", export_xls = export_xls, df = e, miss = T, labels="In your opinion, climate change is..."))
# save_plotly(CC_exists_US)
# # Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F

# TO ADJUST:
e <- us[us$treatment=="None",]
export_xls <- F

##### Pre-treatment ## #####

##### 1. Demographics #####
labels_comp <- c("Sample: non-weighted", "Sample: weighted", "Population")
label_great_deal <- c("Not at all"," A little","Moderately","A lot","A great deal")
labels_agree <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
data_gender <- cbind(dataKN("gender_factor", data=e, miss=F, weights = F), dataKN("gender_factor", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975)) # TODO: remove 0 if there is not "Other" in data
data_age <- cbind(dataKN("age_quota", data=e[e$age_quota!="Below 18",], miss=F, weights = F), dataKN("age_quota", data=e[e$age_quota!="Below 18",], miss=F, weights = T), c(0.118,0.180,0.243,0.2467,0.2118))
data_region <- cbind(dataKN("region", data=e, miss=F, weights = F), dataKN("region", data=e, miss=F, weights = T), c(0.171,0.208,0.383,0.239))
data_core_metropolitan <- cbind(dataKN("core_metropolitan", data=e, miss=F, weights = F), dataKN("core_metropolitan", data=e, miss=F, weights = T), c(0.7324))
data_race <- cbind(dataKN("race", data=e, miss=F, weights = F), dataKN("race", data=e, miss=F, weights = T), c(.134, .185, .080, .601))
data_income <- cbind(dataKN("income", data=e, miss=F, weights = F), dataKN("income", data=e, miss=F, weights = T), c(0.2034,0.239,0.2439,0.3137))
data_vote <- cbind(dataKN("vote_2020", data=e, miss=T, weights = F), dataKN("vote_2020", data=e, miss=T, weights = T), c(0.342171, 0.345006, 0.312823, 0))
# data_urbanity <- cbind(dataKN("urbanity", data=e, miss=F, weights = F), dataKN("urbanity", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975)) # TODO: find official freq for these ones and plot figures, also on polluting sector
# data_education <- cbind(dataKN("education", data=e, miss=F, weights = F), dataKN("education", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_wealth <- cbind(dataKN("wealth", data=e, miss=F, weights = F), dataKN("wealth", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_occupation <- cbind(dataKN("occupation", data=e, miss=F, weights = F), dataKN("occupation", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_employment_agg <- cbind(dataKN("employment_agg", data=e, miss=F, weights = F), dataKN("employment_agg", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_marital_status <- cbind(dataKN("marital_status", data=e, miss=F, weights = F), dataKN("marital_status", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_Nb_children <- cbind(dataKN("Nb_children", data=e, miss=F, weights = F), dataKN("Nb_children", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_HH_size <- cbind(dataKN("HH_size", data=e, miss=F, weights = F), dataKN("HH_size", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# data_home <- cbind(dataKN("home", data=e, miss=F, weights = F), dataKN("home", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
# dataKN("vote_2020", data=e, miss=F, weights = F, return="legend")

(gender_US_comp <- barres(data = data_gender, export_xls = export_xls, df = e, miss = F, sort = F, labels=labels_comp, legend = dataKN("gender_factor", data=e, miss=F, return="legend")))
save_plotly(gender_US_comp, width= 470, height=240)

(age_US_comp <- barres(data = data_age, export_xls = export_xls, df = e, miss=F, rev = F, sort = F, labels=labels_comp, legend = dataKN("age_quota", data=e, miss=F, return="legend")[1:5]))
save_plotly(age_US_comp, width= 560, height=240) 

(region_US_comp <- barres(data = data_region, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_comp, legend = dataKN("region", data=e, miss=F, return="legend")))
save_plotly(region_US_comp, width= 560, height=240)

(core_metropolitan_US_comp <- barres(data = data_core_metropolitan, export_xls = export_xls, df = e, sort = F, miss=F, rev_color = T, rev = F, labels=labels_comp, showLegend = F, legend="Core metropolitan"))
save_plotly(core_metropolitan_US_comp, width= 660, height=240)

(race_US_comp <- barres(data = data_race, export_xls = export_xls, df = e, miss=F, rev = F, sort = F, labels=labels_comp, legend = dataKN("race", data=e, miss=F, return="legend")))
save_plotly(race_US_comp, width= 540, height=240)

(income_US_comp <- barres(data = data_income, export_xls = export_xls, df = e, miss=F, rev_color = T, sort = F, rev = F, labels=labels_comp, legend = dataKN("income", data=e, miss=F, return="legend")))
save_plotly(income_US_comp, width= 510, height=240) # 35/70/120

(vote_US_comp <- barres(data = data_vote, export_xls = export_xls, df = e, miss=T, sort = F, labels=labels_comp, legend = dataKN("vote_2020", data=e, miss=T, return="legend")))
save_plotly(vote_US_comp, width= 550, height=240)

(gender_US <- barres(vars = "gender_factor", export_xls = export_xls, df = e, miss = F, labels="Gender"))
save_plotly(gender_US, width= 470, height=140)

(age_quota_US <- barres(vars = "age_quota", export_xls = export_xls, df = e[e$age_quota!="Below 18",], miss=F, rev = F, labels="Age"))
save_plotly(age_quota_US, width= 500, height=140) 

(region_US <- barres(vars = "region", export_xls = export_xls, df = e, miss=F, labels="Region"))
save_plotly(region_US, width= 560, height=140)

(urbanity_US <- barres(vars = "urbanity", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Size of town"))
save_plotly(urbanity_US, width= 660, height=140)

(race_US <- barres(vars = variables_race[c(1:4)], export_xls = export_xls, df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
save_plotly(race_US, width= 340, height=240)

(race_agg_US <- barres(vars = "race", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Size of town"))
save_plotly(race_agg_US, width= 500, height=140)

(income_US <- barres(vars = "income", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
save_plotly(income_US, width= 510, height=140) # 35/70/120

(wealth_US <- barres(vars = "wealth", export_xls = export_xls, df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
save_plotly(wealth_US, width= 480, height=140)

(education_US <- barres(vars = "education", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
save_plotly(education_US, width= 1080, height=140) 

(employment_status_US <- barres(vars = "employment_agg", export_xls = export_xls, df = e, miss=F, labels="What is your employment status?"))
save_plotly(employment_status_US, width= 630, height=140)

(occupation_US <- barres(vars = "occupation", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Main occupation"))
save_plotly(occupation_US, width= 610, height=140)

#(sector_US <- barres(vars = "sector", export_xls = export_xls, df = e, miss=F, labels="What is your primary line of business?"))
#save_plotly(sector_US, width= 630, height=140)

(polluting_sector_US <- barres(vars = "polluting_sector", export_xls = export_xls, df = e, miss=F, showLegend = F, rev_color = T, rev = F, labels="Current (or last) job is in a polluting sector."))
save_plotly(polluting_sector_US, width= 600, height=140)

(hit_by_covid_US <- barres(vars = "hit_by_covid", export_xls = export_xls, df = e, miss=F, labels="Hit by covid"))
save_plotly(hit_by_covid_US, width= 420, height=140)

labels_home <- c()
for (v in variables_home) labels_home <- c(labels_home, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(home_US <- barres(vars = variables_home, export_xls = export_xls, df = e, rev = F, miss = F, showLegend=F, labels=labels_home, hover=labels_home))
save_plotly(home_US, width= 575, height=320) 

(couple_US <- barres(vars = "couple", export_xls = export_xls, df = e, miss=F, labels="Do you live with your partner?"))
save_plotly(couple_US, width= 630, height=140)

(marital_status_US <- barres(vars = "marital_status", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="What is your marital status?"))
save_plotly(marital_status_US, width= 830, height=175)

(nb_children_US <- barres(vars = "Nb_children", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Number of children", legend = c(0:3, "4 or more")))
save_plotly(nb_children_US, width= 470, height=140)

(hh_size_US <- barres(vars = "HH_size", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F,labels="People in household"))
save_plotly(hh_size_US, width= 470, height=140)

##### 2. HH composition and energy characteristics #####

(heating_US <- barres(vars = "heating", export_xls = export_xls, df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
save_plotly(heating_US, width= 525, height=140)

(heating_expenses_US <- barres(vars = "heating_expenses", export_xls = export_xls, df = e, miss=T, fr="Included", labels="Monthly heating expenses", rev = F))
save_plotly(heating_expenses_US, width= 950, height=140)

(insulation_US <- barres(vars = "insulation", export_xls = export_xls, df = e, miss=F, rev_color = T, labels="Quality of insulation", rev = F))
save_plotly(insulation_US, width= 600, height=140)

(gas_expenses_US <- barres(vars = "gas_expenses", export_xls = export_xls, df = e, miss=F, labels="Monthly gas expenses", rev = F))
save_plotly(gas_expenses_US, width= 800, height=140)

(flights_3y_US <- barres(vars = "flights_agg", export_xls = export_xls, df = e, miss=F, rev = F, labels="Round-trip flights between 2017 and 2019"))
save_plotly(flights_3y_US, width= 800, height=140)

(frequency_beef_US <- barres(vars = "frequency_beef", export_xls = export_xls, df = e, miss=F, rev = F, labels="How often do you eat beef?"))
save_plotly(frequency_beef_US, width= 720, height=140)

variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
labels_transport <- c("Work", "Shopping", "Leisure")
(transport_US <- barres(vars = variables_transport_graph, export_xls = export_xls, df = e, rev = F, miss = F,rev_color = T,  labels=labels_transport))
save_plotly(transport_US, width= 850, height=275) 

(availability_transport_US <- barres(vars = "availability_transport", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Quality and availability of public transport near your home"))
save_plotly(availability_transport_US, width= 870, height=140)

rquery.wordcloud(paste(e$CC_field, collapse=" \n "), max.words = 70) # TODO: update
rquery.wordcloud(paste(e$CC_field, collapse=" \n "), max.words = 70, excludeWords = c("climate", "change", "government"))

##### POST-TREATMENT #####

##### 3. Treatment feedback: local climate #####
(watched_climate_US <- barres(vars = "watched_climate", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (local)"))
save_plotly(watched_climate_US, width= 900, height=140)

# (know_treatment_climate_US <- barres(vars = c("know_temperature_2100", "know_frequence_heatwaves"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Knowledge climate video"))
# save_plotly(know_treatment_climate_US, width= 580, height=140)

(know_treatment_climate_US <- barres(vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Score knowledge climate video"))
save_plotly(know_treatment_climate_US, width= 580, height=140)

# (know_treatment_climate_watched_US <- barres(vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e[e$watched_climate=='Yes',], miss=F, labels="Score knowledge climate video"))
# save_plotly(know_treatment_climate_watched_US, width= 580, height=140)

##### 4. Treatment feedback: local policy #####
(watched_policy_US <- barres(vars = "watched_policy", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (policy)"))
save_plotly(watched_policy_US, width= 900, height=140)

(know_treatment_policy_US <- barres(vars = "know_treatment_policy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="Score knowledge policy video"))
save_plotly(know_treatment_policy_US, width= 580, height=140)

# labels_know_treatment <- c("Knows CC would cause 70 days of<br>heatwaves per year in the US by 2100", "Knows temperature would be +8°F<br>by 2100 with current trend emissions", "Knows first policy presented is<br>ban on combustion-engine cars", "Knows green infrastructure program<br>described would be debt-funded")
labels_know_treatment <- c("Knows CC would cause 70 days of<br>heatwaves per year in the US by 2100", "Knows temperature would be +8 Fahrenheit<br>by 2100 with current trend emissions", "Knows first policy presented is<br>ban on combustion-engine cars", "Knows green infrastructure program<br>described would be debt-funded")
(know_treatment_US <- barres(vars = paste0(variables_know_treatment, "_correct"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=labels_know_treatment))
save_plotly(know_treatment_US, width= 830, height=280)

##### 5. Climate knowledge #####

(CC_talks_US <- barres(vars = "CC_talks", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="How often do you talk about climate change?"))
save_plotly(CC_talks_US, filename="test", width= 760, height=140)

(CC_real_US <- barres(vars = "CC_real", export_xls = export_xls, df = e, miss=F, labels="Climate change real?"))
save_plotly(CC_real_US, width= 540, height=140)

(CC_anthropogenic_US <- barres(vars = "CC_anthropogenic", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Part of climate change anthropogenic"))
save_plotly(CC_anthropogenic_US, width= 660, height=140)

(CC_anthropogenic_non_deniers_US <- barres(vars = "CC_anthropogenic", rev = F, rev_color = T, export_xls = export_xls, df = e[e$CC_real!="No",], miss=F, labels="Part of climate change anthropogenic"))
save_plotly(CC_anthropogenic_non_deniers_US, width= 660, height=140)

(CC_problem_US <- barres(vars = "CC_problem", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Climate change is an important problem."))
save_plotly(CC_problem_US, width= 1125, height=140) 

(CC_knowledgeable_US <- barres(vars = "CC_knowledgeable", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="How knowledgeable about climate change"))
save_plotly(CC_knowledgeable_US, width= 800, height=140)

(CC_dynamic_US <- barres(vars = "CC_dynamic", export_xls = export_xls, df = e, rev=F, miss=F, labels="Cutting GHG emissions by half <br> sufficient to stop rise in temperatures"))
save_plotly(CC_dynamic_US, width= 600, height=140)

(GHG_US <- barres(vars = c("GHG_CO2", "GHG_methane", "GHG_H2", "GHG_particulates"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=c("CO<sub>2</sub>", "Methane", "Hydrogen", "Particulates")))
save_plotly(GHG_US, width= 270, height=220)

(score_GHG_US <- barres(vars = "score_GHG", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="Knowledge score on GHG"))
save_plotly(score_GHG_US, width= 550, height=140) # TODO? split score=3 into all but methane and others

(footprint_elec_US <- barres(vars = Variables_footprint$el[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$el[c(2,1,3)]))
save_plotly(footprint_elec_US, width= 400, height=220) 

(footprint_transport_US <- barres(vars = Variables_footprint$tr[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$tr[c(2,1,3)]))
save_plotly(footprint_transport_US, width= 400, height=220) 

(footprint_food_US <- barres(vars = Variables_footprint$fd[c(2,3,1)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$fd[c(2,3,1)]))
save_plotly(footprint_food_US, width= 400, height=220) 

(footprint_region_US <- barres(vars = paste(Variables_footprint$reg, "original", sep="_")[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, sort = F, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$reg[c(4,2,1,3)]))
save_plotly(footprint_region_US, width= 400, height=280) 

(footprint_region_no_miss_US <- barres(vars = Variables_footprint$reg[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$reg[c(4,2,1,3)]))
save_plotly(footprint_region_no_miss_US, width= 400, height=280) 

(footprint_pc_US <- barres(vars = paste(Variables_footprint$pc, "original", sep="_")[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, sort = F, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$pc[4:1]))
save_plotly(footprint_pc_US, width= 400, height=280) 
# TODO? polluting_sector
(footprint_pc_no_miss_US <- barres(vars = Variables_footprint$pc[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$pc[4:1]))
save_plotly(footprint_pc_no_miss_US, width= 400, height=280) 

(footprint_elec_US_extr <- barres(vars = c("least_footprint_el", "most_footprint_el"), export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Smallest footprint", "Largest footprint")))
save_plotly(footprint_elec_US_extr, width= 470, height=200) 

(footprint_transport_US_extr <- barres(vars = c("least_footprint_tr", "most_footprint_tr"), export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Smallest footprint", "Largest footprint")))
save_plotly(footprint_transport_US_extr, width= 470, height=200) 

(footprint_food_US_extr <- barres(vars = c("least_footprint_fd", "most_footprint_fd"), export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Smallest footprint", "Largest footprint")))
save_plotly(footprint_food_US_extr, width= 470, height=200) 

(footprint_region_US_extr <- barres(vars = c("least_footprint_reg", "most_footprint_reg"), export_xls = export_xls, df = e, miss=T, sort = F, labels=c("Smallest footprint", "Largest footprint")))
save_plotly(footprint_region_US_extr, width= 470, height=200) 

(footprint_pc_US_extr <- barres(vars = c("least_footprint_pc", "most_footprint_pc"), export_xls = export_xls, df = e, miss=T, sort = F, labels=c("Smallest footprint", "Largest footprint")))
save_plotly(footprint_pc_US_extr, width= 470, height=200) 

(score_footprint_transport_US <- barres(vars = "score_footprint_transport", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>transport footprint"))
save_plotly(score_footprint_transport_US, width= 520, height=140) 

(score_footprint_food_US <- barres(vars = "score_footprint_food", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>food footprint"))
save_plotly(score_footprint_food_US, width= 520, height=140)

(score_footprint_pc_US <- barres(vars = "score_footprint_pc", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance to true ranking of<br>regional per capita footprint"))
save_plotly(score_footprint_pc_US, width= 520, height=140)

(score_footprint_region_US <- barres(vars = "score_footprint_region", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance to true ranking of<br>regional total footprint"))
save_plotly(score_footprint_region_US, width= 520, height=140)

(score_footprint_regions_US <- barres(vars = c("score_footprint_pc", "score_footprint_region"), rev = F, sort = F, export_xls = export_xls, df = e, miss=T, labels=c("per capita regional footprint", "Distance to true ranking of<br>regional total footprint")))
save_plotly(score_footprint_regions_US, width= 550, height=200)

(score_footprint_elec_US <- barres(vars = "score_footprint_elec", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>electricity footprint"))
save_plotly(score_footprint_elec_US, width= 520, height=140)

(scores_footprint_US <- barres(vars = c("score_footprint_transport", "score_footprint_elec", "score_footprint_food"), rev = F, export_xls = export_xls, df = e, miss=F, labels=c("Transport", "Electricity","Food")))
save_plotly(scores_footprint_US, width= 500, height=240)

labels_CC_impacts <- c()
for (v in variables_CC_impacts) labels_CC_impacts <- c(labels_CC_impacts, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(CC_impacts_US <- barres(vars = variables_CC_impacts, export_xls = export_xls, df = e, miss=F, rev_color=T, labels=labels_CC_impacts))
save_plotly(CC_impacts_US, width= 800, height=400) 

##### 6. Climate Change (attitudes and risks) ##### 
labels_responsible <- c()
for (v in variables_responsible_CC) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(CC_responsible_US <- barres(vars = variables_responsible_CC, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_responsible, hover= label_great_deal))
save_plotly(CC_responsible_US, width= 720, height=300) 

(net_zero_feasible_US <- barres(vars = "net_zero_feasible", export_xls = export_xls, df = e, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S."))
save_plotly(net_zero_feasible_US, width= 620, height=140)

(CC_affects_self_US <- barres(vars = "CC_affects_self", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Climate change negatively affects personal life"))
save_plotly(CC_affects_self_US, width= 800, height=140) 

(pro_ambitious_policies_US <- barres(vars = "pro_ambitious_policies", export_xls = export_xls, df = e,rev = F, rev_color = T, miss = F, labels="Ambitious public policies needed<br>to halt climate change"))
save_plotly(pro_ambitious_policies_US, width= 670, height=140)

# (CC_attitude_US <- barres(vars = c("net_zero_feasible", "CC_affects_self", "pro_ambitious_policies"), export_xls = export_xls, df = e,rev = F, rev_color = T, sort = F, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S.", "Climate change negatively affects personal life", "Ambitious public policies needed<br>to halt climate change")))
# save_plotly(CC_attitude_US, width= 770, height=250)

(CC_attitude_US <- barres(vars = c("net_zero_feasible", "CC_affects_self"), export_xls = export_xls, df = e,rev = F, rev_color = T, sort = F, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S.", "Climate change negatively affects personal life")))
save_plotly(CC_attitude_US, width= 750, height=220)

(CC_will_end_US <- barres(vars = "CC_will_end", export_xls = export_xls, df = e, miss = F, labels="Likely to halt CC by the end of the century"))
save_plotly(CC_will_end_US, width= 855, height=140)

(effect_halt_CC_economy_US <- barres(vars = "effect_halt_CC_economy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Effects of ambitious policies <br> on the U.S economy and employment"))
save_plotly(effect_halt_CC_economy_US, width= 820, height=140)

(effect_halt_CC_lifestyle_US <- barres(vars = "effect_halt_CC_lifestyle", export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels="Negative effects of ambitious policies on lifestyle"))
save_plotly(effect_halt_CC_lifestyle_US, width= 680, height=140)

labels_willing <- c()
for (v in variables_willing) labels_willing <- c(labels_willing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(willing_US <- barres(vars = variables_willing, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_willing, hover=label_great_deal))
save_plotly(willing_US, width= 800, height=320) 

labels_condition <- c()
for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(condition_US <- barres(vars = variables_condition, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_condition, hover = label_great_deal))
save_plotly(condition_US, width= 805, height=250) 

##### 7. Pref 1: emission standards #####
labels_standard_effects <- c()
for (v in variables_standard_effect) labels_standard_effects <- c(labels_standard_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_standard_effects[3] <- "have a negative effect<br>on the U.S. economy and employment"
labels_standard_effects[4] <- "have a large  effect<br>on the U.S. economy and employment"
(standard_effect_US <- barres(vars = rev(variables_standard_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_standard_effects), hover=labels_agree))
save_plotly(standard_effect_US, width= 890, height=320) 

labels_win_lose <- c("Lose a lot", "Mostly lose", "Neither win nor lose", "Mostly win", "Win a lot")
labels_standard_win_lose <- c()
for (v in variables_standard_win_lose) labels_standard_win_lose <- c(labels_standard_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_standard_win_lose[5] <- "Your household financially"
(standard_win_lose_US <- barres(vars = variables_standard_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, sort=F, miss = F, showLegend=T, labels=labels_standard_win_lose, hover = labels_win_lose))
save_plotly(standard_win_lose_US, width= 1100, height=320) 

(standard_fair_US <- barres(vars = "standard_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Ban on combustion-engine cars fair"))
save_plotly(standard_fair_US, width= 1100, height=140)

(standard_support_US <- barres(vars = "standard_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Ban on combustion-engine cars"))
save_plotly(standard_support_US, width= 980, height=140)

(standard_public_transport_support_US <- barres(vars = "standard_public_transport_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Ban on combustion-engine cars with<br>alternatives such as public transports available"))
save_plotly(standard_public_transport_support_US, width= 950, height=140)

##### 8. Pref 2: Green investments #####
labels_investments_effects <- c()
for (v in variables_investments_effect) labels_investments_effects <- c(labels_investments_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_effect_US <- barres(vars = rev(variables_investments_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_investments_effects), hover=labels_agree))
save_plotly(investments_effect_US, width= 1100, height=320) 

labels_investments_win_lose <- c()
for (v in variables_investments_win_lose) labels_investments_win_lose <- c(labels_investments_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_investments_win_lose[5] <- "Your household financially"
(investments_win_lose_US <- barres(vars = variables_investments_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_investments_win_lose, hover = labels_win_lose))
save_plotly(investments_win_lose_US, width= 1100, height=320) 

(investments_fair_US <- barres(vars = "investments_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Green infrastructure program is fair"))
save_plotly(investments_fair_US, width= 1120, height=140)

(investments_support_US <- barres(vars = "investments_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Green infrastructure program"))
save_plotly(investments_support_US, width= 980, height=140)

labels_investments_funding <- c()
for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_funding_US <- barres(vars = variables_investments_funding, export_xls = export_xls, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
save_plotly(investments_funding_US, width= 560, height=260)

##### 9. Pref 3: Tax and dividend #####
labels_tax_transfers_effects <- c() 
for (v in variables_tax_transfers_effect) labels_tax_transfers_effects <- c(labels_tax_transfers_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_tax_transfers_effects[3] <- "reduce the use of fossil fuels<br>and greenhouse gas emissions"
labels_tax_transfers_effects[6] <- "have a large effect<br>on the U.S. economy and employment"
labels_tax_transfers_effects[5] <- "have a negative effect<br>on the U.S. economy and employment"
(tax_transfers_effect_US <- barres(vars = rev(variables_tax_transfers_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_tax_transfers_effects), hover=labels_agree))
save_plotly(tax_transfers_effect_US, width= 960, height=380) 

labels_tax_transfers_win_lose <- c()
for (v in variables_tax_transfers_win_lose) labels_tax_transfers_win_lose <- c(labels_tax_transfers_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_tax_transfers_win_lose[5] <- "Your household financially"
(tax_transfers_win_lose_US <- barres(vars = variables_tax_transfers_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_tax_transfers_win_lose, hover = labels_win_lose))
save_plotly(tax_transfers_win_lose_US, width= 1100, height=320) 

(tax_transfers_fair_US <- barres(vars = "tax_transfers_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers is fair"))
save_plotly(tax_transfers_fair_US, width= 1100, height=140)

(tax_transfers_support_US <- barres(vars = "tax_transfers_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers"))
save_plotly(tax_transfers_support_US, width= 980, height=140)

##### 6-8. Specific policies #####
labels_policies <- c("A ban on combustion-engine cars", "A green infrastructure program", "A carbon tax with cash transfers")
(policies_cost_effective_US <- barres(vars = paste(names_policies, "cost_effective", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_cost_effective_US, width= 1100, height=240)

(policies_large_effect_US <- barres(vars = paste(names_policies, "large_effect", sep="_"), export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, sort = F, labels=labels_policies))
save_plotly(policies_large_effect_US, width= 1100, height=240)

(policies_negative_effect_US <- barres(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_negative_effect_US, width= 1100, height=240)

(policies_fair_US <- barres(vars = paste(names_policies, "fair", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_fair_US, width= 1100, height=240)

(policies_win_lose_poor_US <- barres(vars = paste(names_policies, "win_lose_poor", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_win_lose_poor_US, width= 870, height=240)

(policies_win_lose_middle_US <- barres(vars = paste(names_policies, "win_lose_middle", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_win_lose_middle_US, width= 870, height=240)

(policies_win_lose_rich_US <- barres(vars = paste(names_policies, "win_lose_rich", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_win_lose_rich_US, width= 870, height=240)

(policies_win_lose_rural_US <- barres(vars = paste(names_policies, "win_lose_rural", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_win_lose_rural_US, width= 870, height=240)

(policies_win_lose_self_US <- barres(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_win_lose_self_US, width= 870, height=240)

(policies_support_US <- barres(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
save_plotly(policies_support_US, width= 1020, height=240)

(policies_all_support_US <- barres(vars = c("standard_public_transport_support", paste(names_policies, "support", sep="_")), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Ban on combustion cars where<br>public transport made available", labels_policies)))
save_plotly(policies_all_support_US, width= 790, height=270)

(standard_effects_US <- barres(vars = c("standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                               export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce CO2 emissions from cars")))
save_plotly(standard_effects_US, width= 1100, height=200)

(standard_all_US <- barres(vars = c("standard_fair", "standard_cost_effective", "standard_negative_effect", "standard_large_effect", "standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                           export_xls = export_xls, df = e, miss=F, labels=c("Be fair", "Costly way<br>to fight climate change", "Negative effect on US<br>economy and employment", "Large effect on US<br>economy and employment", "Reduce air pollution", "Reduce CO2 emissions from cars")))
save_plotly(standard_all_US, width= 1100, height=380)

(investments_effects_US <- barres(vars = c("investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F,rev = F, rev_color = T, 
                                  export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
save_plotly(investments_effects_US, width= 1150, height=240)

(investments_all_US <- barres(vars = c("investments_cost_effective", "investments_negative_effect", "investments_large_effect", "investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F, rev = F, rev_color = T, 
                              export_xls = export_xls, df = e, miss=F, labels=c("Costly way<br>to fight climate change", "Negative effect on US<br>economy and employment", "Large effect on US<br>economy and employment", "Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
save_plotly(investments_all_US, width= 1150, height=400)

(tax_transfers_effects_US <- barres(vars = c("tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F,rev = F, rev_color = T, 
                                    export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
save_plotly(tax_transfers_effects_US, width= 1100, height=280)

(tax_transfers_all_US <- barres(vars = c("tax_transfers_cost_effective", "tax_transfers_negative_effect", "tax_transfers_large_effect", "tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F, rev = F, rev_color = T, 
                                export_xls = export_xls, df = e, miss=F, labels=c("Costly way<br>to fight climate change", "Negative effect on US<br>economy and employment", "Large effect on US<br>economy and employment", "Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
save_plotly(tax_transfers_all_US, width= 1150, height=430)


##### 10. Pref on climate policies #####
labels_support <- c("Strongly oppose", "Somewhat oppose", "Indifferent", "Somewhat support", "Strongly support")
labels_policy <- c()
for (v in variables_policy) labels_policy <- c(labels_policy, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# labels_policy[5] <- "Subsidies for low-carbon technologies (renewables, CCS...)"
# labels_policy[6] <- "Financing clean energy in low-income countries" 
labels_policy[4] <- "Subsidies for low-carbon technologies (renewables, CCS...)"
labels_policy[5] <- "Financing clean energy in low-income countries" 
labels_policy[2] <- "National tax on fossil fuels (+$0.40/gallon)" 
(policy_US <- barres(vars = variables_policy, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_policy, hover=labels_support))
save_plotly(policy_US, width= 920, height=340)

labels_tax <- c()
for (v in variables_tax) labels_tax <- c(labels_tax, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_tax[7] <- "Funding environmental infrastructures (e.g. public transport)"
labels_tax[1] <- "Cash transfers to HH with no alternative to fossil fuels"
labels_tax[8] <- "Subsidizing low-carbon technologies, including renewables"
(tax_US <- barres(vars = variables_tax, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, showLegend=T, labels=labels_tax, hover=labels_support))
save_plotly(tax_US, width= 930, height=400) 

##### 11. WTP #####
(wtp_US <- barres(vars = rev(variables_wtp), export_xls = export_xls, df = e, miss=F, sort = F, labels=rev(c("WTP to limit global warming ($/year): 10", "30", "50", "100", "300", "500", "1000"))))
save_plotly(wtp_US, width= 680, height=340)

# (wtp_US <- barres(vars = "wtp", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="WTP to limit global warming ($/year)"))
# save_plotly(wtp_US, width= 1050, height=200)

(wtp_agg_US <- barres(vars = "wtp_agg", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
save_plotly(wtp_agg_US, width= 950, height=140)

# mar_old <- par()$mar
# cex_old <- par()$cex
# par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
# cdf_wtp_US <- Ecdf(e$wtp, weights = e$weight)
# plot(cdf_wtp_US$x, cdf_wtp_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
# title(ylab=expression("Proportion <= x"), xlab="WTP (in $/year)", line=2.3)
# grid() # TODO legend
# par(mar = mar_old, cex = cex_old)

(donation_US <- barres(vars = "donation", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="Donation to climate charity ($/year)"))
save_plotly(donation_US, width= 1050, height=200)

(donation_agg_US <- barres(vars = "donation_agg", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
save_plotly(donation_agg_US, width= 670, height=140)

mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
cdf_donation_US <- Ecdf(e$donation, weights = e$weight)
plot(cdf_donation_US$x, cdf_donation_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
title(ylab=expression("Proportion <= x"), xlab="Donation (in $/year)", line=2.3)
grid() # TODO legend
par(mar = mar_old, cex = cex_old)

##### 12. International burden-sharing #####

labels_scale <- c()
for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(scale_US <- barres(vars = variables_scale, export_xls = export_xls, df = e, rev = F, miss = T, showLegend=F, labels=labels_scale))
save_plotly(scale_US, width= 460, height=250) 

(should_fight_CC_US <- barres(vars = "should_fight_CC", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels=" U.S. should fight CC"))
save_plotly(should_fight_CC_US, width= 1075, height=140)

labels_if_other_do <- c()
for (v in variables_if_other_do) labels_if_other_do <- c(labels_if_other_do, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(if_other_do_US <- barres(vars = variables_if_other_do, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_if_other_do, hover=c("Much less", "Less", "About the same", "More", "Much more")))
save_plotly(if_other_do_US, width= 830, height=200) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_burden_sharing[4] <- "The richest countries should pay it all, <br> so that the poorest countries do not have to pay anything"
labels_burden_sharing[3] <- "Countries should pay in proportion to <br> their past emissions (from 1990 onwards)"
labels_burden_sharing[5] <- "The richest countries should pay even more <br> to help vulnerable countries face adverse consequences" 
(burden_sharing_US <- barres(vars = variables_burden_sharing, export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels=labels_burden_sharing))
save_plotly(burden_sharing_US, width= 1150, height=325) 

(global_assembly_support_US <- barres(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global democratic assembly<br>on climate change"))
save_plotly(global_assembly_support_US, width= 990, height=140)

(global_tax_support_US <- barres(vars = "global_tax_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on GHG<br> financing a global basic income"))
save_plotly(global_tax_support_US, width= 780, height=140)

(tax_1p_support_US <- barres(vars = "tax_1p_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))
save_plotly(tax_1p_support_US, width= 780, height=140)

labels_global_policies <- c("Global democratic assembly<br>on climate change", "Global tax on GHG<br> financing a global basic income", "Global tax on millionaires <br> to finance low-income countries")
(global_policies_US <- barres(vars = c("global_assembly_support", "global_tax_support", "tax_1p_support"), export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels=labels_global_policies))
save_plotly(global_policies_US, width= 800, height=250)

##### 13. Pref for bans vs. incentives #####

(will_insulate_US <- barres(vars = "will_insulate", export_xls = export_xls, df = e, miss=F, labels="Insulate or replace heating<br>over the next 5 years"))
save_plotly(will_insulate_US, width= 600, height=140)

(insulation_support_US <- barres(vars = "insulation_support", export_xls = export_xls, df = e, miss=F, labels="Mandatory insulation with subsidies"))
save_plotly(insulation_support_US, width= 1035, height=140)

(insulation_support_variant_US <- barres12(vars = "insulation_support", export_xls = export_xls, df = list(e[e$insulation_disruption_variant==T,], e[e$insulation_disruption_variant==F,]), orig="<br>Control", comp = "Priming: renovation cause disruption", miss=F, labels="Mandatory insulation with subsidies"))
save_plotly(insulation_support_variant_US, width= 1035, height=240) 

labels_obstacles_insulation <- c()
for (v in variables_obstacles_insulation) labels_obstacles_insulation <- c(labels_obstacles_insulation, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_obstacles_insulation[1] <- "The choice is not mine"
labels_obstacles_insulation[5] <- "My insulation and heating systems<br>are already satisfactory"
(obstacles_insulation_US <- barres(vars = variables_obstacles_insulation, export_xls = export_xls, df = e, error_margin=F, rev = F, miss = F, showLegend=F, labels=labels_obstacles_insulation, hover=labels_obstacles_insulation))
save_plotly(obstacles_insulation_US, width= 550, height=230) 

# (insulation_subsidies_support_US <- barres(vars = "insulation_subsidies_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for subsidies of insulation"))
# save_plotly(insulation_subsidies_support_US, width= 1030, height=170)

(insulation_mandatory_support_US <- barres(vars = "insulation", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for making insulation mandatory"))
save_plotly(insulation_mandatory_support_US, width= 1030, height=170)

# (insulation_support_US <- barres(vars = c("insulation_subsidies_support", "insulation_mandatory_support"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=c("Insulation subsidized", "...and mandatory")))
# save_plotly(insulation_support_US, width= 960, height=200)

labels_beef <- c()
for (v in variables_beef) labels_beef <- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(beef_US <- barres(vars = variables_beef, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels=labels_beef,showLegend=T, hover= labels_support))
save_plotly(beef_US, width= 900, height=240)  


##### 14. Trust, perceptions of institutions, etc. #####

(can_trust_people_US <- barres(vars = "can_trust_people", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust other people"))
save_plotly(can_trust_people_US, width= 1060, height=140)

(can_trust_govt_US <- barres(vars = "can_trust_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust the U.S. federal government <br> over the last decade to do what is right"))
save_plotly(can_trust_govt_US, width= 875, height=140)

(view_govt_US <- barres(vars = "view_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="View on government intervention"))
save_plotly(view_govt_US, width= 700, height=140)

(problem_inequality_US <- barres(vars = "problem_inequality", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Income inequality in the U.S."))
save_plotly(problem_inequality_US, width= 950, height=140)

(future_richness_US <- barres(vars = "future_richness", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="In 100 years, people in the world will be..."))
save_plotly(future_richness_US, width= 850, height=140)

##### 15. Political views #####

(interested_politics_US <- barres(vars = "interested_politics", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Interested in politics"))
save_plotly(interested_politics_US, width= 700, height=140)

(member_environmental_orga_US <- barres(vars = "member_environmental_orga", export_xls = export_xls, df = e, miss=F, labels="Member of an<br> environmental organization"))
save_plotly(member_environmental_orga_US, width= 622, height=140)

(relative_environmentalist_US <- barres(vars = "relative_environmentalist", export_xls = export_xls, df = e, miss=F, labels="Has an environmentalist relative"))
save_plotly(relative_environmentalist_US, width= 540, height=140)

(relative_environmentalist_US <- barres(vars = "relative_environmentalist", export_xls = export_xls, df = e, miss=F, labels="Has an environmentalist relative"))
save_plotly(relative_environmentalist_US, width= 540, height=140)

(vote_participation_US <- barres(vars = "vote_participation", export_xls = export_xls, df = e, miss=T, labels="Voted in 2020 election"))
save_plotly(vote_participation_US, width= 540, height=140) 

(vote_participation_2016_US <- barres(vars = "vote_participation_2016", export_xls = export_xls, df = e, miss=T, labels="Voted in 2016 election"))
save_plotly(vote_participation_2016_US, width= 540, height=140) 

(vote_US <- barres(vars = "vote", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for"))
save_plotly(vote_US, width= 650, height=140) 

(vote_2020 <- barres(vars = "vote_2020", export_xls = export_xls, df = e, rev_color = T, miss=T, fr = "PNR/no right", labels="In 2020, voted for"))
save_plotly(vote_2020, width= 610, height=140) 

(vote_2016_US <- barres(vars = "vote_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for"))
save_plotly(vote_2016_US, width= 650, height=140) 

(vote_all_US <- barres(vars = c("vote_non_voters", "vote_voters", "vote"), export_xls = export_xls, df = e, rev_color = T, miss=F, sort = F, labels=c("Non-voters", "Voters", "2020 vote: Voters and non-voters")))
save_plotly(vote_all_US, width= 600, height=250) # TODO make it work even if Jorgensen/Hawkins don't appear everywhere

(vote_voters_US <- barres(vars = "vote_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for (voters)"))
save_plotly(vote_voters_US, width= 650, height=140) #

(vote_non_voters_US <- barres(vars = "vote_non_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for (non-voters)"))
save_plotly(vote_non_voters_US, width= 650, height=140)

(vote_voters_2016_US <- barres(vars = "vote_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (voters)"))
save_plotly(vote_voters_2016_US, width= 650, height=140) 

(vote_non_voters_2016_US <- barres(vars = "vote_non_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (non-voters)"))
save_plotly(vote_non_voters_2016_US, width= 650, height=140)

(liberal_conservative_US <- barres(vars = "liberal_conservative", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="On economic policy matters, are you..."))
save_plotly(liberal_conservative_US, width= 950, height=140)

(political_affiliation_US <- barres(vars = "political_affiliation", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Political affiliation"))
save_plotly(political_affiliation_US, width= 800, height=140)

##### 16. Feedback #####

(survey_biased_US <- barres(vars = "survey_biased", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Survey biased"))
save_plotly(survey_biased_US, width= 810, height=140)

rquery.wordcloud(paste(e$comment_field, collapse=" \n "), max.words = 70)
rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = "survey", max.words = 70)
rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = "survey", colorPalette = "Blues", max.words = 70) # Spectral

##### Heterogeneity: function #####
plot_heterogeneity12 <- function(dfs = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", text_file = "_US_inc", export_xls = F) {
  (temp <- barres12(vars = "CC_anthropogenic", export_xls = export_xls, df = dfs, comp = comp, orig=orig, rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
  save_plotly(temp, filename = paste0("CC_anthropogenic", text_file), width= 870, height=220)
  
  (temp <- barres12(vars = "CC_affects_self", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="CC negatively affects personal life"))
  save_plotly(temp, filename = paste0("CC_affects_self", text_file), width= 800, height=220)
  
  (temp <- barres12(vars = "net_zero_feasible", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions")) #  while maintaining<br>satisfactory standards of living in the U.S.
  save_plotly(temp, filename = paste0("net_zero_feasible", text_file), width= 810, height=220)
  
  (temp <- barres12(vars = "CC_will_end", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Likely to halt CC by the end of the century"))
  save_plotly(temp, filename = paste0("CC_will_end", text_file), width= 970, height=220)
  
  (temp <- barres12(vars = "future_richness",rev = F, rev_color = T,  export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="In 100 years, people in the world will be..."))
  save_plotly(temp, filename = paste0("future_richness", text_file), width= 940, height=220)
  
  (temp <- barres12(vars = "effect_halt_CC_lifestyle",rev = F, rev_color = T,  export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Negative effects of ambitious policies on lifestyle"))
  save_plotly(temp, filename = paste0("effect_halt_CC_lifestyle", text_file), width= 1100, height=220)
  
  (temp <- barres12(vars = "willing_limit_beef", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Willing to limit beef consumption"))
  save_plotly(temp, filename = paste0("willing_limit_beef", text_file), width= 770, height=220)
  
  (temp <- barres12(vars = "willing_limit_flying", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Willing to limit flying"))
  save_plotly(temp, filename = paste0("willing_limit_flying", text_file), width= 680, height=220)
  
  (temp <- barres12(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
  save_plotly(temp, filename = paste0("policies_negative_effect", text_file), width= 1100, height=380)
  
  (temp <- barres12(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
  save_plotly(temp, filename = paste0("policies_win_lose_self", text_file), width= 870, height=380)
  
  (temp <- barres12(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
  save_plotly(temp, filename = paste0("policies_support", text_file), width= 1020, height=380)
  
  (temp <- barres12(vars = "donation_agg", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
  save_plotly(temp, filename = paste0("donation_agg", text_file), width= 750, height=220)
  
  (temp <- barres12(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="Global democratic assembly<br>on climate change"))
  save_plotly(temp, filename = paste0("global_assembly_support", text_file), width= 990, height=220)
  
  (temp <- barres12(vars = "wtp", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
  save_plotly(temp, filename = paste0("wtp", text_file), width= 950, height=220)
  
  # (temp <- barres12(vars = "global_tax_support", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="Global tax on GHG<br> financing a global basic income"))
  # save_plotly(temp, filename = paste0("global_tax_support", text_file), width= 780, height=220)
  # 
  # (temp <- barres12(vars = "tax_1p_support", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))
  # save_plotly(temp, filename = paste0("tax_1p_support", text_file), width= 780, height=220)
}

plot_heterogeneityN <- function(df = us, along = "vote", text_file = "_US_vote", export_xls = F) {
  levels <- Levels(df[[along]])
  try({(temp <- barresN(vars = "CC_anthropogenic", export_xls = export_xls, df = df, along = along, rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
    save_plotly(temp, filename = paste0("CC_anthropogenic", text_file), width= 870, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "CC_affects_self", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss = F, labels="CC negatively affects personal life"))
    save_plotly(temp, filename = paste0("CC_affects_self", text_file), width= 800, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "net_zero_feasible", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions")) #  while maintaining<br>satisfactory standards of living in the U.S.
    save_plotly(temp, filename = paste0("net_zero_feasible", text_file), width= 810, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "CC_will_end", export_xls = export_xls, df = df, along = along, miss = F, labels="Likely to halt CC by the end of the century"))
    save_plotly(temp, filename = paste0("CC_will_end", text_file), width= 970, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "future_richness",rev = F, rev_color = T,  export_xls = export_xls, df = df, along = along, miss=F, labels="In 100 years, people in the world will be..."))
    save_plotly(temp, filename = paste0("future_richness", text_file), width= 940, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "effect_halt_CC_lifestyle",rev = F, rev_color = T,  export_xls = export_xls, df = df, along = along, miss = F, labels="Negative effects of ambitious policies on lifestyle"))
    save_plotly(temp, filename = paste0("effect_halt_CC_lifestyle", text_file), width= 1100, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "willing_limit_beef", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss = F, labels="Willing to limit beef consumption"))
    save_plotly(temp, filename = paste0("willing_limit_beef", text_file), width= 770, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "willing_limit_flying", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss = F, labels="Willing to limit flying"))
    save_plotly(temp, filename = paste0("willing_limit_flying", text_file), width= 680, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
    save_plotly(temp, filename = paste0("policies_negative_effect", text_file), width= 1100, height=fig_height(3*length(levels)))})

  try({(temp <- barresN(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
    save_plotly(temp, filename = paste0("policies_win_lose_self", text_file), width= 870, height=fig_height(3*length(levels)))})

  try({(temp <- barresN(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
    save_plotly(temp, filename = paste0("policies_support", text_file), width= 1020, height=fig_height(3*length(levels)))})

  try({(temp <- barresN(vars = "donation_agg", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
    save_plotly(temp, filename = paste0("donation_agg", text_file), width= 750, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels="Global democratic assembly<br>on climate change"))
    save_plotly(temp, filename = paste0("global_assembly_support", text_file), width= 990, height=fig_height(1*length(levels)))})

  try({(temp <- barresN(vars = "wtp", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming"))
    save_plotly(temp, filename = paste0("wtp", text_file), width= 950, height=fig_height(1*length(levels)))})

  # try({(temp <- barresN(vars = "global_tax_support", export_xls = export_xls, df = df, along = along, miss=F, labels="Global tax on GHG<br> financing a global basic income"))},
  #   save_plotly(temp, filename = paste0("global_tax_support", text_file), width= 780, height=fig_height(1*length(levels)))})
  #
  # try({(temp <- barresN(vars = "tax_1p_support", export_xls = export_xls, df = df, along = along, miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))},
  #   save_plotly(temp, filename = paste0("tax_1p_support", text_file), width= 780, height=fig_height(1*length(levels)))})
}


##### Heterogeneity #####
# By income
plot_heterogeneity12(dfs = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), orig="<br>(Bottom 50%)", comp = "(Top 50%)", text_file = "_US_inc", export_xls = export_xls)
# By vote
plot_heterogeneity12(dfs = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", text_file = "_US_pol", export_xls = export_xls)
plot_heterogeneityN(along = "vote3", text_file = "_US_vote", export_xls = export_xls)
# By rural/urban
plot_heterogeneity12(dfs = list(e[e$core_metropolitan==T,], e[e$core_metropolitan==F,]), orig="<br>(Core metro)", comp = "(Non-core metro)", text_file = "_US_urb", export_xls = export_xls)

## Other
(wtp_US_anthropogenic <- barres12(vars = "wtp", export_xls = export_xls, df = list(e[e$CC_anthropogenic == "Most",], e[e$CC_anthropogenic <= 0,]), comp = "<br>(CC not mainly anthropogenic)", orig="<br>(CC anthropogenic)", miss=F, labels="WTP to limit global warming ($/year)"))
save_plotly(wtp_US_anthropogenic, width= 830, height=220)


##### Deprecated: heterogeneity #####
## By income
# (CC_anthropogenic_US_inc <- barres12(vars = "CC_anthropogenic", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", 
#                                      rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
# save_plotly(CC_anthropogenic_US_inc, width= 870, height=220)
# 
# (CC_affects_self_US_inc <- barres12(vars = "CC_affects_self", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss = F, labels="CC negatively affects personal life"))
# save_plotly(CC_affects_self_US_inc, width= 800, height=220)
# 
# (net_zero_feasible_US_inc <- barres12(vars = "net_zero_feasible", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions")) #  while maintaining<br>satisfactory standards of living in the U.S.
# save_plotly(net_zero_feasible_US_inc, width= 810, height=220)
# 
# (CC_will_end_US_inc <- barres12(vars = "CC_will_end", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss = F, labels="Likely to halt CC by the end of the century"))
# save_plotly(CC_will_end_US_inc, width= 970, height=220)
# 
# (future_richness_US_inc <- barres12(vars = "future_richness",rev = F, rev_color = T,  export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, labels="In 100 years, people in the world will be..."))
# save_plotly(future_richness_US_inc, width= 940, height=220)
# 
# (effect_halt_CC_lifestyle_US_inc <- barres12(vars = "effect_halt_CC_lifestyle",rev = F, rev_color = T,  export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss = F, labels="Negative effects of ambitious policies on lifestyle"))
# save_plotly(effect_halt_CC_lifestyle_US_inc, width= 1100, height=220)
# 
# (willing_limit_beef_US_inc <- barres12(vars = "willing_limit_beef", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss = F, labels="Willing to limit beef consumption"))
# save_plotly(willing_limit_beef_US_inc, width= 770, height=220)
# 
# (willing_limit_flying_US_inc <- barres12(vars = "willing_limit_flying", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss = F, labels="Willing to limit flying"))
# save_plotly(willing_limit_flying_US_inc, width= 680, height=220)
# 
# (policies_negative_effect_US_inc <- barres12(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, sort = F, labels=labels_policies))
# save_plotly(policies_negative_effect_US_inc, width= 1100, height=380)
# 
# (policies_win_lose_self_US_inc <- barres12(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, sort = F, labels=labels_policies))
# save_plotly(policies_win_lose_self_US_inc, width= 870, height=380)
# 
# (policies_support_US_inc <- barres12(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, sort = F, labels=labels_policies))
# save_plotly(policies_support_US_inc, width= 1020, height=380)
# 
# (donation_agg_US_inc <- barres12(vars = "donation_agg", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
# save_plotly(donation_agg_US_inc, width= 750, height=220)
# 
# (wtp_agg_US_inc <- barres12(vars = "wtp_agg", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
# save_plotly(wtp_agg_US_inc, width= 950, height=220)
# 
# (global_assembly_support_US_inc <- barres12(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, labels="Global democratic assembly<br>on climate change"))
# save_plotly(global_assembly_support_US_inc, width= 990, height=220)
# 
# # (global_tax_support_US_inc <- barres12(vars = "global_tax_support", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, labels="Global tax on GHG<br> financing a global basic income"))
# # save_plotly(global_tax_support_US_inc, width= 780, height=220)
# # 
# # (tax_1p_support_US_inc <- barres12(vars = "tax_1p_support", export_xls = export_xls, df = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))
# # save_plotly(tax_1p_support_US_inc, width= 780, height=220)
#  
# ## By vote
# (CC_anthropogenic_US_pol <- barres12(vars = "CC_anthropogenic", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", 
#                                      rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
# save_plotly(CC_anthropogenic_US_pol, width= 870, height=220)
# 
# (CC_affects_self_US_pol <- barres12(vars = "CC_affects_self", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss = F, labels="CC negatively affects personal life"))
# save_plotly(CC_affects_self_US_pol, width= 800, height=220)
# 
# (net_zero_feasible_US_pol <- barres12(vars = "net_zero_feasible", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions")) # <br>while maintaining satisfactory<br>standards of living in the U.S.
# save_plotly(net_zero_feasible_US_pol, width= 810, height=220)
# 
# (CC_will_end_US_pol <- barres12(vars = "CC_will_end", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss = F, labels="Likely to halt CC by the end of the century"))
# save_plotly(CC_will_end_US_pol, width= 970, height=220)
# 
# (future_richness_US_pol <- barres12(vars = "future_richness", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, labels="In 100 years, people in the world will be..."))
# save_plotly(future_richness_US_pol, width= 940, height=220)
# 
# (effect_halt_CC_lifestyle_US_pol <- barres12(vars = "effect_halt_CC_lifestyle", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss = F, labels="Negative effects of ambitious policies on lifestyle"))
# save_plotly(effect_halt_CC_lifestyle_US_pol, width= 1100, height=220)
# 
# (willing_limit_beef_US_pol <- barres12(vars = "willing_limit_beef", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss = F, labels="Willing to limit beef consumption"))
# save_plotly(willing_limit_beef_US_pol, width= 770, height=220)
# 
# (willing_limit_flying_US_pol <- barres12(vars = "willing_limit_flying", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss = F, labels="Willing to limit flying"))
# save_plotly(willing_limit_flying_US_pol, width= 680, height=220)
# 
# (policies_negative_effect_US_pol <- barres12(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, sort = F, labels=labels_policies))
# save_plotly(policies_negative_effect_US_pol, width= 1100, height=380)
# 
# (policies_win_lose_self_US_pol <- barres12(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, sort = F, labels=labels_policies))
# save_plotly(policies_win_lose_self_US_pol, width= 870, height=380)
# 
# (policies_support_US_pol <- barres12(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, sort = F, labels=labels_policies))
# save_plotly(policies_support_US_pol, width= 1020, height=380) # TODO: sort=F respected
# 
# (donation_agg_US_pol <- barres12(vars = "donation_agg", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
# save_plotly(donation_agg_US_pol, width= 750, height=220)
# 
# (wtp_agg_US_pol <- barres12(vars = "wtp_agg", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
# save_plotly(wtp_agg_US_pol, width= 950, height=220)
# 
# (global_assembly_support_US_pol <- barres12(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, labels="Global democratic assembly<br>on climate change"))
# save_plotly(global_assembly_support_US_pol, width= 990, height=220)
# 
# # (global_tax_support_US_pol <- barres12(vars = "global_tax_support", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, labels="Global tax on GHG<br> financing a global basic income"))
# # save_plotly(global_tax_support_US_pol, width= 780, height=220)
# # 
# # (tax_1p_support_US_pol <- barres12(vars = "tax_1p_support", export_xls = export_xls, df = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))
# # save_plotly(tax_1p_support_US_pol, width= 780, height=220)
#
# (wtp_agg_US_anthropogenic <- barres12(vars = "wtp_agg", export_xls = export_xls, df = list(e[e$CC_anthropogenic == "Most",], e[e$CC_anthropogenic <= 0,]), comp = "<br>(CC not mainly anthropogenic)", orig="<br>(CC anthropogenic)", miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
# save_plotly(wtp_agg_US_anthropogenic, width= 830, height=220)

##### Correlograms #####
correlogram("_fair")
correlogram("_cost_effective")
correlogram(vars = c(paste(names_policies, "support", sep="_"), "policies_support"))
correlogram("_win_lose_self")
correlogram("_win_lose_poor")
correlogram("_win_lose_rich")
correlogram("_win_lose_middle")
correlogram("_win_lose_rural")
correlogram("_large_effect")
correlogram("_negative_effect")
correlogram("standard_")
correlogram("investments_")
correlogram("tax_transfers_")
correlogram("knowledge")

##### Tables treatment effects #####
## Generate tables
label_treat_wave <- c("Both treatments", "Climate treatment only", "Policy treatment only", "wave: Pilot 2")

desc_table(dep_vars = c("CC_anthropogenic > 0", "CC_impacts_extinction > 0", "donation", "should_fight_CC > 0", "willing_limit_driving > 0"), filename = "US_1",
           dep.var.labels = c("CC caused by humans", "CC likely to cause extinction", "Donation (in \\$)", "US should fight CC", "Willing to limit driving"),
           data = e, keep = c("treatment"), indep_vars = c(variables_main_controls_pilot3, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
)

desc_table(dep_vars = c("tax_transfers_support > 0", "investments_support > 0", "standard_support > 0", "policies_support > 0"), filename = "US_2",
           dep.var.labels = c("Carbon tax with transfers", "Green Infrastructure Program", "Ban on combustion-engine cars", "Average over 3 policies"),
           dep.var.caption = c("Support"), data = e, keep = c("treatment"), indep_vars = c(variables_main_controls_pilot3, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
)

desc_table(dep_vars = c("policies_fair > 0", "policies_self > 0", "policies_poor > 0", "policies_large_effect > 0", "policies_negative_effect > 0"), filename = "US_3",
           dep.var.labels =  c("Fair", "HH would win", "Poor would win", "Large economic effect", "Negative economic effect"),
           data = e, keep = c("treatment"), indep_vars = c(variables_main_controls_pilot3, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
)

# PNR
for (v in names(e)) if (mean(is.na(e[[v]]) | grepl("PNR", e[[v]])) > 0.4) print(paste(round(mean(is.pnr(e[[v]]) | grepl("PNR", e[[v]])), 3), v))
for (v in names(e)) if (mean(is.na(e[[v]]) | grepl("PNR", e[[v]])) %between% c(.05, .4)) print(paste(round(mean(is.pnr(e[[v]]) | grepl("PNR", e[[v]])), 3), v))
for (v in names(e)) if (mean(is.na(e[[v]]) | grepl("PNR", e[[v]])) %between% c(.03, .05)) print(paste(round(mean(is.pnr(e[[v]]) | grepl("PNR", e[[v]])), 3), v))
for (v in names(e)) if (mean(is.na(e[[v]]) | grepl("PNR", e[[v]])) %between% c(.01, .03)) print(paste(round(mean(is.pnr(e[[v]]) | grepl("PNR", e[[v]])), 3), v))
for (v in names(e)) if (mean(is.na(e[[v]]) | grepl("PNR", e[[v]])) < .01) print(paste(round(mean(is.pnr(e[[v]]) | grepl("PNR", e[[v]])), 3), v))

e <- us

# TODO
#  correlation CC_problem / anthropogenic / other knowledge
#  !Denmark: list municipality; lib-cons => left-right; no race; New York - Toronto => 700 km from Copenhagen to Stockholm (plane>car>train) / 800 km Bordeaux - Nice / 500 km Paris - London / 800 km Munich - Hambourg / 1000 km Munich - Copenhagen (mais seulement avec 1 ou 2 passagers pour ces deux derniers !)
#  !Indonesia, Japan, South Africa
# Questions: 
#  merge heating > 200? gas > 175? => not
# Ana: traduction
datasummary(CC_problem + as.numeric(CC_anthropogenic) ~ vote3 * Mean, e)
datasummary(vote3 ~ (CO2_emission + CO2_emission_heating + CO2_emission_gas + flights_agg) * Mean, e)
datasummary((CO2_emission < 13.7) + (CO2_emission %between% c(13.7, 21.5)) + (CO2_emission >= 21.5) ~ (policies_support + tax_transfers_support + CC_problem + CC_anthropogenic) * Mean, e)
modelplot(lm(CC_dynamic == 'Yes' ~ treatment, data = e))
# Hypothesis: because of lack of information, people are too optimistic, find CC easy to solve
# TODO: heterogenous treatment Red/Dem; maps; 
# TODO: Pessimistic w.r.t. future more or less climate friendly?
# TODO: support, index_knowledge ~ rural/urban + income + vote + gender + age + index_affected + index_knowledge
# Interpretation: people lack of info, are too optimistic or think it's too easy to solve

# knowledge_all <- cbind(e$knowledge_CC, knowledge)
# names(knowledge_all) <- c("Knowledge", "GhG", "Activities",  "Anthropogenic", "Exists", "Target", "Region")
# corrc <- cor(knowledge_all, use="complete.obs")
# p.matc <- cor.mtest(knowledge_all)
# corrplot(corrc, method='color', p.mat = p.matc, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'

# R: weights all countries, TODOs, China
# Ana: Spanish translation, Japanese, Indonesian revision, South African voice, Zulu translation.