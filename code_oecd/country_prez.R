# # Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F

# TODO: vote, region, flight, size of town, footprint region, etc., standard/investment/tax _effect_, donation
render_figures_tables_country <- function(data, country, on_control = T, export_xls = F, folder_country = F, name_country = T, figures = T, tables = T) {
  print(country)
  start <- Sys.time()
  if (!(missing(data))) e <- data
  else e <- db[[country]] # db[db$country==country,]
  
  if (on_control) e <- e[e$treatment=="None",]
  
  if (folder_country) folder <- paste0('../figures/', country, '/')
  else folder <- '../figures/'
  replacement_text <- ifelse(name_country, paste0("_", toupper(country)), "")
  
  save_plotly_new_filename <- function(plot, filename = NULL, width = dev.size('px')[1], height = dev.size('px')[2], method='orca', trim = T) {
    if (missing(filename)) filename <- sub("_US", replacement_text, deparse(substitute(plot)))
    return(save_plotly(plot, filename = filename, folder = folder, width = width, height = height, method=method, trim = trim))
  }

  if (figures) {
    ##### Pre-treatment ## #####
    
    # ##### 1. Demographics ##### TODO: comp, i.e. weights
    labels_comp <- c("Sample: non-weighted", "Sample: weighted", "Population")
    label_great_deal <- c("Not at all"," A little","Moderately","A lot","A great deal")
    labels_agree <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
    # # data_urbanity <- cbind(dataKN("urbanity", data=e, miss=F, weights = F), dataKN("urbanity", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975)) # TODO: find official freq for these ones and plot figures, also on polluting sector
    # # data_education <- cbind(dataKN("education", data=e, miss=F, weights = F), dataKN("education", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_wealth <- cbind(dataKN("wealth", data=e, miss=F, weights = F), dataKN("wealth", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_occupation <- cbind(dataKN("occupation", data=e, miss=F, weights = F), dataKN("occupation", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_employment_agg <- cbind(dataKN("employment_agg", data=e, miss=F, weights = F), dataKN("employment_agg", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_marital_status <- cbind(dataKN("marital_status", data=e, miss=F, weights = F), dataKN("marital_status", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_Nb_children <- cbind(dataKN("Nb_children", data=e, miss=F, weights = F), dataKN("Nb_children", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_HH_size <- cbind(dataKN("HH_size", data=e, miss=F, weights = F), dataKN("HH_size", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # data_home <- cbind(dataKN("home", data=e, miss=F, weights = F), dataKN("home", data=e, miss=F, weights = T), c(0.5075, 0, 0.4975))
    # # dataKN("vote_2020", data=e, miss=F, weights = F, return="legend")

    try({ data_gender <- cbind(dataKN("gender_factor", data=e, miss=F, weights = F), dataKN("gender_factor", data=e, miss=F, weights = T), pop_freq[[country]]$gender) # TODO: remove 0 if there is not "Other" in data
    legend_gender <- dataKN("gender_factor", data=e, miss=F, return="legend") 
      if (all(data_gender[2,])<=0.000001) {
        data_gender <- data_gender[c(1,3),]
        legend_gender <- legend_gender[c(1,3)]      }
      (gender_US_comp <- barres(data = data_gender, export_xls = export_xls, df = e, miss = F, sort = F, labels=labels_comp, legend = legend_gender))
      save_plotly_new_filename(gender_US_comp, width= 470, height=240)})

    try({
      data_age <- cbind(dataKN("age_quota", data=e[e$age_quota!="Below 18",], miss=F, weights = F), dataKN("age_quota", data=e[e$age_quota!="Below 18",], miss=F, weights = T), pop_freq[[country]]$age_quota)
      (age_US_comp <- barres(data = data_age, export_xls = export_xls, df = e, miss=F, rev = F, sort = F, labels=labels_comp, legend = dataKN("age_quota", data=e, miss=F, return="legend")[1:5]))
      save_plotly_new_filename(age_US_comp, width= 560, height=240) })

    try({ data_region <- cbind(dataKN("region", data=e, miss=F, weights = F), dataKN("region", data=e, miss=F, weights = T), pop_freq[[country]]$region)
      (region_US_comp <- barres(data = data_region, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_comp, legend = dataKN("region", data=e, miss=F, return="legend")))
      save_plotly_new_filename(region_US_comp, width= 560, height=240)})

    try({data_core_metropolitan <- cbind(dataKN("core_metropolitan", data=e, miss=F, weights = F), dataKN("core_metropolitan", data=e, miss=F, weights = T), pop_freq[[country]]$core_metropolitan[2])
      (core_metropolitan_US_comp <- barres(data = data_core_metropolitan, export_xls = export_xls, df = e, sort = F, miss=F, rev_color = T, rev = F, labels=labels_comp, showLegend = F, legend="Core metropolitan"))
      save_plotly_new_filename(core_metropolitan_US_comp, width= 660, height=240)})

    try({data_race <- cbind(dataKN("race", data=e, miss=F, weights = F), dataKN("race", data=e, miss=F, weights = T), pop_freq[[country]]$race)
      (race_US_comp <- barres(data = data_race, export_xls = export_xls, df = e, miss=F, rev = F, sort = F, labels=labels_comp, legend = dataKN("race", data=e, miss=F, return="legend")))
      save_plotly_new_filename(race_US_comp, width= 540, height=240)})

    try({data_income <- cbind(dataKN("income", data=e, miss=F, weights = F), dataKN("income", data=e, miss=F, weights = T), pop_freq[[country]]$income)
      (income_US_comp <- barres(data = data_income, export_xls = export_xls, df = e, miss=F, rev_color = T, sort = F, rev = F, labels=labels_comp, legend = dataKN("income", data=e, miss=F, return="legend")))
      save_plotly_new_filename(income_US_comp, width= 510, height=240)}) # 35/70/120

    try({data_vote <- cbind(dataKN("vote_2020", data=e, miss=T, weights = F), dataKN("vote_2020", data=e, miss=T, weights = T), pop_freq[[country]]$vote_2020)
      (vote_US_comp <- barres(data = data_vote, export_xls = export_xls, df = e, miss=T, sort = F, labels=labels_comp, legend = dataKN("vote_2020", data=e, miss=T, return="legend")))
      save_plotly_new_filename(vote_US_comp, width= 550, height=240)})
    # 
    try({(gender_US <- barres(vars = "gender_factor", export_xls = export_xls, df = e, miss = F, labels="Gender"))
      save_plotly_new_filename(gender_US, width= 470, height=140)})
    
    try({(age_quota_US <- barres(vars = "age_quota", export_xls = export_xls, df = e[e$age_quota!="Below 18",], miss=F, rev = F, labels="Age"))
      save_plotly_new_filename(age_quota_US, width= 500, height=140) })
    
    try({(region_US <- barres(vars = "region", export_xls = export_xls, df = e, miss=F, labels="Region"))
      save_plotly_new_filename(region_US, width= 560, height=140)})
    
    try({(urbanity_US <- barres(vars = "urbanity", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Size of town"))
      save_plotly_new_filename(urbanity_US, width= 660, height=140)})
    
    # try({(race_US <- barres(vars = variables_race[c(1:4)], export_xls = export_xls, df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
    #   save_plotly_new_filename(race_US, width= 340, height=240)}) # TODO
    
    try({(race_agg_US <- barres(vars = "race", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Size of town"))
      save_plotly_new_filename(race_agg_US, width= 500, height=140)})
    
    try({(income_US <- barres(vars = "income", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
      save_plotly_new_filename(income_US, width= 510, height=140)}) # 35/70/120 TODO
    
    try({(wealth_US <- barres(vars = "wealth", export_xls = export_xls, df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
      save_plotly_new_filename(wealth_US, width= 480, height=140)})
    
    try({(education_US <- barres(vars = "education", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
      save_plotly_new_filename(education_US, width= 1080, height=140) })
    
    try({(employment_status_US <- barres(vars = "employment_agg", export_xls = export_xls, df = e, miss=F, labels="What is your employment status?"))
      save_plotly_new_filename(employment_status_US, width= 630, height=140)})
    
    try({(occupation_US <- barres(vars = "occupation", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Main occupation"))
      save_plotly_new_filename(occupation_US, width= 610, height=140)})
    
    #try({(sector_US <- barres(vars = "sector", export_xls = export_xls, df = e, miss=F, labels="What is your primary line of business?"))
    #  save_plotly_new_filename(sector_US, width= 630, height=140)})
    
    try({(polluting_sector_US <- barres(vars = "polluting_sector", export_xls = export_xls, df = e, miss=F, showLegend = F, rev_color = T, rev = F, labels="Current (or last) job is in a polluting sector."))
      save_plotly_new_filename(polluting_sector_US, width= 600, height=140)})
    
    try({(hit_by_covid_US <- barres(vars = "hit_by_covid", export_xls = export_xls, df = e, miss=F, labels="Hit by covid"))
      save_plotly_new_filename(hit_by_covid_US, width= 420, height=140)})
    
    labels_home <- c()
    for (v in variables_home) labels_home <- c(labels_home, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(home_US <- barres(vars = variables_home, export_xls = export_xls, df = e, rev = F, miss = F, showLegend=F, labels=labels_home, hover=labels_home))
      save_plotly_new_filename(home_US, width= 575, height=320) })
    
    try({(couple_US <- barres(vars = "couple", export_xls = export_xls, df = e, miss=F, labels="Do you live with your partner?"))
      save_plotly_new_filename(couple_US, width= 630, height=140)})
    
    try({(marital_status_US <- barres(vars = "marital_status", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="What is your marital status?"))
      save_plotly_new_filename(marital_status_US, width= 830, height=175)})
    
    try({(nb_children_US <- barres(vars = "Nb_children", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Number of children", legend = c(0:3, "4 or more")))
      save_plotly_new_filename(nb_children_US, width= 470, height=140)})
    
    try({(hh_size_US <- barres(vars = "HH_size", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F,labels="People in household"))
      save_plotly_new_filename(hh_size_US, width= 470, height=140)})
    
    ##### 2. HH composition and energy characteristics #####
    
    try({(heating_US <- barres(vars = "heating", export_xls = export_xls, df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
      save_plotly_new_filename(heating_US, width= 525, height=140)})
    
    try({(heating_expenses_US <- barres(vars = "heating_expenses", export_xls = export_xls, df = e, miss=T, fr="Included", labels="Monthly heating expenses", rev = F))
      save_plotly_new_filename(heating_expenses_US, width= 950, height=140)}) # TODO
    
    try({(insulation_US <- barres(vars = "insulation", export_xls = export_xls, df = e, miss=F, rev_color = T, labels="Quality of insulation", rev = F))
      save_plotly_new_filename(insulation_US, width= 600, height=140)})
    
    try({(gas_expenses_US <- barres(vars = "gas_expenses", export_xls = export_xls, df = e, miss=F, labels="Monthly gas expenses", rev = F))
      save_plotly_new_filename(gas_expenses_US, width= 800, height=140)}) # TODO
    
    try({(flights_3y_US <- barres(vars = "flights_agg", export_xls = export_xls, df = e, miss=F, rev = F, labels="Round-trip flights between 2017 and 2019"))
      save_plotly_new_filename(flights_3y_US, width= 800, height=140)})
    
    try({(frequency_beef_US <- barres(vars = "frequency_beef", export_xls = export_xls, df = e, miss=F, rev = F, labels="How often do you eat beef?"))
      save_plotly_new_filename(frequency_beef_US, width= 720, height=140)})
    
    variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
    labels_transport <- c("Work", "Shopping", "Leisure")
    try({(transport_US <- barres(vars = variables_transport_graph, export_xls = export_xls, df = e, rev = F, miss = F,rev_color = T,  labels=labels_transport))
      save_plotly_new_filename(transport_US, width= 850, height=275) })
    
    try({(availability_transport_US <- barres(vars = "availability_transport", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Quality and availability of public transport near your home"))
      save_plotly_new_filename(availability_transport_US, width= 870, height=140)})
    
    # rquery.wordcloud(paste(e$CC_field, collapse=" \n "), max.words = 70) # TODO: update, save
    # rquery.wordcloud(paste(e$CC_field, collapse=" \n "), max.words = 70, excludeWords = c("climate", "change", "government"))
    
    ##### POST-TREATMENT #####
    
    ##### 3. Treatment feedback: local climate #####
    try({(watched_climate_US <- barres(vars = "watched_climate", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (local)"))
      save_plotly_new_filename(watched_climate_US, width= 900, height=140)})
    
    # try({(know_treatment_climate_US <- barres(vars = c("know_temperature_2100", "know_frequence_heatwaves"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Knowledge climate video"))
    #   save_plotly_new_filename(know_treatment_climate_US, width= 580, height=140)})
    
    try({(know_treatment_climate_US <- barres(vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Score knowledge climate video"))
      save_plotly_new_filename(know_treatment_climate_US, width= 580, height=140)})
    
    # try({(know_treatment_climate_watched_US <- barres(vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e[e$watched_climate=='Yes',], miss=F, labels="Score knowledge climate video"))
    #   save_plotly_new_filename(know_treatment_climate_watched_US, width= 580, height=140)})
    
    ##### 4. Treatment feedback: local policy #####
    try({(watched_policy_US <- barres(vars = "watched_policy", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (policy)"))
      save_plotly_new_filename(watched_policy_US, width= 900, height=140)})
    
    try({(know_treatment_policy_US <- barres(vars = "know_treatment_policy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="Score knowledge policy video"))
      save_plotly_new_filename(know_treatment_policy_US, width= 580, height=140)})
    
    # # labels_know_treatment <- c("Knows CC would cause 70 days of<br>heatwaves per year in the US by 2100", "Knows temperature would be +8°F<br>by 2100 with current trend emissions", "Knows first policy presented is<br>ban on combustion-engine cars", "Knows green infrastructure program<br>described would be debt-funded")
    # labels_know_treatment <- c("Knows CC would cause 70 days of<br>heatwaves per year in the US by 2100", "Knows temperature would be +8 Fahrenheit<br>by 2100 with current trend emissions", "Knows first policy presented is<br>ban on combustion-engine cars", "Knows green infrastructure program<br>described would be debt-funded")
    # try({(know_treatment_US <- barres(vars = paste0(variables_know_treatment, "_correct"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=labels_know_treatment))
    #   save_plotly_new_filename(know_treatment_US, width= 830, height=280)}) # TODO
    
    ##### 5. Climate knowledge #####
    
    try({(CC_talks_US <- barres(vars = "CC_talks", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="How often do you talk about climate change?"))
      save_plotly_new_filename(CC_talks_US, width= 760, height=140)})
    
    try({(CC_real_US <- barres(vars = "CC_real", export_xls = export_xls, df = e, miss=F, labels="Climate change real?"))
      save_plotly_new_filename(CC_real_US, width= 540, height=140)})
    
    try({(CC_anthropogenic_US <- barres(vars = "CC_anthropogenic", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Part of climate change anthropogenic"))
      save_plotly_new_filename(CC_anthropogenic_US, width= 660, height=140)})
    
    try({(CC_anthropogenic_non_deniers_US <- barres(vars = "CC_anthropogenic", rev = F, rev_color = T, export_xls = export_xls, df = e[e$CC_real!="No",], miss=F, labels="Part of climate change anthropogenic"))
      save_plotly_new_filename(CC_anthropogenic_non_deniers_US, width= 660, height=140)})
    
    try({(CC_problem_US <- barres(vars = "CC_problem", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Climate change is an important problem."))
      save_plotly_new_filename(CC_problem_US, width= 1125, height=140) })
    
    try({(CC_knowledgeable_US <- barres(vars = "CC_knowledgeable", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="How knowledgeable about climate change"))
      save_plotly_new_filename(CC_knowledgeable_US, width= 800, height=140)})
    
    try({(CC_dynamic_US <- barres(vars = "CC_dynamic", export_xls = export_xls, df = e, rev=F, miss=F, labels="Cutting GHG emissions by half <br> sufficient to stop rise in temperatures"))
      save_plotly_new_filename(CC_dynamic_US, width= 600, height=140)})
    
    try({(GHG_US <- barres(vars = c("GHG_CO2", "GHG_methane", "GHG_H2", "GHG_particulates"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=c("CO<sub>2</sub>", "Methane", "Hydrogen", "Particulates")))
      save_plotly_new_filename(GHG_US, width= 270, height=220)})
    
    try({(score_GHG_US <- barres(vars = "score_GHG", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="Knowledge score on GHG"))
      save_plotly_new_filename(score_GHG_US, width= 550, height=140)})  # TODO? split score=3 into all but methane and others
    
    try({(footprint_elec_US <- barres(vars = Variables_footprint$el[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$el[c(2,1,3)]))
      save_plotly_new_filename(footprint_elec_US, width= 400, height=220) })
    
    try({(footprint_transport_US <- barres(vars = Variables_footprint$tr[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$tr[c(2,1,3)]))
      save_plotly_new_filename(footprint_transport_US, width= 400, height=220) })
    
    try({(footprint_food_US <- barres(vars = Variables_footprint$fd[c(2,3,1)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$fd[c(2,3,1)]))
      save_plotly_new_filename(footprint_food_US, width= 400, height=220) }) # TODO India no beef
    
    try({(footprint_region_US <- barres(vars = paste(Variables_footprint$reg, "original", sep="_")[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, sort = F, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$reg[c(4,2,1,3)]))
      save_plotly_new_filename(footprint_region_US, width= 400, height=280) })
    
    try({(footprint_region_no_miss_US <- barres(vars = Variables_footprint$reg[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$reg[c(4,2,1,3)]))
      save_plotly_new_filename(footprint_region_no_miss_US, width= 400, height=280) })
    
    try({(footprint_pc_US <- barres(vars = paste(Variables_footprint$pc, "original", sep="_")[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, sort = F, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$pc[4:1]))
      save_plotly_new_filename(footprint_pc_US, width= 400, height=280) })
    # TODO? polluting_sector
    try({(footprint_pc_no_miss_US <- barres(vars = Variables_footprint$pc[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$pc[4:1]))
      save_plotly_new_filename(footprint_pc_no_miss_US, width= 400, height=280) })
    
    try({(footprint_elec_US_extr <- barres(vars = c("least_footprint_el", "most_footprint_el"), export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Smallest footprint", "Largest footprint")))
      save_plotly_new_filename(footprint_elec_US_extr, width= 470, height=200) })
    
    try({(footprint_transport_US_extr <- barres(vars = c("least_footprint_tr", "most_footprint_tr"), export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Smallest footprint", "Largest footprint")))
      save_plotly_new_filename(footprint_transport_US_extr, width= 470, height=200) })
    
    try({(footprint_food_US_extr <- barres(vars = c("least_footprint_fd", "most_footprint_fd"), export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Smallest footprint", "Largest footprint")))
      save_plotly_new_filename(footprint_food_US_extr, width= 470, height=200) })
    
    try({(footprint_region_US_extr <- barres(vars = c("least_footprint_reg", "most_footprint_reg"), export_xls = export_xls, df = e, miss=T, sort = F, labels=c("Smallest footprint", "Largest footprint")))
      save_plotly_new_filename(footprint_region_US_extr, width= 470, height=200) })
    
    try({(footprint_pc_US_extr <- barres(vars = c("least_footprint_pc", "most_footprint_pc"), export_xls = export_xls, df = e, miss=T, sort = F, labels=c("Smallest footprint", "Largest footprint")))
      save_plotly_new_filename(footprint_pc_US_extr, width= 470, height=200) })
    
    try({(score_footprint_transport_US <- barres(vars = "score_footprint_transport", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>transport footprint"))
      save_plotly_new_filename(score_footprint_transport_US, width= 520, height=140) })
    
    try({(score_footprint_food_US <- barres(vars = "score_footprint_food", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>food footprint"))
      save_plotly_new_filename(score_footprint_food_US, width= 520, height=140)})
    
    try({(score_footprint_pc_US <- barres(vars = "score_footprint_pc", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance to true ranking of<br>regional per capita footprint"))
      save_plotly_new_filename(score_footprint_pc_US, width= 520, height=140)})
    
    try({(score_footprint_region_US <- barres(vars = "score_footprint_region", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance to true ranking of<br>regional total footprint"))
      save_plotly_new_filename(score_footprint_region_US, width= 520, height=140)})
    
    try({(score_footprint_regions_US <- barres(vars = c("score_footprint_pc", "score_footprint_region"), rev = F, sort = F, export_xls = export_xls, df = e, miss=T, labels=c("per capita regional footprint", "Distance to true ranking of<br>regional total footprint")))
      save_plotly_new_filename(score_footprint_regions_US, width= 550, height=200)})
    
    try({(score_footprint_elec_US <- barres(vars = "score_footprint_elec", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>electricity footprint"))
      save_plotly_new_filename(score_footprint_elec_US, width= 520, height=140)})
    
    try({(scores_footprint_US <- barres(vars = c("score_footprint_transport", "score_footprint_elec", "score_footprint_food"), rev = F, export_xls = export_xls, df = e, miss=F, labels=c("Transport", "Electricity","Food")))
      save_plotly_new_filename(scores_footprint_US, width= 500, height=240)})
    
    labels_CC_impacts <- c()
    for (v in variables_CC_impacts) labels_CC_impacts <- c(labels_CC_impacts, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(CC_impacts_US <- barres(vars = variables_CC_impacts, export_xls = export_xls, df = e, miss=F, rev_color=T, labels=labels_CC_impacts))
      save_plotly_new_filename(CC_impacts_US, width= 800, height=400) })
    
    ##### 6. Climate Change (attitudes and risks) ##### 
    labels_responsible <- c()
    for (v in variables_responsible_CC) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(CC_responsible_US <- barres(vars = variables_responsible_CC, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_responsible, hover= label_great_deal))
      save_plotly_new_filename(CC_responsible_US, width= 720, height=300) })
    
    try({(net_zero_feasible_US <- barres(vars = "net_zero_feasible", export_xls = export_xls, df = e, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S."))
      save_plotly_new_filename(net_zero_feasible_US, width= 620, height=140)})
    
    try({(CC_affects_self_US <- barres(vars = "CC_affects_self", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Climate change negatively affects personal life"))
      save_plotly_new_filename(CC_affects_self_US, width= 800, height=140) })
    
    try({(pro_ambitious_policies_US <- barres(vars = "pro_ambitious_policies", export_xls = export_xls, df = e,rev = F, rev_color = T, miss = F, labels="Ambitious public policies needed<br>to halt climate change"))
      save_plotly_new_filename(pro_ambitious_policies_US, width= 670, height=140)})
    
    # try({(CC_attitude_US <- barres(vars = c("net_zero_feasible", "CC_affects_self", "pro_ambitious_policies"), export_xls = export_xls, df = e,rev = F, rev_color = T, sort = F, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S.", "Climate change negatively affects personal life", "Ambitious public policies needed<br>to halt climate change")))
    #   save_plotly_new_filename(CC_attitude_US, width= 770, height=250)})
    
    try({(CC_attitude_US <- barres(vars = c("net_zero_feasible", "CC_affects_self"), export_xls = export_xls, df = e,rev = F, rev_color = T, sort = F, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S.", "Climate change negatively affects personal life")))
      save_plotly_new_filename(CC_attitude_US, width= 750, height=220)})
    
    try({(CC_will_end_US <- barres(vars = "CC_will_end", export_xls = export_xls, df = e, miss = F, labels="Likely to halt CC by the end of the century"))
      save_plotly_new_filename(CC_will_end_US, width= 855, height=140)})
    
    try({(effect_halt_CC_economy_US <- barres(vars = "effect_halt_CC_economy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Effects of ambitious policies <br> on the U.S economy and employment"))
      save_plotly_new_filename(effect_halt_CC_economy_US, width= 820, height=140)})
    
    try({(effect_halt_CC_lifestyle_US <- barres(vars = "effect_halt_CC_lifestyle", export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels="Negative effects of ambitious policies on lifestyle"))
      save_plotly_new_filename(effect_halt_CC_lifestyle_US, width= 680, height=140)})
    
    labels_willing <- c()
    for (v in variables_willing) labels_willing <- c(labels_willing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(willing_US <- barres(vars = variables_willing, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_willing, hover=label_great_deal))
      save_plotly_new_filename(willing_US, width= 800, height=320) })
    
    labels_condition <- c()
    for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(condition_US <- barres(vars = variables_condition, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_condition, hover = label_great_deal))
      save_plotly_new_filename(condition_US, width= 805, height=250) })
    
    ##### 7. Pref 1: emission standards #####
    labels_standard_effects <- c()
    for (v in variables_standard_effect) labels_standard_effects <- c(labels_standard_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_standard_effects[3] <- "have a negative effect<br>on the U.S. economy and employment"
    labels_standard_effects[4] <- "have a large  effect<br>on the U.S. economy and employment"
    try({(standard_effect_US <- barres(vars = rev(variables_standard_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_standard_effects), hover=labels_agree))
      save_plotly_new_filename(standard_effect_US, width= 890, height=320) })
    
    labels_win_lose <- c("Lose a lot", "Mostly lose", "Neither win nor lose", "Mostly win", "Win a lot")
    labels_standard_win_lose <- c()
    for (v in variables_standard_win_lose) labels_standard_win_lose <- c(labels_standard_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_standard_win_lose[5] <- "Your household financially"
    try({(standard_win_lose_US <- barres(vars = variables_standard_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, sort=F, miss = F, showLegend=T, labels=labels_standard_win_lose, hover = labels_win_lose))
      save_plotly_new_filename(standard_win_lose_US, width= 1100, height=320) })
    
    try({(standard_fair_US <- barres(vars = "standard_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Ban on combustion-engine cars fair"))
      save_plotly_new_filename(standard_fair_US, width= 1100, height=140)})
    
    try({(standard_support_US <- barres(vars = "standard_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Ban on combustion-engine cars"))
      save_plotly_new_filename(standard_support_US, width= 980, height=140)})
    
    try({(standard_public_transport_support_US <- barres(vars = "standard_public_transport_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Ban on combustion-engine cars with<br>alternatives such as public transports available"))
      save_plotly_new_filename(standard_public_transport_support_US, width= 950, height=140)})
    
    ##### 8. Pref 2: Green investments #####
    labels_investments_effects <- c()
    for (v in variables_investments_effect) labels_investments_effects <- c(labels_investments_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(investments_effect_US <- barres(vars = rev(variables_investments_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_investments_effects), hover=labels_agree))
      save_plotly_new_filename(investments_effect_US, width= 1100, height=320) })
    
    labels_investments_win_lose <- c()
    for (v in variables_investments_win_lose) labels_investments_win_lose <- c(labels_investments_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_investments_win_lose[5] <- "Your household financially"
    try({(investments_win_lose_US <- barres(vars = variables_investments_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_investments_win_lose, hover = labels_win_lose))
      save_plotly_new_filename(investments_win_lose_US, width= 1100, height=320) })
    
    try({(investments_fair_US <- barres(vars = "investments_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Green infrastructure program is fair"))
      save_plotly_new_filename(investments_fair_US, width= 1120, height=140)})
    
    try({(investments_support_US <- barres(vars = "investments_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Green infrastructure program"))
      save_plotly_new_filename(investments_support_US, width= 980, height=140)})
    
    labels_investments_funding <- c()
    for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(investments_funding_US <- barres(vars = variables_investments_funding, export_xls = export_xls, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
      save_plotly_new_filename(investments_funding_US, width= 560, height=260)})
    
    ##### 9. Pref 3: Tax and dividend #####
    labels_tax_transfers_effects <- c() 
    for (v in variables_tax_transfers_effect) labels_tax_transfers_effects <- c(labels_tax_transfers_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_tax_transfers_effects[3] <- "reduce the use of fossil fuels<br>and greenhouse gas emissions"
    labels_tax_transfers_effects[6] <- "have a large effect<br>on the U.S. economy and employment"
    labels_tax_transfers_effects[5] <- "have a negative effect<br>on the U.S. economy and employment"
    try({(tax_transfers_effect_US <- barres(vars = rev(variables_tax_transfers_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_tax_transfers_effects), hover=labels_agree))
      save_plotly_new_filename(tax_transfers_effect_US, width= 960, height=380) })
    
    labels_tax_transfers_win_lose <- c()
    for (v in variables_tax_transfers_win_lose) labels_tax_transfers_win_lose <- c(labels_tax_transfers_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_tax_transfers_win_lose[5] <- "Your household financially"
    try({(tax_transfers_win_lose_US <- barres(vars = variables_tax_transfers_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_tax_transfers_win_lose, hover = labels_win_lose))
      save_plotly_new_filename(tax_transfers_win_lose_US, width= 1100, height=320) })
    
    try({(tax_transfers_fair_US <- barres(vars = "tax_transfers_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers is fair"))
      save_plotly_new_filename(tax_transfers_fair_US, width= 1100, height=140)})
    
    try({(tax_transfers_support_US <- barres(vars = "tax_transfers_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers"))
      save_plotly_new_filename(tax_transfers_support_US, width= 980, height=140)})
    
    ##### 6-8. Specific policies #####
    labels_policies <- c("A ban on combustion-engine cars", "A green infrastructure program", "A carbon tax with cash transfers")
    try({(policies_cost_effective_US <- barres(vars = paste(names_policies, "cost_effective", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_cost_effective_US, width= 1100, height=240)})
    
    try({(policies_large_effect_US <- barres(vars = paste(names_policies, "large_effect", sep="_"), export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_large_effect_US, width= 1100, height=240)})
    
    try({(policies_negative_effect_US <- barres(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_negative_effect_US, width= 1100, height=240)})
    
    try({(policies_fair_US <- barres(vars = paste(names_policies, "fair", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_fair_US, width= 1100, height=240)})
    
    try({(policies_win_lose_poor_US <- barres(vars = paste(names_policies, "win_lose_poor", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_poor_US, width= 870, height=240)})
    
    try({(policies_win_lose_middle_US <- barres(vars = paste(names_policies, "win_lose_middle", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_middle_US, width= 870, height=240)})
    
    try({(policies_win_lose_rich_US <- barres(vars = paste(names_policies, "win_lose_rich", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_rich_US, width= 870, height=240)})
    
    try({(policies_win_lose_rural_US <- barres(vars = paste(names_policies, "win_lose_rural", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_rural_US, width= 870, height=240)})
    
    try({(policies_win_lose_self_US <- barres(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_self_US, width= 870, height=240)})
    
    try({(policies_support_US <- barres(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_support_US, width= 1020, height=240)})
    
    try({(policies_all_support_US <- barres(vars = c("standard_public_transport_support", paste(names_policies, "support", sep="_")), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=c("Ban on combustion cars where<br>public transport made available", labels_policies)))
      save_plotly_new_filename(policies_all_support_US, width= 790, height=270)})
    
    try({(standard_effects_US <- barres(vars = c("standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                                   export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce CO2 emissions from cars")))
      save_plotly_new_filename(standard_effects_US, width= 1100, height=200)})
    
    try({(standard_all_US <- barres(vars = c("standard_fair", "standard_cost_effective", "standard_negative_effect", "standard_large_effect", "standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                               export_xls = export_xls, df = e, miss=F, labels=c("Be fair", "Costly way<br>to fight climate change", paste0("Negative effect on ", country, "<br>economy and employment"), paste0("Large effect on ", country, "<br>economy and employment"), "Reduce air pollution", "Reduce CO2 emissions from cars")))
      save_plotly_new_filename(standard_all_US, width= 1100, height=380)})
    
    try({(investments_effects_US <- barres(vars = c("investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F,rev = F, rev_color = T, 
                                      export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
      save_plotly_new_filename(investments_effects_US, width= 1150, height=240)})
    
    try({(investments_all_US <- barres(vars = c("investments_cost_effective", "investments_negative_effect", "investments_large_effect", "investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F, rev = F, rev_color = T, 
                                  export_xls = export_xls, df = e, miss=F, labels=c("Costly way<br>to fight climate change", paste0("Negative effect on ", country, "<br>economy and employment"), paste0("Large effect on ", country, "<br>economy and employment"), "Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
      save_plotly_new_filename(investments_all_US, width= 1150, height=400)})
    
    try({(tax_transfers_effects_US <- barres(vars = c("tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F,rev = F, rev_color = T, 
                                        export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
      save_plotly_new_filename(tax_transfers_effects_US, width= 1100, height=280)})
    
    try({(tax_transfers_all_US <- barres(vars = c("tax_transfers_cost_effective", "tax_transfers_negative_effect", "tax_transfers_large_effect", "tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F, rev = F, rev_color = T, 
                                    export_xls = export_xls, df = e, miss=F, labels=c("Costly way<br>to fight climate change", paste0("Negative effect on ", country, "<br>economy and employment"), paste0("Large effect on ", country, "<br>economy and employment"), "Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
      save_plotly_new_filename(tax_transfers_all_US, width= 1150, height=430)})
    
    
    ##### 10. Pref on climate policies #####
    labels_support <- c("Strongly oppose", "Somewhat oppose", "Indifferent", "Somewhat support", "Strongly support")
    labels_policy <- c()
    for (v in variables_policy) labels_policy <- c(labels_policy, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    # labels_policy[5] <- "Subsidies for low-carbon technologies (renewables, CCS...)"
    # labels_policy[6] <- "Financing clean energy in low-income countries" 
    labels_policy[4] <- "Subsidies for low-carbon technologies (renewables, CCS...)"
    labels_policy[5] <- "Financing clean energy in low-income countries" 
    labels_policy[2] <- "National tax on fossil fuels (+$0.40/gallon)"  # TODO
    try({(policy_US <- barres(vars = variables_policy, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_policy, hover=labels_support))
      save_plotly_new_filename(policy_US, width= 920, height=340)})
    
    labels_tax <- c()
    for (v in variables_tax) labels_tax <- c(labels_tax, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_tax[7] <- "Funding environmental infrastructures (e.g. public transport)"
    labels_tax[1] <- "Cash transfers to HH with no alternative to fossil fuels"
    labels_tax[8] <- "Subsidizing low-carbon technologies, including renewables"
    try({(tax_US <- barres(vars = variables_tax, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, showLegend=T, labels=labels_tax, hover=labels_support))
      save_plotly_new_filename(tax_US, width= 930, height=400) })
    
    ##### 11. WTP #####
    try({(wtp_US <- barres(vars = rev(variables_wtp), export_xls = export_xls, df = e, miss=F, sort = F, labels=rev(c("WTP to limit global warming ($/year): 10", "30", "50", "100", "300", "500", "1000"))))
      save_plotly_new_filename(wtp_US, width= 680, height=340)})
    
    # try({(wtp_US <- barres(vars = "wtp", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="WTP to limit global warming ($/year)"))
    #   save_plotly_new_filename(wtp_US, width= 1050, height=200)})
    
    try({(wtp_agg_US <- barres(vars = "wtp_agg", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
      save_plotly_new_filename(wtp_agg_US, width= 950, height=140)})
    
    # mar_old <- par()$mar
    # cex_old <- par()$cex
    # par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
    # cdf_wtp_US <- Ecdf(e$wtp, weights = e$weight)
    # plot(cdf_wtp_US$x, cdf_wtp_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
    # title(ylab=expression("Proportion <= x"), xlab="WTP (in $/year)", line=2.3)
    # grid() # TODO legend
    # par(mar = mar_old, cex = cex_old)
    
    try({(donation_US <- barres(vars = "donation", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="Donation to climate charity ($/year)"))
      save_plotly_new_filename(donation_US, width= 1050, height=200)})
    
    try({(donation_agg_US <- barres(vars = "donation_agg", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
      save_plotly_new_filename(donation_agg_US, width= 670, height=140)})
    
    # mar_old <- par()$mar
    # cex_old <- par()$cex
    # par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
    # cdf_donation_US <- Ecdf(e$donation, weights = e$weight)
    # plot(cdf_donation_US$x, cdf_donation_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
    # title(ylab=expression("Proportion <= x"), xlab="Donation (in $/year)", line=2.3)
    # grid() # TODO save
    # par(mar = mar_old, cex = cex_old)
    
    ##### 12. International burden-sharing #####
    
    labels_scale <- c()
    for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(scale_US <- barres(vars = variables_scale, export_xls = export_xls, df = e, rev = F, miss = T, showLegend=F, labels=labels_scale))
      save_plotly_new_filename(scale_US, width= 460, height=250) })
    
    try({(should_fight_CC_US <- barres(vars = "should_fight_CC", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels=" U.S. should fight CC"))
      save_plotly_new_filename(should_fight_CC_US, width= 1075, height=140)})
    
    labels_if_other_do <- c()
    for (v in variables_if_other_do) labels_if_other_do <- c(labels_if_other_do, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(if_other_do_US <- barres(vars = variables_if_other_do, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_if_other_do, hover=c("Much less", "Less", "About the same", "More", "Much more")))
      save_plotly_new_filename(if_other_do_US, width= 830, height=200) })
    
    labels_burden_sharing <- c() # TODO: do not sort
    for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_burden_sharing[4] <- "The richest countries should pay it all, <br> so that the poorest countries do not have to pay anything"
    labels_burden_sharing[3] <- "Countries should pay in proportion to <br> their past emissions (from 1990 onwards)"
    labels_burden_sharing[5] <- "The richest countries should pay even more <br> to help vulnerable countries face adverse consequences" 
    try({(burden_sharing_US <- barres(vars = variables_burden_sharing, export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels=labels_burden_sharing))
      save_plotly_new_filename(burden_sharing_US, width= 1150, height=325) })
    
    try({(global_assembly_support_US <- barres(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global democratic assembly<br>on climate change"))
      save_plotly_new_filename(global_assembly_support_US, width= 990, height=140)})
    
    try({(global_tax_support_US <- barres(vars = "global_tax_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on GHG<br> financing a global basic income"))
      save_plotly_new_filename(global_tax_support_US, width= 780, height=140)})
    
    try({(tax_1p_support_US <- barres(vars = "tax_1p_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))
      save_plotly_new_filename(tax_1p_support_US, width= 780, height=140)})
    
    labels_global_policies <- c("Global democratic assembly<br>on climate change", "Global tax on GHG<br> financing a global basic income", "Global tax on millionaires <br> to finance low-income countries")
    try({(global_policies_US <- barres(vars = c("global_assembly_support", "global_tax_support", "tax_1p_support"), export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels=labels_global_policies))
      save_plotly_new_filename(global_policies_US, width= 800, height=250)})
    
    ##### 13. Pref for bans vs. incentives #####
    
    try({(will_insulate_US <- barres(vars = "will_insulate", export_xls = export_xls, df = e, miss=F, labels="Insulate or replace heating<br>over the next 5 years"))
      save_plotly_new_filename(will_insulate_US, width= 600, height=140)})
    
    try({(insulation_support_US <- barres(vars = "insulation_support", export_xls = export_xls, df = e, miss=F, labels="Mandatory insulation with subsidies"))
      save_plotly_new_filename(insulation_support_US, width= 1035, height=140)})
    
    try({(insulation_support_variant_US <- barres12(vars = "insulation_support", export_xls = export_xls, df = list(e[e$insulation_disruption_variant==T,], e[e$insulation_disruption_variant==F,]), orig="<br>Control", comp = "Priming: renovation cause disruption", miss=F, labels="Mandatory insulation with subsidies"))
      save_plotly_new_filename(insulation_support_variant_US, width= 1035, height=240) })
    
    labels_obstacles_insulation <- c()
    for (v in variables_obstacles_insulation) labels_obstacles_insulation <- c(labels_obstacles_insulation, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_obstacles_insulation[1] <- "The choice is not mine"
    labels_obstacles_insulation[5] <- "My insulation and heating systems<br>are already satisfactory"
    try({(obstacles_insulation_US <- barres(vars = variables_obstacles_insulation, export_xls = export_xls, df = e, error_margin=F, rev = F, miss = F, showLegend=F, labels=labels_obstacles_insulation, hover=labels_obstacles_insulation))
      save_plotly_new_filename(obstacles_insulation_US, width= 550, height=230) })
    
    # try({(insulation_subsidies_support_US <- barres(vars = "insulation_subsidies_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for subsidies of insulation"))
    #   save_plotly_new_filename(insulation_subsidies_support_US, width= 1030, height=170)})
    
    try({(insulation_mandatory_support_US <- barres(vars = "insulation", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for making insulation mandatory"))
      save_plotly_new_filename(insulation_mandatory_support_US, width= 1030, height=170)})
    
    # try({(insulation_support_US <- barres(vars = c("insulation_subsidies_support", "insulation_mandatory_support"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=c("Insulation subsidized", "...and mandatory")))
    #   save_plotly_new_filename(insulation_support_US, width= 960, height=200)})
    
    labels_beef <- c()
    for (v in variables_beef) labels_beef <- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(beef_US <- barres(vars = variables_beef, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels=labels_beef,showLegend=T, hover= labels_support))
      save_plotly_new_filename(beef_US, width= 900, height=240)  })
    
    
    ##### 14. Trust, perceptions of institutions, etc. #####
    
    try({(can_trust_people_US <- barres(vars = "can_trust_people", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust other people"))
      save_plotly_new_filename(can_trust_people_US, width= 1060, height=140)})
    
    try({(can_trust_govt_US <- barres(vars = "can_trust_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust the U.S. federal government <br> over the last decade to do what is right"))
      save_plotly_new_filename(can_trust_govt_US, width= 875, height=140)})
    
    try({(view_govt_US <- barres(vars = "view_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="View on government intervention"))
      save_plotly_new_filename(view_govt_US, width= 700, height=140)})
    
    try({(problem_inequality_US <- barres(vars = "problem_inequality", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Income inequality in the U.S."))
      save_plotly_new_filename(problem_inequality_US, width= 950, height=140)})
    
    try({(future_richness_US <- barres(vars = "future_richness", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="In 100 years, people in the world will be..."))
      save_plotly_new_filename(future_richness_US, width= 850, height=140)})
    
    ##### 15. Political views #####
    
    try({(interested_politics_US <- barres(vars = "interested_politics", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Interested in politics"))
      save_plotly_new_filename(interested_politics_US, width= 700, height=140)})
    
    try({(member_environmental_orga_US <- barres(vars = "member_environmental_orga", export_xls = export_xls, df = e, miss=F, labels="Member of an<br> environmental organization"))
      save_plotly_new_filename(member_environmental_orga_US, width= 622, height=140)})
    
    try({(relative_environmentalist_US <- barres(vars = "relative_environmentalist", export_xls = export_xls, df = e, miss=F, labels="Has an environmentalist relative"))
      save_plotly_new_filename(relative_environmentalist_US, width= 540, height=140)})
    
    try({(relative_environmentalist_US <- barres(vars = "relative_environmentalist", export_xls = export_xls, df = e, miss=F, labels="Has an environmentalist relative"))
      save_plotly_new_filename(relative_environmentalist_US, width= 540, height=140)})
    
    try({(vote_participation_US <- barres(vars = "vote_participation", export_xls = export_xls, df = e, miss=T, labels="Voted in 2020 election"))
      save_plotly_new_filename(vote_participation_US, width= 540, height=140) })
    
    # try({(vote_participation_2016_US <- barres(vars = "vote_participation_2016", export_xls = export_xls, df = e, miss=T, labels="Voted in 2016 election"))
    #   save_plotly_new_filename(vote_participation_2016_US, width= 540, height=140) })
    
    try({(vote_US <- barres(vars = "vote", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for"))
      save_plotly_new_filename(vote_US, width= 650, height=140) })
    
    # try({(vote_2020 <- barres(vars = "vote_2020", export_xls = export_xls, df = e, rev_color = T, miss=T, fr = "PNR/no right", labels="In 2020, voted for"))
    #   save_plotly_new_filename(vote_2020, width= 610, height=140) })
    # 
    # try({(vote_2016_US <- barres(vars = "vote_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for"))
    #   save_plotly_new_filename(vote_2016_US, width= 650, height=140) })
    
    try({(vote_all_US <- barres(vars = c("vote_non_voters", "vote_voters", "vote"), export_xls = export_xls, df = e, rev_color = T, miss=F, sort = F, labels=c("Non-voters", "Voters", "2020 vote: Voters and non-voters")))
      save_plotly_new_filename(vote_all_US, width= 600, height=250)}) # TODO make it work even if Jorgensen/Hawkins don't appear everywhere
    
    try({(vote_voters_US <- barres(vars = "vote_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for (voters)"))
      save_plotly_new_filename(vote_voters_US, width= 650, height=140)})
    
    try({(vote_non_voters_US <- barres(vars = "vote_non_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for (non-voters)"))
      save_plotly_new_filename(vote_non_voters_US, width= 650, height=140)})
    
    # try({(vote_voters_2016_US <- barres(vars = "vote_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (voters)"))
    #   save_plotly_new_filename(vote_voters_2016_US, width= 650, height=140) })
    # 
    # try({(vote_non_voters_2016_US <- barres(vars = "vote_non_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (non-voters)"))
    #   save_plotly_new_filename(vote_non_voters_2016_US, width= 650, height=140)})
    
    try({(liberal_conservative_US <- barres(vars = "liberal_conservative", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="On economic policy matters, are you..."))
      save_plotly_new_filename(liberal_conservative_US, width= 950, height=140)})
    
    try({(political_affiliation_US <- barres(vars = "political_affiliation", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Political affiliation"))
      save_plotly_new_filename(political_affiliation_US, width= 800, height=140)})
    
    ##### 16. Feedback #####
    
    try({(survey_biased_US <- barres(vars = "survey_biased", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Survey biased"))
      save_plotly_new_filename(survey_biased_US, width= 810, height=140)})
    
    # rquery.wordcloud(paste(e$comment_field, collapse=" \n "), max.words = 70)
    # rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = "survey", max.words = 70) # TODO save
    # rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = "survey", colorPalette = "Blues", max.words = 70) # Spectral
    
    ##### Heterogeneity: function #####
    plot_heterogeneity12 <- function(dfs = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), comp = "(Top 50%)", orig="<br>(Bottom 50%)", text_file = paste0(replacement_text, "_inc"), export_xls = F) {
      try({(temp <- barres12(vars = "CC_anthropogenic", export_xls = export_xls, df = dfs, comp = comp, orig=orig, rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
        save_plotly_new_filename(temp, filename = paste0("CC_anthropogenic", text_file), width= 870, height=220)})
      
      try({(temp <- barres12(vars = "CC_affects_self", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="CC negatively affects personal life"))
        save_plotly_new_filename(temp, filename = paste0("CC_affects_self", text_file), width= 800, height=220)})
      
      try({(temp <- barres12(vars = "net_zero_feasible", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions")) #  while maintaining<br>satisfactory standards of living in the U.S.
        save_plotly_new_filename(temp, filename = paste0("net_zero_feasible", text_file), width= 810, height=220)})
      
      try({(temp <- barres12(vars = "CC_will_end", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Likely to halt CC by the end of the century"))
        save_plotly_new_filename(temp, filename = paste0("CC_will_end", text_file), width= 970, height=220)})
      
      try({(temp <- barres12(vars = "future_richness",rev = F, rev_color = T,  export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="In 100 years, people in the world will be..."))
        save_plotly_new_filename(temp, filename = paste0("future_richness", text_file), width= 940, height=220)})
      
      try({(temp <- barres12(vars = "effect_halt_CC_lifestyle",rev = F, rev_color = T,  export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Negative effects of ambitious policies on lifestyle"))
        save_plotly_new_filename(temp, filename = paste0("effect_halt_CC_lifestyle", text_file), width= 1100, height=220)})
      
      try({(temp <- barres12(vars = "willing_limit_beef", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Willing to limit beef consumption"))
        save_plotly_new_filename(temp, filename = paste0("willing_limit_beef", text_file), width= 770, height=220)})
      
      try({(temp <- barres12(vars = "willing_limit_flying", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss = F, labels="Willing to limit flying"))
        save_plotly_new_filename(temp, filename = paste0("willing_limit_flying", text_file), width= 680, height=220)})
      
      try({(temp <- barres12(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
        save_plotly_new_filename(temp, filename = paste0("policies_negative_effect", text_file), width= 1100, height=380)})
      
      try({(temp <- barres12(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
        save_plotly_new_filename(temp, filename = paste0("policies_win_lose_self", text_file), width= 870, height=380)})
      
      try({(temp <- barres12(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
        save_plotly_new_filename(temp, filename = paste0("policies_support", text_file), width= 1020, height=380)})
      
      try({(temp <- barres12(vars = "donation_agg", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
        save_plotly_new_filename(temp, filename = paste0("donation_agg", text_file), width= 750, height=220)})
      
      try({(temp <- barres12(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="Global democratic assembly<br>on climate change"))
        save_plotly_new_filename(temp, filename = paste0("global_assembly_support", text_file), width= 990, height=220)})
      
      try({(temp <- barres12(vars = "wtp", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
        save_plotly_new_filename(temp, filename = paste0("wtp", text_file), width= 950, height=220)})
      
      # try({(temp <- barres12(vars = "global_tax_support", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="Global tax on GHG<br> financing a global basic income"))
      #   save_plotly_new_filename(temp, filename = paste0("global_tax_support", text_file), width= 780, height=220)})
      # 
      # try({(temp <- barres12(vars = "tax_1p_support", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))
      #   save_plotly_new_filename(temp, filename = paste0("tax_1p_support", text_file), width= 780, height=220)})
    }
    
    plot_heterogeneityN <- function(df = e, along = "vote", text_file = paste0(replacement_text, "_vote"), export_xls = F) {
      levels <- Levels(df[[along]])
      try({(temp <- barresN(vars = "CC_anthropogenic", export_xls = export_xls, df = df, along = along, rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
          save_plotly_new_filename(temp, filename = paste0("CC_anthropogenic", text_file), width= 870, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "CC_affects_self", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss = F, labels="CC negatively affects personal life"))
          save_plotly_new_filename(temp, filename = paste0("CC_affects_self", text_file), width= 800, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "net_zero_feasible", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions")) #  while maintaining<br>satisfactory standards of living in the U.S.
          save_plotly_new_filename(temp, filename = paste0("net_zero_feasible", text_file), width= 810, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "CC_will_end", export_xls = export_xls, df = df, along = along, miss = F, labels="Likely to halt CC by the end of the century"))
          save_plotly_new_filename(temp, filename = paste0("CC_will_end", text_file), width= 970, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "future_richness",rev = F, rev_color = T,  export_xls = export_xls, df = df, along = along, miss=F, labels="In 100 years, people in the world will be..."))
          save_plotly_new_filename(temp, filename = paste0("future_richness", text_file), width= 940, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "effect_halt_CC_lifestyle",rev = F, rev_color = T,  export_xls = export_xls, df = df, along = along, miss = F, labels="Negative effects of ambitious policies on lifestyle"))
          save_plotly_new_filename(temp, filename = paste0("effect_halt_CC_lifestyle", text_file), width= 1100, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "willing_limit_beef", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss = F, labels="Willing to limit beef consumption"))
          save_plotly_new_filename(temp, filename = paste0("willing_limit_beef", text_file), width= 770, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "willing_limit_flying", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss = F, labels="Willing to limit flying"))
          save_plotly_new_filename(temp, filename = paste0("willing_limit_flying", text_file), width= 680, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = paste(names_policies, "negative_effect", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
          save_plotly_new_filename(temp, filename = paste0("policies_negative_effect", text_file), width= 1100, height=fig_height(3*length(levels)))})
      
      try({(temp <- barresN(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
          save_plotly_new_filename(temp, filename = paste0("policies_win_lose_self", text_file), width= 870, height=fig_height(3*length(levels)))})
      
      try({(temp <- barresN(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
          save_plotly_new_filename(temp, filename = paste0("policies_support", text_file), width= 1020, height=fig_height(3*length(levels)))})
      
      try({(temp <- barresN(vars = "donation_agg", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color = T, labels="Donation to climate charity (in $)"))
          save_plotly_new_filename(temp, filename = paste0("donation_agg", text_file), width= 750, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels="Global democratic assembly<br>on climate change"))
          save_plotly_new_filename(temp, filename = paste0("global_assembly_support", text_file), width= 990, height=fig_height(1*length(levels)))})
      
      try({(temp <- barresN(vars = "wtp", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming"))
          save_plotly_new_filename(temp, filename = paste0("wtp", text_file), width= 950, height=fig_height(1*length(levels)))})
      
      # try({(temp <- barresN(vars = "global_tax_support", export_xls = export_xls, df = df, along = along, miss=F, labels="Global tax on GHG<br> financing a global basic income"))},
      #     save_plotly_new_filename(temp, filename = paste0("global_tax_support", text_file), width= 780, height=fig_height(1*length(levels)))})
      #
      # try({(temp <- barresN(vars = "tax_1p_support", export_xls = export_xls, df = df, along = along, miss=F, labels="Global tax on millionaires <br> to finance low-income countries"))},
      #     save_plotly_new_filename(temp, filename = paste0("tax_1p_support", text_file), width= 780, height=fig_height(1*length(levels)))})
    }
    
    
    ##### Heterogeneity #####
    # By income
    plot_heterogeneity12(dfs = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), orig="<br>(Bottom 50%)", comp = "(Top 50%)", text_file = paste0(replacement_text, "_inc"), export_xls = export_xls)
    # By vote
    # plot_heterogeneity12(dfs = list(e[e$vote == "Trump",], e[e$vote == "Biden",]), comp = "(Biden voter)", orig="<br>(Trump voter)", text_file = paste0(replacement_text, "_pol"), export_xls = export_xls)
    # plot_heterogeneityN(along = "vote3", text_file = paste0(replacement_text, "_vote"), export_xls = export_xls)
    # # By rural/urban # TODO
    # plot_heterogeneity12(dfs = list(e[e$core_metropolitan==T,], e[e$core_metropolitan==F,]), orig="<br>(Core metro)", comp = "(Non-core metro)", text_file = paste0(replacement_text, "_urb"), export_xls = export_xls)
    
    ## Other
    try({(wtp_US_anthropogenic <- barres12(vars = "wtp", export_xls = export_xls, df = list(e[e$CC_anthropogenic == "Most",], e[e$CC_anthropogenic <= 0,]), comp = "<br>(CC not mainly anthropogenic)", orig="<br>(CC anthropogenic)", miss=F, labels="WTP to limit global warming ($/year)"))
      save_plotly_new_filename(wtp_US_anthropogenic, width= 830, height=220)})
    
    ##### Correlograms ##### TODO: save, database
    # correlogram("_fair")
    # correlogram("_cost_effective")
    # correlogram(vars = c(paste(names_policies, "support", sep="_"), "policies_support"))
    # correlogram("_win_lose_self")
    # correlogram("_win_lose_poor")
    # correlogram("_win_lose_rich")
    # correlogram("_win_lose_middle")
    # correlogram("_win_lose_rural")
    # correlogram("_large_effect")
    # correlogram("_negative_effect")
    # correlogram("standard_")
    # correlogram("investments_")
    # correlogram("tax_transfers_")
    # correlogram("knowledge")  
    
    missing_figures <- setdiff(sub("_US", "", list.files("../figures/US")), sub(replacement_text, "", list.files(folder)))
    cat(paste0(length(missing_figures), " Missing figures for ", country, ": "))
    missing_types <- c("_comp.png", "_pol.png", "_urb.png", "_vote.png")
    for (t in missing_types) if (grepl("_comp.png", missing_figures)) cat("All figures of type: ", t)
    missing_figures <- missing_figures[!grepl(paste(missing_types, collapse="|"), missing_figures)]
    # cat(paste0(length(missing_figures), " Missing figures for ", country, ": "), missing_figures, sep = '\n')
    cat(missing_figures, sep = '\n')
    cat(" ")
  }
  
  if (tables) {
    ##### Tables treatment effects #####
    ## Generate tables
    label_treat_wave <- c("Both treatments", "Climate treatment only", "Policy treatment only", "wave: Pilot 2")
    variables_controls <- variables_main_controls[which(variables_main_controls %in% names(e))] 
    print(paste0("Missing controls for ", country, ": ", variables_main_controls[which(!(variables_main_controls %in% names(e)))]))
    
    desc_table(dep_vars = c("CC_anthropogenic > 0", "CC_impacts_extinction > 0", "donation", "should_fight_CC > 0", "willing_limit_driving > 0"), filename = paste0(country, "_1"), save_folder = "../tables/", 
               dep.var.labels = c("CC caused by humans", "CC likely to cause extinction", "Donation (in \\$)", paste0(country, " should fight CC"), "Willing to limit driving"),
               data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
    )
    
    desc_table(dep_vars = c("tax_transfers_support > 0", "investments_support > 0", "standard_support > 0", "policies_support > 0"), filename = paste0(country, "_2"), save_folder = "../tables/", 
               dep.var.labels = c("Carbon tax with transfers", "Green Infrastructure Program", "Ban on combustion-engine cars", "Average over 3 policies"),
               dep.var.caption = c("Support"), data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
    )
    
    desc_table(dep_vars = c("policies_fair > 0", "policies_self > 0", "policies_poor > 0", "policies_large_effect > 0", "policies_negative_effect > 0"), filename = paste0(country, "_3"), save_folder = "../tables/", 
               dep.var.labels =  c("Fair", "HH would win", "Poor would win", "Large economic effect", "Negative economic effect"),
               data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
    )
    
  }
  print(Sys.time() - start)
}

# render_figures_tables_country(dk, "DK")
# render_figures_tables_country(fr, "FR")