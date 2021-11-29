# # Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F

# TODO! render positive/costless (e.g. standard_effect, standard_all, policies_costless), vote/left_right, wealth, burden_share (incl. opinion_)
# TODO! render figures with errors
# TODO  area (SA, ID), gas_spike, standard_prefer, ia$religion, policy_ban_coal/tax_reduction_EEG_Umlage, investments_funding_global_transfer, scale_asean/african/EU, sp$insulation_mandatory_support_progressive
# TODO? on_control = FALSE on some of them?
# TODO: various things below to debug or improve figures (at most as important as previous line)
# TODO: size of town (DK has not same bins), quality (know_treatment_climate, watched_climate)
# TODO: translation (needs to be done manually), save as EMF https://www.rdocumentation.org/packages/devEMF/versions/4.0-2/topics/emf

update_constant <- function(data = all) {
  df <<- e <<- data
  country <- deparse(substitute(data))
  if (country == "all") {
    folder <<- paste0('../figures/country_comparison/')
    replacement_text <<- "_countries"
  } else {
    folder <<- paste0('../figures/', country, '/')
    replacement_text <<- paste0("_", toupper(country))
  }
  alphabetical <<- FALSE
  heatmap_conditions <<- c("", ">= 1", "/") # c("/") # c("", ">= 1", "/")
  along <<- "country_name"
  nb_levels <<- length(Levels(e[[along]]))
  cond <<- ">= 1"
  parentheses <<- FALSE
  nolabel <<- T
  on_control <<- T
  export_xls <<- F
  special <<- c("World", "OECD", "Non-OECD")
}

# /!\ open the Plots pane at its maximum before running the function
render_figures_tables_country <- function(data, country, on_control = T, export_xls = F, folder_country = F, name_country = T, figures = T, tables = T) {
  print(country)  
  start <- Sys.time()
  if (!(missing(data))) e <- data
  else e <- db[[country]] # db[db$country==country,]
  
  if (folder_country) folder <- paste0('../figures/', country, '/')
  else folder <- '../figures/'
  replacement_text <- ifelse(name_country, paste0("_", toupper(country)), "")
  
  save_plotly_new_filename <<- function(plot, filename = NULL, width = dev.size('px')[1], height = dev.size('px')[2], method='orca', trim = T) {
    if (missing(filename)) filename <- sub("_US", replacement_text, deparse(substitute(plot)))
    return(save_plotly(plot, filename = filename, folder = folder, width = width, height = height, method=method, trim = trim))
  }

  if (tables) {
    ##### Tables treatment effects #####
    ## Generate tables
    # TODO! align variables_controls and control_variables
    variables_controls <- variables_main_controls[which(variables_main_controls %in% names(e))] 
    print(paste0("Missing controls for ", country, ": ", variables_main_controls[which(!(variables_main_controls %in% names(e)))]))
    
    try({desc_table(dep_vars = c("CC_anthropogenic > 0", "CC_impacts_extinction > 0", "donation", "should_fight_CC == 2", "willing_limit_driving > 0"), filename = paste0(toupper(country), "_1"), save_folder = "../tables/", 
                    dep.var.labels = c("CC caused by humans", "CC likely to cause extinction", "Donation (in \\% of max)", paste0(toupper(country), " should fight CC"), "Willing to limit driving"),
                    data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
    )})
    
    try({desc_table(dep_vars = c("tax_transfers_support > 0", "investments_support > 0", "standard_support > 0", "policies_support > 0"), filename = paste0(toupper(country), "_2"), save_folder = "../tables/", 
                    dep.var.labels = c("Carbon tax with transfers", "Green Infrastructure Program", "Ban on combustion-engine cars", "Average over 3 policies"),
                    dep.var.caption = c("Support"), data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
    )})
    
    try({desc_table(dep_vars = c("policies_fair > 0", "policies_self > 0", "policies_poor > 0", "policies_large_effect > 0", "policies_positive_negative > 0"), filename = paste0(toupper(country), "_3"), save_folder = "../tables/", 
                    dep.var.labels =  c("Fair", "HH would win", "Poor would win", "Large economic effect", "Positive economic effect"),
                    data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
    )})
    
  }
  
  if (on_control) e <- e[e$treatment=="None",]
  
  if (figures) {
    ##### Clouds #####
    
    try({ stopwords <- unlist(sapply(c("danish", "french", "english", "german", "italian", "spanish"), function(v) stopwords(v)))
    # stopwords <<- unlist(sapply(c("danish", "french", "english", "german", "italian", "spanish"), function(v) stopwords(v)))
    # rquery.wordcloud(paste(e$CC_field, collapse=" \n "), max.words = 70, excludeWords = stopwords) 
    rquery.wordcloud(paste(e$CC_field, collapse=" \n "), max.words = 70, colorPalette = "Blues", excludeWords = c(stopwords, "climate", "change", "government"))
    save_plot(filename = paste0(folder, "CC_field", replacement_text), height = 400, width = 400)
    # rquery.wordcloud(paste(e$comment_field, collapse=" \n "), max.words = 70)
    # rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = "survey", max.words = 70)
    rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = stopwords, colorPalette = "Blues", max.words = 70) # Spectral
    save_plot(filename = paste0(folder, "comment_field", replacement_text), height = 400, width = 400)  })
    
    ##### Correlograms ##### TODO: labels
    try({for (grep in c("_fair", "_costless_costly", "_win_lose_self", "_win_lose_poor", "_win_lose_rich", "_win_lose_middle",
                        "_win_lose_rural", "_large_effect", "_positive_negative", "standard_", "investments_", "tax_transfers_", "knowledge")) {
      correlogram(grep, df = e)
      save_plot(filename = sub("__", "_", paste0(folder, "corr_", grep, replacement_text)), height = 700, width = 700)   }
      
      correlogram(vars = c(paste(names_policies, "support", sep="_"), "policies_support"))
      save_plot(filename = paste0(folder, "corr_support", replacement_text), height = 400, width = 400)
      
      correlogram(vars = c(variables_knowledge, "index_knowledge", "index_knowledge_efa"))
      save_plot(filename = paste0(folder, "corr_knowledge", replacement_text), height = 700, width = 700)
      
      correlogram(vars = c(variables_affected_index, "index_affected")) 
      save_plot(filename = paste0(folder, "corr_affected", replacement_text), height = 700, width = 700)})


    # correlogram("_fair")
    # correlogram("_costless_costly")
    # correlogram("_win_lose_self")
    # correlogram("_win_lose_poor")
    # correlogram("_win_lose_rich")
    # correlogram("_win_lose_middle")
    # correlogram("_win_lose_rural")
    # correlogram("_large_effect")
    # correlogram("_positive_negative")
    # correlogram("standard_")
    # correlogram("investments_")
    # correlogram("tax_transfers_")
    # correlogram("knowledge")
    
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
      data_age <- cbind(dataKN("age", data=e[e$age!="Below 18",], miss=F, weights = F), dataKN("age", data=e[e$age!="Below 18",], miss=F, weights = T), pop_freq[[country]]$age)
      (age_US_comp <- barres(data = data_age, export_xls = export_xls, df = e, miss=F, rev = F, sort = F, labels=labels_comp, legend = dataKN("age", data=e, miss=F, return="legend")[1:5]))
      save_plotly_new_filename(age_US_comp, width= 560, height=240) })

    try({ data_region <- cbind(dataKN("region", data=e, miss=F, weights = F), dataKN("region", data=e, miss=F, weights = T), pop_freq[[country]][[paste0(toupper(country), "_region")]])
      (region_US_comp <- barres(data = data_region, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_comp, legend = dataKN("region", data=e, miss=F, return="legend")))
      save_plotly_new_filename(region_US_comp, width= 800, height=240)})

    try({data_urban <- cbind(dataKN("urban", data=e, miss=F, weights = F), dataKN("urban", data=e, miss=F, weights = T), pop_freq[[country]]$urban[2])
      (urban_US_comp <- barres(data = data_urban, export_xls = export_xls, df = e, sort = F, miss=F, rev_color = T, rev = F, labels=labels_comp, showLegend = F, legend="Core metropolitan"))
      save_plotly_new_filename(urban_US_comp, width= 660, height=240)})

    try({data_race <- cbind(dataKN("race", data=e, miss=F, weights = F), dataKN("race", data=e, miss=F, weights = T), pop_freq[[country]]$race)
      (race_US_comp <- barres(data = data_race, export_xls = export_xls, df = e, miss=F, rev = F, sort = F, labels=labels_comp, legend = dataKN("race", data=e, miss=F, return="legend")))
      save_plotly_new_filename(race_US_comp, width= 540, height=240)})
    
    try({data_income <- cbind(dataKN("income", data=e, miss=F, weights = F), dataKN("income", data=e, miss=F, weights = T), pop_freq[[country]]$income)
      (income_US_comp <- barres(data = data_income, export_xls = export_xls, df = e, miss=F, rev_color = T, sort = F, rev = F, labels=labels_comp, legend = dataKN("income", data=e, miss=F, return="legend")))
      save_plotly_new_filename(income_US_comp, width= 510, height=240)}) # 35/70/120

    try({(vote_agg_US <- barres(vars = "vote_agg", export_xls = export_xls, df = e, miss=T, rev = F, rev_color = T, sort = F, fr = "PNR or other", labels="Vote or hypothetical vote in last election"))
      save_plotly_new_filename(vote_agg_US, width= 900, height=140)}) 
    
    try({(vote_mode_leaning_US <- barres(vars = "vote_mode_leaning", export_xls = export_xls, df = e, miss=T, rev = F, rev_color = T, sort = F, labels="Vote or hypothetical vote in last election"))
      save_plotly_new_filename(vote_mode_leaning_US, width= 900, height=140)}) 
    
    try({(vote_main_US <- barres(vars = "vote_main", export_xls = export_xls, df = e, miss=T, rev = F, rev_color = T, sort = F, labels="Vote or hypothetical vote in last election"))
      save_plotly_new_filename(vote_main_US, width= 900, height=140)}) 
    
    try({data_diploma <- cbind(dataKN("diploma", data=e, miss=F, weights = F), dataKN("diploma", data=e, miss=F, weights = T), pop_freq[[country]]$diploma)
    (diploma_US_comp <- barres(data = data_diploma, export_xls = export_xls, df = e, miss=F, rev_color = T, sort = F, rev = F, labels=labels_comp, legend = dataKN("diploma", data=e, miss=F, return="legend")))
    save_plotly_new_filename(diploma_US_comp, width= 620, height=240)})
    
    try({data_vote_2020 <- cbind(dataKN("vote_2020", data=e, miss=T, weights = F), dataKN("vote_2020", data=e, miss=T, weights = T), pop_freq[[country]]$vote_2020)
      (vote_2020_US_comp <- barres(data = data_vote_2020, export_xls = export_xls, df = e, miss=T, sort = F, labels=labels_comp, legend = dataKN("vote_2020", data=e, miss=T, return="legend")))
      save_plotly_new_filename(vote_2020_US_comp, width= 550, height=240)})
    
    # try({data_vote_agg <- cbind(dataKN("vote_agg", data=e, miss=T, weights = F), dataKN("vote_agg", data=e, miss=T, weights = T), pop_freq[[country]]$vote_agg)
    #   (vote_agg_US_comp <- barres(data = data_vote_agg, export_xls = export_xls, df = e, miss=T, sort = F, labels=labels_comp, legend = dataKN("vote_agg", data=e, miss=T, return="legend")))
    #   save_plotly_new_filename(vote_agg_US_comp, width= 550, height=240)}) # doesn't make sense to print the comparison for now because official results exclude hypothetical vote
    # 
    try({(gender_US <- barres(vars = "gender_factor", export_xls = export_xls, df = e, miss = F, labels="Gender"))
      save_plotly_new_filename(gender_US, width= 470, height=140)})
    
    try({(age_US <- barres(vars = "age", export_xls = export_xls, df = e[e$age!="Below 18",], miss=F, rev = F, labels="Age"))
      save_plotly_new_filename(age_US, width= 500, height=140) })
    
    try({(region_US <- barres(vars = "region", export_xls = export_xls, df = e, miss=F, labels="Region"))
      save_plotly_new_filename(region_US, width= 800, height=140)})
    
    try({(urbanity_US <- barres(vars = "urbanity", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Size of agglomeration"))
      save_plotly_new_filename(urbanity_US, width= 660, height=140)})
    
    # try({(race_US <- barres(vars = variables_race[c(1:4)], export_xls = export_xls, df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
    #   save_plotly_new_filename(race_US, width= 340, height=240)}) # TODO
    
    try({(origin_US <- barres(vars = "origin", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Origin (or race)"))
      save_plotly_new_filename(origin_US, width= 780, height=140)})
    
    try({(income_US <- barres(vars = "income", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
      save_plotly_new_filename(income_US, width= 510, height=140)}) # 35/70/120 TODO
    
    try({(wealth_US <- barres(vars = "wealth", export_xls = export_xls, df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
      save_plotly_new_filename(wealth_US, width= 480, height=140)})
    
    try({(education_US <- barres(vars = "education", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
      save_plotly_new_filename(education_US, width= 1080, height=140) })
    
    try({(diploma_US <- barres(vars = "diploma", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Highest diploma"))
      save_plotly_new_filename(diploma_US, width= 620, height=140) })
    
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
    
    try({(flights_3y_US <- barres(vars = "flights_3y", export_xls = export_xls, df = e, miss=F, rev = F, labels="Round-trip flights between 2017 and 2019"))
      save_plotly_new_filename(flights_3y_US, width= 800, height=140)})
    
    try({(frequency_beef_US <- barres(vars = "frequency_beef", export_xls = export_xls, df = e, miss=F, rev = F, labels="How often do you eat beef?"))
      save_plotly_new_filename(frequency_beef_US, width= 720, height=140)})
    
    variables_transport_graph <<- c("transport_work", "transport_shopping", "transport_leisure")
    labels_transport <<- c("Work", "Shopping", "Leisure")
    try({(transport_US <- barres(vars = variables_transport_graph, export_xls = export_xls, df = e, rev = F, miss = F,rev_color = T,  labels=labels_transport))
      save_plotly_new_filename(transport_US, width= 850, height=275) })
    
    try({(availability_transport_US <- barres(vars = "availability_transport", export_xls = export_xls, df = e, rev_color = T, rev = F, miss=F, labels="Quality and availability of public transport near your home"))
      save_plotly_new_filename(availability_transport_US, width= 870, height=140)})
    
    ##### POST-TREATMENT #####
    
    ##### 3. Treatment feedback: local climate #####
    try({(watched_climate_US <- barres(vars = "watched_climate", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (local)"))
      save_plotly_new_filename(watched_climate_US, width= 900, height=140)})
    
    # try({(know_treatment_climate_US <- barres(vars = c("know_temperature_2100", "know_local_damage"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Knowledge climate video"))
    #   save_plotly_new_filename(know_treatment_climate_US, width= 580, height=140)})
    
    try({(know_treatment_climate_US <- barres(vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Score knowledge climate video"))
      save_plotly_new_filename(know_treatment_climate_US, width= 580, height=140)}) # TODO "all wrong"..
    
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
    
    try({(CC_anthropogenic_US_CI <- barres(vars = "CC_anthropogenic", error_margin = T, rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Part of climate change anthropogenic"))
      save_plotly_new_filename(CC_anthropogenic_US_CI, width= 660, height=140)})
    
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
    
    if (!country %in% bus_countries) Labels_footprint$tr[2] <- "Train"
    # else Labels_footprint$tr[2] <- "Bus"
    
    try({(footprint_transport_US <- barres(vars = Variables_footprint$tr[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$tr[c(2,1,3)]))
      save_plotly_new_filename(footprint_transport_US, width= 400, height=220) }) 
    
    try({(footprint_food_US <- barres(vars = Variables_footprint$fd[c(2,3,1)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$fd[c(2,3,1)]))
      save_plotly_new_filename(footprint_food_US, width= 400, height=220) }) # TODO India no beef
    
    try({(footprint_region_US <- barres(vars = paste(Variables_footprint$reg, "original", sep="_")[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, sort = F, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$reg[c(4,2,1,3)]))
      save_plotly_new_filename(footprint_region_US, width= 450, height=280) })
    
    try({(footprint_region_no_miss_US <- barres(vars = Variables_footprint$reg[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, sort = F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$reg[c(4,2,1,3)]))
      save_plotly_new_filename(footprint_region_no_miss_US, width= 400, height=280) })
    
    try({(footprint_pc_US <- barres(vars = paste(Variables_footprint$pc, "original", sep="_")[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, sort = F, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$pc[4:1]))
      save_plotly_new_filename(footprint_pc_US, width= 450, height=280) })
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
    
    try({(net_zero_feasible_US <- barres(vars = "net_zero_feasible", export_xls = export_xls, df = e, miss=F, rev = F, rev_color=T, labels=paste("Feasible to stop GHG emissions<br>while", ifelse(country %in% rich_countries, "maintaining", "sustaining"), "satisfactory<br>standards of living in", Country_names[country])))
      save_plotly_new_filename(net_zero_feasible_US, width= 620, height=140)})
    
    try({(CC_affects_self_US <- barres(vars = "CC_affects_self", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Climate change will negatively affect personal life"))
      save_plotly_new_filename(CC_affects_self_US, width= 800, height=140) })
    
    try({(pro_ambitious_policies_US <- barres(vars = "pro_ambitious_policies", export_xls = export_xls, df = e,rev = F, rev_color = T, miss = F, labels="Ambitious public policies needed<br>to halt climate change"))
      save_plotly_new_filename(pro_ambitious_policies_US, width= 670, height=140)})
    
    # try({(CC_attitude_US <- barres(vars = c("net_zero_feasible", "CC_affects_self", "pro_ambitious_policies"), export_xls = export_xls, df = e,rev = F, rev_color = T, sort = F, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S.", "Climate change negatively affects personal life", "Ambitious public policies needed<br>to halt climate change")))
    #   save_plotly_new_filename(CC_attitude_US, width= 770, height=250)})
    
    try({(CC_attitude_US <- barres(vars = c("net_zero_feasible", "CC_affects_self"), export_xls = export_xls, df = e,rev = F, rev_color = T, sort = F, miss = F, labels=c(paste("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in", Country_names[country]), "Climate change negatively affects personal life")))
      save_plotly_new_filename(CC_attitude_US, width= 750, height=220)})
    
    try({(CC_will_end_US <- barres(vars = "CC_will_end", export_xls = export_xls, df = e, miss = F, labels="Likely to halt CC by the end of the century"))
      save_plotly_new_filename(CC_will_end_US, width= 855, height=140)})
    
    try({(effect_halt_CC_economy_US <- barres(vars = "effect_halt_CC_economy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels=paste("Effects of ambitious policies <br> on the", toupper(country), "economy and employment")))
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
    labels_standard_effects[3] <- paste("have a positive effect<br>on the", country_names[country], "economy and employment")
    labels_standard_effects[4] <- paste("have a large effect<br>on the", country_names[country], "economy and employment")
    try({(standard_effect_US <- barres(vars = rev(variables_standard_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_standard_effects), hover=labels_agree))
      save_plotly_new_filename(standard_effect_US, width= 890, height=320) })
    
    labels_win_lose <- c("Lose a lot", "Mostly lose", "Neither win nor lose", "Mostly win", "Win a lot")
    labels_standard_win_lose <<- c()
    for (v in variables_standard_win_lose) labels_standard_win_lose <<- c(labels_standard_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_standard_win_lose[5] <<- "Your household financially"
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
    
    labels_investments_win_lose <<- c()
    for (v in variables_investments_win_lose) labels_investments_win_lose <<- c(labels_investments_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_investments_win_lose[5] <<- "Your household financially"
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
    labels_tax_transfers_effects[6] <- paste("have a large effect<br>on the", country_names[country], "economy and employment")
    labels_tax_transfers_effects[5] <- paste("have a positive effect<br>on the", country_names[country], "economy and employment")
    try({(tax_transfers_effect_US <- barres(vars = rev(variables_tax_transfers_effect), export_xls = export_xls, df = e, sort = F, rev_color = T , rev = F, miss = F, showLegend=T, labels=rev(labels_tax_transfers_effects), hover=labels_agree))
      save_plotly_new_filename(tax_transfers_effect_US, width= 960, height=380) })
    
    labels_tax_transfers_win_lose <<- c()
    for (v in variables_tax_transfers_win_lose) labels_tax_transfers_win_lose <<- c(labels_tax_transfers_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_tax_transfers_win_lose[5] <<- "Your household financially"
    try({(tax_transfers_win_lose_US <- barres(vars = variables_tax_transfers_win_lose, export_xls = export_xls, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_tax_transfers_win_lose, hover = labels_win_lose))
      save_plotly_new_filename(tax_transfers_win_lose_US, width= 1100, height=320) })
    
    try({(tax_transfers_fair_US <- barres(vars = "tax_transfers_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers is fair"))
      save_plotly_new_filename(tax_transfers_fair_US, width= 1100, height=140)})
    
    try({(tax_transfers_support_US <- barres(vars = "tax_transfers_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers"))
      save_plotly_new_filename(tax_transfers_support_US, width= 980, height=140)})
    
    ##### 6-8. Specific policies #####
    labels_policies <- c("A ban on combustion-engine cars", "A green infrastructure program", "A carbon tax with cash transfers")
    try({(policies_costless_costly_US <- barres(vars = paste(names_policies, "costless_costly", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_costless_costly_US, width= 1100, height=240)})
    
    try({(policies_large_effect_US <- barres(vars = paste(names_policies, "large_effect", sep="_"), export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_large_effect_US, width= 1100, height=240)})
    
    try({(policies_positive_negative_US <- barres(vars = paste(names_policies, "positive_negative", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, sort = F, labels=labels_policies))
      save_plotly_new_filename(policies_positive_negative_US, width= 1100, height=240)})
    
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

    try({(policies_less_pollution_US <- barres(vars = c("standard_effect_less_pollution", "investments_effect_less_pollution", "tax_transfers_effect_less_pollution"), sort = F,rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_less_pollution_US, width= 1100, height=240)})
    
    # try({(policies_effects_all_US <- barres(vars = c("standard_effect_less_pollution", "standard_effect_less_emission", "investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener", "tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F,rev = F, rev_color = T, 
    #                                      export_xls = export_xls, df = e, miss=F, labels=c("Ban on combustion-engine cars<br>Reduce air pollution", "Ban on combustion-engine cars<br>Reduce CO2 emissions from cars","Green infrastructure program<br>Reduce air pollution", "Green infrastructure program<br>Increase the use of public transport", "Green infrastructure program<br>Make electricity production greener", 
    # "Tax with cash transfers: Reduce air pollution", "Tax with cash transfers: Increase the use of public transport", "Tax with cash transfers: Make electricity production greener")))
    #   save_plotly_new_filename(policies_effects_all_US, width= 1100, height=fig_height(9))})
    
    try({(policies_effects_US <- barres(vars = c("standard_effect_less_emission", "investments_effect_public_transport", "investments_effect_elec_greener", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F,rev = F, rev_color = T, 
                                         export_xls = export_xls, df = e, miss=F, labels=c("A ban on combustion-engine cars<br>Reduce CO2 emissions from cars", "A green infrastructure program<br>Increase the use of public transport", "A green infrastructure program<br>Make electricity production greener", "A carbon tax with cash transfers<br>Reduce GHG emissions", "A carbon tax with cash transfers<br>Encourage insulation of buildings", "A carbon tax with cash transfers<br>Encourage people to drive less")))
      save_plotly_new_filename(policies_effects_US, width= 1100, height=400)})
    
    
    try({(standard_effects_US <- barres(vars = c("standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                                   export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce CO2 emissions from cars")))
      save_plotly_new_filename(standard_effects_US, width= 1100, height=200)})
    
    try({(standard_all_US <- barres(vars = c("standard_fair", "standard_costless_costly", "standard_positive_negative", "standard_large_effect", "standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                               export_xls = export_xls, df = e, miss=F, labels=c("Be fair", "Costless way<br>to fight climate change", paste0("Positive effect on ", Country_names[country], "<br>economy and employment"), paste0("Large effect on ", Country_names[country], "<br>economy and employment"), "Reduce air pollution", "Reduce CO2 emissions from cars")))
      save_plotly_new_filename(standard_all_US, width= 1100, height=380)})
    
    try({(investments_effects_US <- barres(vars = c("investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F,rev = F, rev_color = T, 
                                      export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
      save_plotly_new_filename(investments_effects_US, width= 1150, height=240)})
    
    try({(investments_all_US <- barres(vars = c("investments_costless_costly", "investments_positive_negative", "investments_large_effect", "investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F, rev = F, rev_color = T, 
                                  export_xls = export_xls, df = e, miss=F, labels=c("Costless way<br>to fight climate change", paste0("Positive effect on ", Country_names[country], "<br>economy and employment"), paste0("Large effect on ", Country_names[country], "<br>economy and employment"), "Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
      save_plotly_new_filename(investments_all_US, width= 1150, height=400)})
    
    try({(tax_transfers_effects_US <- barres(vars = c("tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F,rev = F, rev_color = T, 
                                        export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
      save_plotly_new_filename(tax_transfers_effects_US, width= 1100, height=280)})
    
    try({(tax_transfers_all_US <- barres(vars = c("tax_transfers_costless_costly", "tax_transfers_positive_negative", "tax_transfers_large_effect", "tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F, rev = F, rev_color = T, 
                                    export_xls = export_xls, df = e, miss=F, labels=c("Costless way<br>to fight climate change", paste0("Positive effect on ", Country_names[country], "<br>economy and employment"), paste0("Large effect on ", Country_names[country], "<br>economy and employment"), "Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
      save_plotly_new_filename(tax_transfers_all_US, width= 1150, height=430)})
    
    
    ##### 10. Pref on climate policies #####
    labels_support <- c("Strongly oppose", "Somewhat oppose", "Indifferent", "Somewhat support", "Strongly support")
    labels_policy <- c()
    for (v in variables_policy) labels_policy <- c(labels_policy, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    # labels_policy[5] <- "Subsidies for low-carbon technologies (renewables, CCS...)"
    # labels_policy[6] <- "Financing clean energy in low-income countries" 
    labels_policy[4] <- "Subsidies for low-carbon technologies (renewables, CCS...)"
    labels_policy[5] <- "Financing clean energy in low-income countries" # TODO!DE
    labels_policy[2] <- paste0("National tax on fossil fuels (+", tax_price_increase[country], ")") 
    try({(policy_US <- barres(vars = variables_policy, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_policy, hover=labels_support))
      save_plotly_new_filename(policy_US, width= 920, height=340)}) # TODO: policy_all with insulation_support
    
    labels_tax <- c()
    for (v in variables_tax) labels_tax <- c(labels_tax, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_tax[7] <- "Funding environmental infrastructures (e.g. public transport)"
    labels_tax[1] <- "Cash transfers to HH with no alternative to fossil fuels"
    labels_tax[8] <- "Subsidizing low-carbon technologies, including renewables"# TODO!DE
    try({(tax_US <- barres(vars = variables_tax, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, showLegend=T, labels=labels_tax, hover=labels_support))
      save_plotly_new_filename(tax_US, width= 930, height=400) })
    
    try({(tax_all_US <- barres(vars = c(variables_tax, "tax_transfers_support"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, showLegend=T, labels=c(labels_tax, "Carbon tax with cash transfers"), hover=labels_support))
      save_plotly_new_filename(tax_all_US, width= 930, height=400) })
    
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
    
    # try({(donation_US <- barres(vars = "donation", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="Donation to climate charity (in % of max ~ 100$)"))
    #   save_plotly_new_filename(donation_US, width= 1050, height=200)})
    
    try({(donation_agg_US <- barres(vars = "donation_agg", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Donation to climate charity<br>(in % of max ~ 100$)"))
      save_plotly_new_filename(donation_agg_US, width= 500, height=140)})
    
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
    
    try({(should_fight_CC_US <- barres(vars = "should_fight_CC", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels=paste(Country_names[country], "should fight CC")))
      save_plotly_new_filename(should_fight_CC_US, width= 1075, height=140)})
    
    labels_if_other_do <- c()
    for (v in variables_if_other_do) labels_if_other_do <- c(labels_if_other_do, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(if_other_do_US <- barres(vars = variables_if_other_do, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_if_other_do, hover=c("Much less", "Less", "About the same", "More", "Much more")))
      save_plotly_new_filename(if_other_do_US, width= 830, height=200) })
    
    labels_burden_sharing <- c() 
    for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_burden_sharing[4] <- "The richest countries should pay it all, <br> so that the poorest countries do not have to pay anything"
    labels_burden_sharing[3] <- "Countries should pay in proportion to <br> their past emissions (from 1990 onwards)"
    labels_burden_sharing[5] <- "The richest countries should pay even more <br> to help vulnerable countries face adverse consequences" 
    try({(burden_sharing_US <- barres(vars = rev(variables_burden_sharing), export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, sort = F, labels=rev(labels_burden_sharing)))
      save_plotly_new_filename(burden_sharing_US, width= 1150, height=325) })
    
    try({(global_assembly_support_US <- barres(vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global democratic assembly<br>on climate change"))
      save_plotly_new_filename(global_assembly_support_US, width= 990, height=140)})
    
    try({(global_tax_support_US <- barres(vars = "global_tax_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on GHG<br> financing a global basic income"))
      save_plotly_new_filename(global_tax_support_US, width= 780, height=140)})
    
    try({(tax_1p_support_US <- barres(vars = "tax_1p_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on millionaires <br>to finance low-income countries"))
      save_plotly_new_filename(tax_1p_support_US, width= 780, height=140)})
    
    labels_global_policies <- c("Global democratic assembly<br>on climate change", "Global tax on GHG<br> financing a global basic income", "Global tax on millionaires <br>to finance low-income countries")
    try({(global_policies_US <- barres(vars = c("global_assembly_support", "global_tax_support", "tax_1p_support"), export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels=labels_global_policies))
      save_plotly_new_filename(global_policies_US, width= 800, height=250)})
    
    ##### 13. Pref for bans vs. incentives #####
    
    try({(will_insulate_US <- barres(vars = "will_insulate", export_xls = export_xls, df = e, miss=F, labels="Insulate or replace heating<br>over the next 5 years"))
      save_plotly_new_filename(will_insulate_US, width= 600, height=140)})
    
    try({(insulation_support_US <- barres(vars = "insulation_support", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Mandatory insulation with subsidies"))
      save_plotly_new_filename(insulation_support_US, width= 1035, height=140)})
    
    try({(insulation_support_variant_US <- barres12(vars = "insulation_support", export_xls = export_xls, rev_color = T, rev = F,  df = list(e[e$insulation_disruption_variant==T,], e[e$insulation_disruption_variant==F,]), orig="<br>Control", comp = "Priming: renovation cause disruption", miss=F, labels="Mandatory insulation with subsidies"))
      save_plotly_new_filename(insulation_support_variant_US, width= 1035, height=240) })
    
    labels_obstacles_insulation <<- c()
    for (v in variables_obstacles_insulation) labels_obstacles_insulation <<- c(labels_obstacles_insulation, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    labels_obstacles_insulation[1] <<- "The choice is not mine"
    labels_obstacles_insulation[5] <<- "My insulation and heating systems<br>are already satisfactory"
    try({(obstacles_insulation_US <- barres(vars = variables_obstacles_insulation, export_xls = export_xls, df = e, error_margin=F, rev = F, miss = F, showLegend=F, labels=labels_obstacles_insulation, hover=labels_obstacles_insulation))
      save_plotly_new_filename(obstacles_insulation_US, width= 550, height=230) })
    
    # try({(insulation_subsidies_support_US <- barres(vars = "insulation_subsidies_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for subsidies of insulation"))
    #   save_plotly_new_filename(insulation_subsidies_support_US, width= 1030, height=170)})

    # try({(insulation_support_US <- barres(vars = c("insulation_subsidies_support", "insulation_mandatory_support"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=c("Insulation subsidized", "...and mandatory")))
    #   save_plotly_new_filename(insulation_support_US, width= 960, height=200)})
    
    labels_beef <- c()
    for (v in variables_beef) labels_beef <- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(beef_US <- barres(vars = variables_beef, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels=labels_beef,showLegend=T, hover= labels_support))
      save_plotly_new_filename(beef_US, width= 900, height=240)  })
    
    
    ##### 14. Trust, perceptions of institutions, etc. #####
    
    try({(can_trust_people_US <- barres(vars = "can_trust_people", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust other people"))
      save_plotly_new_filename(can_trust_people_US, width= 1060, height=140)})
    
    try({(can_trust_govt_US <- barres(vars = "can_trust_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels=paste("Trust the", Country_names[country], "government <br> over the last decade to do what is right")))
      save_plotly_new_filename(can_trust_govt_US, width= 875, height=140)})
    
    try({(view_govt_US <- barres(vars = "view_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="View on government intervention"))
      save_plotly_new_filename(view_govt_US, width= 700, height=140)})
    
    try({(problem_inequality_US <- barres(vars = "problem_inequality", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=paste("Income inequality in", Country_names[country])))
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
    
    try({(vote_participation_US <- barres(vars = "vote_participation", export_xls = export_xls, df = e, miss=T, labels="Voted in last election"))
      save_plotly_new_filename(vote_participation_US, width= 540, height=140) })
    
    # try({(vote_participation_2016_US <- barres(vars = "vote_participation_2016", export_xls = export_xls, df = e, miss=T, labels="Voted in 2016 election"))
    #   save_plotly_new_filename(vote_participation_2016_US, width= 540, height=140) })
    
    try({(vote_US <- barres(vars = "vote", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="Vote or hypothetical vote at last election"))
      save_plotly_new_filename(vote_US, width= 1000, height=250) })
    
    # try({(vote_2020 <- barres(vars = "vote_2020", export_xls = export_xls, df = e, rev_color = T, miss=T, fr = "PNR/no right", labels="In 2020, voted for"))
    #   save_plotly_new_filename(vote_2020, width= 610, height=140) })
    # 
    # try({(vote_2016_US <- barres(vars = "vote_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for"))
    #   save_plotly_new_filename(vote_2016_US, width= 650, height=140) })
    
    try({(vote_all_US <- barres(vars = c("vote_non_voters", "vote_voters", "vote"), export_xls = export_xls, df = e, rev_color = T, miss=F, sort = F, labels=c("Non-voters", "Voters", "Last election: Voters and non-voters")))
      save_plotly_new_filename(vote_all_US, width= 1200, height=350)}) # TODO make it work even if Jorgensen/Hawkins don't appear everywhere
    
    try({(vote_voters_US <- barres(vars = "vote_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="Vote in last election (voters)"))
      save_plotly_new_filename(vote_voters_US, width= 1000, height=250)})
    
    try({(vote_non_voters_US <- barres(vars = "vote_non_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="Vote in last election (non-voters)"))
      save_plotly_new_filename(vote_non_voters_US, width= 1000, height=250)})
    
    # try({(vote_voters_2016_US <- barres(vars = "vote_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (voters)"))
    #   save_plotly_new_filename(vote_voters_2016_US, width= 650, height=140) })
    # 
    # try({(vote_non_voters_2016_US <- barres(vars = "vote_non_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (non-voters)"))
    #   save_plotly_new_filename(vote_non_voters_2016_US, width= 650, height=140)})
    
    try({(left_right_US <- barres(vars = "left_right", export_xls = export_xls, df = e, rev_color = T, rev = F, miss=T, labels="On economic policy matters, are you..."))
      save_plotly_new_filename(left_right_US, width= 950, height=140)})
    
    try({(political_affiliation_US <- barres(vars = "political_affiliation", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Political affiliation"))
      save_plotly_new_filename(political_affiliation_US, width= 800, height=140)})
    
    ##### 16. CC field, feedback #####
    
    try({(survey_biased_US <- barres(vars = "survey_biased", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Survey biased"))
      save_plotly_new_filename(survey_biased_US, width= 810, height=140)})
    
    try({(CC_field_mentions_raw_US <- barres(vars = var_CC_field_names[6:18], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=as.character(CC_field_names_names[6:18])))
      save_plotly_new_filename(CC_field_mentions_raw_US, width= 400, height=450)})
    
    try({(CC_field_sectors_US <- barres(vars = var_CC_field_names[9:14], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=as.character(CC_field_names_names[9:14])))
      save_plotly_new_filename(CC_field_sectors_US, width= 400, height=300)})
    
    try({(CC_field_instruments_US <- barres(vars = var_CC_field_names[c(8,15:18)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=as.character(CC_field_names_names[c(8,15:18)])))
      save_plotly_new_filename(CC_field_instruments_US, width= 400, height=280)})
    
    try({(CC_field_contains_US <- barres(vars = variables_CC_field_contains, export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=sub("_", " ", sub("CC_field_contains_", "", variables_CC_field_contains))))
      save_plotly_new_filename(CC_field_contains_US, width= 300, height=600)})
    
    try({(CC_field_mentions_US <- barres(vars = c("should_act_CC_field", "measure_proposed_CC_field", "CC_field_no_worry", "CC_field_do_not_know", "CC_field_na"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=c("Action needed", "Measure proposed", "No action needed", "Do not know", "Empty")))
      save_plotly_new_filename(CC_field_mentions_US, width= 430, height=270)})
    
    try({(CC_field_nb_actions_US <- barres(vars = "nb_actions_CC_field", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="# supported actions mentioned"))
      save_plotly_new_filename(CC_field_nb_actions_US, width= 570, height=140)})
    
    try({(CC_field_nb_elements_US <- barres(vars = "nb_elements_CC_field", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="# supported elements in CC field"))
      save_plotly_new_filename(CC_field_nb_elements_US, width= 650, height=140)})
    
    try({(comment_field_contains_US <- barres(vars = variables_comment_field_contains, export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=sub("_", " ", sub("comment_field_contains_", "", variables_comment_field_contains))))
      save_plotly_new_filename(comment_field_contains_US, width= 250, height=250)})
    
    try({(comment_field_mentions_raw_US <- barres(vars = var_comment_field_names, export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=comment_field_names))
      save_plotly_new_filename(comment_field_mentions_raw_US, width= 400, height=450)})
    
    try({(comment_field_feeling_US <- barres(vars = c("comment_field_good", "critic_comment_field"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=c("Liked", "Disliked")))
      save_plotly_new_filename(comment_field_feeling_US, width= 2000, height=150)})
    
    try({(comment_field_mentions_US <- barres(vars = c("non_empty_comment_field", "comment_field_good", "critic_comment_field"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=c("Non empty", "Liked", "Disliked")))
      save_plotly_new_filename(comment_field_mentions_US, width= 400, height=150)})
    
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
      
      try({(temp <- barres12(vars = paste(names_policies, "positive_negative", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
        save_plotly_new_filename(temp, filename = paste0("policies_positive_negative", text_file), width= 1100, height=380)})
      
      try({(temp <- barres12(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
        save_plotly_new_filename(temp, filename = paste0("policies_win_lose_self", text_file), width= 870, height=380)})
      
      try({(temp <- barres12(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, sort = F, labels=labels_policies))
        save_plotly_new_filename(temp, filename = paste0("policies_support", text_file), width= 1020, height=380)})
      
      try({(temp <- barres12(vars = "donation_agg", export_xls = export_xls, df = dfs, comp = comp, orig = orig, miss=F, rev = F, rev_color = T, labels="Donation to climate charity<br>(in % of max ~ 100$)"))
        save_plotly_new_filename(temp, filename = paste0("donation_agg", text_file), width= 600, height=220)})
      
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
    
    plot_heterogeneityN <- function(df = e, along = "vote_agg", text_file = paste0(replacement_text, "_vote"), export_xls = F) {
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
      
      try({(temp <- barresN(vars = paste(names_policies, "positive_negative", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
          save_plotly_new_filename(temp, filename = paste0("policies_positive_negative", text_file), width= 1100, height=fig_height(3*length(levels)))})
      
      try({(temp <- barresN(vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
          save_plotly_new_filename(temp, filename = paste0("policies_win_lose_self", text_file), width= 870, height=fig_height(3*length(levels)))})
      
      try({(temp <- barresN(vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = df, along = along, miss=F, labels=labels_policies))
          save_plotly_new_filename(temp, filename = paste0("policies_support", text_file), width= 1160, height=fig_height(3*length(levels)))})
      
      try({(temp <- barresN(vars = "donation_agg", export_xls = export_xls, df = df, along = along, miss=F, rev = F, rev_color = T, labels="Donation to climate charity<br>(in % of max ~ 100$)"))
          save_plotly_new_filename(temp, filename = paste0("donation_agg", text_file), width= 650, height=fig_height(1*length(levels)))})
      
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
    try({plot_heterogeneity12(dfs = list(e[e$income %in% c("Q1", "Q2"),], e[e$income %in% c("Q3", "Q4"),]), orig="<br>(Bottom 50%)", comp = "(Top 50%)", text_file = paste0(replacement_text, "_inc"), export_xls = export_xls)})
    # By vote
    # try({plot_heterogeneity12(dfs = list(e[e$vote < 0.5,], e[e$vote >= 0,]), comp = "(Left)", orig="<br>(Center or right)", text_file = paste0(replacement_text, "_pol"), export_xls = export_xls)})
    if (mean(e$left_right < -0.5) < mean(e$left_right > 0)) try({plot_heterogeneity12(dfs = list(e[e$left_right > 0,], e[e$left_right %in% c(-2,-1,0),]), comp = "(Right)", orig="<br>(Center or left)", text_file = paste0(replacement_text, "_pol"), export_xls = export_xls)})
    else try({plot_heterogeneity12(dfs = list(e[e$left_right < -0.5,], e[e$left_right >= 0,]), comp = "(Left)", orig="<br>(Center or right)", text_file = paste0(replacement_text, "_pol"), export_xls = export_xls)})
    if ("vote3" %in% names(e)) try({plot_heterogeneityN(along = "vote3", text_file = paste0(replacement_text, "_vote"), export_xls = export_xls)})
    else try({plot_heterogeneityN(along = "vote_agg", text_file = paste0(replacement_text, "_vote"), export_xls = export_xls)})
    
    # # By rural/urban 
    try({plot_heterogeneity12(dfs = list(e[e$urban==T,], e[e$urban==F,]), orig="<br>(Urban)", comp = "(Rural)", text_file = paste0(replacement_text, "_urb"), export_xls = export_xls)})
    
    # # By treatment
    try({plot_heterogeneityN(along = "treatment", text_file = paste0(replacement_text, "_treat"), export_xls = export_xls)})
    
    ## Other
    try({(wtp_US_anthropogenic <- barres12(vars = "wtp", export_xls = export_xls, df = list(e[e$CC_anthropogenic == "Most",], e[e$CC_anthropogenic <= 0,]), comp = "<br>(CC not mainly anthropogenic)", orig="<br>(CC anthropogenic)", miss=F, labels="WTP to limit global warming ($/year)"))
      save_plotly_new_filename(wtp_US_anthropogenic, width= 830, height=220)})
    
    missing_figures <- setdiff(sub("_US", "", list.files("../figures/US")), sub(replacement_text, "", list.files(folder)))
    cat(paste0(length(missing_figures), " Missing figures for ", country, ": "))
    missing_types <- c("_comp.png", "_pol.png", "_urb.png", "_vote.png")
    for (t in missing_types) if (grepl("_comp.png", missing_figures)) print("All figures of type: ", t)
    missing_figures <- missing_figures[!grepl(paste(missing_types, collapse="|"), missing_figures)]
    # cat(paste0(length(missing_figures), " Missing figures for ", country, ": "), missing_figures, sep = '\n')
    cat(missing_figures, sep = '\n')
    cat(" ")
  }
  
  print(Sys.time() - start)
}

# /!\ open the Plots pane at its maximum before running the function
render_country_comparison <- function(data = all, along = "country_name", parentheses = F, nolabel = T, on_control = T, export_xls = F, folder_country = F, name_country = T, figures = T, tables = T, heatmap_conditions = c("", ">= 1", "/"), alphabetical = FALSE, special = c("World", "OECD", "Non-OECD")) { # c("", ">= 1", "<= -1", ">= 0", "<= 0", "== 2", "== -2")
  start <- Sys.time()
  
  e <- data
  if (on_control) e <- e[e$treatment=="None",]
  levels <- Levels(e[[along]])
  nb_levels <- length(levels)
  
  if (folder_country) folder <- '../figures/country_comparison/'
  else folder <- '../figures/'
  replacement_text <- ifelse(name_country, "_countries", "")
  
  save_plotly_new_filename <- function(plot, filename = NULL, width = dev.size('px')[1], height = dev.size('px')[2], method='orca', trim = T) {
    if (missing(filename)) filename <- sub("_US", replacement_text, deparse(substitute(plot)))
    return(save_plotly(plot, filename = filename, folder = folder, width = width, height = height, method=method, trim = trim))
  }
  
  if (figures) { 
    # To get legend on one line with Suuport...1-5 likert: 1160 / Agree...: 1300 (for Support: min 1070/1110/1160 for one line / wide bars / legend not exceeding the bars)
    
    ##### Pre-treatment ## #####
    
    # ##### 1. Demographics ##### TODO: comp
    try({(gender_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "gender_factor", export_xls = export_xls, df = e, miss = F, labels="Gender"))
      save_plotly_new_filename(gender_US, width= 470, height=fig_height(1*nb_levels))})
    
    try({(age_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "age", export_xls = export_xls, df = e[e$age!="Below 18",], miss=F, rev = F, labels="Age"))
      save_plotly_new_filename(age_US, width= 500, height=fig_height(1*nb_levels)) })
    
    # try({(region_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "region", export_xls = export_xls, df = e, miss=F, labels="Region"))
    #   save_plotly_new_filename(region_US, width= 800, height=fig_height(1*nb_levels))})
    # 
    try({(urbanity_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "urbanity", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Size of town"))
      save_plotly_new_filename(urbanity_US, width= 820, height=fig_height(1*nb_levels))})
    
    try({(urban_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "urban", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Urban"))
      save_plotly_new_filename(urban_US, width= 660, height=fig_height(1*nb_levels))})
    
    # try({(race_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_race[c(1:4)], export_xls = export_xls, df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
    #   save_plotly_new_filename(race_US, width= 340, height=fig_height(3*nb_levels))}) 
    
    try({(dominant_origin_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "dominant_origin", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Origin: largest group"))
      save_plotly_new_filename(dominant_origin_US, width= 400, height=fig_height(1*nb_levels))})
    
    try({(income_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "income", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
      save_plotly_new_filename(income_US, width= 510, height=fig_height(1*nb_levels))})
    
    try({(wealth_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "wealth", export_xls = export_xls, df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
      save_plotly_new_filename(wealth_US, width= 480, height=fig_height(1*nb_levels))})
    
    try({(education_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "education", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
      save_plotly_new_filename(education_US, width= 1300, height=fig_height(1*nb_levels)) })
    
    try({(diploma_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "diploma", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Highest diploma"))
      save_plotly_new_filename(diploma_US, width= 725, height=fig_height(1*nb_levels)) })
    
    try({(employment_status_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "employment_agg", export_xls = export_xls, df = e, miss=F, labels="What is your employment status?"))
      save_plotly_new_filename(employment_status_US, width= 630, height=fig_height(1*nb_levels))})
    
    try({(occupation_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "occupation", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Main occupation"))
      save_plotly_new_filename(occupation_US, width= 610, height=fig_height(1*nb_levels))})
    
    #try({(sector_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "sector", export_xls = export_xls, df = e, miss=F, labels="What is your primary line of business?"))
    #  save_plotly_new_filename(sector_US, width= 630, height=fig_height(1*nb_levels))})
    
    try({(polluting_sector_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "polluting_sector", export_xls = export_xls, df = e, miss=F, showLegend = F, rev_color = T, rev = F, labels="Current (or last) job is in a polluting sector."))
      save_plotly_new_filename(polluting_sector_US, width= 600, height=fig_height(1*nb_levels))})
    
    try({(hit_by_covid_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "hit_by_covid", export_xls = export_xls, df = e, miss=F, labels="Hit by covid"))
      save_plotly_new_filename(hit_by_covid_US, width= 420, height=fig_height(1*nb_levels))})
    
    labels_home <<- c()
    for (v in variables_home) labels_home <<- c(labels_home, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(home_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_home, export_xls = export_xls, df = e, rev = F, miss = F, showLegend=F, labels=labels_home, hover=labels_home))
      save_plotly_new_filename(home_US, width= 575, height=fig_height(4*nb_levels)) })
    
    try({(couple_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "couple", export_xls = export_xls, df = e, miss=F, labels="Do you live with your partner?"))
      save_plotly_new_filename(couple_US, width= 630, height=fig_height(1*nb_levels))})
    
    try({(marital_status_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "marital_status", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="What is your marital status?"))
      save_plotly_new_filename(marital_status_US, width= 830, height=fig_height(1*nb_levels))})
    
    try({(nb_children_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "Nb_children", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F, labels="Number of children", legend = c(0:3, "4 or more")))
      save_plotly_new_filename(nb_children_US, width= 470, height=fig_height(1*nb_levels))})
    
    try({(hh_size_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "HH_size", export_xls = export_xls, df = e, miss=F, rev_color = T, rev = F,labels="People in household"))
      save_plotly_new_filename(hh_size_US, width= 500, height=fig_height(1*nb_levels))})
    
    ##### 2. HH composition and energy characteristics #####
    
    try({(heating_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "heating", export_xls = export_xls, df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
      save_plotly_new_filename(heating_US, width= 525, height=fig_height(1*nb_levels))})
    
    try({(heating_expenses_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "heating_expenses", export_xls = export_xls, df = e, miss=T, fr="Don't know", labels="Monthly heating expenses", rev = F))
      save_plotly_new_filename(heating_expenses_US, width= 950, height=fig_height(1*nb_levels))})
    
    try({(insulation_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "insulation", export_xls = export_xls, df = e, miss=F, rev_color = T, labels="Quality of insulation", rev = F))
      save_plotly_new_filename(insulation_US, width= 690, height=fig_height(1*nb_levels))})
    
    try({(gas_expenses_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "gas_expenses", export_xls = export_xls, df = e, miss=F, labels="Monthly gas expenses", rev = F))
      save_plotly_new_filename(gas_expenses_US, width= 900, height=fig_height(1*nb_levels))})
    
    try({(flights_3y_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "flights_3y", export_xls = export_xls, df = e, miss=F, rev = F, labels="Round-trip flights between 2017 and 2019"))
      save_plotly_new_filename(flights_3y_US, width= 800, height=fig_height(1*nb_levels))})
    
    try({(frequency_beef_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "frequency_beef", export_xls = export_xls, df = e, miss=F, rev = F, labels="How often do you eat beef?"))
      save_plotly_new_filename(frequency_beef_US, width= 720, height=fig_height(1*nb_levels))})
    
    variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
    labels_transport <- c("Work", "Shopping", "Leisure")
    try({(transport_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_transport_graph, export_xls = export_xls, df = e, rev = F, miss = F,rev_color = T,  labels=labels_transport))
      save_plotly_new_filename(transport_US, width= 850, height=fig_height(3*nb_levels)) })
    
    try({(availability_transport_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "availability_transport", export_xls = export_xls, df = e, rev_color = T, rev = F, miss=F, labels="Quality and availability of public transport near your home"))
      save_plotly_new_filename(availability_transport_US, width= 870, height=fig_height(1*nb_levels))})
    
    ##### POST-TREATMENT #####
    
    ##### 3. Treatment feedback: local climate #####
    try({(watched_climate_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "watched_climate", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (local)"))
      save_plotly_new_filename(watched_climate_US, width= 900, height=fig_height(1*nb_levels))})
    
    # try({(know_treatment_climate_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("know_temperature_2100", "know_local_damage"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Knowledge climate video"))
    #   save_plotly_new_filename(know_treatment_climate_US, width= 580, height=fig_height(1*nb_levels))})
    
    try({(know_treatment_climate_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Score knowledge climate video"))
      save_plotly_new_filename(know_treatment_climate_US, width= 580, height=fig_height(1*nb_levels))})
    
    # try({(know_treatment_climate_watched_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "know_treatment_climate", rev = F, rev_color = T, export_xls = export_xls, df = e[e$watched_climate=='Yes',], miss=F, labels="Score knowledge climate video"))
    #   save_plotly_new_filename(know_treatment_climate_watched_US, width= 580, height=fig_height(1*nb_levels))})
    
    ##### 4. Treatment feedback: local policy #####
    try({(watched_policy_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "watched_policy", export_xls = export_xls, df = e, miss=F, labels="Able to watch the video until the end (policy)"))
      save_plotly_new_filename(watched_policy_US, width= 900, height=fig_height(1*nb_levels))})
    
    try({(know_treatment_policy_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "know_treatment_policy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="Score knowledge policy video"))
      save_plotly_new_filename(know_treatment_policy_US, width= 580, height=fig_height(1*nb_levels))})
    
    # # labels_know_treatment <- c("Knows CC would cause 70 days of<br>heatwaves per year in the US by 2100", "Knows temperature would be +8°F<br>by 2100 with current trend emissions", "Knows first policy presented is<br>ban on combustion-engine cars", "Knows green infrastructure program<br>described would be debt-funded")
    # labels_know_treatment <- c("Knows CC would cause 70 days of<br>heatwaves per year in the US by 2100", "Knows temperature would be +8 Fahrenheit<br>by 2100 with current trend emissions", "Knows first policy presented is<br>ban on combustion-engine cars", "Knows green infrastructure program<br>described would be debt-funded")
    try({(know_temperature_2100_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "know_temperature_2100_correct", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels="Knows temperature would be +4°C<br>by 2100 with current trend emissions"))
      save_plotly_new_filename(know_temperature_2100_US, width= 830, height=fig_height(4*nb_levels))}) 
    
    try({(know_ban_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "know_ban_correct", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels="Knows first policy presented is<br>ban on combustion-engine cars"))
      save_plotly_new_filename(know_ban_US, width= 830, height=fig_height(4*nb_levels))}) 
    
    try({(know_investments_funding_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "know_investments_funding_correct", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels="Knows green infrastructure program<br>described would be debt-funded"))
      save_plotly_new_filename(know_investments_funding_US, width= 830, height=fig_height(4*nb_levels))}) 
    
    ##### 5. Climate knowledge #####
    
    try({(CC_talks_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_talks", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="How often do you talk about climate change?"))
      save_plotly_new_filename(CC_talks_US, width= 760, height=fig_height(1*nb_levels))})
    
    try({(CC_real_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_real", export_xls = export_xls, df = e, miss=F, labels="Climate change real?"))
      save_plotly_new_filename(CC_real_US, width= 540, height=fig_height(1*nb_levels))})
    
    try({(CC_anthropogenic_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_anthropogenic", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Part of climate change anthropogenic"))
      save_plotly_new_filename(CC_anthropogenic_US, width= 660, height=fig_height(1*nb_levels))})
    
    try({(CC_anthropogenic_US_CI <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_anthropogenic", error_margin = T, rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Part of climate change anthropogenic"))
      save_plotly_new_filename(CC_anthropogenic_US_CI, width= 660, height=fig_height(1*nb_levels))})
    
    try({(CC_anthropogenic_non_deniers_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_anthropogenic", rev = F, rev_color = T, export_xls = export_xls, df = e[e$CC_real!="No",], miss=F, labels="Part of climate change anthropogenic"))
      save_plotly_new_filename(CC_anthropogenic_non_deniers_US, width= 660, height=fig_height(1*nb_levels))})
    
    try({(CC_problem_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_problem", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Climate change is an important problem."))
      save_plotly_new_filename(CC_problem_US, width= 1300, height=fig_height(1*nb_levels)) })
    
    try({(CC_knowledgeable_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_knowledgeable", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="How knowledgeable about climate change"))
      save_plotly_new_filename(CC_knowledgeable_US, width= 800, height=fig_height(1*nb_levels))})
    
    try({(CC_dynamic_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_dynamic", export_xls = export_xls, df = e, rev=F, miss=F, labels="Cutting GHG emissions by half <br> sufficient to stop rise in temperatures"))
      save_plotly_new_filename(CC_dynamic_US, width= 600, height=fig_height(1*nb_levels))})
    
    try({(GHG_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("GHG_CO2", "GHG_methane", "GHG_H2", "GHG_particulates"), export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, showLegend = F, labels=c("CO<sub>2</sub>", "Methane", "Hydrogen", "Particulates")))
      save_plotly_new_filename(GHG_US, width= 270, height=fig_height(4*nb_levels))})
    
    try({(score_GHG_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "score_GHG", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, labels="Knowledge score on GHG")) # TODO: add "0 (all wrong)" "4 (all right)"
      save_plotly_new_filename(score_GHG_US, width= 550, height=fig_height(1*nb_levels))}) 
    
    try({(footprint_elec_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = Variables_footprint$el[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$el[c(2,1,3)]))
      save_plotly_new_filename(footprint_elec_US, width= 400, height=fig_height(1*nb_levels)) })
    
    try({(footprint_transport_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = Variables_footprint$tr[c(2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$tr[c(2,1,3)]))
      save_plotly_new_filename(footprint_transport_US, width= 400, height=fig_height(1*nb_levels)) })
    
    try({(footprint_food_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = Variables_footprint$fd[c(2,3,1)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, legend = c("1 Most", "2", "3 Least"), labels=Labels_footprint$fd[c(2,3,1)]))
      save_plotly_new_filename(footprint_food_US, width= 400, height=fig_height(1*nb_levels)) }) 
    
    try({(footprint_region_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(Variables_footprint$reg, "original", sep="_")[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$reg[c(4,2,1,3)]))
      save_plotly_new_filename(footprint_region_US, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(footprint_region_no_miss_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = Variables_footprint$reg[c(4,2,1,3)], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$reg[c(4,2,1,3)]))
      save_plotly_new_filename(footprint_region_no_miss_US, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(footprint_pc_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(Variables_footprint$pc, "original", sep="_")[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, legend = c("1 Most", "2", "3", "4 Least", "PNR"), labels=Labels_footprint$pc[4:1]))
      save_plotly_new_filename(footprint_pc_US, width= 640, height=fig_height(1*nb_levels)) })

    try({(footprint_pc_no_miss_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = Variables_footprint$pc[4:1], export_xls = export_xls, df = e, rev = F, rev_color = T, miss=F, legend = c("1 Most", "2", "3", "4 Least"), labels=Labels_footprint$pc[4:1]))
      save_plotly_new_filename(footprint_pc_no_miss_US, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(footprint_elec_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_el", export_xls = export_xls, df = e, miss=F, labels="Smallest electricity footprint"))
      save_plotly_new_filename(footprint_elec_US_least, width= 470, height=fig_height(1*nb_levels)) })
    
    try({(footprint_transport_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_tr", export_xls = export_xls, df = e, miss=F, labels="Smallest transport footprint"))
      save_plotly_new_filename(footprint_transport_US_least, width= 470, height=fig_height(1*nb_levels)) })
    
    try({(footprint_food_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_fd", export_xls = export_xls, df = e, miss=F, labels="Smallest food footprint"))
      save_plotly_new_filename(footprint_food_US_least, width= 470, height=fig_height(1*nb_levels)) })
    
    try({(footprint_region_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_no_pnr_reg", export_xls = export_xls, df = e, miss=T, labels="Smallest regional emissions"))
      save_plotly_new_filename(footprint_region_US_least, width= 640, height=fig_height(1*nb_levels)) })
    
    # try({(footprint_region_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_reg", export_xls = export_xls, df = e, miss=T, labels="Smallest regional emissions"))
    #   save_plotly_new_filename(footprint_region_US_least, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(footprint_pc_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_no_pnr_pc", export_xls = export_xls, df = e, miss=T, labels="Smallest regional footprint per capita"))
      save_plotly_new_filename(footprint_pc_US_least, width= 1100, height=fig_height(1*nb_levels)) }) # 640
    
    # try({(footprint_pc_US_least <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "least_footprint_pc", export_xls = export_xls, df = e, miss=T, labels="Smallest regional footprint per capita"))
    #   save_plotly_new_filename(footprint_pc_US_least, width= 1100, height=fig_height(1*nb_levels)) }) # 640
    
    try({(footprint_elec_US_most <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "most_footprint_el", export_xls = export_xls, df = e, miss=F, labels="Largest electricity footprint"))
      save_plotly_new_filename(footprint_elec_US_most, width= 470, height=fig_height(1*nb_levels)) })
    
    try({(footprint_transport_US_most <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "most_footprint_tr", export_xls = export_xls, df = e, miss=F, labels="Largest transport footprint"))
      save_plotly_new_filename(footprint_transport_US_most, width= 470, height=fig_height(1*nb_levels)) })
    
    try({(footprint_food_US_most <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "most_footprint_fd", export_xls = export_xls, df = e, miss=F, labels="Largest food footprint"))
      save_plotly_new_filename(footprint_food_US_most, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(footprint_region_US_most <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "most_footprint_reg", export_xls = export_xls, df = e, miss=T, labels="Largest regional emissions"))
      save_plotly_new_filename(footprint_region_US_most, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(footprint_pc_US_most <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "most_footprint_pc", export_xls = export_xls, df = e, miss=T, labels="Largest regional footprint per capita"))
      save_plotly_new_filename(footprint_pc_US_most, width= 640, height=fig_height(1*nb_levels)) })
    
    try({(score_footprint_transport_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "score_footprint_transport", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>transport footprint"))
      save_plotly_new_filename(score_footprint_transport_US, width= 520, height=fig_height(1*nb_levels)) })
    
    try({(score_footprint_food_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "score_footprint_food", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>food footprint"))
      save_plotly_new_filename(score_footprint_food_US, width= 520, height=fig_height(1*nb_levels))})
    
    try({(score_footprint_pc_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "score_footprint_pc", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance to true ranking of<br>regional per capita footprint"))
      save_plotly_new_filename(score_footprint_pc_US, width= 520, height=fig_height(1*nb_levels))})
    
    try({(score_footprint_region_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "score_footprint_region", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance to true ranking of<br>regional total footprint"))
      save_plotly_new_filename(score_footprint_region_US, width= 520, height=fig_height(1*nb_levels))})
    
    try({(score_footprint_regions_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("score_footprint_pc", "score_footprint_region"), rev = F, export_xls = export_xls, df = e, miss=T, labels=c("per capita regional footprint", "Distance to true ranking of<br>regional total footprint")))
      save_plotly_new_filename(score_footprint_regions_US, width= 550, height=fig_height(2*nb_levels))})
    
    try({(score_footprint_elec_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "score_footprint_elec", rev = F, export_xls = export_xls, df = e, miss=F, labels="Distance true ranking<br>electricity footprint"))
      save_plotly_new_filename(score_footprint_elec_US, width= 520, height=fig_height(1*nb_levels))})
    
    try({(scores_footprint_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("score_footprint_transport", "score_footprint_elec", "score_footprint_food"), rev = F, export_xls = export_xls, df = e, miss=F, labels=c("Transport", "Electricity","Food")))
      save_plotly_new_filename(scores_footprint_US, width= 500, height=fig_height(3*nb_levels))})
    
    labels_CC_impacts <- c()
    for (v in variables_CC_impacts) labels_CC_impacts <- c(labels_CC_impacts, sub('.* - ', '', sub('.*: ', '', Label(e[[v]])))) # TODO? change color/rev?
    try({(CC_impacts_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_CC_impacts, export_xls = export_xls, df = e, miss=F, rev_color=T, labels=labels_CC_impacts))
      save_plotly_new_filename(CC_impacts_US, width= 800, height=fig_height(5*nb_levels)) })
    
    try({for (i in 1:length(variables_CC_impacts)) { (temp <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_CC_impacts[i], export_xls = export_xls, df = e, miss=F, rev_color=T, labels=labels_CC_impacts[i]))
      save_plotly_new_filename(temp, filename = paste0(variables_CC_impacts[i], replacement_text), width= 900, height=fig_height(1*nb_levels))  }})
    
    ##### 6. Climate Change (attitudes and risks) ##### 
    labels_responsible <- c()
    for (v in variables_responsible_CC) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(CC_responsible_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_responsible_CC, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_responsible, hover= label_great_deal))
      save_plotly_new_filename(CC_responsible_US, width= 720, height=fig_height(5*nb_levels)) })
    
    try({(net_zero_feasible_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "net_zero_feasible", export_xls = export_xls, df = e, miss=F, rev = F, rev_color=T, labels="Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in [Country]"))
      save_plotly_new_filename(net_zero_feasible_US, width= 770, height=fig_height(1*nb_levels))})
    
    try({(CC_affects_self_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_affects_self", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Climate change negatively affects personal life"))
      save_plotly_new_filename(CC_affects_self_US, width= 800, height=fig_height(1*nb_levels)) })
    
    try({(pro_ambitious_policies_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "pro_ambitious_policies", export_xls = export_xls, df = e,rev = F, rev_color = T, miss = F, labels="Ambitious public policies needed<br>to halt climate change"))
      save_plotly_new_filename(pro_ambitious_policies_US, width= 670, height=fig_height(1*nb_levels))})
    
    # try({(CC_attitude_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("net_zero_feasible", "CC_affects_self", "pro_ambitious_policies"), export_xls = export_xls, df = e,rev = F, rev_color = T, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in the U.S.", "Climate change negatively affects personal life", "Ambitious public policies needed<br>to halt climate change")))
    #   save_plotly_new_filename(CC_attitude_US, width= 770, height=fig_height(5*nb_levels))})
    
    try({(CC_attitude_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("net_zero_feasible", "CC_affects_self"), export_xls = export_xls, df = e,rev = F, rev_color = T, miss = F, labels=c("Feasible to stop GHG emissions<br>while maintaining satisfactory<br>standards of living in [Country]", "Climate change negatively affects personal life")))
      save_plotly_new_filename(CC_attitude_US, width= 750, height=fig_height(2*nb_levels))})
    
    try({(CC_will_end_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "CC_will_end", export_xls = export_xls, df = e, miss = F, labels="Likely to halt CC by the end of the century"))
      save_plotly_new_filename(CC_will_end_US, width= 855, height=fig_height(1*nb_levels))})
    
    try({(effect_halt_CC_economy_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "effect_halt_CC_economy", export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels="Effects of ambitious policies <br> on the U.S economy and employment"))
      save_plotly_new_filename(effect_halt_CC_economy_US, width= 850, height=fig_height(1*nb_levels))})
    
    try({(effect_halt_CC_lifestyle_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "effect_halt_CC_lifestyle", export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels="Negative effects of ambitious policies on lifestyle"))
      save_plotly_new_filename(effect_halt_CC_lifestyle_US, width= 770, height=fig_height(1*nb_levels))})
    
    labels_willing <- c()
    for (v in variables_willing) labels_willing <- c(labels_willing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(willing_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_willing, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_willing, hover=label_great_deal))
      save_plotly_new_filename(willing_US, width= 800, height=fig_height(5*nb_levels)) })
    
    labels_condition <- c()
    for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(condition_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_condition, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_condition, hover = label_great_deal))
      save_plotly_new_filename(condition_US, width= 805, height=fig_height(4*nb_levels)) })
    
    ##### 7. Pref 1: emission standards #####
    # try({ temp <- heatmap_table(vars = variables_standard_effect, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_standard_effects_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "standard_effects_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_standard_effect, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_standard_effects_short
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "standard_effects_mean", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_standard_effect, along = along, conditions = c("< 0"), on_control = T)
    # row.names(temp) <- labels_standard_effects_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "standard_effects_disagree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_standard_win_lose, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_standard_win_lose
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "standard_win", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_standard_win_lose, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_standard_win_lose
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "standard_win_lose_mean", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_standard_win_lose, along = along, conditions = c("< 0"), on_control = T)
    # row.names(temp) <- labels_standard_win_lose
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "standard_lose", replacement_text), width = 800, height = 400)})
    
    try({(standard_fair_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "standard_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Ban on combustion-engine cars fair"))
      save_plotly_new_filename(standard_fair_US, width= 1300, height=fig_height(1*nb_levels))})
    
    try({(standard_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "standard_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Ban on combustion-engine cars"))
      save_plotly_new_filename(standard_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    try({(standard_public_transport_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "standard_public_transport_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Ban on combustion-engine cars with<br>alternatives such as public transports available"))
      save_plotly_new_filename(standard_public_transport_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    ##### 8. Pref 2: Green investments #####
    # try({ temp <- heatmap_table(vars = variables_investments_effect, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_investments_effects_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "investments_effects_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_investments_effect, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_investments_effects_short
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "investments_effects_mean", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_investments_effect, along = along, conditions = c("< 0"), on_control = T)
    # row.names(temp) <- labels_investments_effects_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "investments_effects_disagree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_investments_effect, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_investments_win_lose
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "investments_win", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_investments_win_lose, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_investments_win_lose
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "investments_win_lose_mean", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_investments_win_lose, along = along, conditions = c("< 0"), on_control = T)
    # row.names(temp) <- labels_investments_win_lose
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "investments_lose", replacement_text), width = 800, height = 400)})
    
    try({(investments_fair_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "investments_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Green infrastructure program is fair"))
      save_plotly_new_filename(investments_fair_US, width= 1300, height=fig_height(1*nb_levels))})
    
    try({(investments_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "investments_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Green infrastructure program"))
      save_plotly_new_filename(investments_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    labels_investments_funding <- c()
    for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(investments_funding_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_investments_funding, export_xls = export_xls, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
      save_plotly_new_filename(investments_funding_US, width= 560, height=fig_height(5*nb_levels))})
    
    ##### 9. Pref 3: Tax and dividend #####
    # try({ temp <- heatmap_table(vars = variables_tax_transfers_effect, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_tax_transfers_effects_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "tax_transfers_effects_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax_transfers_effect, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_tax_transfers_effects_short
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "tax_transfers_effects_mean", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax_transfers_effect, along = along, conditions = c("< 0"), on_control = T)
    # row.names(temp) <- labels_tax_transfers_effects_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "tax_transfers_effects_disagree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax_transfers_win_lose, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_tax_transfers_win_lose
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "tax_transfers_win", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax_transfers_win_lose, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_tax_transfers_win_lose
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "tax_transfers_win_lose_mean", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax_transfers_win_lose, along = along, conditions = c("< 0"), on_control = T)
    # row.names(temp) <- labels_tax_transfers_win_lose
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "tax_transfers_lose", replacement_text), width = 800, height = 400)})
    
    try({(tax_transfers_fair_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "tax_transfers_fair", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers is fair"))
      save_plotly_new_filename(tax_transfers_fair_US, width= 1300, height=fig_height(1*nb_levels))})
    
    try({(tax_transfers_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "tax_transfers_support", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Tax with cash transfers"))
      save_plotly_new_filename(tax_transfers_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    ##### 6-8. Specific policies #####
    labels_policies <<- c("A ban on combustion-engine cars", "A green infrastructure program", "A carbon tax with cash transfers")
    try({(policies_costless_costly_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "costless_costly", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_costless_costly_US, width= 1100, height=fig_height(3*nb_levels))})
    
    try({(policies_large_effect_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "large_effect", sep="_"), export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels=labels_policies))
      save_plotly_new_filename(policies_large_effect_US, width= 1100, height=fig_height(3*nb_levels))})
    
    try({(policies_positive_negative_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "positive_negative", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_positive_negative_US, width= 1100, height=fig_height(3*nb_levels))})
    
    try({(policies_fair_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "fair", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_fair_US, width= 1300, height=fig_height(3*nb_levels))})
    
    try({(policies_win_lose_poor_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "win_lose_poor", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_poor_US, width= 870, height=fig_height(3*nb_levels))})
    
    try({(policies_win_lose_middle_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "win_lose_middle", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_middle_US, width= 870, height=fig_height(3*nb_levels))})
    
    try({(policies_win_lose_rich_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "win_lose_rich", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_rich_US, width= 870, height=fig_height(3*nb_levels))})
    
    try({(policies_win_lose_rural_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "win_lose_rural", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_rural_US, width= 870, height=fig_height(3*nb_levels))})
    
    try({(policies_win_lose_self_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "win_lose_self", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
      save_plotly_new_filename(policies_win_lose_self_US, width= 870, height=fig_height(3*nb_levels))})
    
    # try({(policies_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = paste(names_policies, "support", sep="_"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=labels_policies))
    #   save_plotly_new_filename(policies_support_US, width= 1020, height=fig_height(3*nb_levels))})
    # 
    # try({(policies_all_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("standard_public_transport_support", paste(names_policies, "support", sep="_")), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=c("Ban on combustion cars where<br>public transport made available", labels_policies)))
    #   save_plotly_new_filename(policies_all_support_US, width= 790, height=fig_height(3*nb_levels))})
    
    try({(standard_effects_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                                         export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce CO2 emissions from cars")))
      save_plotly_new_filename(standard_effects_US, width= 1100, height=fig_height(2*nb_levels))})
    
    try({(standard_all_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("standard_fair", "standard_costless_costly", "standard_positive_negative", "standard_large_effect", "standard_effect_less_pollution", "standard_effect_less_emission"), sort = F,rev = F, rev_color = T, 
                                     export_xls = export_xls, df = e, miss=F, labels=c("Be fair", "Costless way<br>to fight climate change", paste0("Positive effect on [country] <br>economy and employment"), paste0("Large effect on [country] <br>economy and employment"), "Reduce air pollution", "Reduce CO2 emissions from cars")))
      save_plotly_new_filename(standard_all_US, width= 1100, height=fig_height(5*nb_levels))})
    
    try({(investments_effects_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), sort = F,rev = F, rev_color = T, 
                                            export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
      save_plotly_new_filename(investments_effects_US, width= 1150, height=fig_height(3*nb_levels))})
    
    try({(investments_all_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("investments_costless_costly", "investments_positive_negative", "investments_large_effect", "investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener"), rev = F, rev_color = T, 
                                        export_xls = export_xls, df = e, miss=F, labels=c("Costless way<br>to fight climate change", paste0("Positive effect on [country] <br>economy and employment"), paste0("Large effect on [country] <br>economy and employment"), "Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")))
      save_plotly_new_filename(investments_all_US, width= 1150, height=fig_height(5*nb_levels))})
    
    try({(tax_transfers_effects_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), sort = F,rev = F, rev_color = T, 
                                              export_xls = export_xls, df = e, miss=F, labels=c("Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
      save_plotly_new_filename(tax_transfers_effects_US, width= 1100, height=fig_height(4*nb_levels))})
    
    try({(tax_transfers_all_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("tax_transfers_costless_costly", "tax_transfers_positive_negative", "tax_transfers_large_effect", "tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving"), rev = F, rev_color = T, 
                                          export_xls = export_xls, df = e, miss=F, labels=c("Costless way<br>to fight climate change", paste0("Positive effect on [country] <br>economy and employment"), paste0("Large effect on [country] <br>economy and employment"), "Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")))
      save_plotly_new_filename(tax_transfers_all_US, width= 1150, height=fig_height(5*nb_levels))})
    
    
    ##### 10. Pref on climate policies #####
    # try({ temp <- heatmap_table(vars = variables_policy, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_policy_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "policy_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_policy, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_policy_short
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "policy_mean", replacement_text), width = 800, height = 400)})
    
    # try({ temp <- heatmap_table(vars = variables_policy, along = along, conditions = c("== -2"), on_control = T)
    #   row.names(temp) <- labels_policy_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "policy_disagree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_policy, along = along, conditions = c("== 2"), on_control = T)
    #   row.names(temp) <- labels_policy_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "policy_strongly_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_policy, along = along, conditions = c("< 0"), on_control = T)
    #   row.names(temp) <- labels_policy_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "policy_strongly_disagree", replacement_text), width = 800, height = 400)})

    # try({ temp <- heatmap_table(vars = variables_tax, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_tax_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "tax_agree", replacement_text), width = 800, height = 400)}) 
    # 
    # try({ temp <- heatmap_table(vars = variables_tax, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_tax_short
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "tax_mean", replacement_text), width = 800, height = 400)})
    
    # try({ temp <- heatmap_table(vars = variables_tax, along = along, conditions = c("== -2"), on_control = T)
    #   row.names(temp) <- labels_tax_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "tax_disagree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax, along = along, conditions = c("== 2"), on_control = T)
    #   row.names(temp) <- labels_tax_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "tax_strongly_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_tax, along = along, conditions = c("< 0"), on_control = T)
    #   row.names(temp) <- labels_tax_short 
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "tax_strongly_disagree", replacement_text), width = 800, height = 400)})
    
    ##### 11. WTP #####
    # try({(wtp_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = rev(variables_wtp), export_xls = export_xls, df = e, miss=F, labels=rev(c("WTP to limit global warming ($/year): 10", "30", "50", "100", "300", "500", "1000"))))
    #   save_plotly_new_filename(wtp_US, width= 680, height=fig_height(5*nb_levels))})
    # 
    # try({(wtp_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "wtp", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="WTP to limit global warming ($/year)"))
    #   save_plotly_new_filename(wtp_US, width= 1050, height=fig_height(2*nb_levels))})
    
    # try({(wtp_agg_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "wtp", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
    #   save_plotly_new_filename(wtp_agg_US, width= 950, height=fig_height(1*nb_levels))})
    
    # mar_old <- par()$mar
    # cex_old <- par()$cex
    # par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
    # cdf_wtp_US <- Ecdf(e$wtp, weights = e$weight)
    # plot(cdf_wtp_US$x, cdf_wtp_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
    # title(ylab=expression("Proportion <= x"), xlab="WTP (in $/year)", line=2.3)
    # grid() 
    # par(mar = mar_old, cex = cex_old)
    
    # try({(donation_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "donation", export_xls = export_xls, df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="Donation to climate charity ($/year)"))
    #   save_plotly_new_filename(donation_US, width= 1050, height=fig_height(2*nb_levels))})
    
    try({(donation_agg_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "donation_agg", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="Donation to climate charity<br>(in % of max ~ 100$)"))
      save_plotly_new_filename(donation_agg_US, width= 600, height=fig_height(1*nb_levels))})
    
    # mar_old <- par()$mar
    # cex_old <- par()$cex
    # par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
    # cdf_donation_US <- Ecdf(e$donation, weights = e$weight)
    # plot(cdf_donation_US$x, cdf_donation_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
    # title(ylab=expression("Proportion <= x"), xlab="Donation (in $/year)", line=2.3)
    # grid() 
    # par(mar = mar_old, cex = cex_old)
    
    ##### 12. International burden-sharing #####
    
    labels_scale <- c()
    for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(scale_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_scale, export_xls = export_xls, df = e, rev = F, miss = T, showLegend=F, labels=labels_scale))
      save_plotly_new_filename(scale_US, width= 460, height=fig_height(4*nb_levels)) })
    
    try({(should_fight_CC_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "should_fight_CC", export_xls = export_xls, df = e, miss=F, rev = F, rev_color = T, labels="[Country] should fight CC"))
      save_plotly_new_filename(should_fight_CC_US, width= 1160, height=fig_height(1*nb_levels))})
    
    labels_if_other_do <- c()
    for (v in variables_if_other_do) labels_if_other_do <- c(labels_if_other_do, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    try({(if_other_do_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_if_other_do, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_if_other_do, hover=c("Much less", "Less", "About the same", "More", "Much more")))
      save_plotly_new_filename(if_other_do_US, width= 830, height=fig_height(2*nb_levels)) })
    
    # labels_burden_sharing_short <<- c("All pay in proportion to income", "All pay in proportion to current emissions", "All pay in proportion to post-1990 emissions", "Richest countries pay it all", "Richest pay even more to help vulnerable")
    # heatmap_wrapper(vars = variables_burden_sharing, labels = labels_burden_sharing_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)

    # try({ temp <- heatmap_table(vars = variables_burden_sharing, along = along, conditions = c("> 0"), on_control = T)
    # row.names(temp) <- labels_burden_sharing_short
    # heatmap_plot(temp, proportion = T)
    # save_plot(filename = paste0(folder, "burden_sharing_agree", replacement_text), width = 800, height = 400)}) 
    # 
    # try({ temp <- heatmap_table(vars = variables_burden_sharing, along = along, conditions = c(""), on_control = T)
    # row.names(temp) <- labels_burden_sharing_short
    # heatmap_plot(temp, proportion = F)
    # save_plot(filename = paste0(folder, "burden_sharing_mean", replacement_text), width = 800, height = 400)})
    
    # try({ temp <- heatmap_table(vars = variables_burden_sharing, along = along, conditions = c("== -2"), on_control = T)
    #   row.names(temp) <- labels_burden_sharing_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "burden_sharing_disagree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_burden_sharing, along = along, conditions = c("== 2"), on_control = T)
    #   row.names(temp) <- labels_burden_sharing_short
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "burden_sharing_strongly_agree", replacement_text), width = 800, height = 400)})
    # 
    # try({ temp <- heatmap_table(vars = variables_burden_sharing, along = along, conditions = c("< 0"), on_control = T)
    #   row.names(temp) <- labels_burden_sharing_short 
    #   heatmap_plot(temp, proportion = T)
    #   save_plot(filename = paste0(folder, "burden_sharing_strongly_disagree", replacement_text), width = 800, height = 400)})
    
    try({(global_assembly_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "global_assembly_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global democratic assembly<br>on climate change"))
      save_plotly_new_filename(global_assembly_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    try({(global_tax_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "global_tax_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on GHG<br> financing a global basic income"))
      save_plotly_new_filename(global_tax_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    try({(tax_1p_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "tax_1p_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Global tax on millionaires <br>to finance low-income countries"))
      save_plotly_new_filename(tax_1p_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    labels_global_policies <<- c("Global democratic assembly<br> on climate change", "Global tax on GHG<br> financing a global basic income", "Global tax on millionaires <br>to finance low-income countries")
    try({(global_policies_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("global_assembly_support", "global_tax_support", "tax_1p_support"), export_xls = export_xls, df = e, miss = F, rev = F, rev_color = T, labels=labels_global_policies))
      save_plotly_new_filename(global_policies_US, width= 800, height=fig_height(3*nb_levels))})
    
    ##### 13. Pref for bans vs. incentives #####
    
    try({(will_insulate_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "will_insulate", export_xls = export_xls, df = e, miss=F, labels="Insulate or replace heating<br>over the next 5 years"))
      save_plotly_new_filename(will_insulate_US, width= 900, height=fig_height(1*nb_levels))})
    
    try({(insulation_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "insulation_support", export_xls = export_xls, df = e, miss=F, labels="Mandatory insulation with subsidies"))
      save_plotly_new_filename(insulation_support_US, width= 1160, height=fig_height(1*nb_levels))})
    
    # try({(insulation_support_variant_US <- barres12(vars = "insulation_support", export_xls = export_xls, df = list(e[e$insulation_disruption_variant==T,], e[e$insulation_disruption_variant==F,]), orig="<br>Control", comp = "Priming: renovation cause disruption", miss=F, labels="Mandatory insulation with subsidies"))
    #   save_plotly_new_filename(insulation_support_variant_US, width= 1035, height=fig_height(3*nb_levels)) }) # TODO
    
    # labels_obstacles_insulation <- c()
    # for (v in variables_obstacles_insulation) labels_obstacles_insulation <- c(labels_obstacles_insulation, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    # labels_obstacles_insulation[1] <- "The choice is not mine"
    # labels_obstacles_insulation[5] <- "My insulation and heating systems<br>are already satisfactory"
    # try({(obstacles_insulation_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_obstacles_insulation, export_xls = export_xls, df = e, error_margin=F, rev = F, miss = F, showLegend=F, labels=labels_obstacles_insulation, hover=labels_obstacles_insulation))
    #   save_plotly_new_filename(obstacles_insulation_US, width= 550, height=fig_height(5*nb_levels)) })
    
    # try({(insulation_subsidies_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "insulation_subsidies_support", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for subsidies of insulation"))
    #   save_plotly_new_filename(insulation_subsidies_support_US, width= 1030, height=fig_height(1*nb_levels))})
    
    try({(insulation_mandatory_support_no_priming_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "insulation_mandatory_support_no_priming", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for making insulation mandatory<br>if half the costs are subsidized by govt"))
      save_plotly_new_filename(insulation_mandatory_support_no_priming_US, width= 1160, height=fig_height(1*nb_levels))})
    
    try({(insulation_mandatory_support_priming_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "insulation_mandatory_support_priming", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Support for making insulation mandatory<br>if half the costs are subsidized by govt<br>With priming about the hurdles."))
      save_plotly_new_filename(insulation_mandatory_support_priming_US, width= 1160, height=fig_height(1*nb_levels))}) # TODO
    
    # try({(insulation_support_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("insulation_subsidies_support", "insulation_mandatory_support"), rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels=c("Insulation subsidized", "...and mandatory")))
    #   save_plotly_new_filename(insulation_support_US, width= 960, height=fig_height(2*nb_levels))})
    
    # labels_beef <<- c()
    # for (v in variables_beef) labels_beef <<- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
    # try({(beef_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = variables_beef, export_xls = export_xls, df = e, rev = F, rev_color = T, miss = F, labels=labels_beef,showLegend=T, hover= labels_support))
    #   save_plotly_new_filename(beef_US, width= 900, height=fig_height(4*nb_levels))  })
    
    
    ##### 14. Trust, perceptions of institutions, etc. #####
    
    try({(can_trust_people_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "can_trust_people", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust other people"))
      save_plotly_new_filename(can_trust_people_US, width= 1160, height=fig_height(1*nb_levels))})
    
    try({(can_trust_govt_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "can_trust_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="Trust the [country] government <br> over the last decade to do what is right"))
      save_plotly_new_filename(can_trust_govt_US, width= 1160, height=fig_height(1*nb_levels))})
    
    try({(view_govt_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "view_govt", export_xls = export_xls, df = e, miss=F, rev_color = T, rev=F, labels="View on government intervention"))
      save_plotly_new_filename(view_govt_US, width= 700, height=fig_height(1*nb_levels))})
    
    try({(problem_inequality_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "problem_inequality", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Income inequality in [Country]"))
      save_plotly_new_filename(problem_inequality_US, width= 1100, height=fig_height(1*nb_levels))})
    
    try({(future_richness_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "future_richness", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="In 100 years, people in the world will be..."))
      save_plotly_new_filename(future_richness_US, width= 850, height=fig_height(1*nb_levels))})
    
    ##### 15. Political views #####
    
    try({(interested_politics_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "interested_politics", rev = F, rev_color = T, export_xls = export_xls, df = e, miss=F, labels="Interested in politics"))
      save_plotly_new_filename(interested_politics_US, width= 790, height=fig_height(1*nb_levels))})
    
    try({(member_environmental_orga_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "member_environmental_orga", export_xls = export_xls, df = e, miss=F, labels="Member of an<br> environmental organization"))
      save_plotly_new_filename(member_environmental_orga_US, width= 622, height=fig_height(1*nb_levels))})
    
    try({(relative_environmentalist_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "relative_environmentalist", export_xls = export_xls, df = e, miss=F, labels="Has an environmentalist relative"))
      save_plotly_new_filename(relative_environmentalist_US, width= 540, height=fig_height(1*nb_levels))})
    
    try({(relative_environmentalist_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "relative_environmentalist", export_xls = export_xls, df = e, miss=F, labels="Has an environmentalist relative"))
      save_plotly_new_filename(relative_environmentalist_US, width= 540, height=fig_height(1*nb_levels))})
    
    try({(vote_participation_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_participation", export_xls = export_xls, df = e, miss=T, labels="Voted in last election"))
      save_plotly_new_filename(vote_participation_US, width= 540, height=fig_height(1*nb_levels)) })
    
    try({(vote_mode_leaning_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_mode_leaning", export_xls = export_xls, df = e, rev = F, rev_color = T, miss=T, labels="Last election vote or hypothetical vote"))
      save_plotly_new_filename(vote_mode_leaning_US, width= 660, height=fig_height(1*nb_levels)) })
    
    # try({(vote_participation_2016_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_participation_2016", export_xls = export_xls, df = e, miss=T, labels="Voted in 2016 election"))
    #   save_plotly_new_filename(vote_participation_2016_US, width= 540, height=fig_height(1*nb_levels)) })
    
    try({data_vote_agg <- array(NA, dim = c(6, nb_levels), dimnames = list(c(1:6), levels))
      for (l in levels) data_vote_agg[,l] <- c(sapply(-2:2, function(x) { wtd.mean(e$vote_agg_number[e[[along]]==l]==x, weights = e$weight[e[[along]]==l]) } ), wtd.mean(is.na(e$vote_agg_number[e[[along]]==l]), weights = e$weight[e[[along]]==l]))#/wtd.mean(!is.na(e$vote_agg_number[e[[along]]==l]), weights = e$weight[e[[along]]==l])
      (vote_US <- barres(data = data_vote_agg, export_xls = export_xls, df = e, rev_color = T, miss=T, labels=labelsN("Last election: ", levels = levels, parentheses = parentheses), legend = c("Very left", "Left", "Center", "Right", "Very right", "PNR or other"))) # Last election vote or hypothetical vote
      save_plotly_new_filename(vote_US, width= 800, height=fig_height(1*nb_levels)) })
    
    # try({(vote_2020 <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_2020", export_xls = export_xls, df = e, rev_color = T, miss=T, fr = "PNR/no right", labels="In 2020, voted for"))
    #   save_plotly_new_filename(vote_2020, width= 610, height=fig_height(1*nb_levels)) })
    # 
    # try({(vote_2016_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for"))
    #   save_plotly_new_filename(vote_2016_US, width= 650, height=fig_height(1*nb_levels)) })
    
    # try({(vote_all_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = c("vote_non_voters", "vote_voters", "vote"), export_xls = export_xls, df = e, rev_color = T, miss=F, labels=c("Non-voters", "Voters", "2020 vote: Voters and non-voters")))
    #   save_plotly_new_filename(vote_all_US, width= 600, height=fig_height(3*nb_levels))})
    # 
    # try({(vote_voters_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for (voters)"))
    #   save_plotly_new_filename(vote_voters_US, width= 650, height=fig_height(1*nb_levels))})
    # 
    # try({(vote_non_voters_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_non_voters", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2020, voted for (non-voters)"))
    #   save_plotly_new_filename(vote_non_voters_US, width= 650, height=fig_height(1*nb_levels))}) # TODO?
    
    # try({(vote_voters_2016_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (voters)"))
    #   save_plotly_new_filename(vote_voters_2016_US, width= 650, height=fig_height(1*nb_levels)) })
    # 
    # try({(vote_non_voters_2016_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "vote_non_voters_2016", export_xls = export_xls, df = e, rev_color = T, miss=T, labels="In 2016, voted for (non-voters)"))
    #   save_plotly_new_filename(vote_non_voters_2016_US, width= 650, height=fig_height(1*nb_levels))})
    
    try({(left_right_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "left_right", export_xls = export_xls, df = e, rev_color = T, rev = F, miss=T, labels="On economic policy matters, are you..."))
      save_plotly_new_filename(left_right_US, width= 950, height=fig_height(1*nb_levels))})
    
    # try({(political_affiliation_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "political_affiliation", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Political affiliation"))
    #   save_plotly_new_filename(political_affiliation_US, width= 800, height=fig_height(1*nb_levels))})
    
    ##### 16. Feedback #####
    
    try({(survey_biased_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "survey_biased", export_xls = export_xls, df = e, rev_color = T, miss=F, labels="Survey biased"))
      save_plotly_new_filename(survey_biased_US, width= 810, height=fig_height(1*nb_levels))})
    
    try({ stopwords <- unlist(sapply(c("danish", "french", "english", "german", "italian", "spanish"), function(v) stopwords(v)))
    # rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = "survey", max.words = 70) # TODO update
    rquery.wordcloud(paste(e$comment_field, collapse=" \n "), excludeWords = c(stopwords, "survey"), colorPalette = "Blues", max.words = 70)
    save_plot(filename = paste0(folder, "comment_field", replacement_text), height = 400, width = 400)})

    try({(CC_field_nb_actions_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "nb_actions_CC_field", export_xls = export_xls, df = e[e$country %in% countries_field_treated,], rev = F, rev_color = T, miss=F, labels="# supported actions mentioned"))
      save_plotly_new_filename(CC_field_nb_actions_US, width= 570, height=fig_height(1*length(Levels(e[e$country %in% countries_field_treated,][[along]]))))})

    try({(CC_field_nb_elements_US <- barresN(along=along, parentheses=parentheses, nolabel=nolabel, vars = "nb_elements_CC_field", export_xls = export_xls, df = e[e$country %in% countries_field_treated,], rev = F, rev_color = T, miss=F, labels="# supported elements in CC field"))
      save_plotly_new_filename(CC_field_nb_elements_US, width= 650, height=fig_height(1*length(Levels(e[e$country %in% countries_field_treated,][[along]]))))})

    
    ##### Heatmaps #####
    
    # Missing in socio: TODO! nb_children, heating, gas_expenses/emissions
    main_variables_socio <<- c("can_trust_people", "interested_politics", "voted", "can_trust_govt", "view_govt", "left_right", "hit_by_covid", "polluting_sector", "availability_transport", "urban")
    # interested_politics: >= A lot; hit_by_covid: -1/1; polluting_sector: T/F; can_trust_people, govt: >= agree; view_govt: -1/0/1; know_treatment_policy: 0/1/2; availability_transport: >= fair
    labels_main_socio <<- c("Most people can be trusted", "Interested in politics", "Voted in last election", "Government could be trusted in last 10 years", "Government should do more", "Left-right on economics", "HH member lost income or job due to pandemic", "Works in a polluting sector", "Availability of public transport", "Urban")
    heatmap_wrapper(vars = main_variables_socio, labels = labels_main_socio, conditions = heatmap_conditions, name = "socio", alphabetical = alphabetical, special = special, alphabetical = alphabetical, special = special)
    
    heatmap_wrapper(vars = c("should_act_CC_field", "measure_proposed_CC_field", "CC_field_no_worry", "CC_field_do_not_know", "CC_field_na"), labels = c("Action needed", "Measure proposed", "No action needed", "Do not know", "Empty"), name = "CC_field_mentions", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    
    heatmap_wrapper(vars = var_CC_field_names[6:18], labels = as.character(CC_field_names_names[6:18]), name = "CC_field_mentions_raw", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = var_CC_field_names[9:14], labels = as.character(CC_field_names_names[9:14]), name = "CC_field_sectors", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = var_CC_field_names[c(8,15:18)], labels = as.character(CC_field_names_names[c(8,15:18)]), name = "CC_field_instruments", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_CC_field_contains, labels = sub("_", " ", sub("CC_field_contains_", "", variables_CC_field_contains)), name = "CC_field_contains", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    
    variables_feedback <<- c("survey_biased_yes", "survey_biased_left", "survey_biased_right", "non_empty_comment_field", "comment_field_good", "critic_comment_field")
    labels_feedback <<- c("Survey biased", "Left-wing biased", "Right-wing biased", "Comment: non empty", "Comment: liked", "Comment: disliked")
    heatmap_wrapper(vars = variables_feedback, labels = labels_feedback, name = "feedback", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_comment_field_contains, labels = sub("_", " ", sub("comment_field_contains_", "", variables_comment_field_contains)), name = "comment_field_contains", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = var_comment_field_names, labels = comment_field_names, name = "comment_field_mentions_raw", conditions = ">= 1", df = e[e$country %in% countries_field_treated,], alphabetical = alphabetical, special = special)
    # unused: nb_actions_CC_field, nb_elements_CC_field
    
    main_variables_affected <<- c(variables_affected_index[c(1,2,5:7)], "index_affected")
    labels_affected <<- c("Current/past job in a polluting sector", "Uses car/motorbike", "Availability of transport", "Size of agglomeration", "Urban", "Index Affected by CC")
    heatmap_wrapper(vars = main_variables_affected, labels = labels_affected, conditions = heatmap_conditions, name = "affected", alphabetical = alphabetical, special = special)
    
    main_variables_behavior <<- c("flights_agg", "flights_agg", "frequency_beef", "transport_work", "CC_talks", "member_environmental_orga")
    # future_richness: >= richer, net_zero_feasible, CC_affects_self, effect_halt_CC_lifestyle: >= A lot; effect_halt_CC..: >= positive; CC_will_end: >= somewhat likely
    labels_main_behavior <<- c("At least one flight between 2017 and 2019", "More than one flight per year on average", "Eats beef at least once a week", "Commutes by car/motorbike", "Talks or thinks of CC several times a month", "Is member of an environmental organisation")
    try({temp <- heatmap_table(vars = main_variables_behavior, along = along, conditions = list("> 0", "> 1", ">= 1", "== 'Car or Motorbike'", "== 'Monthly'", "== T"), on_control = T, alphabetical = alphabetical, special = special) # TODO! change commutes by leisure
    row.names(temp) <- labels_main_behavior
    heatmap_plot(temp, proportion = (cond != ""))
    save_plot(filename = paste0(folder, "behavior", replacement_text), width = 800, height = 400)})
    
    heatmap_wrapper(vars = c("car_work", "car_shopping", "car_leisure"), labels = paste("Uses own vehicle to", c("go to work", "go shopping", "leisure")), conditions = c(">= 1"), name = "transport", alphabetical = alphabetical, special = special)
    variables_scores_footprint <<- c("score_footprint_elec", "score_footprint_food", "score_footprint_transport", "score_footprint_pc", "score_footprint_region")
    labels_scores_footprint <<- c("Electricity", "Food", "Transport", "Countries per capita", "Countries in absolute") 
    labels_scores_footprint_long <<- c("Electricity: coal > gas > nuclear", "Food: beef > chicken > pasta", "Transport: plane > car > coach/train", "Countries per capita: US > EU > China > India", "Countries in absolute: China > US > EU > India") 
    main_variables_knowledge <<- c("CC_anthropogenic", "CC_knowledgeable", "CC_dynamic", "score_GHG", "score_CC_impacts", variables_scores_footprint, "correct_footprint_pc_compare", "index_knowledge_efa_global")
    # CC_anthropogenic, CC_knowledgeable: >= A lot; CC_dynamic: T/F; scores: number,
    labels_main_knowledge <<- c("CC exists, is anthropogenic", "Considers one's self knowledgeable", "Cutting emissions by half enough to stop global warning (False)", "Score to knowledge of greenhouse gases in [0;+4]", "Knowledge score of impacts in [0;4] (droughts, sea-level, volcanos)", paste("Distance to true ranking of footprints: ", labels_scores_footprint), "Correctly compares p.c. emissions of e.g. own region vs. China", "Standardised knowledge index")
    heatmap_wrapper(vars = variables_scores_footprint, labels = labels_scores_footprint_long, conditions = c(""), alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = c(variables_scores_footprint, "correct_footprint_pc_compare_own", "correct_footprint_pc_compare_US", "knows_beef_footprint"), labels = c(paste("# of mistakes on footprint ranking for\n", labels_scores_footprint_long), "Correctly compares own region's per capita emissions\nand those of China (or of India for CN, SA)", "Knows that US per capita emissions\nare higher than China's", "Knows that beef footprint is higher\nthan chicken's and pasta's"), conditions = c(""), name = "footprints", alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = main_variables_knowledge, labels = labels_main_knowledge, conditions = "", name = "knowledge", alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = main_variables_knowledge[c(1:5,11)], labels = labels_main_knowledge[c(1:5,11)], name = "knowledge_wo_footprint", conditions = "", alphabetical = alphabetical, special = special)
    # TODO? Replace CC_dynamic by its opposite (so knowledge is counted positively), same kind of transformations for scores
    
    variables_future <<- c("future_richness", "net_zero_feasible",  "CC_will_end", "CC_affects_self","CC_impacts_extinction", "effect_halt_CC_economy", "effect_halt_CC_lifestyle")
    # future_richness: >= richer, net_zero_feasible, CC_affects_self, effect_halt_CC_lifestyle: >= A lot; effect_halt_CC..: >= positive; CC_will_end: >= somewhat likely
    labels_future <<- c("World will be richer in 100 years", "Technically possible to stop emissions by 2100", "Likely that humans halt CC by 2100", "CC will affect me negatively", "Likely that CC causes extinction of humankind", "Ambitious climate policies positive for economy", "Ambitious climate policies negative for my lifestyle")
    heatmap_wrapper(vars = variables_future, labels = labels_future, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)

    heatmap_wrapper(vars = c("CC_anthropogenic", "CC_dynamic", "CC_problem", "CC_affects_self", "net_zero_feasible", "CC_will_end", "effect_halt_CC_economy", "effect_halt_CC_lifestyle", alphabetical = alphabetical, special = special) , 
                    labels = c("CC exists, is anthropogenic", "Cutting GHG emissions by half\nsufficient to stop rise in temperatures", "CC is an important problem", "CC will negatively affect personal life", 
                               "Feasible to stop GHG emissions\nwhile sustaining satisfactory\nstandards of living in [Country]", "Likely to halt CC by the end of the century", "Positive effects of ambitious policies\non the [country] economy and employment", "Negative effects of ambitious policies on lifestyle"), conditions = heatmap_conditions, name = "CC_attitude")
    
    heatmap_wrapper(vars = c("CC_affects_self", "net_zero_feasible", "CC_will_end", "future_richness", alphabetical = alphabetical, special = special) , 
                    labels = c("Feels affected by climate change", "Net zero by 2100 feasible", "Likely that climate change ends by 2100", "World in 100 years will be richer"), conditions = heatmap_conditions, name = "future")
    
    heatmap_wrapper(vars = variables_CC_impacts, labels = labels_CC_impacts, conditions = heatmap_conditions, alphabetical = alphabetical, special = special) # >= somewhat likely
    
    variables_willingness_all <<- c(variables_willing, variables_condition, "will_insulate", "wtp", "donation_fraction", "petition") 
    # willing, condition : >= A lot; will insulate: >= somewhat likely; WTP, petition: T/F; donation: number
    labels_willingness <<- paste("Willing to", c("Limit flying", "Limit driving", "Have a fuel-efficient or electric vehicle", "Limit beef consumption", "Limit heating or cooling your home"))
    labels_willingness_all <<- c(labels_willingness, paste("Condition willing:", labels_condition), "Will insulate home in next 5 years", "Willing To Pay to keep global warning below 2°C", "Donation to reforest if wins lottery", "Willing to sign petition")
    heatmap_wrapper(vars = variables_wtp, labels = paste("WTP (~ PPP$/year):", c(" 10", "30", "50", "100", "300", "500", "1000")), conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_condition, labels = labels_condition, conditions = heatmap_conditions, alphabetical = alphabetical, special = special) # test
    heatmap_wrapper(vars = variables_willing, labels = labels_willingness, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    # heatmap_wrapper(vars = variables_willingness_all[1:11], labels = labels_willingness_all[1:11], name = 'willingness_all', conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_willingness_all, labels = labels_willingness_all, name = 'willingness_all', conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    
    heatmap_wrapper(vars = c("insulation_mandatory_support_no_priming", "insulation_mandatory_support_priming"), labels = c("Support for mandatory insulation\nif govt subsidizes half the cost\nNo priming", "Support for mandatory insulation\nif govt subsidizes half the cost\nWith priming about the hurdles"), name = 'insulation_support', conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_obstacles_insulation[c(5,2,1,3,4)], labels = sub("<br>", "\n", labels_obstacles_insulation[c(5,2,1,3,4)]), name = 'obstacles_insulation', conditions = ">= 1", alphabetical = alphabetical, special = special)
    
    temp <- variables_burden_share
    variables_burden_share <- variables_burden_share_ing 
    labels_burden_share_ing <- c(special, countries_names)
    if (any(c('non-OECD', 'Non-OECD', 'non-oecd') %in% special) & levels[15]=="Brazil") labels_burden_share_ing <- c(special[!special %in% c('non-OECD', 'Non-OECD', 'non-oecd')], countries_names[1:14], "Non-OECD", countries_names[15:length(countries_names)])
    labels_burden_share_ing <- sub("France", "France*", sub("Denmark", "Denmark*", sub("United States", "United States*", labels_burden_share_ing)))
    variables_burden_share_all <<- c(variables_scale, "if_other_do_more", "if_other_do_less", "global_quota", variables_burden_share, variables_global_policies)
    variables_burden_share_main <<- c("scale_global", variables_burden_share, variables_global_policies)
    # variables_burden_sharing_few <<- variables_burden_share_all[c(1,8,11,12,14)]
    variables_burden_share_few <<- c("scale_global", variables_burden_share[c(1,3)], "global_assembly_support", "tax_1p_support")
    # Levels: T/F; If...: >= More; burden_share: >= agree; Global..: >= somewhat support
    labels_heatmap_scale <<- paste("Level of climate policies needed:", c("global", "federal/continental", "state/national", "local"))
    labels_heatmap_burden_sharing <<- c("All countries should pay in proportion to income", "All countries should pay in proportion to current emissions", "All countries should pay in proportion to post-1990 emissions", "Richest should countries pay it all so poor ones don't pay", "Richest countries should pay even more to help vulnerable ones") # test longest (if not below)
    labels_heatmap_burden_share <<- c("Emission share should be in proportion to population*", "Emission share should be in proportion to current emissions", "Countries that have emitted more since 1990\nshould receive a lower share*", "Countries that will be hurt more by CC should receive a higher share*") # test longest
    labels_burden_share_all <<- c(labels_heatmap_scale, "If other do more, [country] should do more", "If other do less, [country] should do more", "Global carbon budget (+2°C) divided in tradable country shares", labels_heatmap_burden_share, sub("<br>", "", labels_global_policies))
    heatmap_wrapper(vars = variables_scale, labels = labels_heatmap_scale, conditions = ">= 1", alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_burden_share, labels = labels_heatmap_burden_share, labels_along = labels_burden_share_ing, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_global_policies, labels = sub("<br>", "", labels_global_policies), labels_along = labels_burden_share_ing, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_burden_share_all, labels = labels_burden_share_all, labels_along = labels_burden_share_ing, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_burden_share_main, labels = labels_burden_share_all[c(1,7:14)], labels_along = labels_burden_share_ing, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_burden_share_few, labels = labels_burden_share_all[c(1,7,8,12,14)], labels_along = labels_burden_share_ing, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    # heatmap_wrapper(vars = variables_burden_sharing_main, labels = labels_burden_sharing_all[c(1,7:14)], conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    # heatmap_wrapper(vars = variables_burden_sharing_few, labels = labels_burden_sharing_all[c(1,8,11,12,14)], conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    # labels_burden_share_short <<- c("All pay in proportion to income", "All pay in proportion to current emissions", "All pay in proportion to post-1990 emissions", "Richest countries pay it all", "Richest pay even more to help vulnerable")
    # heatmap_wrapper(vars = variables_burden_share, labels = labels_burden_share_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    variables_burden_share <- temp
    
    main_variables_opinion <<- c("CC_anthropogenic", "CC_problem", "should_fight_CC", "willing_limit_driving", "standard_support", "investments_support", "tax_transfers_support", "beef_ban_intensive_support", "insulation_mandatory_support_no_priming", "burden_share_ing_population", "tax_1p_support")
    # anthropogenic, willing limit driving: >= A lot; problem, should fight, burden_sharing_emissions: >= agree; support: >= somewhat support
    labels_opinion <<- c("CC exists, is anthropogenic", "CC is an important problem", "[Country] should fight CC", "Willing to limit driving", "Ban on combustion-engine cars", "Green infrastructure program", "Carbon tax with cash transfers", "Ban on intensive cattling", "Mandatory insulation of buildings", "Emission share should be in proportion to population*", "Global tax on millionaires funding LDC")
    heatmap_wrapper(vars = main_variables_opinion, labels = labels_opinion, labels_along = labels_burden_share_ing, conditions = heatmap_conditions, name = "opinion", alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = c("frequency_beef", "knows_beef_footprint", "willing_limit_beef", variables_beef), labels = c("Eats beef at least once a week", "Knows that beef has high GHG footprint", "Willing to limit beef consumption", "Support for tax on cattle products\nthat would double beef price", "Support for subsidies on organic and local\nvegetables, fruits, and nuts", "Support for removal of subsidies\nfor cattle farming", "Support for ban of intensive cattle farming"), conditions = heatmap_conditions, name = "beef", alphabetical = alphabetical, special = special)

    variables_main_policies <<- c("tax_transfers_support", "standard_support", "standard_public_transport_support", "investments_support", "beef_ban_intensive_support", "insulation_mandatory_support_no_priming", "policy_tax_flying", "policy_ban_city_centers", "global_quota", "burden_share_population", "global_tax_support", "global_assembly_support", "tax_1p_support")
    # anthropogenic, willing limit driving: >= A lot; problem, should fight, burden_sharing_emissions: >= agree; support: >= somewhat support
    labels_main_policies <<- c("Carbon tax with cash transfers", "Ban on combustion-engine cars", "Ban on combustion-engine cars\nwhere alternatives made available", "Green infrastructure program", "Ban on intensive cattling", "Mandatory insulation of buildings", "A tax on flying (raising price by 20%)", "A ban of polluting vehicles in city centers", "Global carbon budget (+2°C)\ndivided in tradable country shares", "Emission share should be in proportion to population*", "Global tax on GHG financing a global basic income", "Global democratic assembly on climate change", "Global tax on millionaires funding LDC")
    heatmap_wrapper(vars = variables_main_policies, labels = labels_main_policies, labels_along = labels_burden_share_ing, conditions = heatmap_conditions, name = "main_policies", alphabetical = alphabetical, special = special)
    
    # main_variables_burden_sharing_inferred <<- variables_burden_sharing_inferred[c(1:4,6)]
    # labels_burden_sharing_inferred <<- c("Polluter pay", "Rich countries pay it all", "Grand-fathering", "Polluter pay & Rich pay it all", "Pay with emissions & Rich pay it all", "Polluter pay & Vulnerable receive", "Pay with emissions & Vulnerable receive")
    # heatmap_wrapper(vars = main_variables_burden_sharing_inferred, labels = labels_burden_sharing_inferred[c(1:4,6)], conditions = ">= 1", alphabetical = alphabetical, special = special)
    # heatmap_wrapper(vars = variables_burden_sharing_inferred, labels = labels_burden_sharing_inferred, conditions = ">= 1", alphabetical = alphabetical, special = special)
    labels_standard_effects_short <<- c("Reduce car emissions", "Reduce air pollution", "Positive economic effect", "Large economic effect", "Costless way to fight CC")
    heatmap_wrapper(vars = variables_standard_effect, labels = labels_standard_effects_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_standard_win_lose, labels = labels_standard_win_lose, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)

    heatmap_wrapper(name = "policies_support", vars = paste(names_policies, "support", sep="_"), labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(name = "policies_all_support", vars = c(paste(rev(names_policies), "support", sep="_"), "standard_public_transport_support"), labels = c(rev(labels_policies), "Ban on combustion cars where\n public transport made available"), conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    labels_policies_attitudes <<- c("Positive effect on economy and employment", "Large effect on economy and employment", "Costless way to reduce emissions", "Incidence on low-income earners", "Incidence on the middle class", "Incidence on high-income earners", "Incidence on those living in rural areas", "Incidence own household", "Fair")
    heatmap_wrapper(name = "policies_attitudes", vars = variables_policies_attitudes,  labels = labels_policies_attitudes, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(name = "policies_attitudes_all", vars = c(variables_policies_attitudes, "policies_support"), labels = c(labels_policies_attitudes, "Support"), conditions = heatmap_conditions)# TODO! add reduce air pollution and effective (first items, alphabetical = alphabetical, special = special)
    
    labels_investments_effects_short <<- c("Make electricity greener", "Popularize public transport", "Reduce air pollution", "Positive economic effect", "Large economic effect", "Costless way to fight CC")
    heatmap_wrapper(vars = variables_investments_effect, labels = labels_investments_effects_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_investments_win_lose, labels = labels_investments_win_lose, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    
    labels_tax_transfers_effects_short <<- c("Discourage driving", "Encourage insulation", "Reduce use of fuels", "Reduce air pollution", "Positive economic effect", "Large economic effect", "Costless way to fight CC")
    heatmap_wrapper(vars = variables_tax_transfers_effect, labels = labels_tax_transfers_effects_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_tax_transfers_win_lose, labels = labels_tax_transfers_win_lose, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    # , e.g. 0.1€/L)
    labels_policy_short <<- c("Tax on flying (+20%)", "Tax on fossil fuels ($45/tCO2)", "Ban polluting cars in city centers", "Subsidies to low-carbon technos", "Funding clean energy in LDC")
    heatmap_wrapper(vars = variables_policy, labels = labels_policy_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_policy, labels = labels_policy_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(name = "policy_all", vars = c(variables_policy, "insulation_support"), labels = c(labels_policy_short, "Mandatory, subsidised insulation"), conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    
    # labels_support_short <<- c("Tax on flying (+20%)", "Tax on fossil fuels ($45/tCO2)", "Ban polluting cars in city centers", "Subsidies to low-carbon technos", "Funding clean energy in LDC")
    # heatmap_wrapper(vars = variables_support, labels = labels_policy_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    
    labels_tax_short <<- c("Cash for constrained HH", "Cash for the poorest", "Equal cash for all", "Reduction in income tax", "Reduction in corporate tax", "Tax rebate for affected firms", "Funding green infrastructure", "Subsidies to low-carbon technos", "Reduction in the deficit")
    heatmap_wrapper(vars = variables_tax, labels = labels_tax_short, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = c("tax_transfers_support", variables_tax), name = "tax_all", labels = c("Tax with cash transfers", labels_tax_short), conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
 
    heatmap_wrapper(vars = rev(c("investments_support", "tax_transfers_fair", "investments_costless_costly", "investments_positive_negative", "investments_large_effect", "investments_effect_less_pollution", "investments_effect_public_transport", "investments_effect_elec_greener")), alphabetical = alphabetical, special = special, name = "investments_all", 
                    labels = rev(c("Support", "Be fair", "Costless way to fight climate change", "Positive effect on [country]\neconomy and employment", "Large effect on [country]\neconomy and employment", "Reduce air pollution", "Increase the use of public transport", "Make electricity production greener")), conditions = heatmap_conditions)
    heatmap_wrapper(vars = rev(c("standard_support", "standard_fair", "standard_costless_costly", "standard_positive_negative", "standard_large_effect", "standard_effect_less_pollution", "standard_effect_less_emission")), alphabetical = alphabetical, special = special, name = "standard_all", 
                    labels = rev(c("Support", "Be fair", "Costless way to fight climate change", "Positive effect on [country]\neconomy and employment", "Large effect on [country]\neconomy and employment", "Reduce air pollution", "Reduce CO2 emissions from cars")), conditions = heatmap_conditions)
    heatmap_wrapper(vars = rev(c("tax_transfers_support", "tax_transfers_fair", "tax_transfers_costless_costly", "tax_transfers_positive_negative", "tax_transfers_large_effect", "tax_transfers_effect_less_pollution", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving")), alphabetical = alphabetical, special = special, name = "tax_transfers_all", 
                    labels = rev(c("Support", "Be fair", "Costless way to fight climate change", "Positive effect on [country]\neconomy and employment", "Large effect on [country]\neconomy and employment", "Reduce air pollution", "Reduce GHG emissions", "Encourage insulation of buildings", "Encourage people to drive less")), conditions = heatmap_conditions)
    heatmap_wrapper(vars = paste(names_policies, "costless_costly", sep="_"), name = "policies_costless_costly", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "costless", sep="_"), name = "policies_costless", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "cost_effective", sep="_"), name = "policies_costly", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "fair", sep="_"), name = "policies_fair", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "large_effect", sep="_"), name = "policies_large_effect", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "positive_negative", sep="_"), name = "policies_positive_negative", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "positive_effect", sep="_"), name = "policies_positive_effect", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "negative_effect", sep="_"), name = "policies_negative_effect", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "win_lose_middle", sep="_"), name = "policies_win_lose_middle", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "win_lose_poor", sep="_"), name = "policies_win_lose_poor", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "win_lose_rich", sep="_"), name = "policies_win_lose_rich", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "win_lose_rural", sep="_"), name = "policies_win_lose_rural", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = paste(names_policies, "win_lose_self", sep="_"), name = "policies_win_lose_self", labels = labels_policies, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = c("standard_effect_less_emission", "investments_effect_public_transport", "investments_effect_elec_greener", "tax_transfers_effect_less_emission", "tax_transfers_effect_insulation", "tax_transfers_effect_driving", alphabetical = alphabetical, special = special), 
                    name = "policies_effects", labels = c("A ban on combustion-engine cars\nReduces CO2 emissions from cars", "A green infrastructure program\nIncreases the use of public transport", "A green infrastructure program\nMakes electricity production greener", "A carbon tax with cash transfers\nReduces GHG emissions", "A carbon tax with cash transfers\nEncourages insulation of buildings", "A carbon tax with cash transfers\n>Encourages people to drive less"), conditions = heatmap_conditions)

    # TODO: GHG_correct instead of GHG; 2 in subscript
    heatmap_wrapper(vars = c("GHG_CO2", "GHG_methane", "GHG_H2", "GHG_particulates"), name = "GHG", labels = c("CO2 (Yes)", "Methane (Yes)", "Hydrogen (No)", "Particulates (No)"), conditions = ">= 1", alphabetical = alphabetical, special = special)
    heatmap_wrapper(vars = variables_home, name = "home", labels = labels_home, conditions = ">= 1", alphabetical = alphabetical, special = special)
    
    labels_responsible_CC <<- c("Each of us", "The high income earners", "The government", "Companies", "Previous generations")
    # paste("Responsible:", labels_responsible_CC) # T/F
    heatmap_wrapper(vars = variables_responsible_CC, name = "CC_responsible", labels = labels_responsible_CC, conditions = heatmap_conditions, alphabetical = alphabetical, special = special)
          
    ##### Print missing figures #####
    missing_figures <- setdiff(sub("_US", "", list.files("../figures/US")), c(sub(paste(paste0(c("_positive", "_mean", ""), replacement_text), collapse = "|"), "", list.files(folder))))
    cat(paste0(length(missing_figures), " Missing figures for: "))
    missing_types <- c("_comp.png", "_pol.png", "_urb.png", "_vote.png", "_inc.png")
    for (t in missing_types) if (grepl(t, missing_figures)) print(paste("All figures of type: ", t))
    missing_figures <- missing_figures[!grepl(paste(missing_types, collapse="|"), missing_figures)]
    # cat(paste0(length(missing_figures), " Missing figures for ", country, ": "), missing_figures, sep = '\n')
    cat(sub(".png", "", missing_figures), sep = '\n')
    cat(" ")
  }
  
  # if (tables) {
  #   ##### Tables treatment effects #####
  #   ## Generate tables
  #   label_treat_wave <- c("Both treatments", "Climate treatment only", "Policy treatment only", "wave: Pilot 2")
  #   variables_controls <- variables_main_controls[which(variables_main_controls %in% names(e))] 
  #   print(paste0("Missing controls for ", country, ": ", variables_main_controls[which(!(variables_main_controls %in% names(e)))]))
  #   
  #   desc_table(dep_vars = c("CC_anthropogenic > 0", "CC_impacts_extinction > 0", "donation", "should_fight_CC > 0", "willing_limit_driving > 0"), filename = paste0(country, "_1"), save_folder = "../tables/", 
  #              dep.var.labels = c("CC caused by humans", "CC likely to cause extinction", "Donation (in \\$)", paste0(country, " should fight CC"), "Willing to limit driving"),
  #              data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
  #   )
  #   
  #   desc_table(dep_vars = c("tax_transfers_support > 0", "investments_support > 0", "standard_support > 0", "policies_support > 0"), filename = paste0(country, "_2"), save_folder = "../tables/", 
  #              dep.var.labels = c("Carbon tax with transfers", "Green Infrastructure Program", "Ban on combustion-engine cars", "Average over 3 policies"),
  #              dep.var.caption = c("Support"), data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
  #   )
  #   
  #   desc_table(dep_vars = c("policies_fair > 0", "policies_self > 0", "policies_poor > 0", "policies_large_effect > 0", "policies_positive_negative > 0"), filename = paste0(country, "_3"), save_folder = "../tables/", 
  #              dep.var.labels =  c("Fair", "HH would win", "Poor would win", "Large economic effect", "Positive economic effect"),
  #              data = e, keep = c("treatment"), indep_vars = c(variables_controls, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
  #   )
  #   
  # }
  print(Sys.time() - start)
}

render_figures_tables_country(dk, "DK", folder_country = T) # 7.5 min
render_figures_tables_country(fr, "FR", folder_country = T)
render_figures_tables_country(us, "US", folder_country = T)
render_figures_tables_country(de, "DE", folder_country = T)

render_figures_tables_country(dk, "DK", folder_country = T, tables = F, export_xls = T) # 7.5 min
render_figures_tables_country(fr, "FR", folder_country = T, tables = F, export_xls = T)
render_figures_tables_country(us, "US", folder_country = T, tables = F, export_xls = T)

# /!\ The size of plot window matters for proper save of figures: it should be at least 1770*400
render_country_comparison(along = "country_name", parentheses = F, nolabel = T, folder_country = T, name_country = T, on_control = T, export_xls = F, figures = T, tables = T)

update_constant(fr)
update_constant(all)
update_constant(dk)
update_constant(us)
update_constant(de)

# # (temp <- barresN(vars = "CC_anthropogenic", export_xls = export_xls, df = all, along = "country", rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic"))
# # (temp <- barresN(vars = "CC_anthropogenic", export_xls = export_xls, df = all, along = "country_name", rev = F, rev_color = T, miss=F, labels="Part of climate change anthropogenic", parentheses = F, nolabel = T))
# # save_plotly(temp, width= 870, height=fig_height(1*length(levels)))
# # heatmap_table(vars = c("CC_anthropogenic", "standard_support"), along = "country_name", conditions = c("> 0"), on_control = T)
# # heatmap_plot(heatmap_table(vars = c("CC_anthropogenic", "standard_support"), along = "country_name", conditions = c(""), on_control = T), percent = F)
# # save_plot(filename = "test", width = 400, height = 250)V

plot_world_map("net_zero_feasible", continuous = FALSE, width = 900, height = 400)

temp <- heatmap_table(vars = main_variables_behavior, data = all, labels = labels_main_behavior, along = along, special = c('World', 'OECD'), conditions = list("> 0", "> 1", ">= 1", "== 'Car or Motorbike'", "== 'Monthly'", "== T"), on_control = T, alphabetical = alphabetical, special = special) # TODO! change commutes by leisure
heatmap_plot(temp, proportion = T)
# corrplot(temp, method='color', col.lim = c(0, 1), addCoef.col = 'black', addCoefasPercent = T, type='full', is.corr = F)
# corrplot(matrix(c(0.8, 0.5, 0.6, 0.7), ncol=2), method='color', col.lim = c(0, 1), col = COL2('RdYlBu'),  addCoef.col = 'black', addCoefasPercent = T, type="full", is.corr = F)
# corrplot(matrix(c(0.8, 0.5, 0.6, 0.4), ncol=2), method='color', col.lim = c(0, 1), col = COL2('RdYlBu'), addCoef.col = 'black', addCoefasPercent = T, type="full", is.corr = F)
# corrplot(matrix(c(0.8, 0.5, 0.2, 0.4), ncol=2), method='color', col.lim = c(0, 1), col = COL2('RdYlBu'), addCoef.col = 'black', addCoefasPercent = T, type="full", is.corr = F)
