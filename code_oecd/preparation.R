# setwd("/var/www/oecd_climate/code_oecd")
# setwd("C:/Users/afabre/Google Drive/Economie/Travail/oecd_climate/code_oecd")

source(".Rprofile")

remove_id <- function(file, folder = "../data") {
  filename <- paste(folder, file, sep = "/")
  data <- read_csv(filename) 
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID")))]
  write_csv(data, filename, na = "")
}
for (file in c("US_pilot.csv", "US_pilot2.csv", "US_pilot3.csv")) remove_id(file)

# temp <- prepare(country = "US", wave = "pilot2", duration_min = 0, exclude_screened = F, only_finished = F)
# comp <- read_csv2("../data/complete_PSID.csv" )
# psid <- as.matrix(comp$IdParameter, ncol=1)
# comp <- read_csv2("../data/complete_PSID2.csv" )
# tem <- as.matrix(comp$IdParameter, ncol=1)
# psid <- as.character(rbind(psid, tem))
# 
# sum(e$PSID %in% psid)
# sum(temp2$PSID %in% psid)

# PSID <- read_csv("../data/PSID.csv" )
# names(PSID)[9] <- "PSID"
# temp$excluded[is.na(temp$excluded)] <- "Complete"
# write.csv(temp[,c("PSID", "excluded")], "../data/ID.csv")
# temp2 <- merge(PSID[,c("PSID")],temp[,c("PSID", "excluded")], all = T)
# temp3 <- merge(PSID[,c("PSID")],temp[,c("PSID", "excluded")], all.x = T)
# write.csv(temp2, "../data/all_PSIDs.csv")
# write.csv(temp3, "../data/Dynata_PSIDs.csv")

# ruca <- read.xls("../data/ruca2010.xlsx")
# zips <- read.xls("../data/zipcodes2010.xlsx")

relabel_and_rename <- function(e, country, wave = NULL) {
  # Notation: ~ means that it's a random variant / * that the question is only displayed under certain condition
  
  # The commented lines below should be executed before creating relabel_and_rename, to ease the filling of each name and label
  # e <- read_csv("../data/US_pilot3.csv")
  # for (i in 1:length(e)) {
  #   label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") #
  #   print(paste(i, label(e[[i]])))
  # }
  
  if (country == "US" & wave == "pilot1") {
    # names\(e\)\[[0-9]*\] <- (".*")
    names(e) <- c(
    "date",
    "date_end",
    "status_response",
    "ip",
    "progress",
    "time",
    "terminated",
    "date_recored",
    # "ID_qualtrics",
    "name",
    "firstname",
    "mmail",
    "ref",
    "lat",
    "long",
    "distr",
    "lang",
    "gender",
    "age",
    "region",
    "zipcode",
    "urbanity",
    "race_white",
    "race_black",
    "race_hispanic",
    "race_asian",
    "race_native",
    "race_hawaii",
    "race_other_choice",
    "race_pnr",
    "race_other",
    "speaks_well",
    "education", # IRL: 45% have a college degree, 13% a master
    "employment_status",
    "hit_by_covid",
    "income",
    "home_tenant",
    "home_owner",
    "home_landlord",
    "home_hosted",
    "wealth",
    "nb_children",
    "hh_adults",
    "hh_children",
    "heating",
    "km_driven",
    "flights",
    "frequency_beef",
    "transport_work",
    "transport_shopping",
    "transport_leisure",
    "transport_available",
    "trust_people",
    "trust_govt",
    "trust_public_spending",
    "statist",
    "inequality_problem",
    "future_gdp",
    "envi", 
    "envi_order_collapse",
    "envi_order_progress",
    "envi_order_pro_envi",
    "envi_order_anti_envi",
    "envi_order_pnr",
    "Q146_First",
    "Q146_Last",
    "duration_politics_field",
    "Q146_Click",
    "politics_field",
    "Q145_First",
    "Q145_Last",
    "duration_CC_field",
    "Q145_Click",
    "CC_field",
    "Q144_First",
    "Q144_Last",
    "duration_policies_field",
    "Q144_Click",
    "policies_field", 
    "CC_exists", 
    "CC_dynamics",
    "CC_factor_beef",
    "CC_factor_nuclear",
    "CC_factor_car",
    "CC_responsible_each",
    "CC_responsible_rich",
    "CC_responsible_govts",
    "CC_responsible_companies",
    "CC_responsible_past", 
    "CC_responsible_foreign", 
    "CC_responsible_nature",
    "CC_responsible_denial",
    "CC_responsible_order_each",
    "CC_responsible_order_rich",
    "CC_responsible_order_govts",
    "CC_responsible_order_companies",
    "CC_responsible_order_past",
    "CC_responsible_order_foreign",
    "CC_responsible_order_nature",
    "CC_responsible_order_denial",
    "CC_stoppable",
    "CC_stoppable_order_no_influence",
    "CC_stoppable_order_adapt",
    "CC_stoppable_order_pessimistic",
    "CC_stoppable_order_policies",
    "CC_stoppable_order_optimistic",
    "CC_stoppable_order_pnr",
    "CC_talks",
    "CC_affected_1960",
    "CC_affected_1990",
    "CC_affected_2020",
    "CC_affected_2050",
    "CC_affected_none",
    "CC_affected_pnr",
    "change_lifestyle",
    "change_condition_policies",
    "change_condition_income",
    "change_condition_all",
    "change_condition_no_rich",
    "change_condition_no_selfish",
    "change_condition_no_denial",
    "change_condition_already",
    "change_condition_try",
    "change_condition_pnr",
    "effect_policies_opportunity",
    "effect_policies_cost",
    "effect_policies_lifestyle",
    "effect_policies_pnr",
    "kaya_techno",
    "kaya_waste",
    "kaya_wealth",
    "kaya_overconsumption",
    "kaya_overpopulation",
    "kaya_none",
    "kaya_other_choice",
    "kaya_pnr",
    "kaya_other",
    "Q147_First",
    "Q147_Last",
    "duration_burden_sharing",
    "Q147_Click",
    "scale_local",
    "scale_state",
    "scale_federal",
    "scale_global",
    "scale_pnr",
    "burden_sharing_income",
    "burden_sharing_emissions",
    "burden_sharing_cumulative",
    "burden_sharing_rich_pay",
    "burden_sharing_poor_receive",
    "equal_quota",
    "country_should_act",
    "country_should_act_condition", 
    "pro_global_assembly",
    "pro_global_tax", 
    "pro_tax_1p",
    "Q140_First",
    "Q140_Last",
    "duration_treatment_climate",
    "Q140_Click",
    "Q141_First",
    "Q141_Last",
    "duration_treatment_policy",
    "Q141_Click",
    "standard_exists", 
    "standard_trust",
    "standard_effective",
    "standard_employment",
    "standard_side_effects",
    "standard_incidence_poor",
    "standard_incidence_middle",
    "standard_incidence_rich",
    "standard_incidence_urban",
    "standard_incidence_rural",
    "standard_incidence_self",
    "standard_support",
    "investments_trust",
    "investments_effective",
    "investments_employment", 
    "investments_side_effects",
    "investments_incidence_poor",
    "investments_incidence_middle",
    "investments_incidence_rich",
    "investments_incidence_urban",
    "investments_incidence_rural",
    "investments_incidence_self",
    "investments_support",
    "Q142_First",
    "Q142_Last",
    "duration_tax_transfers",
    "Q142_Click",
    "tax_transfers_trust",
    "tax_transfers_effective",
    "tax_transfers_employment",
    "tax_transfers_side_effects", 
    "tax_transfers_incidence_poor",
    "tax_transfers_incidence_middle",
    "tax_transfers_incidence_rich", 
    "tax_transfers_incidence_urban", 
    "tax_transfers_incidence_rural", 
    "tax_transfers_incidence_self",
    "tax_transfers_support", 
    "Q143_First", 
    "Q143_Last", 
    "duration_policies",
    "Q143_Click",
    "CC_worries",
    "policy_tax_flying",
    "policy_tax_fuels",
    "policy_insulation",
    "policy_ban_city_centers",
    "policy_subsidies",
    "policy_climate_fund",
    "policy_order_tax_flying",
    "policy_order_tax_fuels",
    "policy_order_insulation",
    "policy_order_ban_city_centers", 
    "policy_order_subsidies",
    "policy_order_climate_fund",
    "tax_transfer_constrained_hh", 
    "tax_transfer_poor",
    "tax_transfer_all",
    "tax_rebates_affected_firms",
    "tax_investments",
    "tax_subsidies",
    "tax_reduction_deficit",
    "tax_reduction_corporate_tax",
    "tax_reduction_personal_tax",
    "tax_other_choice",
    "tax_other",
    "tax_order_transfers_constrained_hh",
    "tax_order_transfers_poor",
    "tax_order_transfers_all",
    "tax_order_rebates_affected_firms",
    "tax_order_investments",
    "tax_order_subsidies",
    "tax_order_reduction_deficit",
    "tax_order_reduction_corporate_tax",
    "tax_order_reduction_personal_tax",
    "tax_order_other",
    "insulation_compulsory",
    "flight_quota_1000km", 
    "flight_quota_3000km",
    "flight_quota_one_trip",
    "beef_tax",
    "beef_subsidies_vegetables",
    "beef_subsidies_removal",
    "beef_ban_intensive",
    "beef_pnr",
    "beef_order_tax",
    "beef_order_subsidies_vegetables",
    "beef_order_subsidies_removal",
    "beef_order_ban_intensive",
    "beef_order_pnr",
    "ban_incentives",
    "wtp",
    "interest_politics", 
    "member_environmental_orga",
    "relative_environmentalist",
    "far_left",
    "left",
    "center",
    "right",
    "far_right",
    "liberal",
    "conservative",
    "humanist",
    "patriot",
    "apolitical",
    "environmentalist",
    "feminist",
    "political_identity_other_choice",
    "political_identity_other",
    "media",
    "vote_participation",
    "vote",
    "vote_other",
    "survey_biased",
    "comment_field",
    "language",
    "finished",
    "excluded",
    "duration",
    # "PSID",
    # "PID",
    "urban_category",
    "treatment_policy",
    "treatment_climate",
    "Preferenceforbansvs.incentives_DO_Q16.5",
    "Preferenceforbansvs.incentives_DO_Q16.4",
    "Preferenceforbansvs.incentives_DO_Q16.2",
    "Preferenceforbansvs.incentives_DO_Q16.3",
    "Preferenceforbansvs.incentives_DO_Q16.1",
    "Preferenceforbansvs.incentives_DO_Q16.6")
  
  } # TODO: remove ip, "PSID", "ResponseId", "PID" for consistency with remove_id
  if (country == "US" & wave == "pilot2") {
    names(e) <- c(
      "date",
      "date_end",
      "statut_reponse",
      "ip",
      "progress",
      "time",
      "terminated",
      "date_recored",
      # "ID_qualtrics",
      "name",
      "firstname",
      "mmail",
      "ref",
      "lat",
      "long",
      "distr",
      "lang",
      "gender",
      "age",
      "region",
      "zipcode",
      "urbanity",
      "race_white",
      "race_black",
      "race_hispanic",
      "race_asian",
      "race_native",
      "race_hawaii",
      "race_other_choice",
      "race_pnr",
      "race_other",
      "speaks_well",
      "education", 
      "employment_status",
      "hit_by_covid",
      "income",
      "home_tenant",
      "home_owner",
      "home_landlord",
      "home_hosted",
      "wealth",
      "nb_children",
      "hh_adults",
      "hh_children",
      "heating",
      "km_driven",
      "flights",
      "frequency_beef",
      "transport_work",
      "transport_shopping",
      "transport_leisure",
      "transport_available",
      "trust_people",
      "trust_govt",
      "trust_public_spending",
      "statist",
      "inequality_problem",
      "future_gdp",
      "envi", 
      "envi_order_collapse",
      "envi_order_progress",
      "envi_order_pro_envi",
      "envi_order_anti_envi",
      "envi_order_pnr",
      "Q146_First",
      "Q146_Last",
      "duration_politics_field",
      "Q146_Click",
      "politics_field",
      "Q145_First",
      "Q145_Last",
      "duration_CC_field",
      "Q145_Click",
      "CC_field",
      "Q144_First",
      "Q144_Last",
      "duration_policies_field",
      "Q144_Click",
      "policies_field", 
      "CC_exists", 
      "CC_dynamics",
      "CC_factor_beef",
      "CC_factor_nuclear",
      "CC_factor_car",
      "CC_responsible_each",
      "CC_responsible_rich",
      "CC_responsible_govts",
      "CC_responsible_companies",
      "CC_responsible_past", 
      "CC_responsible_foreign", 
      "CC_responsible_nature",
      "CC_responsible_denial",
      "CC_responsible_order_each",
      "CC_responsible_order_rich",
      "CC_responsible_order_govts",
      "CC_responsible_order_companies",
      "CC_responsible_order_past",
      "CC_responsible_order_foreign",
      "CC_responsible_order_nature",
      "CC_responsible_order_denial",
      "CC_stoppable",
      "CC_stoppable_order_no_influence",
      "CC_stoppable_order_adapt",
      "CC_stoppable_order_pessimistic",
      "CC_stoppable_order_policies",
      "CC_stoppable_order_optimistic",
      "CC_stoppable_order_pnr",
      "CC_talks",
      "CC_impacts", 
      "change_condition_policies",
      "change_condition_income",
      "change_condition_all",
      "change_condition_no_rich",
      "change_condition_no_selfish",
      "change_condition_no_denial",
      "change_condition_already",
      "change_condition_try",
      "change_condition_pnr",
      "effect_policies_opportunity",
      "effect_policies_cost",
      "effect_policies_lifestyle",
      "effect_policies_pnr",
      "kaya_techno",
      "kaya_waste",
      "kaya_wealth",
      "kaya_overconsumption",
      "kaya_overpopulation",
      "kaya_none",
      "kaya_other_choice",
      "kaya_pnr",
      "kaya_other",
      "Q147_First",
      "Q147_Last",
      "duration_burden_sharing",
      "Q147_Click",
      "scale_local",
      "scale_state",
      "scale_federal",
      "scale_global",
      "scale_pnr",
      "burden_sharing_income",
      "burden_sharing_emissions",
      "burden_sharing_cumulative",
      "burden_sharing_rich_pay",
      "burden_sharing_poor_receive",
      "equal_quota",
      "pro_global_assembly",
      "pro_global_tax", 
      "pro_tax_1p",
      "Q140_First",
      "Q140_Last",
      "duration_treatment_climate",
      "Q140_Click",
      "watched_climate", # TODO
      "enjoyed_climate",
      "learned_climate",
      "Q141_First",
      "Q141_Last",
      "duration_treatment_policy",
      "Q141_Click",
      "watched_policy",
      "enjoyed_policy",
      "learned_policy",
      "CC_worries",
      "change_lifestyle",
      "country_should_act",
      "country_should_act_condition", 
      "standard_exists", 
      "standard_trust",
      "standard_effective",
      "standard_employment",
      "standard_side_effects",
      "standard_incidence_poor",
      "standard_incidence_middle",
      "standard_incidence_rich",
      "standard_incidence_urban",
      "standard_incidence_rural",
      "standard_incidence_self",
      "standard_support",
      "investments_trust",
      "investments_effective",
      "investments_employment", 
      "investments_side_effects",
      "investments_incidence_poor",
      "investments_incidence_middle",
      "investments_incidence_rich",
      "investments_incidence_urban",
      "investments_incidence_rural",
      "investments_incidence_self",
      "investments_support",
      "Q142_First",
      "Q142_Last",
      "duration_tax_transfers",
      "Q142_Click",
      "tax_transfers_trust",
      "tax_transfers_effective",
      "tax_transfers_employment",
      "tax_transfers_side_effects", 
      "tax_transfers_incidence_poor",
      "tax_transfers_incidence_middle",
      "tax_transfers_incidence_rich", 
      "tax_transfers_incidence_urban", 
      "tax_transfers_incidence_rural", 
      "tax_transfers_incidence_self",
      "tax_transfers_support", 
      "Q143_First", 
      "Q143_Last", 
      "duration_policies",
      "Q143_Click",
      "inattentive", # TODO
      "policy_tax_flying",
      "policy_tax_fuels",
      "policy_insulation",
      "policy_ban_city_centers",
      "policy_subsidies",
      "policy_climate_fund",
      "policy_order_tax_flying",
      "policy_order_tax_fuels",
      "policy_order_insulation",
      "policy_order_ban_city_centers", 
      "policy_order_subsidies",
      "policy_order_climate_fund",
      "tax_transfer_constrained_hh", 
      "tax_transfer_poor",
      "tax_transfer_all",
      "tax_rebates_affected_firms",
      "tax_investments",
      "tax_subsidies",
      "tax_reduction_deficit",
      "tax_reduction_corporate_tax",
      "tax_reduction_personal_tax",
      "tax_other_choice",
      "tax_other",
      "tax_order_transfers_constrained_hh",
      "tax_order_transfers_poor",
      "tax_order_transfers_all",
      "tax_order_rebates_affected_firms",
      "tax_order_investments",
      "tax_order_subsidies",
      "tax_order_reduction_deficit",
      "tax_order_reduction_corporate_tax",
      "tax_order_reduction_personal_tax",
      "tax_order_other",
      "insulation_compulsory",
      "flight_quota_1000km", 
      "flight_quota_1000km_global",
      "flight_quota_one_trip",
      "beef_tax",
      "beef_subsidies_vegetables",
      "beef_subsidies_removal",
      "beef_ban_intensive",
      "beef_pnr",
      "beef_order_tax",
      "beef_order_subsidies_vegetables",
      "beef_order_subsidies_removal",
      "beef_order_ban_intensive",
      "beef_order_pnr",
      "ban_incentives",
      "wtp",
      "interest_politics", 
      "member_environmental_orga",
      "relative_environmentalist",
      "far_left",
      "left",
      "center",
      "right",
      "far_right",
      "liberal",
      "conservative",
      "humanist",
      "patriot",
      "apolitical",
      "environmentalist",
      "feminist",
      "political_identity_other_choice",
      "political_identity_other",
      "media",
      "vote_participation",
      "vote_participation_2016", # TODO
      "vote_voters",
      "vote_non_voters",
      "vote_voters_2016",
      "vote_non_voters_2016",
      "liberal_conservative", # TODO
      "political_affiliation",
      "survey_biased", 
      "comment_field",
      "language",
      "finished",
      "excluded",
      "duration",
      # "PSID",
      # "PID",
      "urban_category",
      "treatment_policy",
      "treatment_climate",
      "Preferenceforbansvs.incentives_DO_Q16.5",
      "Preferenceforbansvs.incentives_DO_Q16.4",
      "Preferenceforbansvs.incentives_DO_Q16.2",
      "Preferenceforbansvs.incentives_DO_Q16.3",
      "Preferenceforbansvs.incentives_DO_Q16.1",
      "Preferenceforbansvs.incentives_DO_Q16.6")
    
  }
  if (country == "US" & wave == "pilot3") {
    names(e) <- c(
      "date",
      "date_end",
      "statut_reponse",
      "progress",
      "time",
      "terminated",
      "date_recored",
      "distr",
      "lang",
      "consent",
      "gender",
      "age_quota",
      "zipcode",
      "urbanity",
      "race_white",
      "race_black",
      "race_hispanic",
      "race_asian",
      "race_native",
      "race_hawaii",
      "race_other_choice",
      "race_pnr",
      "race_other",
      "income",
      "wealth",
      "education", 
      "employment_status",
      "occupation",
      "sector_choice",
      "sector_other",
      "sector",
      "hit_by_covid",
      "home_tenant",
      "home_owner",
      "home_landlord",
      "home_hosted",
      "couple",
      "marital_status",
      "Nb_children",
      "HH_size",
      "heating",
      "heating_expenses",
      "insulation",
      "gas_expenses",
      "flights_agg",
      "frequency_beef",
      "transport_work",
      "transport_shopping",
      "transport_leisure",
      "availability_transport",
      "Q3.7_First",
      "Q3.7_Last",
      "duration_energy",
      "Q3.7_Click",
      "Q5.1_First",
      "Q5.1_Last",
      "duration_CC_field",
      "Q5.1_Click",
      "CC_field",
      "Q140_First",
      "Q140_Last",
      "duration_treatment_climate",
      "Q140_Click",
      "watched_climate", # TODO
      "know_temperature_2100",
      "know_frequence_heatwaves",
      "Q141_First",
      "Q141_Last",
      "duration_treatment_policy",
      "Q141_Click",
      "watched_policy",
      "know_investments_jobs",
      "know_standard",
      "CC_talks",
      "CC_real",
      "CC_anthropogenic",
      "CC_problem",
      "CC_knowledgeable",
      "GHG_CO2",
      "GHG_H2",
      "GHG_methane",
      "GHG_particulates",
      "GHG_dont_know",
      "CC_dynamic",
      "footprint_tr_car",
      "footprint_tr_coach",
      "footprint_tr_plane",
      "footprint_tr_order_car",
      "footprint_tr_order_coach",
      "footprint_tr_order_plane",
      "footprint_fd_beef",
      "footprint_fd_pasta",
      "footprint_fd_chicken",
      "footprint_fd_order_beef",
      "footprint_fd_order_pasta",
      "footprint_fd_order_chicken",
      "footprint_el_gas",
      "footprint_el_wind",
      "footprint_el_coal",
      "footprint_el_order_gas",
      "footprint_el_order_wind",
      "footprint_el_order_coal",
      "CC_impacts_droughts",
      "CC_impacts_volcanos",
      "CC_impacts_sea_rise",
      "CC_impacts_low_yield",
      "CC_impacts_drop_conso",
      "CC_impacts_more_migration",
      "CC_impacts_more_wars",
      "CC_impacts_extinction",
      "footprint_reg_US",
      "footprint_reg_europe",
      "footprint_reg_china",
      "footprint_reg_india",
      "footprint_reg_order_US",
      "footprint_reg_order_europe",
      "footprint_reg_order_china",
      "footprint_reg_order_india",
      "responsible_CC_each", # TODO: rename in CC_responsible_ and change in previous waves?
      "responsible_CC_rich",
      "responsible_CC_govt",
      "responsible_CC_companies",
      "responsible_CC_past", 
      "responsible_CC_order_each",
      "responsible_CC_order_rich",
      "responsible_CC_order_govt",
      "responsible_CC_order_companies",
      "responsible_CC_order_past",
      "net_zero_feasible", # TODO rename in CC_stoppable and change name in previous waves?
      "CC_affects_self",
      "pro_ambitious_policies",
      "CC_will_end",
      "effect_halt_CC_economy",
      "effect_halt_CC_lifestyle",
      "willing_limit_flying",
      "willing_limit_driving",
      "willing_electric_car",
      "willing_limit_beef",
      "willing_limit_heating",
      "condition_ambitious_policies",
      "condition_financial_aid",
      "condition_people_change",
      "condition_rich_change",
      "standard_effect_less_emission",
      "standard_effect_less_pollution",
      "standard_large_effect",
      "standard_negative_effect",
      "standard_cost_effective",
      "standard_win_lose_poor",
      "standard_win_lose_middle",
      "standard_win_lose_rich",
      "standard_win_lose_rural",
      "standard_win_lose_self",
      "standard_fair",
      "standard_support",
      "standard_public_transport_support",
      "investments_effect_elec_greener",
      "investments_effect_public_transport",
      "investments_effect_less_pollution",
      "investments_large_effect",
      "investments_negative_effect",
      "investments_cost_effective",
      "investments_win_lose_poor",
      "investments_win_lose_middle",
      "investments_win_lose_rich",
      "investments_win_lose_rural",
      "investments_win_lose_self",
      "investments_fair",
      "investments_support",
      "investments_funding_debt",
      "investments_funding_sales_tax",
      "investments_funding_wealth_tax",
      "investments_funding_less_social",
      "investments_funding_less_military",
      "Q142_First",
      "Q142_Last",
      "duration_tax_transfers",
      "Q142_Click",
      "tax_transfers_effect_driving",
      "tax_transfers_effect_insulation",
      "tax_transfers_effect_less_emission",
      "tax_transfers_effect_less_pollution",
      "tax_transfers_large_effect",
      "tax_transfers_negative_effect",
      "tax_transfers_cost_effective",
      "tax_transfers_win_lose_poor",
      "tax_transfers_win_lose_middle",
      "tax_transfers_win_lose_rich",
      "tax_transfers_win_lose_rural",
      "tax_transfers_win_lose_self",
      "tax_transfers_fair",
      "tax_transfers_support",
      "Q142_First",
      "Q142_Last",
      "duration_policies",
      "Q142_Click",
      "news_ABC",
      "news_CNBC",
      "news_CNN",
      "news_FOX",
      "news_New_York_Post",
      "news_Drudge",
      "news_WSJ",
      "news_Mercury",
      "news_NYT",
      "news_USA_Today",
      "news_other",
      "policy_tax_flying",
      "policy_tax_fuels",
      "policy_insulation",
      "policy_ban_city_centers",
      "policy_subsidies",
      "policy_climate_fund",
      "policy_order_tax_flying",
      "policy_order_tax_fuels",
      "policy_order_insulation",
      "policy_order_ban_city_centers", 
      "policy_order_subsidies",
      "policy_order_climate_fund",
      "tax_transfer_constrained_hh", 
      "tax_transfer_poor",
      "tax_transfer_all",
      "tax_reduction_personal_tax",
      "tax_reduction_corporate_tax",
      "tax_rebates_affected_firms",
      "tax_investments",
      "tax_subsidies",
      "tax_reduction_deficit",
      "tax_order_transfers_constrained_hh",
      "tax_order_transfers_poor",
      "tax_order_transfers_all",
      "tax_order_reduction_personal_tax",
      "tax_order_reduction_corporate_tax",
      "tax_order_rebates_affected_firms",
      "tax_order_investments",
      "tax_order_subsidies",
      "tax_order_reduction_deficit",
      "wtp",
      "donation",
      "Q147_First",
      "Q147_Last",
      "duration_burden_sharing",
      "Q147_Click",
      "scale_global",
      "scale_federal",
      "scale_state",
      "scale_local",
      "should_fight_CC",
      "if_other_do_more",
      "if_other_do_less",
      "burden_sharing_income",
      "burden_sharing_emissions",
      "burden_sharing_cumulative",
      "burden_sharing_rich_pay",
      "burden_sharing_poor_receive",
      "global_assembly_support",
      "global_tax_support", 
      "tax_1p_support",
      "will_insulate",
      "obstacles_insulation_cannot",
      "obstacles_insulation_cost",
      "obstacles_insulation_effort",
      "obstacles_insulation_useless",
      "obstacles_insulation_other_choice",
      "obstacles_insulation_other",
      "insulation_subsidies_support",
      "insulation_mandatory_support",
      "beef_tax_support",
      "beef_subsidies_vegetables_support",
      "beef_subsidies_removal_support",
      "beef_ban_intensive_support",
      "beef_order_tax_support",
      "beef_order_subsidies_vegetables_support",
      "beef_order_subsidies_removal_support",
      "beef_order_ban_intensive_support",
      "can_trust_people",
      "can_trust_govt",
      "view_govt",
      "problem_inequality",
      "future_richness",
      "interested_politics", 
      "member_environmental_orga",
      "relative_environmentalist",
      "vote_participation",
      "vote_participation_2016", # TODO
      "vote_voters",
      "vote_non_voters",
      "vote_voters_2016",
      "vote_non_voters_2016",
      "liberal_conservative", # TODO
      "political_affiliation",
      "survey_biased", 
      "comment_field",
      "winner_confirm",
      "winner_confirm_text",
      "language",
      "finished",
      "excluded",
      "duration",
      "urban_category",
      "treatment_climate",
      "treatment_policy",
      "region",
      "winner_latent",
      "winner"
      )
    
  }
  
  for (i in 1:length(e)) {
    label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") # 
    # print(paste(i, label(e[[i]])))
  }
  e <- e[-c(1:2),]
  
  {
  ## names\(e\[\[([0-9]*)\]\]\) -> names(e)[$1]
  # names(e)[1] <- "date"
  # names(e)[2] <- "date_end"
  # names(e)[3] <- "statut_reponse"
  # names(e)[4] <- "ip"
  # names(e)[5] <- "progress"
  # names(e)[6] <- "time"
  # names(e)[7] <- "terminated"
  # names(e)[8] <- "date_recored"
  # names(e)[9] <- "ID_qualtrics"
  # names(e)[10] <- "name"
  # names(e)[11] <- "firstname"
  # names(e)[12] <- "mmail"
  # names(e)[13] <- "ref"
  # names(e)[14] <- "lat"
  # names(e)[15] <- "long"
  # names(e)[16] <- "distr"
  # names(e)[17] <- "lang"
  # names(e)[18] <- "gender"
  # names(e)[19] <- "age"
  # names(e)[20] <- "region"
  # names(e)[21] <- "zipcode"
  # names(e)[22] <- "urbanity"
  # names(e)[23] <- "race_white"
  # names(e)[24] <- "race_black"
  # names(e)[25] <- "race_hispanic"
  # names(e)[26] <- "race_asian"
  # names(e)[27] <- "race_native"
  # names(e)[28] <- "race_hawaii"
  # names(e)[29] <- "race_other_choice"
  # names(e)[30] <- "race_pnr"
  # names(e)[31] <- "race_other"
  # names(e)[32] <- "speaks_well"
  # names(e)[33] <- "education" 
  # names(e)[34] <- "employment_status"
  # names(e)[35] <- "hit_by_covid"
  # names(e)[36] <- "income"
  # names(e)[37] <- "home_tenant"
  # names(e)[38] <- "home_owner"
  # names(e)[39] <- "home_landlord"
  # names(e)[40] <- "home_hosted"
  # names(e)[41] <- "wealth"
  # names(e)[42] <- "nb_children"
  # names(e)[43] <- "hh_adults"
  # names(e)[44] <- "hh_children"
  # names(e)[45] <- "heating"
  # names(e)[46] <- "km_driven"
  # names(e)[47] <- "flights"
  # names(e)[48] <- "frequency_beef"
  # names(e)[49] <- "transport_work"
  # names(e)[50] <- "transport_shopping"
  # names(e)[51] <- "transport_leisure"
  # names(e)[52] <- "transport_available"
  # names(e)[53] <- "trust_people"
  # names(e)[54] <- "trust_govt"
  # names(e)[55] <- "trust_public_spending"
  # names(e)[56] <- "statist"
  # names(e)[57] <- "inequality_problem"
  # names(e)[58] <- "future_gdp"
  # names(e)[59] <- "envi" # longer name? TODO
  # names(e)[60] <- "envi_order_collapse"
  # names(e)[61] <- "envi_order_progress"
  # names(e)[62] <- "envi_order_pro_envi"
  # names(e)[63] <- "envi_order_anti_envi"
  # names(e)[64] <- "envi_order_pnr"
  # names(e)[65] <- "Q146_First"
  # names(e)[66] <- "Q146_Last"
  # names(e)[67] <- "duration_politics_field"
  # names(e)[68] <- "Q146_Click"
  # names(e)[69] <- "politics_field"
  # names(e)[70] <- "Q145_First"
  # names(e)[71] <- "Q145_Last"
  # names(e)[72] <- "duration_CC_field"
  # names(e)[73] <- "Q145_Click"
  # names(e)[74] <- "CC_field"
  # names(e)[75] <- "Q144_First"
  # names(e)[76] <- "Q144_Last"
  # names(e)[77] <- "duration_policies_field"
  # names(e)[78] <- "Q144_Click"
  # names(e)[79] <- "policies_field" # specify climate? TODO
  # names(e)[80] <- "CC_exists" 
  # names(e)[81] <- "CC_dynamics"
  # names(e)[82] <- "CC_factor_beef"
  # names(e)[83] <- "CC_factor_nuclear"
  # names(e)[84] <- "CC_factor_car"
  # names(e)[85] <- "CC_responsible_each"
  # names(e)[86] <- "CC_responsible_rich"
  # names(e)[87] <- "CC_responsible_govts"
  # names(e)[88] <- "CC_responsible_companies"
  # names(e)[89] <- "CC_responsible_past" 
  # names(e)[90] <- "CC_responsible_foreign" 
  # names(e)[91] <- "CC_responsible_nature"
  # names(e)[92] <- "CC_responsible_denial"
  # names(e)[93] <- "CC_responsible_order_each"
  # names(e)[94] <- "CC_responsible_order_rich"
  # names(e)[95] <- "CC_responsible_order_govts"
  # names(e)[96] <- "CC_responsible_order_companies"
  # names(e)[97] <- "CC_responsible_order_past"
  # names(e)[98] <- "CC_responsible_order_foreign"
  # names(e)[99] <- "CC_responsible_order_nature"
  # names(e)[100] <- "CC_responsible_order_denial"
  # names(e)[101] <- "CC_stoppable"
  # names(e)[102] <- "CC_stoppable_order_no_influence"
  # names(e)[103] <- "CC_stoppable_order_adapt"
  # names(e)[104] <- "CC_stoppable_order_pessimistic"
  # names(e)[105] <- "CC_stoppable_order_policies"
  # names(e)[106] <- "CC_stoppable_order_optimistic"
  # names(e)[107] <- "CC_stoppable_order_pnr"
  # names(e)[108] <- "CC_talks"
  # names(e)[109] <- "CC_affected_1960"
  # names(e)[110] <- "CC_affected_1990"
  # names(e)[111] <- "CC_affected_2020"
  # names(e)[112] <- "CC_affected_2050"
  # names(e)[113] <- "CC_affected_none"
  # names(e)[114] <- "CC_affected_pnr"
  # names(e)[115] <- "change_lifestyle"
  # names(e)[116] <- "change_condition_policies"
  # names(e)[117] <- "change_condition_income"
  # names(e)[118] <- "change_condition_all"
  # names(e)[119] <- "change_condition_no_rich"
  # names(e)[120] <- "change_condition_no_selfish"
  # names(e)[121] <- "change_condition_no_denial"
  # names(e)[122] <- "change_condition_already"
  # names(e)[123] <- "change_condition_try"
  # names(e)[124] <- "change_condition_pnr"
  # names(e)[125] <- "effect_policies_opportunity"
  # names(e)[126] <- "effect_policies_cost"
  # names(e)[127] <- "effect_policies_lifestyle"
  # names(e)[128] <- "effect_policies_pnr"
  # names(e)[129] <- "kaya_techno"
  # names(e)[130] <- "kaya_waste"
  # names(e)[131] <- "kaya_wealth"
  # names(e)[132] <- "kaya_overconsumption"
  # names(e)[133] <- "kaya_overpopulation"
  # names(e)[134] <- "kaya_none"
  # names(e)[135] <- "kaya_other_choice"
  # names(e)[136] <- "kaya_pnr"
  # names(e)[137] <- "kaya_other"
  # names(e)[138] <- "Q147_First"
  # names(e)[139] <- "Q147_Last"
  # names(e)[140] <- "duration_burden_sharing"
  # names(e)[141] <- "Q147_Click"
  # names(e)[142] <- "scale_local"
  # names(e)[143] <- "scale_state"
  # names(e)[144] <- "scale_federal"
  # names(e)[145] <- "scale_global"
  # names(e)[146] <- "scale_pnr"
  # names(e)[147] <- "burden_sharing_income"
  # names(e)[148] <- "burden_sharing_emissions"
  # names(e)[149] <- "burden_sharing_cumulative"
  # names(e)[150] <- "burden_sharing_rich_pay"
  # names(e)[151] <- "burden_sharing_poor_receive"
  # names(e)[152] <- "equal_quota"
  # names(e)[153] <- "country_should_act"
  # names(e)[154] <- "country_should_act_condition" # If yes to country_should_act
  # names(e)[155] <- "pro_global_assembly"
  # names(e)[156] <- "pro_global_tax" # TODO: check % & allowed
  # names(e)[157] <- "pro_tax_1p"
  # names(e)[158] <- "Q140_First"
  # names(e)[159] <- "Q140_Last"
  # names(e)[160] <- "duration_treatment_climate"
  # names(e)[161] <- "Q140_Click"
  # names(e)[162] <- "Q141_First"
  # names(e)[163] <- "Q141_Last"
  # names(e)[164] <- "duration_treatment_policy"
  # names(e)[165] <- "Q141_Click"
  # names(e)[166] <- "standard_exists" 
  # names(e)[167] <- "standard_trust"
  # names(e)[168] <- "standard_effective"
  # names(e)[169] <- "standard_employment"
  # names(e)[170] <- "standard_side_effects"
  # names(e)[171] <- "standard_incidence_poor"
  # names(e)[172] <- "standard_incidence_middle"
  # names(e)[173] <- "standard_incidence_rich"
  # names(e)[174] <- "standard_incidence_urban"
  # names(e)[175] <- "standard_incidence_rural"
  # names(e)[176] <- "standard_incidence_self"
  # names(e)[177] <- "standard_support"
  # names(e)[178] <- "investments_trust"
  # names(e)[179] <- "investments_effective"
  # names(e)[180] <- "investments_employment" 
  # names(e)[181] <- "investments_side_effects"
  # names(e)[182] <- "investments_incidence_poor"
  # names(e)[183] <- "investments_incidence_middle"
  # names(e)[184] <- "investments_incidence_rich"
  # names(e)[185] <- "investments_incidence_urban"
  # names(e)[186] <- "investments_incidence_rural"
  # names(e)[187] <- "investments_incidence_self"
  # names(e)[188] <- "investments_support"
  # names(e)[189] <- "Q142_First"
  # names(e)[190] <- "Q142_Last"
  # names(e)[191] <- "duration_tax_transfers"
  # names(e)[192] <- "Q142_Click"
  # names(e)[193] <- "tax_transfers_trust"
  # names(e)[194] <- "tax_transfers_effective"
  # names(e)[195] <- "tax_transfers_employment"
  # names(e)[196] <- "tax_transfers_side_effects" 
  # names(e)[197] <- "tax_transfers_incidence_poor"
  # names(e)[198] <- "tax_transfers_incidence_middle"
  # names(e)[199] <- "tax_transfers_incidence_rich" 
  # names(e)[200] <- "tax_transfers_incidence_urban" 
  # names(e)[201] <- "tax_transfers_incidence_rural" 
  # names(e)[202] <- "tax_transfers_incidence_self"
  # names(e)[203] <- "tax_transfers_support" 
  # names(e)[204] <- "Q143_First" 
  # names(e)[205] <- "Q143_Last" 
  # names(e)[206] <- "duration_policies"
  # names(e)[207] <- "Q143_Click"
  # names(e)[208] <- "CC_worries"
  # names(e)[209] <- "policy_tax_flying"
  # names(e)[210] <- "policy_tax_fuels"
  # names(e)[211] <- "policy_insulation"
  # names(e)[212] <- "policy_ban_city_centers"
  # names(e)[213] <- "policy_subsidies"
  # names(e)[214] <- "policy_climate_fund"
  # names(e)[215] <- "policy_order_tax_flying"
  # names(e)[216] <- "policy_order_tax_fuels"
  # names(e)[217] <- "policy_order_insulation"
  # names(e)[218] <- "policy_order_ban_city_centers" 
  # names(e)[219] <- "policy_order_subsidies"
  # names(e)[220] <- "policy_order_climate_fund"
  # names(e)[221] <- "tax_transfer_constrained_hh" # don't put "s" at transfers because it would create a bug
  # names(e)[222] <- "tax_transfer_poor"
  # names(e)[223] <- "tax_transfer_all"
  # names(e)[224] <- "tax_rebates_affected_firms"
  # names(e)[225] <- "tax_investments"
  # names(e)[226] <- "tax_subsidies"
  # names(e)[227] <- "tax_reduction_deficit"
  # names(e)[228] <- "tax_reduction_corporate_tax"
  # names(e)[229] <- "tax_reduction_personal_tax"
  # names(e)[230] <- "tax_other_choice"
  # names(e)[231] <- "tax_other"
  # names(e)[232] <- "tax_order_transfers_constrained_hh"
  # names(e)[233] <- "tax_order_transfers_poor"
  # names(e)[234] <- "tax_order_transfers_all"
  # names(e)[235] <- "tax_order_rebates_affected_firms"
  # names(e)[236] <- "tax_order_investments"
  # names(e)[237] <- "tax_order_subsidies"
  # names(e)[238] <- "tax_order_reduction_deficit"
  # names(e)[239] <- "tax_order_reduction_corporate_tax"
  # names(e)[240] <- "tax_order_reduction_personal_tax"
  # names(e)[241] <- "tax_order_other"
  # names(e)[242] <- "insulation_compulsory"
  # names(e)[243] <- "flight_quota_1000km" # TODO variant
  # names(e)[244] <- "flight_quota_3000km"
  # names(e)[245] <- "flight_quota_one_trip"
  # names(e)[246] <- "beef_tax"
  # names(e)[247] <- "beef_subsidies_vegetables"
  # names(e)[248] <- "beef_subsidies_removal"
  # names(e)[249] <- "beef_ban_intensive"
  # names(e)[250] <- "beef_pnr"
  # names(e)[251] <- "beef_order_tax"
  # names(e)[252] <- "beef_order_subsidies_vegetables"
  # names(e)[253] <- "beef_order_subsidies_removal"
  # names(e)[254] <- "beef_order_ban_intensive"
  # names(e)[255] <- "beef_order_pnr"
  # names(e)[256] <- "ban_incentives"
  # names(e)[257] <- "wtp"
  # names(e)[258] <- "interest_politics" 
  # names(e)[259] <- "member_environmental_orga"
  # names(e)[260] <- "relative_environmentalist"
  # names(e)[261] <- "far_left"
  # names(e)[262] <- "left"
  # names(e)[263] <- "center"
  # names(e)[264] <- "right"
  # names(e)[265] <- "far_right"
  # names(e)[266] <- "liberal"
  # names(e)[267] <- "conservative"
  # names(e)[268] <- "humanist"
  # names(e)[269] <- "patriot"
  # names(e)[270] <- "apolitical"
  # names(e)[271] <- "environmentalist"
  # names(e)[272] <- "feminist"
  # names(e)[273] <- "political_identity_other_choice"
  # names(e)[274] <- "political_identity_other"
  # names(e)[275] <- "media"
  # names(e)[276] <- "vote_participation"
  # names(e)[277] <- "vote"
  # names(e)[278] <- "vote_other"
  # names(e)[279] <- "survey_biased"
  # names(e)[280] <- "comment_field"
  # names(e)[281] <- "language"
  # names(e)[282] <- "finished"
  # names(e)[283] <- "excluded"
  # names(e)[284] <- "duration"
  # names(e)[285] <- "PSID"
  # names(e)[286] <- "PID"
  # names(e)[287] <- "urban_category"
  # names(e)[288] <- "treatment_policy"
  # names(e)[289] <- "treatment_climate"
  # names(e)[290] <- "Preferenceforbansvs.incentives_DO_Q16.5"
  # names(e)[291] <- "Preferenceforbansvs.incentives_DO_Q16.4"
  # names(e)[292] <- "Preferenceforbansvs.incentives_DO_Q16.2"
  # names(e)[293] <- "Preferenceforbansvs.incentives_DO_Q16.3"
  # names(e)[294] <- "Preferenceforbansvs.incentives_DO_Q16.1"
  # names(e)[295] <- "Preferenceforbansvs.incentives_DO_Q16.6"

  # label(e[[1]]) <- "date:"
  # label(e[[2]]) <- "date_fin:"
  # label(e[[3]]) <- "statut_reponse:"
  # label(e[[4]]) <- "ip:"
  # label(e[[5]]) <- "progres:"
  # label(e[[6]]) <- "duree:"
  # label(e[[7]]) <- "fini:"
  # label(e[[8]]) <- "date_enregistree:"
  # label(e[[9]]) <- "ID_qualtrics:"
  # label(e[[10]]) <- "nom:"
  # label(e[[11]]) <- "prenom:"
  # label(e[[12]]) <- "mmail:"
  # label(e[[13]]) <- "ref:"
  # label(e[[14]]) <- "lat:"
  # label(e[[15]]) <- "long:"
  # label(e[[16]]) <- "distr:"
  # label(e[[17]]) <- "lang:"
  # label(e[[18]]) <- ":"
  # label(e[[19]]) <- ":"
  # label(e[[20]]) <- ":"
  # label(e[[21]]) <- ":"
  # label(e[[22]]) <- ":"
  # label(e[[23]]) <- ":"
  # label(e[[24]]) <- ":"
  # label(e[[25]]) <- ":"
  # label(e[[26]]) <- ":"
  # label(e[[27]]) <- ":"
  # label(e[[28]]) <- ":"
  # label(e[[29]]) <- ":"
  # label(e[[30]]) <- ":"
  # label(e[[31]]) <- ":"
  # label(e[[32]]) <- ":"
  # label(e[[33]]) <- ":" 
  # label(e[[34]]) <- ":"
  # label(e[[35]]) <- ":"
  # label(e[[36]]) <- ":"
  # label(e[[37]]) <- ":"
  # label(e[[38]]) <- ":"
  # label(e[[39]]) <- ":"
  # label(e[[40]]) <- ":"
  # label(e[[41]]) <- ":"
  # label(e[[42]]) <- ":"
  # label(e[[43]]) <- ":"
  # label(e[[44]]) <- ":"
  # label(e[[45]]) <- ":"
  # label(e[[46]]) <- ":"
  # label(e[[47]]) <- ":"
  # label(e[[48]]) <- ":"
  # label(e[[49]]) <- ":"
  # label(e[[50]]) <- ":"
  # label(e[[51]]) <- ":"
  # label(e[[52]]) <- ":"
  # label(e[[53]]) <- ":"
  # label(e[[54]]) <- ":"
  # label(e[[55]]) <- ":"
  # label(e[[56]]) <- ":"
  # label(e[[57]]) <- ":"
  # label(e[[58]]) <- ":"
  # label(e[[59]]) <- ":"
  # label(e[[60]]) <- ":"
  # label(e[[61]]) <- ":"
  # label(e[[62]]) <- ":"
  # label(e[[63]]) <- ":"
  # label(e[[64]]) <- ":"
  # label(e[[65]]) <- ":"
  # label(e[[66]]) <- ":"
  # label(e[[67]]) <- ":"
  # label(e[[68]]) <- ":"
  # label(e[[69]]) <- ":"
  # label(e[[70]]) <- ":"
  # label(e[[71]]) <- ":"
  # label(e[[72]]) <- ":"
  # label(e[[73]]) <- ":"
  # label(e[[74]]) <- ":"
  # label(e[[75]]) <- ":"
  # label(e[[76]]) <- ":"
  # label(e[[77]]) <- ":"
  # label(e[[78]]) <- ":"
  # label(e[[79]]) <- ":"
  # label(e[[80]]) <- ":"
  # label(e[[81]]) <- ":"
  # label(e[[82]]) <- ":"
  # label(e[[83]]) <- ":"
  # label(e[[84]]) <- ":"
  # label(e[[85]]) <- ":"
  # label(e[[86]]) <- ":"
  # label(e[[87]]) <- ":"
  # label(e[[88]]) <- ":"
  # label(e[[89]]) <- ":" 
  # label(e[[90]]) <- ":" 
  # label(e[[91]]) <- ":"
  # label(e[[92]]) <- ":"
  # label(e[[93]]) <- ":"
  # label(e[[94]]) <- ":"
  # label(e[[95]]) <- ":"
  # label(e[[96]]) <- ":"
  # label(e[[97]]) <- ":"
  # label(e[[98]]) <- ":"
  # label(e[[99]]) <- ":"
  # label(e[[100]]) <- ":"
  # label(e[[101]]) <- ":"
  # label(e[[102]]) <- ":"
  # label(e[[103]]) <- ":"
  # label(e[[104]]) <- ":"
  # label(e[[105]]) <- ":"
  # label(e[[106]]) <- ":"
  # label(e[[107]]) <- ":"
  # label(e[[108]]) <- ":"
  # label(e[[109]]) <- ":"
  # label(e[[110]]) <- ":"
  # label(e[[111]]) <- ":"
  # label(e[[112]]) <- ":"
  # label(e[[113]]) <- ":"
  # label(e[[114]]) <- ":"
  # label(e[[115]]) <- ":"
  # label(e[[116]]) <- ":"
  # label(e[[117]]) <- ":"
  # label(e[[118]]) <- ":"
  # label(e[[119]]) <- ":"
  # label(e[[120]]) <- ":"
  # label(e[[121]]) <- ":"
  # label(e[[122]]) <- ":"
  # label(e[[123]]) <- ":"
  # label(e[[124]]) <- ":"
  # label(e[[125]]) <- ":"
  # label(e[[126]]) <- ":"
  # label(e[[127]]) <- ":"
  # label(e[[128]]) <- ":"
  # label(e[[129]]) <- ":"
  # label(e[[130]]) <- ":"
  # label(e[[131]]) <- ":"
  # label(e[[132]]) <- ":"
  # label(e[[133]]) <- ":"
  # label(e[[134]]) <- ":"
  # label(e[[135]]) <- ":"
  # label(e[[136]]) <- ":"
  # label(e[[137]]) <- ":"
  # label(e[[138]]) <- ":"
  # label(e[[139]]) <- ":"
  # label(e[[140]]) <- ":"
  # label(e[[141]]) <- ":"
  # label(e[[142]]) <- ":"
  # label(e[[143]]) <- ":"
  # label(e[[144]]) <- ":"
  # label(e[[145]]) <- ":"
  # label(e[[146]]) <- ":"
  # label(e[[147]]) <- ":"
  # label(e[[148]]) <- ":"
  # label(e[[149]]) <- ":"
  # label(e[[150]]) <- ":"
  # label(e[[151]]) <- ":"
  # label(e[[152]]) <- ":"
  # label(e[[153]]) <- ":"
  # label(e[[154]]) <- ":"
  # label(e[[155]]) <- ":"
  # label(e[[156]]) <- ":"
  # label(e[[157]]) <- ":"
  # label(e[[158]]) <- ":"
  # label(e[[159]]) <- ":"
  # label(e[[160]]) <- ":"
  # label(e[[161]]) <- ":"
  # label(e[[162]]) <- ":"
  # label(e[[163]]) <- ":"
  # label(e[[164]]) <- ":"
  # label(e[[165]]) <- ":"
  # label(e[[166]]) <- ":" 
  # label(e[[167]]) <- ":"
  # label(e[[168]]) <- ":"
  # label(e[[169]]) <- ":"
  # label(e[[170]]) <- ":"
  # label(e[[171]]) <- ":"
  # label(e[[172]]) <- ":"
  # label(e[[173]]) <- ":"
  # label(e[[174]]) <- ":"
  # label(e[[175]]) <- ":"
  # label(e[[176]]) <- ":"
  # label(e[[177]]) <- ":"
  # label(e[[178]]) <- ":"
  # label(e[[179]]) <- ":"
  # label(e[[180]]) <- ":" 
  # label(e[[181]]) <- ":"
  # label(e[[182]]) <- ":"
  # label(e[[183]]) <- ":"
  # label(e[[184]]) <- ":"
  # label(e[[185]]) <- ":"
  # label(e[[186]]) <- ":"
  # label(e[[187]]) <- ":"
  # label(e[[188]]) <- ":"
  # label(e[[189]]) <- ":"
  # label(e[[190]]) <- ":"
  # label(e[[191]]) <- ":"
  # label(e[[192]]) <- ":"
  # label(e[[193]]) <- ":"
  # label(e[[194]]) <- ":"
  # label(e[[195]]) <- ":"
  # label(e[[196]]) <- ":" 
  # label(e[[197]]) <- ":"
  # label(e[[198]]) <- ":"
  # label(e[[199]]) <- ":" 
  # label(e[[200]]) <- ":" 
  # label(e[[201]]) <- ":" 
  # label(e[[202]]) <- ":"
  # label(e[[203]]) <- ":" 
  # label(e[[204]]) <- ":" 
  # label(e[[205]]) <- ":" 
  # label(e[[206]]) <- ":"
  # label(e[[207]]) <- ":"
  # label(e[[208]]) <- ":"
  # label(e[[209]]) <- ":"
  # label(e[[210]]) <- ":"
  # label(e[[211]]) <- ""
  # label(e[[212]]) <- ":"
  # label(e[[213]]) <- ":"
  # label(e[[214]]) <- ":"
  # label(e[[215]]) <- ":"
  # label(e[[216]]) <- ":"
  # label(e[[217]]) <- ":"
  # label(e[[218]]) <- ":" 
  # label(e[[219]]) <- ":"
  # label(e[[220]]) <- ":"
  # label(e[[221]]) <- ":"
  # label(e[[222]]) <- ":"
  # label(e[[223]]) <- ":"
  # label(e[[224]]) <- ":"
  # label(e[[225]]) <- ":"
  # label(e[[226]]) <- ":"
  # label(e[[227]]) <- ":"
  # label(e[[228]]) <- ":"
  # label(e[[229]]) <- ":"
  # label(e[[230]]) <- ":"
  # label(e[[231]]) <- ":"
  # label(e[[232]]) <- ":"
  # label(e[[233]]) <- ":"
  # label(e[[234]]) <- ":"
  # label(e[[235]]) <- ":"
  # label(e[[236]]) <- ":"
  # label(e[[237]]) <- ":"
  # label(e[[238]]) <- ":"
  # label(e[[239]]) <- ":"
  # label(e[[240]]) <- ":"
  # label(e[[241]]) <- ":"
  # label(e[[242]]) <- ":"
  # label(e[[243]]) <- ":"
  #   
  # for (i in 1:length(e)) names(e)[i] <- sub(':.*', '', label(e[[i]]))
} # old code
  return(e)
}

# e <- prepare(country = "US")
# e <- read_csv("../data/US_pilot.csv") #[-c(1:2),]
# e <- relabel_and_rename(e, country="US", wave = "pilot3")

convert <- function(e, country, wave = NULL) {
  text_pnr <- c( "US" = "Prefer not to say",  "US" = "Don't know, or prefer not to say",  "US" = "Don't know",  "US" = "Don't know or prefer not to say", 
                 "US" = "Don't know, prefer not to say",  "US" = "Don't know, or prefer not to say.",  "US" = "Don't know,  or prefer not to say", "PNR")
  text_yes <- c("US" = "Yes")
  text_no <- c("US" = "No", "US" = "No or I don't have a partner")
  names_policies <- c("standard", "investments", "tax_transfers")
  
  for (i in 1:length(e)) {
    # levels(e[[i]]) <- c(levels(e[[i]]), "PNR")
    e[[i]][e[[i]] %in% text_pnr] <- "PNR"
  }

  variables_duration <<- names(e)[grepl('duration', names(e))]
  if (length(grep('footprint', names(e)))>0) variables_footprint <<- names(e)[grepl('footprint', names(e)) & !grepl('order', names(e))]
  for (i in intersect(c(variables_duration, variables_footprint,  # US pilot: age[22]=NA, km_driven[17]=none => NA by coercion
    "statist", "trust_people", "flights", "km_driven", "hh_adults", "hh_children", "hh_size", "nb_children", "age", "zipcode", "donation"
  ), names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector(e[[i]]))
    label(e[[i]]) <- lab
  }
  for (v in variables_duration) e[[v]] <- e[[v]]/60
  
  if (country=="US" & "km_driven" %in% names(e)) {
    e$miles_driven <- e$km_driven
    e$km_driven <- 1.60934 * e$miles_driven
    label(e$km_driven) <- "km_driven: How many kilometers have you and your household members driven in 2019?" }
  if ("hh_children" %in% names(e)) {
    e$hh_size <- e$hh_adults + e$hh_children
  # e$bad_quality <- 0 # TODO! inattention question 
  # e$bad_quality[e$hh_size > 12] <- 1.3 + e$bad_quality[e$hh_size > 12] # 
  # e$bad_quality[e$hh_children > 10] <- 1 + e$bad_quality[e$hh_children > 10] # 
    e$hh_size <- pmin(e$hh_size, 12)
    label(e$hh_size) <- "hh_size: How many people are in you household?" }
  if ("hh_children" %in% names(e)) e$hh_children <- pmin(e$hh_children, 10)
  if ("hh_adults" %in% names(e)) e$hh_adults <- pmin(e$hh_adults, 5)
  # e$bad_quality[e$km_driven > 10^6] <- 1 + e$bad_quality[e$km_driven > 10^6] # 
  # e$bad_quality[e$flights >= 100] <- 1 + e$bad_quality[e$flights >= 100] # 
  # label(e$bad_quality) <- "bad_quality: Indicator of aberrant answers at hh_size, km_driven or flights."

  # TODO relative_environmentalist
  if (country=="US") yes_no_names <- c("","No","PNR","Yes")
  if (country=="FR") yes_no_names <- c("","Non","PNR","Oui")
  for (j in intersect(c("couple", "CC_real", "CC_dynamic", "change_lifestyle", "pro_global_assembly", "pro_global_tax", "pro_tax_1p", "tax_transfers_trust", "investments_trust",
                        "standard_trust", "tax_transfers_effective", "investments_effective", "standard_effective", "tax_transfers_supports", "investments_supports", # TODO! in pilot1, 2 add an "s" to [policies]_supports & add cap to Beef, incl. in other files
                        "standard_supports", "hit_by_covid", "member_environmental_orga", "relative_environmentalist", "standard_exists"
              ), names(e))) {
    temp <- 1*(e[j][[1]] %in% text_yes) - (e[j][[1]] %in% text_no) - 0.1*(e[j][[1]] %in% text_pnr)
    e[j][[1]] <- as.item(temp, labels = structure(c(-1,-0.1,1), names = c( "No","PNR","Yes")),
                         missing.values = c("",NA,"PNR"), annotation=attr(e[j][[1]], "label"))
    # e[j][[1]] <- as.item(as.character(e[j][[1]]), labels = structure(yes_no_names, names = c("NA","No","PNR","Yes")),
    #             missing.values = c("","PNR"), annotation=attr(e[j][[1]], "label"))
  }

  for (j in intersect(c(#"gender", "region", "speaks_well", "education", "employment_status", "income", "wealth", "frequency_beef", "survey_biased", "vote", "media", "country_should_act_condition
              "heating", "transport_available", "trust_govt", "trust_public_spending", "inequality_problem", "CC_exists", "CC_dynamics", "CC_stoppable", 
              "CC_talks", "CC_worries", "interest_politics"#, "flight_quota_1000km", "flight_quota_1000km_global", "flight_quota_one_trip", "vote_participation"
              # "standard_employment", "investments_employment", "tax_transfers_employment", "standard_side_effects", "investments_side_effects", "tax_transfers_side_effects", 
              # "standard_incidence_poor", "investments_incidence_poor", "tax_transfers_incidence_poor", "standard_incidence_rich", "investments_incidence_rich", "tax_transfers_incidence_rich", 
              # "standard_incidence_middle", "investments_incidence_middle", "tax_transfers_incidence_middle", "standard_incidence_urban", "investments_incidence_urban", "tax_transfers_incidence_urban", 
              # "standard_incidence_rural", "investments_incidence_rural", "tax_transfers_incidence_rural", "standard_incidence_self", "investments_incidence_self", "tax_transfers_incidence_self", 
              # "future_gdp", "envi", "equal_quota", "country_should_act", "insulation_compulsory", "flight_quota_1000km", "flight_quota_3000km", "flight_quota_one_trip", "ban_incentives"
      ), names(e))) {
    e[j][[1]] <- as.item(as.factor(e[j][[1]]), missing.values = c("PNR", "", NA), annotation=paste(attr(e[j][[1]], "label"))) 
  } # TODO! all $likert scales?

  for (j in names(e)) {
    if ((grepl('race_|home_|CC_factor_|CC_responsible_|CC_affected_|change_condition_|effect_policies_|kaya_|scale_|Beef_|far_left|left|center$', j)
        | grepl('right|far_right|liberal|conservative|humanist|patriot|apolitical|^environmentalist|feminist|political_identity_other_choice|GHG_|investments_funding_|obstacles_insulation_', j))
        & !(grepl('_other$|order_|liberal_conservative', j))) {
      temp <- label(e[[j]])
      e[[j]] <- e[[j]]!="" # e[[j]][e[[j]]!=""] <- TRUE
      e[[j]][is.na(e[[j]])] <- FALSE
      label(e[[j]]) <- temp
    }
  }
  variables_race <<- names(e)[grepl('race_', names(e))]
  variables_home <<- names(e)[grepl('home_', names(e))]
  variables_transport <<- names(e)[grepl('transport_', names(e))]
  if (length(grep('CC_factor_', names(e)))>0) variables_CC_factor <<- names(e)[grepl('CC_factor_', names(e))]
  if (length(grep('CC_responsible_', names(e)))>0) variables_CC_responsible <<- names(e)[grepl('CC_responsible_', names(e)) & !grepl("order_", names(e))]
  if (length(grep('responsible_CC_', names(e)))>0) variables_responsible_CC <<- names(e)[grepl('responsible_CC_', names(e)) & !grepl("order_", names(e))]
  if (length(grep('CC_affected_', names(e)))>0) variables_CC_affected <<- names(e)[grepl('CC_affected_', names(e))]
  if (length(grep('change_condition_', names(e)))>0) variables_change_condition <<- names(e)[grepl('change_condition_', names(e))]
  if (length(grep('effect_policies_', names(e)))>0) variables_effect_policies <<- names(e)[grepl('effect_policies_', names(e))]
  if (length(grep('effect_halt_CC_', names(e)))>0) variables_effect_policies <<- names(e)[grepl('effect_halt_CC_', names(e))]
  if (length(grep('kaya_', names(e)))>0) variables_kaya <<- names(e)[grepl('kaya_', names(e))]
  variables_scale <<- names(e)[grepl('scale_', names(e))]
  variables_beef <<- names(e)[grepl('beef_', names(e)) & !grepl("order_", names(e))]
  variables_burden_sharing <<- names(e)[grepl('burden_sharing_', names(e))]
  if ('standard_cost_effective' %in% names(e)) variables_standard_effect <<- names(e)[grepl('standard_', names(e)) & grepl('_effect', names(e))]
  if ('standard_cost_effective' %in% names(e)) variables_investments_effect <<- names(e)[grepl('investments_', names(e)) & grepl('_effect', names(e))]
  if ('standard_cost_effective' %in% names(e)) variables_tax_transfers_effect <<- names(e)[grepl('tax_transfers_', names(e)) & grepl('_effect', names(e))]
  if ('standard_cost_effective' %in% names(e)) variables_policies_effect <<- c(variables_standard_effect, variables_investments_effect, variables_tax_transfers_effect)
  if ("standard_fair" %in% names(e)) variables_policies_fair  <<- names(e)[grepl('_fair', names(e))]
  variables_policies_support <<- c("standard_support", "investments_support", "tax_transfers_support")
  variables_support <<- names(e)[grepl('_support', names(e)) & !grepl('order_', names(e))]
  variables_incidence <<- names(e)[grepl('incidence_', names(e))]
  variables_standard_incidence <<- names(e)[grepl('standard_incidence_', names(e))]
  variables_investments_incidence <<- names(e)[grepl('investments_incidence_', names(e))]
  variables_tax_transfers_incidence <<- names(e)[grepl('tax_transfers_incidence_', names(e))]
  variables_win_lose <<- names(e)[grepl('win_lose_', names(e))]
  variables_standard_win_lose <<- names(e)[grepl('standard_win_lose_', names(e))]
  variables_investments_win_lose <<- names(e)[grepl('investments_win_lose_', names(e))]
  variables_tax_transfers_win_lose <<- names(e)[grepl('tax_transfers_win_lose_', names(e))]
  variables_standard <<- c("standard_support", "standard_trust", "standard_effective", "standard_employment", "standard_side_effects", variables_standard_incidence, variables_standard_win_lose)
  variables_investments <<- c("investments_support", "investments_trust", "investments_effective", "investments_employment", "investments_side_effects", variables_investments_incidence, variables_investments_win_lose)
  variables_tax_transfers <<- c("tax_transfers_support", "tax_transfers_trust", "tax_transfers_effective", "tax_transfers_employment", "tax_transfers_side_effects", variables_tax_transfers_incidence, variables_tax_transfers_win_lose)
  if (length(grep('_side_effects', names(e)))>0) variables_side_effects <<- names(e)[grepl('_side_effects', names(e))]
  if (length(grep('_employment', names(e)))>0) variables_employment <<- names(e)[grepl('_employment', names(e))]
  if (length(grep('willing_', names(e)))>0) variables_willing <<- names(e)[grepl('willing_', names(e))]
  if (length(grep('condition_', names(e)))>0) variables_condition <<- names(e)[grepl('condition_', names(e))]
  if (length(grep('CC_impacts_', names(e)))>0) variables_CC_impacts <<- names(e)[grepl('CC_impacts_', names(e))]
  variables_policy <<- names(e)[grepl('policy_', names(e)) & !grepl("order_", names(e))]
  variables_tax <<- names(e)[grepl('^tax_', names(e)) & !grepl("order_|transfers_|1p", names(e))]
  variables_political_identity <<- c("liberal", "conservative", "humanist", "patriot", "apolitical", "environmentalist", "feminist", "political_identity_other")
  variables_socio_demo <<- c("gender", "age", "region", "race_white", "education", "hit_by_covid", "employment_status", "income", "wealth", "core_metropolitan", "nb_children", "hh_children", "hh_adults", "heating", "km_driven", "flights", "frequency_beef")
  # variables_main_controls <<- c("gender", "age", "income", "education", "hit_by_covid", "employment_status", "Left_right", "(vote == 'Biden')", "as.factor(urbanity)", "core_metropolitan")
  variables_main_controls_pilot12 <<- c("gender", "age", "income", "education", "hit_by_covid", "employment_status", "Left_right", "vote_dum", "as.factor(urbanity)", "core_metropolitan")
  variables_main_controls_pilot3 <<- c("gender", "age_quota", "income", "education", "hit_by_covid", "employment_agg", "liberal_conservative", "vote_dum", "as.factor(urbanity)", "core_metropolitan", "rush")
  variables_pro <<- names(e)[grepl('^pro_', names(e))]
  variables_know_treatment_climate <<- c("know_frequence_heatwaves", "know_temperature_2100")
  variables_know_treatment_policy <<- c("know_standard", "know_investments_jobs")
  variables_know_treatment <<- c(variables_know_treatment_climate, variables_know_treatment_policy)
  if (length(grep('GHG_', names(e)))>0) variables_GHG <<- names(e)[grepl('GHG_', names(e))]
  if (length(grep('investments_funding_', names(e)))>0) variables_investments_funding <<- names(e)[grepl('investments_funding_', names(e))]
  if (length(grep('if_other_do_', names(e)))>0) variables_if_other_do <<- names(e)[grepl('if_other_do_', names(e))]
  if (length(grep('obstacles_insulation_', names(e)))>0) variables_obstacles_insulation <<- names(e)[grepl('obstacles_insulation_', names(e)) & !grepl('other$', names(e))]
  if (length(grep('footprint', names(e)))>0) {
    Variables_footprint <<- Labels_footprint <<- list()
    for (v in c("el", "fd", "tr", "reg")) {
      Variables_footprint[[v]] <<- names(e)[grepl(paste("footprint_", v, "_", sep=""), names(e)) & !grepl("order", names(e))]
      Labels_footprint[[v]] <<- capitalize(sub(paste("footprint_", v, "_", sep=""), "", Variables_footprint[[v]]))
    }
  }

  text_strongly_agree <- c( "US" = "Strongly agree",  "US" = "I fully agree")
  text_somewhat_agree <- c( "US" = "Somewhat agree",  "US" = "I somewhat agree")
  text_neutral <- c( "US" = "Neither agree or disagree",  "US" = "Neither agree nor disagree",  "US" = "I neither agree nor disagree")
  text_somewhat_disagree <- c( "US" = "Somewhat disagree",  "US" = "I somewhat disagree")
  text_strongly_disagree <- c("US" = "Strongly disagree", "US" = "Fully disagree")
  #  variables_incidence variables_burden_sharing
  
  text_support_strongly <- c("US" = "Yes, absolutely", "US" = "Strongly support") # first: policy / second: tax
  text_support_somewhat <- c("US" = "Yes, somewhat", "US" = "Rather support", "US" = "Somewhat support")
  text_support_indifferent <- c("US" = "Indifferent", "US" = "Neither support nor oppose")
  text_support_not_really <- c("US" = "No, not really", "US" = "Rather oppose", "US" = "Somewhat oppose")
  text_support_not_at_all <- c("US" = "No, not at all", "US" = "Strongly oppose")
  
  text_excellent <- c("US" = "Excellent")
  text_good <- c("US" = "Good")
  text_fair <- c("US" = "Fair")
  text_poor <- c("US" = "Poor")
  text_very_poor <- c("US" = "Very poor")
  
  text_rural <- c("US" = "A rural area")
  text_small_town <- c("US" = "A small town (between 5,000 and 20,000 inhabitants)", "US" = "A small town (5,000 – 20,000 inhabitants)")
  text_large_town <- c("US" = "A large town (between 20,000 and 50,000 inhabitants)", "US" = "A large town (20,000 – 50,000 inhabitants)")
  text_small_city <- c("US" = "A small city (between 50,000 and 250,000 inhabitants)", "US" = "A small city (50,000 – 250,000 inhabitants)")
  text_medium_city <- c("US" = " A medium-size city (between 250,000 and 3,000,000 inhabitants)", "US" = "A medium-sized city (250,000 – 3,000,000 inhabitants)")
  text_large_city <- c("US" = "A large city (more than 3 million inhabitants)")
  
  text_speaks_native <- c("US" = "Native")
  text_speaks_well <- c("US" = "Well or very well")
  text_speaks_somewhat <- c("US" = "Somewhat well")
  text_speaks_no <- c("US" = "I cannot speak English")
  
  text_education_no <- c("US" = "No schooling completed")
  text_education_primary <- c("US" = "Primary school")
  text_education_secondary <- c("US" = "Lower secondary school")
  text_education_vocational <- c("US" = "Vocational degree")
  text_education_high <- c("US" = "High school")
  text_education_college <- c("US" = "College degree")
  text_education_master <- c("US" = "Master's degree or above")
  
  text_income_q1 <- c("US" = "less than $35,000")
  text_income_q2 <- c("US" = "between $35,000 and $70,000")
  text_income_q3 <- c("US" = "between $70,000 and $120,000")
  text_income_q4 <- c("US" = "more than $120,000")
  
  text_wealth_q1 <- c("US" = "Less than $0 (I have a net debt)")
  text_wealth_q2 <- c("US" = "Close to $0")
  text_wealth_q3 <- c("US" = "Between $4,000 and $120,000")
  text_wealth_q4 <- c("US" = "Between $120,000 and $380,000")
  text_wealth_q5 <- c("US" = "More than $380,000")
  
  text_frequency_beef_daily <- c("US" = "Almost or at least daily")
  text_frequency_beef_weekly <- c("US" = "One to four times per week")
  text_frequency_beef_rarely <- c("US" = "Less than once a week")
  text_frequency_beef_never <- c("US" = "Never")
  
  text_transport_available_yes_easily <- c("US" = "Yes, public transport is easily and frequently available")
  text_transport_available_yes_limited <- c("US" = "Yes, public transport is available but with limitations")
  text_transport_available_not_so_much <- c("US" = "Not so much, public transport is available but with many limitations")
  text_transport_available_not_at_all <- c("US" = "No, there is no public transport")
  
  text_none <- c("US" = "None")
  text_a_little <- c("US" = "A little")
  text_some <- c("US" = "Some")
  text_a_lot <- c("US" = "A lot")
  text_most <- c("US" = "Most")
  
  text_intensity_not <- c("US" = "Not at all")
  text_intensity_little <- c("US" = "A little")
  text_intensity_some <- c("US" = "Moderately")
  text_intensity_lot <- c("US" = "A lot")
  text_intensity_great_deal <- c("US" = "A great deal")
  
  text_very_unlikely <- c("US" = "Very unlikely")
  text_somewhat_unlikely <- c("US" = "Somewhat unlikely")
  text_somewhat_likely <- c("US" = "Somewhat likely")
  text_very_likely <- c("US" = "Very likely")
  
  text_very_negative_effects <- c("US" = "Very negative effects")
  text_negative_effects <- c("US" = "Somewhat negative effects")
  text_no_effects <- c("US" = "No noticeable effects")
  text_positive_effects <- c("US" = "Somewhat positive effects")
  text_very_positive_effects <- c("US" = "Very positive effects")
  
  text_trust_govt_always <- c("US" = "Nearly all the time")
  text_trust_govt_often <- c("US" = "Most of the time")
  text_trust_govt_sometimes <- c("US" = "Only some of the time")
  text_trust_govt_never <- c("US" = "Never")
  
  text_inequality_not <- c("US" = "Not a problem at all")
  text_inequality_small <- c("US" = "A small problem")
  text_inequality_problem <- c("US" = "A problem")
  text_inequality_serious <- c("US" = "A serious problem")
  text_inequality_very_serious <- c("US" = "A very serious problem")
  
  text_future_richer <- c("US" = "Richer, for example thanks to technological progress")
  text_future_poorer <- c("US" = "Poorer, for example due to resource depletion and/or climate change")
  text_future_as_rich <- c("US" = "About as rich as now on average")
  
  text_much_richer <- c("US" = "Much richer")
  text_richer <- c("US" = "Richer")
  text_as_rich <- c("US" = "As rich as now")
  text_poorer <- c("US" = "Poorer")
  text_much_poorer <- c("US" = "Much poorer")
  
  text_envi_pro_envi <- c("US" = "We should make our society as sustainable as possible to avoid irreversible damages")
  text_envi_anti_envi <- c("US" = "I believe we have more important goals than sustainability")
  text_envi_progress <- c("US" = "Our civilization will develop so much that environmental issues will not be a problem in the distant future")
  text_envi_collapse <- c("US" = "Our civilization will eventually collapse, it is useless to try making society more sustainable")
  
  text_CC_exists_not <- c("US" = "is not a reality")
  text_CC_exists_natural <- c("US" = "is mainly due to natural climate variability")
  text_CC_exists_human <- c("US" = "is mainly due to human activity")
  
  text_CC_dynamics_rise <- c("US" = "temperatures will continue to rise, just more slowly")
  text_CC_dynamics_stabilize <- c("US" = "temperatures will stabilize")
  text_CC_dynamics_decrease <- c("US" = "temperatures will decrease")
  text_CC_dynamics_none <- c("US" = "none of the above: greenhouse gas emissions have no impact on temperatures")
  
  text_CC_stoppable_no_influence <- c("US" = "Humans have no noticeable influence on the climate.")
  text_CC_stoppable_adapt <- c("US" = "We'd better live with climate change rather than try to halt it. Stopping emissions would cause more harm than climate change itself.", "US" = "We’d better live with climate change rather than try to halt it. Stopping emissions would cause more harm than climate change itself.")
  text_CC_stoppable_pessimistic <- c("US" = "We should stop emissions, but unfortunately this is not going to happen.")
  text_CC_stoppable_policies <- c("US" = "Ambitious policies and raising awareness will eventually succeed in stopping emissions within the next century.")
  text_CC_stoppable_optimistic <- c("US" = "Technologies and habits are changing and this will suffice to prevent disastrous climate change. We do not need ambitious policies.")
  
  text_CC_talks_yearly <- c("US" = "Several times a year")
  text_CC_talks_monthly <- c("US" = "Several times a month")
  text_CC_talks_never <- c("US" = "Almost never")
  
  text_CC_impacts_insignificant <- c("US" = "Insignificant, or even beneficial")
  text_CC_impacts_small <- c("US" = "Small, because humans would be able to live with it")
  text_CC_impacts_grave <- c("US" = "Grave, because there would be more natural disasters")
  text_CC_impacts_disastrous <- c("US" = "Disastrous, lifestyles would be largely altered")
  text_CC_impacts_cataclysmic <- c("US" = "Cataclysmic, humankind would disappear")
  
  text_equal_quota_yes <- c("US" = "Yes, this would be a fair solution")
  text_equal_quota_no_grand_fathering <- c("US" = "No, those who currently pollute more should have more rights to pollute")
  text_equal_quota_no_redistribution <- c("US" = "No, the poor or those who will be hurt more by climate change should be compensated more")
  text_equal_quota_no_scale <- c("US" = "No, rights to pollute should not be defined at the individual level but at another level, for example at the country level")
  text_equal_quota_no_restriction <- c("US" = "No, we should not restrict greenhouse gas emissions")
  
  text_should_act_yes <- c("US" = "Yes")
  text_should_act_depends <- c("US" = "It depends: only if it is part of a fair international agreement")
  text_should_act_no <- c("US" = "No, by no means")
  
  text_should_act_condition_compensation <- c("US" = "The US should take even more ambitious measures if other countries are less ambitious")
  text_should_act_condition_reciprocity <- c("US" = "The US should take even more ambitious measures if other countries also take similar measures")
  text_should_act_condition_free_riding <- c("US" = "The US should be less ambitious if other countries take ambitious measures")
  
  # text_employment_positive <- c("US" = "Positive impacts")
  # text_employment_no_impact <- c("US" = "No notable impact")
  # text_employment_negative <- c("US" = "Negative impacts")
  
  text_effects_positive <- c("US" = "Positive impacts", "US" = "Positive side effects")
  text_effects_no_impact <- c("US" = "No notable impact", "US" = "No notable side effects")
  text_effects_negative <- c("US" = "Negative impacts", "US" = "Negative side effects")
  
  text_incidence_win <- c("US" = "Would win", "US" = "Win")
  text_incidence_lose <- c("US" = "Would lose", "US" = "Would be lose", "US" = "Lose")
  text_incidence_unaffected <- c("US" = "Would not be severely affected", "US" = "Neither win nor lose", "US" = "Be unaffected")
  
  text_win_a_lot <- c("US" = "Win a lot")
  text_mostly_win <- c("US" = "Mostly win")
  text_mostly_lose <- c("US" = "Mostly lose")
  text_lose_a_lot <- c("US" = "Lose a lot")
  text_unaffected <- c("US" = "Neither win nor lose")
  
  text_much_more <- c("US" = "Much more")
  text_more <- c("US" = "More")
  text_same <- c("US" = "About the same")
  text_less <- c("US" = "Less")
  text_much_less <- c("US" = "Much less")
  
  text_govt_do_too_much <- c("US" = "Government is doing too much")
  text_govt_doing_right <- c("US" = "Government is doing just the right amount")
  text_govt_should_do_more <- c("US" = "Government should do more")
  
  text_issue_not <- c("US" = "Not an issue at all")
  text_issue_small <- c("US" = "A small issue")
  text_issue_issue <- c("US" = "An issue")
  text_issue_serious <- c("US" = "A serious issue")
  text_issue_very_serious <- c("US" = "A very serious issue")
  
  text_CC_worries_very <- c("US" = "Very worried")
  text_CC_worries_worried <- c("US" = "Worried")
  text_CC_worries_not <- c("US" = "Not worried")
  text_CC_worries_not_at_all <- c("US" = "Not worried at all")
  
  text_insulation_mandatory <- c("US" = "Mandatory: every building should be renovated before a certain date")
  text_insulation_voluntary <- c("US" = "Voluntary: an owner should be able to not renovate their house")
  
  # first: 1000km / second: 3000km / third: one_trip
  text_flight_quota_rationing <- c("US" = "No one would be allowed to fly more than 12,000 miles between now and 2040.", "US" = "No one would be allowed to fly more than 40,000 miles between now and 2040.",
                                      "US" = "No one would be allowed to fly more than one round-trip every two years.")
  text_flight_quota_tradable <- c("US" = "Those who plan to not fly within a given year would be allowed to sell their “right to fly” to someone who wants to fly but has already reached their quota of 12,000 miles.",
                                  "US" = "Those who plan to not fly within a given year would be allowed to sell their “right to fly” to someone who wants to fly but has already reached their quota of 40,000 miles.",
                                  "US" = "Those who plan to not fly within a two-year period would be allowed to sell their “right to fly” to someone who wants to fly more than once during these two years.")
  
  text_ban_incentives_force <- c("US" = "Governments should force people to protect the environment, even if it prevents people from doing what they want")
  text_ban_incentives_encourage <- c("US" = "Governments should only encourage people to protect the environment, even if it means people do not always do the right thing")
  
  text_interest_politics_no <- c("US" = "Not really or not at all")
  text_interest_politics_little <- c("US" = "A little")
  text_interest_politics_lot <- c("US" = "A lot")
  
  text_very_liberal <- c("US" = "Very liberal")
  text_liberal <- c("US" = "Liberal")
  text_moderate <- c("US" = "Moderate")
  text_conservative <- c("US" = "Conservative")
  text_very_conservative <- c("US" = "Very conservative")
  
  text_media_TV_public <- c("US" = "TV (mostly public broadcasting channels)")
  text_media_TV_private <- c("US" = "TV (mostly private channels)")
  text_media_radio <- c("US" = "Radio")
  text_media_social <- c("US" = "Social media (e.g., Facebook, Twitter, etc.)")
  text_media_print <- c("US" = "Print media (e.g., print newspapers, magazines etc.)")
  text_media_web <- c("US" = "News websites (e.g. online newspapers)")
  text_media_other <- c("US" = "Other")
  
  text_vote_participation_no_right <- c("US" = "I don't have the right to vote in the US",
                                        "US" = "I didn't have the right to vote in the US",
                                        "US" = "I don't have the right to vote in the U.S.",
                                        "US" = "I didn't have the right to vote in the U.S.")
  
  text_survey_biased_no <- c("US" = "No, I do not feel it was biased")
  text_survey_biased_pro_envi <- c("US" = "Yes, biased towards environmental causes")
  text_survey_biased_anti_envi <- c("US" = "Yes, biased against the environment")
  text_survey_biased_left <- c("US" = "Yes, left-wing biased")
  text_survey_biased_right <- c("US" = "Yes, right-wing biased")
  
  text_independent <- c("US" = "Manager or independent (e.g. manager, executive, health or independent professional, teacher, lawyer, architect, researcher, artist...)")
  text_clerc <- c("US" = "Clerical support or services (e.g. caring, sales, leisure, administrative...)")
  text_skilled <- c("US" = "Skilled work (e.g. craft worker, plants and machine operator, farmer...)")
  text_manual <- c("US" = "Manual operations (e.g. cleaning, agriculture, delivery, transport, military...)")
  text_none_above <- c("US" = "None of the above")
  
  for (v in intersect(names(e), c(variables_burden_sharing, variables_policies_effect, variables_policies_fair, "should_fight_CC", "can_trust_people", "can_trust_govt", "trust_public_spending", "CC_problem"))) { 
    temp <-  2 * (e[[v]] %in% text_strongly_agree) + (e[[v]] %in% text_somewhat_agree) - (e[[v]] %in% text_somewhat_disagree) - 2 * (e[[v]] %in% text_strongly_disagree) - 0.1 * (e[[v]] %in% text_pnr | is.na(e[[v]]))
    e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1),
                          names = c("Strongly disagree","Somewhat disagree","Neither agree or disagree","Somewhat agree","Strongly agree","PNR")),
                        missing.values=-0.1, annotation=Label(e[[v]]))
  }
  
  for (v in intersect(names(e), c(variables_CC_impacts, "will_insulate", "CC_will_end"))) { 
    temp <-  2 * (e[[v]] %in% text_very_likely) + (e[[v]] %in% text_somewhat_likely) - (e[[v]] %in% text_somewhat_unlikely) - 2 * (e[[v]] %in% text_very_unlikely) - 0.1 * (e[[v]] %in% text_pnr | is.na(e[[v]])) # TODO accommodate NA everywhere?
    e[[v]] <- as.item(temp, labels = structure(c(-2,-1,1,2,-0.1),
                                               names = c("Very unlikely","Somewhat unlikely","Somewhat likely","Very likely","PNR")),
                      missing.values=-0.1, annotation=Label(e[[v]]))
  }
  
  for (v in intersect(names(e), c(variables_responsible_CC, variables_willing, variables_condition, "CC_knowledgeable", "net_zero_feasible", "CC_affects_self", "pro_ambitious_policies", "effect_halt_CC_lifestyle", "interested_politics"))) { 
    temp <-  2 * (e[[v]] %in% text_intensity_great_deal) + (e[[v]] %in% text_intensity_lot) - (e[[v]] %in% text_intensity_little) - 2 * (e[[v]] %in% text_intensity_not) - 0.1 * (e[[v]] %in% text_pnr | is.na(e[[v]])) # TODO accommodate NA everywhere?
    e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                               names = c("Not at all","A little","Moderately","A lot","A great deal","PNR")),
                      missing.values=-0.1, annotation=Label(e[[v]]))
  }

  for (v in c(variables_policy , variables_tax, variables_support)) { # TODO! compatibility pilots 1, 2
    temp <-  2 * (e[[v]] %in% text_support_strongly) + (e[[v]] %in% text_support_somewhat) - (e[[v]] %in% text_support_not_really) - 2 * (e[[v]] %in% text_support_not_at_all) - 0.1 * (e[[v]] %in% text_pnr | is.na(e[[v]]))
    e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1),
                          names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support","PNR")),
                        missing.values=-0.1, annotation=Label(e[[v]]))
  }
  
  temp <-  (e$urbanity %in% text_small_town) + 2 * (e$urbanity %in% text_large_town) + 3 * (e$urbanity %in% text_small_city) + 4 * (e$urbanity %in% text_medium_city) + 5 * (e$urbanity %in% text_large_city)
  e$urbanity <- as.item(temp, labels = structure(c(0:5),
                        # names = c("Rural","Small town","Large town","Small city","Medium-size city","Large city")),
                        names = c("Rural","5-20k","20-50k","50-250k","250k-3M",">3M")),
                      annotation=Label(e$urbanity))
  
  if ("speaks_well" %in% names(e)) temp <-  (e$speaks_well %in% text_speaks_well) + 2 * (e$speaks_well %in% text_speaks_native) - 1 * (e$speaks_well %in% text_speaks_no) 
  if ("speaks_well" %in% names(e)) e$speaks_well <- as.item(temp, labels = structure(c(-1:2),
                        names = c("Cannot speak","Somewhat well","Well or very well","Native")),
                      annotation=Label(e$speaks_well))
  
  temp <-  (e$education %in% text_education_primary) + 2 * (e$education %in% text_education_secondary) + 3 * (e$education %in% text_education_vocational) + 4 * (e$education %in% text_education_high) + 5 * (e$education %in% text_education_college) + 6 * (e$education %in% text_education_master)
  e$education <- as.item(temp, labels = structure(c(0:6),
                        names = c("None", "Primary", "Lower secondary", "Vocational", "High school", "College degree", "Master degree")),
                      annotation=Label(e$education))
  
  temp <-  (e$income %in% text_income_q1) + 2 * (e$income %in% text_income_q2) + 3 * (e$income %in% text_income_q3) + 4 * (e$income %in% text_income_q4) 
  e$income <- as.item(temp, labels = structure(c(1:4),
                        names = c("Q1","Q2","Q3","Q4")),
                      annotation=Label(e$income))
  
  temp <-  (e$wealth %in% text_wealth_q1) + 2 * (e$wealth %in% text_wealth_q2) + 3 * (e$wealth %in% text_wealth_q3) + 4 * (e$wealth %in% text_wealth_q4) + 5 * (e$wealth %in% text_wealth_q5) 
  e$wealth <- as.item(temp, labels = structure(c(1:5),
                        names = c("Q1","Q2","Q3","Q4","Q5")),
                      annotation=Label(e$wealth))

  temp <-  (e$frequency_beef %in% text_frequency_beef_rarely) + 2 * (e$frequency_beef %in% text_frequency_beef_weekly) + 3 * (e$frequency_beef %in% text_frequency_beef_daily) 
  e$frequency_beef <- as.item(temp, labels = structure(c(0:3),
                        names = c("Never", "Rarely", "Weekly", "Daily")),
                      annotation=Label(e$frequency_beef))
  
  if ("insulation" %in% names(e)) temp <- 2 * (e$insulation %in% text_excellent) + (e$insulation %in% text_good) - (e$insulation %in% text_poor) - 2 * (e$insulation %in% text_very_poor) - 0.1 * (e$insulation %in% text_pnr | is.na(e$insulation))
  if ("insulation" %in% names(e)) e$insulation <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                                       names = c("Very poor", "Poor", "Fair", "Good", "Excellent", "PNR")),
                              annotation=Label(e$insulation))
  
  if ("CC_anthropogenic" %in% names(e)) temp <- 2 * (e$CC_anthropogenic %in% text_most) + (e$CC_anthropogenic %in% text_a_lot) - (e$CC_anthropogenic %in% text_a_little) - 2 * (e$CC_anthropogenic %in% text_none | e$CC_real == 'No') - 0.1 * ((e$CC_anthropogenic %in% text_pnr | is.na(e$CC_anthropogenic)) & e$CC_real == 'Yes')
  if ("CC_anthropogenic" %in% names(e)) e$CC_anthropogenic <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                                                                   names = c("None", "A little", "Some", "A lot", "Most", "PNR")),
                                                          annotation=Label(e$CC_anthropogenic))
  
  
  if ("effect_halt_CC_economy" %in% names(e)) temp <- 2 * (e$effect_halt_CC_economy %in% text_very_positive_effects) + (e$effect_halt_CC_economy %in% text_positive_effects) - (e$effect_halt_CC_economy %in% text_negative_effects) - 2 * (e$effect_halt_CC_economy %in% text_very_negative_effects) - 0.1 * (e$effect_halt_CC_economy %in% text_pnr | is.na(e$effect_halt_CC_economy))
  if ("effect_halt_CC_economy" %in% names(e)) e$effect_halt_CC_economy <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                                                                               names = c("Very negative", "Negative", "None", "Positive", "Very positive", "PNR")),
                                                                      annotation=Label(e$effect_halt_CC_economy))
  
  
  for (v in intersect(names(e), c("if_other_do_more", "if_other_do_less"))) {
  temp <- 2 * (e[[v]] %in% text_much_more) + (e[[v]] %in% text_more) - (e[[v]] %in% text_less) - 2 * (e[[v]] %in% text_much_less) - 0.1 * (e[[v]] %in% text_pnr | is.na(e[[v]]))
  e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1), names = c("Much less", "Less", "About the same", "More", "Much more", "PNR")),
                                                                                  annotation=Label(e[[v]])) }
  
  if ("view_govt" %in% names(e)) temp <- (e$view_govt %in% text_govt_should_do_more) - (e$view_govt %in% text_govt_do_too_much) - 0.1 * (e$view_govt %in% text_pnr | is.na(e$view_govt))
  if ("view_govt" %in% names(e)) e$view_govt <- as.item(temp, labels = structure(c(-1:1,-0.1), names = c("Does too much", "Doing right amount", "Should do more", "PNR")),
                                                                                  annotation=Label(e$view_govt))
  
  
  if ("problem_inequality" %in% names(e)) temp <- 2 * (e$problem_inequality %in% text_issue_very_serious) + (e$problem_inequality %in% text_issue_serious) - (e$problem_inequality %in% text_issue_small) - 2 * (e$problem_inequality %in% text_issue_not) - 0.1 * (e$problem_inequality %in% text_pnr | is.na(e$problem_inequality))
  if ("problem_inequality" %in% names(e)) e$problem_inequality <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                                                                                           names = c("Not an issue at all", "A small issue", "An issue", "A serious issue", "A very serious issue", "PNR")),
                                                                                  annotation=Label(e$problem_inequality))
  
  if ("future_richness" %in% names(e)) temp <- 2 * (e$future_richness %in% text_much_richer) + (e$future_richness %in% text_richer) - (e$future_richness %in% text_poorer) - 2 * (e$future_richness %in% text_much_poorer) - 0.1 * (e$future_richness %in% text_pnr | is.na(e$future_richness))
  if ("future_richness" %in% names(e)) e$future_richness <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                                                                                   names = c("Much poorer", "Poorer", "As rich as now", "Richer", "Much richer", "PNR")),
                                                                          annotation=Label(e$future_richness))
  
  if ("liberal_conservative" %in% names(e)) temp <- -2 * (e$liberal_conservative %in% text_very_liberal) - (e$liberal_conservative %in% text_liberal) + (e$liberal_conservative %in% text_conservative) + 2 * (e$liberal_conservative %in% text_very_conservative) - 0.1 * (e$liberal_conservative %in% text_pnr | is.na(e$liberal_conservative))
  if ("liberal_conservative" %in% names(e)) e$liberal_conservative <- as.item(temp, labels = structure(c(-2:2,-0.1),
                              names = c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative", "PNR")),
                              missing.values=-0.1, annotation=Label(e$liberal_conservative))
  
  if ("transport_available" %in% names(e)) temp <-  (e$transport_available %in% text_transport_available_yes_limited) + 2 * (e$transport_available %in% text_transport_available_yes_easily) - (e$transport_available %in% text_transport_available_not_at_all) - 0.1*(e$transport_available %in% text_pnr)
  if ("transport_available" %in% names(e)) e$transport_available <- as.item(temp, labels = structure(c(-1:2,-0.1),
                        names = c("Not at all", "Not so much", "Yes but limited", "Yes, easily", "PNR")),
                      missing.values=-0.1, annotation=Label(e$transport_available)) # TODO: waves
  
  if ("trust_govt" %in% names(e)) temp <-  (e$trust_govt %in% text_trust_govt_sometimes) + 2 * (e$trust_govt %in% text_trust_govt_often) + 3 * (e$trust_govt %in% text_trust_govt_always) - 0.1*(e$trust_govt %in% text_pnr)
  if ("trust_govt" %in% names(e)) e$trust_govt <- as.item(temp, labels = structure(c(0:3,-0.1),
                        names = c("Never","Only some of the time","Most of the time","Nearly all the time","PNR")),
                      missing.values=-0.1, annotation=Label(e$trust_govt))
  
  if ("inequality_problem" %in% names(e)) temp <-  2 * (e$inequality_problem %in% text_inequality_very_serious) + (e$inequality_problem %in% text_inequality_serious) - (e$inequality_problem %in% text_inequality_small) - 2 * (e$inequality_problem %in% text_inequality_not) - 0.1 * (e$inequality_problem %in% text_pnr)
  if ("inequality_problem" %in% names(e)) e$inequality_problem <- as.item(temp, labels = structure(c(-2:2,-0.1),
                        names = c("Very serious problem","Serious problem","A problem","Small problem","Not a problem at all","PNR")),
                      missing.values=-0.1, annotation=Label(e$inequality_problem)) # TODO: waves

  if ("future_gdp" %in% names(e)) temp <-  (e$future_gdp %in% text_future_richer) - (e$future_gdp %in% text_future_poorer) - 0.1 * (e$future_gdp %in% text_pnr)
  if ("future_gdp" %in% names(e)) e$future_gdp <- as.item(temp, labels = structure(c(-1:1,-0.1),
                        names = c("Poorer","About as rich", "Richer","PNR")),
                      missing.values=-0.1, annotation=Label(e$future_gdp))
  
  if ("envi" %in% names(e)) {
    e$envi[e$envi %in% text_envi_pro_envi] <- "Pro environmental action"
    e$envi[e$envi %in% text_envi_collapse] <- "Useless: collapse"
    e$envi[e$envi %in% text_envi_anti_envi] <- "Other goals"
    e$envi[e$envi %in% text_envi_progress] <- "Not a pb: progress"
    e$envi <- as.item(as.factor(e$envi), missing.values = c("PNR", "", NA), annotation=paste(attr(e$envi, "label")))
  }
    
  if ("CC_exists" %in% names(e)) temp <-  (e$CC_exists %in% text_CC_exists_human) - (e$CC_exists %in% text_CC_exists_not) - 0.1 * (e$CC_exists %in% text_pnr)
  if ("CC_exists" %in% names(e)) e$CC_exists <- as.item(temp, labels = structure(c(-1:1,-0.1),
                        names = c("Not a reality","Natural", "Anthropogenic","PNR")),
                      missing.values=-0.1, annotation=Label(e$CC_exists))
    
  if ("CC_dynamics" %in% names(e)) temp <-  (e$CC_dynamics %in% text_CC_dynamics_rise) - (e$CC_dynamics %in% text_CC_dynamics_decrease) - 2 * (e$CC_dynamics %in% text_CC_dynamics_none) - 0.1 * (e$CC_dynamics %in% text_pnr)
  if ("CC_dynamics" %in% names(e)) e$CC_dynamics <- as.item(temp, labels = structure(c(-2:1,-0.1),
                        names = c("No impact","Decrease", "Stabilize","Rise more slowly","PNR")),
                      missing.values=-0.1, annotation=Label(e$CC_dynamics))

  if ("CC_stoppable" %in% names(e)) {
    e$CC_stoppable <- as.character(e$CC_stoppable)
    e$CC_stoppable[e$CC_stoppable %in% text_CC_stoppable_no_influence] <- "No influence"
    e$CC_stoppable[e$CC_stoppable %in% text_CC_stoppable_adapt] <- "Better to adapt"
    e$CC_stoppable[e$CC_stoppable %in% text_CC_stoppable_optimistic] <- "Progress will suffice"
    e$CC_stoppable[e$CC_stoppable %in% text_CC_stoppable_policies] <- "Policies & awareness will"
    e$CC_stoppable[e$CC_stoppable %in% text_CC_stoppable_pessimistic] <- "Should but not happening"
    e$CC_stoppable <- relevel(relevel(relevel(relevel(relevel(as.factor(e$CC_stoppable), "No influence"), "Better to adapt"), "Should but not happening"), "Policies & awareness will"), "Progress will suffice") # TODO! uncomment
  }
  
  temp <-  (e$CC_talks %in% text_CC_talks_monthly) - (e$CC_talks %in% text_CC_talks_never) - 0.1 * (e$CC_talks %in% text_pnr)
  e$CC_talks <- as.item(temp, labels = structure(c(-1:1,-0.1),
                        names = c("Never","Yearly","Monthly","PNR")), 
                        missing.values=-0.1, annotation=Label(e$CC_talks))
  
  if ("equal_quota" %in% names(e)) temp <-  2 * (e$equal_quota %in% text_equal_quota_no_redistribution) + (e$equal_quota %in% text_equal_quota_yes) - (e$equal_quota %in% text_equal_quota_no_grand_fathering) - 2 * (e$equal_quota %in% text_equal_quota_no_restriction) - 0.1 * (e$equal_quota %in% text_pnr)
  if ("equal_quota" %in% names(e)) e$equal_quota <- as.item(temp, labels = structure(c(-2:2,-0.1),
                        names = c("No, against restriction","No, grand-fathering","No, not individual level","Yes","No, more to vulnerable","PNR")),
                      missing.values=-0.1, annotation=Label(e$equal_quota))
  
  if (country=="US" & wave=="pilot2") e$equal_quota2 <- as.item(e$equal_quota, labels = structure(c(-2,-1,1,2,-0.1),
                                                                                         names = c("No, against restriction","No, grand-fathering","Yes","No, more to vulnerable","PNR")),
                                                                missing.values=-0.1, annotation=Label(e$equal_quota))
    
  if ("country_should_act" %in% names(e)) temp <-  (e$country_should_act %in% text_should_act_yes) - (e$country_should_act %in% text_should_act_no) - 0.1 * (e$country_should_act %in% text_pnr)
  if ("country_should_act" %in% names(e)) e$country_should_act <- as.item(temp, labels = structure(c(-1:1,-0.1),
                        names = c("No","Only if international agreement", "Yes","PNR")),
                      missing.values=-0.1, annotation=Label(e$country_should_act))
  
  # e$country_should_act_condition[e$country_should_act_condition %in% text_should_act_condition_compensation] <- 1
  # e$country_should_act_condition[e$country_should_act_condition %in% text_should_act_condition_reciprocity] <- 0
  # e$country_should_act_condition[e$country_should_act_condition %in% text_should_act_condition_free_riding] <- -1
  if ("country_should_act_condition" %in% names(e)) temp <- (e$country_should_act_condition %in% text_should_act_condition_compensation) - (e$country_should_act_condition %in% text_should_act_condition_free_riding) - 0.1 * (e$country_should_act_condition %in% text_pnr)
  if ("country_should_act_condition" %in% names(e)) e$country_should_act_condition <- as.item(temp, labels = structure(c(-1:1), # No PNR option here, but question asked only if Yes to country_should_act
                                                                     names = c("Free-riding","Reciprocity", "Compensation")),
                                             annotation=Label(e$country_should_act))
  
  for (v in intersect(names(e), c(variables_side_effects, variables_employment))) { # TODO bug when variables_side_effects is not yet defined
    temp <-  (e[[v]] %in% text_effects_positive) - (e[[v]] %in% text_effects_negative) - 0.1 * (e[[v]] %in% text_pnr)
    e[[v]] <- as.item(temp, labels = structure(c(-1:1,-0.1),
                          names = c("Negative","None notable","Positive","PNR")),
                        missing.values=-0.1, annotation=Label(e[[v]]))
  }  

  for (v in intersect(names(e), c(variables_incidence))) { # TODO! why 0 missing?
    temp <-  (e[[v]] %in% text_incidence_win) - (e[[v]] %in% text_incidence_lose) - 0.1 * (e[[v]] %in% text_pnr)
    e[[v]] <- as.item(temp, labels = structure(c(-1:1,-0.1),
                          names = c("Lose","Unaffected","Win","PNR")),
                        missing.values=-0.1, annotation=Label(e[[v]]))
  }
  
  for (v in intersect(names(e), c(variables_win_lose))) {
    temp <-  -2*(e[[v]] %in% text_lose_a_lot) - (e[[v]] %in% text_mostly_lose) + (e[[v]] %in% text_mostly_win) + 2*(e[[v]] %in% text_win_a_lot) - 0.1 * is.na(e[[v]])
    e[[v]] <- as.item(temp, labels = structure(c(-2:2,-0.1),
                                               names = c("Lose a lot", "Mostly lose", "Neither win nor lose","Mostly win", "Win a lot","PNR")),
                      missing.values=-0.1, annotation=Label(e[[v]]))
  }
    
  if ("CC_worries" %in% names(e)) temp <-  (e$CC_worries %in% text_CC_worries_very) - (e$CC_worries %in% text_CC_worries_not) - 2 * (e$CC_worries %in% text_CC_worries_not_at_all) - 0.1 * (e$CC_worries %in% text_pnr)
  if ("CC_worries" %in% names(e)) e$CC_worries <- as.item(temp, labels = structure(c(-2:1,-0.1),
                        names = c("No worried at all","Not worried", "Worried","Very worried","PNR")),
                      missing.values=-0.1, annotation=Label(e$CC_worries))
  
  if ("occupation" %in% names(e)) temp <-  (e$occupation %in% text_clerc) - 2*(e$occupation %in% text_none_above) - 1 * (e$occupation %in% text_manual) + 2 * (e$occupation %in% text_independent) - 0.1*(is.na(e$occupation))
  if ("occupation" %in% names(e)) e$occupation <- as.item(temp, labels = structure(c(-2:2,-0.1), names = c("Other","Manual","Skilled", "Clerc","Independent","PNR")),
                                                          missing.values=-0.1, annotation=Label(e$occupation))
  
  if ("heating_expenses" %in% names(e)) temp <- 10*(e$heating_expenses == "Less than $20") + 50*(e$heating_expenses == "$20 – $75") + 100*(e$heating_expenses == "$76 – $125") + 167*(e$heating_expenses == "$126 – $200") + 225*(e$heating_expenses == "$201 – $250") + 275*(e$heating_expenses == "$251 – $300") + 
          350*(e$heating_expenses == "More than $300")  - 0.1*(e$heating_expenses == "I am not in charge of paying for heating; utilities are included in my rent") - 0.1*is.na(e$heating_expenses) #
  if ("heating_expenses" %in% names(e)) e$heating_expenses <- as.item(temp, labels = structure(c(-0.1, 10, 50, 100, 167, 225, 275, 350), names = c("Included","< 20","21-75", "76-125","126-200", "201-250", "251-300", "> 300")),
                                                                      missing.values=-0.1, annotation=Label(e$heating_expenses))
  
  if ("gas_expenses" %in% names(e)) temp <-  15*(e$gas_expenses == "$5 – $25") + 50*(e$gas_expenses == "$26 – $75") + 100*(e$gas_expenses== "$76 – $125") + 150*(e$gas_expenses == "$126 – $175") + 200*(e$gas_expenses== "$176 – $225") + 250*(e$gas_expenses == "More than $225")
  if ("gas_expenses" %in% names(e)) e$gas_expenses <- as.item(temp, labels = structure(c(0, 15, 50, 100, 150, 200, 250), names = c("< 5","5-25","26-75", "76-125","126-175", "176-225", "> 225")),
                                                          annotation=Label(e$gas_expenses))
  
  if ("insulation_compulsory" %in% names(e)) e$insulation_compulsory[e$insulation_compulsory %in% text_insulation_mandatory] <- "Mandatory"
  if ("insulation_compulsory" %in% names(e)) e$insulation_compulsory[e$insulation_compulsory %in% text_insulation_voluntary] <- "Voluntary"
  if ("insulation_compulsory" %in% names(e)) e$insulation_compulsory <- as.item(as.character(e$insulation_compulsory), labels = structure(c("Mandatory", "Voluntary", "PNR"), names=c("Mandatory", "Voluntary", "PNR")), 
                                      missing.values = "PNR", annotation=Label(e$insulation_compulsory))
  
  if("flight_quota" %in% names(e)) {
    e$flight_quota <- e$flight_quota_1000km
    if (wave == "pilot1") e$flight_quota[!is.na(e$flight_quota_3000km)] <- e$flight_quota_3000km[!is.na(e$flight_quota_3000km)]
    if (wave == "pilot2") e$flight_quota[!is.na(e$flight_quota_1000km_global)] <- e$flight_quota_1000km_global[!is.na(e$flight_quota_1000km_global)]
    e$flight_quota[!is.na(e$flight_quota_one_trip)] <- e$flight_quota_one_trip[!is.na(e$flight_quota_one_trip)]
    label(e$flight_quota) <- "flight_quota: ~ Given that the govt decides to limit average flights per person, what do you prefer? Rationing / Tradable quota / PNR. Variants (distance per year): 1000km/1000km global/one round-trip every two years. [units adjusted to country]"
    variables_flight_quota <<- names(e)[grepl('flight_quota', names(e))]
  
    for (v in variables_flight_quota) {
      e[[v]][e[[v]] %in% text_flight_quota_rationing] <- "Rationing"
      e[[v]][e[[v]] %in% text_flight_quota_tradable] <- "Tradable" }
    e$variant_flight_quota <- ""
    e$variant_flight_quota[!is.na(e$flight_quota_3000km)] <- "3000km"
    e$variant_flight_quota[!is.na(e$flight_quota_1000km_global)] <- "1000km global"
    e$variant_flight_quota[!is.na(e$flight_quota_1000km)] <- "1000km"
    e$variant_flight_quota[!is.na(e$flight_quota_one_trip)] <- "1 trip"
    for (v in variables_flight_quota) { e[[v]] <- as.item(as.character(e[[v]]), labels = structure(c("Rationing", "Tradable", "PNR"), 
                            names=c("Rationing", "Tradable", "PNR")), missing.values = "PNR", annotation=Label(e[[v]])) }
  }

  if ("ban_incentives" %in% names(e)) {
    e$ban_incentives[e$ban_incentives %in% text_ban_incentives_encourage] <- "Encourage"
    e$ban_incentives[e$ban_incentives %in% text_ban_incentives_force] <- "Force"
    e$ban_incentives <- as.item(as.character(e$ban_incentives), missing.values = 'PNR', annotation=Label(e$ban_incentives))
  }
  
  if ("interest_politics" %in% names(e)) temp <-  (e$interest_politics %in% text_interest_politics_lot) - (e$interest_politics %in% text_interest_politics_no) - 0.1 * (e$interest_politics %in% text_pnr)
  if ("interest_politics" %in% names(e)) e$interest_politics <- as.item(temp, labels = structure(c(-1:1,-0.1),
                        names = c("Not really or not at all","A little", "A lot","PNR")),
                      missing.values=-0.1, annotation=Label(e$interest_politics))
  # TODO! intensity questions
  
  if ("media" %in% names(e)) {
    e$media[e$media %in% text_media_other] <- "Other"
    e$media[e$media %in% text_media_print] <- "Print"
    e$media[e$media %in% text_media_radio] <- "Radio"
    e$media[e$media %in% text_media_social] <- "Social media"
    e$media[e$media %in% text_media_TV_private] <- "TV (private)"
    e$media[e$media %in% text_media_TV_public] <- "TV (public)"
    e$media[e$media %in% text_media_web] <- "News websites"
  }
  
  e$vote_participation[e$vote_participation %in% text_vote_participation_no_right] <- "No right to vote"
  if ("vote_voters_2016" %in% names(e)) {
    e$vote_participation_2016[e$vote_participation_2016 %in% text_vote_participation_no_right] <- "No right to vote"
    e$vote[!is.na(e$vote_voters) & e$vote_participation=="Yes"] <- e$vote_voters[!is.na(e$vote_voters) & e$vote_participation=="Yes"]
    e$vote[!is.na(e$vote_non_voters_2016) & e$vote_participation!="Yes"] <- e$vote_non_voters[!is.na(e$vote_non_voters_2016) & e$vote_participation!="Yes"]
    e$vote_2016[!is.na(e$vote_voters_2016) & e$vote_participation_2016=="Yes"] <- e$vote_voters_2016[!is.na(e$vote_voters_2016) & e$vote_participation_2016=="Yes"]
    e$vote_2016[!is.na(e$vote_non_voters_2016) & e$vote_participation_2016!="Yes"] <- e$vote_non_voters_2016[!is.na(e$vote_non_voters_2016) & e$vote_participation_2016!="Yes"]
    e$vote_participation_2016 <- as.item(as.character(e$vote_participation_2016), missing.values = 'PNR', annotation=Label(e$vote_participation_2016))
    e$vote_2016 <- as.item(as.character(e$vote_2016), missing.values = 'PNR', annotation=Label(e$vote_2016))
    e$vote_2016_factor <- as.factor(e$vote_2016)
    e$vote_2016_factor <- relevel(relevel(e$vote_2016_factor, "Stein"), "Clinton")
  }
  e$vote_participation <- as.item(as.character(e$vote_participation), missing.values = 'PNR', annotation=Label(e$vote_participation))
  e$vote <- as.item(as.character(e$vote), missing.values = 'PNR', annotation=Label(e$vote))

  e$survey_biased[e$survey_biased %in% text_survey_biased_pro_envi] <- "Yes, pro environment"
  e$survey_biased[e$survey_biased %in% text_survey_biased_anti_envi] <- "Yes, anti environment"
  e$survey_biased[e$survey_biased %in% text_survey_biased_left] <- "Yes, left"
  e$survey_biased[e$survey_biased %in% text_survey_biased_right] <- "Yes, right"
  e$survey_biased[e$survey_biased %in% text_survey_biased_no] <- "No"
  if ("Yes, right" %in% levels(as.factor(e$survey_biased))) e$survey_biased <- relevel(relevel(as.factor(e$survey_biased), "Yes, right"), "No")
  
  e$wtp <- as.numeric(as.vector(gsub('[[:alpha:] $]', '', e$wtp))) # /!\ Careful with different currencies and use of cents vs. currency (for US $, not pb as cents are not used)
  e$wtp_agg <- 5 * (e$wtp > 0 & e$wtp <= 10) + 50 * (e$wtp > 10 & e$wtp <= 70) + 100 * (e$wtp > 70 & e$wtp <= 100) + 200 * (e$wtp > 100 & e$wtp <= 300) + 500 * (e$wtp > 300 & e$wtp <= 500) + 1000 * (e$wtp > 500)
  e$wtp_agg <- as.item(e$wtp_agg, labels = structure(c(0,5,50,100,200,500,1000), names = c("0", "From 0.5 to 10", "30 to 70", "100", "150 to 300", "500", "1000 or more")), annotation=Label(e$wtp_agg))
  
  # TODO: Yes/No => T/F?, heating, CC_affected, label should_act_condition & vote, nb_policies_supported, score_knowlege_CC, score_trust, zipcode, race, standard of living
  
  if ("left" %in% names(e)) {
    e$left_right <- pmax(-2,pmin(2,-2 * e$far_left - 1*e$left + 1*e$right + 2 * e$far_right))
    is.na(e$left_right) <- (e$left_right == 0) & !e$center
    e$Left_right <- as.factor(e$left_right)
    e$left_right <- as.item(as.numeric(as.vector(e$left_right)), labels = structure(c(-2:2),
                            names = c("Far left", "Left or center-left", "Center", "Right or center-right", "Far right")), annotation="left_right: scale from -2 (far left) to +2 (far right) - Political leaning - How would you define yourself? Multiple answers are possible: (Far) left/Center/(Far) right/Liberal/Conservative/Humanist/Patriot/Apolitical/Environmentalist/Feminist/Other (specify)")
    levels(e$Left_right) <- c("Far left", "Left or center-left", "Center", "Right or center-right", "Far right", "Indeterminate")
    e$Left_right[is.na(e$Left_right)] <- "Indeterminate"
    e$indeterminate <- e$Left_right == "Indeterminate"
    e$left_right_pnr <- as.character(e$left_right)
    e$left_right_pnr[e$Left_right=='Indeterminate'] <- 'PNR'
    e$left_right_pnr <- as.factor(e$left_right_pnr)
    e$left_right_pnr <- relevel(relevel(e$left_right_pnr, "Left or center-left"), "Far left")
    label(e$left_right) <- "left_right: How would you define yourself? Far Left/Left or center-left/Center/Right or center-right/Far right"
  }
  
  e$country <- country
  label(e$country) <- "country: Country of the survey. US/FRA/IND/DEN"
  e$wave <- wave
  label(e$wave) <- "wave: Wave of the survey. pilot1/pilot2/full"

  # TODO! policies_cost_effective, employment, side_effects
  if ("standard_trust" %in% names(e)) e$policies_trust <- ((e$standard_trust=="Yes") + (e$investments_trust=="Yes") + (e$tax_transfers_trust=="Yes") - (e$standard_trust=="No") - (e$investments_trust=="No") - (e$tax_transfers_trust=="No"))/3
  if ("standard_trust" %in% names(e)) label(e$policies_trust) <- "policies_trust: Could the U.S. federal government be trusted to correctly implement an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Yes/No/PNR"
  if ("standard_effective" %in% names(e)) e$policies_effective <- ((e$standard_effective=="Yes") + (e$investments_effective=="Yes") + (e$tax_transfers_effective=="Yes") - (e$standard_effective=="No") - (e$investments_effective=="No") - (e$tax_transfers_effective=="No"))/3
  if ("standard_effective" %in% names(e)) label(e$policies_effective) <- "policies_effective: Woudl an emission limit for cars, a green infrastrcuture program and a carbon tax be effective to fight climate change? Yes/No/PNR"
  if ("standard_employment" %in% names(e)) e$policies_employment <- ((e$standard_employment=="Positive") + (e$investments_employment=="Positive") + (e$tax_transfers_employment=="Positive") - (e$standard_employment=="Negative") - (e$investments_employment=="Negative") - (e$tax_transfers_employment=="Negative"))/3
  if ("standard_employment" %in% names(e)) label(e$policies_employment) <- "policies_employment: Would an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers have positive or negative impact on employment? Postive impacts/No notable impact/Negative impacts/PNR"
  if ("standard_side_effects" %in% names(e)) e$policies_side_effects <- ((e$standard_side_effects=="Positive") + (e$investments_side_effects=="Positive") + (e$tax_transfers_side_effects=="Positive") - (e$standard_side_effects=="Negative") - (e$investments_side_effects=="Negative") - (e$tax_transfers_side_effects=="Negative"))/3
  if ("standard_side_effects" %in% names(e)) label(e$policies_side_effects) <- "policies_side_effects: Would an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers have positive or negative side effects overall? Positive impacts/No notable impact/Negative impacts/PNR"
  if ("standard_large_effect" %in% names(e)) e$policies_large_effect <- (e$standard_large_effect + e$investments_large_effect + e$tax_transfers_large_effect)/3
  if ("standard_large_effect" %in% names(e)) label(e$policies_large_effect) <- "policies_large_effect: An emission limit for cars, a green infrastructure program and a carbon tax with cash transfers would have large effect on the economy and employment? Strongly disagree-agree"
  if ("standard_negative_effect" %in% names(e)) e$policies_negative_effect <- (e$standard_negative_effect + e$investments_negative_effect + e$tax_transfers_negative_effect)/3
  if ("standard_negative_effect" %in% names(e)) label(e$policies_negative_effect) <- "policies_negative_effect: An emission limit for cars, a green infrastructure program and a carbon tax with cash transfers would have negative effect on the economy and employment? Strongly disagree-agree"
  if ("standard_fair" %in% names(e)) e$policies_fair <- (e$standard_fair + e$investments_fair + e$tax_transfers_fair)/3
  if ("standard_fair" %in% names(e)) label(e$policies_fair) <- "policies_fair: An emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers is fair? Strongly disagree - strongly agree"
  if ("standard_cost_effective" %in% names(e)) e$policies_cost_effective <- (e$standard_cost_effective + e$investments_cost_effective + e$tax_transfers_cost_effective)/3
  if ("standard_cost_effective" %in% names(e)) label(e$policies_cost_effective) <- "policies_cost_effective: An emission limit for cars, a green infrastrcuture program and a carbon tax would be cost-effective to fight climate change. Strongly disagree - strongly sagree"
  # e$policies_support <- ((e$standard_support=="Yes") + (e$investments_support=="Yes") + (e$tax_transfers_support=="Yes") - (e$standard_support=="No") - (e$investments_support=="No") - (e$tax_transfers_support=="No"))/3
  # label(e$policies_support) <- "policies_support: Would you support an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Yes/No/PNR" # TODO compatibility pilots 1, 2
  e$policies_support <- (e$standard_support + e$investments_support + e$tax_transfers_support) / 3
  label(e$policies_support) <- "policies_support: Average of responses in [-2;+2] to Would you support an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers?"
  e$policies_self <- e$policies_incidence <- e$policies_poor <- e$policies_middle <- e$policies_rich <- e$policies_rural <- e$policies_urban <- 0
  label(e$policies_self) <- "policies_self: Would your household win or lose financially from an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Win/Lose/Be unaffected/PNR" # TODO labels
  label(e$policies_poor) <- "policies_self: Would the poorest win or lose financially from an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Win/Lose/Be unaffected/PNR" # TODO labels
  label(e$policies_middle) <- "policies_self: Would the middle class win or lose financially from an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Win/Lose/Be unaffected/PNR" # TODO labels
  label(e$policies_rich) <- "policies_self: Would the richest financially win or lose from an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Win/Lose/Be unaffected/PNR" # TODO labels
  label(e$policies_rural) <- "policies_self: Would rural financially win or lose from an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Win/Lose/Be unaffected/PNR" # TODO labels
  label(e$policies_urban) <- "policies_self: Would urban dwellers win or lose financially from an emission limit for cars, a green infrastrcuture program and a carbon tax with cash transfers? Win/Lose/Be unaffected/PNR" # TODO labels
  # for (v in variables_incidence) e$policies_incidence <- e$policies_incidence + e[[v]]/length(variables_incidence) # this variable makes no sense
  text_incidence <- ifelse("standard_incidence_poor" %in% names(e), "incidence", "win_lose")
  for (v in names_policies) e$policies_self <- e$policies_self + e[[paste(v, text_incidence, "self", sep="_")]]/3
  for (v in names_policies) e$policies_poor <- e$policies_poor + e[[paste(v, text_incidence, "poor", sep="_")]]/3
  for (v in names_policies) e$policies_middle <- e$policies_middle + e[[paste(v, text_incidence, "middle", sep="_")]]/3
  for (v in names_policies) e$policies_rich <- e$policies_rich + e[[paste(v, text_incidence, "rich", sep="_")]]/3
  for (v in names_policies) e$policies_rural <- e$policies_rural + e[[paste(v, text_incidence, "rural", sep="_")]]/3
  if ("standard_incidence_urban" %in% names(e)) for (v in names_policies) e$policies_urban <- e$policies_urban + e[[paste(v, text_incidence, "urban", sep="_")]]/3

  e$core_metropolitan <- as.numeric(as.vector(e$urban_category))==1
  label(e$core_metropolitan) <- "core_metropolitan: Live in a core metropolitan zip code. TRUE/FALSE"

  if ("CC_affected_2050" %in% names(e)) {
    e$CC_affected_min <- 2100
    e$CC_affected_min[e$CC_affected_2050==T] <- 2050
    e$CC_affected_min[e$CC_affected_2020==T] <- 2020
    e$CC_affected_min[e$CC_affected_1990==T] <- 1990
    e$CC_affected_min[e$CC_affected_1960==T] <- 1960
    e$CC_affected_min[e$CC_affected_pnr==T] <- -0.1
    e$CC_affected_min <- as.item(e$CC_affected_min, labels = structure(c(1960,1990,2020,2050,2100,-0.1),
                                                          names = c("1960","1990","2020","2050","None","PNR")),
                                 missing.values=-0.1, annotation=Label(e$CC_affected_min))
    label(e$CC_affected_min) <- "CC_affected_min: Youngest generation seriously affected by climate change. 2100/2050/2020/1990/1960/PNR" 
  }
  
  if ("CC_impacts" %in% names(e)) {
    temp <- -2*(e$CC_impacts %in% text_CC_impacts_cataclysmic) -1*(e$CC_impacts %in% text_CC_impacts_disastrous) + 1*(e$CC_impacts %in% text_CC_impacts_small) + 2*(e$CC_impacts %in% text_CC_impacts_insignificant) -0.1*(e$CC_impacts %in% text_pnr)
    e$CC_impacts <- as.item(temp, labels = structure(c(-2:2,-0.1), names = c("Insignificant","Small","Grave","Disastrous","Cataclysmic","PNR")),
                                 missing.values=-0.1, annotation=Label(e$CC_impacts))
  }
  
  if (!("flights_agg" %in% names(e))) {
    e$flights_agg <- 1.8*(e$flights %in% 1:2) + 5*(e$flights %in% 3:7) + 11*(e$flights %in% 8:14) + 25*(e$flights > 14)
    e$flights_agg <- as.item(e$flights_agg, labels = structure(c(0,1.8,5,11,25), names = c("0", "1 or 2", "3 to 7", "8 to 14", "15 or more")), annotation=attr(e$flights, "label"))
  } else {
    e$flights_agg <- 1*(e$flights_agg == "1") + 2*(e$flights_agg == "2") + 3.5*(e$flights_agg == "3 or 4") + 7*(e$flights_agg == "5 to 10") + 12*(e$flights_agg == "10 or more")
    e$flights_agg <- as.item(e$flights_agg, labels = structure(c(0,1,2,3.5,7,12), names = c("0", "1", "2", "3 or 4", "5 to 10", "10 or more")), annotation=attr(e$flights_agg, "label"))
  } 
  
  if ("km_driven" %in% names(e)) {
    e$km_driven_agg <- 3000*(e$km_driven > 1000 & e$km_driven <= 5000) + 7500*(e$km_driven > 5000 & e$km_driven <= 10000) + 15000*(e$km_driven > 10000 & e$km_driven <= 20000) + 25000*(e$km_driven > 20000 & e$km_driven <= 30000) + 60000*(e$km_driven > 30000)
    e$km_driven_agg <- as.item(e$km_driven_agg, labels = structure(c(0,3000,7500,15000,25000,60000), names = c("Below 1,000", "1,001 to 5,000", "5k to 10k", "10k to 20k", "20k to 30k", "More than 30k")), annotation=attr(e$flights, "label"))
  }
  
  if ("donation" %in% names(e)) {
    e$donation_agg <- 0*(e$donation == 0) + 10*(e$donation %in% 1:20) + 30*(e$donation %in% 21:40) + 70*(e$donation %in% 41:99) + 100*(e$donation == 100)
    e$donation_agg <- as.item(e$donation_agg, labels = structure(c(0,10,30,70,100), names = c("0", "1 to 20", "21 to 40", "41 to 99", "100")), annotation=attr(e$donation, "label"))
  }
  
  e$treatment_climate <- ifelse(e$treatment_climate > sqrt(5/17), 1, 0)
  e$treatment_policy <- ifelse(e$treatment_policy > sqrt(5/17), 1, 0)
  e$treatment <- "None"
  e$treatment[e$treatment_climate == 1 & e$treatment_policy == 0] <- "Climate"
  e$treatment[e$treatment_climate == 0 & e$treatment_policy == 1] <- "Policy"
  e$treatment[e$treatment_climate == 1 & e$treatment_policy == 1] <- "Both"
  e$treatment <- relevel(relevel(relevel(as.factor(e$treatment), "Policy"), "Climate"), "None")
  label(e$treatment) <- "treatment: Treatment received: Climate/Policy/Both/None" 
  
  if ("know_standard" %in% names(e)) {
    e$know_treatment_climate <- (e$know_temperature_2100 == "8 °F") + (e$know_frequence_heatwaves == "70 days per year")
    e$know_treatment_policy <- (e$know_standard == "A limit on CO2 emissions from cars") + (e$know_investments_jobs == "1.5 million people")
    label(e$know_treatment_climate) <- "know_treatment_climate: Number of good responses among the 2 knowledge questions related to treatment content: temperature_2100 = 8°F & frequence_heatwaves = 70 days per year"
    label(e$know_treatment_policy) <- "know_treatment_policy: Number of good responses among the 2 knowledge questions related to treatment content: standard = limit on car emissions & investments_jobs = 1.5 million"
  }
  
  if ("GHG_methane" %in% names(e)) {
    e$score_GHG <- e$GHG_CO2 + e$GHG_methane - e$GHG_H2 - e$GHG_particulates + 2
    e$know_GHG_CO2 <- e$GHG_CO2
    e$know_GHG_methane <- e$GHG_methane
    e$know_GHG_H2 <- ifelse(e$GHG_H2, F, T)
    e$know_GHG_particulates <- ifelse(e$GHG_particulates, F, T)
    label(e$score_GHG) <- "score_GHG: Score to the knowledge of GHG [0;+4] = CO2 + methane - H2 - particulates + 2"
    label(e$know_GHG_CO2) <- "know_GHG_CO2: Correct answer that CO2 is a GHG"
    label(e$know_GHG_methane) <- "know_GHG_methane: Correct answer that methane is a GHG"
    label(e$know_GHG_H2) <- "know_GHG_H2: Correct answer that H2 is not a GHG"
    label(e$know_GHG_particulates) <- "know_GHG_particulates: Correct answer that particulates is not a GHG"
  }
  
  if ("footprint_el_coal" %in% names(e)) {
    e$score_footprint_elec <- AllSeqDists(cbind(e$footprint_el_coal, e$footprint_el_gas, e$footprint_el_wind))
    e$score_footprint_food <- AllSeqDists(cbind(e$footprint_fd_beef, e$footprint_fd_chicken, e$footprint_fd_pasta))
    e$score_footprint_transport <- AllSeqDists(cbind(e$footprint_tr_plane, e$footprint_tr_car, e$footprint_tr_coach))
    e$score_footprint_region <- AllSeqDists(cbind(e$footprint_reg_US, e$footprint_reg_europe, e$footprint_reg_china, e$footprint_reg_india))
    label(e$score_footprint_elec) <- "e$score_footprint_elec: Kendall distance with true ranking of electricity footprints: coal>gas>wind"
    label(e$score_footprint_food) <- "e$score_footprint_food: Kendall distance with true ranking of food footprints: beef>chicken>pasta"
    label(e$score_footprint_transport) <- "e$score_footprint_transport: Kendall distance with true ranking of transport footprints: plane>car>coach"
    label(e$score_footprint_region) <- "e$score_footprint_region: Kendall distance with true ranking of region footprints: US>Western Europe>China>India"
    e$know_footprint_elec <- e$score_footprint_elec == 0
    e$know_footprint_food <- e$score_footprint_elec == 0
    e$know_footprint_transport <- e$score_footprint_elec == 0
    e$know_footprint_region <- e$score_footprint_elec == 0
    label(e$know_footprint_elec) <- "know_footprint_elec: Correct answer to the ranking of electricity footprints"
    label(e$know_footprint_food) <- "know_footprint_food: Correct answer to the ranking of electricity footprints"
    label(e$know_footprint_transport) <- "know_footprint_transport: Correct answer to the ranking of electricity footprints"
    label(e$know_footprint_region) <- "know_footprint_region: Correct answer to the ranking of electricity footprints"
    e$most_footprint_el <- e$most_footprint_fd <- e$most_footprint_tr <- e$most_footprint_reg <- e$least_footprint_el <- e$least_footprint_fd <- e$least_footprint_tr <- e$least_footprint_reg <- "PNR"
    for (v in c("el", "fd", "tr", "reg")) {
      for (i in Variables_footprint[[v]]) {
        e[[paste("most_footprint", v, sep="_")]][e[[i]]==1] <- capitalize(sub(paste("footprint_", v, "_", sep=""), "", i))
        e[[paste("least_footprint", v, sep="_")]][e[[i]]==3] <- capitalize(sub(paste("footprint_", v, "_", sep=""), "", i))
      }
      label(e[[paste("most_footprint", v, sep="_")]]) <- paste("most_footprint_", v, ": Largest footprint of type ", v, " according to the respondent", sep="")
      label(e[[paste("least_footprint", v, sep="_")]]) <- paste("least_footprint_", v, ": Smallest footprint of type ", v, " according to the respondent", sep="")
    }
  }

  e$rush_treatment <- e$duration_treatment_climate < 2.4 | e$duration_treatment_policy < 4.45 # TODO adapt time for climate
  e$rush_treatment[is.na(e$rush_treatment)] <- F
  label(e$rush_treatment) <- "rush_treatment: Has rushed the treatment. TRUE/FALSE" 

  
  e$rush <- e$rush_treatment | (e$duration < 15)
  label(e$rush) <- "rush: Has rushed the treatment or the survey. TRUE/FALSE" 
  # race TODO: problem: someone can be at the same time Hispanic and black or white. Why don't you keep the dummies race_white, race_black, race_hispanic?
  e$race_white_only <- 0
  e$race_white_only[e$race_white == TRUE & e$race_black == FALSE & e$race_hispanic == FALSE & e$race_asian == FALSE & e$race_native == FALSE] <- 1
  #e[e$race_black == TRUE, "race"] <- "Black"
  #e[e$race_hispanic == TRUE, "race"] <- "Hispanic"
  #e[e$race_asian == TRUE | e$race_native == TRUE | e$race_hawaii == TRUE | e$race_other_choice == TRUE | e$race_pnr == TRUE , "race"] <- "Other"
  e$race_white_only <- as.factor(e$race_white_only)
  
  #gender: Other set as Male for the moment, see if lot of similar answers in final data
  e$gender_dum <- as.character(e$gender)
  e[e$gender == "Other", "gender_dum"] <- "Male"
  e$gender_dum <- as.factor(e$gender_dum)
  e$gender_factor <- as.factor(e$gender)
  if ("Other" %in% levels(e$gender_factor)) e$gender_factor <- relevel(relevel(as.factor(e$gender), "Other"), "Female")
 
  e$children <- 0
  if ("nb_children" %in% names(e)) e[e$nb_children >= 1, "children"] <- 1
  else if ("Nb_children" %in% names(e)) e$children[!(e$Nb_children %in% c(0, "0"))] <- 1
  e$children <- as.factor(e$children)
  if ("nb_children" %in% names(e)) e$nb_children_ceiling_4 <- pmin(e$nb_children, 4)
  else if ("Nb_children" %in% names(e)) {
    e$nb_children_ceiling_4 <- e$Nb_children
    e$nb_children_ceiling_4[e$Nb_children == "4 or more"] <- 4
    e$nb_children_ceiling_4 <- as.numeric(as.vector(e$nb_children_ceiling_4)) }
  
  e$college <- "No college"
  e[e$education >= 5, "college"] <- "College Degree"
  e$college <- as.factor(e$college)

  e$employment_agg <-  "Not working"
  e[e$employment_status == "Student", "employment_agg"] <- "Student"
  e[e$employment_status == "Retired", "employment_agg"] <- "Retired"
  e[e$employment_status == "Self-employed" | e$employment_status == "Full-time employed" | e$employment_status == "Part-time employed", "employment_agg"] <- "Working"
  e$employment_agg <- as.factor(e$employment_agg)
  
  if ("age" %in% names(e)) {
    e$age_agg <- NULL
    e[e$age %in% 18:29, "age_agg"] <- "18-29"
    e[e$age %in% 30:49, "age_agg"] <- "30-49"
    e[e$age %in% 50:87, "age_agg"] <- "50-87"
    e$age_agg <- as.factor(e$age_agg)
    e$age_quota <- NULL
    e$age_quota[e$age %in% 18:24] <- "18-24"
    e$age_quota[e$age %in% 25:34] <- "25-34"
    e$age_quota[e$age %in% 35:49] <- "35-49"
    e$age_quota[e$age %in% 50:64] <- "50-64"
    e$age_quota[e$age > 64] <- "65+" # TODO "> 65" instead
  } else { 
    e$age_quota[e$age_quota == "18 to 24"] <- "18-24"
    e$age_quota[e$age_quota == "25 to 34"] <- "25-34"
    e$age_quota[e$age_quota == "35 to 49"] <- "35-49"
    e$age_quota[e$age_quota == "50 to 64"] <- "50-64"
    e$age_quota[e$age_quota == "65 or above"] <- "65+" 
  }
  
  # political position
  # AF TODO I'd rather use the dummy vote=='Biden' than a variable vote_dum with 4 modalities
  e$vote_dum <- as.character(e$vote)
  if (country == "US") {
    e$vote_dum[!(e$vote %in% c("Biden", "Trump"))] <- "Other"
    if ("Other" %in% levels(as.factor(e$vote_dum))) e$vote_dum <- relevel(as.factor(e$vote_dum), "Other") }
  # # e[e$vote_participation == 2, "vote_dum"] <- "Other" # add non-voters as others
  # # e$vote_dum <- as.factor(e$vote_dum)
  # # e$vote_dum <- relevel(e$vote_dum, ref ="Other")
  
  e$race <- "Other"
  e$race[e$race_white==T & e$race_asian == FALSE & e$race_native == FALSE] <- "White only"
  e$race[e$race_hispanic==T] <- "Hispanic"
  e$race[e$race_black==T] <- "Black"
  label(e$race) <- "race: White only/Hispanic/Black/Other. True proportions: .601/.185/.134/.08"

  e$vote_2020 <- "Other/Non-voter"
  e$vote_2020[e$vote_participation %in% c("No right to vote", "PNR") | e$vote_voters=="PNR"] <- "PNR/no right"
  e$vote_2020[e$vote_voters == "Biden"] <- "Biden"
  e$vote_2020[e$vote_voters == "Trump"] <- "Trump"
  e$vote_2020 <- as.item(e$vote_2020, annotation = "vote_2020: Biden / Trump / Other/Non-voter / PNR/No right. True proportions: .342/.313/.333/.0")
  missing.values(e$vote_2020) <- "PNR/no right"
  # label(e$vote_2020) <- "vote_2020: Biden / Trump / Other/Non-voter / PNR/No right. True proportions: .342/.313/.333/.0"
  
  e$income_factor <- as.factor(e$income)
  
  # e <- e[, -c(9:17)] 
  return(e)
}

weighting <- function(data, printWeights = T, vote = F) { 
  d <- data 
  d$core_metropolitan[is.na(d$core_metropolitan)] <- "NA"
  d$age_quota[is.na(d$age_quota)] <- "NA"
  d$region[is.na(d$region)] <- "NA"
  
  unweigthed <- svydesign(ids=~1, data=d)
  if ("NA" %in% levels(as.factor(d$core_metropolitan))) core_metropolitan <- data.frame(core_metropolitan = c(FALSE, TRUE, "NA"), 
                                  Freq=nrow(d)*c(0.2676,0.7324, 0.0001))
  else core_metropolitan <- data.frame(core_metropolitan = c(FALSE, TRUE), Freq=nrow(d)*c(0.2676,0.7324))
  # taille_agglo <- data.frame(taille_agglo = c(1:5), Freq=nrow(d)*c(0.2166,0.1710,0.1408,0.3083,0.1633))
  if ("Other" %in% levels(as.factor(d$gender))) gender <- data.frame(gender = c("Female", "Male", "Other"), 
                                                                     Freq=nrow(d)*c(0.5074,0.4974, 0.0002)) # France: c(0.516,0.484)
  else gender <- data.frame(gender = c("Female", "Male"), Freq=nrow(d)*c(0.5074,0.4974))
  # csp <- data.frame(csp = c("Inactif", "Ouvrier", "Cadre", "Indépendant", "Intermédiaire", "Retraité", "Employé", "Agriculteur"),
  #                   Freq=nrow(d)*c(0.129,0.114,0.101,0.035,0.136,0.325,0.15,0.008))
  income <- data.frame(income = c("Q1", "Q2", "Q3", "Q4"),
                    Freq=nrow(d)*c(0.2034,0.239,0.2439,0.3137))
  # region <- data.frame(region = c("autre","ARA", "Est", "Nord", "IDF", "Ouest", "SO", "Occ", "Centre", "PACA"), 
  #                      Freq=nrow(d)*c(0.0001,0.12446,0.12848,0.09237,0.1902,0.10294,0.09299,0.09178,0.09853,0.07831))
  if ("NA" %in% levels(as.factor(d$region))) region <- data.frame(region = c("Midwest","Northeast", "South", "West", "NA"), 
                       Freq=nrow(d)*c(0.171,0.208,0.383,0.239, 0.0001))
  else region <- data.frame(region = c("Midwest","Northeast", "South", "West"), 
                       Freq=nrow(d)*c(0.171,0.208,0.383,0.239))
  if ("NA" %in% levels(as.factor(d$age_quota))) age_quota <- data.frame(age_quota = c("18-24", "25-34", "35-49", "50-64", "65+", "NA"), 
                                                       Freq=nrow(d)*c(0.118,0.180,0.243,0.2467,0.2118, 0.0001))
  else if ("Below 18" %in% levels(as.factor(d$age_quota))) age_quota <- data.frame(age_quota = c("18-24", "25-34", "35-49", "50-64", "65+", "Below 18"), 
                                                                             Freq=nrow(d)*c(0.118,0.180,0.243,0.2467,0.2118, 0.0001))
  else age_quota <- data.frame(age_quota = c("18-24", "25-34", "35-49", "50-64", "65+"), 
                    Freq=nrow(d)*c(0.118,0.180,0.243,0.2467,0.2118)) # France: c(0.120,0.150,0.240,0.240,0.250)
  # revenu <- data.frame(revenu = c(), Freq=nrow(d)*c())
  # diplome4 <- data.frame(diplome4 = c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur"), 
  #                        Freq=nrow(d)*c(0.290, 0.248, 0.169, 0.293))
  race <- data.frame(race = c("White only", "Hispanic", "Black", "Other"), Freq=nrow(d)*c(.601, .185, .134, .080))
  vote_2020 <- data.frame(vote_2020 = c("Biden", "Trump", "Other/Non-voter", "PNR/no right"), Freq=nrow(d)*c(c(0.342171, 0.312823, 0.345006)*(nrow(d)-sum(d$vote_2020=="PNR/no right")), sum(d$vote_2020=="PNR/no right"))/nrow(d))
  
  if (vote) raked <- rake(design= unweigthed, sample.margins = list(~gender,~income,~region,~core_metropolitan,~age_quota,~race,~vote_2020),
                                                              population.margins = list(gender,income,region,core_metropolitan,age_quota,race,vote_2020))     
  else raked <- rake(design= unweigthed, sample.margins = list(~gender,~income,~region,~core_metropolitan,~age_quota,~race),
                     population.margins = list(gender,income,region,core_metropolitan,age_quota,race))     

  if (printWeights) {    print(summary(weights(raked))  )
    print(paste("(mean w)^2 / (n * mean w^2): ", round(sum( weights(raked) )^2/(length(weights(raked))*sum(weights(raked)^2)), 3), " (pb if < 0.5)")) # <0.5 : problématique   
    print(paste("proportion not in [0.25; 4]: ", round(length(which(weights(raked)<0.25 | weights(raked)>4))/ length(weights(raked)), 3)))
  }
  return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE))) # TODO! race
}


prepare <- function(exclude_speeder=TRUE, exclude_screened=TRUE, only_finished=TRUE, only_known_agglo=T, duration_min=0, country = "US", wave = NULL) { # , exclude_quotas_full=TRUE
  if (country == "US") {
    if (wave == "pilot1") e <- read_csv("../data/US_pilot.csv") 
    else if (wave == "pilot2") e <- read_csv("../data/US_pilot2.csv") 
    else if (wave == "pilot3") e <- read_csv("../data/US_pilot3.csv") 
    else if (wave == "full") e <- read_csv("../data/US.csv") 
  }

  e <- relabel_and_rename(e, country = country, wave = wave)
  
  print(paste(length(which(e$excluded=="QuotaMet")), "QuotaMet"))
  e$finished[e$excluded=="QuotaMet"] <- "False" # To check the number of QuotaMet that shouldn't have incremented the quota, comment this line and: decrit(e$each_strate[e$exclu=="QuotaMet" & e$csp=="Employé" & !grepl("2019-03-04 07", e$date)])
  if (exclude_screened) { e <- e[is.na(e$excluded),] }
  if (exclude_speeder) { e <- e[as.numeric(as.vector(e$duration)) > duration_min,] } 
  if (only_finished) { # TODO: le faire marcher même pour les autres
    e <- e[e$finished==1,] 
    e <- convert(e, country = country, wave = wave)
   
    e$weight <- weighting(e) 
    e$weight_vote <- weighting(e, vote = T)
  
    # e$left_right_na <- as.numeric(e$left_right)
    # e$left_right_na[e$indeterminate == T] <- wtd.mean(e$left_right, weights = e$weight)
  } else {
  }
  
  # e$sample <- "a"
  # e$sample[e$finished=="True"] <- "e"
  # e$sample[e$finished=="True" & n(e$duration) > duration_min] <- "p"
  # e$sample[e$finished=="True" & n(e$duration) > duration_min & e$excluded==""] <- "r"
  
  return(e)
}

usp1 <- prepare(country = "US", wave = "pilot1", duration_min = 0)
usp2 <- prepare(country = "US", wave = "pilot2", duration_min = 686)
e <- usp3 <- prepare(country = "US", wave = "pilot3", duration_min = 686)
usp12 <- merge(usp1, usp2, all = T)
us <- merge(usp3, usp12, all = T, by="date")

# e <- read_csv("../data/US_pilot3.csv") 
# e <- e[-c(1:2),]
# sum((is.na(e$Q18.2_6) | is.na(e$Q18.2_1)) & !is.na(e$Q18.3_1), na.rm=T)/sum(!is.na(e$Q18.3_1)) # error rate attention test
# e$Finished_1[e$Q_TerminateFlag=="QuotaMet"] <- "False"
# e <- e[is.na(e$Q_TerminateFlag),]
# e <- e[as.numeric(as.vector(e$Q_TotalDuration)) > 686,]
# e <- e[e$Finished_1==1,]
# decrit(as.numeric(e$Q_TotalDuration)/60)
# decrit(e$Q12.1)
# decrit(e$Q9.1) # no bug in videos
