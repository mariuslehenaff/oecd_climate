library(haven)
library(xtable)
library(stargazer)
library(clipr)
library(wesanderson)
library(ggplot2)


Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Google Drive/Economie/Travail/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre")
setwd(Paths[Sys.info()[7]])

#source(".Rprofile")


#us <- load("../data/US_pilot_both_clean_210201.RData")


##### 1. Creation control variables #####

# moved inside "convert" in preparation.


##### 2. Regressions #####

# control_variables, etc. are defined in preparation.R

## Block: Policy views and media consumption
desc_table(dep_vars = c("interested_politics > 0", "member_environmental_orga == 'Yes'", "relative_environmentalist == 'Yes'", "vote_agg > 0"), filename = "pol_views",
           dep.var.labels = c("Interest in politics", "Environmental org. member", "Relative is environmentalist", "Last election: right"),
           nolabel = F,
           dep.var.caption = c("Political views"), data = us, indep_vars = control_variables, indep_labels = cov_lab, mean_control = T
)

desc_table(dep_vars = c("political_affiliation =='Democrat'", "political_affiliation=='Independent'", "political_affiliation=='Republican'"), filename = "pol_positions",
           dep.var.labels = c("Democrat", "Independent", "Republican"),
           dep.var.caption = c("Political affiliations"), data = us, indep_vars = control_variables, indep_labels = cov_lab, mean_control = T
)


## Block: Energy charac.
# Heating
# NB: heating expenses greater than $200
desc_table(dep_vars = c("heating == 'Electricity'", "heating == 'Gas'", "heating == 'Heating oil'", "heating == 5", "heating_expenses >= 225"), filename = "heating",
           dep.var.labels = c("Electricity", "Gas", "Heating oil", "Renewable", "Heating expenses \\textdollar 200+"),
           dep.var.caption = c("At home"), data = us, indep_vars = control_variables, indep_labels = cov_lab)

# Behavior
# NB: gas expenses greater than $125
desc_table(dep_vars = c("gas_expenses >=150", "flights_agg >= 2", "frequency_beef >= 2"), filename = "behavior_GHG",
           dep.var.labels = c("Gas expenses \\textdollar 125+", "Flights (2015-19) 5+ ", "Often eat beef"), mean_above = F,
           dep.var.caption = c("Own household"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

# Transports
desc_table(dep_vars = c("transport_work == 'Car or Motorbike'", "transport_work == 'Public Transport'", "transport_work == 'Walking or Cycling'", 
                        "transport_shopping == 'Car or Motorbike'", "transport_shopping == 'Public Transport'", "transport_shopping == 'Walking or Cycling'", 
                        "transport_leisure == 'Car or Motorbike'", "transport_leisure == 'Public Transport'", "transport_leisure == 'Walking or Cycling'"), filename = "transports",
           dep.var.labels = c("Car/Bike (work)", "Public (work)", "Bicycle/Walk (work)", "Car/Bike (shop)", "Public (shop)", "Bicycle/Walk (shop)","Car/Bike (leisure)", "Public (leisure)", "Bicycle/Walk (leisure)"),
           dep.var.caption = c("Transports"), data = us, indep_vars = c(control_variables, 'availability_transport < 0'), indep_labels = c(cov_lab, 'PT not available'))


## Block: CC (knowledge)

## Talks about CC
desc_table(dep_vars = c("CC_talks=='Never'", "CC_talks=='Yearly'", "CC_talks=='Monthly'"), filename = "CC_talks",
           dep.var.labels = c("Never","Yearly",  "Monthly"),
           nolabel =F,
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, only_mean = T)


## Cause of CC
desc_table(dep_vars = c("CC_real == 'Yes'", "CC_anthropogenic >=1", "CC_problem >=1", "CC_knowledgeable >=1"), filename = "CC_exists",
           dep.var.labels = c("is real", "mostly due to human activity", "important problem", "knowledgeable"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)


## Knowledge of CC
desc_table(dep_vars = c("score_GHG >= 3", "CC_dynamic < 0", "score_CC_impacts >=3"), filename = "CC_knowledge",
           dep.var.labels = c("score GHG", "not sufficient to halve GHG", "score impacts"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

## Score footprint
desc_table(dep_vars = c("score_footprint_transport <= 1", "score_footprint_food <= 1", "score_footprint_elec <= 1", "score_footprint_region <= 1", "score_footprint_pc <= 1"), filename = "CC_footprint",
           dep.var.labels = c("score transport", "score food", "score electricity", "score region emissions", "score per capita emissions"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

## Block: CC (attitudes and risks)

## Responsible party for CC
desc_table(dep_vars = c("responsible_CC_each > 0", "responsible_CC_rich > 0", "responsible_CC_govt > 0", "responsible_CC_companies > 0", "responsible_CC_past > 0"), filename = "responsible_GHG",
           dep.var.labels = c("Each of us","The rich",  "Governments", "Companies", "Previous generations"),
           dep.var.caption = c("Predominantly responsible for CC…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

## Possible to halt CC
desc_table(dep_vars = c("net_zero_feasible >= 1", "CC_affects_self >= 1", "CC_will_end >= 1", "effect_halt_CC_economy >= 1", "effect_halt_CC_lifestyle >= 1"), filename = "CC_stoppable",
           dep.var.labels = c("Technically feasible","Affected personally",  "Halt by end of century", "Positive effects on the economy", "Negative effects personally"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

## Generations most affected
#control_variables_w_treatment_usp <- c("dominant_origin", "female", "children", "college", "employment_agg", "income_factor", "age_agg", "left_right < 0", "left_right > 0", "left_right == 0")
#cov_lab_w_treatment_usp <- c("race: White only", "Female", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 30-49", "age: 50-87", "Left or Very left", "Right or Very right", "Center")
#
#desc_table(dep_vars = c("CC_affected_1960", "CC_affected_1990", "CC_affected_2020", "CC_affected_2050", "CC_affected_none"), filename = "CC_affected",
#           dep.var.labels = c("Born in 1960s", "Born in 1990s", "Born in 2020s", "Born in 2050s", "None of them"),
#           dep.var.caption = c("Generations"), data = usp1, indep_vars = control_variables_w_treatment_usp, indep_labels = cov_lab_w_treatment_usp, only_mean = T)


## Willing to change lifestyle
desc_table(dep_vars = c("willing_limit_flying >= 1", "willing_limit_driving >= 1", "willing_electric_car >= 1", "willing_limit_beef >= 1", "willing_limit_heating >= 1"), filename = "limit_behavior",
           dep.var.labels = c("Limit flying","Limit driving",  "Have an eletric vehicle", "Limit beef consumption", "Limit heating"),
           dep.var.caption = c("Willing to change lifestyle?"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

## Conditions
desc_table(dep_vars = c("condition_ambitious_policies >= 1", "condition_financial_aid >= 1", "condition_people_change >= 1", "condition_rich_change >= 1"), filename = "change_condition",
           dep.var.labels = c("Ambitious policies", "Financial support", "People around changing", "Rich changing"),
           dep.var.caption = c("Important factors"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)


## Block Pref 1: emission standards
desc_table(dep_vars = c("standard_effect_less_emission >= 1", "standard_effect_less_pollution >= 1", "standard_negative_effect >= 1", "standard_large_effect >= 1", "standard_cost_effective >= 1"), filename = "standard_opinion",
           dep.var.labels = c("Reduce car emissions", "Reduce pollution", "Negative effect", "Large effect", "Costly"),
           dep.var.caption = c("Effects of ban on combustion engine"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)
#would win
desc_table(dep_vars = c("standard_win_lose_poor >= 1 ", "standard_win_lose_middle >= 1", "standard_win_lose_rich >= 1", 
                        "standard_win_lose_rural >= 1", "standard_win_lose_self >= 1"), filename = "standard_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Rural", "Own household"),
           dep.var.caption = c("Winners of ban on combustion engine"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would lose
desc_table(dep_vars = c("standard_win_lose_poor < 1 ", "standard_win_lose_middle < 1", "standard_win_lose_rich < 1", 
                        "standard_win_lose_rural < 1", "standard_win_lose_self < 1"), filename = "standard_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Rural", "Own household"),
           dep.var.caption = c("Losers of ban on combustion engine"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# perceptions
desc_table(dep_vars = c("standard_fair >= 1", "standard_support >= 1", "standard_public_transport_support >= 1"), filename = "standard_perception",
           dep.var.labels = c("Fair", "Support", "Support with alternatives"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Pref 2: green investments
desc_table(dep_vars = c("investments_effect_elec_greener >= 1","investments_effect_public_transport >= 1" , "investments_effect_less_pollution >= 1", "investments_negative_effect >= 1", "investments_large_effect >= 1", "investments_cost_effective >= 1"), filename = "investments_opinion",
           dep.var.labels = c("Greener electricity", "More use of public transport","Reduce pollution", "Negative effect", "Large effect", "Costly"),
           dep.var.caption = c("Effects of green infrastructure program"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)
#would win
desc_table(dep_vars = c("investments_win_lose_poor >= 1 ", "investments_win_lose_middle >= 1", "investments_win_lose_rich >= 1", 
                        "investments_win_lose_rural >= 1", "investments_win_lose_self >= 1"), filename = "investments_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Rural", "Own household"),
           dep.var.caption = c("Winners of green infrastructure program"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would lose
desc_table(dep_vars = c("investments_win_lose_poor < 1 ", "investments_win_lose_middle < 1", "investments_win_lose_rich < 1", 
                        "investments_win_lose_rural < 1", "investments_win_lose_self < 1"), filename = "investments_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Rural", "Own household"),
           dep.var.caption = c("Losers of green infrastructure program"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# perceptions
desc_table(dep_vars = c("investments_fair >= 1", "investments_support >= 1"), filename = "investments_perception",
           dep.var.labels = c("Fair", "Support"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# funding
desc_table(dep_vars = c("investments_funding_debt == 1", "investments_funding_sales_tax == 1", "investments_funding_wealth_tax == 1", "investments_funding_less_social == 1", "investments_funding_less_military == 1"), filename = "investments_funding",
           dep.var.labels = c("Public debt", "Sales tax", "Wealth tax", "Reduce social spending", "Reduce military spending"),
           dep.var.caption = c("Appropriate source of funding"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Pref 3: tax and dividend
desc_table(dep_vars = c("tax_transfers_effect_driving >= 1","tax_transfers_effect_insulation >= 1","tax_transfers_effect_less_emission >= 1", "tax_transfers_effect_less_pollution >= 1", "tax_transfers_negative_effect >= 1", "tax_transfers_large_effect >= 1", "tax_transfers_cost_effective >= 1"), filename = "tax_transfer_opinion",
           dep.var.labels = c("Drive less", "Insulate more", "Reduce fossil fuels","Reduce pollution", "Negative effect", "Large effect", "Costly"),
           dep.var.caption = c("Effects of carbon tax with cash transfers"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)
#would win
desc_table(dep_vars = c("tax_transfers_win_lose_poor >= 1 ", "tax_transfers_win_lose_middle >= 1", "tax_transfers_win_lose_rich >= 1", 
                        "tax_transfers_win_lose_rural >= 1", "tax_transfers_win_lose_self >= 1"), filename = "tax_transfer_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Rural", "Own household"),
           dep.var.caption = c("Winners of carbon tax with cash transfers"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would lose
desc_table(dep_vars = c("tax_transfers_win_lose_poor < 1 ", "tax_transfers_win_lose_middle < 1", "tax_transfers_win_lose_rich < 1", 
                        "tax_transfers_win_lose_rural < 1", "tax_transfers_win_lose_self < 1"), filename = "tax_transfer_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Rural", "Own household"),
           dep.var.caption = c("Losers of carbon tax with cash transfers"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T)

# perceptions
desc_table(dep_vars = c("tax_transfers_fair >= 1", "tax_transfers_support >= 1"), filename = "tax_transfer_perception",
           dep.var.labels = c("Fair", "Support"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Pref on climate policies

# Support policies climate
desc_table(dep_vars = c("policy_tax_flying > 0", "policy_tax_fuels > 0", "policy_ban_city_centers > 0", 
                        "policy_subsidies > 0", "policy_climate_fund > 0"), filename = "policy_climate",
           dep.var.labels = c("Tax on flying", "Tax on fossil fuels", "Ban polluting vehicles in city centers", "Technology subsidies", "Global climate fund"),
           dep.var.caption = c("Support"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# Revenues of carbon tax
desc_table(dep_vars = c("tax_transfer_constrained_hh > 0", "tax_transfer_poor > 0", "tax_transfer_all > 0", "tax_rebates_affected_firms > 0", 
                        "tax_reduction_corporate_tax > 0", "tax_reduction_personal_tax > 0", "tax_investments > 0", "tax_subsidies > 0", "tax_reduction_deficit > 0"), filename = "revenue_carbon_tax",
           dep.var.labels = c("Transfer to constrained HH", "Transfers to poorest", "Equal transfers", "Tax rebates for affected firms", "Reduce CIT", "Reduce PIT", "Infrastructure projects", "Technology subsidies", "Reduce deficit"),
           dep.var.caption = c("Support carbon tax if revenues allocated as/to…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)


## WTP
desc_table(dep_vars = c("wtp == 'Yes'"), filename = "wtp",
           dep.var.labels = c("WTP"),
           nolabel=F,
           dep.var.caption = c("WTP to limit global warming to safe levels"), data = us, indep_vars = c(control_variables_w_treatment, "as.factor(wtp_variant)"), indep_labels = c(cov_lab_w_treatment, "WTP 30", "WTP 50", "WTP 100", "WTP 300", "WTP 500", "WTP 1000"), mean_control = T
)

## Donation/Petition
desc_table(dep_vars = c("donation", "petition == 'Yes'"), filename = "altruism",
           dep.var.labels = c("Donation to charity \\textdollar", "Signed petition"),
           dep.var.caption = c("Altruism"), data = us, indep_vars = c(control_variables_w_treatment), indep_labels = c(cov_lab_w_treatment), mean_control = T
)


## Block International burden-sharing

# Level for PP to tackle CC
desc_table(dep_vars = c("scale_local", "scale_state", "scale_federal", "scale_global"), filename = "scale",
           dep.var.labels = c("Local","State", "Federal", "Global"),
           dep.var.caption = c("Policy level"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, only_mean = T)

# US should act
desc_table(dep_vars = c("should_fight_CC > 0", "if_other_do_more > 0", "if_other_do_less > 0"), filename = "country_should_act_condition",
           dep.var.labels = c("fight climate change","be more ambitious, if others more", "be more ambitious, if others less"),
           dep.var.caption = c("U.S. should… "), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)


# Burden
desc_table(dep_vars = c("burden_sharing_income >= 1", "burden_sharing_emissions >= 1", "burden_sharing_cumulative >= 1", "burden_sharing_rich_pay >= 1", "burden_sharing_poor_receive >= 1"), filename = "burden_sharing",
           dep.var.labels = c("Pay in proportion to income","Pay in proportion to current emissions", "Pay in proportion to past emissions (from 1990)", "Richest pay alone", "Richest pay, and even more to help vulnerable countries"),
           dep.var.caption = c("Countries should"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Approve propositions
desc_table(dep_vars = c("global_assembly_support > 0", "global_tax_support > 0", "tax_1p_support > 0"), filename = "support_inter",
           dep.var.labels = c("Global democratic assembly to fight CC", "Global tax on GHG emissions funding a global basic income (\\textdollar 30/month/adult)", "Global tax on top 1\\% to finance poorest countries"),
           dep.var.caption = c("Approve"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)



## Block Housing/Preference for bans vs. incentives

desc_table(dep_vars = c("will_insulate > 0"), filename = "will_insulate",
           dep.var.labels = c("Likely to insulate"),
           dep.var.caption = c(""), data = us[us$home_owner==T,], indep_vars = c(control_variables_w_treatment, "obstacles_insulation_cannot", "obstacles_insulation_cost", "obstacles_insulation_effort", "obstacles_insulation_useless", "obstacles_insulation_satisfactory"), indep_labels = c(cov_lab_w_treatment,"Insulation: not my choice", "Insulation: cost", "Insulation: effort", "Insulation: not efficient", "Insulation: already satisfactory"), mean_control = T
)

desc_table(dep_vars = c("insulation_support > 0"), filename = "pref_thermal",
           dep.var.labels = c("Support thermal renovation if subsidized"),
           dep.var.caption = c(""), data = us, indep_vars = c(control_variables_w_treatment, "insulation_disruption_variant"), indep_labels = c(cov_lab_w_treatment, "Formulation: Costs underlined"), mean_control = T
)

desc_table(dep_vars = c("beef_tax_support > 0", "beef_subsidies_vegetables_support > 0", "beef_subsidies_removal_support > 0", "beef_ban_intensive_support > 0"), filename = "pref_beef",
           dep.var.labels = c("Tax on cattle products (beefx2)", "Subsidies Vegetables", "No subsidies cattle", "Ban intensive cattle"),
           dep.var.caption = c("If gov. limits cattle products, I would support…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)


## Block: Trust, perceptions of institution, inequality, and the future
# Trust
desc_table(dep_vars = c("can_trust_people > 0", "can_trust_govt > 0"), filename = "trust",
           dep.var.labels = c("most people","government to do what is right"),
           dep.var.caption = c("Trust…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Positive views
desc_table(dep_vars = c("view_govt > 0", "problem_inequality > 0", "future_richness <= 0"), filename = "ineq_intervention_future",
           dep.var.labels = c("Active government","Inequality serious problem", "World poorer or same"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)


## Feedback
desc_table(dep_vars = c("survey_biased == 'No'", "survey_biased == 'Yes, right'", "survey_biased == 'Yes, left'"), filename = "survey_biased",
           dep.var.labels = c("No", "Yes, right", "Yes, left"),
           dep.var.caption = c("Biased"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Covid Regressions for Tobias
control_variables_left_right <- c(control_variables[1:7], "left_right_factor")

desc_table(dep_vars = c("standard_support", "investments_support", "tax_transfers_support", "policies_support", "index_main_policies_dummies2SD", "index_all_policies_dummies2SD"), filename = "All/support_w_covid",
           dep.var.labels = c("Support ban of combustion engine", "Support Green Investments", "Support Carbon tax \n with cash transfers", "Support 3 main policies", 
                              "Index Main Policies", "Index All Policies"),
           nolabel = F,
           dep.var.caption = c(""), data = e, indep_vars = c(control_variables_left_right, "as.factor(country)", "urban", "as.factor(hit_by_covid)"),
          indep_labels = c(cov_lab[1:14], "Center", "Left", "Right", "country: Denmark", "country: France", "country: US", "Urban","Hit by COVID"), mean_control = T
)

desc_table(dep_vars = c("standard_support", "investments_support", "tax_transfers_support", "policies_support", "index_main_policies_dummies2SD", "index_all_policies_dummies2SD"), filename = "All/support_w_covid_het_income",
           dep.var.labels = c("Support ban of combustion engine", "Support Green Investments", "Support Carbon tax \n with cash transfers", "Support 3 main policies", 
                              "Index Main Policies", "Index All Policies"),
           nolabel = F,
           dep.var.caption = c(""), data = e, indep_vars = c(control_variables_left_right, "as.factor(country)", "urban", "as.factor(hit_by_covid)", "as.factor(hit_by_covid)*income_factor"),
           indep_labels = c(cov_lab[1:14], "Center", "Left", "Right", "country: Denmark", "country: France", "country: US", "Urban","Hit by COVID",
                            "Income Q2 $\\times$ Hit by COVID", "Income Q3 $\\times$ Hit by COVID", "Income Q4 $\\times$ Hit by COVID"), mean_control = T
)

##### indices ####

## indices
desc_table(dep_vars = c("index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"), filename = "indices",
           dep.var.labels = c("Affected Index", "Knowledge Index", "Knowledge Index (EFA)", "CO$_{2}$ emissions (t/year)"),
           nolabel = T,
           dep.var.caption = c(""), data = us, indep_vars = c(control_variables, "urban"), indep_labels = c(cov_lab, "Core metropolitan"), mean_control = T
)

## TEMPORARY: for data w/o CO2
desc_table(dep_vars = c("index_affected", "index_knowledge", "index_knowledge_efa"), filename = "indices_DK",
           dep.var.labels = c("Affected Index", "Knowledge Index", "Knowledge Index (EFA)"),
           nolabel = F,
           dep.var.caption = c(""), data = dk, indep_vars = c(control_variables, "urban"), indep_labels = c(cov_lab, "Core metropolitan"), mean_control = T
)

## SAME as above
desc_table(dep_vars = "policies_support > 0", 
           dep.var.caption = c("Support"), data = fr, indep_vars = c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa"), 
           filename = "support_all_indices",
           indep_labels = c(cov_lab, "Index affected","Index knowledge", "Index knowledge EFA"), mean_control = T,
           nolabel =F,
           indep_vars_included = list(c(rep(T, length(control_variables)-1),F, F, F, F), c(rep(T, length(control_variables)), F, F, F), c(rep(T, length(control_variables)), T, F, F), c(rep(T, length(control_variables)), F, T, F), c(rep(T, length(control_variables)), F, F, T), c(rep(T, length(control_variables)), T, T, F), c(rep(T, length(control_variables)), T, F, T), c(rep(T, length(control_variables)), T, T, T))
)


# Support with indices
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"), 
           filename = "support_w_indices",
           nolabel= F,
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge"), indep_labels = c(cov_lab, "Index affected", "Index knowledge"), mean_control = T
)

# EFA
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"), 
           filename = "support_w_indices_efa",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge_efa"), indep_labels = c(cov_lab, "Index affected", "Index knowledge EFA"), mean_control = T
)

# Both
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"),
           filename = "support_w_indices_both",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa"), indep_labels = c(cov_lab, "Index affected","Index knowledge", "Index knowledge EFA"), mean_control = T
)

# CO2 emissions
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"),
           filename = "support_w_indices_CO2",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "CO2_emission"), indep_labels = c(cov_lab, "Index affected", "CO$_{2}$ emissions (t/year)"), mean_control = T
)

# CO2 + index
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"),
           filename = "support_w_indices_bothCO2",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge", "CO2_emission"), indep_labels = c(cov_lab, "Index affected","Index knowledge", "CO$_{2}$ emissions (t/year)"), mean_control = T
)

# CO2 + EFA
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"),
           filename = "support_w_indices_CO2_efa",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge_efa", "CO2_emission"), indep_labels = c(cov_lab, "Index affected","Index knowledge EFA", "CO$_{2}$ emissions (t/year)"), mean_control = T
)

# All 3
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"),
           filename = "support_w_indices_all_three",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"), indep_labels = c(cov_lab, "Index affected","Index knowledge", "Index knowledge EFA", "CO$_{2}$ emissions (t/year)"), mean_control = T
)

# Test new desc_table
desc_table(dep_vars = "policies_support > 0", 
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"), 
           filename = "support_all_indices",
           indep_labels = c(cov_lab, "Index affected","Index knowledge", "Index knowledge EFA", "CO$_{2}$ emissions (t/year)"), mean_control = T,
           nolabel =F,
           indep_vars_included = list(c(rep(T, length(control_variables)-1),F, F, F, F, F), c(rep(T, length(control_variables)), F, F, F, F), c(rep(T, length(control_variables)), T, F, F, F), c(rep(T, length(control_variables)), F, T, F, F), c(rep(T, length(control_variables)), F, F, T, F), c(rep(T, length(control_variables)), F, F, F, T), c(rep(T, length(control_variables)), T, T, F, F), c(rep(T, length(control_variables)), T, F, T, F), c(rep(T, length(control_variables)), T, T, T, T))
)

# desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0"), 
#            dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"), 
#            filename = "support_each_index",
#            indep_labels = c(cov_lab, "Index affected","Index knowledge", "Index knowledge EFA", "CO_2 emissions (t/year)"), mean_control = T,
#            nolabel = T,
#            indep_vars_included = list(control_variables, c(control_variables, "index_affected", "index_knowledge"), c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"))
# )

## Plots coefficient as figures
e <- dk
# Need to change names (e.g., _US, _DK, _FR) of files lines 398/400; 409/411; 417/419
# e <- us
# e <- fr


e$availability_transport_dummy <- e$availability_transport >= 0
e$can_trust_govt_dummy <- e$can_trust_govt > 0


# e$Gilets_jaunes_agg <- e$Gilets_jaunes
# e$Gilets_jaunes_agg[e$Gilets_jaunes == "est_dedans"] <- "soutient"

control_variables <- c(control_variables[1:7], "as.factor(vote_agg)")
end_formula <- paste(c(control_variables), collapse = ') + (') #  treatment_climate * treatment_policy
end_formula <- paste(c("(", end_formula), collapse = "")
end_formula <- paste(c(end_formula, ")"), collapse = "")

# Policies support with indices
# end_formula3 <- paste(c(end_formula, "index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"), collapse = ' + ') #  treatment_climate * treatment_policy


models <- list()
models[["Policies support (simple no vote)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(control_variables[1:7]), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (simple)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", end_formula)), data = e, weights = e$weight)
models[["Policies support (affected)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "index_affected"), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (knowledge)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "index_knowledge"), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (knowledge efa)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "index_knowledge_efa"), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (CO2)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "CO2_emission"), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (affected+knowledge)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "index_affected", "index_knowledge"), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (affected+knowledge efa)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "index_affected", "index_knowledge_efa"), collapse = ' + '))), data = e, weights = e$weight)
models[["Policies support (all)"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "index_affected", "index_knowledge", "index_knowledge_efa", "CO2_emission"), collapse = ' + '))), data = e, weights = e$weight)

models[["Policies support"]] <- lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "polluting_sector", "can_trust_govt_dummy", "availability_transport_dummy", "index_affected", "index_knowledge"), collapse = ' + '))), data = e, weights = e$weight)


cov_lab_mod <- c("dominant_originTRUE" = "race/origin: largest group", "femaleTRUE" = "Female", "childrenTRUE" = "Child(ren) at home", "collegeNo college" = "No college",
                 "as.factor(employment_agg)Retired" = "status: Retired" , "as.factor(employment_agg)Student" = "status: Student", 
                 "as.factor(employment_agg)Working" = "status: Working", "income_factorQ2" = "Income Q2", "income_factorQ3" = "Income Q3", 
                 "income_factorQ4" = "Income Q4", "age25-34" = "age: 25-34", "age35-49" = "age: 35-49", "age50-64" = "age: 50-64", 
                 "age65+" = "age: 65+", "vote_aggBiden" = "vote: Biden", "vote_aggTrump" = "vote: Trump", "index_affected" = "Index affected",
                 "index_knowledge" = "Index knowledge")

cov_lab_mod2 <- c("childrenTRUE" = "Child(ren) at home", "collegeNo college" = "No college",
                 "as.factor(employment_agg)Retired" = "status: Retired" , "as.factor(employment_agg)Student" = "status: Student", 
                 "as.factor(employment_agg)Working" = "status: Working", "age25-34" = "age: 25-34", "age35-49" = "age: 35-49", "age50-64" = "age: 50-64", 
                 "age65+" = "age: 65+", "as.factor(vote_agg)Social democracts & Center" = "vote: Social democracts & Center", "as.factor(vote_agg)Right" = "vote: Right",
                 "as.factor(vote_agg)Far right" = "vote: Far right", "as.factor(vote_agg)Trump" = "vote: Trump", "as.factor(vote_agg)Center" = "vote: Center", "as.factor(vote_agg)Right" = "vote: Right", "as.factor(vote_agg)Far right" = "vote: Far right", 
                "can_trust_govt_dummyTRUE" = "Trust government","polluting_sectorTRUE"= "Work in polluting sector",
                "availability_transport_dummyTRUE" = "Public transport available",
                "index_affected" = "Index affected","index_knowledge" = "Index knowledge")
# Run first w/o coef_map to check variables you want to include
# Include in cov_lab_mod, only the variables you want to display. Order matter for order of appearance on the graph
coef_support_indices_US <- modelplot(models, conf_level = .95,
          background = list(geom_vline(xintercept = 0, color = "grey"))) + labs(x = 'Coefficients', y = 'Covariates', title = '')
save_plotly(coef_support_indices_US, width= 736, height=719)

# indices

models <- list()
models[["Affected Index"]] <- lm(as.formula(paste("index_affected ~ ", paste(c(end_formula, "urban"), collapse = ' + '))), data = e, weights = e$weight)
models[["Knowledge Index"]] <- lm(as.formula(paste("index_knowledge ~ ", paste(c(end_formula, "urban"), collapse = ' + '))), data = e, weights = e$weight)
models[["Knowledge Index (EFA)"]] <- lm(as.formula(paste("index_knowledge_efa ~ ", paste(c(end_formula, "urban"), collapse = ' + '))), data = e, weights = e$weight)
# models[["CO2 emissions (t/year)"]] <- lm(as.formula(paste("CO2_emission ~ ", paste(c(end_formula, "urban"), collapse = ' + '))), data = e, weights = e$weight)
coef_indices_FR <- modelplot(models, coef_map = cov_lab_mod, 
                                  background = list(geom_vline(xintercept = 0, color = "grey"))) + labs(x = 'Coefficients', y = 'Covariates', title = 'indices')
save_plotly(coef_indices_FR, width= 736, height=719)

# Right_wing affiliation

models <- list()
models[["Right"]] <- lm(as.formula(paste("left_right > 0 ~ ", paste(c(c(control_variables[1:7]), "urban"), collapse = ' + '))), data = e, weights = e$weight)
coef_Right_FR <- modelplot(models, coef_map = cov_lab_mod, 
                             background = list(geom_vline(xintercept = 0, color = "grey"))) + labs(x = 'Coefficients', y = 'Covariates', title = 'Right-Wing Affiliation')
save_plotly(coef_Right_FR, width= 736, height=719)


# Decomposing Tax Policy
# Do not use index_progressist
indices_list <- c("index_knowledge", "index_affected", "index_concerned_about_CC", "index_worried", "index_positive_economy", "index_constrained",
                  "index_policies_efficient", "index_care_poverty", "index_altruism","index_affected_subjective","index_willing_change", "index_lose_policies_subjective", "index_fairness", "index_trust_govt")


end_formula_treatment <- paste(c(control_variables_w_treatment[c(1:5,7:11)], "as.factor(country)"), collapse = ') + (') #  Do not take left and income 
end_formula_treatment <- paste(c("(", end_formula_treatment), collapse = "")
end_formula_treatment <- paste(c(end_formula_treatment, ")"), collapse = "")

end_formula_treatment_indices <- paste(c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)", indices_list[2:length(indices_list)]), collapse = ') + (') #  Do not take left and income 
end_formula_treatment_indices <- paste(c("(", end_formula_treatment_indices), collapse = "")
end_formula_treatment_indices <- paste(c(end_formula_treatment_indices, ")"), collapse = "")


cov_lab_treatment_mod <- c("dominant_originTRUE" = "race/origin: largest group", "femaleTRUE" = "Female", "childrenTRUE" = "Child(ren) at home", "collegeNo college" = "No college",
                 "as.factor(employment_agg)Retired" = "status: Retired" , "as.factor(employment_agg)Student" = "status: Student", 
                 "as.factor(employment_agg)Working" = "status: Working", "age25-34" = "age: 25-34", "age35-49" = "age: 35-49", "age50-64" = "age: 50-64", 
                 "age65+" = "age: 65+", "left_right <= -1TRUE" = "political: Left", "left_right >= 1TRUE" = "political: Right", "left_right == 0TRUE" = "political: Center",
                 "treatmentClimate" = "treatment: Climate", "treatmentPolicy" = "treatment: Policy", "treatmentBoth" = "treatment: Both")

cov_lab_treatment_indices_mod <- c("index_knowledge" = "Has a good knowledge of climate change", "index_affected" = "Is affected by climate change",
                                   "index_concerned_about_CC" = "Is concerned about climate change", "index_worried" = "Is worried about the future",
                                   "index_positive_economy" = "Climate policies have a positive effect \n on the economy",
                                   "index_constrained" = "Is financially constrained","index_policies_efficient" = "Climate policies are efficient",
                                   "index_care_poverty" = "Cares about poverty and inequalities", "index_altruism" = "Is willing to donate to reforestation project",
                                   "index_affected_subjective" = "Thinks will suffer of climate change", "index_willing_change" = "Is willing to adopt climate friendly behavior",
                                   "index_lose_policies_subjective" = "Thinks will lose from main policies", "index_fairness" = "Thinks main policies are fair", "index_trust_govt" = "Trusts the governement",
                                   "left_right <= -1TRUE" = "political: Left", "left_right >= 1TRUE" = "political: Right", "left_right == 0TRUE" = "political: Center")

models <- list()
models[["Ban on combustion-engine cars Index"]] <- lm(as.formula(paste("index_standard_policy_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + '))), data = e, weights = e$weight)
models[["Carbon tax with cash transfers Index"]] <- lm(as.formula(paste("index_tax_transfers_policy_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + '))), data = e, weights = e$weight)
models[["Green investment program Index"]] <- lm(as.formula(paste("index_investments_policy_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + '))), data = e, weights = e$weight)
models[["Main policies Index"]] <- lm(as.formula(paste("index_main_policies_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + '))), data = e, weights = e$weight)
models[["All climate policies Index"]] <- lm(as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + '))), data = e, weights = e$weight)

coef_policy_views_all <- modelplot(models, coef_map = cov_lab_treatment_mod, 
                             background = list(geom_vline(xintercept = 0, color = "grey"))) + labs(x = 'Coefficients', y = 'Covariates', title = 'Indices')


models <- list()
models[["Ban on combustion-engine cars Index"]] <- lm(as.formula(paste("index_standard_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["Carbon tax with cash transfers Index"]] <- lm(as.formula(paste("index_tax_transfers_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["Green investment program Index"]] <- lm(as.formula(paste("index_investments_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["Main policies Index"]] <- lm(as.formula(paste("index_main_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["All climate policies Index"]] <- lm(as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)

coef_policy_views_indices_all <- modelplot(models, coef_map = cov_lab_treatment_indices_mod, 
                                   background = list(geom_vline(xintercept = 0, color = "grey"))) + labs(x = 'Coefficients', y = 'Views', title = 'Indices')

# 
# 
# ## Plot heterogeneity [DEPRECATED SEE heterogeneity_graph.R]
# models <- list(models_1, models_2, models_3)
# 
# models_1 <- dlply(e, "political_affiliation", function(x) 
#   lm(as.formula(paste("standard_support > 0 ~ ", paste(c("1"), collapse = ' + '))), data = x, weights = x$weight))
# models_1 <- models_1[c(1,5)]
# models_2 <- dlply(e, "political_affiliation", function(x) 
#   lm(as.formula(paste("investments_support > 0 ~ ", paste(c("1"), collapse = ' + '))), data = x, weights = x$weight))
# models_2 <- models_2[c(1,5)]
# names(models_2$Democrat$coefficients) <- "Mean 2"
# names(models_2$Republican$coefficients) <- "Mean 2"
# models <- list()
# # models_3 <- dlply(e, "political_affiliation", function(x) 
# #   lm(as.formula(paste("tax_transfers_support > 0 ~ ", paste(c("1"), collapse = ' + '))), data = x, weights = x$weight))
# # models_3 <- models_3[c(1,5)]
# 
# 
# modelplot(models_1) + labs(x = 'Support', y = 'Policies', title = 'Support by political affiliation')

# Yellow Vests
all_controls <- c("income", "interested_politics", "member_environmental_orga", "left_right", "gender", "urban", "urbanity", "age", "diploma", "employment_status", "polluting_sector", "region", "HH_size", "Nb_children__14", "children", "couple", "marital_status", "heating", "gas_expenses", "heating_expenses", "availability_transport", "transport_work", "transport_leisure")
desc_table(dep_vars = c("tax_transfers_support > 0", "tax_transfers_support >= 0"), filename = "FR/yellow_vests_tax_transfers",
           dep.var.labels = c("Approval", "Acceptance"), nolabel = T,
           dep.var.caption = c("Tax with cash transfers"), data = fr, indep_vars = c("Gilets_jaunes", 'index_knowledge_efa', "(CC_problem == 2)", all_controls))
desc_table(dep_vars = c("CC_anthropogenic > 0", "index_knowledge_efa", "CC_problem == 2"), filename = "FR/yellow_vests_climate",
           dep.var.labels = c("CC is anthropogenic", "Knowledge about CC", "CC an important problem"), nolabel = T,
           data = fr, indep_vars = c("Gilets_jaunes", all_controls))

summary(lm(as.formula(paste("tax_transfers_support > 0 ~ ", paste(c("Gilets_jaunes", 'index_knowledge_efa', "(CC_problem == 2)", all_controls), collapse = '+'))), data = fr, weights = fr$weight))
summary(lm(as.formula(paste("tax_transfers_support >= 0 ~ ", paste(c("Gilets_jaunes", 'index_knowledge_efa', "(CC_problem == 2)", all_controls), collapse = '+'))), data = fr, weights = fr$weight))
summary(lm(as.formula(paste("CC_anthropogenic > 0 ~ ", paste(c("Gilets_jaunes", all_controls), collapse = '+'))), data = fr, weights = fr$weight))
summary(lm(as.formula(paste("index_knowledge_efa ~ ", paste(c("Gilets_jaunes", all_controls), collapse = '+'))), data = fr, weights = fr$weight))
summary(lm(as.formula(paste("CC_problem == 2 ~ ", paste(c("Gilets_jaunes", all_controls), collapse = '+'))), data = fr, weights = fr$weight))
decrit(fr$effect_halt_CC_economy)
# Insulation support ~ tenant
summary(lm(insulation_support > 0 ~ home_tenant, data = fr, weights = fr$weight))
summary(lm(policies_support > 0 ~ policies_negative_effect > 0, data = fr, weights = fr$weight))
summary(lm(policies_support > 0 ~ effect_halt_CC_economy > 0, data = fr, weights = fr$weight))
summary(lm(policies_support ~ policies_negative_effect, data = fr, weights = fr$weight))
summary(lm(policies_support ~ effect_halt_CC_economy, data = fr, weights = fr$weight))
end_formula
summary(lm(as.formula(paste("policies_support > 0 ~ ", paste(c(end_formula, "polluting_sector", "can_trust_govt_dummy", "availability_transport_dummy", "index_affected", "index_knowledge"), collapse = ' + '))), data = e, weights = e$weight))
