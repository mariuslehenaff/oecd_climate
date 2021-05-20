library(haven)
library(xtable)
library(stargazer)
library(clipr)


Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Google Drive/Economie/Travail/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre")
setwd(Paths[Sys.info()[7]])

#source(".Rprofile")


#us <- load("../data/US_pilot_both_clean_210201.RData")


##### 1. Creation control variables #####

# moved inside "convert" in preparation.


##### 2. Regressions #####

control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_quota", "vote_dum")
cov_lab <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 25-34", "age: 35-49", "age: 50-64", "age: 64+", "vote: Biden", "vote: Trump")

## Block: Policy views and media consumption
desc_table(dep_vars = c("interested_politics > 0", "member_environmental_orga == 'Yes'", "relative_environmentalist == 'Yes'", "liberal_conservative >=1"), filename = "pol_views",
           dep.var.labels = c("Interest in politics", "Environmental org. member", "Relative is environmentalist", "Econ. conservative"),
           dep.var.caption = c("Political views"), data = us, indep_vars = control_variables, indep_labels = cov_lab, mean_control = T
)


desc_table(dep_vars = c("political_affiliation =='Democrat'", "political_affiliation=='Independent'", "political_affiliation=='Republican'"), filename = "pol_positions",
           dep.var.labels = c("Democrat", "Independent"),
           dep.var.caption = c("Political affiliations"), data = us, indep_vars = control_variables, indep_labels = cov_lab, mean_control = T
)


## Block: Energy charac.
# Heating
# NB: heating expenses greater than $200
desc_table(dep_vars = c("heating == 'Electricity'", "heating == 'Gas'", "heating == 'Heating oil'", "heating == 5", "heating_expenses >= 225"), filename = "heating",
           dep.var.labels = c("Electricity", "Gas", "Heating oil", "Renewable", "Heating expenses \\$200\\+"),
           dep.var.caption = c("At home"), data = us, indep_vars = control_variables, indep_labels = cov_lab)

# Behavior
# NB: gas expenses greater than $125
desc_table(dep_vars = c("gas_expenses >=150", "flights_agg >= 2", "frequency_beef >= 2"), filename = "behavior_GHG",
           dep.var.labels = c("Gas expenses \\$125\\+", "Flights (2015-19) 5\\+ ", "Often eat beef"), mean_above = F,
           dep.var.caption = c("Own household"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

# Transports
desc_table(dep_vars = c("transport_work == 'Car or Motorbike'", "transport_work == 'Public Transport'", "transport_work == 'Walking or Cycling'", 
                        "transport_shopping == 'Car or Motorbike'", "transport_shopping == 'Public Transport'", "transport_shopping == 'Walking or Cycling'", 
                        "transport_leisure == 'Car or Motorbike'", "transport_leisure == 'Public Transport'", "transport_leisure == 'Walking or Cycling'"), filename = "transports",
           dep.var.labels = c("Car/Bike (work)", "Public (work)", "Bicycle/Walk (work)", "Car/Bike (shop)", "Public (shop)", "Bicycle/Walk (shop)","Car/Bike (leisure)", "Public (leisure)", "Bicycle/Walk (leisure)"),
           dep.var.caption = c("Transports"), data = us, indep_vars = c(control_variables, 'availability_transport <= 0'), indep_labels = c(cov_lab, 'PT not available'))


## Post-Treatment
control_variables_w_treatment <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_quota", "vote_dum", "treatment")
cov_lab_w_treatment <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 25-34", "age: 35-49", "age: 50-64", "age: 64+", "vote: Biden", "vote: Trump", "Climate treatment only", "Policy treatment only", "Both treatments")


## Block: CC (knowledge)

## Talks about CC
desc_table(dep_vars = c("CC_talks=='Never'", "CC_talks=='Yearly'", "CC_talks=='Monthly'"), filename = "CC_talks",
           dep.var.labels = c("Never","Yearly",  "Monthly"),
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
#control_variables_w_treatment_usp <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_agg", "vote_dum")
#cov_lab_w_treatment_usp <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 30-49", "age: 50-87", "vote: Biden", "vote: Trump")
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
desc_table(dep_vars = c("investments_funding_debt == 1", "investments_funding_sales_tax == 1", "investments_funding_wealth_tax == 1", "investments_funding_less_social == 1", "investments_funding_less_military == 1"), filename = "investments_perception",
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
           dep.var.labels = c("Tax on flying", "Tax on fossil fuels", "Ban polluting vehicles in city centers", "Subsidies", "Global climate fund"),
           dep.var.caption = c("Climate policies"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
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
           dep.var.caption = c("WTP to limit global warming to safe levels"), data = us, indep_vars = c(control_variables_w_treatment, "as.factor(wtp_variant)"), indep_labels = c(cov_lab_w_treatment, "WTP 30", "WTP 50", "WTP 100", "WTP 300", "WTP 500", "WTP 1000"), only_mean = T
)

## Donation/Petition
desc_table(dep_vars = c("donation", "petition == 1"), filename = "altruism",
           dep.var.labels = c("Donation to charity \\$", "Signed petition"),
           dep.var.caption = c("Altruism"), data = us, indep_vars = c(control_variables_w_treatment), indep_labels = c(cov_lab_w_treatment), only_mean = T
)


## Block International burden-sharing

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
           dep.var.labels = c("Global democratic assembly to fight CC", "Global tax on GHG emissions funding a global basic income (\\$ 30/month/adult)", "Global tax on top 1\\% to finance poorest countries"),
           dep.var.caption = c("Approve"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)



## Block Housing/Preference for bans vs. incentives

desc_table(dep_vars = c("will_insulate > 0"), filename = "pref_thermal",
           dep.var.labels = c("Likely to insulate"),
           dep.var.caption = c(""), data = us, indep_vars = c(control_variables_w_treatment, "obstacles_insulation_cannot", "obstacles_insulation_cost", "obstacles_insulation_effort", "obstacles_insulation_useless", "obstacles_insulation_satisfactory"), indep_labels = c(cov_lab_w_treatment,"Insulation: not my choice", "Insulation: cost", "Insulation: effort", "Insulation: not efficient", "Insulation: already satisfactory"), mean_control = T
)

desc_table(dep_vars = c("insulation_mandatory_support_no_priming > 0", "insulation_mandatory_support_priming > 0"), filename = "pref_thermal",
           dep.var.labels = c("Costs not mentioned", "Costs mentioned"),
           dep.var.caption = c("Support thermal renovation if subsidized"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
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



#####

## Indexes
desc_table(dep_vars = c("index_affected", "index_knowledge"), filename = "indexes",
           dep.var.labels = c("Affected Index", "Knowledge Index"),
           dep.var.caption = c(""), data = us, indep_vars = c(control_variables, "core_metropolitan == 1"), indep_labels = c(cov_lab_w_treatment, "Core metropolitan"), mean_control = T
)

# Support with indexes
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"
), filename = "support_w_indexes",
           dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
           dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge"), indep_labels = c(cov_lab_w_treatment, "Index affected", "Index knowledge"), mean_control = T
)

# EFA
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"
), filename = "support_w_indexes_efa",
dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge_efa"), indep_labels = c(cov_lab_w_treatment, "Index affected", "Index knowledge EFA"), mean_control = T
)

# Both
desc_table(dep_vars = c("standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"
), filename = "support_w_indexes_both",
dep.var.labels = c("Ban on combustion engine", "Ban on combustion engine with alternatives", "Green infrastructure program", "Carbon tax with cash transfers"),
dep.var.caption = c("Support"), data = us, indep_vars = c(control_variables, "index_affected", "index_knowledge", "index_knowledge_efa"), indep_labels = c(cov_lab_w_treatment, "Index affected","Index knowledge", "Index knowledge EFA"), mean_control = T
)
