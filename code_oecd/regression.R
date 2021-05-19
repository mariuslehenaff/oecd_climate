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
           dep.var.labels = c("Electricity", "Gas", "Heating oil", "Renewable", "Heating expenses \$200\+"),
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
desc_table(dep_vars = c("net_zero_feasible >= 1'", "CC_affects_self >= 1", "CC_will_end >= 1", "effect_halt_CC_economy >= 1", "effect_halt_CC_lifestyle >= 1"), filename = "CC_stoppable",
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
desc_table(dep_vars = c("standard_effect_less_emission >= 1", "standard_effect_less_emission >= 1", "standard_negative_effect >= 1", "standard_large_effect >= 1", "standard_cost_effective >= 1"), filename = "standard_opinion",
           dep.var.labels = c("Car emissions", "Pollution", "Negative effect", "Large effect", "Costly"),
           dep.var.caption = c("C02 emission limit for cars policy in the U.S."), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)
#TODO BP
#would win
desc_table(dep_vars = c("standard_incidence_poor == 'Win'", "standard_incidence_middle == 'Win'", "standard_incidence_rich == 'Win'", 
                        "standard_incidence_urban == 'Win'", "standard_incidence_rural == 'Win'", "standard_incidence_self == 'Win'"), filename = "standard_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Winners of emission limits for cars policy"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would lose
desc_table(dep_vars = c("standard_incidence_poor == 'Lose'", "standard_incidence_middle == 'Lose'", "standard_incidence_rich == 'Lose'", 
                        "standard_incidence_urban == 'Lose'", "standard_incidence_rural == 'Lose'", "standard_incidence_self == 'Lose'"), filename = "standard_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Losers of emission limits for cars policy"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Pref 2: green investments
desc_table(dep_vars = c("investments_trust == 'Yes'", "investments_effective == 'Yes'", 
                        "investments_employment == 'Positive'", "investments_side_effects == 'Positive'", "investments_support == 'Yes'"), filename = "investments_opinion",
           dep.var.labels = c("Trust federal gov.", "Effective", "Positive impact on jobs", "Positive side effects", "Support"),
           dep.var.caption = c("Public investment program in green infrastructures for the U.S."), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would win
desc_table(dep_vars = c("investments_incidence_poor == 'Win'", "investments_incidence_middle == 'Win'", "investments_incidence_rich == 'Win'", 
                        "investments_incidence_urban == 'Win'", "investments_incidence_rural == 'Win'", "investments_incidence_self == 'Win'"), filename = "investments_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Winners of green investments"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would lose
desc_table(dep_vars = c("investments_incidence_poor == 'Lose'", "investments_incidence_middle == 'Lose'", "investments_incidence_rich == 'Lose'", 
                        "investments_incidence_urban == 'Lose'", "investments_incidence_rural == 'Lose'", "investments_incidence_self == 'Lose'"), filename = "investments_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Losers of green investments"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Pref 3: tax and dividend
desc_table(dep_vars = c("tax_transfers_trust == 'Yes'", "tax_transfers_effective == 'Yes'", 
                        "tax_transfers_employment == 'Positive'", "tax_transfers_side_effects == 'Positive'", "tax_transfers_support == 'Yes'"), filename = "tax_transfers_opinion",
           dep.var.labels = c("Trust federal gov.", "Effective", "Positive impact on jobs", "Positive side effects", "Support"),
           dep.var.caption = c("Carbon tax with cash transfers"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would win
desc_table(dep_vars = c("tax_transfers_incidence_poor == 'Win'", "tax_transfers_incidence_middle == 'Win'", "tax_transfers_incidence_rich == 'Win'", 
                        "tax_transfers_incidence_urban == 'Win'", "tax_transfers_incidence_rural == 'Win'", "tax_transfers_incidence_self == 'Win'"), filename = "tax_transfers_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Winners of carbon tax with cash transfers of \\textdollar 600/year/adult"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

#would lose
desc_table(dep_vars = c("tax_transfers_incidence_poor == 'Lose'", "tax_transfers_incidence_middle == 'Lose'", "tax_transfers_incidence_rich == 'Lose'", 
                        "tax_transfers_incidence_urban == 'Lose'", "tax_transfers_incidence_rural == 'Lose'", "tax_transfers_incidence_self == 'Lose'"), filename = "tax_transfers_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Losers of carbon tax with cash transfers of \\textdollar 600/year/adult"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Pref on climate policies
desc_table(dep_vars = c("CC_worries >= 0"), filename = "CC_worries", dep.var.labels = c("Worried"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# Support policies climate
desc_table(dep_vars = c("policy_tax_flying > 0", "policy_tax_fuels > 0", "policy_insulation > 0", 
                        "policy_ban_city_centers > 0", "policy_subsidies > 0", "policy_climate_fund > 0"), filename = "policy_climate",
           dep.var.labels = c("Tax on flying", "Tax on fossil fuels", "Thermal renovation", "Ban polluting vehicles in city centers", "Subsidies", "Global climate fund"),
           dep.var.caption = c("Climate policies"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# Revenues of carbon tax
desc_table(dep_vars = c("tax_transfer_constrained_hh > 0", "tax_transfer_poor > 0", "tax_transfer_all > 0", "tax_rebates_affected_firms > 0", 
                        "tax_investments > 0", "tax_subsidies > 0", "tax_reduction_deficit > 0" , "tax_reduction_corporate_tax > 0", "tax_reduction_personal_tax > 0"), filename = "revenue_carbon_tax",
           dep.var.labels = c("Transfer to constrained HH", "Transfers to poorest", "Equal transfers", "Tax rebates for affected firms", "Infrastructure projects", "Technology subsidies", "Reduce deficit", "Reduce CIT", "Reduce PIT"),
           dep.var.caption = c("Support carbon tax if revenues allocated as/to…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Block Preference for bans vs. incentives
desc_table(dep_vars = c("insulation_compulsory =='Mandatory'", "insulation_compulsory =='Voluntary'"), filename = "pref_thermal",
           dep.var.labels = c("made mandatory", "on a voluntary basis"),
           dep.var.caption = c("Thermal renovation should be (if subsidized)…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

# desc_table(dep_vars = c("flight_quota_1000km == 'Tradable'", "flight_quota_1000km == 'Rationing'", "flight_quota_1000km_global == 'Tradable'", "flight_quota_1000km_global == 'Rationing'", "flight_quota_one_trip == 'Tradable'", "flight_quota_one_trip == 'Rationing'"), filename = "pref_flight",
#            dep.var.labels = c("Rationing (1000km)", "Tradable (1000km)", "Rationing (1000km global)", "Tradable (1000km global)", "Rationing (0.5 round-trip/year)", "Tradable (0.5 round-trip/year)"),
#            dep.var.caption = c("Limit to flight trips"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
# )
desc_table(dep_vars = c("flight_quota_1000km == 'Tradable'", "flight_quota_1000km == 'Rationing'", "flight_quota_one_trip == 'Tradable'", "flight_quota_one_trip == 'Rationing'"), filename = "pref_flight",
           dep.var.labels = c("Rationing (1000km)", "Tradable (1000km)", "Rationing (0.5 round-trip/year)", "Tradable (0.5 round-trip/year)"),
           dep.var.caption = c("Limit to flight trips"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

desc_table(dep_vars = c("beef_tax", "beef_subsidies_vegetables", "beef_subsidies_removal", "beef_ban_intensive"), filename = "pref_beef",
           dep.var.labels = c("Tax on cattle products (beefx2)", "Sub. Vegetables", "No sub. cattle", "Ban intensive cattle"),
           dep.var.caption = c("If gov. limits cattle products, I would approve…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

desc_table(dep_vars = c("ban_incentives == 'Force'", "ban_incentives == 'Encourage'"), filename = "pref_incentives",
           dep.var.labels = c("Force people", "Encourage people"),
           dep.var.caption = c("Government should…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## WTP
desc_table(dep_vars = c("wtp"), filename = "wtp",
           dep.var.labels = c("WTP (\\textdollar  a year)"),
           dep.var.caption = c("WTP to limit global warming to safe levels"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)


## Block International burden-sharing

# Level for PP to tackle CC
desc_table(dep_vars = c("scale_local", "scale_state", "scale_federal", "scale_global"), filename = "scale",
           dep.var.labels = c("Local","State", "Federal", "Global"),
           dep.var.caption = c("Policy level"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, only_mean = T)

# Burden
desc_table(dep_vars = c("burden_sharing_income >= 1", "burden_sharing_emissions >= 1", "burden_sharing_cumulative >= 1", "burden_sharing_rich_pay >= 1", "burden_sharing_poor_receive >= 1"), filename = "burden_sharing",
           dep.var.labels = c("Pay in proportion to income","Pay in proportion to current emissions", "Pay in proportion to past emissions (from 1990)", "Richest pay alone", "Richest pay, and even more to help vulnerable countries"),
           dep.var.caption = c("Countries should"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Equal quota
desc_table(dep_vars = c("equal_quota == 'No, more to vulnerable'", "equal_quota == 'Yes'", "equal_quota == 'No, grand-fathering'", "equal_quota == 'No, against restriction'"), filename = "equal_quota",
           dep.var.labels = c("No, should compensate the poorest","Yes", "No, if pollute more more rights", "No, no restrictions of emissions"),
           dep.var.caption = c("In favor of a system of equal GHG quota at individual levels (with monetary compensation and tax)"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, only_mean = T)

# US should act
desc_table(dep_vars = c("country_should_act == 'Yes'", "country_should_act == 'Only if international agreement'", "country_should_act == 'No'"), filename = "country_should_act",
           dep.var.labels = c("Yes", "Only if fair international agreement", "No"),
           dep.var.caption = c("U.S. should take measures to fight CC"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Conditions to act
desc_table(dep_vars = c("country_should_act_condition == 'Compensation'", "country_should_act_condition == 'Reciprocity'", "country_should_act_condition == 'Free-riding'"), filename = "country_should_act_condition",
           dep.var.labels = c("U.S. more ambitious, if others less", "U.S. more ambitious, if others as well", "U.S. less ambitious, if others are"),
           dep.var.caption = c("U.S. should… (if other countries do…)"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Approve propositions
desc_table(dep_vars = c("pro_global_assembly == 'Yes'", "pro_global_tax == 'Yes'", "pro_tax_1p == 'Yes'"), filename = "pro_inter",
           dep.var.labels = c("Global democratic assembly to fight CC", "Global tax on GHG emissions funding a global basic income (\\textdollar 30/month/adult)", "Global tax on top 1\\% to finance poorest countries"),
           dep.var.caption = c("Approve"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)



## Block: Trust, perceptions of institution, inequality, and the future
# Trust
desc_table(dep_vars = c("trust_people > 5", "trust_govt >= 2", "trust_public_spending >= 2"), filename = "trust",
           dep.var.labels = c("most people","government to do what is right", "government to spend revenue wisely"),
           dep.var.caption = c("Trust…"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Positive views
desc_table(dep_vars = c("statist > 3", "inequality_problem <= -1", "future_gdp %in% c(-1,0)"), filename = "ineq_intervention_future",
           dep.var.labels = c("Active government","Inequality serious problem", "World poorer or same"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment)

# Environment
desc_table(dep_vars = c("envi =='Useless: collapse'", "envi =='Not a pb: progress'", "envi =='Pro environmental action'", "envi =='Other goals'"), filename = "envi_views",
           dep.var.labels = c("Collapse","Not a problem, progress", "Need sustainable society", "Other goals"),
           dep.var.caption = c("Views"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, only_mean = T)


## Feedback
desc_table(dep_vars = c("survey_biased == 'No'", "survey_biased %in% c('Yes, right', 'Yes, anti environment')", "survey_biased %in% c('Yes, left', 'Yes, pro environment')"), filename = "survey_biased",
           dep.var.labels = c("No", "Yes, right / anti-environment", "Yes, left / pro environment"),
           dep.var.caption = c("Biased"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)
