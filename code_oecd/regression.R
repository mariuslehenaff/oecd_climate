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

control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_agg", "vote_dum", "wave")
cov_lab <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 30-49", "age: 50-87", "vote: Biden", "vote: Trump", "wave: Pilote 2")

## Block: Energy charac.
# Heating
desc_table(dep_vars = c("heating == 'Electricity'", "heating == 'Gas'", "heating == 'Heating oil'", "heating == 5"), filename = "heating",
           dep.var.labels = c("Electricity", "Gas", "Heating oil", "Renewable"),
           dep.var.caption = c("At home"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Behavior
desc_table(dep_vars = c("km_driven", "flights", "frequency_beef < 2"), filename = "behavior_GHG",
           dep.var.labels = c("Km driven (2019)", "Flights (2015-19)", "Rarely eat beef"), mean_above = F,
           dep.var.caption = c("Own household"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

# Transports
desc_table(dep_vars = c("transport_work == 'car or motorbike'", "transport_work == 'public transport'", "transport_work == 'walking or cycling'", 
                        "transport_shopping == 'car or motorbike'", "transport_shopping == 'public transport'", "transport_shopping == 'walking or cycling'", 
                        "transport_leisure == 'car or motorbike'", "transport_leisure == 'public transport'", "transport_leisure == 'walking or cycling'"), filename = "transports",
           dep.var.labels = c("Car/Bike (work)", "Public (work)", "Bicycle/Walk (work)", "Car/Bike (shop)", "Public (shop)", "Bicycle/Walk (shop)","Car/Bike (leisure)", "Public (leisure)", "Bicycle/Walk (leisure)"),
           dep.var.caption = c("Transports"), data = us, indep_vars = c(control_variables, 'transport_available <= 0'), indep_labels = c(cov_lab, 'PT not available'), weights = NULL)

## Block: Trust, perceptions of institution, inequality, and the future
# Trust
desc_table(dep_vars = c("trust_people > 5", "trust_govt >= 2", "trust_public_spending >= 2"), filename = "trust",
           dep.var.labels = c("most people","government to do what is right", "government to spend revenue wisely"),
           dep.var.caption = c("Trust…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Positive views
desc_table(dep_vars = c("statist > 3", "inequality_problem <= -1", "future_gdp %in% c(-1,0)"), filename = "ineq_intervention_future",
           dep.var.labels = c("Active government","Inequality serious problem", "World poorer or same"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Environment
desc_table(dep_vars = c("envi =='Useless: collapse'", "envi =='Not a pb: progress'", "envi =='Pro environmental action'", "envi =='Other goals'"), filename = "envi_views",
           dep.var.labels = c("Collapse","Not a problem, progress", "Need sustainable society", "Other goals"),
           dep.var.caption = c("Views"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

## Block: CC (attitudes and risks)

## Cause of CC
desc_table(dep_vars = c("CC_exists == 'Not a reality'", "CC_exists == 'Natural'", "CC_exists == 'Anthropogenic'"), filename = "CC_exists",
           dep.var.labels = c("not a reality","mainly due to natural climate variability", "mainly due to human activity"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)


## Halving GHG
desc_table(dep_vars = c("CC_dynamics == 'No impact'", "CC_dynamics == 'Decrease'", "CC_dynamics == 'Stabilize '", "CC_dynamics == 'Rise more slowly'"), filename = "CC_dynamics",
           dep.var.labels = c("has no impact on temperatures","will decrease temperatures", "will stabilize temperatures", "will increase temperatures, just more slowly"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)


## Comparisons GHG questions
desc_table(dep_vars = c("CC_factor_beef", "CC_factor_nuclear", "CC_factor_car"), filename = "comparisons_GHG",
           dep.var.labels = c("eating beef vs. two servings of pasta","eletricity produced by nuclear power vs. wind turbines",  "commuting by car vs. food waste"),
           dep.var.caption = c("… emits fare more GHG than …"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

## Responsible party for CC
desc_table(dep_vars = c("CC_responsible_each", "CC_responsible_rich", "CC_responsible_govts", "CC_responsible_companies", "CC_responsible_past", "CC_responsible_foreign", "CC_responsible_nature", "CC_responsible_denial"), filename = "responsible_GHG",
           dep.var.labels = c("Each of us","The rich",  "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "Climate change is not a reality"),
           dep.var.caption = c("Predominantly responsible for CC…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Possible to halt CC
desc_table(dep_vars = c("CC_stoppable == 'No influence'", "CC_stoppable == 'Better to adapt'", "CC_stoppable == 'Should but not happening'", "CC_stoppable == 'Policies & awareness will'", "CC_stoppable == 'Progress will suffice'"), filename = "CC_stoppable",
           dep.var.labels = c("Human have no noticeable influence","Better live with CC than try to halt it",  "Should stop emissions, but not going to happen", "Ambitious policies and awareness will succeed", "Technologies and habits will suffice"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Talks about CC
desc_table(dep_vars = c("CC_talks=='Never'", "CC_talks=='Yearly'", "CC_talks=='Monthly'"), filename = "CC_talks",
           dep.var.labels = c("Never","Yearly",  "Monthly"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

## Generations most affected
desc_table(dep_vars = c("CC_affected_1960", "CC_affected_1990", "CC_affected_2020", "CC_affected_2050", "CC_affected_none"), filename = "CC_affected",
           dep.var.labels = c("Born in 1960s", "Born in 1990s", "Born in 2020s", "Born in 2050s", "None of them"),
           dep.var.caption = c("Generations"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

## Sustainable lifestyle
desc_table(dep_vars = c("change_lifestyle == 'Yes'"), filename = "change_lifestyle",
           dep.var.labels = c("Willing to change lifestyle"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

## Willing to change lifestyle
desc_table(dep_vars = c("change_condition_policies", "change_condition_income", "change_condition_all", "change_condition_no_rich", "change_condition_no_selfish", "change_condition_no_denial", "change_condition_already", "change_condition_try"), filename = "change_condition",
           dep.var.labels = c("Yes, if policies in the good direction","Yes, if financial means",  "Yes, if everyone does the same", "No, only rich should", "No, would affect me more than living with CC", "No, CC not a real problem", "Lifestyle already sustainable", "Trying, but trouble to change"),
           dep.var.caption = c("Willing to change lifestyle?"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Effect of policies
desc_table(dep_vars = c("effect_policies_opportunity", "effect_policies_cost", "effect_policies_lifestyle"), filename = "effect_policies",
           dep.var.labels = c("be an opportunity for our economy and improve our lifestyle","be costly, but we would maintain our lifestyle", "require deep change in our lifestyle"),
           dep.var.caption = c("Those policies would…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Issues to address to halt CC
#BP : might need to add this var. but empty for the moment us$kaya_other_choice
desc_table(dep_vars = c("kaya_techno", "kaya_waste", "kaya_wealth", "kaya_overconsumption", "kaya_overpopulation", "kaya_none"), filename = "kaya",
           dep.var.labels = c("Use of technologies that emit GHG", "Level of waste", "High tax transfers of living", "Overconsumption", "Overpopulation", "None of them"),
           dep.var.caption = c("Issues"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

## Block International burden-sharing

# Level for PP to tackle CC
desc_table(dep_vars = c("scale_local", "scale_state", "scale_federal", "scale_global"), filename = "scale",
           dep.var.labels = c("Local","State", "Federal", "Global"),
           dep.var.caption = c("Policy level"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

# Burden
desc_table(dep_vars = c("burden_sharing_income >= 1", "burden_sharing_emissions >= 1", "burden_sharing_cumulative >= 1", "burden_sharing_rich_pay >= 1", "burden_sharing_poor_receive >= 1"), filename = "burden_sharing",
           dep.var.labels = c("Pay in proportion to income","Pay in proportion to current emissions", "Pay in proportion to past emissions (from 1990)", "Richest pay alone", "Richest pay, and even more to help vulnerable countries"),
           dep.var.caption = c("Countries should"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Equal quota
desc_table(dep_vars = c("equal_quota == 'No, more to vulnerable'", "equal_quota == 'Yes'", "equal_quota == 'No, grand-fathering'", "equal_quota == 'No, against restriction'"), filename = "equal_quota",
           dep.var.labels = c("No, should compensate the poorest","Yes", "No, if pollute more more rights", "No, no restrictions of emissions"),
           dep.var.caption = c("In favor of a system of equal GHG quota at individual levels (with monetary compensation and tax)"), data = us, indep_vars = control_variables, indep_labels = cov_lab, only_mean = T)

# US should act
desc_table(dep_vars = c("country_should_act == 'Yes'", "country_should_act == 'Only if international agreement'", "country_should_act == 'No'"), filename = "country_should_act",
           dep.var.labels = c("Yes", "Only if fair international agreement", "No"),
           dep.var.caption = c("U.S. should take measures to fight CC"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Conditions to act
desc_table(dep_vars = c("country_should_act_condition == 'Compensation'", "country_should_act_condition == 'Reciprocity'", "country_should_act_condition == 'Free-riding'"), filename = "country_should_act_condition",
           dep.var.labels = c("U.S. more ambitious, if others less", "U.S. more ambitious, if others as well", "U.S. less ambitious, if others are"),
           dep.var.caption = c("U.S. should… (if other countries do…)"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Approve propositions
desc_table(dep_vars = c("pro_global_assembly == 'Yes'", "pro_global_tax == 'Yes'", "pro_tax_1p == 'Yes'"), filename = "pro_inter",
           dep.var.labels = c("Global democratic assembly to fight CC", "Global tax on GHG emissions funding a global basic income (\\textdollar 30/month/adult)", "Global tax on top 1\\% to finance poorest countries"),
           dep.var.caption = c("Approve"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Post-Treatment
control_variables_w_treatment <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_agg", "vote_dum", "treatment", "wave")
cov_lab_w_treatment <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "staths: Working", "Income Q2", "Income Q3", "Income Q4","age: 30-49", "age: 50-87", "vote: Biden", "vote: Trump", "Both treatments", "Climate treatment only", "Policy treatment only", "wave: Pilote 2")

## Block Pref 1: emission standards
desc_table(dep_vars = c("standard_exists == 'Yes'", "standard_trust == 'Yes'", "standard_effective == 'Yes'", 
                        "standard_employment == 'Positive'", "standard_side_effects == 'Positive'", "standard_support == 'Yes'"), filename = "standard_opinion",
           dep.var.labels = c("Does exist", "Trust federal gov.", "Effective", "Positive impact on jobs", "Positive side effects", "Support"),
           dep.var.caption = c("C02 emission limit for cars policy in the U.S."), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

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

## Block Political views and media consumption
desc_table(dep_vars = c("interest_politics >= 0", "member_environmental_orga == 'Yes'", "relative_environmentalist == 'Yes'"), filename = "pol_views",
           dep.var.labels = c("Interest in politics", "Environmental org. member", "Relative is environmentalist"),
           dep.var.caption = c("Political views"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)


desc_table(dep_vars = c("far_left", "left", "center", "right", "far_right", "liberal", "conservative", "humanist", "patriot", "apolitical", "environmentalist", "feminist"), filename = "pol_positions",
           dep.var.labels = c("Far Left", "Left", "Center", "Right", "Far Right", "Liberal", "Conservative", "Humanist", "Patriot", "Apolitical", "Environmentalist", "Feminist"),
           dep.var.caption = c("Political positions"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

desc_table(dep_vars = c("media == 'TV (private)'", "media == 'TV (public)'", "media == 'Radio'", "media == 'Social media'", "media == 'Print'", "media == 'News websites'"), filename = "media",
           dep.var.labels = c("TV (private)", "TV (public)", "Radio", "Social media", "Print", "News websites"),
           dep.var.caption = c("Media mainly used"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)

## Feedback
desc_table(dep_vars = c("survey_biased == 'No'", "survey_biased %in% c('Yes, right', 'Yes, anti environment')", "survey_biased %in% c('Yes, left', 'Yes, pro environment')"), filename = "survey_biased",
           dep.var.labels = c("No", "Yes, right / anti-environment", "Yes, left / pro environment"),
           dep.var.caption = c("Biased"), data = us, indep_vars = control_variables_w_treatment, indep_labels = cov_lab_w_treatment, mean_control = T
)
