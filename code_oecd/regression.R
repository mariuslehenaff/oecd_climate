library(haven)
library(xtable)
library(stargazer)
library(clipr)


Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Google Drive/Economie/Travail/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre")
setwd(Paths[Sys.info()[7]])

#source(".Rprofile")


us <- readRDS("../data/US_pilot_clean_210115.rds")


##### 1. Creation control variables #####

# moved inside "convert" in preparation.


##### 2. Regressions #####

control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_agg", "vote_dum")
cov_lab = c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 30-49", "age: 50-87", "vote: Biden", "vote: Trump")

desc_table <- function(dep_vars, filename = NULL, data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL,
                       save_folder = "../tables/", dep.var.labels = dep_vars, dep.var.caption = NULL, digits= 3, mean_control = FALSE,
                       mean_above = T, only_mean = F) {
  models <- list()
  means <- c()
  for (i in seq_along(dep_vars)) {
    models[[i]] <- lm(as.formula(paste(dep_vars[i], "~", paste(indep_vars, collapse = '+'))), data = data, weights = weights)
    if (mean_control==FALSE){
     means[i] <- round(mean(data[[dep_vars[i]]], na.rm = T), d = digits)
     mean_text <- "Mean"
    } else {
     means[i] <- round(mean(data.matrix(data[data$treatment_agg=="None", dep_vars[i]]), na.rm = T), d = digits)
     mean_text <- "Control group mean"      
    }
  }
  if (missing(filename)) file_path <- NULL
  else file_path <- paste(save_folder, filename, ".tex", sep="")
  if (only_mean) mean_above <- T
  if (mean_above) { 
    table <- do.call(stargazer, c(models,
                       list(out=NULL, header=F, model.numbers = F,
                            covariate.labels = indep_labels, add.lines = list(c(mean_text, means)),
                            dep.var.labels = dep.var.labels,
                            dep.var.caption = dep.var.caption,
                            multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n" #, omit.stat = c("n")
                       )))
    mean_line <- regmatches(table, regexpr('(Mean|Control group mean) &[^\\]*', table))
    if (only_mean) {
      table <- write_clip(gsub(paste(indep_labels[1], ".*"), paste(mean_line, '\\\\\\\\'), table), collapse=' ')
      table <- table[c(1:grep('(Mean|Control group mean) &[^\\]*', table)[1], (length(table)-3):length(table))]
    } else table <- write_clip(gsub(indep_labels[1], paste(mean_line, '\\\\\\\\ \\\\hline \\\\\\\\[-1.8ex]', indep_labels[1]), 
                             gsub('(Mean|Control group mean) &.*', '', table)), collapse=' ')
    cat(paste(table, collapse="\n"), file = file_path)
  } else table <- do.call(stargazer, c(models,
                                list(out=file_path, header=F,
                                     covariate.labels = indep_labels, add.lines =list(c(mean_text, means)),
                                     dep.var.labels = dep.var.labels,
                                     dep.var.caption = dep.var.caption,
                                     multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n"
                                )))
  return(table)
}


## Block: Energy charac.
# Heating
us$heating_elec <- (us$heating == 1)
us$heating_gas <- (us$heating == 2)
us$heating_oil <- (us$heating == 3)
us$heating_renew <- (us$heating == 5)
desc_table(dep_vars = c("heating_elec", "heating_gas", "heating_oil", "heating_renew"), filename = "heating",
           dep.var.labels = c("Electricity", "Gas", "Heating oil", "Renewable"),
           dep.var.caption = c("At home"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Behavior
us$dummy_frequency_beef <- (us$frequency_beef < 2)
temp <- desc_table(dep_vars = c("km_driven", "flights", "dummy_frequency_beef"), filename = "behavior_GHG",
           dep.var.labels = c("Km driven (2019)", "Flights (2015-19)", "Rarely eat beef"), mean_above = F,
           dep.var.caption = c("Own household"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

# Transports
us$transport_work_car <- (us$transport_work == "car or motorbike")
us$transport_work_PT <- (us$transport_work == "public transport")
us$transport_work_walk <- (us$transport_work == "walking or cycling")
us$transport_shopping_car <- (us$transport_shopping == "car or motorbike")
us$transport_shopping_PT <- (us$transport_shopping == "public transport")
us$transport_shopping_walk <- (us$transport_shopping == "walking or cycling")
us$transport_leisure_car <- (us$transport_leisure == "car or motorbike")
us$transport_leisure_PT <- (us$transport_leisure == "public transport")
us$transport_leisure_walk <- (us$transport_leisure == "walking or cycling")
us$transport_available_not <- (us$transport_available <= 0)
control_transport <- append(control_variables, "transport_available_not")
cov_lab_transport <- append(cov_lab, "PT not available")

desc_table(dep_vars = c("transport_work_car", "transport_work_PT", "transport_work_walk", "transport_shopping_car", "transport_shopping_PT", "transport_shopping_walk", "transport_leisure_car", "transport_leisure_PT", "transport_leisure_walk"), filename = "transports",
           dep.var.labels = c("Car/Bike (work)", "Public (work)", "Bicycle/Walk (work)", "Car/Bike (shop)", "Public (shop)", "Bicycle/Walk (shop)","Car/Bike (leisure)", "Public (leisure)", "Bicycle/Walk (leisure)"),
           dep.var.caption = c("Transports"), data = us, indep_vars = control_transport, indep_labels = cov_lab_transport, weights = NULL)

## Block: Trust, perceptions of institution, inequality, and the future
# Trust
us$dummy_trust_people <- (us$trust_people > 5)
us$dummy_trust_govt <- (us$trust_govt >= 2)
us$dummy_trust_public_spending <- (us$trust_public_spending >= 2)

desc_table(dep_vars = c("dummy_trust_people", "dummy_trust_govt", "dummy_trust_public_spending"), filename = "trust",
           dep.var.labels = c("most people","government to do what is right", "government to spend revenue wisely"),
           dep.var.caption = c("Trust…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Positive views
us$dummy_statist <- (us$statist > 3)
us$dummy_inequality_problem <- (us$inequality_problem <= -1)
us$dummy_future_gdp <- (us$future_gdp == -1 | us$future_gdp == 0)

desc_table(dep_vars = c("dummy_statist", "dummy_inequality_problem", "dummy_future_gdp"), filename = "ineq_intervention_future",
           dep.var.labels = c("Active government","Inequality serious problem", "World poorer or same"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Environment
us$envi_collapse <- (us$envi =="Useless: collapse")
us$envi_progress <- (us$envi =="Not a pb: progress")
us$envi_pro <- (us$envi =="Pro environmental action")
us$envi_other <- (us$envi =="Other goals")

desc_table(dep_vars = c("envi_collapse", "envi_progress", "envi_pro", "envi_other"), filename = "envi_views",
           dep.var.labels = c("Collapse","Not a problem, progress", "Need sustainable society", "Other goals"),
           dep.var.caption = c("Views"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

## Block: CC (attitudes and risks)

## Cause of CC
us$CC_exists_no <- (us$CC_exists == -1)
us$CC_exists_nat <- (us$CC_exists == 0)
us$CC_exists_anthro <- (us$CC_exists == 1)

desc_table(dep_vars = c("CC_exists_no", "CC_exists_nat", "CC_exists_anthro"), filename = "CC_exists",
           dep.var.labels = c("not a reality","mainly due to natural climate variability", "mainly due to human activity"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)


## Halving GHG
us$CC_dynamics_no <- (us$CC_dynamics == -2)
us$CC_dynamics_dec <- (us$CC_dynamics == -1)
us$CC_dynamics_stab <- (us$CC_dynamics == 0)
us$CC_dynamics_rise <- (us$CC_dynamics == 1)

desc_table(dep_vars = c("CC_dynamics_no", "CC_dynamics_dec", "CC_dynamics_stab", "CC_dynamics_rise"), filename = "CC_dynamics",
           dep.var.labels = c("has no impact on temperatures","will decrease temperatures", "will stabilize temperatures", "will increase temperatures, just more slowly"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)


## Comparisons GHG questions
desc_table(dep_vars = c("CC_factor_beef", "CC_factor_nuclear", "CC_factor_car"), filename = "comparisons_GHG",
           dep.var.labels = c("eating beef vs. two servings of pasta","eletricity produced by nuclear power vs. wind turbines",  "commuting by car vs. food waste"),
           dep.var.caption = c("… emits fare more GHG than …"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

## Responsible party for CC
desc_table(dep_vars = c("CC_responsible_each", "CC_responsible_rich", "CC_responsible_govts", "CC_responsible_companies", "CC_responsible_past", "CC_responsible_foreign", "CC_responsible_nature", "CC_responsible_denial"), filename = "responsible_GHG",
           dep.var.labels = c("Each of us","The rich",  "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "Climate change is not a reality"),
           dep.var.caption = c("Predominantly responsible for CC…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Possible to halt CC
us$CC_stoppable_no_infl <- (us$CC_stoppable == "No influence")
us$CC_stoppable_adapt <- (us$CC_stoppable == "Better to adapt")
us$CC_stoppable_should <- (us$CC_stoppable == "Should but not happening")
us$CC_stoppable_policies <- (us$CC_stoppable == "Policies & awareness will")
us$CC_stoppable_progress <- (us$CC_stoppable == "Progress will suffice")

desc_table(dep_vars = c("CC_stoppable_no_infl", "CC_stoppable_adapt", "CC_stoppable_should", "CC_stoppable_policies", "CC_stoppable_progress"), filename = "CC_stoppable",
           dep.var.labels = c("Human have no noticeable influence","Better live with CC than try to halt it",  "Should stop emissions, but not going to happen", "Ambitious policies and awareness will succeed", "Technologies and habits will suffice"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Talks about CC
us$CC_talks_never <- (us$CC_talks == -1)
us$CC_talks_yearly <- (us$CC_talks == 0)
us$CC_talks_monthly <- (us$CC_talks == 1)

desc_table(dep_vars = c("CC_talks_never", "CC_talks_yearly", "CC_talks_monthly"), filename = "CC_talks",
           dep.var.labels = c("Never","Yearly",  "Monthly"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

## Generations most affected
desc_table(dep_vars = c("CC_affected_1960", "CC_affected_1990", "CC_affected_2020", "CC_affected_2050", "CC_affected_none"), filename = "CC_affected",
           dep.var.labels = c("Born in 1960s", "Born in 1990s", "Born in 2020s", "Born in 2050s", "None of them"),
           dep.var.caption = c("Generations"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

## Sustainable lifestyle
us$dummy_change_lifestyle <- (us$change_lifestyle == "Yes")

desc_table(dep_vars = c("dummy_change_lifestyle"), filename = "change_lifestyle",
           dep.var.labels = c("Willing to change lifestyle"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

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
           dep.var.caption = c("Issues"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

## Block International burden-sharing

# Level for PP to tackle CC
desc_table(dep_vars = c("scale_local", "scale_state", "scale_federal", "scale_global"), filename = "scale",
           dep.var.labels = c("Local","State", "Federal", "Global"),
           dep.var.caption = c("Policy level"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

# Burden
us$dummy_burden_sharing_income <- 0
us[us$burden_sharing_income >= 1, "dummy_burden_sharing_income"] <- 1
us$dummy_burden_sharing_emissions <- 0
us[us$burden_sharing_emissions >= 1, "dummy_burden_sharing_emissions"] <- 1
us$dummy_burden_sharing_cumulative <- 0
us[us$burden_sharing_cumulative >= 1, "dummy_burden_sharing_cumulative"] <- 1
us$dummy_burden_sharing_rich_pay <- 0
us[us$burden_sharing_rich_pay >= 1, "dummy_burden_sharing_rich_pay"] <- 1
us$dummy_burden_sharing_poor_receive <- 0
us[us$burden_sharing_poor_receive >= 1, "dummy_burden_sharing_poor_receive"] <- 1

desc_table(dep_vars = c("dummy_burden_sharing_income", "dummy_burden_sharing_emissions", "dummy_burden_sharing_cumulative", "dummy_burden_sharing_rich_pay", "dummy_burden_sharing_poor_receive"), filename = "burden_sharing",
           dep.var.labels = c("Pay in proportion to income","Pay in proportion to current emissions", "Pay in proportion to past emissions (from 1990)", "Richest pay alone", "Richest pay, and even more to help vulnerable countries"),
           dep.var.caption = c("Countries should"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Equal quota
us$equal_quota_no_vul <- (us$equal_quota == 2)
us$equal_quota_yes <- (us$equal_quota == 1)
us$equal_quota_no_indiv <- (us$equal_quota == 0)
us$equal_quota_no_gf <- (us$equal_quota == -1)
us$equal_quota_no_restrict <- (us$equal_quota == -2)

desc_table(dep_vars = c("equal_quota_no_vul", "equal_quota_yes", "equal_quota_no_gf", "equal_quota_no_indiv", "equal_quota_no_restrict"), filename = "equal_quota",
           dep.var.labels = c("No, should compensate the poorest","Yes", "No, if pollute more more rights", "No, not at individual level", "No, no restrictions of emissions"),
           dep.var.caption = c("In favor of a system of equal GHG quota at individual levels (with monetary compensation and tax)"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, only_mean = T)

# US should act
us$country_should_act_yes <- (us$country_should_act == 1)
us$country_should_act_inter <- (us$country_should_act == 0)
us$country_should_act_no <- (us$country_should_act == -1)

desc_table(dep_vars = c("country_should_act_yes", "country_should_act_inter", "country_should_act_no"), filename = "country_should_act",
           dep.var.labels = c("Yes", "Only if fair international agreement", "No"),
           dep.var.caption = c("U.S. should take measures to fight CC"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Conditions to act
us$country_should_act_condition_comp <- (us$country_should_act_condition == "Compensation")
us$country_should_act_condition_freerid <- (us$country_should_act_condition == "Free-riding")
us$country_should_act_condition_recip <- (us$country_should_act_condition == "Reciprocity")

desc_table(dep_vars = c("country_should_act_condition_comp", "country_should_act_condition_recip", "country_should_act_condition_freerid"), filename = "country_should_act_condition",
           dep.var.labels = c("U.S. more ambitious, if others less", "U.S. more ambitious, if others as well", "U.S. less ambitious, if others are"),
           dep.var.caption = c("U.S. should… (if other countries do…)"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

# Approve propositions
us$dummy_pro_global_assembly <- (us$pro_global_assembly == "Yes")
us$dummy_pro_global_tax <- (us$pro_global_tax == "Yes")
us$dummy_pro_tax_1p <- (us$pro_tax_1p == "Yes")

desc_table(dep_vars = c("dummy_pro_global_assembly", "dummy_pro_global_tax", "dummy_pro_tax_1p"), filename = "pro_inter",
           dep.var.labels = c("Global democratic assembly to fight CC", "Global tax on GHG emissions funding a global basic income (\\textdollar 30/month/adult)", "Global tax on top 1\\% to finance poorest countries"),
           dep.var.caption = c("Approve"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL)

## Post-Treatment
control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_agg", "vote_dum", "treatment_agg")
cov_lab = c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "staths: Working", "Income Q2", "Income Q3", "Income Q4","age: 30-49", "age: 50-87", "vote: Biden", "vote: Trump", "Both treatments", "Climate treatment only", "Policy treatment only")

## Block Pref 1: emission standards
us$dummy_standard_exists <- (us$standard_exists == "Yes")
us$dummy_standard_trust <- (us$standard_trust == "Yes")
us$dummy_standard_effective <- (us$standard_effective == "Yes")
us$standard_employment_pos <- (us$standard_employment == 1)
us$standard_side_effects_pos <- (us$standard_side_effects == 1)
us$dummy_standard_support <- (us$standard_support == "Yes")

desc_table(dep_vars = c("dummy_standard_exists", "dummy_standard_trust", "dummy_standard_effective", "standard_employment_pos", "standard_side_effects_pos", "dummy_standard_support"), filename = "standard_opinion",
           dep.var.labels = c("Does exist", "Trust federal gov.", "Effective", "Positive impact on jobs", "Positive side effects", "Support"),
           dep.var.caption = c("C02 emission limit for cars policy in the U.S."), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

#would win
us$standard_incidence_poor_win <- (us$standard_incidence_poor == 1)
us$standard_incidence_middle_win <- (us$standard_incidence_middle == 1)
us$standard_incidence_rich_win <- (us$standard_incidence_rich == 1)
us$standard_incidence_urban_win <- (us$standard_incidence_urban == 1)
us$standard_incidence_rural_win <- (us$standard_incidence_rural == 1)
us$standard_incidence_self_win <- (us$standard_incidence_self == 1)

desc_table(dep_vars = c("standard_incidence_poor_win", "standard_incidence_middle_win", "standard_incidence_rich_win", "standard_incidence_urban_win", "standard_incidence_rural_win", "standard_incidence_self_win"), filename = "standard_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Winners of emission limits for cars policy"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

#would lose
us$standard_incidence_poor_lose <- (us$standard_incidence_poor == -1)
us$standard_incidence_middle_lose <- (us$standard_incidence_middle == -1)
us$standard_incidence_rich_lose <- (us$standard_incidence_rich == -1)
us$standard_incidence_urban_lose <- (us$standard_incidence_urban == -1)
us$standard_incidence_rural_lose <- (us$standard_incidence_rural == -1)
us$standard_incidence_self_lose <- (us$standard_incidence_self == -1)

desc_table(dep_vars = c("standard_incidence_poor_lose", "standard_incidence_middle_lose", "standard_incidence_rich_lose", "standard_incidence_urban_lose", "standard_incidence_rural_lose", "standard_incidence_self_lose"), filename = "standard_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Losers of emission limits for cars policy"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## Block Pref 2: green investments
us$dummy_investments_trust <- (us$investments_trust == "Yes")
us$dummy_investments_effective <- (us$investments_effective == "Yes")
us$investments_employment_pos <- (us$investments_employment == 1)
us$investments_side_effects_pos <- (us$investments_side_effects == 1)
us$dummy_investments_support <- (us$investments_support == "Yes")

desc_table(dep_vars = c("dummy_investments_trust", "dummy_investments_effective", "investments_employment_pos", "investments_side_effects_pos", "dummy_investments_support"), filename = "investments_opinion",
           dep.var.labels = c("Trust federal gov.", "Effective", "Positive impact on jobs", "Positive side effects", "Support"),
           dep.var.caption = c("Public investment program in green infrastructures for the U.S."), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

#would win
us$investments_incidence_poor_win <- (us$investments_incidence_poor == 1)
us$investments_incidence_middle_win <- (us$investments_incidence_middle == 1)
us$investments_incidence_rich_win <- (us$investments_incidence_rich == 1)
us$investments_incidence_urban_win <- (us$investments_incidence_urban == 1)
us$investments_incidence_rural_win <- (us$investments_incidence_rural == 1)
us$investments_incidence_self_win <- (us$investments_incidence_self == 1)

desc_table(dep_vars = c("investments_incidence_poor_win", "investments_incidence_middle_win", "investments_incidence_rich_win", "investments_incidence_urban_win", "investments_incidence_rural_win", "investments_incidence_self_win"), filename = "investments_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Winners of green investments"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

#would lose
us$investments_incidence_poor_lose <- (us$investments_incidence_poor == -1)
us$investments_incidence_middle_lose <- (us$investments_incidence_middle == -1)
us$investments_incidence_rich_lose <- (us$investments_incidence_rich == -1)
us$investments_incidence_urban_lose <- (us$investments_incidence_urban == -1)
us$investments_incidence_rural_lose <- (us$investments_incidence_rural == -1)
us$investments_incidence_self_lose <- (us$investments_incidence_self == -1)

desc_table(dep_vars = c("investments_incidence_poor_lose", "investments_incidence_middle_lose", "investments_incidence_rich_lose", "investments_incidence_urban_lose", "investments_incidence_rural_lose", "investments_incidence_self_lose"), filename = "investments_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Losers of green investments"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## Block Pref 3: tax and dividend
us$dummy_tax_transfers_trust <- (us$tax_transfers_trust == "Yes")
us$dummy_tax_transfers_effective <- (us$tax_transfers_effective == "Yes")
us$tax_transfers_employment_pos <- (us$tax_transfers_employment == 1)
us$tax_transfers_side_effects_pos <- (us$tax_transfers_side_effects == 1)
us$dummy_tax_transfers_support <- (us$tax_transfers_support == "Yes")

desc_table(dep_vars = c("dummy_tax_transfers_trust", "dummy_tax_transfers_effective", "tax_transfers_employment_pos", "tax_transfers_side_effects_pos", "dummy_tax_transfers_support"), filename = "tax_transfers_opinion",
           dep.var.labels = c("Trust federal gov.", "Effective", "Positive impact on jobs", "Positive side effects", "Support"),
           dep.var.caption = c("Carbon tax with cash transfers"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

#would win
us$tax_transfers_incidence_poor_win <- (us$tax_transfers_incidence_poor == 1)
us$tax_transfers_incidence_middle_win <- (us$tax_transfers_incidence_middle == 1)
us$tax_transfers_incidence_rich_win <- (us$tax_transfers_incidence_rich == 1)
us$tax_transfers_incidence_urban_win <- (us$tax_transfers_incidence_urban == 1)
us$tax_transfers_incidence_rural_win <- (us$tax_transfers_incidence_rural == 1)
us$tax_transfers_incidence_self_win <- (us$tax_transfers_incidence_self == 1)

desc_table(dep_vars = c("tax_transfers_incidence_poor_win", "tax_transfers_incidence_middle_win", "tax_transfers_incidence_rich_win", "tax_transfers_incidence_urban_win", "tax_transfers_incidence_rural_win", "tax_transfers_incidence_self_win"), filename = "tax_transfers_winner",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Winners of carbon tax with cash transfers of \\textdollar 600/year/adult"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

#would lose
us$tax_transfers_incidence_poor_lose <- (us$tax_transfers_incidence_poor == -1)
us$tax_transfers_incidence_middle_lose <- (us$tax_transfers_incidence_middle == -1)
us$tax_transfers_incidence_rich_lose <- (us$tax_transfers_incidence_rich == -1)
us$tax_transfers_incidence_urban_lose <- (us$tax_transfers_incidence_urban == -1)
us$tax_transfers_incidence_rural_lose <- (us$tax_transfers_incidence_rural == -1)
us$tax_transfers_incidence_self_lose <- (us$tax_transfers_incidence_self == -1)

desc_table(dep_vars = c("tax_transfers_incidence_poor_lose", "tax_transfers_incidence_middle_lose", "tax_transfers_incidence_rich_lose", "tax_transfers_incidence_urban_lose", "tax_transfers_incidence_rural_lose", "tax_transfers_incidence_self_lose"), filename = "tax_transfers_loser",
           dep.var.labels = c("Poorest", "Middle class", "Richest", "Urban", "Rural", "Own household"),
           dep.var.caption = c("Losers of carbon tax with cash transfers of \\textdollar 600/year/adult"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## Block Pref on climate policies
us$dummy_CC_worries <- (us$CC_worries >= 0)

desc_table(dep_vars = c("dummy_CC_worries"), filename = "CC_worries",
           dep.var.labels = c("Worried"),
           dep.var.caption = c(""), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

# Support policies climate
us$policy_tax_flying_supp <- (us$policy_tax_flying > 0)
us$policy_tax_fuels_supp <- (us$policy_tax_fuels > 0)
us$policy_insulation_supp <- (us$policy_insulation > 0)
us$policy_ban_city_centers_supp <- (us$policy_ban_city_centers > 0)
us$policy_subsidies_supp <- (us$policy_subsidies > 0)
us$policy_climate_fund_supp <- (us$policy_climate_fund > 0)

desc_table(dep_vars = c("policy_tax_flying_supp", "policy_tax_fuels_supp", "policy_insulation_supp", "policy_ban_city_centers_supp", "policy_subsidies_supp", "policy_climate_fund_supp"), filename = "policy_climate",
           dep.var.labels = c("Tax on flying", "Tax on fossil fuels", "Thermal renovation", "Ban polluting vehicles in city centers", "Subsidies", "Global climate fund"),
           dep.var.caption = c("Climate policies"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

# Revenues of carbon tax
us$tax_transfer_constrained_hh_supp <- (us$tax_transfer_constrained_hh > 0)
us$tax_transfer_poor_supp <- (us$tax_transfer_poor > 0)
us$tax_transfer_all_supp <- (us$tax_transfer_all > 0)
us$tax_rebates_affected_firms_supp <- (us$tax_rebates_affected_firms > 0)
us$tax_investments_supp <- (us$tax_investments > 0)
us$tax_subsidies_supp <- (us$tax_subsidies > 0)
us$tax_reduction_deficit_supp <- (us$tax_reduction_deficit > 0)
us$tax_reduction_corporate_tax_supp <- (us$tax_reduction_corporate_tax > 0)
us$tax_reduction_personal_tax_supp <- (us$tax_reduction_personal_tax > 0)
us$tax_other_choice_supp <- (us$tax_other_choice > 0)

desc_table(dep_vars = c("tax_transfer_constrained_hh_supp", "tax_transfer_poor_supp", "tax_transfer_all_supp", "tax_rebates_affected_firms_supp", "tax_investments_supp", "tax_subsidies_supp", "tax_reduction_deficit_supp" , "tax_reduction_corporate_tax_supp", "tax_reduction_personal_tax_supp", "tax_other_choice_supp"), filename = "revenue_carbon_tax",
           dep.var.labels = c("Transfer to constrained HH", "Transfers to poorest", "Equal transfers", "Tax rebates for affected firms", "Infrastructure projects", "Technology subsidies", "Reduce deficit", "Reduce CIT", "Reduce PIT", "Other"),
           dep.var.caption = c("Support carbon tax if revenues allocated as/to…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## Block Preference for bans vs. incentives
us$insulation_compulsory_compul <- (us$insulation_compulsory =="Mandatory")
us$insulation_compulsory_volunt <- (us$insulation_compulsory =="Voluntary")

desc_table(dep_vars = c("insulation_compulsory_compul", "insulation_compulsory_volunt"), filename = "pref_thermal",
           dep.var.labels = c("made mandatory", "on a voluntary basis"),
           dep.var.caption = c("Thermal renovation should be (if subsidized)…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

us$flight_quota_1000km_trad <- (us$flight_quota_1000km == "Tradable")
us$flight_quota_1000km_rat <- (us$flight_quota_1000km == "Rationing")
us$flight_quota_3000km_trad <- (us$flight_quota_3000km == "Tradable")
us$flight_quota_3000km_rat <- (us$flight_quota_3000km == "Rationing")
us$flight_quota_one_trip_trad <- (us$flight_quota_one_trip == "Tradable")
us$flight_quota_one_trip_rat <- (us$flight_quota_one_trip == "Rationing")

desc_table(dep_vars = c("flight_quota_1000km_trad", "flight_quota_1000km_rat", "flight_quota_3000km_trad", "flight_quota_3000km_rat", "flight_quota_one_trip_trad", "flight_quota_one_trip_rat"), filename = "pref_flight",
           dep.var.labels = c("Rationing (1000km)", "Tradable (1000km)", "Rationing (3000km)", "Tradable (3000km)", "Rationing (0.5 round-trip/year)", "Tradable (0.5 round-trip/year)"),
           dep.var.caption = c("Limit to flight trips"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

us$beef_tax <- as.logical(us$beef_tax)
us$beef_subsidies_vegetables <- as.logical(us$beef_subsidies_vegetables)
us$beef_subsidies_removal <- as.logical(us$beef_subsidies_removal)
us$beef_ban_intensive <- as.logical(us$beef_ban_intensive)

desc_table(dep_vars = c("beef_tax", "beef_subsidies_vegetables", "beef_subsidies_removal", "beef_ban_intensive"), filename = "pref_beef",
           dep.var.labels = c("Tax on cattle products (beefx2)", "Sub. Vegetables", "No sub. cattle", "Ban intensive cattle"),
           dep.var.caption = c("If gov. limits cattle products, I would approve…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

us$ban_incentives_for <- (us$ban_incentives == "Force")
us$ban_incentives_enc <- (us$ban_incentives == "Encourage")

desc_table(dep_vars = c("ban_incentives_for", "ban_incentives_enc"), filename = "pref_incentives",
           dep.var.labels = c("Force people", "Encourage people"),
           dep.var.caption = c("Government should…"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## WTP
desc_table(dep_vars = c("wtp"), filename = "wtp",
           dep.var.labels = c("WTP (\\textdollar  a year)"),
           dep.var.caption = c("WTP to limit global warming to safe levels"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## Block Political views and media consumption
us$dummy_interest_politics <- (us$interest_politics >= 0)
us$dummy_member_envi_orga <- (us$member_environmental_orga == "Yes")
us$dummy_relative_envi <- (us$relative_environmentalist == "Yes")

desc_table(dep_vars = c("dummy_interest_politics", "dummy_member_envi_orga", "dummy_relative_envi"), filename = "pol_views",
           dep.var.labels = c("Interest in politics", "Environmental org. member", "Relative is environmentalist"),
           dep.var.caption = c("Political views"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)


desc_table(dep_vars = c("far_left", "left", "center", "right", "far_right", "liberal", "conservative", "humanist", "patriot", "apolitical", "environmentalist", "feminist"), filename = "pol_positions",
           dep.var.labels = c("Far Left", "Left", "Center", "Right", "Far Right", "Liberal", "Conservative", "Humanist", "Patriot", "Apolitical", "Environmentalist", "Feminist"),
           dep.var.caption = c("Political positions"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

us$media_tv_pr <- (us$media == "TV (private)")
us$media_tv_pu <- (us$media == "TV (public)")
us$media_radio <- (us$media == "Radio")
us$media_social <- (us$media == "Social media")
us$media_print <- (us$media == "Print")
us$media_web <- (us$media == "News websites")
us$media_other <- (us$media == "Other")

desc_table(dep_vars = c("media_tv_pr", "media_tv_pu", "media_radio", "media_social", "media_print", "media_web", "media_other"), filename = "media",
           dep.var.labels = c("TV (private)", "TV (public)", "Radio", "Social media", "Print", "News websites", "Other"),
           dep.var.caption = c("Media mainly used"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)

## Feedback
us$survey_biased_no <- (us$survey_biased == "No")
us$survey_biased_anti <- (us$survey_biased == "Yes, anti environment")
us$survey_biased_pro <- (us$survey_biased == "Yes, pro environment")

desc_table(dep_vars = c("survey_biased_no", "survey_biased_anti", "survey_biased_pro"), filename = "survey_biased",
           dep.var.labels = c("No", "Yes, anti environment", "Yes, pro environment"),
           dep.var.caption = c("Biased"), data = us, indep_vars = control_variables, indep_labels = cov_lab, weights = NULL, mean_control = T
)