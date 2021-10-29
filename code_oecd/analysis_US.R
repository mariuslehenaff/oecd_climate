##### Metadata #####
e <- it
e <- jp
e <- sa
e <- pl
e <- mx
e <- id
e <- sp
e <- au
e <- ia
e <- tr
e <- br
for (c in ongoing_countries) print(paste(c, round(median(All[[c]]$duration),1), nrow(All[[c]])))
for (c in ongoing_countries) print(paste(c, round(mean(All[[c]]$duration),1)))
for (c in current_countries) print(paste(c, round(median(All[[c]]$duration),1)))
for (c in ongoing_countries) print(paste(c, All[[c]]$date[1]))
for (c in ongoing_countries) print(paste(c, All[[c]]$date[7]))
decrit("finished", data = e)
decrit("excluded", data = e)
sum(is.na(e$excluded) & e$finished == 1)
decrit("duration", data = e) # mean(26.8, 23.2, 24.7, 27, 27.5, 22.7, 19.4, 18.4, 20.8, 30.9, 24.8, 26, 25.9, 31.4, 23.1)
decrit("duration", data = e, which = e$duration > 686/60)
decrit("language", data = e)
decrit("urban_category", data = e) 
decrit("treatment", data = e)
decrit("treatment_policy", data = e)
decrit("treatment_climate", data = e)
decrit("watched_policy", data = e)
decrit("watched_climate", data = e)
decrit("know_treatment_climate", data = e)
decrit("know_treatment_policy", data = e)
decrit("know_temperature_2100", data = e)
decrit("know_local_damage", data = e)
decrit("know_investments_funding", data = e)
decrit("know_ban", data = e)
decrit("attentive", data = e)
decrit("survey_biased", data = e)
decrit("petition", data = e)
decrit("positive_treatment", data = e)
decrit("left_click_petition", data = e)
decrit("right_click_petition", data = e)
decrit("petition", data = e)
decrit("petition", data = e, which = e$variant_petition_real == T)
decrit("polluting_sector", data = e)
any(duplicated(e$PSID))
# New questions
decrit("origin_country", data = e)
decrit("origin_continent", data = e)
decrit("origin_other", data = e)
decrit("standard_prefer_ban", data = e)
decrit("standard_prefer_10k_fine", data = e)
decrit("standard_prefer_100k_fine", data = e)
decrit("standard_10k_fine", data = e)
decrit("standard_100k_fine", data = e)
decrit("policy_ban_coal", data = e)
decrit("tax_reduction_EEG_Umlage", data = e)
decrit("tax_more_commuter_allowance", data = e)
decrit("global_quota", data = e)
decrit("burden_share_population", data = e)
decrit("burden_share_emissions", data = e)
decrit("burden_share_historical", data = e)
decrit("burden_share_damages", data = e)
# DE ~30 zipcodes absent from .csv:
e$zipcode[is.na(e$region)]
sum(is.na(e$region))


##### Durations ######
decrit("duration", data = e) # median 19 min / mean 23.5
decrit(e$duration < 8) # 11%
decrit(e$duration < 15) # 35%
decrit(e$duration[e$duration > 15]) # median 25 min, mean 31 min
decrit("duration_politics_field", data = e) # median 20s / mean 1 min
decrit("duration_CC_field", data = e) # median 20s / mean < 1 min
decrit("duration_policies_field", data = e) # median 20s / mean < 1 min
decrit("duration_burden_sharing", data = e) # 1-2 min
decrit("duration_treatment_climate", data = e) 
decrit("duration_treatment_policy", data = e)
decrit(e$duration_treatment_climate < 2.4) # 31%
decrit(e$duration_treatment_policy < 4.45) # 40%
decrit(e$duration_treatment_climate > 3) # 8%
decrit(e$duration_treatment_policy > 5) # 15%
decrit("duration_tax_transfers", data = e) # < 1 min
decrit("duration_policies", data = e) # 1 min


##### Socio-demographics ####
xtabs(~sector, e)
xtabs(~sector_choice, usp3all)
usp3all$sector_other[grepl("specify", usp3all$sector_choice)]
# AGR Agriculture 01 - 03
# CHM Chemical Industry 20 - 21
# MCH Machinery and Equipment 26 - 30, 33
# EGY Energy (Electricity, Oil, Gas, Heat) 19, 35, 38
# CON Construction 41 - 43
# TRN Transportation 49 - 52
# BNK Banking and Financial Services 64
# INS Insurances 65
# HEA Health 86
# OSE Other Services 36 - 39, 45 - 47, 53 - 63, 68 - 97
# OIN Other Industries
sum(grepl("nformation|IT|^it$|software", usp3all$sector_other)) 
decrit("gender", data = e)
decrit("age", data = e)
decrit("age", data = e)
decrit("region", data = e)
decrit("zipcode", data = e)
decrit("urbanity", data = e) # 35% rural
decrit("urban_category", data = e)
CrossTable(e$urbanity, e$urban, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
CrossTable(e$urban, e$urbanity, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
for (v in variables_race) print(decrit(v, data = e)) # 4% black, 6% hispanic, 3% PNR or other
decrit("race_white", data = e, which = e$race_hispanic)
decrit("race_black", data = e, which = e$race_hispanic)
decrit("speaks_well", data = e)
decrit("education", data = e) # few low education
decrit("employment_status", data = e) 
decrit("employment_agg", data = e) 
decrit("hit_by_covid", data = e)
decrit("income", data = e)
for (v in variables_home) print(decrit(v, data = e))
decrit("wealth", data = e) 
decrit("nb_children", data = e)
decrit("hh_children", data = e)
decrit("hh_adults", data = e)
decrit("heating", data = e)
decrit("km_driven", data = e)
decrit("flights", data = e)
decrit("flights_agg", data = e)
decrit("frequency_beef", data = e) # 7/23/54/17%
for (v in variables_transport) print(decrit(v, data = e)) # available but using car


##### Trust, perceptions of institutions, inequality, and the future ######
decrit("trust_people", data = e)
decrit("trust_govt", data = e)
decrit("trust_public_spending", data = e)
decrit("statist", data = e)
decrit("inequality_problem", data = e)
decrit("future_gdp", data = e) 
decrit("envi", data = e)


##### Essay: politics ######
mean(e$politics_field!="",na.rm=T)
Label(e$politics_field)
usp1$politics_field
usp2$politics_field
mean(grepl("Climate|climate|green|Green|fuel|fracking", e$politics_field))
# proposed variables: liberal, conservative, democrat, republican, independent, moderate / middle of the road, leaning dummy, 
#     abortion, reduce govt, reduce inequality, against discrimination, reduce deficit, climate, healthcare, other proposal, proposal dummy
# + in every field: meaningless/irrelevant, wrong grammar/spelling, detailed, "good"

##### Essay: climate change ######
decrit(e$CC_field!="") 
Label(e$CC_field)
usp1$CC_field
usp2$CC_field
# proposed variables: exists, hoax/doesn't exist, man-made, natural, existence dummy, specific factors mentioned, reaction needed / threat, 
#     no reaction needed, not a problem, don't know, impacts, fatalism, solutions/hope


##### Essay: climate policies ###### 
decrit(e$policies_field!="")
Label(e$policies_field)
usp1$policies_field
usp2$policies_field
# proposed variables: yes, no, neither yes or no, renewable, Paris, international effort, coal, cars, fossil fuels, preserve livelihood, mention political actors (except institutions),
#     pro govt funding/subsidies, anti govt funding, guidelines/regulation, adaptation, waste, carbon tax, other proposal proposal dummy


##### Climate change (attitudes and risks) ######
decrit("CC_exists", data = e) # 50% anthropogenic
decrit("CC_dynamic", data = e) # 47% good
modelplot(lm(CC_dynamic == 'Yes' ~ treatment, data = e)) # * of Policy, Both.. but not Climate where we say it
decrit("net_zero_feasible", data = e) # mixed
summary(lm(net_zero_feasible > 0 ~ treatment, data = e)) # . Both but not Climate alone where we say it
for (v in variables_CC_factor) print(decrit(v, data = e)) # 40-70% true
for (v in variables_CC_responsible) print(decrit(v, data = e))
decrit("CC_stoppable", data = e) 
decrit("CC_talks", data = e) 
for (v in variables_CC_affected) print(decrit(v, data = e)) # less in 2050 than 2020!
decrit("CC_affected_min", data = e)
decrit("change_lifestyle", data = e) # 60%
for (v in variables_change_condition) print(decrit(v, data = e))
for (v in variables_effect_policies) print(decrit(v, data = e))
for (v in variables_kaya) print(decrit(v, data = e))


##### International burden-sharing ######
for (v in variables_scale) print(decrit(v, data = e))
for (v in variables_burden_sharing) print(decrit(v, data = e)) # Weird that last line > second to last
decrit("equal_quota", data = e) # 49% No, not individual level. 
decrit("equal_quota", data = usp1)
decrit("equal_quota", data = usp2)
decrit("country_should_act", data = e)
decrit("country_should_act_condition", data = e)
decrit("pro_global_assembly", data = e)
decrit("pro_global_tax", data = e)
decrit("pro_tax_1p", data = e)
# US who know they pollute more than Chinese are 5 p.p. less prone to fair burden-sharing, contrary to French who know who are also more generous TODO: check if it holds adding controls
summary(lm(pro_polluter_pay ~ correct_footprint_pc_compare * country_name, data = e, weights = e$weight)) # -0.04* aux US/DK mais +0.05 en FR
summary(lm(pro_rich_pay ~ correct_footprint_pc_compare * country_name, data = e, weights = e$weight)) # - 0.06. aux US, 0 FR/DK
summary(lm(pro_grand_fathering ~ correct_footprint_pc_compare * country_name, data = e, weights = e$weight)) # +0.03* US
summary(lm(pro_polluter_and_rich_pay ~ correct_footprint_pc_compare * country_name, data = e, weights = e$weight)) # -0.05 US
summary(lm(pro_differentiated_responsibilities_large ~ correct_footprint_pc_compare * country_name, data = e, weights = e$weight)) 


##### Treatment effects #####
end_formula12 <- paste(paste(variables_main_controls_pilot12, collapse = ' + '), " + wave + treatment") #  treatment_climate * treatment_policy

## Placebo tests (questions asked before treatment)
summary(lm(as.formula(paste("equal_quota > 0 ~", end_formula12)), data = e, weights = e$weight))  # /!\ Placebo test fails: there is a (non significant) effect!
summary(lm(as.formula(paste("CC_exists=='Anthropogenic' ~", end_formula12)), data = e, weights = e$weight)) 
# summary(lm(country_should_act=='Yes' ~ treatment_climate * treatment_policy, data = e, weights = e$weight)) # /!\ Placebo test fails: there is an effect!
# summary(lm(equal_quota > 0 ~ treatment_climate * treatment_policy, data = e, weights = e$weight))
# summary(lm(change_lifestyle=='Yes' ~ treatment_climate * treatment_policy, data = e, weights = e$weight))
# summary(lm(CC_exists=='Anthropogenic' ~ treatment_climate * treatment_policy, data = e, weights = e$weight))

# Table 1
## Placebo tests (questions asked before treatment)
summary(reg_quota <- lm(as.formula(paste("equal_quota > 0 ~", end_formula12)), data = e, weights = e$weight))  # /!\ Placebo test fails: there is a (non significant) effect!
summary(reg_anthro <- lm(as.formula(paste("CC_exists=='Anthropogenic' ~", end_formula12)), data = e, weights = e$weight)) 
## Genuine treatment effects
summary(reg_worry <- lm(as.formula(paste("CC_worries >= 0 ~ ", end_formula12)), data = e, weights = e$weight)) 
summary(reg_act <- lm(as.formula(paste("country_should_act=='Yes' ~ ", end_formula12)), data = e, weights = e$weight))
summary(reg_lifestyle <- lm(as.formula(paste("change_lifestyle=='Yes' ~ ", end_formula12)), data = e, weights = e$weight)) 
temp <- c()
for (dep_var in c("equal_quota >0", "CC_exists == 1", "CC_worries >= 0", "country_should_act == 'Yes'", "change_lifestyle == 'Yes'")) {
  temp <- c(temp, round(wtd.mean(eval(parse(text = paste( "(e$", parse(text = dep_var), ")[data$treatment_agg=='None']", sep=""))), weights = weights[e$treatment=='None'], na.rm = T), d = 3)) }
stargazer(reg_quota, reg_anthro, reg_worry, reg_act, reg_lifestyle, header=F, model.numbers = F,
          covariate.labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both", "Wave: Pilot 2", "Pilot 2 $\\times$ Climate", "Pilot 2 $\\times$ Policy", "Pilot 2 $\\times$ Both"),
          add.lines = list(c("Control group mean", temp)),
          dep.var.labels = c("Approve global equal quota", "CC Anthropogenic", "Worried about CC", "US should act", "Willing to change lifestyle"),
          dep.var.caption = c("\\multicolumn{2}{c}{Placebo tets} & \\multicolumn{3}{c}{Genuine treatment effects}"),
          multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=c("treatment","wave"),  order = c("^treatment","2$","wave"))

# Table 2
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~", end_formula12)), data = e, weights = e$weight))  # /!\ negative effect of climate video
summary(lm(as.formula(paste("investments_support=='Yes' ~", end_formula12)), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("standard_support=='Yes' ~", end_formula12)), data = e, weights = e$weight))  
summary(lm(as.formula(paste("policies_support ~", end_formula12)), data = e, weights = e$weight)) # effect of interaction

# Table 3
summary(lm(as.formula(paste("policies_trust ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("policies_effective ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("policies_self ~", end_formula12)), data = e, weights = e$weight)) # effect of interaction
summary(lm(as.formula(paste("policies_poor ~", end_formula12)), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("policies_employment ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("policies_side_effects ~", end_formula12)), data = e, weights = e$weight))

## Generate tables
label_treat_wave <- c("Both treatments", "Climate treatment only", "Policy treatment only", "wave: Pilot 2")

desc_table(dep_vars = c("equal_quota >0", "CC_exists == 1", "CC_worries >= 0", "country_should_act == 'Yes'", "change_lifestyle == 'Yes'"), filename = "USP1_1",
           dep.var.labels = c("Approve global equal quota", "CC Anthropogenic", "Worried about CC", "US should act", "Willing to change lifestyle"),
           dep.var.caption = c("\\multicolumn{2}{c}{Placebo tets} & \\multicolumn{3}{c}{Genuine treatment effects}"), data = e, keep = c("treatment","wave"), 
           indep_vars = control_variables, indep_labels = label_treat_wave, mean_control = T
)

desc_table(dep_vars = c("tax_transfers_support == 'Yes'", "investments_support == 'Yes'", "standard_support == 'Yes'", "policies_support"), filename = "USP1_2bis",
           dep.var.labels = c("Carbon tax with transfers", "Green Infrastructure Program", "Standard emission for cars", "Average over 3 policies"),
           dep.var.caption = c("Support"), data = e, keep = c("treatment","wave"), indep_vars = control_variables, indep_labels = label_treat_wave, mean_control = T
)

desc_table(dep_vars = c("policies_trust", "policies_effective", "policies_self", "policies_poor", "policies_employment", "policies_side_effects"), filename = "USP1_3",
           dep.var.labels =  c("Trust implementation", "Effective", "Effect on my HH", "Effect on poorest", "Effect on employment", "Other side effects"),
           dep.var.caption = c(""), data = e, keep = c("treatment","wave"), indep_vars = control_variables, indep_labels = label_treat_wave, mean_control = T
)


summary(lm(as.formula(paste("standard_exists=='Yes' ~", end_formula12)), data = e, weights = e$weight)) # /!\ effect although we expect none
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~ treatment_climate +", paste(variables_main_controls, collapse = ' + '))), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("tax_transfers_trust=='Yes' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_effective=='Yes' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_employment=='Positive' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_side_effects=='Positive' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_incidence_self=='Win' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_incidence_poor=='Win' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_incidence_rich=='Lose' ~", end_formula12)), data = e, weights = e$weight))
summary(lm(as.formula(paste("policies_incidence ~", end_formula12)), data = e)) # stupid dependent variable
# summary(lm(CC_worries >= 0 ~ treatment_climate * treatment_policy, data = e))
# summary(lm(tax_transfers_support=='Yes' ~ treatment_climate * treatment_policy, data = e)) # /!\ negative effect of climate video
# summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e)) # effect of interaction
# summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e)) # effect of interaction
# summary(lm(standard_exists=='Yes' ~ treatment_climate * treatment_policy, data = e)) # /!\ effect although we expect none
# summary(lm(standard_support=='Yes' ~ treatment_climate * treatment_policy, data = e))
# summary(lm(investments_support=='Yes' ~ treatment_climate * treatment_policy, data = e))
# summary(lm(tax_transfers_support=='Yes' ~ treatment_climate, data = e))
# summary(lm(policies_trust ~ treatment_climate * treatment_policy, data = e))
# summary(lm(policies_effective ~ treatment_climate * treatment_policy, data = e))
# summary(lm(policies_employment ~ treatment_climate * treatment_policy, data = e))
# summary(lm(policies_side_effects ~ treatment_climate * treatment_policy, data = e))
# summary(lm(policies_incidence ~ treatment_climate * treatment_policy, data = e))
# summary(lm((ban_incentives=='Force') ~ treatment_climate * treatment_policy, data = e))

## Correlations of support with motives
summary(lm(policies_support ~ policies_trust + policies_effective + policies_employment + policies_side_effects + 
             policies_self + policies_incidence + treatment, data = e))
summary(lm(standard_support=='Yes' ~ standard_employment + standard_side_effects + 
             standard_incidence_self + treatment, data = e)) # bug with standard_effective + standard_trust : colinearity?
summary(lm(investments_support=='Yes' ~ investments_employment + investments_side_effects + 
            investments_incidence_self + treatment, data = e))
summary(lm(tax_transfers_support=='Yes' ~ tax_transfers_employment + tax_transfers_side_effects + 
             tax_transfers_incidence_self + treatment, data = e))
summary(lm(as.formula(paste("policies_support ~ policies_trust + policies_effective + policies_employment + policies_side_effects + standard_incidence_self +", end_formula12)), data = e)) # negative effect of policy_treatment, indicating that effect is mediating by a motive
summary(lm(as.formula(paste("standard_support=='Yes' ~ standard_employment + standard_side_effects + standard_incidence_self +", end_formula12)), data = e))
summary(lm(as.formula(paste("investments_support=='Yes' ~ investments_employment + investments_side_effects + investments_incidence_self +", end_formula12)), data = e))
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~ tax_transfers_employment + tax_transfers_side_effects + tax_transfers_incidence_self +", end_formula12)), data = e))

## Heterogenous effects
# Heterogenous effects: duration > 15. Nothing different for these ones.
summary(lm(standard_exists=='Yes' ~ treatment_climate * treatment_policy, data = e, subset = duration > 15)) 
summary(lm(tax_transfers_support=='Yes' ~ treatment_climate * treatment_policy, data = e, subset = duration > 15))
summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e, subset = duration > 15))
summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e, subset = duration > 15)) 
# # Heterogenous effects: duration > 8
# summary(lm(standard_exists=='Yes' ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 
# summary(lm(tax_transfers_support=='Yes' ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 
# summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 
# summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 

# Heterogenous effects: rush. Negative climate results driven by Trump supporters.
summary(lm(tax_transfers_support=='Yes' ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(as.formula(paste("CC_exists=='Anthropogenic' ~", end_formula12)), data = e, subset = !rush)) 
summary(lm(as.formula(paste("CC_worries >= 0 ~", end_formula12)), data = e, subset = !rush)) 
summary(lm(tax_transfers_incidence_self=="Win" ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(tax_transfers_incidence_rich=="Lose" ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(tax_transfers_effective=='Yes' ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e, subset = !rush))

# Heterogenous effects: politics. Negative climate results driven by Trump supporters.
summary(lm(tax_transfers_support=='Yes' ~ treatment_climate * treatment_policy * (vote < - 0.5), data = e)) 
summary(lm(policies_support ~ treatment_climate * treatment_policy * (vote  < - 0.5), data = e)) 
summary(lm(policies_self ~ treatment_climate * treatment_policy * (vote  < - 0.5), data = e))


##### Pilot 3: Treatment effects #####
# Those who haven't watched the video are much more wrong than the others on knowledge questions
summary(lm(know_treatment_climate ~ watched_climate=='Yes', data = e))
summary(lm(know_treatment_policy ~ watched_policy=='Yes', data = e))
summary(lm((age %in% c("50-64", "65+")) ~  watched_policy=='Yes', data = e)) # old people have *less* technical problems

end_formula3 <- paste(paste(variables_main_controls_pilot3, collapse = ' + '), " +  treatment") #  treatment_climate * treatment_policy

summary(lm(as.formula(paste("CC_anthropogenic > 0 ~ ", end_formula3)), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("CC_impacts_extinction>0 ~ ", end_formula3)), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("donation ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("pro_ambitious_policies>0 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("willing_limit_driving>0 ~ ", end_formula3)), data = e, weights = e$weight)) 

summary(lm(as.formula(paste("CC_problem==2 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("CC_impacts_more_migration==2 ~ ", end_formula3)), data = e, weights = e$weight))
# summary(lm(as.formula(paste("WTP >= 50 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("wtp == 'Yes' ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("net_zero_feasible>0 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("CC_affects_self>0 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("CC_will_end>0 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("effect_halt_CC_economy>0 ~ ", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("effect_halt_CC_lifestyle>0 ~ ", end_formula3)), data = e, weights = e$weight))

# Table 2
summary(lm(as.formula(paste("tax_transfers_support>0 ~", end_formula3)), data = e, weights = e$weight))  # /!\ negative effect of climate video
summary(lm(as.formula(paste("investments_support>0 ~", end_formula3)), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("standard_support>0 ~", end_formula3)), data = e, weights = e$weight))  
summary(lm(as.formula(paste("policies_support>0 ~", end_formula3)), data = e, weights = e$weight)) # effect of interaction

# Table 3
summary(lm(as.formula(paste("policies_fair>0 ~", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("policies_self>0 ~", end_formula3)), data = e, weights = e$weight)) # effect of interaction
summary(lm(as.formula(paste("policies_poor>0 ~", end_formula3)), data = e, weights = e$weight)) 
summary(lm(as.formula(paste("policies_large_effect>0 ~", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("policies_negative_effect>0 ~", end_formula3)), data = e, weights = e$weight))

summary(lm(as.formula(paste("policies_cost_effective>0 ~", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("tax_transfers_win_lose_poor>0 ~", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("investments_win_lose_poor>0 ~", end_formula3)), data = e, weights = e$weight))
summary(lm(as.formula(paste("standard_win_lose_poor>0 ~", end_formula3)), data = e, weights = e$weight))

##### Tables treatment effects #####
## Generate tables
label_treat_wave <- c("Both treatments", "Climate treatment only", "Policy treatment only", "wave: Pilot 2")
variables_main_controls_new <- variables_main_controls_pilot3[c(1:6,8:9,11)]

desc_table(dep_vars = c("CC_anthropogenic > 0", "CC_impacts_extinction > 0", "donation", "should_fight_CC > 0", "willing_limit_driving > 0"), # filename = "US_1",
           dep.var.labels = c("CC caused by humans", "CC likely to cause extinction", "Donation (in \\$)", "US should fight CC", "Willing to limit driving"),
           data = e, keep = c("treatment"), indep_vars = c(variables_main_controls_new, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
)

desc_table(dep_vars = c("tax_transfers_support > 0", "investments_support > 0", "standard_support > 0", "policies_support > 0"), # filename = "US_2",
           dep.var.labels = c("Carbon tax with transfers", "Green Infrastructure Program", "Emission standard for cars", "Average over 3 policies"),
           dep.var.caption = c("Support"), data = e, keep = c("treatment"), indep_vars = c(variables_main_controls_new, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
)

desc_table(dep_vars = c("policies_fair > 0", "policies_self > 0", "policies_poor > 0", "policies_large_effect > 0", "policies_negative_effect > 0"), # filename = "US_3",
           dep.var.labels =  c("Fair", "HH would win", "Poor would win", "Large economic effect", "Negative economic effect"),
           data = e, keep = c("treatment"), indep_vars = c(variables_main_controls_new, "treatment"), indep_labels = c("Treatment: Climate", "Treatment: Policy", "Treatment: Both"), mean_control = T
)


##### Treatment Feedback #####
decrit("watched_climate", data = e)
decrit("watched_policy", data = e)
decrit("enjoyed_climate", data = e)
decrit("enjoyed_policy", data = e)
decrit("learned_climate", data = e)
decrit("learned_policy", data = e)


##### Preference 1: emission limit for cars ######
decrit("standard_exists", data = e)
decrit("standard_trust", data = e)
decrit("standard_effective", data = e)
decrit("standard_employment", data = e)
decrit("standard_side_effects", data = e)
for (v in variables_standard_incidence) print(decrit(v, data = e))
decrit("standard_support", data = e)


##### Preference 2: green infrastructure program ######
decrit("investments_trust", data = e)
decrit("investments_effective", data = e)
decrit("investments_employment", data = e)
decrit("investments_side_effects", data = e)
for (v in variables_investments_incidence) print(decrit(v, data = e))
decrit("investments_support", data = e)


##### Preference 3: carbon tax with cash transfers ######
decrit("tax_transfers_trust", data = e)
decrit("tax_transfers_effective", data = e)
decrit("tax_transfers_employment", data = e)
decrit("tax_transfers_side_effects", data = e)
for (v in variables_tax_transfers_incidence) print(decrit(v, data = e))
decrit("tax_transfers_support", data = e)


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


##### Specific policies #####
CrossTable(e$age, e$policies_support>0, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 


##### Preferences on climate policies ######
decrit("CC_worries", data = e)
for (v in variables_policy) print(decrit(v, data = e))
for (v in variables_tax) print(decrit(v, data = e)) 


##### Preference for bans vs. incentives ######
decrit("insulation_support", data = e)
decrit("insulation_disruption_variant", data = e)
decrit("insulation_compulsory", data = e)
decrit("flights", data = e)
decrit("flights", data = e, which = e$flights == 0) # 54
decrit("flights", data = e, which = e$flights %in% c(1:9)) # 91 
decrit("flights", data = e, which = e$flights >= 10) # 50
decrit("flight_quota", which = fr$variant_flight_quota == "1 trip", data = fr)
decrit("flight_quota", which = fr$variant_flight_quota == "1000km", data = fr)
decrit("flight_quota", which = fr$variant_flight_quota == "1000km global", data = fr)
all(as.integer(e$flights)==e$flights) # TODO: table, plot flight_pref(revenu/nb_flight)
for (v in variables_flight_quota) print(decrit(v, data = e, miss = F)) # <<<
# More want it tradable when it is global (normal). Those who fly more / richer prefer more market when quota is global
for (v in variables_flight_quota[1:2]) print(decrit(v, data = e, which = e$flights == 0))
for (v in variables_flight_quota[1:2]) print(decrit(v, data = e, which = e$flights < 2.5, miss = F))
for (v in variables_flight_quota[1:2]) print(decrit(v, data = e, which = e$flights > 2.5, miss = F))
for (v in variables_flight_quota[1:2]) print(decrit(v, data = e, which = e$income %in% c("Q1", "Q2"), miss = F)) # seuils 35/70/120k
for (v in variables_flight_quota[1:2]) print(decrit(v, data = e, which = e$income %in% c("Q3", "Q4"), miss = F))
for (v in variables_beef) print(decrit(v, data = e))
decrit("ban_incentives", data = e)


##### WTP ######
decrit("wtp", data = e)
decrit("WTP", data = usp3all)
CrossTable(e$wtp, e$wtp_variant, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 


##### Poltical views and media consumption ######
decrit("interest_politics", data = e)
decrit("member_environmental_orga", data = e)
decrit("relative_environmentalist", data = e)
decrit("left_right", data = e)
decrit("Left_right", data = e)
for (v in variables_political_identity) print(decrit(v, data = e))
decrit("media", data = e)
decrit("vote_participation", data = e)
decrit("vote", data = e)
decrit("vote_participation_2016", data = e)
decrit("vote_agg", data = e)
decrit("liberal_conservative", data = e)
decrit("political_affiliation", data = e)


##### Feedback ######
decrit("survey_biased", data = usp1) # 40% Pro-envi biased. 
decrit("survey_biased", data = usp2)
decrit("survey_biased", data = e)
e$comment_field # Most don't leave a comment, many "Good survey", many (but less) critics that it's pro-envi biased, a few critics that some options are missing (but no example): c(3, 46, 135, 192)


##### Insulation #####
e$obstacles_insulation_other[!is.na(e$obstacles_insulation_other)]


##### Decision trees #####
rpart.plot(tree_support <- rpart(as.formula(paste("policies_support>0 ~", end_formula3)), e)) # instead of formula, could be: var ~ . 
prp(tree_support, box.palette = "Blues", tweak = 1.2)

##### To check #####
# To check (quality, autres consignes)
# - duration is fine: 24.5 min in median (28 in mean, 17-34 quartiles).
# - comprehension question (for the videos): results in-line with the first survey, though improved for the policy video where almost everyone gets that we talk of a ban on combustion-engine cars. Still about half of people who get it wrong for the other questions (even when there is no number: probably due to a lack of attention because only 3% didn't watch until the end).
# - 35% are excluded from the final sample (the one I analyzed) either because they were too fast or failed the attention test. I will code some variables to check the consistency of responses, but for the moment we don't have further info on the quality.
# - 95% do/did not work in the list of polluting sectors we present them, which I find surprisingly low (but here again, may be due to sample size).
# - for the petition it is funny because 30% say they are willing to sign but only 3% click on the link.
# - apart from that, answers to new questions make sense (e.g. China more frequently put "most" for total rather than per capita, only 11% choosing "Other" for the sector question), although for the WTP the answers do not seem to really depend on the amount proposed (but here low sample size may be at play).


##### Acquiescence ####
decrit(usp3$investments_cost_effective)
decrit(us$investments_cost_effective)
decrit(usp3$standard_cost_effective)
decrit(us$standard_cost_effective)
decrit(usp3$tax_transfers_cost_effective)
decrit(us$tax_transfers_cost_effective)
decrit(usp3$policies_cost_effective)
decrit(us$policies_cost_effective)


##### Survey biased #####
summary(lm(survey_biased != 'No' ~ treatment * country_name, data = all, weights = all$weight)) # + 5 p.p. policy
decrit("survey_biased", data = all, which = all$treatment == 'None') # 73/21/7 No/left/right


##### Future #####
# Pessimistic w.r.t. future more or less climate friendly? Same same
decrit(e$future_richness)
summary(lm(should_fight_CC ~ future_richness, data = e))
summary(lm(wtp==1 ~ future_richness, data = e)) # +5***p.p. only regression that reproduces that positive correlation of Fairbrother et al. 2021
summary(lm(as.numeric(willing_limit_driving) ~ future_richness, data = e))
summary(lm(willing_limit_driving > 0 ~ future_richness, data = e))
summary(lm(should_fight_CC ~ as.factor(future_richness), data = e))
summary(lm(should_fight_CC ~ future_richness + CC_anthropogenic, data = e))


##### Quality of answers #####
decrit(e$duration[e$weird_good_CC_field==T]) # short
decrit(e$zipcode[e$weird_good_CC_field==T]) # seem normal


##### Beef #####
decrit("willing_limit_beef", all)
decrit("willing_limit_beef", all, which = all$frequency_beef > 0)