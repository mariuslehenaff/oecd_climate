##### Metadata #####
decrit("finished", data = e)
decrit("excluded", data = e)
decrit("language", data = e)
decrit("urban_category", data = e) 
decrit("treatment_policy", data = e)
decrit("treatment_climate", data = e)
decrit("variant_flight_quota", data = e)
any(duplicated(e$PSID))


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
decrit("gender", data = e)
decrit("age", data = e)
decrit("age_quota", data = e)
decrit("region", data = e)
decrit("zipcode", data = e)
decrit("urbanity", data = e) # 35% rural
decrit("urban_category", data = e)
CrossTable(e$urbanity, e$core_metropolitan, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
CrossTable(e$core_metropolitan, e$urbanity, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
for (v in variables_race) print(decrit(v, data = e)) # 4% black, 6% hispanic, 3% PNR or other
decrit("race_white", data = e, which = e$race_hispanic)
decrit("race_black", data = e, which = e$race_hispanic)
decrit("speaks_well", data = e)
decrit("education", data = e) # few low education
decrit("employment_status", data = e) # TODO
decrit("hit_by_covid", data = e)
decrit("income", data = e)
for (v in variables_home) print(decrit(v, data = e))
decrit("wealth", data = e) # TODO label
decrit("nb_children", data = e)
decrit("hh_children", data = e)
decrit("hh_adults", data = e)
decrit("heating", data = e) # 44% Elec TODO check
decrit("km_driven", data = e)
decrit("flights", data = e)
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
decrit(e$politics_field!="")
Label(e$politics_field)
usp1$politics_field
usp2$politics_field
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
decrit(e$policies_field!="")# TODO length
Label(e$policies_field)
usp1$policies_field
usp2$policies_field
# proposed variables: yes, no, neither yes or no, renewable, Paris, international effort, coal, cars, fossil fuels, preserve livelihood, mention political actors (except institutions),
#     pro govt funding/subsidies, anti govt funding, guidelines/regulation, adaptation, waste, carbon tax, other proposal proposal dummy


##### Climate change (attitudes and risks) ######
decrit("CC_exists", data = e) # 50% anthropogenic
decrit("CC_dynamics", data = e) # 47% good
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
decrit("equal_quota", data = e) # 49% No, not individual level. TODO: Change?
decrit("equal_quota", data = usp1)
decrit("equal_quota", data = usp2)
decrit("country_should_act", data = e)
decrit("country_should_act_condition", data = e) # TODO order
decrit("pro_global_assembly", data = e)
decrit("pro_global_tax", data = e)
decrit("pro_tax_1p", data = e)


##### Treatment effects #####
end_formula <- paste(paste(variables_main_controls, collapse = ' + '), " + treatment") #  treatment_climate * treatment_policy

## Placebo tests (questions asked before treatment)
summary(lm(as.formula(paste("equal_quota > 0 ~", end_formula)), data = e))  # /!\ Placebo test fails: there is a (non significant) effect!
summary(lm(as.formula(paste("CC_exists=='Anthropogenic' ~", end_formula)), data = e)) 
# summary(lm(country_should_act=='Yes' ~ treatment_climate * treatment_policy, data = e)) # /!\ Placebo test fails: there is an effect!
# summary(lm(equal_quota > 0 ~ treatment_climate * treatment_policy, data = e))
# summary(lm(change_lifestyle=='Yes' ~ treatment_climate * treatment_policy, data = e))
# summary(lm(CC_exists=='Anthropogenic' ~ treatment_climate * treatment_policy, data = e))

## Genuine treatment effects
summary(lm(as.formula(paste("CC_worries >= 0 ~ ", end_formula)), data = e)) 
summary(lm(as.formula(paste("country_should_act=='Yes' ~ wave +", end_formula)), data = e))
summary(lm(as.formula(paste("change_lifestyle=='Yes' ~ wave +", end_formula)), data = e)) 
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~", end_formula)), data = e))  # /!\ negative effect of climate video
summary(lm(as.formula(paste("investments_support=='Yes' ~", end_formula)), data = e)) 
summary(lm(as.formula(paste("standard_support=='Yes' ~", end_formula)), data = e))  
summary(lm(as.formula(paste("policies_support ~", end_formula)), data = e)) # effect of interaction
summary(lm(as.formula(paste("policies_self ~", end_formula)), data = e)) # effect of interaction
summary(lm(as.formula(paste("policies_trust ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_effective ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_employment ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_side_effects ~", end_formula)), data = e))
summary(lm(as.formula(paste("policies_incidence ~", end_formula)), data = e))

summary(lm(as.formula(paste("standard_exists=='Yes' ~", end_formula)), data = e)) # /!\ effect although we expect none
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~ treatment_climate +", paste(variables_main_controls, collapse = ' + '))), data = e)) 
summary(lm(as.formula(paste("tax_transfers_trust=='Yes' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_effective=='Yes' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_employment=='Positive' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_side_effects=='Positive' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_incidence_self=='Win' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_incidence_poor=='Win' ~", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_incidence_rich=='Lose' ~", end_formula)), data = e))
# summary(lm(CC_worries >= 0 ~ treatment_climate * treatment_policy, data = e))
# summary(lm(tax_transfers_support=="Yes" ~ treatment_climate * treatment_policy, data = e)) # /!\ negative effect of climate video
# summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e)) # effect of interaction
# summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e)) # effect of interaction
# summary(lm(standard_exists=="Yes" ~ treatment_climate * treatment_policy, data = e)) # /!\ effect although we expect none
# summary(lm(standard_support=="Yes" ~ treatment_climate * treatment_policy, data = e))
# summary(lm(investments_support=="Yes" ~ treatment_climate * treatment_policy, data = e))
# summary(lm(tax_transfers_support=="Yes" ~ treatment_climate, data = e))
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
summary(lm(as.formula(paste("policies_support ~ policies_trust + policies_effective + policies_employment + policies_side_effects + standard_incidence_self +", end_formula)), data = e)) # negative effect of policy_treatment, indicating that effect is mediating by a motive
summary(lm(as.formula(paste("standard_support=='Yes' ~ standard_employment + standard_side_effects + standard_incidence_self +", end_formula)), data = e))
summary(lm(as.formula(paste("investments_support=='Yes' ~ investments_employment + investments_side_effects + investments_incidence_self +", end_formula)), data = e))
summary(lm(as.formula(paste("tax_transfers_support=='Yes' ~ tax_transfers_employment + tax_transfers_side_effects + tax_transfers_incidence_self +", end_formula)), data = e))

## Heterogenous effects
# Heterogenous effects: duration > 15. Nothing different for these ones.
summary(lm(standard_exists=="Yes" ~ treatment_climate * treatment_policy, data = e, subset = duration > 15)) 
summary(lm(tax_transfers_support=="Yes" ~ treatment_climate * treatment_policy, data = e, subset = duration > 15))
summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e, subset = duration > 15))
summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e, subset = duration > 15)) 
# # Heterogenous effects: duration > 8
# summary(lm(standard_exists=="Yes" ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 
# summary(lm(tax_transfers_support=="Yes" ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 
# summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 
# summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e, subset = duration > 8)) 

# Heterogenous effects: rush. Negative climate results driven by Trump supporters.
summary(lm(tax_transfers_support=="Yes" ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(as.formula(paste("CC_exists=='Anthropogenic' ~", end_formula)), data = e, subset = !rush)) 
summary(lm(as.formula(paste("CC_worries >= 0 ~", end_formula)), data = e, subset = !rush)) 
summary(lm(tax_transfers_incidence_self=="Win" ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(tax_transfers_incidence_rich=="Lose" ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(tax_transfers_effective=="Yes" ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(policies_support ~ treatment_climate * treatment_policy, data = e, subset = !rush)) 
summary(lm(policies_self ~ treatment_climate * treatment_policy, data = e, subset = !rush))

# Heterogenous effects: politics. Negative climate results driven by Trump supporters.
summary(lm(tax_transfers_support=="Yes" ~ treatment_climate * treatment_policy * (vote == 'Biden'), data = e)) 
summary(lm(policies_support ~ treatment_climate * treatment_policy * (vote == 'Biden'), data = e)) 
summary(lm(policies_self ~ treatment_climate * treatment_policy * (vote == 'Biden'), data = e))


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
data_cor_standard <- e[,variables_standard] 
names(data_cor_standard) <- variables_standard
corr_standard <- cor(data_cor_standard, use="complete.obs")
p.mat <- cor.mtest(data_cor_standard) # corrplot does not work when some packages are loaded before 'corrplot' => if it doesn't work, restart R and load only corrplot.
corrplot(corr_standard, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'


##### Preference 2: green infrastructure program ######
decrit("investments_trust", data = e)
decrit("investments_effective", data = e)
decrit("investments_employment", data = e)
decrit("investments_side_effects", data = e)
for (v in variables_investments_incidence) print(decrit(v, data = e))
decrit("investments_support", data = e)
data_cor_investments <- e[,variables_investments] 
names(data_cor_investments) <- variables_investments
corr_investments <- cor(data_cor_investments, use="complete.obs")
p.mat <- cor.mtest(data_cor_investments) # corrplot does not work when some packages are loaded before 'corrplot' => if it doesn't work, restart R and load only corrplot.
corrplot(corr_investments, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'


##### Preference 3: carbon tax with cash transfers ######
decrit("tax_transfers_trust", data = e)
decrit("tax_transfers_effective", data = e)
decrit("tax_transfers_employment", data = e)
decrit("tax_transfers_side_effects", data = e)
for (v in variables_tax_transfers_incidence) print(decrit(v, data = e))
decrit("tax_transfers_support", data = e)
data_cor_tax_transfers <- e[,variables_tax_transfers] 
names(data_cor_tax_transfers) <- variables_tax_transfers
corr_tax_transfers <- cor(data_cor_tax_transfers, use="complete.obs")
p.mat <- cor.mtest(data_cor_tax_transfers) # corrplot does not work when some packages are loaded before 'corrplot' => if it doesn't work, restart R and load only corrplot.
corrplot(corr_tax_transfers, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'


##### Preferences on climate policies ######
decrit("CC_worries", data = e)
for (v in variables_policy) print(decrit(v, data = e))
for (v in variables_tax) print(decrit(v, data = e)) 


##### Preference for bans vs. incentives ######
decrit("insulation_compulsory", data = e)
decrit("flights", data = e)
decrit("flights", data = e, which = e$flights == 0) # 54
decrit("flights", data = e, which = e$flights %in% c(1:9)) # 91 
decrit("flights", data = e, which = e$flights >= 10) # 50
all(as.integer(e$flights)==e$flights) # TODO: table, plot flight_pref(revenu/nb_flight)
for (v in variables_flight_quota) print(decrit(v, data = e))
for (v in variables_flight_quota) print(decrit(v, data = e, which = e$flights == 0))
for (v in variables_flight_quota) print(decrit(v, data = e, which = e$flights %in% c(1:9)))
for (v in variables_flight_quota) print(decrit(v, data = e, which = e$flights >= 10))
for (v in variables_beef) print(decrit(v, data = e))
decrit("ban_incentives", data = e)


##### WTP ######
decrit("wtp", data = e)


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
decrit("vote_2016", data = e)
decrit("liberal_conservative", data = e)
decrit("political_affiliation", data = e)


##### Feedback ######
decrit("survey_biased", data = usp1) # 40% Pro-envi biased. TODO: Change?
decrit("survey_biased", data = usp2)
e$comment_field # Most don't leave a comment, many "Good survey", many (but less) critics that it's pro-envi biased, a few critics that some options are missing (but no example): c(3, 46, 135, 192)
