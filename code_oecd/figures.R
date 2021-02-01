# Examples
# Be careful with the arguments (df, miss...)

# e <- readRDS("../data/US_pilot_reg_210115.rds")

#source(".Rprofile")

# In case you want to redefine the labels, it need not be automatized, e.g.:
# labels_responsible <- c("Each of us", "The rich", "Governments", "Companies", "Previous generations", "Some foreign countries", "Natural causes", "CC doesn't exist")
# Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F 

# (CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
# save_plotly(CC_exists_US)
# # Tip: if you encounter a bug with the width of the bars, try to passe the argument: thin = F

## Pre-treatment ##

## 1. Demographics
(gender_US <- barres(vars = "gender_factor", df = e, miss = F, labels="Gender"))
save_plotly(gender_US, width= 470, height=140)

(age_US <- barres(vars = "age_agg", df = e, miss=F, rev = F, labels="Age"))
save_plotly(age_US, width= 470, height=140) # TODO comparaison real

(age_quota_US <- barres(vars = "age_quota", df = e, miss=F, rev = F, labels="Age"))
save_plotly(age_quota_US, width= 500, height=140) 

(region_US <- barres(vars = "region", df = e, miss=F, labels="Region"))
save_plotly(region_US, width= 560, height=140)

(race_US <- barres(vars = variables_race[c(1:4)], df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
save_plotly(race_US, width= 340, height=240)

(speak_US <- barres(vars = "speaks_well", df = e, miss=F, labels="How well do you speak english?"))
save_plotly(speak_US, width= 750, height=140)

(education_US <- barres(vars = "education", df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
save_plotly(education_US, width= 880, height=140) # TODO

(employment_status_US <- barres(vars = "employment_agg", df = e, miss=F, labels="What is your employment status?"))
save_plotly(employment_status_US, width= 630, height=140)

## Problem with this one
(hit_by_covid_US <- barres(vars = "hit_by_covid", df = e, miss=F, labels="Hit by covid"))
save_plotly(hit_by_covid_US, width= 420, height=140)

(income_US <- barres(vars = "income", df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
save_plotly(income_US, width= 510, height=140)

(wealth_US <- barres(vars = "wealth", df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
save_plotly(wealth_US, width= 480, height=140)

## 2. HH composition and energy characteristics

(nb_children_US <- barres(vars = "nb_children", df = e, miss=F, rev_color = T, rev = F, labels="Number of children"))
save_plotly(nb_children_US, width= 560, height=140) # TODO

(heating_US <- barres(vars = "heating", df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
save_plotly(heating_US, width= 525, height=140)

### /!\ Var. cont. TODO
(flights_US <- barres(vars = "flights", df = e, miss=F, labels="How many round-trip flights did you take between 2015 and 2019?"))
save_plotly(flights_US, width= 1194, height=140)

(km_driven_US <- barres(vars = "km_driven", df = e, miss=F, labels="How many km have your household driven in 2019?"))
save_plotly(km_driven_US, width= 1194, height=140) # TODO

(frequency_beef_US <- barres(vars = "frequency_beef", df = e, miss=F, rev = F, labels="How often do you eat beef?"))
save_plotly(frequency_beef_US, width= 530, height=140)

variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
labels_transport <- c("Work", "Shopping", "Leisure")
(transport_US <- barres(vars = variables_transport_graph, df = e, rev = F, miss = T, labels=labels_transport))
save_plotly(transport_US, width= 750, height=235) 

(transport_available_US <- barres(vars = "transport_available", df = e, miss=T, labels="Is public transport available near you live?"))
save_plotly(transport_available_US, width= 850, height=140)

## 3. Trust, perceptions of institutions, etc.

### Should we append the 3 graphs below?
(trust_people_US <- barres(vars = "trust_people", df = e, miss=T, rev_color = T, rev=F, labels="Trust other people"))
save_plotly(trust_people_US, width= 815, height=140)

(trust_govt_US <- barres(vars = "trust_govt", df = e, miss=T, rev_color = T, rev=F, labels="Trust the government<br>  to do what is right"))
save_plotly(trust_govt_US, width= 900, height=140)

(trust_public_spending_US <- barres(vars = "trust_public_spending", df = e, miss=T, labels="Authorities spend revenue in a sensible way"))
save_plotly(trust_public_spending_US, width= 1300, height=140)

(statist_US <- barres(vars = "statist", df = e, miss=T, rev_color = T, rev=F, labels="Pro government intervention"))
save_plotly(statist_US, width= 530, height=140)

(inequality_problem_US <- barres(vars = "inequality_problem", df = e, miss=T, labels="Is inequality a serious problem?"))
save_plotly(inequality_problem_US, width= 1120, height=140)

(future_gdp_US <- barres(vars = "future_gdp", df = e, miss=T, labels="Where do you see the world in 100 years?"))
save_plotly(future_gdp_US, width= 770, height=140)

(envi_US <- barres(vars = "envi", df = e, miss=T, labels="Views on environment"))
save_plotly(envi_US, width= 950, height=140)

## 4. Climate Change (attitudes and risks)

(CC_exists_US <- barres(vars = "CC_exists", df = e, miss = T, labels="In your opinion, climate change is..."))
save_plotly(CC_exists_US,width= 680, height=140)

(CC_dynamics_US <- barres(vars = "CC_dynamics", df = e, miss = T, labels="If we halve global GHG emissions,<br>temperature will..."))
save_plotly(CC_dynamics_US, width= 770, height=140)

labels_CC_factor <- c("Eating one beef steak emits <br> far more than eating two servings of pasta", "Electricity from nuclear power emits <br> far more than electricity produced by wind turbines", "Commuting by car emits far more than food waste")
(CC_factor_US <- barres(vars = variables_CC_factor, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_CC_factor, hover=labels_CC_factor))
save_plotly(CC_factor_US, width= 942, height=200) 


labels_responsible <- c()
for (v in variables_CC_responsible) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_responsible[8] <- "None of the above, <br>climate change is not a reality"
(responsible_US <- barres(vars = variables_CC_responsible, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_responsible, hover=labels_responsible))
save_plotly(responsible_US, width= 575, height=320) 

(CC_stoppable_US <- barres(vars = "CC_stoppable", df = e, miss=T, rev = F, labels="Can humanity stop emitting GHG?"))
save_plotly(CC_stoppable_US, width= 1200, height=140)

(CC_talks_US <- barres(vars = "CC_talks", df = e, miss=T, labels="How often do you <br> talk about climate change?"))
save_plotly(CC_talks_US, width= 540, height=140)

labels_CC_affected <- c("People born in the 1960s", "People born in the 1990s", "People born in the 2020", "People born in the 2050s", "None", "PNR")
(CC_affected_US <- barres(vars = variables_CC_affected, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_CC_affected, hover=labels_CC_affected))
save_plotly(CC_affected_US, width= 470, height=300) 

(CC_affected_min_US <- barres(vars = "CC_affected_min", df = e, rev = F, rev_color = T, miss = T, labels="First US generation seriously affected by CC"))
save_plotly(CC_affected_min_US, width= 750, height=140) # TODO!

(CC_impacts_US <- barres(vars = "CC_impacts", df = e, miss=T, rev_color = F, rev = F, labels="Effects of CC if humanity does not limit it"))
save_plotly(CC_impacts_US, width= 885, height=110)

(change_lifestyle_US <- barres(vars = "change_lifestyle", df = e, miss=T, thin=F, labels="Willing to adopt <br> a sustainable lifestyle"))
save_plotly(change_lifestyle_US, width= 570, height=110)

labels_change_condition <- c("Yes, if policies went in this direction", "Yes, if financial means", "Yes, if everyone did the same", "No, only richest should", "No, changing lifestyle would<br> affect me more than CC", "No, CC not a real problem", "Already have sustainable lifestyle", "I'm trying, but troubles to change", "PNR")
(change_condition_US <- barres(vars = variables_change_condition, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_change_condition, hover=labels_change_condition))
save_plotly(change_condition_US, width= 500, height=350) 

labels_effect_policies <- c("Opportunity for our economy", "Costly, but could maintain our lifestyle", "Require deep change of lifestyle", "PNR")
(effect_policies_US <- barres(vars = variables_effect_policies, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_effect_policies, hover=labels_effect_policies))
save_plotly(effect_policies_US, width= 570, height=200) 

labels_kaya <- c("Technologies that emit GHG", "Level of waste", "High-standards of living", "Overconsumption", "Overpopulation", "None of the above", "Other", "PNR", "")
(kaya_US <- barres(vars = variables_kaya, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_kaya, hover=labels_kaya))
save_plotly(kaya_US, width= 540, height=330) 

## 5. International burden-sharing

labels_scale <- c()
for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_scale[5] <- "PNR"
(scale_US <- barres(vars = variables_scale, df = e, error_margin=T, rev = F, miss = T, showLegend=F, labels=labels_scale, hover=labels_scale))
save_plotly(scale_US, width= 260, height=250) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_burden_sharing[3] <- "Countries should pay in proportion to their past emissions <br> (from 1990 onwards)"
labels_burden_sharing[4] <- "The richest countries should pay it all, so that<br> the poorest countries do not have to pay anything"
labels_burden_sharing[5] <- "The richest countries should pay even more, <br> vulnerable countries would then receive money instead of paying"
(burden_sharing_US <- barres(vars = variables_burden_sharing, df = e, miss=T, labels=labels_burden_sharing))
save_plotly(burden_sharing_US, width= 1150, height=325) 

(equal_quota_US <- barres(vars = "equal_quota", df = e, miss=T, labels="Should the U.S. take measures <br> to fight climate change?"))
save_plotly(equal_quota_US, width= 930, height=140)

(country_should_act_condition_US <- barres(vars = "country_should_act_condition", df = e, miss=F, labels="You think the US should take measures against CC.<br> How does this depend on what other countries do?"))
save_plotly(country_should_act_condition_US, width= 870, height=140)

labels_pro <- c("Global democratic assembly<br>on climate change", "Global tax on carbon <br> to fund a global basic income", "Global tax on top 1% <br> to finance low-income countries")
(pro_US <- barres(vars = variables_pro, df = e, miss = T, labels=labels_pro, legend=c("Yes", "No", "PNR")))
save_plotly(pro_US, width= 615, height=250)

## Post-treatment

## 6. Pref 1: emission standards (full)

(standard_exists_US <- barres(vars = "standard_exists", df = e, miss=T, labels="An emission limit for cars <br> exists in the U.S.", legend=c("Yes", "No", "Don't know"))) 
save_plotly(standard_exists_US, width= 700, height=140)

(standard_trust_US <- barres(vars = "standard_trust", df = e, miss=T, labels="U.S. government could correctly <br> implement an emission limits for cars", legend=c("Yes ", "No", "PNR")))
save_plotly(standard_trust_US, width= 600, height=140)

(standard_effective_US <- barres(vars = "standard_effective", df = e, miss=T, labels="An emission limit for cars would <br> be effective to flight climate change", legend=c("Yes ", "No", "PNR")))
save_plotly(standard_effective_US, width= 620, height=140)

(standard_employment_US <- barres(vars = "standard_employment", df = e, miss=T, labels="Impact on employment of <br> an emission limit for cars"))
save_plotly(standard_employment_US, width= 620, height=140)

(standard_side_effects_US <- barres(vars = "standard_side_effects", df = e, miss=T, labels="Side effects of <br> an emission limit for cars"))
save_plotly(standard_side_effects_US, width= 620, height=140)

labels_incidence <- c()
for (v in variables_standard_incidence) labels_incidence <- c(labels_incidence, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_incidence[5] <- "Rural"
labels_incidence[6] <- "Your household"
(standard_incidence_US <- barres(vars = variables_standard_incidence, df = e, rev_color=T, miss = T, labels=labels_incidence, legend=c("Lose", "Unaffected", "Win", "PNR")))
save_plotly(standard_incidence_US, width= 600, height=340) 

(standard_support_US <- barres(vars = "standard_support", df = e, miss=T, labels="Support for an emission limit for cars", legend=c("Yes ", "No", "PNR")))
save_plotly(standard_support_US, width= 670, height=140)

## 7. Pref 2: Green investments

(investments_trust_US <- barres(vars = "investments_trust", df = e, miss=T, labels="U.S. govt could correctly implement<br>  a green infrastructure program", legend=c("Yes ", "No", "PNR")))
save_plotly(investments_trust_US, width= 590, height=140)

(investments_effective_US <- barres(vars = "investments_effective", df = e, miss=T, labels="A green infrastructure program would <br> be effective to flight climate change", legend=c("Yes ", "No", "PNR")))
save_plotly(investments_effective_US, width= 620, height=140)

(investments_employment_US <- barres(vars = "investments_employment", df = e, miss=T, labels="Impact on employment of <br> a green infrastructure program"))
save_plotly(investments_employment_US, width= 620, height=140)

(investments_side_effects_US <- barres(vars = "investments_side_effects", df = e, miss=T, labels="Side effects of <br> a green infrastructure program"))
save_plotly(investments_side_effects_US, width= 620, height=140)

(investments_incidence_US <- barres(vars = variables_investments_incidence, df = e, rev_color=T, miss = T, labels=labels_incidence, legend=c("Lose", "Unaffected", "Win", "PNR")))
save_plotly(investments_incidence_US, width= 600, height=340) 

(investments_support_US <- barres(vars = "investments_support", df = e, miss=T, labels="Support for a <br>green infrastructure program", legend=c("Yes ", "No", "PNR")))
save_plotly(investments_support_US, width= 650, height=140)

## 8. Pref 3: Tax and dividend
(tax_transfers_trust_US <- barres(vars = "tax_transfers_trust", df = e, miss=T, labels="U.S. govt could correctly implement <br> a carbon tax with cash transfers", legend=c("Yes ", "No", "PNR")))
save_plotly(tax_transfers_trust_US, width= 590, height=140)

(tax_transfers_effective_US <- barres(vars = "tax_transfers_effective", df = e, miss=T, labels="A carbon tax with cash transfers would <br> be effective to flight climate change", legend=c("Yes ", "No", "PNR")))
save_plotly(tax_transfers_effective_US, width= 625, height=140)

(tax_transfers_employment_US <- barres(vars = "tax_transfers_employment", df = e, miss=T, labels="Impact on employment of a <br> carbon tax with cash transfers"))
save_plotly(tax_transfers_employment_US, width= 620, height=140)

(tax_transfers_side_effects_US <- barres(vars = "tax_transfers_side_effects", df = e, miss=T, labels="Side effects of <br> a carbon tax with cash transfers"))
save_plotly(tax_transfers_side_effects_US, width= 620, height=140)

(tax_transfers_incidence_US <- barres(vars = variables_tax_transfers_incidence, df = e, rev_color=T, miss = T, labels=labels_incidence, legend=c("Lose", "Unaffected", "Win", "PNR")))
save_plotly(tax_transfers_incidence_US, width= 600, height=340) 

(tax_transfers_support_US <- barres(vars = "tax_transfers_support", df = e, miss=T, labels="Support for a <br>carbon tax with cash transfers", legend=c("Yes ", "No", "PNR")))
save_plotly(tax_transfers_support_US, width= 680, height=140)

## 6-8. Specific policies
labels_policies <- c("An emission limit for cars", "A green infrastructure program", "A carbon tax with cash transfers")
(policies_trust_US <- barres(vars = paste(names_policies, "trust", sep="_"), df = e, miss=T, sort = F, labels=labels_policies, legend=c("Yes ", "No", "PNR")))
save_plotly(policies_trust_US, width= 620, height=250)

(policies_effective_US <- barres(vars = paste(names_policies, "effective", sep="_"), df = e, miss=T, sort = F, labels=labels_policies, legend=c("Yes ", "No", "PNR")))
save_plotly(policies_effective_US, width= 625, height=250)

(policies_employment_US <- barres(vars = paste(names_policies, "employment", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_employment_US, width= 620, height=250)

(policies_side_effects_US <- barres(vars = paste(names_policies, "side_effects", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_side_effects_US, width= 620, height=250)

(policies_incidence_poor_US <- barres(vars = paste(names_policies, "incidence_poor", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_incidence_poor_US, width= 620, height=250)

(policies_incidence_middle_US <- barres(vars = paste(names_policies, "incidence_middle", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_incidence_middle_US, width= 620, height=250)

(policies_incidence_rich_US <- barres(vars = paste(names_policies, "incidence_rich", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_incidence_rich_US, width= 620, height=250)

(policies_incidence_urban_US <- barres(vars = paste(names_policies, "incidence_urban", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_incidence_urban_US, width= 620, height=250)

(policies_incidence_rural_US <- barres(vars = paste(names_policies, "incidence_rural", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_incidence_rural_US, width= 620, height=250)

(policies_incidence_self_US <- barres(vars = paste(names_policies, "incidence_self", sep="_"), df = e, miss=T, sort = F, labels=labels_policies))
save_plotly(policies_incidence_self_US, width= 620, height=250)

(policies_support_US <- barres(vars = paste(names_policies, "support", sep="_"), df = e, miss=T, sort = F, labels=labels_policies, legend=c("Yes ", "No", "PNR")))
save_plotly(policies_support_US, width= 620, height=250)

(standard_yes_no_US <- barres(vars = c("standard_support", "standard_effective", "standard_trust"), sort = F,
                            df = e, miss=T, labels=c("Supports", "Would be effective to flight CC", "Government could correctly implement"), legend=c("Yes ", "No", "PNR")))
save_plotly(standard_yes_no_US, width= 715, height=250)
 
(standard_effects_US <- barres(vars = c("standard_side_effects", "standard_employment"), sort = F,
                            df = e, miss=T, labels=c("Side effects", "Impacts of employment")))
save_plotly(standard_effects_US, width= 600, height=200)

(investments_yes_no_US <- barres(vars = c("investments_support", "investments_effective", "investments_trust"), sort = F,
                                   df = e, miss=T, labels=c("Supports", "Would be effective to flight CC", "Government could correctly implement"), legend=c("Yes ", "No", "PNR")))
save_plotly(investments_yes_no_US, width= 715, height=250)

(investments_effects_US <- barres(vars = c("investments_side_effects", "investments_employment"), sort = F,
                                    df = e, miss=T, labels=c("Side effects", "Impacts of employment")))
save_plotly(investments_effects_US, width= 600, height=200)

(tax_transfers_yes_no_US <- barres(vars = c("tax_transfers_support", "tax_transfers_effective", "tax_transfers_trust"), sort = F,
                                   df = e, miss=T, labels=c("Supports", "Would be effective to flight CC", "Government could correctly implement"), legend=c("Yes ", "No", "PNR")))
save_plotly(tax_transfers_yes_no_US, width= 715, height=250)

(tax_transfers_effects_US <- barres(vars = c("tax_transfers_side_effects", "tax_transfers_employment"), sort = F,
                                    df = e, miss=T, labels=c("Side effects", "Impacts of employment")))
save_plotly(tax_transfers_effects_US, width= 600, height=200)

## 9. Pref on climate policies

(CC_worries_US <- barres(vars = "CC_worries", df = e, miss=T, labels="Worry about impacts of climate change"))
save_plotly(CC_worries_US, width= 830, height=140)

labels_policy <- c()
for (v in variables_policy) labels_policy <- c(labels_policy, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_policy[3] <- "Required thermal renovation of buildings <br>(e.g., better insulation)"
labels_policy[5] <- "Subsidies for low-carbon technologies <br>(renewable energy, capture and storage of carbon...)"
labels_policy[6] <- "A contribution to a global climate fund<br> to finance clean energy in low-income countries"
# labels_policy <- c("Tax on flying", "Tax on fossil fuels", "Required thermal renovation", "Ban polluting cars in city centers", "Subsidies to low-carbon", "Contribution to global climate fund")
(policy_US <- barres(vars = variables_policy, df = e, miss = T, labels=labels_policy))
save_plotly(policy_US, width= 870, height=320) 

labels_tax <- c()
for (v in variables_tax[1:9]) labels_tax <- c(labels_tax, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# labels_tax <- c("Transfer to constrained HH", "Transfers to poorest", "Equal transfers", "Tax rebates for affected firms", "Infrastructure projects", "Technology subsidies", "Reduce deficit", "Reduce CIT", "Reduce PIT")
labels_tax[5] <- "Funding environmental infrastructure projects <br>(public transport, cycling ways, etc.)"
labels_tax[6] <- "Subsidizing low-carbon technos, incl. renewable energy"
labels_tax[1] <- "Transfers to HH with no alternative to using fossil fuels"
(tax_US <- barres(vars = variables_tax[1:9], df = e, miss = T, labels=labels_tax))
save_plotly(tax_US, width= 870, height=470) 	

## 10. Pref for bans vs. incentives

(insulation_compulsory_US <- barres(vars = "insulation_compulsory", df = e, miss=F, labels="If government subsidizes <br> thermal renovation it should be"))
save_plotly(insulation_compulsory_US, width= 550, height=140)

(flight_quota_1000km_US <- barres(vars = "flight_quota_1000km", df = e, miss=F, labels="National quota:<br>1000km"))
save_plotly(flight_quota_1000km_US, width= 510, height=140)

(flight_quota_1000km_global_US <- barres(vars = "flight_quota_1000km_global", df = e, miss=F, labels="Global quota:<br>1000km"))
save_plotly(flight_quota_1000km_global_US, width= 510, height=140)

(flight_quota_one_trip_US <- barres(vars = "flight_quota_one_trip", df = e, miss=F, labels="National quota:<br>1 round-trip every 2 years"))
save_plotly(flight_quota_one_trip_US, width= 510, height=140) # TODO!
# TODO: group them

labels_beef <- c()
for (v in variables_beef) labels_beef <- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_beef[1] <- "A high tax on cattle products,<br>so that the price of beef doubles"
labels_beef[2] <- "Subsidies on organic and local<br>vegetables, fruits and nuts"
labels_beef[3] <- "Removal of subsidies for cattle farming"
# labels_beef <- c("Tax on cattle products (beefx2)", "Sub. Vegetables", "No sub. cattle", "Ban intensive cattle", "PNR")
(beef_pnr_US <- barres(vars = variables_beef, df = e, rev = F, miss = T, labels=labels_beef,showLegend=F))
save_plotly(beef_pnr_US, width= 640, height=280)  

(beef_US <- barres(vars = variables_beef[1:4], df = e[e$beef_pnr==F,], rev = F, miss = T, labels=labels_beef[1:4],showLegend=F))
save_plotly(beef_US, width= 640, height=280)  

(ban_incentives_US <- barres(vars = "ban_incentives", df = e, miss=T, labels="To protect the environment,<br>the government should..."))
save_plotly(ban_incentives_US, width= 550, height=140)

## 11. WTP

(wtp_US <- barres(vars = "wtp", df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="WTP to limit global warming ($/year)"))
save_plotly(wtp_US, width= 1050, height=200)

(wtp_agg_US <- barres(vars = "wtp_agg", df = e, miss=F, rev = F, rev_color = T, labels="WTP to limit global warming ($/year)"))
save_plotly(wtp_agg_US, width= 950, height=140)

mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
cdf_wtp_US <- Ecdf(e$wtp, weights = e$weight)
plot(cdf_wtp_US$x, cdf_wtp_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
title(ylab=expression("Proportion "<=" x"), xlab="WTP (in $/year)", line=2.3)
grid() # TODO legend
par(mar = mar_old, cex = cex_old)

## 12. Political views

(interest_politics_US <- barres(vars = "interest_politics", df = e, miss=T, labels="Interested in politics"))
save_plotly(interest_politics_US, width= 600, height=140)

(member_environmental_orga_US <- barres(vars = "member_environmental_orga", df = e, miss=F, labels="Member of an<br> environmental organization"))
save_plotly(member_environmental_orga_US, width= 622, height=140)

(relative_environmentalist_US <- barres(vars = "relative_environmentalist", df = e, miss=F, labels="Has an environmentalist relative"))
save_plotly(relative_environmentalist_US, width= 540, height=140)

labels_political_identity <- c()
for (v in variables_political_identity[1:7]) labels_political_identity <- c(labels_political_identity, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(political_identity_US <- barres(vars = variables_political_identity[1:7], df = e, rev = F, miss = F, labels=labels_political_identity, showLegend=F))
save_plotly(political_identity_US, width= 330, height=290)

(media_US <- barres(vars = "media", df = e, miss=F, labels="Main media used"))
save_plotly(media_US, width= 925, height=140) # TODO order

(vote_participation_US <- barres(vars = "vote_participation", df = e, miss=T, labels="Voted in 2020 election"))
save_plotly(vote_participation_US, width= 540, height=140) 

(vote_US <- barres(vars = "vote", df = e, rev_color = T, miss=T, labels="Voted for"))
save_plotly(vote_US, width= 480, height=140) # TODO

(vote_participation_2016_US <- barres(vars = "vote_participation_2016", df = e, miss=T, labels="Voted in 2016 election"))
save_plotly(vote_participation_2016_US, width= 540, height=140) 

(vote_2016_US <- barres(vars = "vote_2016", df = e, rev_color = T, miss=T, labels="Voted for"))
save_plotly(vote_2016_US, width= 530, height=140) # TODO: order

## 13. Feedback

(survey_biased_US <- barres(vars = "survey_biased", df = e, rev_color = T, miss=F, labels="Survey biased"))
save_plotly(survey_biased_US, width= 450, height=140) # TODO
