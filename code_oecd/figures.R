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

(age_quota_US <- barres(vars = "age_quota", df = e, miss=F, rev = F, labels="Age"))
save_plotly(age_quota_US, width= 500, height=140) 

(region_US <- barres(vars = "region", df = e, miss=F, labels="Region"))
save_plotly(region_US, width= 560, height=140)

(race_US <- barres(vars = variables_race[c(1:4)], df = e, miss=F, showLegend=F, rev = F, labels=c("White", "Black", "Hispanic", "Asian")))
save_plotly(race_US, width= 340, height=240)

(income_US <- barres(vars = "income", df = e, miss=F, rev_color = T, rev = F, labels="2019 household income"))
save_plotly(income_US, width= 510, height=140)

(wealth_US <- barres(vars = "wealth", df = e, miss=F,  rev_color = T, rev = F, labels="Wealth of household"))
save_plotly(wealth_US, width= 480, height=140)

(education_US <- barres(vars = "education", df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
save_plotly(education_US, width= 1080, height=140) 

(employment_status_US <- barres(vars = "employment_agg", df = e, miss=F, labels="What is your employment status?"))
save_plotly(employment_status_US, width= 630, height=140)

(occupation_US <- barres(vars = "occupation", df = e, miss=F, labels="Main occupation?"))
save_plotly(occupation_US, width= 885, height=240)

#(sector_US <- barres(vars = "sector", df = e, miss=F, labels="What is your primary line of business?"))
#save_plotly(sector_US, width= 630, height=140)

(hit_by_covid_US <- barres(vars = "hit_by_covid", df = e, miss=F, labels="Hit by covid"))
save_plotly(hit_by_covid_US, width= 420, height=140)

labels_home <- c()
for (v in variables_home) labels_home <- c(labels_home, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(home_US <- barres(vars = variables_home, df = e, rev = F, miss = F, showLegend=F, labels=labels_home, hover=labels_home))
save_plotly(home_US, width= 575, height=320) 

(couple_US <- barres(vars = "couple", df = e, miss=F, labels="Do you live with your partner?"))
save_plotly(couple_US, width= 630, height=140)

(marital_status_US <- barres(vars = "marital_status", df = e, miss=F, labels="What is your marital status?"))
save_plotly(marital_status_US, width= 830, height=175)

(nb_children_US <- barres(vars = "Nb_children", df = e, miss=F, rev_color = T, rev = F, labels="Number of children", legend = c(0:3, "4 or more")))
save_plotly(nb_children_US, width= 470, height=140)

(hh_size_US <- barres(vars = "HH_size", df = e, miss=F, rev_color = T, rev = F,labels="People in household"))
save_plotly(hh_size_US, width= 470, height=140)

## 2. HH composition and energy characteristics

(heating_US <- barres(vars = "heating", df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
save_plotly(heating_US, width= 525, height=140)

(heating_expenses_US <- barres(vars = "heating_expenses", df = e, miss=F, labels="Monthly heating expenses", rev = F))
save_plotly(heating_expenses_US, width= 840, height=425)

(insulation_US <- barres(vars = "insulation", df = e, miss=F, labels="Quality accommodation's insulation", rev = F))
save_plotly(insulation_US, width= 625, height=140)

(gas_expenses_US <- barres(vars = "gas_expenses", df = e, miss=F, labels="Monthly gas expenses", rev = F))
save_plotly(gas_expenses_US, width= 800, height=200)

(flights_desagg_US <- barres(vars = "flights_agg", df = e, miss=F, labels="Round-trip flights taken in 2019?"))
save_plotly(flights_desagg_US, width= 1194, height=140)

(frequency_beef_US <- barres(vars = "frequency_beef", df = e, miss=F, rev = F, labels="How often do you eat beef?"))
save_plotly(frequency_beef_US, width= 530, height=140)

variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
labels_transport <- c("Work", "Shopping", "Leisure")
(transport_US <- barres(vars = variables_transport_graph, df = e, rev = F, miss = T, labels=labels_transport))
save_plotly(transport_US, width= 850, height=275) 

(availability_transport_US <- barres(vars = "availability_transport", df = e, miss=F, labels="Quality and availability of public transport near your home"))
save_plotly(availability_transport_US, width= 850, height=140)


## POST-TREATMENT

## 3. Treatment feedback: local climate
(watched_climate_US <- barres(vars = "watched_climate", df = e, miss=F, labels="Able to watch the video until the end (local)"))
save_plotly(watched_climate_US, width= 850, height=140)

(know_treatment_climate_US <- barres(vars = "know_treatment_climate", df = e, miss=T, labels="Score knowledge climate video"))
save_plotly(know_treatment_climate_US, width= 850, height=140)

## 4. Treatment feedback: local policy
(watched_policy_US <- barres(vars = "watched_policy", df = e, miss=F, labels="Able to watch the video until the end (policy)"))
save_plotly(watched_policy_US, width= 850, height=140)

(know_treatment_policy_US <- barres(vars = "know_treatment_policy", df = e, miss=T, labels="Score knowledge policy video"))
save_plotly(know_treatment_policy_US, width= 850, height=140)

## 5. Climate knowledge

(CC_talks_US <- barres(vars = "CC_talks", df = e, miss=F, labels="How often do you talk about climate change?"))
save_plotly(CC_talks_US, width= 760, height=140)

(CC_real_US <- barres(vars = "CC_real", df = e, miss=F, labels="Climate change real?"))
save_plotly(CC_real_US, width= 540, height=140)

(CC_anthropogenic_US <- barres(vars = "CC_anthropogenic", df = e, miss=F, labels="Part of climate change anthropogenic"))
save_plotly(CC_anthropogenic_US, width= 640, height=140)

(CC_problem_US <- barres(vars = "CC_problem", df = e, miss=F, labels="Climate change is an important problem."))
save_plotly(CC_problem_US, width= 800, height=200)

(CC_knowledgeable_US <- barres(vars = "CC_knowledgeable", df = e, miss=F, labels="How knowledgeable about climate change"))
save_plotly(CC_knowledgeable_US, width= 800, height=140)

(score_GHG_US <- barres(vars = "score_GHG", df = e, miss=T, labels="Knowledge score on GHG"))
save_plotly(score_GHG_US, width= 540, height=140)

(CC_dynamic_US <- barres(vars = "CC_dynamic", df = e, miss=F, labels="Cutting GHG emissions by half <br> sufficient to stop temperatures from rising"))
save_plotly(CC_dynamic_US, width= 540, height=140)

(score_footprint_transport_US <- barres(vars = "score_footprint_transport", df = e, miss=F, labels="Score knowledge transport"))
save_plotly(score_footprint_transport_US, width= 540, height=140)

(score_footprint_food_US <- barres(vars = "score_footprint_food", df = e, miss=F, labels="Score knowledge food"))
save_plotly(score_footprint_food_US, width= 540, height=140)

(score_footprint_region_US <- barres(vars = "score_footprint_region", df = e, miss=F, labels="Score knowledge region"))
save_plotly(score_footprint_region_US, width= 540, height=140)

(score_footprint_elec_US <- barres(vars = "score_footprint_elec", df = e, miss=F, labels="Score knowledge electricity"))
save_plotly(score_footprint_elec_US, width= 540, height=140)

labels_CC_impacts <- c()
for (v in variables_CC_impacts) labels_CC_impacts <- c(labels_CC_impacts, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(CC_impacts_US <- barres(vars = variables_CC_impacts, df = e, miss=F, labels=labels_CC_impacts))
save_plotly(CC_impacts_US, width= 1150, height=325) 

## 6. Climate Change (attitudes and risks)
label_great_deal <- c("Not at all"," A little","Moderately","A lot","A great deal")
labels_responsible <- c()
for (v in variables_responsible_CC) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(CC_responsible_US <- barres(vars = variables_responsible_CC, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_responsible, hover= label_great_deal))
save_plotly(CC_responsible_US, width= 680, height=250) 

(net_zero_feasible_US <- barres(vars = "net_zero_feasible", df = e, miss=T, rev = F, labels="Technically feasible to stop GHG emissions <br> while maintaining satisfactory standards of living in the U.S."))
save_plotly(net_zero_feasible_US, width= 1200, height=140)

(CC_affects_self_US <- barres(vars = "CC_affects_self", df = e, miss = F, labels="Climate change negatively affects your personal life"))
save_plotly(CC_affects_self_US, width= 820, height=140)

(pro_ambitious_policies_US <- barres(vars = "pro_ambitious_policies", df = e, miss = F, labels="Ambitious public policies to halt climate change"))
save_plotly(pro_ambitious_policies_US, width= 820, height=140)

(CC_will_end_US <- barres(vars = "CC_will_end", df = e, miss = F, labels="Likely to halt climate change by the end of the century"))
save_plotly(CC_will_end_US, width= 820, height=140)

(effect_halt_CC_economy_US <- barres(vars = "effect_halt_CC_economy", df = e, miss = F, labels="Effects of ambitious policies <br> on the U.S economy and employment"))
save_plotly(effect_halt_CC_economy_US, width= 820, height=140)

(effect_halt_CC_lifestyle_US <- barres(vars = "effect_halt_CC_lifestyle", df = e, miss = F, labels="Negative effects of ambitious policies on lifestyle"))
save_plotly(effect_halt_CC_lifestyle_US, width= 680, height=140)

labels_willing <- c()
for (v in variables_willing) labels_willing <- c(labels_willing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(willing_US <- barres(vars = variables_willing, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_willing, hover=label_great_deal))
save_plotly(willing_US, width= 820, height=220) 

labels_condition <- c()
for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(condition_US <- barres(vars = variables_condition, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_condition, hover = label_great_deal))
save_plotly(condition_US, width= 820, height=220) 

## 7. Pref 1: emission standards (full)

labels_agree <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
labels_standard_effects <- c()
for (v in variables_standard_effect) labels_standard_effects <- c(labels_standard_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(standard_effect_US <- barres(vars = variables_standard_effect, df = e, rev_color = T , rev = F, miss = F, showLegend=T, labels=labels_standard_effects, hover=labels_agree))
save_plotly(standard_effect_US, width= 1100, height=320) 

labels_win_lose <- c("Lose a lot", "Mostly lose", "Neither win nor lose", "Mostly win", "Win a lot")
labels_standard_win_lose <- c()
for (v in variables_standard_win_lose) labels_standard_win_lose <- c(labels_standard_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_standard_win_lose[5] <- "Your household financially"
(standard_win_lose_US <- barres(vars = variables_standard_win_lose, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_standard_win_lose, hover = labels_win_lose))
save_plotly(standard_win_lose_US, width= 1100, height=320) 

(standard_fair_US <- barres(vars = "standard_fair", df = e, miss=F, labels="An emission limit for cars is fair"))
save_plotly(standard_fair_US, width= 640, height=200)

(standard_support_US <- barres(vars = "standard_support", df = e, miss=F, labels="Support or oppose an emission limit for cars"))
save_plotly(standard_support_US, width= 740, height=200)

(standard_public_transport_support_US <- barres(vars = "standard_public_transport_support", df = e, miss=F, labels="Support or oppose an emission limit for cars <br>with alternatives such as public transports available"))
save_plotly(standard_public_transport_support_US, width= 810, height=240)

## 8. Pref 2: Green investments

labels_investments_effects <- c()
for (v in variables_investments_effect) labels_investments_effects <- c(labels_investments_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_effect_US <- barres(vars = variables_investments_effect, df = e, rev_color = T , rev = F, miss = F, showLegend=T, labels=labels_investments_effects, hover=labels_agree))
save_plotly(investments_effect_US, width= 1100, height=320) 

labels_investments_win_lose <- c()
for (v in variables_investments_win_lose) labels_investments_win_lose <- c(labels_investments_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_investments_win_lose[5] <- "Your household financially"
(investments_win_lose_US <- barres(vars = variables_investments_win_lose, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_investments_win_lose, hover = labels_win_lose))
save_plotly(investments_win_lose_US, width= 1100, height=320) 

(investments_fair_US <- barres(vars = "investments_fair", df = e, miss=F, labels="An emission limit for cars is fair"))
save_plotly(investments_fair_US, width= 640, height=200)

(investments_support_US <- barres(vars = "investments_support", df = e, miss=F, labels="Support or oppose an emission limit for cars"))
save_plotly(investments_support_US, width= 740, height=200)

labels_investments_funding <- c()
for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_funding_pnr_US <- barres(vars = variables_investments_funding, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
save_plotly(investments_funding_pnr_US, width= 880, height=143)  

## 9. Pref 3: Tax and dividend
labels_tax_transfers_effects <- c()
for (v in variables_tax_transfers_effect) labels_tax_transfers_effects <- c(labels_tax_transfers_effects, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(tax_transfers_effect_US <- barres(vars = variables_tax_transfers_effect, df = e, rev_color = T , rev = F, miss = F, showLegend=T, labels=labels_tax_transfers_effects, hover=labels_agree))
save_plotly(tax_transfers_effect_US, width= 1100, height=320) 

labels_tax_transfers_win_lose <- c()
for (v in variables_tax_transfers_win_lose) labels_tax_transfers_win_lose <- c(labels_tax_transfers_win_lose, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_tax_transfers_win_lose[5] <- "Your household financially"
(tax_transfers_win_lose_US <- barres(vars = variables_tax_transfers_win_lose, df = e, rev_color = T,rev = F, miss = F, showLegend=T, labels=labels_tax_transfers_win_lose, hover = labels_win_lose))
save_plotly(tax_transfers_win_lose_US, width= 1100, height=320) 

(tax_transfers_fair_US <- barres(vars = "tax_transfers_fair", df = e, miss=F, labels="An emission limit for cars is fair"))
save_plotly(tax_transfers_fair_US, width= 640, height=200)

(tax_transfers_support_US <- barres(vars = "tax_transfers_support", df = e, miss=F, labels="Support or oppose an emission limit for cars"))
save_plotly(tax_transfers_support_US, width= 740, height=200)


## 10. Pref on climate policies

# BP TODO: for the trick question on ABC/CNBC --> do we need a graph. If yes, just need nbr of people tricked.
labels_support <- c("Strongly oppose", "Somewhat oppose", "Indifferent", "Somewhat support", "Strongly support")
labels_policy <- c()
for (v in variables_policy) labels_policy <- c(labels_policy, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(policy_US <- barres(vars = variables_policy, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_policy, hover=labels_support))
save_plotly(policy_US, width= 1500, height=400)

labels_tax <- c()
variables_tax <- variables_tax[1:9]#BP: removed tax_1p_support
for (v in variables_tax) labels_tax <- c(labels_tax, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(tax_US <- barres(vars = variables_tax, df = e, rev = F, miss = F, showLegend=T, labels=labels_tax, hover=labels_support))
save_plotly(tax_US, width= 1500, height=400)

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
title(ylab=expression("Proportion <= x"), xlab="WTP (in $/year)", line=2.3)
grid() # TODO legend
par(mar = mar_old, cex = cex_old)


(donation_US <- barres(vars = "donation", df = e, miss=F, rev = F, color = color(20, theme = "rainbow"), labels="Donation to climate charity ($/year)"))
save_plotly(donation_US, width= 1050, height=200)

(donation_agg_US <- barres(vars = "donation_agg", df = e, miss=F, rev = F, rev_color = T, labels="Donation to climate charity ($/year)"))
save_plotly(donation_agg_US, width= 950, height=140)

mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
cdf_donation_US <- Ecdf(e$donation, weights = e$weight)
plot(cdf_donation_US$x, cdf_donation_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
title(ylab=expression("Proportion <= x"), xlab="Donation (in $/year)", line=2.3)
grid() # TODO legend
par(mar = mar_old, cex = cex_old)

## 12. International burden-sharing

labels_scale <- c()
for (v in variables_scale) labels_scale <- c(labels_scale, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(scale_US <- barres(vars = variables_scale, df = e, rev = F, miss = T, showLegend=F, labels=labels_scale))
save_plotly(scale_US, width= 460, height=250) 

(should_fight_CC_US <- barres(vars = "should_fight_CC", df = e, miss=F, rev = F, rev_color = T, labels=" U.S. should take measures to fight climate change"))
save_plotly(should_fight_CC_US, width= 950, height=140)

labels_if_other_do <- c()
for (v in variables_if_other_do) labels_if_other_do <- c(labels_if_other_do, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(if_other_do_US <- barres(vars = variables_if_other_do, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_if_other_do, hover=c("Much less", "Less", "About the same", "More", "Much more")))
save_plotly(if_other_do_US, width= 260, height=250) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_burden_sharing[4] <- "The richest countries should pay it all, <br> so that the poorest countries do not have to pay anything"
labels_burden_sharing[3] <- "Countries should pay in proportion to <br> their past emissions (from 1990 onwards)"
labels_burden_sharing[5] <- "The richest countries should pay even more <br> to help vulnerable countries face adverse consequences" 
(burden_sharing_US <- barres(vars = variables_burden_sharing, df = e, miss=F, labels=labels_burden_sharing))
save_plotly(burden_sharing_US, width= 1150, height=325) 

(global_assembly_support_US <- barres(vars = "global_assembly_support", df = e, miss=T, labels="Support for global democratic assembly"))
save_plotly(global_assembly_support_US, width= 1200, height=140)

(global_tax_support_US <- barres(vars = "global_tax_support", df = e, miss=F, labels="Support for global tax on GHG to finance a global basic income"))
save_plotly(global_tax_support_US, width= 1050, height=140)

(tax_1p_support_US <- barres(vars = "tax_1p_support", df = e, miss=F, labels="Support tax on millionaires <br> to finance low-income countries"))
save_plotly(tax_1p_support_US, width= 1050, height=140)


## 13. Pref for bans vs. incentives

(will_insulate_US <- barres(vars = "will_insulate", df = e, miss=F, labels="Likely to insulate or replace heating system <br> over the next 5 years."))
save_plotly(will_insulate_US, width= 550, height=140)

labels_obstacles_insulation <- c()
for (v in variables_obstacles_insulation) labels_obstacles_insulation <- c(labels_obstacles_insulation, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
labels_obstacles_insulation[1] <- "Not applicable"
(obstacles_insulation_US <- barres(vars = variables_obstacles_insulation, df = e, rev = F, miss = T, showLegend=F, labels=labels_obstacles_insulation))
save_plotly(obstacles_insulation_US, width= 1050, height=250) 

(insulation_subsidies_support_US <- barres(vars = "insulation_subsidies_support", df = e, miss=F, labels="Support for subsidies of insulation"))
save_plotly(insulation_subsidies_support_US, width= 850, height=170)

(insulation_mandatory_support_US <- barres(vars = "insulation_mandatory_support", df = e, miss=F, labels="Support for making insulation mandatory"))
save_plotly(insulation_mandatory_support_US, width= 850, height=170)

labels_beef <- c()
for (v in variables_beef) labels_beef <- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(beef_US <- barres(vars = variables_beef, df = e, rev = F, miss = F, labels=labels_beef,showLegend=T, hover= labels_support))
save_plotly(beef_US, width= 900, height=240)  


## 14. Trust, perceptions of institutions, etc.

(can_trust_people_US <- barres(vars = "can_trust_people", df = e, miss=T, rev_color = T, rev=F, labels="Trust other people"))
save_plotly(can_trust_people_US, width= 815, height=140)

(can_trust_govt_US <- barres(vars = "can_trust_govt", df = e, miss=T, rev_color = T, rev=F, labels="Trust the U.S. federal government <br> over the last decade to do what is right"))
save_plotly(can_trust_govt_US, width= 800, height=140)

(view_govt_US <- barres(vars = "view_govt", df = e, miss=T, rev_color = T, rev=F, labels="Pro government intervention"))
save_plotly(view_govt_US, width= 800, height=140)

(problem_inequality_US <- barres(vars = "problem_inequality", df = e, miss=F, labels="Importance income inequality is in the U.S."))
save_plotly(problem_inequality_US, width= 1120, height=140)

(future_richness_US <- barres(vars = "future_richness", df = e, miss=F, labels="Will people in the world be richer in 100 years from now"))
save_plotly(future_richness_US, width= 850, height=140)

## 15. Political views

(interested_politics_US <- barres(vars = "interested_politics", df = e, miss=T, labels="Interested in politics"))
save_plotly(interested_politics_US, width= 800, height=140)

(member_environmental_orga_US <- barres(vars = "member_environmental_orga", df = e, miss=F, labels="Member of an<br> environmental organization"))
save_plotly(member_environmental_orga_US, width= 622, height=140)

(relative_environmentalist_US <- barres(vars = "relative_environmentalist", df = e, miss=F, labels="Has an environmentalist relative"))
save_plotly(relative_environmentalist_US, width= 540, height=140)

(relative_environmentalist_US <- barres(vars = "relative_environmentalist", df = e, miss=F, labels="Has an environmentalist relative"))
save_plotly(relative_environmentalist_US, width= 540, height=140)

(vote_participation_US <- barres(vars = "vote_participation", df = e, miss=T, labels="Voted in 2020 election"))
save_plotly(vote_participation_US, width= 540, height=140) 

(vote_participation_2016_US <- barres(vars = "vote_participation_2016", df = e, miss=T, labels="Voted in 2016 election"))
save_plotly(vote_participation_2016_US, width= 540, height=140) 

(vote_US <- barres(vars = "vote", df = e, rev_color = T, miss=T, labels="In 2020, voted for (voters)"))
save_plotly(vote_US, width= 650, height=140) 

(vote_2016_US <- barres(vars = "vote_2016", df = e, rev_color = T, miss=T, labels="In 2016, voted for (voters)"))
save_plotly(vote_2016_US, width= 650, height=140) 

(liberal_conservative_US <- barres(vars = "liberal_conservative", df = e, rev_color = T, miss=T, labels="On economic policy matters, where do you see yourself on the liberal/conservative spectrum?"))
save_plotly(liberal_conservative_US, width= 650, height=140)

(political_affiliation_US <- barres(vars = "political_affiliation", df = e, rev_color = T, miss=F, labels="Political affiliation"))
save_plotly(political_affiliation_US, width= 800, height=140)

## 16. Feedback

(survey_biased_US <- barres(vars = "survey_biased", df = e, rev_color = T, miss=F, labels="Survey biased"))
save_plotly(survey_biased_US, width= 810, height=140)
