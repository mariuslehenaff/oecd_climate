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

# Not in wave 3
#(speak_US <- barres(vars = "speaks_well", df = e, miss=F, labels="How well do you speak english?"))
#save_plotly(speak_US, width= 750, height=140)

(education_US <- barres(vars = "education", df = e, miss=F, rev_color = T, rev = F, labels="Highest level of education"))
save_plotly(education_US, width= 1080, height=140) 

(employment_status_US <- barres(vars = "employment_agg", df = e, miss=F, labels="What is your employment status?"))
save_plotly(employment_status_US, width= 630, height=140)

##BP need to check those 2 (NEW)TODO

(occupation_US <- barres(vars = "occupation", df = e, miss=F, labels="Which category best describes your main occupation?"))
save_plotly(occupation_US, width= 630, height=140)

(sector_US <- barres(vars = "sector", df = e, miss=F, labels="What is your primary line of business?"))
save_plotly(sector_US, width= 630, height=140)

## Problem with this one BP: still a pb in wave 3?
(hit_by_covid_US <- barres(vars = "hit_by_covid", df = e, miss=F, labels="Hit by covid"))
save_plotly(hit_by_covid_US, width= 420, height=140)

## BP: following 5 are new

# TODO: need to create aggregate, factor from the different home variables
(home_US <- barres(vars = "home", df = e, miss=F, labels="Are you a homeowner or a tenant?"))
save_plotly(home_US, width= 630, height=140)

(couple_US <- barres(vars = "couple", df = e, miss=F, labels="Do you live with your partner?"))
save_plotly(couple_US, width= 630, height=140)

(marital_status_US <- barres(vars = "marital_status", df = e, miss=F, labels="What is your marital status?"))
save_plotly(marital_status_US, width= 630, height=140)

# BP: do we need ceiling_4 variable here, or can we just use nb_children ?
(nb_children_US <- barres(vars = "Nb_children", df = e, miss=F, rev_color = T, rev = F, labels="Number of children", legend = c(0:3, "4 or more")))
save_plotly(nb_children_US, width= 470, height=140)


(hh_size_US <- barres(vars = "HH_size", df = e, miss=F, labels="People in household"))
save_plotly(hh_size_US, width= 470, height=140)

## 2. HH composition and energy characteristics

(heating_US <- barres(vars = "heating", df = e, miss=T, labels="Heating type", rev = F, legend=c("Electricity", "Gas", "Oil", "Other", "PNR")))
save_plotly(heating_US, width= 525, height=140)

## BP : Following 3 new 
#BP: need to be relabelled. Is the first option a PNR?
(heating_expenses_US <- barres(vars = "heating_expenses", df = e, miss=F, labels="Monthly heating expenses", rev = F))
save_plotly(heating_expenses_US, width= 525, height=140)

(insulation_US <- barres(vars = "insulation", df = e, miss=F, labels="How do you rate the insulation of your accommodation?", rev = F))
save_plotly(insulation_US, width= 525, height=140)

(gas_expenses_US <- barres(vars = "gas_expenses", df = e, miss=F, labels="Monthly gas expenses", rev = F))
save_plotly(gas_expenses_US, width= 525, height=140)

#BP: still relevant with new scale?
#(flights_US <- barres(vars = "flights_agg", rev = F, df = e, miss=F, labels="How many round-trip flights did you<br>take between 2015 and 2019?"))
#save_plotly(flights_US, width= 655, height=140)

# BP: Updated for wave 3. TODO need to modify convert() if we want to use flights_agg instead of flight
(flights_desagg_US <- barres(vars = "flights_agg", df = e, miss=F, labels="How many round-trip flights did you take in 2019?"))
save_plotly(flights_desagg_US, width= 1194, height=140)

# BP: do we still need those with wave 3 ?
#mar_old <- par()$mar
#cex_old <- par()$cex
#par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
#cdf_flights_US <- Ecdf(e$flights, weights = e$weight)
#plot(cdf_flights_US$x, cdf_flights_US$y, lwd=2, log='x', type='s', col="red", xlab="", ylab="")
#title(ylab=expression("Proportion <= x"), xlab="Number of flights from 2015 to 2019", line=2.3)
#grid() 
#par(mar = mar_old, cex = cex_old)
#
#(km_driven_US <- barres(vars = "km_driven_agg", df = e, miss=F, rev = F, labels="How many km have your household driven in 2019?"))
#save_plotly(km_driven_US, width= 1100, height=140) 
#
#mar_old <- par()$mar
#cex_old <- par()$cex
#par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
#cdf_km_US <- Ecdf(e$km_driven, weights = e$weight)
#plot(cdf_km_US$x, cdf_km_US$y, lwd=2, xlim=c(0, 40000), type='s', col="red", xlab="", ylab="")
#title(ylab=expression("Proportion <= x"), xlab="km driven by househould in 2019", line=2.3)
#grid() 
#par(mar = mar_old, cex = cex_old)
#
(frequency_beef_US <- barres(vars = "frequency_beef", df = e, miss=F, rev = F, labels="How often do you eat beef?"))
save_plotly(frequency_beef_US, width= 530, height=140)

variables_transport_graph <- c("transport_work", "transport_shopping", "transport_leisure")
labels_transport <- c("Work", "Shopping", "Leisure")
(transport_US <- barres(vars = variables_transport_graph, df = e, rev = F, miss = T, labels=labels_transport))
save_plotly(transport_US, width= 750, height=235) 

(availability_transport_US <- barres(vars = "availability_transport", df = e, miss=F, labels="Is public transport available near you live?"))
save_plotly(availability_transport_US, width= 850, height=140)


## POST-TREATMENT

## BP: New blocks (following 3)
## 3. Treatment feedback: local climate
(watched_climate_US <- barres(vars = "watched_climate", df = e, miss=F, labels="Able to watch the video until the end (local)"))
save_plotly(watched_climate_US, width= 850, height=140)

#BP: For the two following questions. Shouldn't we just label responses as % of right and wrong answer? And give title: "Knowledge question 1: rise in global average temperature in 2100"
(know_temperature_2100_US <- barres(vars = "know_temperature_2100", df = e, miss=T, labels="What will be the rise in global average temperature in 2100 if greenhouse gas emissions continue on their current trend?"))
save_plotly(know_temperature_2100_US, width= 850, height=140)

(know_frequence_heatwaves_US <- barres(vars = "know_frequence_heatwaves", df = e, miss=T, labels="Absent of ambitious action against climate change, how frequent will extreme temperatures occur on average across the U.S.S by the end of the century?"))
save_plotly(know_frequence_heatwaves_US, width= 850, height=140)

## 4. Treatment feedback: local policy
(watched_policy_US <- barres(vars = "watched_policy", df = e, miss=F, labels="Able to watch the video until the end (policy)"))
save_plotly(watched_policy_US, width= 850, height=140)

#BP: same comment as above for labels
(know_investments_jobs_US <- barres(vars = "know_investments_jobs", df = e, miss=T, labels="How many people could find a job in green sectors in the U.S.?"))
save_plotly(know_investments_jobs, width= 850, height=140)

(know_standard_US <- barres(vars = "know_standard", df = e, miss=T, labels="What is the emission limit described in the video?"))
save_plotly(know_standard_US, width= 850, height=140)

## 5. Climate knowledge

(CC_talks_US <- barres(vars = "CC_talks", df = e, miss=F, labels="How often do you talk about climate change?"))
save_plotly(CC_talks_US, width= 540, height=140)

(CC_real_US <- barres(vars = "CC_real", df = e, miss=F, labels="In your opinion is climate change real?"))
save_plotly(CC_real_US, width= 540, height=140)

(CC_antrhopogenic_US <- barres(vars = "CC_antrhopogenic", df = e, miss=F, labels="What part of climate change do you think is due to human activity"))
save_plotly(CC_antrhopogenic_US, width= 540, height=140)

(CC_problem_US <- barres(vars = "CC_problem", df = e, miss=F, labels="Do you agree or disagree with the following statement: 'Climate change is an important problem.'"))
save_plotly(CC_problem_US, width= 540, height=140)

(CC_knowledgeable_US <- barres(vars = "CC_knowledgeable", df = e, miss=F, labels="How knowledgeable do you consider yourself about climate change?"))
save_plotly(CC_knowledgeable_US, width= 540, height=140)

# BP: Need to create aggregate GHG variable? Do you merge all the 5 variables, or do we make 5 different graphs?
(GHG_agg_US <- barres(vars = "GHG_agg", df = e, miss=T, labels="Which of the following elements contribute to climate change?"))
save_plotly(GHG_agg_US, width= 540, height=140)

(CC_dynamic_US <- barres(vars = "CC_dynamic", df = e, miss=F, labels="Do you think that cutting global GHG emissions by half would be sufficient to eventually stop temperatures from rising?"))
save_plotly(CC_dynamic_US, width= 540, height=140)

# BP: not sure how to handle ranking questions. Maybe the best is to have separate graphs for each response.
(footprint_tr_order_car_US <- barres(vars = "footprint_tr_order_car", df = e, miss=F, labels="If a family of 4 travels 500 miles from New York to Toronto, which mode of transportation emits the most GHG? <br> Rank of Car"))
save_plotly(footprint_tr_order_car_US, width= 540, height=140)
(footprint_tr_order_coach_US <- barres(vars = "footprint_tr_order_coach", df = e, miss=F, labels="If a family of 4 travels 500 miles from New York to Toronto, which mode of transportation emits the most GHG? <br> Rank of Coach"))
save_plotly(footprint_tr_order_coach_US, width= 540, height=140)
(footprint_tr_order_plane_US <- barres(vars = "footprint_tr_order_plane", df = e, miss=F, labels="If a family of 4 travels 500 miles from New York to Toronto, which mode of transportation emits the most GHG? <br> Rank of Plane"))
save_plotly(footprint_tr_order_plane_US, width= 540, height=140)

(footprint_fd_order_beef_US <- barres(vars = "footprint_fd_order_beef", df = e, miss=F, labels="Which dish emits the most GHG? <br> Rank of Beef"))
save_plotly(footprint_fd_order_beef_US, width= 540, height=140)
(footprint_fd_order_pasta_US <- barres(vars = "footprint_fd_order_pasta", df = e, miss=F, labels="Which dish emits the most GHG? <br> Rank of Pasta"))
save_plotly(footprint_fd_order_pasta_US, width= 540, height=140)
(footprint_fd_order_chicken_US <- barres(vars = "footprint_fd_order_chicken", df = e, miss=F, labels="Which dish emits the most GHG? <br> Rank of Chicken"))
save_plotly(footprint_fd_order_chicken_US, width= 540, height=140)

(footprint_el_order_gas_US <- barres(vars = "footprint_el_order_gas", df = e, miss=F, labels="Which source of electricity energy emits the most GHG to provide power for a house? <br> Rank of Gas-fired power plant"))
save_plotly(footprint_el_order_gas_US, width= 540, height=140)
(footprint_el_order_wind_US <- barres(vars = "footprint_el_order_wind", df = e, miss=F, labels="Which source of electricity energy emits the most GHG to provide power for a house? <br> Rank of Wind turbines"))
save_plotly(footprint_el_order_wind_US, width= 540, height=140)
(footprint_el_order_coal_US <- barres(vars = "footprint_el_order_coal", df = e, miss=F, labels="Which source of electricity energy emits the most GHG to provide power for a house? <br> Rank of Coal-fired power station"))
save_plotly(footprint_el_order_coal_US, width= 540, height=140)

(footprint_reg_order_US_US <- barres(vars = "footprint_reg_order_US", df = e, miss=F, labels="In which region does the consumption of a typical person contribute most to climate change? <br> Rank of the U.S."))
save_plotly(footprint_reg_order_US_US, width= 540, height=140)
(footprint_reg_order_europe_US <- barres(vars = "footprint_reg_order_europe", df = e, miss=F, labels="In which region does the consumption of a typical person contribute most to climate change? <br> Rank of Western Europe"))
save_plotly(footprint_reg_order_europe_US, width= 540, height=140)
(footprint_reg_order_china_US <- barres(vars = "footprint_reg_order_china", df = e, miss=F, labels="In which region does the consumption of a typical person contribute most to climate change? <br> Rank of China"))
save_plotly(footprint_reg_order_china_US, width= 540, height=140)
(footprint_reg_order_india_US <- barres(vars = "footprint_reg_order_india", df = e, miss=F, labels="In which region does the consumption of a typical person contribute most to climate change? <br> Rank of India"))
save_plotly(footprint_reg_order_india_US, width= 540, height=140)

## BP: need to create variables_CC_impacts. Why in preparation footprint_reg_US come after CC_impacts (other way around in questionnaire)
labels_CC_impacts <- c()
for (v in variables_CC_impacts) labels_CC_impacts <- c(labels_CC_impacts, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(CC_impacts_US <- barres(vars = variables_CC_impacts, df = e, miss=F, labels=labels_CC_impacts))
save_plotly(CC_impacts_US, width= 1150, height=325) 

## 6. Climate Change (attitudes and risks)

# BP : name CC responsible or responsible CC?
labels_responsible <- c()
for (v in variables_CC_responsible) labels_responsible <- c(labels_responsible, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(CC_responsible_US <- barres(vars = variables_CC_responsible, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_responsible, hover=labels_responsible))
save_plotly(CC_responsible_US, width= 575, height=320) 

# BP : CC_stoppable or net_zero-feasible?
(CC_stoppable_US <- barres(vars = "CC_stoppable", df = e, miss=T, rev = F, labels="Technically feasible to stop GHG emissions while maintaining satisfactory standards of living in the U.S.?"))
save_plotly(CC_stoppable_US, width= 1200, height=140)

(CC_affects_self_US <- barres(vars = "CC_affects_self", df = e, miss = F, labels="To what extent do you think climate change already affects or will negatively affect your personal life?"))
save_plotly(CC_affects_self_US, width= 680, height=140)

(pro_ambitious_policies_US <- barres(vars = "pro_ambitious_policies", df = e, miss = F, labels="How ambitious do you think public policies should be to halt climate change?"))
save_plotly(pro_ambitious_policies_US, width= 680, height=140)

(CC_will_end_US <- barres(vars = "CC_will_end", df = e, miss = F, labels="How likely is it that human kind halt climate change by the end of the century?"))
save_plotly(CC_will_end_US, width= 680, height=140)

(effect_halt_CC_economy_US <- barres(vars = "effect_halt_CC_economy", df = e, miss = F, labels="If we decide to halt climate change through ambitious policies, what would be the effects on the U.S economy and employment?"))
save_plotly(effect_halt_CC_economy_US, width= 680, height=140)

(effect_halt_CC_lifestyle_US <- barres(vars = "effect_halt_CC_lifestyle", df = e, miss = F, labels="If we decide to halt climate change through ambitious policies, to what extent do you think it would negatively affect your lifestyle?"))
save_plotly(effect_halt_CC_lifestyle_US, width= 680, height=140)

# BP : need to create variables_willing in preparation?
labels_willing <- c()
for (v in variables_willing) labels_willing <- c(labels_willing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(willing_US <- barres(vars = variables_willing, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_willing, hover=labels_willing))
save_plotly(willing_US, width= 575, height=320) 

# BP : need to create variables_condition in preparation?
labels_condition <- c()
for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(condition_US <- barres(vars = variables_condition, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_condition, hover=labels_condition))
save_plotly(condition_US, width= 575, height=320) 

## 7. Pref 1: emission standards (full)

# BP: Need to create variable_standard ?
labels_standard <- c()
for (v in variables_standard) labels_standard <- c(labels_standard, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(standard_US <- barres(vars = variables_standard, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_standard, hover=labels_standard))
save_plotly(standard_US, width= 575, height=320) 

# BP: Need to create variable_standard_incidence ?
labels_standard_incidence <- c()
for (v in variables_standard_incidence) labels_standard_incidence <- c(labels_standard_incidence, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(standard_incidence_US <- barres(vars = variables_standard_incidence, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_standard_incidence, hover=labels_standard_incidence))
save_plotly(standard_incidence_US, width= 575, height=320) 

(standard_incidence_self_US <- barres(vars = "standard_incidence_self", df = e, miss=F, labels="Do you think that financially your household would win or lose from an emission limit for cars?"))
save_plotly(standard_incidence_self_US, width= 600, height=140)

(standard_fair_US <- barres(vars = "standard_fair", df = e, miss=F, labels="Do you agree or disagree with the following statement: 'An emission limit for cars is fair'?"))
save_plotly(standard_fair_US, width= 600, height=140)

(standard_support_US <- barres(vars = "standard_support", df = e, miss=F, labels="Do you support or oppose an emission limit for cars?"))
save_plotly(standard_support_US, width= 600, height=140)


(standard_public_transport_support_US <- barres(vars = "standard_public_transport_support", df = e, miss=F, labels="Do you support or oppose an emission limit for cars where alternatives such as public transports are made available to people?"))
save_plotly(standard_public_transport_support_US, width= 600, height=140)

## 8. Pref 2: Green investments

# BP: Need to create variable_investments ?
labels_investments <- c()
for (v in variables_investments) labels_investments <- c(labels_investments, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_US <- barres(vars = variables_investments, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_investments, hover=labels_investments))
save_plotly(investments_US, width= 575, height=320) 

# BP: Need to create variable_investments_incidence ?
labels_investments_incidence <- c()
for (v in variables_investments_incidence) labels_investments_incidence <- c(labels_investments_incidence, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_incidence_US <- barres(vars = variables_investments_incidence, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_investments_incidence, hover=labels_investments_incidence))
save_plotly(investments_incidence_US, width= 575, height=320) 

(investments_incidence_self_US <- barres(vars = "investments_incidence_self", df = e, miss=F, labels="Do you think that financially your household would win or lose from an emission limit for cars?"))
save_plotly(investments_incidence_self_US, width= 600, height=140)

(investments_fair_US <- barres(vars = "investments_fair", df = e, miss=F, labels="Do you agree or disagree with the following statement: 'An emission limit for cars is fair'?"))
save_plotly(investments_fair_US, width= 600, height=140)

(investments_support_US <- barres(vars = "investments_support", df = e, miss=F, labels="Do you support or oppose an emission limit for cars?"))
save_plotly(investments_support_US, width= 600, height=140)

# BP : need to create variable_investments_funding
labels_investments_funding <- c()
for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(investments_funding_pnr_US <- barres(vars = variables_investments_funding, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
save_plotly(investments_funding_pnr_US, width= 640, height=280)  

## 9. Pref 3: Tax and dividend

# BP: Need to create variable_tax_transfers ?
labels_tax_transfers <- c()
for (v in variables_tax_transfers) labels_tax_transfers <- c(labels_tax_transfers, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(tax_transfers_US <- barres(vars = variables_tax_transfers, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_tax_transfers, hover=labels_tax_transfers))
save_plotly(tax_transfers_US, width= 575, height=320) 

# BP: Need to create variable_tax_transfers_incidence ?
labels_tax_transfers_incidence <- c()
for (v in variables_tax_transfers_incidence) labels_tax_transfers_incidence <- c(labels_tax_transfers_incidence, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(tax_transfers_incidence_US <- barres(vars = variables_tax_transfers_incidence, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_tax_transfers_incidence, hover=labels_tax_transfers_incidence))
save_plotly(tax_transfers_incidence_US, width= 575, height=320) 

(tax_transfers_incidence_self_US <- barres(vars = "tax_transfers_incidence_self", df = e, miss=F, labels="Do you think that financially your household would win or lose from an emission limit for cars?"))
save_plotly(tax_transfers_incidence_self_US, width= 600, height=140)

(tax_transfers_fair_US <- barres(vars = "tax_transfers_fair", df = e, miss=F, labels="Do you agree or disagree with the following statement: 'An emission limit for cars is fair'?"))
save_plotly(tax_transfers_fair_US, width= 600, height=140)

(tax_transfers_support_US <- barres(vars = "tax_transfers_support", df = e, miss=F, labels="Do you support or oppose an emission limit for cars?"))
save_plotly(tax_transfers_support_US, width= 600, height=140)


## 10. Pref on climate policies

# BP : for the trick question on ABC/CNBC --> do we need a graph. If yes, just need nbr of people tricked.

# BP: Need to create/update variable_policy ?
labels_policy <- c()
for (v in variable_policy) labels_policy <- c(labels_policy, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(policy_US <- barres(vars = variable_policy, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_policy, hover=labels_policy

# BP: Need to create/update variable_tax ?
labels_tax <- c()
for (v in variable_tax) labels_tax <- c(labels_tax, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(tax_US <- barres(vars = variable_tax, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_tax, hover=labels_tax

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


# BP: need to adapt the code here ?
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
labels_scale[5] <- "PNR"
(scale_US <- barres(vars = variables_scale, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_scale, hover=labels_scale))
save_plotly(scale_US, width= 260, height=250) 

(should_fight_CC_US <- barres(vars = "should_fight_CC", df = e, miss=F, rev = F, rev_color = T, labels="Do you agree or disagree with the following statement: 'The U.S. should take measures to fight climate change.'"))
save_plotly(should_fight_CC_US, width= 950, height=140)

# BP : need to create variables_if_other_do
labels_if_other_do <- c()
for (v in variables_if_other_do) labels_if_other_do <- c(labels_if_other_do, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(if_other_do_US <- barres(vars = variables_if_other_do, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_if_other_do, hover=labels_if_other_do))
save_plotly(if_other_do_US, width= 260, height=250) 

labels_burden_sharing <- c()
for (v in variables_burden_sharing) labels_burden_sharing <- c(labels_burden_sharing, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
#labels_burden_sharing[3] <- "Countries should pay in proportion to their past emissions <br> (from 1990 onwards)"
#labels_burden_sharing[4] <- "The richest countries should pay it all, so that<br> the poorest countries do not have to pay anything"
#labels_burden_sharing[5] <- "The richest countries should pay even more, <br> vulnerable countries would then receive money instead of paying"
(burden_sharing_US <- barres(vars = variables_burden_sharing, df = e, miss=F, labels=labels_burden_sharing))
save_plotly(burden_sharing_US, width= 1150, height=325) 

(global_assembly_support_US <- barres(vars = "global_assembly_support", df = e, miss=T, labels="Support for global democratic assembly"))
save_plotly(global_assembly_support_US, width= 1200, height=140)

(global_tax_support_US <- barres(vars = "global_tax_support", df = e, miss=F, labels="Support for global tax on GHG to finance a global basic income"))
save_plotly(global_tax_support_US, width= 1050, height=140)

(tax_1p_support_US <- barres(vars = "tax_1p_support", df = e, miss=F, labels="Support tax on millionaires to finance low-income countries"))
save_plotly(tax_1p_support_US, width= 600, height=140)


## 13. Pref for bans vs. incentives

(will_insulate_US <- barres(vars = "will_insulate", df = e, miss=F, labels="Likely to insulate or replace heating system of your home over the next 5 years."))
save_plotly(will_insulate_US, width= 550, height=140)

# BP : need to create variables_obstacles_insulation
labels_obstacles_insulation <- c()
for (v in variables_obstacles_insulation) labels_obstacles_insulation <- c(labels_obstacles_insulation, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
(obstacles_insulation_US <- barres(vars = variables_obstacles_insulation, df = e, error_margin=T, rev = F, miss = F, showLegend=F, labels=labels_obstacles_insulation, hover=labels_obstacles_insulation))
save_plotly(obstacles_insulation_US, width= 260, height=250) 

(insulation_subsidies_support_US <- barres(vars = "insulation_subsidies_support", df = e, miss=F, labels="Support for subsidies of insulation"))
save_plotly(insulation_subsidies_support_US, width= 550, height=140)

(insulation_mandatory_support_US <- barres(vars = "insulation_mandatory_support", df = e, miss=F, labels="Support for making insulation mandatory"))
save_plotly(insulation_mandatory_support_US, width= 550, height=140)

# BP : need to update variables_beef ? Also why beef pnr 
labels_beef <- c()
for (v in variables_beef) labels_beef <- c(labels_beef, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
#labels_beef[1] <- "A high tax on cattle products,<br>so that the price of beef doubles"
#labels_beef[2] <- "Subsidies on organic and local<br>vegetables, fruits and nuts"
#labels_beef[3] <- "Removal of subsidies for cattle farming"
# labels_beef <- c("Tax on cattle products (beefx2)", "Sub. Vegetables", "No sub. cattle", "Ban intensive cattle", "PNR")
(beef_US <- barres(vars = variables_beef, df = e, rev = F, miss = F, labels=labels_beef,showLegend=F))
save_plotly(beef_US, width= 640, height=280)  


## 14. Trust, perceptions of institutions, etc.

(can_trust_people_US <- barres(vars = "can_trust_people", df = e, miss=T, rev_color = T, rev=F, labels="Trust other people"))
save_plotly(can_trust_people_US, width= 815, height=140)

(can_trust_govt_US <- barres(vars = "can_trust_govt", df = e, miss=T, rev_color = T, rev=F, labels="Over the last decade the U.S. federal government could generally be trusted to do what is right"))
save_plotly(can_trust_govt_US, width= 900, height=140)

(statist_US <- barres(vars = "statist", df = e, miss=T, rev_color = T, rev=F, labels="Pro government intervention"))
save_plotly(statist_US, width= 530, height=140)

(problem_inequality_US <- barres(vars = "problem_inequality", df = e, miss=F, labels="How big of an issue do you think income inequality is in the U.S.?"))
save_plotly(problem_inequality_US, width= 1120, height=140)

(future_richness_US <- barres(vars = "future_richness", df = e, miss=F, labels="Do you think that overall people in the world will be richer or poorer in 100 years from now?"))
save_plotly(future_richness_US, width= 770, height=140)

## 15. Political views

(interested_politics_US <- barres(vars = "interested_politics", df = e, miss=T, labels="Interested in politics"))
save_plotly(interested_politics_US, width= 600, height=140)

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

#BP: do we merge voters and non-voters into 1 variable?
(vote_voters_US <- barres(vars = "vote_voters", df = e, rev_color = T, miss=T, labels="In 2020, voted for (voters)"))
save_plotly(vote_voters_US, width= 650, height=140) 
(vote_non_voters_US <- barres(vars = "vote_non_voters", df = e, rev_color = T, miss=T, labels="In 2020, voted for (non-voters)"))
save_plotly(vote_non_voters_US, width= 650, height=140) 

(vote_voters_2016_US <- barres(vars = "vote_voters_2016", df = e, rev_color = T, miss=T, labels="In 2016, voted for (voters)"))
save_plotly(vote_voters_2016_US, width= 650, height=140) 
(vote_non_voters_2016_US <- barres(vars = "vote_non_voters_2016", df = e, rev_color = T, miss=T, labels="In 2016, voted for (non-voters)"))
save_plotly(vote_non_voters_2016_US, width= 650, height=140)

(liberal_conservative_US <- barres(vars = "liberal_conservative", df = e, rev_color = T, miss=T, labels="On economic policy matters, where do you see yourself on the liberal/conservative spectrum?"))
save_plotly(liberal_conservative_US, width= 650, height=140)

(political_affiliation_US <- barres(vars = "political_affiliation", df = e, rev_color = T, miss=F, labels="Political affiliation"))
save_plotly(political_affiliation_US, width= 650, height=140)

## 16. Feedback

(survey_biased_US <- barres(vars = "survey_biased", df = e, rev_color = T, miss=F, labels="Survey biased"))
save_plotly(survey_biased_US, width= 810, height=140) # TODO
