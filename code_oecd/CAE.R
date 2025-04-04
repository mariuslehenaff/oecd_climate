##### Data preparation #####
# The objects of preparation.R should be defined, including the dataframes containing the survey data.
# The functions from .Rprofile, heterogeneity_graph.R and render.R should be defined.
update_constant(fr) # To restrict the data to FR and defines constant for FR analysis. 
# update_constant(all) may be needed at the beginning of international comparisons. Such "international" sections should be then followed by update_constant(fr).


##### Knowledge: Descriptive stats #####


##### Knowledge: Socio-demographic determinants #####
# TODO? replace OLS by logit for binary outcomes?
folder <- "../figures/FR/"
replacement_text <- "_FR"
(reg_anthropogenic_knowledge_A <- modelplot(list("CC anthropogenic" = lm(as.formula(paste("CC_anthropogenic > 0 ~ ", paste(rev(setA), collapse = ' + '))), data = e, weights = e$weight),
                                                 "Index knowledge" = lm(as.formula(paste("index_knowledge ~ ", paste(rev(setA), collapse = ' + '))), data = e, weights = e$weight)), 
                                            coef_omit = "Intercept", coef_map = regressors_names, background = list(geom_vline(xintercept = 0, color = "grey"))) + 
    labs(x = 'Coefficients', y = '', title = 'Knowledge about Climate change'))
save_plot(filename = paste0(folder, "reg_anthropogenic_knowledge_A", replacement_text), width = 701, height = 394)

(reg_anthropogenic_knowledge_AB <- modelplot(list("CC anthropogenic" = lm(as.formula(paste("CC_anthropogenic > 0 ~ ", paste(c(rev(setA), setB), collapse = ' + '))), data = e, weights = e$weight),
                                                 "Index knowledge" = lm(as.formula(paste("index_knowledge ~ ", paste(c(rev(setA), setB), collapse = ' + '))), data = e, weights = e$weight)), 
                                            coef_map = regressors_names[c(45:55)], background = list(geom_vline(xintercept = 0, color = "grey"))) + 
    labs(x = 'Coefficients', y = '', title = 'Knowledge about Climate change'))
save_plot(filename = paste0(folder, "reg_anthropogenic_knowledge_AB", replacement_text), width = 701, height = 394)


##### Support among social groups: Descriptive stats #####
update_constant(all) # TODO automatic width/height
main_variables_opinion_willing <<- c("CC_anthropogenic", "CC_problem", "should_fight_CC", variables_willing)
# anthropogenic, willing limit driving: >= A lot; problem, should fight, burden_sharing_emissions: >= agree; support: >= somewhat support
labels_opinion_willing <<- c("CC exists, is anthropogenic", "CC is an important problem", "[Country] should fight CC", labels_willingness)
heatmap_wrapper(vars = main_variables_opinion_willing, labels = labels_opinion_willing, conditions = heatmap_conditions, name = "opinion_willing", alphabetical = alphabetical, special = special)

variables_main_policies <<- c("tax_transfers_support", "standard_support", "standard_public_transport_support", "investments_support", "beef_ban_intensive_support", "insulation_mandatory_support_no_priming", "policy_tax_flying", "policy_ban_city_centers", "global_quota", "burden_share_ing_population", "global_tax_support", "global_assembly_support", "tax_1p_support")
# # anthropogenic, willing limit driving: >= A lot; problem, should fight, burden_sharing_emissions: >= agree; support: >= somewhat support
labels_main_policies <<- c("Carbon tax with cash transfers", "Ban on combustion-engine cars", "Ban on combustion-engine cars\nwhere alternatives made available", "Green infrastructure program", "Ban on intensive cattling", "Mandatory insulation of buildings", "A tax on flying (raising price by 20%)", "A ban of polluting vehicles in city centers", "Global carbon budget (+2°C)\ndivided in tradable country shares", "Emission share should be in proportion to population*", "Global tax on GHG financing a global basic income", "Global democratic assembly on climate change", "Global tax on millionaires funding LDC")
# heatmap_wrapper(vars = variables_main_policies, labels = labels_main_policies, labels_along = labels_burden_share_ing, conditions = heatmap_conditions, name = "main_policies", alphabetical = alphabetical, special = special)
heatmap_wrapper(vars = variables_main_policies[1:8], labels = labels_main_policies[1:8], conditions = heatmap_conditions, name = "main_national_policies", alphabetical = alphabetical, special = special)
heatmap_wrapper(vars = variables_main_policies[9:13], labels = labels_main_policies[9:13], labels_along = labels_burden_share_ing, conditions = heatmap_conditions, name = "main_international_policies", alphabetical = alphabetical, special = special)

update_constant(fr) 



# Simple OLS
(reg_support_policies_A <- modelplot(list("Average support" = lm(as.formula(paste("policies_support > 0 ~ ", paste(rev(setA), collapse = ' + '))), data = e, weights = e$weight),
                                 "Index main policies" = lm(as.formula(paste("index_main_policies ~ ", paste(rev(setA), collapse = ' + '))), data = e, weights = e$weight),
                                 "Index all policies" = lm(as.formula(paste("index_all_policies ~ ", paste(rev(setA), collapse = ' + '))), data = e, weights = e$weight)), 
                                            coef_omit = "Intercept", coef_map = regressors_names, background = list(geom_vline(xintercept = 0, color = "grey"))) + 
    labs(x = 'Coefficients', y = '', title = 'Support Policies'))
save_plot(filename = paste0(folder, "reg_support_policies_A", replacement_text), width = 701, height = 394)

(reg_support_policies_AB <- modelplot(list("Average support" = lm(as.formula(paste("policies_support > 0 ~ ", paste(c(rev(setA), setB), collapse = ' + '))), data = e, weights = e$weight),
                                  "Index main policies" = lm(as.formula(paste("index_main_policies ~ ", paste(c(rev(setA), setB), collapse = ' + '))), data = e, weights = e$weight),
                                  "Index all policies" = lm(as.formula(paste("index_all_policies ~ ", paste(c(rev(setA), setB), collapse = ' + '))), data = e, weights = e$weight)), 
                                             coef_map = regressors_names[c(45:55)], background = list(geom_vline(xintercept = 0, color = "grey"))) + 
    labs(x = 'Coefficients', y = '', title = 'Support Policies'))
save_plot(filename = paste0(folder, "reg_support_policies_AB", replacement_text), width = 701, height = 394)

map_setC <- setC_indices_label
names(map_setC) <- setC_indices
(reg_support_policies_ABC <- modelplot(list("Average support" = lm(as.formula(paste("policies_support > 0 ~ ", paste(c(rev(setA), setB, setC_indices[c(-6,-10,-12)]), collapse = ' + '))), data = e, weights = e$weight),
                                           "Index main policies" = lm(as.formula(paste("index_main_policies ~ ", paste(c(rev(setA), setB, setC_indices[c(-6,-10,-12)]), collapse = ' + '))), data = e, weights = e$weight),
                                           "Index all policies" = lm(as.formula(paste("index_all_policies ~ ", paste(c(rev(setA), setB, setC_indices[c(-6,-10,-12)]), collapse = ' + '))), data = e, weights = e$weight)), 
                                      coef_map = map_setC[-6], background = list(geom_vline(xintercept = 0, color = "grey"))) + 
    labs(x = 'Coefficients', y = '', title = 'Support Policies'))
save_plot(filename = paste0(folder, "reg_support_policies_ABC", replacement_text), width = 701, height = 394)

# labels_investments_funding <- c()
# for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# try({(investments_funding_US <- barres(vars = variables_investments_funding, export_xls = export_xls, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
#   save_plotly_new_filename(investments_funding_US, width= 560, height=260)})

# labels_condition <- c()
# for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# try({(condition_US <- barres(vars = variables_condition, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_condition, hover = label_great_deal))
#   save_plotly_new_filename(condition_US, width= 805, height=250) })


##### Support among social groups: Socio-demographic determinants #####
plot_along(vars = c("policies_support"), along = "children", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "urban", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "college", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
# plot_along(vars = c("policies_support"), along = "urban_category", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
# plot_along(vars = c("policies_support"), along = "diploma", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "Gilets_jaunes_agg", labels = c("Average support for main policies"), covariates = c(setAt, setB, "Gilets_jaunes_agg"), df = fr)
plot_along(vars = c("policies_support"), along = "age", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "availability_transport", factor_along = T, labels = c("Average support for main policies"), covariates = c(setAt, setB[-5], "as.factor(availability_transport)"), df = fr) 
plot_along(vars = c("policies_support"), along = "vote_agg_factor", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "gas_expenses", factor_along = T,labels = c("Average support for main policies"), covariates = c(setAt, setB[-2], "as.factor(gas_expenses)"), df = fr) 
# TODO: choose x-axis limits, reg, cond in automatic name


##### Support among social groups: LDA #####


##### Explanatory ideas: Regressions #####



##### Explanatory ideas: Variance decompositions #####
# Specifications 
temp <-  -1*(e$vote_agg %in% c("Far left", "Left")) -0*(e$vote_agg %in% c("Center")) + 1*(e$vote_agg %in% c("Right", "Far right")) -0.1*(e$vote_agg == -.1)
e$vote_agg_factor <- as.factor(as.item(temp, labels = structure(c(-1,0,1,-0.1), names = c("Left", "Center", "Right","PNR or other")),
                     annotation="vote_agg_factor: Vote or hypothetical vote in last election aggregated into 3 categories. Left/Center/Right"))


controls_lmg <- c(control_variables_w_treatment[c(1:5,7)], "urban", "urbanity", "as.character(left_right)","treatment == \"Climate\"",
                  "treatment == \"Policy\"", "treatment == \"Both\"","as.factor(country)")
end_formula_treatment_socio_demographics <- paste(c(setA[c(1:7)], "vote_agg_factor", setB), collapse = ') + (')
end_formula_treatment_socio_demographics <- paste(c("(", end_formula_treatment_socio_demographics), collapse = "")
end_formula_treatment_socio_demographics <- paste(c(end_formula_treatment_socio_demographics, ")"), collapse = "")
end_formula_treatment_indices <- paste(c(setC_indices[c(-6,-10,-12)]), collapse = ') + (')
end_formula_treatment_indices <- paste(c("(", end_formula_treatment_indices), collapse = "")
end_formula_treatment_indices <- paste(c(end_formula_treatment_indices, ")"), collapse = "")
# Alphabetical order of variables matters here
controls_labels_lm <- c("Age", "Income", "Wealth", "Employment", "Gender", "Parenthood", "Education", "Origin/Ethnicity", "Right", "Center", "Left", "Urban", "Gas expenses", "Heating expenses", "Polluting sector",
                        "Availability of Public Transport", "Car dependency", "Owner", "Flights")
controls_labels_lm_signs <- c("Age (-)", "Income (-)","Employment (+)", "Vote (-)", "Gender (+)", "Parenthood (+)", "No Education (-)", "Origin (-)", "Urban (+)", "Gas expenses (-)", "Heating expenses (+)", "Polluting sector (+)",
                              "Availability of Public Transport (+)" , "Car dependency (-)", "Owner (-)", "Flights (+)")
setC_indices_label_signs <- paste(setC_indices_label, c(rep("(+)", 5), "NA", rep("(+)", 3), "NA", "(-)", "NA", "(-)", "(+)"), sep = " ")

formulas <- models <- list()
formulas[["Main policies Index - Socio-demographics"]] <- as.formula(paste("index_main_policies ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Main policies Index - Indices"]] <- as.formula(paste("index_main_policies ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
for (i in names(formulas)) models[[i]] <- lm(formulas[[i]], data = e, weights = e$weight)
main_policies_socio_non_standardized <- calc.relimp(models[[1]], type = c("lmg"), rela = F, rank= F)
main_policies_indices_non_standardized <- calc.relimp(models[[2]], type = c("lmg"), rela = F, rank= F)

# Graphs
(lmg_main_policies_socio_non_standardized <- barres(data = t(as.matrix(main_policies_socio_non_standardized@lmg)), labels = controls_labels_lm_signs,legend = "% of response variances", rev = F))
(lmg_main_policies_indices_non_standardized <- barres(data = t(as.matrix(main_policies_indices_non_standardized@lmg)), labels = setC_indices_label_signs[c(-6,-10,-12)],legend = "% of response variances", rev = F))
save_plotly(lmg_main_policies_socio_non_standardized, width = 701, height = 394)
save_plotly(lmg_main_policies_indices_non_standardized, width = 701, height = 394)


##### Explanatory ideas: Gelbach decompositions #####
# /!\ This need to be changed if you're note using STATA SE 17 or a Mac
if (Sys.info()[7] == "Bluebii") {
  options("RStata.StataPath" = '/Applications/Stata/StataSE17.app/Contents/MacOS/stata-se')
  options("RStata.StataVersion" = 17)  
} else if (Sys.info()[7] == "Ana") {
  options("RStata.StataPath" = 'C:/Program Files/Stata/StataSE17')
  options("RStata.StataVersion" = 17)  
}


# var_to_decompose and group_of_interest: you need to input only one variable as a character
# controls and indices, can be a character vector
# Factor variables from control need to be in controls_factor
gelbach_decomposition <- function(var_to_decompose, group_of_interest, controls, controls_factor, indices, df=e, weights = "weight") {
  # We restrict the df to the variables we'll use, since there can be some incompatibilities
  # in using R dataframes in Stata
  df <- df %>%
    select(c(var_to_decompose, group_of_interest, controls, controls_factor, indices, weights))
  
  # Rename var because problem with Stata for variables with names too long
  indices_short <- c()
  for (i in seq_along(indices)){
    indices_short[i] <- paste("index_", i, sep = "")
  }
  df <- df %>%
    rename_with(~ indices_short[which(indices == .x)], .cols = indices)
  df <- df %>%
    rename("var_to_decompose" = var_to_decompose)
  
  
  # First, we prepare the options for the analysis
  option_b1x2 <- ""
  for (i in seq_along(indices)){
    option_b1x2 <- paste(option_b1x2,"g", i, " = ", indices_short[i], " : ", sep = "")
  }
  option_b1x2 <- substr(option_b1x2, 1, nchar(option_b1x2)-3)
  nbr_indices <- length(indices)
  
  # We stock the different lines of codes for Stata into a vector
  # Each element corresponds to a different line of code to run in Stata
  # We will then collapse those commands altogether to run them w/ RStata
  stata_cmd <- c()
  stata_cmd[1] <- "
  set more off
  ssc install b1x2, replace"
  stata_cmd[2] <- paste("global indices", paste('"', paste(indices_short, collapse = " "), '"', sep =""), sep = " ")
  stata_cmd[3] <- paste("global controls", paste('"', paste(controls, collapse = " "), '"', sep = ""), sep = " ")
  stata_cmd[4] <- paste("global controls_factor", paste('"', paste(controls_factor, collapse = " "), '"', sep = ""), sep = " ")
  stata_cmd[5] <- paste("global option_b1x2", paste('"', option_b1x2, '"', sep = ""), sep = " ")
  stata_cmd[6] <- paste("global nbr_indices", paste(nbr_indices), sep = " ")
  stata_cmd[7] <- paste("global nbr_plus_one_indices", paste(nbr_indices+1), sep = " ")
  stata_cmd[8] <- paste("global var_to_decompose", paste("var_to_decompose"), sep = " ")
  stata_cmd[9] <- paste("local var_to_decompose", paste("var_to_decompose"), sep = " ")
  stata_cmd[10] <- paste("global group_of_interest", paste(group_of_interest), sep = " ")
  stata_cmd[11] <- paste("local group_of_interest", paste(group_of_interest), sep = " ")
  stata_cmd[12] <- paste("global local_weight [aw=", paste(weights), paste("]"), sep = "")
  stata_cmd[13] <- "do gelbach_stata.do"
  
  stata_cmd <- paste(stata_cmd, collapse = "\n")
  # We input df, and obtain the data frame with the share explained by each indice
  final <- stata(stata_cmd, data.in = df, data.out = T)
  
  return(final)
}
# Need to create dummies for Stata
# Set A
e$pol_right <- e$vote_agg >= 1
e$pol_center <- e$vote_agg == 0
e$pol_pnr <- e$vote_agg == -0.1
e$pol_left <- e$vote_agg <= -1
# Set B
e$gas_expenses_dum <- e$gas_expenses > 50
e$heating_expenses_dum <- e$heating_expenses > 500
e$availability_transport_dum <- e$availability_transport >= 1
e$flights_agg_dum <- e$flights_agg > 1

setB_dum <- c(setB[1], "gas_expenses_dum", "heating_expenses_dum", setB[4], "availability_transport_dum", setB[c(6,7)], "flights_agg_dum")
# Set C
setC_indices <- c("index_trust_govt", setC[c(6:14)], "index_lose_policies_subjective", "index_fairness", "index_lose_policies_poor", "index_lose_policies_rich")

setC_indices_label <- c("Trusts the governement", "Is concerned about climate change", "Is worried about the future", "Has a good knowledge of climate change", "Climate policies have a positive effect \n on the economy",
                        "Is financially constrained","Climate policies are effective", "Cares about poverty and inequalities", "Believes will suffer from climate change",
                        "Is willing to adopt climate friendly behavior", "Believes will personally lose from main policies", "Main policies are fair",
                        "Poor people will lose from main policies", "Rich people will lose from main policies")

# For Gelbach labels need to check correlation (if positive correlation have a negative sentence (IF on Latex we say "other category support less because…"), if negative have a Do-sentence)
# Therefore check with cor(e[,setC_indices[c(-6,-10,-12)]], e$pol_left == 1) / cor(e[,setC_indices[c(-6,-10,-12)]], e$young == 1) / cor(e[,setC_indices[c(-6,-10,-12)]], e$college_dum == 1)
setC_indices_label_gelbach_vote <- c("Trust the governement", "Are not concerned about climate change", "Are not worried about the future", "Do not have a good knowledge of climate change", "Think climate policies have a positive effect \n on the economy",
                        "Are not financially constrained","Do not think climate policies are effective", "Do not care about poverty and inequalities", "Do not believe they will suffer from climate change",
                        "Are not willing to adopt climate friendly behavior", "Believe they will personally lose from main policies", "Do not think main policies are fair",
                        "Believe poor people will lose from main policies", "Believe rich people will lose from main policies")

setC_indices_label_gelbach_young <- c("Do not trust the governement", "Are not concerned about climate change", "Are worried about the future", "Have a good knowledge of climate change", "Do not think climate policies have a positive effect \n on the economy",
                                     "Are not financially constrained","Do not think climate policies are effective", "Care about poverty and inequalities", "Do not believe they will suffer from climate change",
                                     "Are not willing to adopt climate friendly behavior", "Believe they will personally lose from main policies", "Do not think main policies are fair",
                                     "Believe poor people will lose from main policies", "Do not believe rich people will lose from main policies")

setC_indices_label_gelbach_college <- c("Do no trust the governement", "Are not concerned about climate change", "Are not worried about the future", "Do not have a good knowledge of climate change", "Do not think climate policies have a positive effect \n on the economy",
                                     "Are not financially constrained","Do not think climate policies are effective", "Care about poverty and inequalities", "Do not believe they will suffer from climate change",
                                     "Are not willing to adopt climate friendly behavior", "Believe they will personally lose from main policies", "Do not think main policies are fair",
                                     "Believe poor people will lose from main policies", "Do not believe rich people will lose from main policies")
## Prepare the Graphs
# Vote
# unexplained: .435; coef Partial:0.229 ; coef Full: 0.104
gelbach_vote_agg_no_fairness_index_main_policies <- gelbach_decomposition(var_to_decompose = "index_main_policies", group_of_interest = "pol_left",
                                                              controls = c(setA[c(1,3,6,7)], setB_dum), controls_factor = c("age", "income_factor", "employment_agg"),
                                                              indices = setC_indices[c(-6,-10,-12)])

barres(data = t(matrix(gelbach_vote_agg_no_fairness_index_main_policies$shareExplained/100)), labels = setC_indices_label_gelbach_vote[c(-6,-10,-12)],legend = "% Partisan gap explained", rev = F)

# Age
e$young <-e$age %in% c("18-24", "25-34")
# share unexplained: 0.167; coef Partial: .176 coef Full: .031
gelbach_young_no_fairness_index_main_policies <- gelbach_decomposition(var_to_decompose = "index_main_policies", group_of_interest = "young",
                                                           controls = c(setA[c(1,3,6,7)], setB_dum), controls_factor = c("income_factor", "vote_agg_factor", "employment_agg"),
                                                           indices = setC_indices[c(-6,-10,-12)])
barres(data = t(matrix(gelbach_young_no_fairness_index_main_policies$shareExplained/100)), labels = setC_indices_label_gelbach_young[c(-6,-10,-12)],legend = "% Age gap explained", rev = F)

# College
# share unexplained: 0; coef Partial: .128 coef Full: .004
e$college_dum <- e$college == "College Degree"
gelbach_college_no_fairness_index_main_policies <- gelbach_decomposition(var_to_decompose = "index_main_policies", group_of_interest = "college_dum",
                                                             controls = c(setA[c(1,3,7)]), controls_factor = c("age", "income_factor", "vote_agg_factor", "employment_agg"),
                                                             indices = setC_indices[c(-6,-10,-12)])
barres(data = t(matrix(gelbach_college_no_fairness_index_main_policies$shareExplained/100)), labels = setC_indices_label_gelbach_college[c(-6,-10,-12)],legend = "% Diploma gap explained", rev = F)

##### Explanatory ideas: treatments #####
plot_along(vars = c("policies_support"), along = "treatment", labels = c("Average support for main policies"), plot_origin_line = T, name = "policies_support_by_treatment", covariates = setAt, df = fr)
plot_along(vars = c("policies_support", variables_policies_support), along = "treatment", labels = c("The three main policies", labels_policies), name = "all_main_policies_support_by_treatment", covariates = setAt, df = fr)
plot_along(vars = variables_policies_support, along = "treatment", labels = labels_policies, name = "main_policies_support_by_treatment", covariates = setAt, df = fr)
plot_along(vars = variables_policies_support, origin = "control_mean", along = "treatment", labels = labels_policies, name = "main_policies_support_by_treatment_originMean", covariates = setAt, df = fr)
# plot_along(vars = c("policies_support", "share_policies_supported", "CC_anthropogenic"), name = "support_knowledge_by_treatment", along = "treatment", labels = c("Average support for main policies", "Share of climate policies supported", "CC is anthropogenic"), covariates = setAt, df = fr) 
# plot_along(vars = c("CC_anthropogenic"), along = "treatment", labels = c("CC is anthropogenic"), covariates = setAt, df = fr)
plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", origin = 0, labels = c("Index of support to main policies", "Index of support to all policies"), plot_origin_line = T, name = "indices_policies_by_treatment_origin0", covariates = setAt, df = fr)
plot_along(vars = c("index_main_policies", "index_all_policies", "index_willing_change"), along = "treatment", origin = 0, labels = c("Index of support to main policies", "Index of support to all policies", "Index of willingness to adopt green behavior"), plot_origin_line = T, name = "indices_policies_willing_by_treatment_origin0", covariates = setAt, df = fr)
# plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", labels = c("Index of support to main policies", "Index of support to all policies"), name = "indices_policies_by_treatment", covariates = setAt, df = fr)
# plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", origin = "control_mean", labels = c("Index of support to main policies", "Index of support to all policies"), name = "indices_policies_by_treatment_originControl", covariates = setAt, df = fr)
# plot_along(vars = c("policies_support", "index_main_policies", "index_all_policies"), along = "treatment", labels = c("Average support for main policies", "Index of support to main policies", "Index of support to all policies"), name = "support_indices_policies_by_treatment", covariates = setAt, df = fr)
# TODO: order along like e$treatment, CC_anthropogenic CI, save width/height
# TODO! origin in indices

# TODO! I suggest at least showing treatment effects on the policies separately. 
# TODO! Was there any effect of the treatment on willingness to make changes? —> could show those outcomes. (In which case also add a slide showing the heatmap across countries of “willingness to make changes to own behavior”)
##### %% Sandbox %% #####
summary(lm(as.formula("index_main_policies ~ ", paste(setAt, collapse = ' + ')), data = fr, weights = fr$weight))
plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", labels = c("Index of support to main policies", "Index of support to all policies"), name = "indices_policies_by_treatment", covariates = setAt, df = fr)

# plot_along(plot_origin_line = T, vars = c("policies_support"), along = "treatment", labels = c("Average support for main policies"), name = "temp", covariates = setAt, df = fr, save = F)
# plot_along(plot_origin_line = T, vars = variables_policies_support, along = "treatment", labels = labels_policies, name = "temp", covariates = setAt, df = fr, save = F)


##### Appendix: representativity by vote #####
# e <- eval(parse(text = tolower(country)))
sum(fr$vote_participation == "Yes" & !(fr$vote %in% c("PNR", "Preferisco non dirlo", "Prefiero no decirlo")))/NROW(fr)