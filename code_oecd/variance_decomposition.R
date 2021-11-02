library(relaimpo)

# Variance is decomposed using relaimpo::lmg, which takes the average of ANOVAs of all permutations of regressors
#   to neutralize the order in which regressors are removed in the different regressions. cf. Grömping (2007) for details.

##### Specifications #####
indices_list <- c("index_progressist", "index_knowledge", "index_affected", "index_concerned_about_CC", "index_worried", "index_positive_economy", "index_constrained",
                  "index_policies_efficient", "index_care_poverty", "index_altruism","index_affected_subjective","index_willing_change", "index_lose_policies_subjective", "index_fairness", "index_trust_govt", "index_lose_policies_poor", "index_lose_policies_rich")


controls_lmg <- c("dominant_origin == TRUE", "female == TRUE", "children == TRUE", "college == \"No college\"","employment_agg == \"Retired\"",
                  "employment_agg == \"Student\"","employment_agg == \"Working\"",
                  "age == \"25-34\"", "age == \"35-49\"", "age == \"50-64\"","age == \"65+\"",
                  control_variables_w_treatment[8:10], "treatment == \"Climate\"",
                  "treatment == \"Policy\"", "treatment == \"Both\"","country == \"DK\"",
                  "country == \"FR\"", "country == \"US\"")

controls_lmg <- c(control_variables_w_treatment[c(1:5,7)], "as.character(left_right)","treatment == \"Climate\"",
                  "treatment == \"Policy\"", "treatment == \"Both\"","as.factor(country)")

end_formula_treatment_socio_demographics <- paste(c(controls_lmg, "index_affected"), collapse = ') + (')
end_formula_treatment_socio_demographics <- paste(c("(", end_formula_treatment_socio_demographics), collapse = "")
end_formula_treatment_socio_demographics <- paste(c(end_formula_treatment_socio_demographics, ")"), collapse = "")

end_formula_treatment_indices <- paste(c(indices_list[c(2,4:6,8,9,11,13:length(indices_list))]), collapse = ') + (')
end_formula_treatment_indices <- paste(c("(", end_formula_treatment_indices), collapse = "")
end_formula_treatment_indices <- paste(c(end_formula_treatment_indices, ")"), collapse = "")

end_formula_treatment_indices_nofairness <- paste(c(indices_list[c(2,4:6,8,9,11,13,15:length(indices_list))]), collapse = ') + (')
end_formula_treatment_indices_nofairness <- paste(c("(", end_formula_treatment_indices_nofairness), collapse = "")
end_formula_treatment_indices_nofairness <- paste(c(end_formula_treatment_indices_nofairness, ")"), collapse = "")


indices_lab <- c("Is progressist", "Has a good knowledge of climate change", "Is affected by climate change", "Is concerned about climate change", "Is worried about the future", "Climate policies have a positive effect \n on the economy",
                 "Is financially constrained","Climate policies are effective", "Cares about poverty and inequalities", "Is willing to donate to reforestation project",
                 "Thinks will suffer from climate change", "Is willing to adopt climate friendly behavior", "Thinks will lose from main policies", "Thinks main policies are fair", "Trusts the governement",
                 "Thinks poor people will lose from main policies", "Thinks rich people will lose from main policies")

controls_labels_lm <- c("Employment Status", "Age", "Political Leaning", "Country", "Origin", "Gender", "Parenthood",
                        "Education", "Treatment: Climate", "Treatment: Policy", "Treatment: Both")

formulas <- models <- list()
formulas[["Ban on combustion-engine cars Index - Socio-demographics"]] <- as.formula(paste("index_standard_policy_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Ban on combustion-engine cars Index - Indices"]] <- as.formula(paste("index_standard_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Carbon tax with cash transfers Index - Socio-demographics"]] <- as.formula(paste("index_tax_transfers_policy_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Carbon tax with cash transfers Index - Indices"]] <- as.formula(paste("index_tax_transfers_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Green investment program Index - Socio-demographics"]] <- as.formula(paste("index_investments_policy_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Green investment program Index - Indices"]] <- as.formula(paste("index_investments_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Main policies Index - Socio-demographics"]] <- as.formula(paste("index_main_policies_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Main policies Index - Indices"]] <- as.formula(paste("index_main_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["All climate policies Index - Socio-demographics"]] <- as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["All climate policies Index - Indices"]] <- as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Willing to change Index - Socio-demographics"]] <- as.formula(paste("index_willing_change_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Willing to change Index - Indices"]] <- as.formula(paste("index_willing_change_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Index Preferences Pricing (mean) - Socio-demographics"]] <- as.formula(paste("index_preferences_pricing_norms_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Index Preferences Pricing (mean) - Indices"]] <- as.formula(paste("index_preferences_pricing_norms_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Index Preferences Pricing (difference w/o investments) - Socio-demographics"]] <- as.formula(paste("index_pricing_norm_main_policies_V1_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Index Preferences Pricing (difference w/o investments) - Indices"]] <- as.formula(paste("index_pricing_norm_main_policies_V1_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Index Preferences Pricing (difference w/ investments) - Socio-demographics"]] <- as.formula(paste("index_pricing_norm_main_policies_V2_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Index Preferences Pricing (difference w/ investments) - Indices"]] <- as.formula(paste("index_pricing_norm_main_policies_V2_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Index Fairness - Socio-demographics"]] <- as.formula(paste("index_fairness_dummies2SD ~ ", paste(c(end_formula_treatment_socio_demographics), collapse = ' + ')))
formulas[["Index Fairness - Indices"]] <- as.formula(paste("index_fairness_dummies2SD ~ ", paste(c(end_formula_treatment_indices_nofairness), collapse = ' + ')))

for (i in names(formulas)) models[[i]] <- lm(formulas[[i]], data = e, weights = e$weight)

#coef_non_significant <- controls_lmg[c(1,5,7:11,15:16,20)]

##### Computations #####
# standardized: variance shares sum to 1
# var: variance explained by opinions (with socio-demos always as regressors) / no_indices: ..by socio-demos / w_controls: ..by both opinions and socio-demos
standard_socio_non_standardized <- calc.relimp(models[[1]], type = c("lmg"), rela = F, rank= F)
standard_indices_non_standardized <- calc.relimp(models[[2]], type = c("lmg"), rela = F, rank= F)
tax_transfers_socio_non_standardized <- calc.relimp(models[[3]], type = c("lmg"), rela = F, rank= F)
tax_transfers_indices_non_standardized <- calc.relimp(models[[4]], type = c("lmg"), rela = F, rank= F)
investments_socio_non_standardized <- calc.relimp(models[[5]], type = c("lmg"), rela = F, rank= F)
investments_indices_non_standardized <- calc.relimp(models[[6]], type = c("lmg"), rela = F, rank= F)
main_policies_socio_non_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = F, rank= F)
main_policies_indices_non_standardized <- calc.relimp(models[[8]], type = c("lmg"), rela = F, rank= F)
all_policies_socio_non_standardized <- calc.relimp(models[[9]], type = c("lmg"), rela = F, rank= F)
all_policies_indices_non_standardized <- calc.relimp(models[[10]], type = c("lmg"), rela = F, rank= F)
# each of the two below line take ~1h to compute
# all_policies_w_controls_var_standardized <- calc.relimp(models[[5]], type = c("lmg"), rela = T, rank= F)
# all_policies_w_controls_var_non_standardized <- calc.relimp(models[[5]], type = c("lmg"), rela = F, rank= F)
willing_change_socio_non_standardized <- calc.relimp(models[[11]], type = c("lmg"), rela = F, rank= F)
willing_change_indices_non_standardized <- calc.relimp(models[[12]], type = c("lmg"), rela = F, rank= F)
# each of the two below line take ~1h to compute
# willing_change_w_controls_var_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = T, rank= F)
# willing_change_w_controls_var_non_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = F, rank= F)
pref_pricing_norms_mean_socio_non_standardized <- calc.relimp(models[[13]], type = c("lmg"), rela = F, rank= F)
pref_pricing_norms_mean_indices_non_standardized <- calc.relimp(models[[14]], type = c("lmg"), rela = F, rank= F)
pref_pricing_norms_diff_V1_socio_non_standardized <- calc.relimp(models[[15]], type = c("lmg"), rela = F, rank= F)
pref_pricing_norms_diff_V1_indices_non_standardized <- calc.relimp(models[[16]], type = c("lmg"), rela = F, rank= F)
pref_pricing_norms_diff_V2_socio_non_standardized <- calc.relimp(models[[17]], type = c("lmg"), rela = F, rank= F)
pref_pricing_norms_diff_V2_indices_non_standardized <- calc.relimp(models[[18]], type = c("lmg"), rela = F, rank= F)
fairness_socio_non_standardized <- calc.relimp(models[[19]], type = c("lmg"), rela = F, rank= F)
fairness_indices_non_standardized <- calc.relimp(models[[20]], type = c("lmg"), rela = F, rank= F)


##### Figures #####
lmg_standard_socio_non_standardized <- barres(data = t(as.matrix(standard_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_standard_indices_non_standardized <- barres(data = t(as.matrix(standard_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_tax_transfers_socio_non_standardized <- barres(data = t(as.matrix(tax_transfers_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_tax_transfers_indices_non_standardized <- barres(data = t(as.matrix(tax_transfers_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_investments_socio_non_standardized <- barres(data = t(as.matrix(investments_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_investments_indices_non_standardized <- barres(data = t(as.matrix(investments_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_main_policies_socio_non_standardized <- barres(data = t(as.matrix(main_policies_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_main_policies_indices_non_standardized <- barres(data = t(as.matrix(main_policies_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_all_policies_socio_non_standardized <- barres(data = t(as.matrix(all_policies_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_all_policies_indices_non_standardized <- barres(data = t(as.matrix(all_policies_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_willing_change_socio_non_standardized <- barres(data = t(as.matrix(willing_change_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_willing_change_indices_non_standardized <- barres(data = t(as.matrix(willing_change_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_pref_pricing_norms_mean_socio_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_mean_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_pref_pricing_norms_mean_indices_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_mean_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_pref_pricing_norms_diff_V1_socio_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_diff_V1_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_pref_pricing_norms_diff_V1_indices_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_diff_V1_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_pref_pricing_norms_diff_V2_socio_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_diff_V2_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_pref_pricing_norms_diff_V2_indices_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_diff_V2_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_fairness_socio_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_diff_V2_socio_non_standardized@lmg)), labels = c(controls_labels_lm, indices_lab[3]),legend = "% of response variances", rev = F)
lmg_fairness_indices_non_standardized <- barres(data = t(as.matrix(pref_pricing_norms_diff_V2_indices_non_standardized@lmg)), labels = indices_lab[c(2,4:6,8,9,11,13,15:length(indices_lab))],legend = "% of response variances", rev = F)


##### Decision trees #####
rpart.plot(tree_all_support <- rpart(formulas[["All climate policies Index"]], e))
prp(tree_all_support, box.palette = "Blues", tweak = 1.2)

