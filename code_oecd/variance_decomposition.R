library(relaimpo)

# Variance is decomposed using relaimpo::lmg, which takes the average of ANOVAs of all permutations of regressors
#   to neutralize the order in which regressors are removed in the different regressions. cf. Grömping (2007) for details.

##### Specifications #####
indices_list <- c("index_progressist", "index_knowledge", "index_affected", "index_concerned_about_CC", "index_worried", "index_positive_economy", "index_constrained",
                  "index_policies_effective", "index_care_poverty", "index_altruism","index_affected_subjective","index_willing_change", "index_lose_policies_subjective", "index_fairness", "index_trust_govt", "index_lose_policies_poor", "index_lose_policies_rich", "index_preferences_pricing_norms")


controls_lmg <- c("dominant_origin == TRUE", "female == TRUE", "children == TRUE", "college == \"No college\"","employment_agg == \"Retired\"",
                  "employment_agg == \"Student\"","employment_agg == \"Working\"",
                  "age == \"25-34\"", "age == \"35-49\"", "age == \"50-64\"","age == \"65+\"",
                  control_variables_w_treatment[8:10], "treatment == \"Climate\"",
                  "treatment == \"Policy\"", "treatment == \"Both\"","country == \"DK\"",
                  "country == \"FR\"", "country == \"US\"")

controls_lmg <- c(control_variables_w_treatment[c(1:5,7)], "as.character(left_right)","treatment == \"Climate\"",
                  "treatment == \"Policy\"", "treatment == \"Both\"","as.factor(country)")

end_formula_treatment_indices <- paste(c(controls_lmg, indices_list[2:length(indices_list)]), collapse = ') + (')
end_formula_treatment_indices <- paste(c("(", end_formula_treatment_indices), collapse = "")
end_formula_treatment_indices <- paste(c(end_formula_treatment_indices, ")"), collapse = "")

end_formula_treatment_indices_no_willingness <- paste(c(controls_lmg, indices_list[c(2:11,13:length(indices_list))]), collapse = ') + (')
end_formula_treatment_indices_no_willingness <- paste(c("(", end_formula_treatment_indices_no_willingness), collapse = "")
end_formula_treatment_indices_no_willingness <- paste(c(end_formula_treatment_indices_no_willingness, ")"), collapse = "")

end_formula_treatment <- paste(controls_lmg, collapse = ') + (') #  Do not take left and income 
end_formula_treatment <- paste(c("(", end_formula_treatment), collapse = "")
end_formula_treatment <- paste(c(end_formula_treatment, ")"), collapse = "")


indices_lab <- c("Is progressist", "Has a good knowledge of climate change", "Is affected by climate change", "Is concerned about climate change", "Is worried about the future", "Climate policies have a positive effect on the economy",
                 "Is financially constrained","Climate policies are effective", "Cares about poverty and inequalities", "Is willing to donate to reforestation project",
                 "Thinks will suffer of climate change", "Is willing to adopt climate friendly behavior", "Thinks will lose from main policies", "Thinks main policies are fair", "Trusts the governement",
                 "Thinks poor people will from main policies", "Thinks rich people will lose from main policies", "Support more pricing policies than norms")

controls_labels_lm <- c("Employment Status", "Age", "Political Leaning", "Country", "Origin", "Gender", "Parenthood",
                        "Education", "Treatment: Climate", "Treatment: Policy", "Treatment: Both")

formulas <- models <- list()
formulas[["Ban on combustion-engine cars Index"]] <- as.formula(paste("index_standard_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Carbon tax with cash transfers Index"]] <- as.formula(paste("index_tax_transfers_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Green investment program Index"]] <- as.formula(paste("index_investments_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["Main policies Index"]] <- as.formula(paste("index_main_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["All climate policies Index"]] <- as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + ')))
formulas[["All climate policies Index - no indices"]] <- as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + ')))
formulas[["Willing to change Index"]] <- as.formula(paste("index_willing_change_dummies2SD ~ ", paste(c(end_formula_treatment_indices_no_willingness), collapse = ' + ')))
formulas[["Willing to change Index - no indices"]] <- as.formula(paste("index_willing_change_dummies2SD ~ ", paste(c(end_formula_treatment), collapse = ' + ')))

for (i in names(formulas)) models[[i]] <- lm(formulas[[i]], data = e, weights = e$weight)

#coef_non_significant <- controls_lmg[c(1,5,7:11,15:16,20)]

##### Computations #####
# standardized: variance shares sum to 1
# var: variance explained by opinions (with socio-demos always as regressors) / no_indices: ..by socio-demos / w_controls: ..by both opinions and socio-demos
standard_var_standardized <- calc.relimp(formulas[[1]], type = c("lmg"), rela = T, rank= F, always = controls_lmg)
standard_var_non_standardized <- calc.relimp(formulas[[1]], type = c("lmg"), rela = F, rank= F, always = controls_lmg)
tax_transfers_var_standardized <- calc.relimp(formulas[[2]], type = c("lmg"), rela = T, rank= F, always = controls_lmg)
tax_transfers_var_non_standardized <- calc.relimp(formulas[[2]], type = c("lmg"), rela = F, rank= F, always = controls_lmg)
investments_var_standardized <- calc.relimp(formulas[[3]], type = c("lmg"), rela = T, rank= F, always = controls_lmg)
investments_var_non_standardized <- calc.relimp(formulas[[3]], type = c("lmg"), rela = F, rank= F, always = controls_lmg)
main_policies_var_standardized <- calc.relimp(formulas[[4]], type = c("lmg"), rela = T, rank= F, always = controls_lmg)
main_policies_var_non_standardized <- calc.relimp(formulas[[4]], type = c("lmg"), rela = F, rank= F, always = controls_lmg)

all_policies_var_standardized <- calc.relimp(models[[5]], type = c("lmg", "pratt"), rela = T, rank= F, always = controls_lmg)
all_policies_var_non_standardized <- calc.relimp(models[[5]], type = c("lmg", "pratt"), rela = F, rank= F, always = controls_lmg)
# each of the two below line take ~1h to compute
# all_policies_w_controls_var_standardized <- calc.relimp(models[[5]], type = c("lmg"), rela = T, rank= F)
# all_policies_w_controls_var_non_standardized <- calc.relimp(models[[5]], type = c("lmg"), rela = F, rank= F)
all_policies_no_indices_var_standardized <- calc.relimp(models[[6]], type = c("lmg"), rela = T, rank= F)
all_policies_no_indices_var_non_standardized <- calc.relimp(models[[6]], type = c("lmg"), rela = F, rank= F)

willing_change_var_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = T, rank= F, always = controls_lmg)
willing_change_var_non_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = F, rank= F, always = controls_lmg)
# each of the two below line take ~1h to compute
# willing_change_w_controls_var_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = T, rank= F)
# willing_change_w_controls_var_non_standardized <- calc.relimp(models[[7]], type = c("lmg"), rela = F, rank= F)
willing_change_no_indices_var_standardized <- calc.relimp(models[[8]], type = c("lmg"), rela = T, rank= F)
willing_change_no_indices_var_non_standardized <- calc.relimp(models[[8]], type = c("lmg"), rela = F, rank= F)


##### Figures #####
lmg_standard_standardized <- barres(data = t(as.matrix(standard_var_standardized@lmg)), labels = indices_lab[c(2:length(indices_lab))],legend = "% of remaining R-squared", rev = F)
# Note R^2 = 43.34%, metrics are normalized to sum 100%. Analysis adjusted for 20 regressors that make up 6.35 pcts. pts of R^2
lmg_standard_non_standardized <- barres(data = t(as.matrix(standard_var_non_standardized@lmg)), labels = indices_lab[c(2:length(indices_lab))],legend = "% of response variances", rev = F)
# Note R^2 = 43.34%, metrics are not normalized. Analysis adjusted for 20 regressors that make up 6.35 pcts. pts of R^2

lmg_all_policies_standardized <- barres(data = t(as.matrix(all_policies_var_standardized@lmg)), labels = indices_lab[c(2:length(indices_lab))],legend = "% of remaining R-squared", rev = F)
lmg_all_policies_non_standardized <- barres(data = t(as.matrix(all_policies_var_non_standardized@lmg)), labels = indices_lab[c(2:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_all_policies_w_controls_standardized <- barres(data = t(as.matrix(all_policies_w_controls_var_standardized@lmg)),
                                                  labels = c(controls_labels_lm, indices_lab[c(2:length(indices_lab))]),
                                                  legend = "% of remaining R-squared", rev = F)
lmg_all_policies_w_controls_non_standardized <- barres(data = t(as.matrix(all_policies_w_controls_var_non_standardized@lmg)),
                                                      labels = c(controls_labels_lm, indices_lab[c(2:length(indices_lab))]),
                                                      legend = "% of response variances", rev = F)
lmg_all_policies_no_indices_standardized <- barres(data = t(as.matrix(all_policies_no_indices_var_standardized@lmg)),
                                                   labels = controls_labels_lm,
                                                   legend = "% of remaining R-squared", rev = F)
lmg_all_policies_no_indices_non_standardized <- barres(data = t(as.matrix(all_policies_no_indices_var_non_standardized@lmg)),
                                                       labels = controls_labels_lm,
                                                       legend = "% of response variances", rev = F)

lmg_willing_change_standardized <- barres(data = t(as.matrix(willing_change_var_standardized@lmg)), labels = indices_lab[c(2:11,13:length(indices_lab))],legend = "% of remaining R-squared", rev = F)
lmg_willing_change_non_standardized <- barres(data = t(as.matrix(willing_change_var_non_standardized@lmg)), labels = indices_lab[c(2:11,13:length(indices_lab))],legend = "% of response variances", rev = F)
lmg_willing_change_w_controls_standardized <- barres(data = t(as.matrix(willing_change_w_controls_var_standardized@lmg)),
                                                    labels = c(controls_labels_lm, indices_lab[c(2:11,13:length(indices_lab))]),
                                                    legend = "% of remaining R-squared", rev = F)
lmg_willing_change_w_controls_non_standardized <- barres(data = t(as.matrix(willing_change_w_controls_var_non_standardized@lmg)),
                                                        labels = c(controls_labels_lm, indices_lab[c(2:11,13:length(indices_lab))]),
                                                        legend = "% of response variances", rev = F)
lmg_willing_change_no_indices_standardized <- barres(data = t(as.matrix(willing_change_no_indices_var_standardized@lmg)),
                                                     labels = controls_labels_lm,
                                                     legend = "% of remaining R-squared", rev = F)
lmg_willing_change_no_indices_non_standardized <- barres(data = t(as.matrix(willing_change_no_indices_var_non_standardized@lmg)),
                                                         labels = controls_labels_lm,
                                                         legend = "% of response variances", rev = F)


##### Decision trees #####
rpart.plot(tree_all_support <- rpart(formulas[["All climate policies Index"]], e))
prp(tree_all_support, box.palette = "Blues", tweak = 1.2)

