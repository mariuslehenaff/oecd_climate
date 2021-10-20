library(relaimpo)

end_formula_treatment_indices <- paste(c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)", indices_list[2:length(indices_list)]), collapse = ') + (') #  Do not take left and income 
end_formula_treatment_indices <- paste(c("(", end_formula_treatment_indices), collapse = "")
end_formula_treatment_indices <- paste(c(end_formula_treatment_indices, ")"), collapse = "")

indices_lab <- c("Is progressist", "Has a good knowledge of climate change", "Is affected by climate change", "Is concerned about climate change", "Is worried about the future", "Climate policies have a positive effect \n on the economy",
                 "Is financially constrained","Climate policies are efficient", "Care about poverty and inequalities", "Is willing to donate to reforestation project", "Think will suffer of climate change", "Is willing to adopt climate friendly behavior")

models <- list()
models[["Ban on combustion-engine cars Index"]] <- lm(as.formula(paste("index_standard_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["Carbon tax with cash transfers Index"]] <- lm(as.formula(paste("index_tax_transfers_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["Green investment program Index"]] <- lm(as.formula(paste("index_investments_policy_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["Main policies Index"]] <- lm(as.formula(paste("index_main_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)
models[["All climate policies Index"]] <- lm(as.formula(paste("index_all_policies_dummies2SD ~ ", paste(c(end_formula_treatment_indices), collapse = ' + '))), data = e, weights = e$weight)

calc.relimp(type = "lmg", always = )
standard_var_standardized <- calc.relimp(models[[1]], type = c("lmg", "pmvd", "pratt"), rela = T, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
standard_var_non_standardized <- calc.relimp(models[[1]], type = c("lmg", "pmvd", "pratt"), rela = F, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
tax_transfers_var_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = T, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
tax_transfers_var_non_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = F, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
investments_var_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = T, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
investments_var_non_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = F, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
main_policies_var_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = T, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
main_policies_var_non_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = F, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
all_policies_var_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = T, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))
all_policies_var_non_standardized <- calc.relimp(models[[2]], type = c("lmg", "pmvd", "pratt"), rela = F, rank= F, always = c(control_variables_w_treatment[c(1:5,7:11)],"as.factor(country)"))


lmg_standard_var_standardized <- barres(data = t(as.matrix(all_policies_var_standardized@lmg)), labels = indices_lab[c(2:length(indices_lab))],legend = "% of remaining R-squared", rev = F)
# Note R^2 = 43.34%, metrics are normalized to sum 100%. Analysis adjusted for 20 regressorsthat make up 6.35 pcts. pts of R^2
lmg_standard_var_non_standardized <- barres(data = t(as.matrix(standard_var_non_standardized@lmg)), labels = indices_lab[c(2:length(indices_lab))],legend = "% of response variances", rev = F)
# Note R^2 = 43.34%, metrics are not normalized. Analysis adjusted for 20 regressorsthat make up 6.35 pcts. pts of R^2

