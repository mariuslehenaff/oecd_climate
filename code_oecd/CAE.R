##### Data preparation #####
# The objects of preparation.R should be defined, including the dataframes containing the survey data.
# The functions from .Rprofile, heterogeneity_graph.R and render.R should be defined.
update_constant(fr) # To restrict the data to FR and defines constant for FR analysis. 
# update_constant(all) may be needed at the beginning of international comparisons. Such "international" sections should be then followed by update_constant(fr).


##### Knowledge: Descriptive stats #####


##### Knowledge: Socio-demographic determinants #####
# TODO? replace OLS by logit for binary outcomes?
(reg_anthropogenic_A <- modelplot(list(lm(as.formula(paste("CC_anthropogenic > 0 ~ ", paste(rev(setAt), collapse = ' + '))), data = e, weights = e$weight)), 
                                  coef_omit = c("Intercept|treatment"), coef_map = regressors_names, background = list(geom_vline(xintercept = 0, color = "grey"))) + 
   labs(x = 'Coefficients', y = '', title = 'CC is anthropogenic'))
save_plot(filename = paste0(folder, "reg_anthropogenic_A", replacement_text), width = 400, height = 500)

(reg_anthropogenic_knowledge_A <- modelplot(list("CC anthropogenic" = lm(as.formula(paste("CC_anthropogenic > 0 ~ ", paste(rev(setAt), collapse = ' + '))), data = e, weights = e$weight),
                                                 "Index knowledge" = lm(as.formula(paste("index_knowledge ~ ", paste(rev(setAt), collapse = ' + '))), data = e, weights = e$weight)), 
                                            coef_omit = "Intercept|treatment", coef_map = regressors_names, background = list(geom_vline(xintercept = 0, color = "grey"))) + 
    labs(x = 'Coefficients', y = '', title = 'Knowledge about CC'))
save_plot(filename = paste0(folder, "reg_anthropogenic_knowledge_A", replacement_text), width = 400, height = 500)


##### Support among social groups: Descriptive stats #####
update_constant(all)
variables_main_policies <<- c("tax_transfers_support", "standard_support", "standard_public_transport_support", "investments_support", "beef_ban_intensive_support", "insulation_mandatory_support_no_priming", "policy_tax_flying", "policy_ban_city_centers", "global_quota", "burden_share_ing_population", "global_tax_support", "global_assembly_support", "tax_1p_support")
# anthropogenic, willing limit driving: >= A lot; problem, should fight, burden_sharing_emissions: >= agree; support: >= somewhat support
labels_main_policies <<- c("Carbon tax with cash transfers", "Ban on combustion-engine cars", "Ban on combustion-engine cars\nwhere alternatives made available", "Green infrastructure program", "Ban on intensive cattling", "Mandatory insulation of buildings", "A tax on flying (raising price by 20%)", "A ban of polluting vehicles in city centers", "Global carbon budget (+2°C)\ndivided in tradable country shares", "Emission share should be in proportion to population*", "Global tax on GHG financing a global basic income", "Global democratic assembly on climate change", "Global tax on millionaires funding LDC")
heatmap_wrapper(vars = variables_main_policies, labels = labels_main_policies, labels_along = labels_burden_share_ing, conditions = heatmap_conditions, name = "main_policies", alphabetical = alphabetical, special = special)
update_constant(fr) 

# labels_investments_funding <- c()
# for (v in variables_investments_funding) labels_investments_funding <- c(labels_investments_funding, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# try({(investments_funding_US <- barres(vars = variables_investments_funding, export_xls = export_xls, df = e, rev = F, miss = T, labels=labels_investments_funding,showLegend=F))
#   save_plotly_new_filename(investments_funding_US, width= 560, height=260)})

# labels_condition <- c()
# for (v in variables_condition) labels_condition <- c(labels_condition, sub('.* - ', '', sub('.*: ', '', Label(e[[v]]))))
# try({(condition_US <- barres(vars = variables_condition, export_xls = export_xls, df = e, rev_color = T, rev = F, miss = F, showLegend=T, labels=labels_condition, hover = label_great_deal))
#   save_plotly_new_filename(condition_US, width= 805, height=250) })


##### Support among social groups: Socio-demographic determinants #####
#  plot_along (with only one outcome on the y axis) by children, age, diploma, urban/rural, availability transport, YV, vote, gas expenses
plot_along(vars = c("policies_support"), along = "children", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "urban", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "college", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
# plot_along(vars = c("policies_support"), along = "urban_category", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
# plot_along(vars = c("policies_support"), along = "diploma", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "Gilets_jaunes_agg", labels = c("Average support for main policies"), covariates = c(setAt, setB, "Gilets_jaunes_agg"), df = fr)
plot_along(vars = c("policies_support"), along = "age", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr)
plot_along(vars = c("policies_support"), along = "availability_transport", labels = c("Average support for main policies"), covariates = c(setAt, setB, "availability_transport"), df = fr)  # TODO!
plot_along(vars = c("policies_support"), along = "vote_agg", labels = c("Average support for main policies"), covariates = c(setAt, setB), df = fr) # TODO!
plot_along(vars = c("policies_support"), along = "gas_expenses", labels = c("Average support for main policies"), covariates = c(setAt, setB, "gas_expenses"), df = fr) # TODO!
# TODO: choose x-axis limits, reg in automatic name


##### Support among social groups: LDA #####


##### Explanatory ideas: Regressions #####


##### Explanatory ideas: Variance decompositions #####


##### Explanatory ideas: Gelbach decompositions #####


##### Explanatory ideas: treatments #####
# plot_along policies_support ~ treatment (also perhaps CC_anthropogenic, index_policies)
plot_along(vars = c("policies_support"), along = "treatment", labels = c("Average support for main policies"), name = "policies_support_by_treatment", covariates = setAt, df = fr)
# plot_along(vars = c("policies_support", "share_policies_supported", "CC_anthropogenic"), name = "support_knowledge_by_treatment", along = "treatment", labels = c("Average support for main policies", "Share of climate policies supported", "CC is anthropogenic"), covariates = setAt, df = fr) 
# plot_along(vars = c("CC_anthropogenic"), along = "treatment", labels = c("CC is anthropogenic"), covariates = setAt, df = fr)
plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", origin = 0, labels = c("Index of support to main policies", "Index of support to all policies"), name = "indices_policies_by_treatment_origin0", covariates = setAt, df = fr)
# plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", labels = c("Index of support to main policies", "Index of support to all policies"), name = "indices_policies_by_treatment", covariates = setAt, df = fr)
# plot_along(vars = c("index_main_policies", "index_all_policies"), along = "treatment", origin = "control_mean", labels = c("Index of support to main policies", "Index of support to all policies"), name = "indices_policies_by_treatment_originControl", covariates = setAt, df = fr)
# plot_along(vars = c("policies_support", "index_main_policies", "index_all_policies"), along = "treatment", labels = c("Average support for main policies", "Index of support to main policies", "Index of support to all policies"), name = "support_indices_policies_by_treatment", covariates = setAt, df = fr)
# TODO: order along like e$treatment, CC_anthropogenic CI, save width/height
# TODO! origin in indices