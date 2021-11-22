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


##### Support among social groups: Socio-demographic determinants #####


##### Support among social groups: LDA #####


##### Explanatory ideas: Regressions #####


##### Explanatory ideas: Variance decompositions #####


##### Explanatory ideas: Gelbach decompositions #####


##### Explanatory ideas: treatments #####


