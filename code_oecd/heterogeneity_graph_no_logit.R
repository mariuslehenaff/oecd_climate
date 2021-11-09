# Nb: condition is > 0. Might be different for other variables
# Return a df with mean and CI for each sub-group
# condition: corresponds to condition we want to apply to variables of interests. Default is "> 0" as origianlly the function
# was made for support variables
# heterogeneity_condition: corresponds to condition we want to apply to a subgroup. For instance, focus on people who don't trust
# by default there is no condition.
heterogeneity_mean_CI <- function(variable_name, heterogeneity_group, heterogeneity_condition = "", country_heterogeneity = FALSE, along_labels = "", df=e, weights = "weight", condition = "> 0", confidence = 0.95){
  # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
  CI <- 1-(1-confidence)/2
  if (country_heterogeneity){
    mean_ci <- as.data.frame(sapply(split(df, list(df$country_name, eval(str2expression(paste("df[[heterogeneity_group]]", heterogeneity_condition, sep = "")))), drop = F), function(x) c(unique(x$country_name), unique(x[[heterogeneity_group]]), eval(str2expression(paste("wtd.mean(x[[variable_name]]",condition,", w = x[[weights]], na.rm=T)*100"))), eval(str2expression(paste("sqrt(modi::weighted.var(x[[variable_name]]", condition," , w = x[[weights]], na.rm=T))/sqrt(NROW(x))*100"))))))
    # Get name of the country of group of heterogeneity into 1 df
    categories <- mean_ci[c(1:2),]
    # Take the mean and SD to compute CI
    mean_ci <- mean_ci[c(3:4),]
    # Get low and high bounds of CI. For the moment only 90% CI
    mean_ci <- as.data.frame(t(apply(mean_ci, 2, function(x) c(as.numeric(x[1]),as.numeric(x[1])-qnorm(CI)*as.numeric(x[2]), as.numeric(x[1])+qnorm(CI)*as.numeric(x[2])))))
    mean_ci$country <- t(categories[1,])
    # Prepare countries' names to be alphabetically order in geom_pointranger
    mean_ci$country <- factor(mean_ci$country, levels = rev(levels(factor(mean_ci$country))))
    mean_ci[[heterogeneity_group]] <- t(categories[2,])
    mean_ci <- tibble::rownames_to_column(mean_ci, "combined_name")
    mean_ci$variable <- variable_name # changed policy to variable, and mean_sd to mean_ci
    names(mean_ci) <- c("combined_name", "mean", "CI_low", "CI_high", "country", "along", "variable") # this line is new
    mean_ci$along <- factor(mean_ci$along, labels = along_labels)
    
  } else {
    mean_ci <- as.data.frame(sapply(split(df, eval(str2expression(paste("df[[heterogeneity_group]]", heterogeneity_condition, sep = "")))), function(x) c(eval(str2expression(paste("wtd.mean(x[[variable_name]]", condition, ", w = x[[weights]], na.rm=T)*100"))), eval(str2expression(paste("sqrt(modi::weighted.var(x[[variable_name]]", condition," , w = x[[weights]], na.rm=T))/sqrt(NROW(x))*100"))))))
    mean_ci <- as.data.frame(t(apply(mean_ci,2, function(x) c(x[1],x[1]-qnorm(CI)*x[2], x[1]+qnorm(CI)*x[2]))))
    mean_ci <- tibble::rownames_to_column(mean_ci, heterogeneity_group)
    mean_ci$variable <- variable_name # changed policy to variable, and mean_sd to mean_ci
    names(mean_ci) <- c("along", "mean", "CI_low", "CI_high", "variable") # this line is new
    mean_ci$along <- factor(mean_ci$along, labels = along_labels)
    
  }
  return(mean_ci)
}

variables_list <- variables_all_policies_support <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
#variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")
policies_label <- labels_all_policies_support <- c("Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")

plot_along_old <- function(vars, along, name = NULL, labels = vars, legend_x = '', legend_y = '', invert_point_y_axis = FALSE, df = e, folder = '../figures/country_comparison/', weights = "weight", width = dev.size('px')[1], height = dev.size('px')[2]) {
  levels_along <- Levels(df[[along]]) # TODO! automatic name, conditions, show legend for 20 countries (display UA!) even if there is less than 4 variables, order countries as usual
  if (is.missing(name)) name <- paste0(vars[1], "_by_", along, "_") #name <- sub("variables_", "", deparse(substitute(vars)))
  mean_sd <- bind_rows((lapply(vars, heterogeneity_mean_CI, heterogeneity_group = along, df=df, weights = weights)))
  mean_sd$policy <- factor(mean_sd$policy, levels = vars, labels = labels)
  
  if (invert_point_y_axis){
    plot <- ggplot(mean_sd) +
      geom_pointrange( aes(x = V1, y = factor(.data[[along]], levels = rev(levels(factor(.data[[along]])))), color = policy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
      labs(x = legend_x, y = legend_y, color="") + 
      theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
      scale_color_manual(labels = labels, values = color(length(labels), theme='rainbow')) # can be theme = 'rainbow', 'RdBu', 'default' or any brewer theme, but the issue with RdBu/default is that the middle one is white for odd number of categories
    
  } else{
    plot <- ggplot(mean_sd) +
      geom_pointrange( aes(x = V1, y = policy, color = .data[[along]], xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
      labs(x = legend_x, y = legend_y, color="") + 
      theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
      scale_color_manual(labels = levels_along, values = color(length(levels_along), theme='rainbow')) # can be theme = 'rainbow', 'RdBu', 'default' or any brewer theme, but the issue with RdBu/default is that the middle one is white for odd number of categories
  }
  plot
  # save_plotly(plot, filename = name, folder = folder, width = width, height = height, trim = T)
  return(plot)
}

plot_along <- function(vars, along, name = NULL, labels = vars, legend_x = '', legend_y = '', invert_variable_along = FALSE, df = e, 
                       confidence = 0.95, heterogeneity_condition = "", condition = "> 0", country_heterogeneity = FALSE, along_labels,
                       folder = '../figures/country_comparison/', weights = "weight", width = dev.size('px')[1], height = dev.size('px')[2]) {
  # TODO multiple conditions, show legend for 20 countries (display UA!) even if there is less than 4 variables, order countries as usual
  if (missing(name)) { 
    if (grepl('["\']', deparse(substitute(vars)))) {
      name <- ifelse(invert_variable_along, paste0(along, "_by_", vars[1], "_"), paste0(vars[1], "_by_", along, "_"))
      warning("The filename is formed with the first variable name, given that the argument 'name' is missing.")
    } else name <- ifelse(invert_variable_along, name <- paste0(along, "_by_", deparse(substitute(vars)), "_"), paste0(deparse(substitute(vars)), "_by_", along, "_"))
  }
  if (missing(folder) & deparse(substitute(df)) %in% tolower(countries)) folder <- paste0("../figures/", toupper(deparse(substitute(df))), "/")
  
  mean_ci <- bind_rows((lapply(vars, heterogeneity_mean_CI, heterogeneity_group = along, df=df, weights = weights, along_labels = along_labels, country_heterogeneity = country_heterogeneity, heterogeneity_condition = heterogeneity_condition, condition = condition, confidence = confidence)))
  mean_ci$variable <- factor(mean_ci$variable, levels = vars, labels = labels)
  
  if (invert_variable_along & country_heterogeneity == F) {
    names(mean_ci)[which(names(mean_ci) == "along")] <- "temp"
    names(mean_ci)[which(names(mean_ci) == "variable")] <- "along"
    names(mean_ci)[which(names(mean_ci) == "temp")] <- "variable" # or the les robust one-liner: names(mean_ci) <- c("variable", "mean", "CI_low", "CI_high", "along")
  } else if (country_heterogeneity) {
    names(mean_ci)[which(names(mean_ci) == "variable")] <- "policy"
    names(mean_ci)[which(names(mean_ci) == "country")] <- "variable"
  }
  
  if (missing(labels)) labels <- vars
  
  plot <- ggplot(mean_ci) +
    geom_pointrange( aes(x = mean, y = variable, color = along, xmin = CI_low, xmax = CI_high), position = position_dodge(width = .5)) +
    labs(x = legend_x, y = legend_y, color="") + theme(legend.title = element_blank(), legend.position = "top") +
    theme_minimal() # + scale_color_manual(values = color(length(levels_along), theme='rainbow')) # can be theme = 'rainbow', 'RdBu', 'default' or any brewer theme, but the issue with RdBu/default is that the middle one is white for odd number of categories
  # scale_color_manual(labels = Levels(df[[along]]), values = color(length(Levels(df[[along]])), theme='rainbow'))# BUG when we specify labels: the legend does not correspond to the colors
  plot
  # save_plotly(plot, filename = name, folder = folder, width = width, height = height, trim = T)
  return(plot)
}
# example :
plot_along(vars = "tax_transfers_support", along = "treatment")
plot_along(vars = rev(variables_all_policies_support), along = "treatment")
plot_along(vars = c("CC_affects_self", "net_zero_feasible", "CC_will_end", "future_richness"), along = "country_name", name = "future_by_country", labels = c("Feels affected by climate change", "Net zero by 2100 feasible", "Likely that climate change ends by 2100", "World in 100 years will be richer"))
plot_along(vars = variables_all_policies_support, along = "urban_category", df = fr, name = "policies_support_by_urban_category", labels = labels_all_policies_support)
plot_along(vars = c("CC_affects_self"), along = "country_name", name = "CC_affects_self_by_country", labels = c("Feels affected by climate change"))
# Beware, only use one variable at a time with country_heterogeneity = T
plot_along(vars = "policies_support", along = "urban", country_heterogeneity = T)
# For labels, check the output of heterogeneity_mean_CI (first column)
plot_along(vars = "policies_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"))



## GRAPHS


## Graph 1.A
# Urban
main_policies_support_urban_each_country <- plot_along(vars = "policies_support", along = "urban", country_heterogeneity = T, along_labels = c("Rural", "Urban"))
standard_public_transport_support_urban_each_country <- plot_along(vars = "standard_public_transport_support", along = "urban", country_heterogeneity = T, along_labels = c("Rural", "Urban"))
standard_support_urban_each_country <- plot_along(vars = "standard_support", along = "urban", country_heterogeneity = T, along_labels = c("Rural", "Urban"))
investments_support_urban_each_country <- plot_along(vars = "investments_support", along = "urban", country_heterogeneity = T, along_labels = c("Rural", "Urban"))
tax_transfers_support_urban_each_country <- plot_along(vars = "tax_transfers_support", along = "urban", country_heterogeneity = T, along_labels = c("Rural", "Urban"))

# Income
main_policies_support_income_each_country <- plot_along(vars = "policies_support", along = "income", country_heterogeneity = T, along_labels = c("Q1", "Q2", "Q3", "Q4"))
standard_public_transport_support_income_each_country <- plot_along(vars = "standard_public_transport_support", along = "income", country_heterogeneity = T, along_labels = c("Q1", "Q2", "Q3", "Q4"))
standard_support_income_each_country <- plot_along(vars = "standard_support", along = "income", country_heterogeneity = T, along_labels = c("Q1", "Q2", "Q3", "Q4"))
investments_support_income_each_country <- plot_along(vars = "investments_support", along = "income", country_heterogeneity = T, along_labels = c("Q1", "Q2", "Q3", "Q4"))
tax_transfers_support_income_each_country <- plot_along(vars = "tax_transfers_support", along = "income", country_heterogeneity = T, along_labels = c("Q1", "Q2", "Q3", "Q4"))

# Age
main_policies_support_age_each_country <- plot_along(vars = "policies_support", along = "age", country_heterogeneity = T)
standard_public_transport_support_age_each_country <- plot_along(vars = "standard_public_transport_support", along = "age", country_heterogeneity = T)
standard_support_age_each_country <- plot_along(vars = "standard_support", along = "age", country_heterogeneity = T)
investments_support_age_each_country <- plot_along(vars = "investments_support", along = "age", country_heterogeneity = T)
tax_transfers_support_age_each_country <- plot_along(vars = "tax_transfers_support", along = "age", country_heterogeneity = T)

## Graphs 1.B
# Willingness
variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")
policies_label <- c("WTP", "Limit flying", "Limit driving", "Have electric car", "Limit heating or cooling home", "Limit beef consumption")
willingness_by_country_urban_all <- plot_along(vars = variables_list, along = "urban", name = "willingness_by_country", labels = policies_label, along_labels = c("Rural", "Urban"))
willingness_by_country_age_all <- plot_along(vars = variables_list, along = "age", name = "willingness_by_country", labels = policies_label)
willingness_by_country_income_all <- plot_along(vars = variables_list, along = "income", name = "willingness_by_country", labels = policies_label, along_labels = c("Q1", "Q2", "Q3", "Q4"))

# Main variables
variables_list <- c("CC_anthropogenic","willing_limit_driving","wtp",
                    "standard_support","standard_public_transport_support",
                    "investments_support","tax_transfers_support",
                    "beef_ban_intensive_support","insulation_support",
                    "tax_1p_support")
policies_label <- c("Climate change is anthropogenic", "Willing to limit driving",
                    "Willing to Pay for climate action", "Support ban on combustion engine",
                    "Support ban on combustion engine \n (public transport made available)",
                    "Support green investments program", "Support carbon tax with cash transfer",
                    "Support ban on intensive cattle farming", "Support mandatory insulation",
                    "Support global wealth tax to fund LDCs")
main_var_by_country_urban_all <- plot_along(vars = variables_list, along = "urban", name = "main_var_by_country", labels = policies_label, along_labels = c("Rural", "Urban"))
main_var_by_country_age_all <- plot_along(vars = variables_list, along = "age", name = "main_var_by_country", labels = policies_label)
main_var_by_country_income_all <- plot_along(vars = variables_list, along = "income", name = "main_var_by_country", labels = policies_label, along_labels = c("Q1", "Q2", "Q3", "Q4"))

# Views
variables_list <- c("tax_transfer_constrained_hh", "tax_transfer_poor", "tax_transfer_all", "tax_reduction_personal_tax", "tax_reduction_corporate_tax", "tax_rebates_affected_firms", "tax_investments", "tax_subsidies", "tax_reduction_deficit")
policies_label <- c("Cash for constrained HH", "Cash for the poorest", "Equal cash for all", "Reduction in income tax", "Reduction in corporate tax", "Tax rebate for affected firms", "Funding green infrastructures", "Subsidies to low-carbon technologies", "Reduction in the deficit")
views_by_country_urban_all <- plot_along(vars = variables_list, along = "urban", name = "views_by_country", labels = policies_label, along_labels = c("Rural", "Urban"))
views_by_country_age_all <- plot_along(vars = variables_list, along = "age", name = "views_by_country", labels = policies_label)
views_by_country_income_all <- plot_along(vars = variables_list, along = "income", name = "views_by_country", labels = policies_label, along_labels = c("Q1", "Q2", "Q3", "Q4"))

# Support
variables_list <- c("policies_support","standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
policies_label <- c("Main Policies", "Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with \n cash transfer")
main_support_var_by_treatment_urban_all <- plot_along(vars = variables_list, along = "urban", name = "main_support_var_by_treatment", labels = policies_label, along_labels = c("Rural", "Urban"))
main_support_var_by_treatment_age_all <- plot_along(vars = variables_list, along = "age", name = "main_support_var_by_treatment", labels = policies_label)
main_support_var_by_treatment_income_all <- plot_along(vars = variables_list, along = "income", name = "main_support_var_by_treatment", labels = policies_label, along_labels = c("Q1", "Q2", "Q3", "Q4"))

# Attitudes CC
variables_list <- c("CC_anthropogenic", "CC_impacts_extinction", "donation", "should_fight_CC", "willing_limit_driving")
policies_label <- c("CC caused by humans", "CC likely to cause extinction", "Donation", "[country] should fight CC", "Willing to limit driving")
attitudes_CC_by_country_urban_all <- plot_along(vars = variables_list, along = "urban", name = "attitudes_CC_by_country", labels = policies_label, along_labels = c("Rural", "Urban"))
attitudes_CC_by_country_age_all <- plot_along(vars = variables_list, along = "age", name = "attitudes_CC_by_country", labels = policies_label)
attitudes_CC_by_country_income_all <- plot_along(vars = variables_list, along = "income", name = "attitudes_CC_by_country", labels = policies_label, along_labels = c("Q1", "Q2", "Q3", "Q4"))

# Attitudes Pol
variables_list <- c("policies_fair", "policies_self", "policies_poor", "policies_rich", "policies_large_effect", "policies_positive_negative")
policies_label <- c("Fair", "HH would win", "Poor would win", "Rich would win", "Large economic effects", "Negative economic effects")
attitudes_pol_by_country_urban_all <- plot_along(vars = variables_list, along = "urban", name = "attitudes_pol_by_country", labels = policies_label, along_labels = c("Rural", "Urban"))
attitudes_pol_by_country_age_all <- plot_along(vars = variables_list, along = "age", name = "attitudes_pol_by_country", labels = policies_label)
attitudes_pol_by_country_income_all <- plot_along(vars = variables_list, along = "income", name = "attitudes_pol_by_country", labels = policies_label, along_labels = c("Q1", "Q2", "Q3", "Q4"))

## Graphs 2.A
main_policies_support_treatment_each_country <- plot_along(vars = "policies_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"))
standard_public_transport_support_treatment_each_country <- plot_along(vars = "standard_public_transport_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"))
standard_support_treatment_each_country <- plot_along(vars = "standard_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"))
investments_support_treatment_each_country <- plot_along(vars = "investments_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"))
tax_transfers_support_treatment_each_country <- plot_along(vars = "tax_transfers_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"))

## Graphs 2.B
variables_list <- c("policies_support","standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
policies_label <- c("Main Policies", "Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with \n cash transfer")
support_var_by_treatment_all <- plot_along(vars = variables_list, along = "treatment", name = "support_var_by_treatment", labels = policies_label)

variables_list <- c("CC_anthropogenic", "CC_impacts_extinction", "donation", "should_fight_CC", "willing_limit_driving")
policies_label <- c("CC caused by humans", "CC likely to cause extinction", "Donation", "[country] should fight CC", "Willing to limit driving")
attitudes_CC_by_country_all <- plot_along(vars = variables_list, along = "treatment", name = "attitudes_CC_by_country", labels = policies_label)

variables_list <- c("policies_fair", "policies_self", "policies_poor", "policies_rich", "policies_large_effect", "policies_positive_negative")
policies_label <- c("Fair", "HH would win", "Poor would win", "Rich would win", "Large economic effects", "Negative economic effects")
attitudes_pol_by_country_all <- plot_along(vars = variables_list, along = "treatment", name = "attitudes_pol_by_country", labels = policies_label)


# # Attitudes
# variables_list <- c("CC_problem", "CC_anthropogenic", "CC_dynamic", "CC_will_end", "net_zero_feasible", "CC_affects_self", "effect_halt_CC_lifestyle", "effect_halt_CC_economy")
# policies_label <- c("CC is an important problem", "CC exists, is anthropogenic", "Cutting GHG emisions by half \n sufficient to stop rise in temperatures", "Likely to halt CC by the end of the century",
#                     "Feasible to stop GHG emissions \n while sustaining satisfactory \n standards of living in [country]", "CC will negatively affect personal lifestyle", "Negative 
#                     effects of ambitious policies on lifestyle", "Positive effects of ambitious policies \n on the [country] economy and employment")
# plot_along(vars = variables_list, along = "country_name", name = "attitudes_by_country", labels = policies_label)
