
# Nb: condition is > 0. Might be different for other variables
# Return a df with mean and CI for each sub-group
# condition: corresponds to condition we want to apply to variables of interests. Default is "> 0" as origianlly the function
# was made for support variables
# heterogeneity_condition: corresponds to condition we want to apply to a subgroup. For instance, focus on people who don't trust
# by default there is no condition.
heterogeneity_mean_CI <- function(variable_name, heterogeneity_group, heterogeneity_condition = "", df=e, weights = "weight", condition = "> 0", CI = 1-0.05/2){
  # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
  mean_sd <- as.data.frame(sapply(split(df, eval(str2expression(paste("df[[heterogeneity_group]]", heterogeneity_condition, sep = "")))), function(x) c(eval(str2expression(paste("wtd.mean(x[[variable_name]]",condition,", w = x[[weights]], na.rm=T)*100"))), eval(str2expression(paste("sqrt(wtd.var(x[[variable_name]]", condition," , w = x[[weights]], na.rm=T))/sqrt(sum(x[[weights]]))*100"))))))
  # Get low and high bounds of CI. For the moment only 90% CI
  mean_sd <- as.data.frame(t(apply(mean_sd,2, function(x) c(x[1],x[1]-qnorm(CI)*x[2], x[1]+qnorm(CI)*x[2]))))
  mean_sd <- tibble::rownames_to_column(mean_sd, heterogeneity_group)
  mean_sd$policy <- variable_name
  
  return(mean_sd)
}


# e <- us
e <- fr
# e <- dk
variables_list <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
#variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")

# Apply to get df with info on each variable
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weights = "weight")))

## Plot creation
#mean_sd <- subset(mean_sd, political_affiliation %in% c("Democrat", "Republican"))
policies_label <- c("Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

mean_sd$left_right <- factor(mean_sd$left_right, levels = c("Very left", "Left", "Center", "Right", "Very right", "PNR"))

# 120 130 vert
support_by_political_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
#  scale_color_brewer(breaks = unique(mean_sd$left_right), direction = +1, palette="RdYlBu")
#  scale_color_hue(breaks = unique(mean_sd$left_right), direction = -1, palette="RdBu") 
support_by_political_FR
# /!\ Need to be saved manually
#save_plotly(support_by_political_US, width= 800, height=400)


## Dépensent bcp en énergie
## Rural
## Droite

# Vote_agg
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "vote_agg", df=e, weights = "weight")))
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$vote_agg <- factor(mean_sd$vote_agg, levels = c("Left", "Center", "Right", "Far right", "PNR or other"))

support_by_vote_agg_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
support_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

support_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
support_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#ABD9E9"))
support_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Poorly available","Adequately available"),values = c("#FDAE61", "#ABD9E9"))
support_by_availability_transport_FR

# Gas expenses
e$gas_expenses_dummy <- e$gas_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "gas_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_gas_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
support_by_gas_expenses_FR

# Heating expenses
e$heating_expenses_dummy <- e$heating_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "heating_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
support_by_heating_expenses_FR


### WILLINGNESS PART


#Set-up
variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")

policies_label <- c("WTP", "Limit flying", "Limit driving", "Have electric car", "Limit heating or cooling home", "Limit beef consumption")

# Graphs
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

mean_sd$left_right <- factor(mean_sd$left_right, levels = c("Very left", "Left", "Center", "Right", "Very right", "PNR"))

willing_by_political_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
willing_by_political_FR

# Vote_agg
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "vote_agg", df=e, weights = "weight")))
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$vote_agg <- factor(mean_sd$vote_agg, levels = c("Left", "Center", "Right", "Far right", "PNR or other"))

willing_by_vote_agg_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
willing_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

willing_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
willing_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not Polluting", "Polluting"),values = c("#FDAE61", "#ABD9E9"))
willing_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Poorly available","Adequately available"),values = c("#FDAE61", "#ABD9E9"))
willing_by_availability_transport_FR

# Gas expenses
e$gas_expenses_dummy <- e$gas_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "gas_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_gas_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
willing_by_gas_expenses_FR

# Heating expenses
e$heating_expenses_dummy <- e$heating_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "heating_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
willing_by_heating_expenses_FR

# Income
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "income", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_income_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
willing_by_incomes_FR

### ALL POSITIVE ANSWERS PART

# heterogeneity_mean_CI_01 <- function(variable_name, heterogeneity_group, df=e, weights = "weight"){
#   # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
#   mean_sd <- as.data.frame(sapply(split(df, df[[heterogeneity_group]]), function(x) c(wtd.mean(x[[variable_name]], w = x[[weights]], na.rm=T)*100, sqrt(wtd.var(x[[variable_name]], w = x[[weights]], na.rm=T))/sqrt(sum(x[[weights]]))*100)))
#   # Get low and high bounds of CI. For the moment only 10% CI
#   mean_sd <- as.data.frame(t(apply(mean_sd,2, function(x) c(x[1],x[1]-1.645*x[2], x[1]+1.645*x[2]))))
#   mean_sd <- tibble::rownames_to_column(mean_sd, heterogeneity_group)
#   mean_sd$policy <- variable_name
#   
#   return(mean_sd)
# }
# mean_sd <- bind_rows((lapply(variables_list_index, heterogeneity_mean_CI_01,
#                                           heterogeneity_group = "left_right", df=e, weights = "weight")))
# variables_list_index <- c("index_knowledge" ,"index_affected")

#Set-up
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
  
# Graphs
mean_sd <- bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

mean_sd$left_right <- factor(mean_sd$left_right, levels = c("Very left", "Left", "Center", "Right", "Very right", "PNR"))

positive_all_by_political_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
positive_all_by_political_FR

# Vote_agg
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "vote_agg", df=e, weights = "weight")))
mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))
mean_sd$vote_agg <- factor(mean_sd$vote_agg, levels = c("Left", "Center", "Right", "Far right", "PNR or other"))

positive_all_by_vote_agg_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFED6F", "#ABD9E9", "#2C7BB6", "gray70"))
positive_all_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

positive_all_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
positive_all_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not Polluting", "Polluting"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Poorly available","Adequately available"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_availability_transport_FR

# Gas expenses
e$gas_expenses_dummy <- e$gas_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "gas_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_gas_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_gas_expenses_FR

# Heating expenses
e$heating_expenses_dummy <- e$heating_expenses > 100 # More than 125
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "heating_expenses_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_heating_expenses_FR

# Income
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "income", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_income_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
positive_all_by_income_FR

# Age
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "age", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_age_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = age, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(5, "RdBu")))
positive_all_by_age_FR

# Diploma
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "diploma", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_diploma_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = diploma, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
positive_all_by_diploma_FR

# Female
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "female", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_female_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = female, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Male", "Female"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_female_FR

# Yellow Vests
e$Gilets_jaunes_agg <- e$Gilets_jaunes
e$Gilets_jaunes_agg[e$Gilets_jaunes == "est_dedans"] <- "soutient"
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "Gilets_jaunes_agg", df=e, weights = "weight")))

mean_sd <- mean_sd %>%
  subset(Gilets_jaunes_agg != "est_dedans")
mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))
mean_sd$Gilets_jaunes_agg <- factor(mean_sd$Gilets_jaunes_agg, levels = c("oppose", "NSP", "comprend", "soutient"))


positive_all_by_yellow_vests_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = Gilets_jaunes_agg, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Oppose", "PNR", "Understand", "Support"),values = c(brewer.pal(4, "RdBu")))
positive_all_by_yellow_vests_FR

# CC anthropogenic
e$CC_anthropogenic_dummy <- e$CC_anthropogenic > 0
mean_sd <- bind_rows((lapply(variables_list[2:10], heterogeneity_mean_CI, 
                             heterogeneity_group = "CC_anthropogenic_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list[2:10], labels = policies_label[2:10]))

positive_all_by_CC_anthropogenic_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = CC_anthropogenic_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("CC is not anthropogenic", "CC is anthropogenic"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_CC_anthropogenic_FR

mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "children", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

#Children
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "children", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_children_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = children, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("No child", "child(ren) at home"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_children_FR

#Employment
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "employment_status", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_employment_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = employment_status, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(7, "RdBu")))
positive_all_by_employment_FR



### CC responsible

# Graphs
e$responsible_CC_companies_dummy_2 <- e$responsible_CC_companies == 2
e$responsible_CC_each_dummy_2 <- e$responsible_CC_each == 2
e$responsible_CC_govt_dummy_2 <- e$responsible_CC_govt == 2

e$responsible_CC_companies_dummy_pos <- e$responsible_CC_companies > 0
e$responsible_CC_each_dummy_pos <- e$responsible_CC_each > 0
e$responsible_CC_govt_dummy_pos <- e$responsible_CC_govt > 0

e$collective_pb_score <- e$responsible_CC_each - (e$responsible_CC_rich + e$responsible_CC_govt + e$responsible_CC_companies)/3
e$collective_pb_score_dummy <- e$collective_pb_score > 0

# Option 1
mean_sd <- bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                             heterogeneity_group = "responsible_CC_companies_dummy_2", df=e, weights = "weight")))
mean_sd$responsible_CC_companies_dummy_2[mean_sd$responsible_CC_companies_dummy_2 == "TRUE"] <- "T_companies" 
mean_sd$responsible_CC_companies_dummy_2[mean_sd$responsible_CC_companies_dummy_2 == "FALSE"] <- "F_companies" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_each_dummy_2 = responsible_CC_companies_dummy_2)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                             heterogeneity_group = "responsible_CC_each_dummy_2", df=e, weights = "weight"))))
mean_sd$responsible_CC_each_dummy_2[mean_sd$responsible_CC_each_dummy_2 == "TRUE"] <- "T_each" 
mean_sd$responsible_CC_each_dummy_2[mean_sd$responsible_CC_each_dummy_2 == "FALSE"] <- "F_each" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_govt_dummy_2 = responsible_CC_each_dummy_2)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                           heterogeneity_group = "responsible_CC_govt_dummy_2", df=e, weights = "weight"))))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))


positive_all_by_responsible_option1_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = responsible_CC_govt_dummy_2, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not companies", "Not each", "Not government", "Companies", "Each", "Government"),values = c("#FDDBC7", "#EF8A62", "#B2182B", "#D1E5F0", "#67A9CF", "#2166AC"))
positive_all_by_responsible_option1_FR

# Option 2 (same graph)
mean_sd <- bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                             heterogeneity_group = "responsible_CC_companies_dummy_pos", df=e, weights = "weight")))
mean_sd$responsible_CC_companies_dummy_pos[mean_sd$responsible_CC_companies_dummy_pos == "TRUE"] <- "T_companies" 
mean_sd$responsible_CC_companies_dummy_pos[mean_sd$responsible_CC_companies_dummy_pos == "FALSE"] <- "F_companies" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_each_dummy_pos = responsible_CC_companies_dummy_pos)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                            heterogeneity_group = "responsible_CC_each_dummy_pos", df=e, weights = "weight"))))
mean_sd$responsible_CC_each_dummy_pos[mean_sd$responsible_CC_each_dummy_pos == "TRUE"] <- "T_each" 
mean_sd$responsible_CC_each_dummy_pos[mean_sd$responsible_CC_each_dummy_pos == "FALSE"] <- "F_each" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_govt_dummy_pos = responsible_CC_each_dummy_pos)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                            heterogeneity_group = "responsible_CC_govt_dummy_pos", df=e, weights = "weight"))))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))


positive_all_by_responsible_option2_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = responsible_CC_govt_dummy_pos, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not companies", "Not each", "Not government", "Companies", "Each", "Government"),values = c("#FDDBC7", "#EF8A62", "#B2182B", "#D1E5F0", "#67A9CF", "#2166AC"))
positive_all_by_responsible_option2_FR

# Option 3
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "collective_pb_score_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_responsible_option3_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = collective_pb_score_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Negative score", "Positive score"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_responsible_option3_FR

# Trust
e$can_trust_govt_dummy <- e$can_trust_govt > 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "can_trust_govt_dummy", df=e, weights = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_trust_govt_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = can_trust_govt_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Don't trust govt", "Trust govt") ,values = c("#FDAE61", "#ABD9E9"))
positive_all_by_trust_govt_FR



### Comparative graph for Urban_Rural
## Urban category
variables_list <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
policies_label <- c("Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")

# FR
mean_sd_FR <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban", df=fr, weights = "weight")))
mean_sd_FR <- mean_sd_FR %>%
  subset(urban != "0") # 1 obs is 0
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

support_by_urban_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = urban, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Rural", "Urban"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(24.5, 67.5)
support_by_urban_FR

# US
mean_sd_US <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban", df = us, weights = "weight")))

mean_sd_US$policy <- factor(mean_sd_US$policy, levels =  variables_list, labels = policies_label)
mean_sd_US$urban_category <- factor(mean_sd_US$urban, levels = c("Rural", "Urban"))

support_by_urban_US <- ggplot(mean_sd_US) +
  geom_pointrange( aes(x = V1, y = policy, color = urban, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'U.S.', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "none", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Rural", "Urban"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(24.5, 67.5)
support_by_urban_US

# DK
mean_sd_DK <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban", df=dk, weights = "weight")))

mean_sd_DK$policy <- factor(mean_sd_DK$policy, levels =  variables_list, labels = policies_label)
mean_sd_DK$urban_category <- factor(mean_sd_DK$urban, levels = c("Rural", "Urban"))

support_by_urban_DK <- ggplot(mean_sd_DK) +
  geom_pointrange( aes(x = V1, y = policy, color = urban, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Denmark', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "none", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Rural", "Urban"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(24.5, 67.5)
support_by_urban_DK

# DE
mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban", df=de, weights = "weight")))
mean_sd_DE <- mean_sd_DE %>%
  subset(urban != "0") # 1 obs is 0
mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)

support_by_urban_DE <- ggplot(mean_sd_DE) +
  geom_pointrange( aes(x = V1, y = policy, color = urban, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Germany', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "none", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Rural", "Urban"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(24.5, 67.5)
support_by_urban_DE


# mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI,
#                                 heterogeneity_group = "urban_category", df=de, weights = "weight")))
# mean_sd_DE <- mean_sd_DE %>%
#   subset(urban_category != "0") # 1 obs is 0
# mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)
# mean_sd_DE$urban_category <- factor(mean_sd_DE$urban, levels = c('"Rural"', '"Towns_and_Suburbs"', '"Cities"'))
# 
# support_by_urban_cat_DE <- ggplot(mean_sd_DE) +
#   geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
#   labs(x = 'Support', y = 'Germany', color="") +
#   theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
#   scale_color_manual(labels = c("Rural", "Towns and Suburbs", "Cities"), values = c("#FDAE61", "#FFED6F", "#ABD9E9"))
# support_by_urban_cat_DE

# All
ggarrange(support_by_urban_FR, support_by_urban_US, support_by_urban_DE, support_by_urban_DK,
          nrow=2, ncol=2, common.legend = TRUE, legend = "top", align = "v")


## Income Quartiles

# FR
mean_sd_FR <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", df=fr, weights = "weight")))
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

support_by_incomeQ_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Q1", "Q2", "Q3", "Q4"),values = c(brewer.pal(4, "RdBu"))) +
  xlim(27, 68)
support_by_incomeQ_FR

# US
mean_sd_US <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", df=us, weights = "weight")))
mean_sd_US$policy <- factor(mean_sd_US$policy, levels =  variables_list, labels = policies_label)

support_by_incomeQ_US <- ggplot(mean_sd_US) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'U.S.', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Q1", "Q2", "Q3", "Q4"),values = c(brewer.pal(4, "RdBu"))) +
  xlim(27, 68)
support_by_incomeQ_US

# DK
mean_sd_DK <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", df=dk, weights = "weight")))
mean_sd_DK$policy <- factor(mean_sd_DK$policy, levels =  variables_list, labels = policies_label)

support_by_incomeQ_DK <- ggplot(mean_sd_DK) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Denmark', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Q1", "Q2", "Q3", "Q4"),values = c(brewer.pal(4, "RdBu"))) +
  xlim(27, 68)
support_by_incomeQ_DK

# DE
mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", df=de, weights = "weight")))
mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)

support_by_incomeQ_DE <- ggplot(mean_sd_DE) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Germany', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Q1", "Q2", "Q3", "Q4"),values = c(brewer.pal(4, "RdBu"))) +
  xlim(27, 68)
support_by_incomeQ_DE

# All
ggarrange(support_by_incomeQ_FR, support_by_incomeQ_US, support_by_incomeQ_DE, support_by_incomeQ_DK,
          nrow=2, ncol=2, common.legend = TRUE, legend = "top", align = "v")


## Income Median

# FR
mean_sd_FR <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=fr, weights = "weight")))
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

support_by_incomeM_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("M1", "M2"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(29, 66)
support_by_incomeM_FR


# US
mean_sd_US <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=us, weights = "weight")))
mean_sd_US$policy <- factor(mean_sd_US$policy, levels =  variables_list, labels = policies_label)

support_by_incomeM_US <- ggplot(mean_sd_US) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'U.S.', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("M1", "M2"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(29, 66)
support_by_incomeM_US

# DK
mean_sd_DK <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=dk, weights = "weight")))
mean_sd_DK$policy <- factor(mean_sd_DK$policy, levels =  variables_list, labels = policies_label)

support_by_incomeM_DK <- ggplot(mean_sd_DK) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Denmark', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("M1", "M2"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(29, 66)
support_by_incomeM_DK
# DE
mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=de, weights = "weight")))
mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)

support_by_incomeM_DE <- ggplot(mean_sd_DE) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Germany', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("M1", "M2"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(29, 66)
support_by_incomeM_DE

## All Countries
ggarrange(support_by_incomeM_FR, support_by_incomeM_US, support_by_incomeM_DE, support_by_incomeM_DK,
          nrow=2, ncol=2, common.legend = TRUE, legend = "top", align = "v")


## Trust in government

# FR
mean_sd_FR <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "can_trust_govt < 0", df=fr, weights = "weight", condition = " > 0")))
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

support_by_govtrust_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = can_trust_govt, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("M1", "M2"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(29, 66)
support_by_govtrust_FR

## Attitudes positives
variables_list <- c("CC_problem", "CC_anthropogenic", "CC_dynamic", "CC_will_end", "net_zero_feasible", "CC_affects_self", "effect_halt_CC_lifestyle", "effect_halt_CC_economy")
policies_label <- c("CC is an important problem", "CC exists, is anthropogenic", "Cutting GHG emisions by half \n sufficient to stop rise in temperatures", "Likely to halt CC by the end of the century",
                    "Feasible to stop GHG emissions \n while sustaining satisfactory \n standards of living in [country]", "CC will negatively affect personal lifestyle", "Negative effects of ambitious policies on lifestyle", "Positive effects of ambitious policies \n on the [country] economy and employment")

mean_sd_all <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                 heterogeneity_group = "country", df=all, weights = "weight")))
mean_sd_all$policy <- factor(mean_sd_all$policy, levels =  variables_list, labels = policies_label)

CC_attitude_positive_CI_countries <- ggplot(mean_sd_all) +
  geom_pointrange( aes(x = V1, y = fct_rev(policy), color = country, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Germany", "Denmark", "France", "U.S."), values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9"))

CC_attitude_positive_CI_countries

## Willing to change behavior
variables_list <- c("willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_beef", "willing_limit_heating")
policies_label <- c("Willing to limit flying", "Willing to limit driving", "Willing to have a fuel-efficient or electric vehicle", "Willing to limit beef consumption", "Willing to limit heating or cooling your home")


mean_sd_all <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                 heterogeneity_group = "country", df=all, weights = "weight")))
mean_sd_all$policy <- factor(mean_sd_all$policy, levels =  variables_list, labels = policies_label)

willing_positive_CI_countries <- ggplot(mean_sd_all) +
  geom_pointrange( aes(x = V1, y = fct_rev(policy), color = country, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Germany", "Denmark", "France", "U.S."), values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9"))

willing_positive_CI_countries

## Support Policies
variables_list <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
policies_label <- c("Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")

mean_sd_all <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                 heterogeneity_group = "country", df=all, weights = "weight")))
mean_sd_all$policy <- factor(mean_sd_all$policy, levels =  variables_list, labels = policies_label)

policies_all_support_positive_CI_countries <- ggplot(mean_sd_all) +
  geom_pointrange( aes(x = V1, y = fct_rev(policy), color = country, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Germany", "Denmark", "France", "U.S."), values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9"))

policies_all_support_positive_CI_countries

## Use of tax revenue
variables_list <- c("tax_transfer_constrained_hh", "tax_transfer_poor", "tax_transfer_all", "tax_reduction_personal_tax", "tax_reduction_corporate_tax", "tax_rebates_affected_firms", "tax_investments", "tax_subsidies", "tax_reduction_deficit")
policies_label <- c("Cash for constrained HH", "Cash for the poorest", "Equal cash for all", "Reduction in income tax", "Reduction in corporate tax", "Tax rebate for affected firms", "Funding green infrastructures", "Subsidies to low-carbon technologies", "Reduction in the deficit")

mean_sd_all <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                 heterogeneity_group = "country", df=all, weights = "weight")))
mean_sd_all$policy <- factor(mean_sd_all$policy, levels =  variables_list, labels = policies_label)

tax_all_positive_CI_countries <- ggplot(mean_sd_all) +
  geom_pointrange( aes(x = V1, y = fct_rev(policy), color = country, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Germany", "Denmark", "France", "U.S."), values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9"))

tax_all_positive_CI_countries
