# Nb: condition is > 0. Might be different for other variables
# Return a df with mean and CI for each sub-group
heterogeneity_mean_CI <- function(variable_name, heterogeneity_group, df=e, weights = "weight"){
  # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
  mean_sd <- as.data.frame(sapply(split(df, df[[heterogeneity_group]]), function(x) c(wtd.mean(x[[variable_name]] > 0, w = x[[weights]], na.rm=T)*100, sqrt(wtd.var(x[[variable_name]] > 0, w = x[[weights]], na.rm=T)*100/sqrt(NROW(x))))))
  # Get low and high bounds of CI. For the moment only 10% CI
  mean_sd <- as.data.frame(t(apply(mean_sd,2, function(x) c(x[1],x[1]-1.645*x[2], x[1]+1.645*x[2]))))
  mean_sd <- tibble::rownames_to_column(mean_sd, heterogeneity_group)
  mean_sd$policy <- variable_name
  
  return(mean_sd)
}

# e <- us
e <- fr
# e <- dk
variables_list <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")

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
  geom_pointrange( aes(x = V1, y = policy, color = left_right, xmin = V2, xmax = V3)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(5, "RdYlBu"), "gray70"))
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
  geom_pointrange( aes(x = V1, y = policy, color = vote_agg, xmin = V2, xmax = V3)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c("#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6", "gray70"))
support_by_vote_agg_FR

# Urban category
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "urban_category", df=e, weights = "weight")))
mean_sd <- mean_sd %>%
  subset(urban_category != "0") # 1 obs is 0
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)
mean_sd$urban_category <- factor(mean_sd$urban_category, levels = c("Other", "Couronne_GP", "GP"))

support_by_urban_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = urban_category, xmin = V2, xmax = V3)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Rural", "Couronne Grand Pôle", "Grand Pôle"), values = c(brewer.pal(3, "RdYlBu")))
support_by_urban_FR

# Polluting_sector
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "polluting_sector", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_polluting_sector_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = polluting_sector, xmin = V2, xmax = V3)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not Polluting", "Polluting"),values = c("#FDAE61", "#ABD9E9"))
support_by_polluting_sector_FR

# Availability Transport
e$availability_transport_dummy <- e$availability_transport >= 0
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "availability_transport_dummy", df=e, weights = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_availability_transport_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = availability_transport_dummy, xmin = V2, xmax = V3)) +
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
  geom_pointrange( aes(x = V1, y = policy, color = gas_expenses_dummy, xmin = V2, xmax = V3)) +
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
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3)) +
  labs(x = 'Support', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
support_by_heating_expenses_FR
