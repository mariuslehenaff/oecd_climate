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
variables_list <- c("standard_support", "investments_support", "tax_transfers_support")

# Apply to get df with info on each variable
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "political_affiliation", df=e, weights = "weight")))

## Plot creation
mean_sd <- subset(mean_sd, political_affiliation %in% c("Democrat", "Republican"))
policies_label <- c("Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")
mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

support_by_political_US <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = political_affiliation, xmin = V2, xmax = V3)) +
  labs(x = 'Support', y = '', color ="", title = 'Policy support by political affiliations') + 
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_color_hue(direction = -1)
save_plotly(support_by_political_US, width= 800, height=400)
