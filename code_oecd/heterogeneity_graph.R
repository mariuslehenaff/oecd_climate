
# Nb: condition is > 0. Might be different for other variables
# Return a df with mean and CI for each sub-group
# condition: corresponds to condition we want to apply to variables of interests. Default is "> 0" as origianlly the function
# was made for support variables
# heterogeneity_condition: corresponds to condition we want to apply to a subgroup. For instance, focus on people who don't trust
# by default there is no condition.
heterogeneity_mean_CI <- function(variable_name, heterogeneity_group, heterogeneity_condition = "", country_heterogeneity = FALSE, along_labels = "", df=e, weight = "weight", condition = "> 0", confidence = 0.95){
  # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
  # TODO: on_control = T
  CI <- 1-(1-confidence)/2
  if (country_heterogeneity){
   mean_ci <- as.data.frame(sapply(split(df, list(df$country_name, eval(str2expression(paste("df[[heterogeneity_group]]", heterogeneity_condition, sep = "")))), drop = F), function(x) c(unique(x$country_name), unique(x[[heterogeneity_group]]), eval(str2expression(paste("wtd.mean(x[[variable_name]]",condition,", w = x[[weight]], na.rm=T)*100"))), eval(str2expression(paste("sqrt(modi::weighted.var(x[[variable_name]]", condition," , w = x[[weight]], na.rm=T))/sqrt(NROW(x))*100"))))))
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
    mean_ci <- as.data.frame(sapply(split(df, eval(str2expression(paste("df[[heterogeneity_group]]", heterogeneity_condition, sep = "")))), function(x) c(eval(str2expression(paste("wtd.mean(x[[variable_name]]", condition, ", w = x[[weight]], na.rm=T)*100"))), eval(str2expression(paste("sqrt(modi::weighted.var(x[[variable_name]]", condition," , w = x[[weight]], na.rm=T))/sqrt(NROW(x))*100"))))))
    mean_ci <- as.data.frame(t(apply(mean_ci,2, function(x) c(x[1],x[1]-qnorm(CI)*x[2], x[1]+qnorm(CI)*x[2]))))
    mean_ci <- tibble::rownames_to_column(mean_ci, heterogeneity_group)
    mean_ci$variable <- variable_name # changed policy to variable, and mean_sd to mean_ci
    names(mean_ci) <- c("along", "mean", "CI_low", "CI_high", "variable") # this line is new
  }
  return(mean_ci)
}

variables_list <- variables_all_policies_support <- c("standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
#variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")
policies_label <- labels_all_policies_support <- c("Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with cash transfer")

plot_along_old <- function(vars, along, name = NULL, labels = vars, legend_x = '', legend_y = '', invert_point_y_axis = FALSE, df = e, folder = '../figures/country_comparison/', weight = "weight", width = dev.size('px')[1], height = dev.size('px')[2]) {
  levels_along <- Levels(df[[along]]) # TODO! automatic name, conditions, show legend for 20 countries (display UA!) even if there is less than 4 variables, order countries as usual
  if (is.missing(name)) name <- paste0(vars[1], "_by_", along, "_") #name <- sub("variables_", "", deparse(substitute(vars)))
  mean_sd <- bind_rows((lapply(vars, heterogeneity_mean_CI, heterogeneity_group = along, df=df, weight = weight)))
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

# gives a list of regressions with given covariates and the different values for the 'subsamples' variable and the 'outcomes'
# outcomes, covariates: string vectors / subsamples: variable name
# /!\ when logit_margin = T, we don't take weight into account (haven't found an R function that gives the marginal logit effects with weight)
regressions_list <- function(outcomes, covariates, subsamples = NULL, df = e, logit = c(FALSE), weight = 'weight', atmean = T, logit_margin = T, summary = FALSE) {
  # TODO! handle outcomes of type "future_richness" (so that they are understood as as.numeric(future_richness))
  # TODO! handle along of type "income" (so that they are understood as as.factor(income)) => this can be done using , names_levels = paste0("as.factor(income)", c("Q1", "Q2", "Q3", "Q4"))
  if (length(logit)==1) if (is.null(subsamples)) logit <- rep(logit, length(outcomes)) else logit <- logit <- rep(logit, length(outcomes)*max(1, length(Levels(df[[subsamples]]))))
  regs <- list()
  i <- 0
  if (!is.null(subsamples)) {
    if (subsamples %in% covariates) {
      warning("subsamples should not be in covariates")
      covariates <- covariates[covariates!=subsamples] }
    for (s in Levels(df[[subsamples]])) {
      regs <- c(regs, regressions_list(outcomes = outcomes, covariates = covariates, subsamples = NULL, df = df[df[[subsamples]]==s,], logit = logit[(i*length(outcomes)+1):((i+1)*length(outcomes))], weight = weight, atmean = atmean, logit_margin = logit_margin))
      i <- i + 1   }
  } else for (y in outcomes) {
    formula <- as.formula(paste(y, " ~ ", paste(covariates, collapse = ' + ')))
    i <- i + 1
    if (logit[i]) {
      if (logit_margin) {
        reg <- logitmfx(formula, data = df, atmean = atmean)$mfxest 
      } else reg <- glm(formula, family = binomial(link='logit'), data = df, weights = weight)
    } else reg <- lm(formula, data = df, weights = weight)
    if (summary) reg <- summary(reg)
    regs <- c(regs, list(reg))
  }
  return(regs)
}
  
# Given a set of regressions with one common variable (along), gives the coefs and CI of the levels of that variable.
mean_ci_along_regressions <- function(regs, along, labels, df = e, origin = 0, logit = c(FALSE), logit_margin = T, confidence = 0.95,
                                      subsamples = NULL, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), weight = 'weight') { # to handle numeric variables: levels_along = ifelse(is.numeric(df[[along]]), c(), Levels(df[[along]]))
  # names_levels[1] should correspond to the control group (concatenation of along and the omitted level)
  # origin can be 0, the intercept, the true (or predicted) mean of the control group, or all variables at their mean except along (at 0 i.e. the control group)
  # TODO: logit, origin
  k <- length(names_levels)
  if (k < 2) warning("along must have several levels.")
  if (length(levels_along)!=length(names_levels)) warning("levels_along and names_levels must have same length.")
  mean_ci <- data.frame()
  i <- 0
  if (class(regs) != "list") regs <- list(regs)
  if (length(logit)==1) logit <- rep(logit, length(regs)) # TODO determine automatically whether logit through class(regs)
  for (reg in regs) {
    i <- i+1
    label <- rep(labels[i], length(levels_along))
    if (logit[i] & logit_margin) {
      if ("lm" %in% class(reg)) warning("Logit margins should be provided: logitmfx(..)$mfxest")
      coefs <- c(0, reg[names_levels[2:k],1])
      sd <- c(0, reg[names_levels[2:k],2])
      z <- qnorm(1-(1-confidence)/2)
      CI <- cbind(coefs - z*sd, coefs + z*sd)       
    } else {
      if (logit[i]) warning("Are you sure you want the logit coefficients rather than the marginal effects? If not, set logit_margin = T.")
      if (!is.null(subsamples)) data_s <- df[df[[subsamples]] == Levels(df[[subsamples]])[i],] else data_s <- df
      if (origin == 'others_at_mean') {
        origin <- 0
        CI_origin <- c(0, 0) # to be coded, perhaps with: emmeans(reg, ~ 1 | along)
      } else { # TODO! check that these confidence intervals are correct (I doubt it)
        mean_ci_origin <- binconf(x = sum(data_s[[weight]][data_s[[along]] == levels_along[1]], na.rm=T), n = sum(data_s[[weight]], na.rm=T), alpha = 1-confidence)
        CI_origin <- mean_ci_origin[2:3] - (origin == 0) * mean_ci_origin[1] } # c(0, 0) # 
      coefs <- origin + c(0, reg$coefficients[names_levels[2:k]])
      CI <- rbind(CI_origin, confint(reg, names_levels[2:k], confidence)) 
    }
    mean_ci_reg <- data.frame(y = label, mean = coefs, CI_low = CI[,1], CI_high = CI[,2], along = levels_along)
    mean_ci <- rbind(mean_ci, mean_ci_reg)    
  }
  row.names(mean_ci) <- NULL
  return(mean_ci)
}

# default marginal effects represent the partial effects for the average observation. If atmean = FALSE the function calculates average partial effects.
# formula <- as.formula("policies_support>0 ~ gender + age + income + education + hit_by_covid + employment_agg + treatment")
# formula2 <- as.formula("tax_transfers_support>0 ~ gender + age + income + education + hit_by_covid + employment_agg + treatment")
# reg <- lm(formula, data=e, weight=e$weight)
# reg2 <- lm(formula, data=e, weight=e$weight)
# (foo <- mean_ci_along_regressions(list(reg, logit_margins), "treatment", c("main policies", "tax and transfers"), logit = c(F, T)))
# plot_along(foo, save = FALSE)
# summary(reg)
# 
# confint(reg)
# logit <- glm(formula, family = binomial(link='logit'), data=e)
# summary(logit)
# logit_margins <- logitmfx(formula, e, atmean=T)$mfxest
# logit_margins

# Two cases: with covariates (coefficients or marginal effects are shown, depending on origin = 0 or not) or without covariates (i.e. unconditional means of subgroups)
# Three configurations: a. one outcomes, two heterogeneities / b. and c. different outcomes, one heterogeneity (c. is invert_y_along = T)
# a. one outcome, y: subsamples (e.g. countries), along: heterogeneity; b. y: outcomes, along; c. y: heterogeneity, along: outcomes
mean_ci <- function(along, outcome_vars = outcomes, outcomes = paste(outcome_vars, conditions), covariates = NULL, subsamples = NULL, conditions = c("> 0"), invert_y_along = FALSE, df = e, labels = outcome_vars,
                    origin = 0, logit = c(FALSE), weight = 'weight', atmean = T, logit_margin = T, confidence = 0.95, 
                    labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), heterogeneity_condition = "") {
  z <- qnorm(1-(1-confidence)/2)
  if (labels_along == levels_along & is.logical(df[[along]])) { labels_along <- paste0(along, ": ", levels_along) }
  names(labels_along) <- as.character(levels_along) # setNames(levels_along, levels_along)
  if (!is.null(covariates)) {
    if (!(along %in% covariates)) print("ERROR: along must be in covariates")
    if (any(logit) & !logit_margin) print("Warning: Are you sure you want the logit coefficients rather than the marginal effects? If not, set logit_margin = T.")
    if (!is.null(subsamples) & (missing(labels) | labels == outcome_vars)) labels <- Levels(df[[subsamples]])
    regs <- regressions_list(outcomes = outcomes, covariates = covariates, subsamples = subsamples, df = df, logit = logit, weight = weight, atmean = atmean, logit_margin = logit_margin, summary = FALSE)
    mean_ci <- mean_ci_along_regressions(regs = regs, along = along, labels = labels, df = df, origin = origin, logit = logit, logit_margin = logit_margin, confidence = confidence, subsamples = subsamples, names_levels = names_levels, levels_along = levels_along, weight = weight)
  } else {
    if (!is.null(subsamples)) { # Configuration a.
      if (length(outcomes) > 1) warning("There cannot be several outcomes with subsamples, only the first outcome will be used.")
      outcome <- outcomes[1]
      y_loop <- Levels(df[[subsamples]])
      if (missing(labels) | labels == outcome_vars) labels <- y_loop # TODO: replace by/use name_levels or levels_along
      if (is.character(y_loop)) y_loop <- paste0("'", y_loop, "'")
      cond <- paste0("[x$", subsamples, "==", y_loop, "]")
      configurations <- paste0("(x$", outcome, ")", cond, ", w = x[[weight]]", cond)
    } else { # Configuration b.
      y_loop <- outcomes
      configurations <- paste0("x$", y_loop, ", w = x[[weight]]")
    }
    i <- 0
    mean_ci <- data.frame()
    for (configuration in configurations) {
      i <- i + 1
      mean_ci_reg <- as.data.frame(sapply(split(df, eval(str2expression(paste("df[[along]]", heterogeneity_condition, sep = "")))), 
                                      function(x) c(eval(str2expression(paste("wtd.mean(", configuration, ", na.rm=T)"))), 
                                                    eval(str2expression(paste("sqrt(modi::weighted.var(", configuration,", na.rm=T))/sqrt(NROW(x))"))))))
      mean_ci_reg <- as.data.frame(t(apply(mean_ci_reg, 2, function(x) c(x[1],x[1]-z*x[2], x[1]+z*x[2]))))
      mean_ci_reg <- tibble::rownames_to_column(mean_ci_reg, along) 
      mean_ci_reg$y <- labels[i]
      names(mean_ci_reg) <- c("along", "mean", "CI_low", "CI_high", "y")
      mean_ci_reg$along <- labels_along[as.character(mean_ci_reg$along)]
      mean_ci <- rbind(mean_ci, mean_ci_reg) 
    }
  }
  if (invert_y_along) names(mean_ci) <- c("y", "mean", "CI_low", "CI_high", "along")
  # if (invert_y_along) {
  #   names(mean_ci)[which(names(mean_ci) == "along")] <- "temp"
  #   names(mean_ci)[which(names(mean_ci) == "y")] <- "along"
  #   names(mean_ci)[which(names(mean_ci) == "temp")] <- "y"  }
  return(mean_ci)
}

plot_along <- function(along, mean_ci = NULL, vars = outcomes, outcomes = paste(vars, conditions), covariates = NULL, subsamples = NULL, conditions = c("> 0"), invert_y_along = FALSE, df = e, labels = vars,
                       origin = 0, logit = c(FALSE), atmean = T, logit_margin = T, labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), 
                       confidence = 0.95, weight = "weight", heterogeneity_condition = "", return_mean_ci = FALSE, print_name = FALSE, # condition = "> 0", #country_heterogeneity = FALSE, along_labels,
                       legend_x = '', legend_y = '', name = NULL, folder = '../figures/country_comparison/', width = dev.size('px')[1], height = dev.size('px')[2], save = T) {
  # TODO multiple conditions, show legend for 20 countries (display UA!) even if there is less than 4 variables, order countries as usual
  # TODO: automatic values when missing(legend_x), legend_y
  # TODO: labels legend (e.g. instead of T/F for urban)
  if (missing(name) & !missing(vars) & !missing(along)) {
    if (grepl('["\']', deparse(substitute(vars)))) {
      name <- ifelse(invert_y_along, paste0(along, "_by_", vars[1]), paste0(vars[1], "_by_", along))
      warning("The filename is formed with the first variable name, given that the argument 'name' is missing.")
    } else name <- ifelse(invert_y_along, name <- paste0(along, "_by_", deparse(substitute(vars))), paste0(deparse(substitute(vars)), "_by_", along))
  } else if (missing(name) & !missing(mean_ci)) { 
    potential_name <- deparse(substitute(mean_ci))
    if (missing(along)) along <- ""
    if (grepl('["\']', potential_name)) warning("(file)name should be provided, or mean_ci should have a name.")
    name <- ifelse(invert_y_along, paste0(along, "_by_", mean_ci), paste0(mean_ci, "_by_", along))
  } else name <- "temp"
  name <- sub("rev(", "", sub(")", "", sub("country_name", "country", name, fixed = T), fixed = T), fixed = T)
  if (print_name) print(name) # TODO: name with subsamples
  
  if (missing(folder) & deparse(substitute(df)) %in% tolower(countries)) folder <- paste0("../figures/", toupper(deparse(substitute(df))), "/")

  if (missing(mean_ci)) mean_ci <- mean_ci(along = along, outcome_vars = vars, outcomes = outcomes, covariates = covariates, subsamples = subsamples, conditions = conditions, invert_y_along = invert_y_along, df = df, labels = labels,
                                    origin = origin, logit = logit, weight = weight, atmean = atmean, logit_margin = logit_margin, confidence = confidence,
                                    names_levels = names_levels, labels_along = labels_along, levels_along = levels_along, heterogeneity_condition = heterogeneity_condition)
    
 #  if (missing(mean_ci)) {
 #    mean_ci <- bind_rows((lapply(vars, heterogeneity_mean_CI, heterogeneity_group = along, df=df, weight = weight, along_labels = along_labels, country_heterogeneity = country_heterogeneity, heterogeneity_condition = heterogeneity_condition, condition = condition, confidence = confidence)))
 #    mean_ci$y <- factor(mean_ci$y, levels = vars, labels = labels) }
 #  
 #  if (invert_y_along & country_heterogeneity == F) {
 #    names(mean_ci)[which(names(mean_ci) == "along")] <- "temp"
 #    names(mean_ci)[which(names(mean_ci) == "y")] <- "along"
 #    names(mean_ci)[which(names(mean_ci) == "temp")] <- "y" # or the les robust one-liner: names(mean_ci) <- c("variable", "mean", "CI_low", "CI_high", "along")
 #  } else if (country_heterogeneity) {
 #    names(mean_ci)[which(names(mean_ci) == "variable")] <- "policy" # TODO: generalize this by rewriting heterogeneity_mean_CI
 #    names(mean_ci)[which(names(mean_ci) == "country")] <- "y"
 # }
  
  plot <- ggplot(mean_ci) + # For plot, we need mean_ci (cols: mean, CI_low,high, variable, along), legend_x, legend_y. For save, we need: name, folder, width, height.
    geom_pointrange( aes(x = mean, y = y, color = along, xmin = CI_low, xmax = CI_high), position = position_dodge(width = .5)) +
    labs(x = legend_x, y = legend_y, color="") + theme(legend.title = element_blank(), legend.position = "top") +
    theme_minimal() # + scale_color_manual(values = color(length(levels_along), theme='rainbow')) # can be theme = 'rainbow', 'RdBu', 'default' or any brewer theme, but the issue with RdBu/default is that the middle one is white for odd number of categories
    # scale_color_manual(labels = Levels(df[[along]]), values = color(length(Levels(df[[along]])), theme='rainbow'))# BUG when we specify labels: the legend does not correspond to the colors
  plot
  if (save) save_plotly(plot, filename = name, folder = folder, width = width, height = height, trim = T)
  if (return_mean_ci) return(mean_ci)
  else return(plot)
}
# example :
example_covariates <- c("treatment", "gender", "income", "urbanity", "country_name")
plot_along(vars = c("tax_transfers_support", "policies_support"), along = "treatment", covariates = example_covariates, save = F)
plot_along(vars = "policies_support", along = "income_factor", subsamples = "country_name", covariates = example_covariates, save = F)
plot_along(vars = "tax_transfers_support", along = "treatment", covariates = example_covariates, logit = T, logit_margin = FALSE)
plot_along(vars = rev(variables_all_policies_support), along = "treatment", covariates = example_covariates, logit = T, logit_margin = FALSE)
# mean_ci(outcome_vars = c("CC_affects_self", "net_zero_feasible", "CC_will_end", "future_richness"), along = "country_name", labels = c("Feels affected by climate change", "Net zero by 2100 feasible", "Likely that climate change ends by 2100", "World in 100 years will be richer"), covariates = example_covariates, logit = T)
plot_along(vars = c("CC_affects_self", "net_zero_feasible", "CC_will_end", "future_richness"), along = "country_name", name = "future_by_country", labels = c("Feels affected by climate change", "Net zero by 2100 feasible", "Likely that climate change ends by 2100", "World in 100 years will be richer"), covariates = example_covariates, logit = T, logit_margin = FALSE)
plot_along(vars = c("CC_affects_self"), along = "country_name", name = "CC_affects_self_by_country", labels = c("Feels affected by climate change"), covariates = example_covariates, logit = T, logit_margin = FALSE)
plot_along(vars = "policies_support", along = "treatment", covariates = example_covariates, logit = T, logit_margin = FALSE)
# Beware, only use one variable at a time with country_heterogeneity = T
# plot_along(vars = "policies_support", along = "urban", country_heterogeneity = T, save = FALSE)
plot_along(vars = variables_all_policies_support, along = "urban_category", df = fr, name = "policies_support_by_urban_category", labels = labels_all_policies_support, covariates = example_covariates)
plot_along(vars = "policies_support", along = "urban", subsamples = "country_name", names_levels = c("Rural", "Urban"))
# For labels, check the output of heterogeneity_mean_CI (first column)
# plot_along(vars = "policies_support", along = "treatment", country_heterogeneity = T, along_labels = c("None", "Climate", "Policy", "Both"), save = FALSE)
mean_sd <- bind_rows((lapply(variables_all_policies_support, heterogeneity_mean_CI, heterogeneity_group = "country", df=all)))
mean_sd$variable <- factor(mean_sd$variable, levels = variables_all_policies_support, labels = variables_all_policies_support)

# eval(parse(along)) !!along as.name(along) substitute(eval(along)) eval(along) substitute(temp) deparse(substitute(temp))
# e <- us
e <- fr
# e <- dk

# Apply to get df with info on each variable
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "left_right", df=e, weight = "weight")))

## Plot creation
#mean_sd <- subset(mean_sd, political_affiliation %in% c("Democrat", "Republican"))
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
                             heterogeneity_group = "vote_agg", df=e, weight = "weight")))
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
                             heterogeneity_group = "urban_category", df=e, weight = "weight")))
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
                             heterogeneity_group = "polluting_sector", df=e, weight = "weight")))

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
                             heterogeneity_group = "availability_transport_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "gas_expenses_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "heating_expenses_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "left_right", df=e, weight = "weight")))

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
                             heterogeneity_group = "vote_agg", df=e, weight = "weight")))
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
                             heterogeneity_group = "urban_category", df=e, weight = "weight")))
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
                             heterogeneity_group = "polluting_sector", df=e, weight = "weight")))

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
                             heterogeneity_group = "availability_transport_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "gas_expenses_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "heating_expenses_dummy", df=e, weight = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
willing_by_heating_expenses_FR

# Income
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "income", df=e, weight = "weight")))

mean_sd$policy <- factor(mean_sd$policy, levels =  variables_list, labels = policies_label)

willing_by_income_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Willingness', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
willing_by_incomes_FR

### ALL POSITIVE ANSWERS PART

# heterogeneity_mean_CI_01 <- function(variable_name, heterogeneity_group, df=e, weight = "weight"){
#   # Take mean normalised on 100 and se*100 = sd/sqrt(N)*100
#   mean_sd <- as.data.frame(sapply(split(df, df[[heterogeneity_group]]), function(x) c(wtd.mean(x[[variable_name]], w = x[[weight]], na.rm=T)*100, sqrt(wtd.var(x[[variable_name]], w = x[[weight]], na.rm=T))/sqrt(sum(x[[weight]]))*100)))
#   # Get low and high bounds of CI. For the moment only 10% CI
#   mean_sd <- as.data.frame(t(apply(mean_sd,2, function(x) c(x[1],x[1]-1.645*x[2], x[1]+1.645*x[2]))))
#   mean_sd <- tibble::rownames_to_column(mean_sd, heterogeneity_group)
#   mean_sd$policy <- variable_name
#   
#   return(mean_sd)
# }
# mean_sd <- bind_rows((lapply(variables_list_index, heterogeneity_mean_CI_01,
#                                           heterogeneity_group = "left_right", df=e, weight = "weight")))
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
                             heterogeneity_group = "left_right", df=e, weight = "weight")))

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
                             heterogeneity_group = "vote_agg", df=e, weight = "weight")))
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
                             heterogeneity_group = "urban_category", df=e, weight = "weight")))
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
                             heterogeneity_group = "polluting_sector", df=e, weight = "weight")))

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
                             heterogeneity_group = "availability_transport_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "gas_expenses_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "heating_expenses_dummy", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_heating_expenses_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = heating_expenses_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Less than €125", "More than €125"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_heating_expenses_FR

# Income
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "income", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_income_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = income, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
positive_all_by_income_FR

# Age
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "age", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_age_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = age, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(5, "RdBu")))
positive_all_by_age_FR

# Diploma
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "diploma", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_diploma_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = diploma, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(values = c(brewer.pal(4, "RdBu")))
positive_all_by_diploma_FR

# Female
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "female", df=e, weight = "weight")))

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
                             heterogeneity_group = "Gilets_jaunes_agg", df=e, weight = "weight")))

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
                             heterogeneity_group = "CC_anthropogenic_dummy", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list[2:10], labels = policies_label[2:10]))

positive_all_by_CC_anthropogenic_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = CC_anthropogenic_dummy, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("CC is not anthropogenic", "CC is anthropogenic"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_CC_anthropogenic_FR

mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "children", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

#Children
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "children", df=e, weight = "weight")))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))

positive_all_by_children_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = children, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("No child", "child(ren) at home"),values = c("#FDAE61", "#ABD9E9"))
positive_all_by_children_FR

#Employment
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "employment_status", df=e, weight = "weight")))

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
                             heterogeneity_group = "responsible_CC_companies_dummy_2", df=e, weight = "weight")))
mean_sd$responsible_CC_companies_dummy_2[mean_sd$responsible_CC_companies_dummy_2 == "TRUE"] <- "T_companies" 
mean_sd$responsible_CC_companies_dummy_2[mean_sd$responsible_CC_companies_dummy_2 == "FALSE"] <- "F_companies" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_each_dummy_2 = responsible_CC_companies_dummy_2)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                            heterogeneity_group = "responsible_CC_each_dummy_2", df=e, weight = "weight"))))
mean_sd$responsible_CC_each_dummy_2[mean_sd$responsible_CC_each_dummy_2 == "TRUE"] <- "T_each" 
mean_sd$responsible_CC_each_dummy_2[mean_sd$responsible_CC_each_dummy_2 == "FALSE"] <- "F_each" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_govt_dummy_2 = responsible_CC_each_dummy_2)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                            heterogeneity_group = "responsible_CC_govt_dummy_2", df=e, weight = "weight"))))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))


positive_all_by_responsible_option1_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = responsible_CC_govt_dummy_2, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not companies", "Not each", "Not government", "Companies", "Each", "Government"),values = c("#FDDBC7", "#EF8A62", "#B2182B", "#D1E5F0", "#67A9CF", "#2166AC"))
positive_all_by_responsible_option1_FR

# Option 2 (same graph)
mean_sd <- bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                             heterogeneity_group = "responsible_CC_companies_dummy_pos", df=e, weight = "weight")))
mean_sd$responsible_CC_companies_dummy_pos[mean_sd$responsible_CC_companies_dummy_pos == "TRUE"] <- "T_companies" 
mean_sd$responsible_CC_companies_dummy_pos[mean_sd$responsible_CC_companies_dummy_pos == "FALSE"] <- "F_companies" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_each_dummy_pos = responsible_CC_companies_dummy_pos)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                            heterogeneity_group = "responsible_CC_each_dummy_pos", df=e, weight = "weight"))))
mean_sd$responsible_CC_each_dummy_pos[mean_sd$responsible_CC_each_dummy_pos == "TRUE"] <- "T_each" 
mean_sd$responsible_CC_each_dummy_pos[mean_sd$responsible_CC_each_dummy_pos == "FALSE"] <- "F_each" 
mean_sd <- mean_sd %>%
  rename(responsible_CC_govt_dummy_pos = responsible_CC_each_dummy_pos)

mean_sd <- rbind(mean_sd, bind_rows((lapply(variables_list_logic, heterogeneity_mean_CI, 
                                            heterogeneity_group = "responsible_CC_govt_dummy_pos", df=e, weight = "weight"))))

mean_sd$policy <- fct_rev(factor(mean_sd$policy, levels =  variables_list, labels = policies_label))


positive_all_by_responsible_option2_FR <- ggplot(mean_sd) +
  geom_pointrange( aes(x = V1, y = policy, color = responsible_CC_govt_dummy_pos, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers in (%)', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Not companies", "Not each", "Not government", "Companies", "Each", "Government"),values = c("#FDDBC7", "#EF8A62", "#B2182B", "#D1E5F0", "#67A9CF", "#2166AC"))
positive_all_by_responsible_option2_FR

# Option 3
mean_sd <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                             heterogeneity_group = "collective_pb_score_dummy", df=e, weight = "weight")))

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
                             heterogeneity_group = "can_trust_govt_dummy", df=e, weight = "weight")))

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
                                heterogeneity_group = "urban", df=fr, weight = "weight")))
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
                                heterogeneity_group = "urban", df = us, weight = "weight")))

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
                                heterogeneity_group = "urban", df=dk, weight = "weight")))

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
                                heterogeneity_group = "urban", df=de, weight = "weight")))
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
#                                 heterogeneity_group = "urban_category", df=de, weight = "weight")))
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
                                heterogeneity_group = "income", df=fr, weight = "weight")))
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
                                heterogeneity_group = "income", df=us, weight = "weight")))
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
                                heterogeneity_group = "income", df=dk, weight = "weight")))
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
                                heterogeneity_group = "income", df=de, weight = "weight")))
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
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=fr, weight = "weight")))
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
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=us, weight = "weight")))
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
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=dk, weight = "weight")))
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
                                heterogeneity_group = "income", heterogeneity_condition = "> 2", df=de, weight = "weight")))
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
                                heterogeneity_group = "can_trust_govt", heterogeneity_condition = "< 0", df=fr, weight = "weight")))
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

support_by_govtrust_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = can_trust_govt, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Do not trust Government"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(22.5, 66.5)
support_by_govtrust_FR

# US
mean_sd_US <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "can_trust_govt", heterogeneity_condition = "< 0", df=us, weight = "weight")))
mean_sd_US$policy <- factor(mean_sd_US$policy, levels =  variables_list, labels = policies_label)

support_by_govtrust_US <- ggplot(mean_sd_US) +
  geom_pointrange( aes(x = V1, y = policy, color = can_trust_govt, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'U.S.', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Do not trust Government"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(22.5, 66.5)
support_by_govtrust_US

# DK
mean_sd_DK <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "can_trust_govt", heterogeneity_condition = "< 0", df=dk, weight = "weight")))
mean_sd_DK$policy <- factor(mean_sd_DK$policy, levels =  variables_list, labels = policies_label)

support_by_govtrust_DK <- ggplot(mean_sd_DK) +
  geom_pointrange( aes(x = V1, y = policy, color = can_trust_govt, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Denmark', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Do not trust Government"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(22.5, 66.5)
support_by_govtrust_DK

# DE
mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "can_trust_govt", heterogeneity_condition = "< 0", df=de, weight = "weight")))
mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)

support_by_govtrust_DE <- ggplot(mean_sd_DE) +
  geom_pointrange( aes(x = V1, y = policy, color = can_trust_govt, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Germany', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Do not trust Government"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(22.5, 66.5)
support_by_govtrust_DE

## All Countries
ggarrange(support_by_govtrust_FR, support_by_govtrust_US, support_by_govtrust_DE, support_by_govtrust_DK,
          nrow=2, ncol=2, common.legend = TRUE, legend = "top", align = "v")

## Education

# FR
mean_sd_FR <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "college", df=fr, weight = "weight")))
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

support_by_college_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = college, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("College Degree", "No College Degree"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(25, 72.5)
support_by_college_FR

# US
mean_sd_US <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "college", df=us, weight = "weight")))
mean_sd_US$policy <- factor(mean_sd_US$policy, levels =  variables_list, labels = policies_label)

support_by_college_US <- ggplot(mean_sd_US) +
  geom_pointrange( aes(x = V1, y = policy, color = college, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'U.S.', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("College Degree", "No College Degree"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(25, 72.5)
support_by_college_US

# DK
mean_sd_DK <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "college", df=dk, weight = "weight")))
mean_sd_DK$policy <- factor(mean_sd_DK$policy, levels =  variables_list, labels = policies_label)

support_by_college_DK <- ggplot(mean_sd_DK) +
  geom_pointrange( aes(x = V1, y = policy, color = college, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Denmark', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("College Degree", "No College Degree"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(25, 72.5)
support_by_college_DK

# DE
mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "college", df=de, weight = "weight")))
mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)

support_by_college_DE <- ggplot(mean_sd_DE) +
  geom_pointrange( aes(x = V1, y = policy, color = college, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Support', y = 'Germany', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("College Degree", "No College Degree"), values = c("#ABD9E9", "#FDAE61")) +
  xlim(25, 72.5)
support_by_college_DE

## All Countries
ggarrange(support_by_college_FR, support_by_college_US, support_by_college_DE, support_by_college_DK,
          nrow=2, ncol=2, common.legend = TRUE, legend = "top", align = "v")

## Attitudes positives
variables_list <- c("CC_problem", "CC_anthropogenic", "CC_dynamic", "CC_will_end", "net_zero_feasible", "CC_affects_self", "effect_halt_CC_lifestyle", "effect_halt_CC_economy")
policies_label <- c("CC is an important problem", "CC exists, is anthropogenic", "Cutting GHG emisions by half \n sufficient to stop rise in temperatures", "Likely to halt CC by the end of the century",
                    "Feasible to stop GHG emissions \n while sustaining satisfactory \n standards of living in [country]", "CC will negatively affect personal lifestyle", "Negative 
                    effects of ambitious policies on lifestyle", "Positive effects of ambitious policies \n on the [country] economy and employment")

mean_sd_all <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                 heterogeneity_group = "country", df=all, weight = "weight")))
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
                                 heterogeneity_group = "country", df=all, weight = "weight")))
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
                                 heterogeneity_group = "country", df=all, weight = "weight")))
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
                                 heterogeneity_group = "country", df=all, weight = "weight")))
mean_sd_all$policy <- factor(mean_sd_all$policy, levels =  variables_list, labels = policies_label)

tax_all_positive_CI_countries <- ggplot(mean_sd_all) +
  geom_pointrange( aes(x = V1, y = fct_rev(policy), color = country, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers', y = '', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_manual(labels = c("Germany", "Denmark", "France", "U.S."), values = c("#D7191C", "#FDAE61", "#FFED6F", "#ABD9E9"))

tax_all_positive_CI_countries

# Details by country: perception of inequalities
# FR
mean_sd_FR <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "problem_inequality", heterogeneity_condition = "> 0", df=fr, weight = "weight")))
mean_sd_FR$policy <- factor(mean_sd_FR$policy, levels =  variables_list, labels = policies_label)

tax_positive_by_inequality_CI95_FR <- ggplot(mean_sd_FR) +
  geom_pointrange( aes(x = V1, y = policy, color = problem_inequality, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'France', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Concerned about inequalities"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(20.5, 77)
tax_positive_by_inequality_CI95_FR

# US
mean_sd_US <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "problem_inequality", heterogeneity_condition = "> 0", df=us, weight = "weight")))
mean_sd_US$policy <- factor(mean_sd_US$policy, levels =  variables_list, labels = policies_label)

tax_positive_by_inequality_CI95_US <- ggplot(mean_sd_US) +
  geom_pointrange( aes(x = V1, y = policy, color = problem_inequality, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = '', y = 'U.S.', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Concerned about inequalities"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(20.5, 77)
tax_positive_by_inequality_CI95_US

# DK
mean_sd_DK <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "problem_inequality", heterogeneity_condition = "> 0", df=dk, weight = "weight")))
mean_sd_DK$policy <- factor(mean_sd_DK$policy, levels =  variables_list, labels = policies_label)

tax_positive_by_inequality_CI95_DK <- ggplot(mean_sd_DK) +
  geom_pointrange( aes(x = V1, y = policy, color = problem_inequality, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers', y = 'Denmark', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Concerned about inequalities"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(20.5, 77)
tax_positive_by_inequality_CI95_DK

# DE
mean_sd_DE <- bind_rows((lapply(variables_list, heterogeneity_mean_CI, 
                                heterogeneity_group = "problem_inequality", heterogeneity_condition = "> 0", df=de, weight = "weight")))
mean_sd_DE$policy <- factor(mean_sd_DE$policy, levels =  variables_list, labels = policies_label)

tax_positive_by_inequality_CI95_DE <- ggplot(mean_sd_DE) +
  geom_pointrange( aes(x = V1, y = policy, color = problem_inequality, xmin = V2, xmax = V3), position = position_dodge(width = .5)) +
  labs(x = 'Positive answers', y = 'Germany', color="") + 
  theme_minimal() + theme(legend.title = element_blank(), legend.position = "top", axis.title.y = element_text(angle=0, vjust = 0.5)) +
  scale_color_manual(labels = c("Others", "Concerned about inequalities"), values = c("#FDAE61", "#ABD9E9")) +
  xlim(20.5, 77)
tax_positive_by_inequality_CI95_DE

## All Countries
ggarrange(tax_positive_by_inequality_CI95_FR, tax_positive_by_inequality_CI95_US, tax_positive_by_inequality_CI95_DE, tax_positive_by_inequality_CI95_DK,
          nrow=2, ncol=2, common.legend = TRUE, legend = "top", align = "v")



# Heterogeneity Graphs
variables_list <- c("wtp", "willing_limit_flying", "willing_limit_driving", "willing_electric_car", "willing_limit_heating", "willing_limit_beef")
policies_label <- c("WTP", "Limit flying", "Limit driving", "Have electric car", "Limit heating or cooling home", "Limit beef consumption")

plot_along(vars = variables_list, along = "country_name", name = "willingness_by_country", labels = policies_label)


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
plot_along(vars = variables_list, along = "country_name", name = "main_var_by_country", labels = policies_label)


variables_list <- c("CC_problem", "CC_anthropogenic", "CC_dynamic", "CC_will_end", "net_zero_feasible", "CC_affects_self", "effect_halt_CC_lifestyle", "effect_halt_CC_economy")
policies_label <- c("CC is an important problem", "CC exists, is anthropogenic", "Cutting GHG emisions by half \n sufficient to stop rise in temperatures", "Likely to halt CC by the end of the century",
                    "Feasible to stop GHG emissions \n while sustaining satisfactory \n standards of living in [country]", "CC will negatively affect personal lifestyle", "Negative 
                    effects of ambitious policies on lifestyle", "Positive effects of ambitious policies \n on the [country] economy and employment")
plot_along(vars = variables_list, along = "country_name", name = "attitudes_by_country", labels = policies_label)

variables_list <- c("tax_transfer_constrained_hh", "tax_transfer_poor", "tax_transfer_all", "tax_reduction_personal_tax", "tax_reduction_corporate_tax", "tax_rebates_affected_firms", "tax_investments", "tax_subsidies", "tax_reduction_deficit")
policies_label <- c("Cash for constrained HH", "Cash for the poorest", "Equal cash for all", "Reduction in income tax", "Reduction in corporate tax", "Tax rebate for affected firms", "Funding green infrastructures", "Subsidies to low-carbon technologies", "Reduction in the deficit")
plot_along(vars = variables_list, along = "country_name", name = "views_by_country", labels = policies_label)

variables_list <- c("policies_support","standard_public_transport_support", "standard_support", "investments_support", "tax_transfers_support")
policies_label <- c("Main Policies", "Ban of combustion engine \n (public transport made available)", "Ban of combustion engine", "Green investments program", "Carbon tax with \n cash transfer")
plot_along(vars = variables_list, along = "country_name", name = "support_var_by_country", labels = policies_label)
plot_along(vars = variables_list, along = "country_name", invert_point_y_axis = T, name = "support_var_by_country", labels = policies_label)
plot_along(vars = variables_list, along = "treatment", name = "support_var_by_treatment", labels = policies_label)
plot_along(vars = "policies_support", along = "treatment", invert_point_y_axis = T, name = "main_support_var_by_treatment", labels = "Main Policies")

variables_list <- c("CC_anthropogenic", "CC_impacts_extinction", "donation", "should_fight_CC", "willing_limit_driving")
policies_label <- c("CC caused by humans", "CC likely to cause extinction", "Donation", "[country] should fight CC", "Willing to limit driving")
plot_along(vars = variables_list, along = "treatment", name = "attitudes_CC_by_country", labels = policies_label)

variables_list <- c("policies_fair", "policies_self", "policies_poor", "policies_rich", "policies_large_effect", "policies_positive_negative")
policies_label <- c("Fair", "HH would win", "Poor would win", "Rich would win", "Large economic effects", "Negative economic effects")
plot_along(vars = variables_list, along = "treatment", name = "attitudes_pol_by_country", labels = policies_label)
