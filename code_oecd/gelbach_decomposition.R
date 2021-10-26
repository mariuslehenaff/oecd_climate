# /!\ This need to be changed if you're note using STATA SE 17 or a Mac
if (Sys.info()[7] == "Bluebii") {
  options("RStata.StataPath" = '/Applications/Stata/StataSE17.app/Contents/MacOS/stata-se')
  options("RStata.StataVersion" = 17)  
} else if (Sys.info()[7] == "Ana") {
  options("RStata.StataPath" = 'C:/Program Files/Stata/StataSE17')
  options("RStata.StataVersion" = 17)  
}


# var_to_decompose and group_of_interest: you need to input only one variable as a character
# controls and indices, can be a character vector
# Factor variables from control need to be in controls_factor
gelbach_decomposition <- function(var_to_decompose, group_of_interest, controls, controls_factor, indices, df=e, weight=T) {
  # We restrict the df to the variables we'll use, since there can be some incompatibilities
  # in using R dataframes in Stata
  df <- df %>%
    select(c(var_to_decompose, group_of_interest, controls, controls_factor, indices))
  
  # Rename var because problem with Stata for variables with names too long
  indices_short <- c()
  for (i in seq_along(indices)){
    indices_short[i] <- paste("index_", i, sep = "")
  }
  df <- df %>%
    rename_with(~ indices_short[which(indices == .x)], .cols = indices)
  df <- df %>%
    rename("var_to_decompose" = var_to_decompose)
  
  
  # First, we prepare the options for the analysis
  option_b1x2 <- ""
  for (i in seq_along(indices)){
    option_b1x2 <- paste(option_b1x2,"g", i, " = ", indices_short[i], " : ", sep = "")
  }
  option_b1x2 <- substr(option_b1x2, 1, nchar(option_b1x2)-3)
  nbr_indices <- length(indices)
  
  # We stock the different lines of codes for Stata into a vector
  # Each element corresponds to a different line of code to run in Stata
  # We will then collapse those commands altogether to run them w/ RStata
  stata_cmd <- c()
  stata_cmd[1] <- "
  set more off
  ssc install b1x2, replace"
  stata_cmd[2] <- paste("global indices", paste('"', paste(indices_short, collapse = " "), '"', sep =""), sep = " ")
  stata_cmd[3] <- paste("global controls", paste('"', paste(controls, collapse = " "), '"', sep = ""), sep = " ")
  stata_cmd[4] <- paste("global controls_factor", paste('"', paste(controls_factor, collapse = " "), '"', sep = ""), sep = " ")
  stata_cmd[5] <- paste("global option_b1x2", paste('"', option_b1x2, '"', sep = ""), sep = " ")
  stata_cmd[6] <- paste("global nbr_indices", paste(nbr_indices), sep = " ")
  stata_cmd[7] <- paste("global nbr_plus_one_indices", paste(nbr_indices+1), sep = " ")
  stata_cmd[8] <- paste("global var_to_decompose", paste("var_to_decompose"), sep = " ")
  stata_cmd[9] <- paste("local var_to_decompose", paste("var_to_decompose"), sep = " ")
  stata_cmd[10] <- paste("global group_of_interest", paste(group_of_interest), sep = " ")
  stata_cmd[11] <- paste("local group_of_interest", paste(group_of_interest), sep = " ")
  stata_cmd[12] <- "do gelbach_stata.do"
  
  stata_cmd <- paste(stata_cmd, collapse = "\n")
  # We input df, and obtain the data frame with the share explained by each indice
  final <- stata(stata_cmd, data.in = df, data.out = T)
  
  return(final)
}

# List of indices to use
indices_list <- c("index_progressist", "index_knowledge", "index_affected", "index_concerned_about_CC", "index_worried", "index_positive_economy", "index_constrained",
                  "index_policies_efficient", "index_care_poverty", "index_altruism","index_affected_subjective","index_willing_change", "index_lose_policies_subjective", "index_fairness", "index_trust_govt")
indices_lab <- c("Is progressist", "Has a good knowledge of climate change", "Is affected by climate change", "Is concerned about climate change", "Is worried about the future", "Climate policies have a positive effect \n on the economy",
                 "Is financially constrained","Climate policies are efficient", "Cares about poverty and inequalities", "Is willing to donate to reforestation project", "Thinks will suffer of climate change",
                 "Is willing to adopt climate friendly behavior", "Thinks will lose from main policies", "Thinks main policies are fair", "Trusts the governement")
indices_non_left_right <- indices_list[2:length(indices_list)]
indices_policies <- c("index_standard_policy", "index_tax_transfers_policy", "index_investments_policy","index_main_policies",
                      "index_beef_policies","index_international_policies","index_other_policies","index_all_policies")

paste(indices_non_left_right, "_dummies", sep = "")
paste(indices_non_left_right, "_dummies2SD", sep = "")

# Create dummy for left_right
e$right_pol <- e$left_right > 0
#TODO replace right_pol with left_right >=1 and add left_right == 0 in the control
# Creation of Graphs

# Gelbach decomposition of the partisan gap in the policy view index for a ban on combustion_engine
# i.e. which factor better explain the share of partisan gap
# We remove left_right and income_agg from the controls as they are respectively the group of interest and integrated in index_constrained

# Other specifications possible (not used)
# gelbach_right_standard_noD <- gelbach_decomposition(var_to_decompose = indices_policies[1], group_of_interest = "right_pol",
#                                                     controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
#                                                     indices = indices_non_left_right)
# gelbach_right_standard_noD[,1] <- indices_lab[2:length(indices_lab)]
# gelbach_right_standard_D <- gelbach_decomposition(var_to_decompose = indices_policies[1], group_of_interest = "right_pol",
#                                                   controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
#                                                   indices = paste(indices_non_left_right, "_dummies", sep = ""))
# gelbach_right_standard_D[,1] <- indices_lab[2:length(indices_lab)]
# barres(data = t(matrix(gelbach_right_standard_noD$shareExplained/100)), labels = gelbach_right_standard_noD$n,legend = "% Partisan gap explained", rev = F)
# barres(data = t(matrix(gelbach_right_standard_D$shareExplained/100)), labels = gelbach_right_standard_D$n,legend = "% Partisan gap explained", rev = F)


gelbach_right_standard_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[1], group_of_interest = "right_pol",
                                                     controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
                                                     indices = paste(indices_non_left_right, "_dummies2SD", sep = ""))
gelbach_right_standard_D2SD[,1] <- indices_lab[2:length(indices_lab)]
gelbach_right_tax_transfers_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[2], group_of_interest = "right_pol",
                                                          controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
                                                          indices = paste(indices_non_left_right, "_dummies2SD", sep = ""))
gelbach_right_tax_transfers_D2SD[,1] <- indices_lab[2:length(indices_lab)]
gelbach_right_investments_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[3], group_of_interest = "right_pol",
                                                        controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
                                                        indices = paste(indices_non_left_right, "_dummies2SD", sep = ""))
gelbach_right_investments_D2SD[,1] <- indices_lab[2:length(indices_lab)]
gelbach_right_main_policies_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[4], group_of_interest = "right_pol",
                                                          controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
                                                          indices = paste(indices_non_left_right, "_dummies2SD", sep = ""))
gelbach_right_main_policies_D2SD[,1] <- indices_lab[2:length(indices_lab)]
gelbach_right_all_policies_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[8], group_of_interest = "right_pol",
                                                         controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
                                                         indices = paste(indices_non_left_right, "_dummies2SD", sep = ""))
gelbach_right_all_policies_D2SD[,1] <- indices_lab[2:length(indices_lab)]
gelbach_right_willing_change_D2SD <- gelbach_decomposition(var_to_decompose = "index_willing_change_dummies2SD", group_of_interest = "right_pol",
                                                           controls = control_variables[1:4], controls_factor = c("age", "employment_agg", "country"),
                                                           indices = paste(indices_non_left_right[c(1:10,12:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_right_willing_change_D2SD[,1] <- indices_lab[c(2:11,13:length(indices_lab))]

barres(data = t(matrix(gelbach_right_standard_D2SD$shareExplained/100)), labels = gelbach_right_standard_D2SD$n,legend = "% Partisan gap explained", rev = F)
barres(data = t(matrix(gelbach_right_tax_transfers_D2SD$shareExplained/100)), labels = gelbach_right_tax_transfers_D2SD$n,legend = "% Partisan gap explained", rev = F)
barres(data = t(matrix(gelbach_right_investments_D2SD$shareExplained/100)), labels = gelbach_right_investments_D2SD$n,legend = "% Partisan gap explained", rev = F)
barres(data = t(matrix(gelbach_right_main_policies_D2SD$shareExplained/100)), labels = gelbach_right_main_policies_D2SD$n,legend = "% Partisan gap explained", rev = F)
barres(data = t(matrix(gelbach_right_all_policies_D2SD$shareExplained/100)), labels = gelbach_right_all_policies_D2SD$n,legend = "% Partisan gap explained", rev = F)
barres(data = t(matrix(gelbach_right_willing_change_D2SD$shareExplained/100)), labels = gelbach_right_willing_change_D2SD$n,legend = "% Partisan gap explained", rev = F)

# Urban 
gelbach_urban_standard_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[1], group_of_interest = "urban",
                                                     controls = c(control_variables[1:4], "right_pol"), controls_factor = c("age", "employment_agg", "country"),
                                                     indices = paste(indices_non_left_right[c(1,3:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_urban_standard_D2SD[,1] <- indices_lab[c(2,4:length(indices_lab))]
gelbach_urban_tax_transfers_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[2], group_of_interest = "urban",
                                                          controls = c(control_variables[1:4], "right_pol"), controls_factor = c("age", "employment_agg", "country"),
                                                          indices = paste(indices_non_left_right[c(1,3:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_urban_tax_transfers_D2SD[,1] <- indices_lab[c(2,4:length(indices_lab))]
gelbach_urban_investments_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[3], group_of_interest = "urban",
                                                        controls = c(control_variables[1:4], "right_pol"), controls_factor = c("age", "employment_agg", "country"),
                                                        indices = paste(indices_non_left_right[c(1,3:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_urban_investments_D2SD[,1] <- indices_lab[c(2,4:length(indices_lab))]
gelbach_urban_main_policies_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[4], group_of_interest = "urban",
                                                          controls = c(control_variables[1:4], "right_pol"), controls_factor = c("age", "employment_agg", "country"),
                                                          indices = paste(indices_non_left_right[c(1,3:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_urban_main_policies_D2SD[,1] <- indices_lab[c(2,4:length(indices_lab))]
gelbach_urban_all_policies_D2SD <- gelbach_decomposition(var_to_decompose = paste(indices_policies, "_dummies2SD", sep = "")[8], group_of_interest = "urban",
                                                         controls = c(control_variables[1:4], "right_pol"), controls_factor = c("age", "employment_agg", "country"),
                                                         indices = paste(indices_non_left_right[c(1,3:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_urban_all_policies_D2SD[,1] <- indices_lab[c(2,4:length(indices_lab))]

gelbach_urban_willing_change_D2SD <- gelbach_decomposition(var_to_decompose = "index_willing_change_dummies2SD", group_of_interest = "urban",
                                                           controls = c(control_variables[1:4], "right_pol"), controls_factor = c("age", "employment_agg", "country"),
                                                           indices = paste(indices_non_left_right[c(1,3:10,12:length(indices_non_left_right))], "_dummies2SD", sep = ""))
gelbach_urban_willing_change_D2SD[,1] <- indices_lab[c(2,4:11,13:length(indices_lab))]

barres(data = t(matrix(gelbach_urban_standard_D2SD$shareExplained/100)), labels = gelbach_urban_standard_D2SD$n,legend = "% geographical gap explained", rev = F)
barres(data = t(matrix(gelbach_urban_tax_transfers_D2SD$shareExplained/100)), labels = gelbach_urban_tax_transfers_D2SD$n,legend = "% geographical gap explained", rev = F)
barres(data = t(matrix(gelbach_urban_investments_D2SD$shareExplained/100)), labels = gelbach_urban_investments_D2SD$n,legend = "% geographical gap explained", rev = F)
barres(data = t(matrix(gelbach_urban_main_policies_D2SD$shareExplained/100)), labels = gelbach_urban_main_policies_D2SD$n,legend = "% geographical gap explained", rev = F)
barres(data = t(matrix(gelbach_urban_all_policies_D2SD$shareExplained/100)), labels = gelbach_urban_all_policies_D2SD$n,legend = "% geographical gap explained", rev = F)
barres(data = t(matrix(gelbach_urban_willing_change_D2SD$shareExplained/100)), labels = gelbach_urban_willing_change_D2SD$n,legend = "% geographical gap explained", rev = F)
