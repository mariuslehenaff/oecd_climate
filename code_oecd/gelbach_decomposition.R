install.packages('RStata')
library(RStata)

# /!\ This need to be changed if you're note using STATA SE 17 or a Mac
options("RStata.StataPath" = '/Applications/Stata/StataSE17.app/Contents/MacOS/stata-se')
options("RStata.StataVersion" = 17)

# var_to_decompose and group_of_interest: you need to input only one variable as a character
# controls and indices, can be a character vector
# NB: For the moment, input variables need to be name of variables that you can work with in Stata
# TODO: how to take care of dummies we want to create for the controls? E.g., tab agegroup_agg, gen(age_class)
# and the nuse age_class2 and age_class3 in the controls (if 3 age groups)
gelbach_decomposition <- function(var_to_decompose, group_of_interest, controls, indices, df=e, weight=T) {
  # We restrict the df to the variables we'll use, since there can be some incompatibilities
  # in using R dataframe in Stata
  df <- df %>%
    select(c(var_to_decompose, group_of_interest, controls, indices))
  
  
  # First, we prepare the options for the analysis
  option_b1x2 <- ""
  for (i in seq_along(indices)){
    option_b1x2 <- paste(option_b1x2,"g", i, " = ", indices[i], " : ", sep = "")
  }
  option_b1x2 <- substr(option_b1x2, 1, nchar(option_b1x2)-3)
  nbr_indices <- max(seq_along(indices))
  
  # We stock the different lines of codes for Stata into a vector
  # Each element corresponds to a different line of code to run in Stata
  # We will then collapse those commands altogether to run them w/ RStata
  stata_cmd <- c()
  stata_cmd[1] <- "
  set more off
  ssc install b1x2"
  stata_cmd[2] <- paste("global indices", paste('"', paste(indices, collapse = " "), '"', sep =""), sep = " ")
  stata_cmd[3] <- paste("global controls", paste('"', paste(controls, collapse = " "), '"', sep = ""), sep = " ")
  stata_cmd[4] <- paste("global option_b1x2", paste('"', option_b1x2, '"', sep = ""), sep = " ")
  stata_cmd[5] <- paste("global nbr_indices", paste(nbr_indices), sep = " ")
  stata_cmd[6] <- paste("global nbr_plus_one_indices", paste(nbr_indices+1), sep = " ")
  stata_cmd[7] <- paste("global var_to_decompose", paste(var_to_decompose), sep = " ")
  stata_cmd[8] <- paste("local var_to_decompose", paste(var_to_decompose), sep = " ")
  stata_cmd[9] <- paste("global group_of_interest", paste(group_of_interest), sep = " ")
  stata_cmd[10] <- paste("local group_of_interest", paste(group_of_interest), sep = " ")
  stata_cmd[11] <- "do gelbach_stata.do"

  stata_cmd <- paste(stata_cmd, collapse = "\n")
  # We input df, and obtain the data frame with the share explained by each indice
  final <- stata(stata_cmd, data.in = df, data.out = T)
  
  return(final)
}

# Test
e$right_pol <- e$left_right > 0
y <- gelbach_decomposition(var_to_decompose = "index_knowledge", group_of_interest = "right_pol", controls = control_variables[1:4], indices = c("index_affected"))

