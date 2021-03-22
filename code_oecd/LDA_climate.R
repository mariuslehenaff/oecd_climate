#rm(list=ls())
#gc()


list_packages <- c("quanteda","forcats", "tidytext", "haven", "dplyr", "topicmodels", "textclean", "tm" , "SnowballC", "wordcloud", "quanteda", "lmtest", "sandwich","ggplot2", "stargazer", "DBI", "readstata13", "haven", "dplyr", "xlsx", "textstem", "readxl",  "stringr", "ggrepel")
lapply(list_packages,require, character.only=TRUE)
library("tidyverse")
library("descr")
library("broom")
library(stargazer)

#### Define Data and function #### 

df <- usp3
colnames(df)

# Function to scale-up probabilities from the beta vectors. Does not account for mutually exclusive answers.
beta_transformation <- function(beta, nbr_question=107) {
  prob <- 0
  for (i in 1:nbr_question){
    prob <- prob + beta*(1-beta)^(i-1)
  }
  return(prob)
}

# Normalized PMI (pairwise)
NPMI <- function(NPMI_matrix){
  #NPMI_matrix needs to be a 2x2 matrix of joint distributions
  log(NPMI_matrix[1,2]/(NPMI_matrix[1,1]*NPMI_matrix[2,2]))/(-log(NPMI_matrix[1,2]))
}

# Cohesion score based on Draca and Schwarz (2020)
# dfm: a dfm matrix for package topicmodels
# nbr_profiles: nbr of profiles for the LDA
# nbr_topics: nbr of top topics for the NPMI average
cohesion <- function(dfm, nbr_profiles=2, nbr_topics=c(5)){
  #Top answers for each profile
  lda <- LDA(dfm, method="Gibbs", k=nbr_profiles, control=list(seed=42))
  m <- 0
  cohesion <- c()
  for(l in nbr_topics){
    terms<-terms(lda, as.integer(l))
    m <- m + 1
    
    topics_int <- as.integer(nbr_topics[m])
    #Joint probability matrix
    dfm <- as.matrix(dfm)
    joint_prob <- (t(dfm) %*% dfm)/NROW(dfm)
    
    # Calculate avg of NPMI for each profile
    NPMI_avg <- c()
    for(k in 1:nbr_profiles){
      #Matrix of joint prob for top topics of the profile
      B_prob <- joint_prob[terms[,k],terms[,k]]
      
      # Apply NPMI to all pair
      NPMI_matrix <- c()
      for(j in terms[,k]){
        for(i in terms[,k]){
          NPMI_matrix<-c(NPMI_matrix,NPMI(B_prob[c(j,i),c(j,i)]))
        }
      }
      dim(NPMI_matrix) <- c(topics_int, topics_int)
      
      # Avg of NPMIs within a profile
      NPMI_avg[k] <- (sum(NPMI_matrix*(upper.tri(NPMI_matrix)+lower.tri(NPMI_matrix)))/(topics_int*(topics_int-1)))
    }
    # Avg of NPMI_avg of all profiles
    cohesion[m] <- sum(NPMI_avg)/nbr_profiles
  }
  return(cohesion)
}



######## Classify each question into clear cut categories

df$lda_var <- ""

##### 1. Climate knowledge #####

#Q13.2
## In your opinion, is climate change real?

df$lda_var <- case_when(df$CC_real == 'Yes'  ~ paste0(df$lda_var, "yes_CC_real "),
                        df$CC_real == 'No' ~ paste0(df$lda_var, "no_CC_real "),
                        is.pnr(df$CC_real) ~ paste0(df$lda_var, "neutral_CC_real "))

#Q13.3
## What part of climate change do you think is due to human activity?
# Only asked if yes to Q13.2, thus impute no if said no before
# Transformed to binary variable as in Draca and Schwarz (2020)
# NA as neutral, to be discussed

df$lda_var <- case_when(df$CC_anthropogenic == 2|df$CC_anthropogenic==1  ~ paste0(df$lda_var, "alot_CC_anthropogenic "),
                        df$CC_anthropogenic == 0|is.pnr(df$CC_anthropogenic) ~ paste0(df$lda_var, "neutral_CC_anthropogenic "),
                        df$CC_anthropogenic == -1|df$CC_anthropogenic == -2 ~ paste0(df$lda_var, "alittle_CC_anthropogenic "))


##### 2. Climate attitudes #####

#Q14.1
## To what extent are the following groups responsible for climate change in the U.S.?

# Each


df$lda_var <- case_when(df$responsible_CC_each == 2|df$responsible_CC_each==1  ~ paste0(df$lda_var, "alot_responsible_CC_each "),
                        df$responsible_CC_each == 0|is.pnr(df$responsible_CC_each) ~ paste0(df$lda_var, "neutral_responsible_CC_each "),
                        df$responsible_CC_each == -1|df$responsible_CC_each == -2 ~ paste0(df$lda_var, "alittle_responsible_CC_each "))
# High-income

df$lda_var <- case_when(df$responsible_CC_rich == 2|df$responsible_CC_rich==1  ~ paste0(df$lda_var, "alot_responsible_CC_rich "),
                        df$responsible_CC_rich == 0|is.pnr(df$responsible_CC_rich) ~ paste0(df$lda_var, "neutral_responsible_CC_rich "),
                        df$responsible_CC_rich == -1|df$responsible_CC_rich == -2 ~ paste0(df$lda_var, "alittle_responsible_CC_rich "))
# US Fed. Gov

df$lda_var <- case_when(df$responsible_CC_govt == 2|df$responsible_CC_govt==1  ~ paste0(df$lda_var, "alot_responsible_CC_govt "),
                        df$responsible_CC_govt == 0|is.pnr(df$responsible_CC_govt) ~ paste0(df$lda_var, "neutral_responsible_CC_govt "),
                        df$responsible_CC_govt == -1|df$responsible_CC_govt == -2 ~ paste0(df$lda_var, "alittle_responsible_CC_govt "))
# Companies

df$lda_var <- case_when(df$responsible_CC_companies == 2|df$responsible_CC_companies==1  ~ paste0(df$lda_var, "alot_responsible_CC_companies "),
                        df$responsible_CC_companies == 0|is.pnr(df$responsible_CC_companies) ~ paste0(df$lda_var, "neutral_responsible_CC_companies "),
                        df$responsible_CC_companies == -1|df$responsible_CC_companies == -2 ~ paste0(df$lda_var, "alittle_responsible_CC_companies "))

# Previous Gen

df$lda_var <- case_when(df$responsible_CC_past == 2|df$responsible_CC_past==1  ~ paste0(df$lda_var, "alot_responsible_CC_past "),
                        df$responsible_CC_past == 0|is.pnr(df$responsible_CC_past) ~ paste0(df$lda_var, "neutral_responsible_CC_past "),
                        df$responsible_CC_past == -1|df$responsible_CC_past == -2 ~ paste0(df$lda_var, "alittle_responsible_CC_past "))

#Q14.3 
## To what extent do you think that it is technically feasible to stop greenhouse gas emissions 
# while maintaining satisfactory standards of living in the U.S.?

df$lda_var <- case_when(df$net_zero_feasible == 2|df$net_zero_feasible==1  ~ paste0(df$lda_var, "alot_net_zero_feasible "),
                        df$net_zero_feasible == 0|is.pnr(df$net_zero_feasible) ~ paste0(df$lda_var, "neutral_net_zero_feasible "),
                        df$net_zero_feasible == -1|df$net_zero_feasible == -2 ~ paste0(df$lda_var, "alittle_net_zero_feasible "))

#Q14.5
# How ambitious do you think public policies should be to halt climate change?

df$lda_var <- case_when(df$pro_ambitious_policies == 2|df$pro_ambitious_policies==1  ~ paste0(df$lda_var, "alot_pro_ambitious_policies "),
                        df$pro_ambitious_policies == 0|is.pnr(df$pro_ambitious_policies) ~ paste0(df$lda_var, "neutral_pro_ambitious_policies "),
                        df$pro_ambitious_policies == -1|df$pro_ambitious_policies == -2 ~ paste0(df$lda_var, "alittle_pro_ambitious_policies "))

#Q14.6
# How likely is it that human kind halt climate change by the end of the century?
# BP: added a neutral category for PNR, otherwise can take median (PNR= 4)

df$lda_var <- case_when(df$CC_will_end == 2|df$CC_will_end==1  ~ paste0(df$lda_var, "likely_CC_will_end "),
                        is.pnr(df$CC_will_end) ~ paste0(df$lda_var, "neutral_CC_will_end "),
                        df$CC_will_end == -1|df$CC_will_end == -2 ~ paste0(df$lda_var, "unlikely_CC_will_end "))

#Q14.7
# If we decide to halt climate change through ambitious policies, what would be the effects on the U.S economy and employment?

df$lda_var <- case_when(df$effect_halt_CC_economy == 2|df$effect_halt_CC_economy==1  ~ paste0(df$lda_var, "positive_effect_halt_CC_economy "),
                        df$effect_halt_CC_economy == 0|is.pnr(df$effect_halt_CC_economy) ~ paste0(df$lda_var, "neutral_effect_halt_CC_economy "),
                        df$effect_halt_CC_economy == -1|df$effect_halt_CC_economy == -2 ~ paste0(df$lda_var, "negative_effect_halt_CC_economy "))

#Q14.8
# If we decide to halt climate change through ambitious policies
# to what extent do you think it would negatively affect your lifestyle?

df$lda_var <- case_when(df$effect_halt_CC_lifestyle == 2|df$effect_halt_CC_lifestyle==1  ~ paste0(df$lda_var, "positive_effect_halt_CC_lifestyle "),
                        df$effect_halt_CC_lifestyle == 0|is.pnr(df$effect_halt_CC_lifestyle) ~ paste0(df$lda_var, "neutral_effect_halt_CC_lifestyle "),
                        df$effect_halt_CC_lifestyle == -1|df$effect_halt_CC_lifestyle == -2 ~ paste0(df$lda_var, "negative_effect_halt_CC_lifestyle "))


#Q14.9
# Here are possible habits that experts say would help reduce greenhouse gas emissions.
# To what extent would you be willing to adopt the following behaviors?

# Limit flying

df$lda_var <- case_when(df$willing_limit_flying == 2|df$willing_limit_flying==1  ~ paste0(df$lda_var, "alot_willing_limit_flying "),
                        df$willing_limit_flying == 0|is.pnr(df$willing_limit_flying) ~ paste0(df$lda_var, "neutral_willing_limit_flying "),
                        df$willing_limit_flying == -1|df$willing_limit_flying == -2 ~ paste0(df$lda_var, "alittle_willing_limit_flying "))

# Limit driving

df$lda_var <- case_when(df$willing_limit_driving == 2|df$willing_limit_driving==1  ~ paste0(df$lda_var, "alot_willing_limit_driving "),
                        df$willing_limit_driving == 0|is.pnr(df$willing_limit_driving) ~ paste0(df$lda_var, "neutral_willing_limit_driving "),
                        df$willing_limit_driving == -1|df$willing_limit_driving == -2 ~ paste0(df$lda_var, "alittle_willing_limit_driving "))

# Have a fuel-efficient or an eletric vehicle

df$lda_var <- case_when(df$willing_electric_car == 2|df$willing_electric_car==1  ~ paste0(df$lda_var, "alot_willing_electric_car "),
                        df$willing_electric_car == 0|is.pnr(df$willing_electric_car) ~ paste0(df$lda_var, "neutral_willing_electric_car "),
                        df$willing_electric_car == -1|df$willing_electric_car == -2 ~ paste0(df$lda_var, "alittle_willing_electric_car "))

# Limit beef C°

df$lda_var <- case_when(df$willing_limit_beef == 2|df$willing_limit_beef==1  ~ paste0(df$lda_var, "alot_willing_limit_beef "),
                        df$willing_limit_beef == 0|is.pnr(df$willing_limit_beef) ~ paste0(df$lda_var, "neutral_willing_limit_beef "),
                        df$willing_limit_beef == -1|df$willing_limit_beef == -2 ~ paste0(df$lda_var, "alittle_willing_limit_beef "))

# Limit heating or cooling your home

df$lda_var <- case_when(df$willing_limit_heating == 2|df$willing_limit_heating==1  ~ paste0(df$lda_var, "alot_willing_limit_heating "),
                        df$willing_limit_heating == 0|is.pnr(df$willing_limit_heating) ~ paste0(df$lda_var, "neutral_willing_limit_heating "),
                        df$willing_limit_heating == -1|df$willing_limit_heating == -2 ~ paste0(df$lda_var, "alittle_willing_limit_heating "))


#Q14.11
# How important are the factors below in order for you to adopt a sustainable lifestyle
# (i.e. limit driving, flying, and consumption, cycle more, etc.)?

# Ambitious climate policies

df$lda_var <- case_when(df$condition_ambitious_policies == 2|df$condition_ambitious_policies==1  ~ paste0(df$lda_var, "alot_condition_ambitious_policies "),
                        df$condition_ambitious_policies == 0|is.pnr(df$condition_ambitious_policies) ~ paste0(df$lda_var, "neutral_condition_ambitious_policies "),
                        df$condition_ambitious_policies == -1|df$condition_ambitious_policies == -2 ~ paste0(df$lda_var, "alittle_condition_ambitious_policies "))

# Having enough financial support

df$lda_var <- case_when(df$condition_financial_aid == 2|df$condition_financial_aid==1  ~ paste0(df$lda_var, "alot_condition_financial_aid "),
                        df$condition_financial_aid == 0|is.pnr(df$condition_financial_aid) ~ paste0(df$lda_var, "neutral_condition_financial_aid "),
                        df$condition_financial_aid == -1|df$condition_financial_aid == -2 ~ paste0(df$lda_var, "alittle_condition_financial_aid "))

# People around changing behaviors

df$lda_var <- case_when(df$condition_people_change == 2|df$condition_people_change==1  ~ paste0(df$lda_var, "alot_condition_people_change "),
                        df$condition_people_change == 0|is.pnr(df$condition_people_change) ~ paste0(df$lda_var, "neutral_condition_people_change "),
                        df$condition_people_change == -1|df$condition_people_change == -2 ~ paste0(df$lda_var, "alittle_condition_people_change "))

# Most well off changing behaviors

df$lda_var <- case_when(df$condition_rich_change == 2|df$condition_rich_change==1  ~ paste0(df$lda_var, "alot_condition_rich_change "),
                        df$condition_rich_change == 0|is.pnr(df$condition_rich_change) ~ paste0(df$lda_var, "neutral_condition_rich_change "),
                        df$condition_rich_change == -1|df$condition_rich_change == -2 ~ paste0(df$lda_var, "alittle_condition_rich_change "))


##### 3. Pref 1: emission standards #####

#Q15.2
# Do you agree or disagree with the following statements? An emission limit for cars would...

# reduce C0_2 emissions from cars

df$lda_var <- case_when(df$standard_effect_less_emission == 2|df$standard_effect_less_emission==1  ~ paste0(df$lda_var, "agree_standard_effect_less_emission "),
                        df$standard_effect_less_emission == 0|is.pnr(df$standard_effect_less_emission) ~ paste0(df$lda_var, "neutral_standard_effect_less_emission "),
                        df$standard_effect_less_emission == -1|df$standard_effect_less_emission == -2 ~ paste0(df$lda_var, "disagree_standard_effect_less_emission "))

# reduce air pollution

df$lda_var <- case_when(df$standard_effect_less_pollution == 2|df$standard_effect_less_pollution==1  ~ paste0(df$lda_var, "agree_standard_effect_less_pollution "),
                        df$standard_effect_less_pollution == 0|is.pnr(df$standard_effect_less_pollution) ~ paste0(df$lda_var, "neutral_standard_effect_less_pollution "),
                        df$standard_effect_less_pollution == -1|df$standard_effect_less_pollution == -2 ~ paste0(df$lda_var, "disagree_standard_effect_less_pollution "))

# have a large effect on the US economy and employment

df$lda_var <- case_when(df$standard_large_effect == 2|df$standard_large_effect==1  ~ paste0(df$lda_var, "agree_standard_large_effect "),
                        df$standard_large_effect == 0|is.pnr(df$standard_large_effect) ~ paste0(df$lda_var, "neutral_standard_large_effect "),
                        df$standard_large_effect == -1|df$standard_large_effect == -2 ~ paste0(df$lda_var, "disagree_standard_large_effect "))

# have a negative effect on the US economy and employment

df$lda_var <- case_when(df$standard_negative_effect == 2|df$standard_negative_effect==1  ~ paste0(df$lda_var, "agree_standard_negative_effect "),
                        df$standard_negative_effect == 0|is.pnr(df$standard_negative_effect) ~ paste0(df$lda_var, "neutral_standard_negative_effect "),
                        df$standard_negative_effect == -1|df$standard_negative_effect == -2 ~ paste0(df$lda_var, "disagree_standard_negative_effect "))

# be cost-effective to fight CC

df$lda_var <- case_when(df$standard_cost_effective == 2|df$standard_cost_effective==1  ~ paste0(df$lda_var, "agree_standard_cost_effective "),
                        df$standard_cost_effective == 0|is.pnr(df$standard_cost_effective) ~ paste0(df$lda_var, "neutral_standard_cost_effective "),
                        df$standard_cost_effective == -1|df$standard_cost_effective == -2 ~ paste0(df$lda_var, "disagree_standard_cost_effective "))

#Q15.3
# In your view, would the following groups win or lose if an emission limit for cars was implemented in the U.S.?
# Low-income

df$lda_var <- case_when(df$standard_win_lose_poor == 2|df$standard_win_lose_poor==1  ~ paste0(df$lda_var, "win_standard_win_lose_poor "),
                        df$standard_win_lose_poor == 0|is.pnr(df$standard_win_lose_poor) ~ paste0(df$lda_var, "neutral_standard_win_lose_poor "),
                        df$standard_win_lose_poor == -1|df$standard_win_lose_poor == -2 ~ paste0(df$lda_var, "lose_standard_win_lose_poor "))
# Middle class

df$lda_var <- case_when(df$standard_win_lose_middle == 2|df$standard_win_lose_middle==1  ~ paste0(df$lda_var, "win_standard_win_lose_middle "),
                        df$standard_win_lose_middle == 0|is.pnr(df$standard_win_lose_middle) ~ paste0(df$lda_var, "neutral_standard_win_lose_middle "),
                        df$standard_win_lose_middle == -1|df$standard_win_lose_middle == -2 ~ paste0(df$lda_var, "lose_standard_win_lose_middle "))
# High-income

df$lda_var <- case_when(df$standard_win_lose_rich == 2|df$standard_win_lose_rich==1  ~ paste0(df$lda_var, "win_standard_win_lose_rich "),
                        df$standard_win_lose_rich == 0|is.pnr(df$standard_win_lose_rich) ~ paste0(df$lda_var, "neutral_standard_win_lose_rich "),
                        df$standard_win_lose_rich == -1|df$standard_win_lose_rich == -2 ~ paste0(df$lda_var, "lose_standard_win_lose_rich "))
# Rural areas

df$lda_var <- case_when(df$standard_win_lose_rural == 2|df$standard_win_lose_rural==1  ~ paste0(df$lda_var, "win_standard_win_lose_rural "),
                        df$standard_win_lose_rural == 0|is.pnr(df$standard_win_lose_rural) ~ paste0(df$lda_var, "neutral_standard_win_lose_rural "),
                        df$standard_win_lose_rural == -1|df$standard_win_lose_rural == -2 ~ paste0(df$lda_var, "lose_standard_win_lose_rural "))

#Q15.4
# Do you think that financially your household would win or lose from an emission limit for cars?

df$lda_var <- case_when(df$standard_win_lose_self == 2|df$standard_win_lose_self==1  ~ paste0(df$lda_var, "win_standard_win_lose_self "),
                        df$standard_win_lose_self == 0|is.pnr(df$standard_win_lose_self) ~ paste0(df$lda_var, "neutral_standard_win_lose_self "),
                        df$standard_win_lose_self == -1|df$standard_win_lose_self == -2 ~ paste0(df$lda_var, "lose_standard_win_lose_self "))

#Q15.6 
# Do you agree or disagree with the following statement: "An emission limit for cars is fair"?

df$lda_var <- case_when(df$standard_fair == 2|df$standard_fair==1  ~ paste0(df$lda_var, "agree_standard_fair "),
                        df$standard_fair == 0|is.pnr(df$standard_fair) ~ paste0(df$lda_var, "neutral_standard_fair "),
                        df$standard_fair == -1|df$standard_fair == -2 ~ paste0(df$lda_var, "disagree_standard_fair "))
# Q15.5
# Do you support or oppose an emission limit for cars?

df$lda_var <- case_when(df$standard_support == 2|df$standard_support==1  ~ paste0(df$lda_var, "support_standard_support "),
                        df$standard_support == 0|is.pnr(df$standard_support) ~ paste0(df$lda_var, "neutral_standard_support "),
                        df$standard_support == -1|df$standard_support == -2 ~ paste0(df$lda_var, "oppose_standard_support "))

# Q15.7 
# Do you support or oppose an emission limit for cars 
# where alternatives such as public transports are made available to people?

df$lda_var <- case_when(df$standard_public_transport_support == 2|df$standard_public_transport_support==1  ~ paste0(df$lda_var, "support_standard_public_transport_support "),
                        df$standard_public_transport_support == 0|is.pnr(df$standard_public_transport_support) ~ paste0(df$lda_var, "neutral_standard_public_transport_support "),
                        df$standard_public_transport_support == -1|df$standard_public_transport_support == -2 ~ paste0(df$lda_var, "oppose_standard_public_transport_support "))


##### 4. Pref 2: green investments #####
#Q16.2
# Do you agree or disagree with the following statements? A green infrastructure program would...

# make electricity production greener

df$lda_var <- case_when(df$investments_effect_elec_greener == 2|df$investments_effect_elec_greener==1  ~ paste0(df$lda_var, "agree_investments_effect_elec_greener "),
                        df$investments_effect_elec_greener == 0|is.pnr(df$investments_effect_elec_greener) ~ paste0(df$lda_var, "neutral_investments_effect_elec_greener "),
                        df$investments_effect_elec_greener == -1|df$investments_effect_elec_greener == -2 ~ paste0(df$lda_var, "disagree_investments_effect_elec_greener "))

# increase the use of public transport

df$lda_var <- case_when(df$investments_effect_public_transport == 2|df$investments_effect_public_transport==1  ~ paste0(df$lda_var, "agree_investments_effect_public_transport "),
                        df$investments_effect_public_transport == 0|is.pnr(df$investments_effect_public_transport) ~ paste0(df$lda_var, "neutral_investments_effect_public_transport "),
                        df$investments_effect_public_transport == -1|df$investments_effect_public_transport == -2 ~ paste0(df$lda_var, "disagree_investments_effect_public_transport "))

# reduce air pollution

df$lda_var <- case_when(df$investments_effect_less_pollution == 2|df$investments_effect_less_pollution==1  ~ paste0(df$lda_var, "agree_investments_effect_less_pollution "),
                        df$investments_effect_less_pollution == 0|is.pnr(df$investments_effect_less_pollution) ~ paste0(df$lda_var, "neutral_investments_effect_less_pollution "),
                        df$investments_effect_less_pollution == -1|df$investments_effect_less_pollution == -2 ~ paste0(df$lda_var, "disagree_investments_effect_less_pollution "))

# have a large effect on the US economy and employment

df$lda_var <- case_when(df$investments_large_effect == 2|df$investments_large_effect==1  ~ paste0(df$lda_var, "agree_investments_large_effect "),
                        df$investments_large_effect == 0|is.pnr(df$investments_large_effect) ~ paste0(df$lda_var, "neutral_investments_large_effect "),
                        df$investments_large_effect == -1|df$investments_large_effect == -2 ~ paste0(df$lda_var, "disagree_investments_large_effect "))

# have a negative effect on the US economy and employment

df$lda_var <- case_when(df$investments_negative_effect == 2|df$investments_negative_effect==1  ~ paste0(df$lda_var, "agree_investments_negative_effect "),
                        df$investments_negative_effect == 0|is.pnr(df$investments_negative_effect) ~ paste0(df$lda_var, "neutral_investments_negative_effect "),
                        df$investments_negative_effect == -1|df$investments_negative_effect == -2 ~ paste0(df$lda_var, "disagree_investments_negative_effect "))

# be cost-effective to fight CC

df$lda_var <- case_when(df$investments_cost_effective == 2|df$investments_cost_effective==1  ~ paste0(df$lda_var, "agree_investments_cost_effective "),
                        df$investments_cost_effective == 0|is.pnr(df$investments_cost_effective) ~ paste0(df$lda_var, "neutral_investments_cost_effective "),
                        df$investments_cost_effective == -1|df$investments_cost_effective == -2 ~ paste0(df$lda_var, "disagree_investments_cost_effective "))

#Q16.3
# In your view, would the following groups win or lose with a green infrastructure program?
# Low-income

df$lda_var <- case_when(df$investments_win_lose_poor == 2|df$investments_win_lose_poor==1  ~ paste0(df$lda_var, "win_investments_win_lose_poor "),
                        df$investments_win_lose_poor == 0|is.pnr(df$investments_win_lose_poor) ~ paste0(df$lda_var, "neutral_investments_win_lose_poor "),
                        df$investments_win_lose_poor == -1|df$investments_win_lose_poor == -2 ~ paste0(df$lda_var, "lose_investments_win_lose_poor "))
# Middle class

df$lda_var <- case_when(df$investments_win_lose_middle == 2|df$investments_win_lose_middle==1  ~ paste0(df$lda_var, "win_investments_win_lose_middle "),
                        df$investments_win_lose_middle == 0|is.pnr(df$investments_win_lose_middle) ~ paste0(df$lda_var, "neutral_investments_win_lose_middle "),
                        df$investments_win_lose_middle == -1|df$investments_win_lose_middle == -2 ~ paste0(df$lda_var, "lose_investments_win_lose_middle "))
# High-income

df$lda_var <- case_when(df$investments_win_lose_rich == 2|df$investments_win_lose_rich==1  ~ paste0(df$lda_var, "win_investments_win_lose_rich "),
                        df$investments_win_lose_rich == 0|is.pnr(df$investments_win_lose_rich) ~ paste0(df$lda_var, "neutral_investments_win_lose_rich "),
                        df$investments_win_lose_rich == -1|df$investments_win_lose_rich == -2 ~ paste0(df$lda_var, "lose_investments_win_lose_rich "))
# Rural areas

df$lda_var <- case_when(df$investments_win_lose_rural == 2|df$investments_win_lose_rural==1  ~ paste0(df$lda_var, "win_investments_win_lose_rural "),
                        df$investments_win_lose_rural == 0|is.pnr(df$investments_win_lose_rural) ~ paste0(df$lda_var, "neutral_investments_win_lose_rural "),
                        df$investments_win_lose_rural == -1|df$investments_win_lose_rural == -2 ~ paste0(df$lda_var, "lose_investments_win_lose_rural "))

#Q16.4
# Do you think that financially your household would win or lose from a green infrastructure program?

df$lda_var <- case_when(df$investments_win_lose_self == 2|df$investments_win_lose_self==1  ~ paste0(df$lda_var, "win_investments_win_lose_self "),
                        df$investments_win_lose_self == 0|is.pnr(df$investments_win_lose_self) ~ paste0(df$lda_var, "neutral_investments_win_lose_self "),
                        df$investments_win_lose_self == -1|df$investments_win_lose_self == -2 ~ paste0(df$lda_var, "lose_investments_win_lose_self "))

#Q16.6 
# Do you agree or disagree with the following statement: "A green infrastructure program mainly financed by public debt is fair."

df$lda_var <- case_when(df$investments_fair == 2|df$investments_fair==1  ~ paste0(df$lda_var, "agree_investments_fair "),
                        df$investments_fair == 0|is.pnr(df$investments_fair) ~ paste0(df$lda_var, "neutral_investments_fair "),
                        df$investments_fair == -1|df$investments_fair == -2 ~ paste0(df$lda_var, "disagree_investments_fair "))
# Q16.5
# Do you support or oppose a green infrastructure program?

df$lda_var <- case_when(df$investments_support == 2|df$investments_support==1  ~ paste0(df$lda_var, "support_investments_support "),
                        df$investments_support == 0|is.pnr(df$investments_support) ~ paste0(df$lda_var, "neutral_investments_support "),
                        df$investments_support == -1|df$investments_support == -2 ~ paste0(df$lda_var, "oppose_investments_support "))

# Q16.7 
# Until now, we have considered that a green infrastructure program would be financed by public debt, but other sources of funding are possible.
# What sources of funding do you find appropriate for a green infrastructure program? (Multiple answers are possible)

# Debt

df$lda_var <- case_when(df$investments_funding_debt == T ~ paste0(df$lda_var, "yes_investments_funding_debt "),
                        df$investments_funding_debt == F~ paste0(df$lda_var, "no_investments_funding_debt "),
                        is.pnr(df$investments_funding_debt) ~ paste0(df$lda_var, "neutral_investments_funding_debt "))
# Sale taxes

df$lda_var <- case_when(df$investments_funding_sales_tax == T ~ paste0(df$lda_var, "yes_investments_funding_sales_tax "),
                        df$investments_funding_sales_tax == F~ paste0(df$lda_var, "no_investments_funding_sales_tax "),
                        is.pnr(df$investments_funding_sales_tax)~ paste0(df$lda_var, "neutral_investments_funding_sales_tax "))
# Taxes on wealthiest

df$lda_var <- case_when(df$investments_funding_wealth_tax == T ~ paste0(df$lda_var, "yes_investments_funding_wealth_tax "),
                        df$investments_funding_wealth_tax == F~ paste0(df$lda_var, "no_investments_funding_wealth_tax "),
                        is.pnr(df$investments_funding_wealth_tax) ~ paste0(df$lda_var, "neutral_investments_funding_wealth_tax "))
# Reduction in social spending

df$lda_var <- case_when(df$investments_funding_less_social == T ~ paste0(df$lda_var, "yes_investments_funding_less_social "),
                        df$investments_funding_less_social == F~ paste0(df$lda_var, "no_investments_funding_less_social "),
                        is.pnr(df$investments_funding_less_social) ~ paste0(df$lda_var, "neutral_investments_funding_less_social "))
# Reduction in military spending

df$lda_var <- case_when(df$investments_funding_less_military == T ~ paste0(df$lda_var, "yes_investments_funding_less_military "),
                        df$investments_funding_less_military == F~ paste0(df$lda_var, "no_investments_funding_less_military "),
                        is.pnr(df$investments_funding_less_military) ~ paste0(df$lda_var, "neutral_investments_funding_less_military "))
##### 5. Pref 3: tax and dividend #####

#Q17.2
# Do you agree or disagree with the following statements? A carbon tax with cash transfers would...

# encourage people to drive less

df$lda_var <- case_when(df$tax_transfers_effect_driving == 2|df$tax_transfers_effect_driving==1  ~ paste0(df$lda_var, "agree_tax_transfers_effect_driving "),
                        df$tax_transfers_effect_driving == 0|is.pnr(df$tax_transfers_effect_driving) ~ paste0(df$lda_var, "neutral_tax_transfers_effect_driving "),
                        df$tax_transfers_effect_driving == -1|df$tax_transfers_effect_driving == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_effect_driving "))

# encourage people and companies to insulate buildings

df$lda_var <- case_when(df$tax_transfers_effect_insulation == 2|df$tax_transfers_effect_insulation==1  ~ paste0(df$lda_var, "agree_tax_transfers_effect_insulation "),
                        df$tax_transfers_effect_insulation == 0|is.pnr(df$tax_transfers_effect_insulation) ~ paste0(df$lda_var, "neutral_tax_transfers_effect_insulation "),
                        df$tax_transfers_effect_insulation == -1|df$tax_transfers_effect_insulation == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_effect_insulation "))


# reduce the use of fossil fuels

df$lda_var <- case_when(df$tax_transfers_effect_less_emission == 2|df$tax_transfers_effect_less_emission==1  ~ paste0(df$lda_var, "agree_tax_transfers_effect_less_emission "),
                        df$tax_transfers_effect_less_emission == 0|is.pnr(df$tax_transfers_effect_less_emission) ~ paste0(df$lda_var, "neutral_tax_transfers_effect_less_emission "),
                        df$tax_transfers_effect_less_emission == -1|df$tax_transfers_effect_less_emission == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_effect_less_emission "))

# reduce air pollution

df$lda_var <- case_when(df$tax_transfers_effect_less_pollution == 2|df$tax_transfers_effect_less_pollution==1  ~ paste0(df$lda_var, "agree_tax_transfers_effect_less_pollution "),
                        df$tax_transfers_effect_less_pollution == 0|is.pnr(df$tax_transfers_effect_less_pollution) ~ paste0(df$lda_var, "neutral_tax_transfers_effect_less_pollution "),
                        df$tax_transfers_effect_less_pollution == -1|df$tax_transfers_effect_less_pollution == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_effect_less_pollution "))

# have a large effect on the US economy and employment

df$lda_var <- case_when(df$tax_transfers_large_effect == 2|df$tax_transfers_large_effect==1  ~ paste0(df$lda_var, "agree_tax_transfers_large_effect "),
                        df$tax_transfers_large_effect == 0|is.pnr(df$tax_transfers_large_effect) ~ paste0(df$lda_var, "neutral_tax_transfers_large_effect "),
                        df$tax_transfers_large_effect == -1|df$tax_transfers_large_effect == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_large_effect "))

# have a negative effect on the US economy and employment

df$lda_var <- case_when(df$tax_transfers_negative_effect == 2|df$tax_transfers_negative_effect==1  ~ paste0(df$lda_var, "agree_tax_transfers_negative_effect "),
                        df$tax_transfers_negative_effect == 0|is.pnr(df$tax_transfers_negative_effect) ~ paste0(df$lda_var, "neutral_tax_transfers_negative_effect "),
                        df$tax_transfers_negative_effect == -1|df$tax_transfers_negative_effect == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_negative_effect "))

# be cost-effective to fight CC

df$lda_var <- case_when(df$tax_transfers_cost_effective == 2|df$tax_transfers_cost_effective==1  ~ paste0(df$lda_var, "agree_tax_transfers_cost_effective "),
                        df$tax_transfers_cost_effective == 0|is.pnr(df$tax_transfers_cost_effective) ~ paste0(df$lda_var, "neutral_tax_transfers_cost_effective "),
                        df$tax_transfers_cost_effective == -1|df$tax_transfers_cost_effective == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_cost_effective "))

#Q17.3
# In your view, would the following groups win or lose under a carbon tax with cash transfers?
# Low-income

df$lda_var <- case_when(df$tax_transfers_win_lose_poor == 2|df$tax_transfers_win_lose_poor==1  ~ paste0(df$lda_var, "win_tax_transfers_win_lose_poor "),
                        df$tax_transfers_win_lose_poor == 0|is.pnr(df$tax_transfers_win_lose_poor) ~ paste0(df$lda_var, "neutral_tax_transfers_win_lose_poor "),
                        df$tax_transfers_win_lose_poor == -1|df$tax_transfers_win_lose_poor == -2 ~ paste0(df$lda_var, "lose_tax_transfers_win_lose_poor "))
# Middle class

df$lda_var <- case_when(df$tax_transfers_win_lose_middle == 2|df$tax_transfers_win_lose_middle==1  ~ paste0(df$lda_var, "win_tax_transfers_win_lose_middle "),
                        df$tax_transfers_win_lose_middle == 0|is.pnr(df$tax_transfers_win_lose_middle) ~ paste0(df$lda_var, "neutral_tax_transfers_win_lose_middle "),
                        df$tax_transfers_win_lose_middle == -1|df$tax_transfers_win_lose_middle == -2 ~ paste0(df$lda_var, "lose_tax_transfers_win_lose_middle "))
# High-income

df$lda_var <- case_when(df$tax_transfers_win_lose_rich == 2|df$tax_transfers_win_lose_rich==1  ~ paste0(df$lda_var, "win_tax_transfers_win_lose_rich "),
                        df$tax_transfers_win_lose_rich == 0|is.pnr(df$tax_transfers_win_lose_rich) ~ paste0(df$lda_var, "neutral_tax_transfers_win_lose_rich "),
                        df$tax_transfers_win_lose_rich == -1|df$tax_transfers_win_lose_rich == -2 ~ paste0(df$lda_var, "lose_tax_transfers_win_lose_rich "))
# Rural areas

df$lda_var <- case_when(df$tax_transfers_win_lose_rural == 2|df$tax_transfers_win_lose_rural==1  ~ paste0(df$lda_var, "win_tax_transfers_win_lose_rural "),
                        df$tax_transfers_win_lose_rural == 0|is.pnr(df$tax_transfers_win_lose_rural) ~ paste0(df$lda_var, "neutral_tax_transfers_win_lose_rural "),
                        df$tax_transfers_win_lose_rural == -1|df$tax_transfers_win_lose_rural == -2 ~ paste0(df$lda_var, "lose_tax_transfers_win_lose_rural "))

#Q17.4
# Do you think that financially your household would win or lose under a carbon tax with cash transfers?

df$lda_var <- case_when(df$tax_transfers_win_lose_self == 2|df$tax_transfers_win_lose_self==1  ~ paste0(df$lda_var, "win_tax_transfers_win_lose_self "),
                        df$tax_transfers_win_lose_self == 0|is.pnr(df$tax_transfers_win_lose_self) ~ paste0(df$lda_var, "neutral_tax_transfers_win_lose_self "),
                        df$tax_transfers_win_lose_self == -1|df$tax_transfers_win_lose_self == -2 ~ paste0(df$lda_var, "lose_tax_transfers_win_lose_self "))

#Q17.6 
# Do you agree or disagree with the following statement: "A carbon tax with cash transfers is fair."

df$lda_var <- case_when(df$tax_transfers_fair == 2|df$tax_transfers_fair==1  ~ paste0(df$lda_var, "agree_tax_transfers_fair "),
                        df$tax_transfers_fair == 0|is.pnr(df$tax_transfers_fair) ~ paste0(df$lda_var, "neutral_tax_transfers_fair "),
                        df$tax_transfers_fair == -1|df$tax_transfers_fair == -2 ~ paste0(df$lda_var, "disagree_tax_transfers_fair "))
# Q17.5
# Do you support or oppose a carbon tax with cash transfers?

df$lda_var <- case_when(df$tax_transfers_support == 2|df$tax_transfers_support==1  ~ paste0(df$lda_var, "support_tax_transfers_support "),
                        df$tax_transfers_support == 0|is.pnr(df$tax_transfers_support) ~ paste0(df$lda_var, "neutral_tax_transfers_support "),
                        df$tax_transfers_support == -1|df$tax_transfers_support == -2 ~ paste0(df$lda_var, "oppose_tax_transfers_support "))


##### 6. Pref for Climate Policies #####

# Q18.3
# Do you support or oppose the following climate policies?

# Tax on flying

df$lda_var <- case_when(df$policy_tax_flying == 2|df$policy_tax_flying==1  ~ paste0(df$lda_var, "support_policy_tax_flying "),
                        df$policy_tax_flying == 0|is.pnr(df$policy_tax_flying) ~ paste0(df$lda_var, "neutral_policy_tax_flying "),
                        df$policy_tax_flying == -1|df$policy_tax_flying == -2 ~ paste0(df$lda_var, "oppose_policy_tax_flying "))
# Tax on fossil fuels

df$lda_var <- case_when(df$policy_tax_fuels == 2|df$policy_tax_fuels==1  ~ paste0(df$lda_var, "support_policy_tax_fuels "),
                        df$policy_tax_fuels == 0|is.pnr(df$policy_tax_fuels) ~ paste0(df$lda_var, "neutral_policy_tax_fuels "),
                        df$policy_tax_fuels == -1|df$policy_tax_fuels == -2 ~ paste0(df$lda_var, "oppose_policy_tax_fuels "))
# Mandatory insulation

df$lda_var <- case_when(df$policy_insulation == 2|df$policy_insulation==1  ~ paste0(df$lda_var, "support_policy_insulation "),
                        df$policy_insulation == 0|is.pnr(df$policy_insulation) ~ paste0(df$lda_var, "neutral_policy_insulation "),
                        df$policy_insulation == -1|df$policy_insulation == -2 ~ paste0(df$lda_var, "oppose_policy_insulation "))
# Ban of polluting vehicles in city-centers

df$lda_var <- case_when(df$policy_ban_city_centers == 2|df$policy_ban_city_centers==1  ~ paste0(df$lda_var, "support_policy_ban_city_centers "),
                        df$policy_ban_city_centers == 0|is.pnr(df$policy_ban_city_centers) ~ paste0(df$lda_var, "neutral_policy_ban_city_centers "),
                        df$policy_ban_city_centers == -1|df$policy_ban_city_centers == -2 ~ paste0(df$lda_var, "oppose_policy_ban_city_centers "))
# Subsidies for low-carbon technologies

df$lda_var <- case_when(df$policy_subsidies == 2|df$policy_subsidies==1  ~ paste0(df$lda_var, "support_policy_subsidies "),
                        df$policy_subsidies == 0|is.pnr(df$policy_subsidies) ~ paste0(df$lda_var, "neutral_policy_subsidies "),
                        df$policy_subsidies == -1|df$policy_subsidies == -2 ~ paste0(df$lda_var, "oppose_policy_subsidies "))
# Contribution to a global climate fund

df$lda_var <- case_when(df$policy_climate_fund == 2|df$policy_climate_fund==1  ~ paste0(df$lda_var, "support_policy_climate_fund "),
                        df$policy_climate_fund == 0|is.pnr(df$policy_climate_fund) ~ paste0(df$lda_var, "neutral_policy_climate_fund "),
                        df$policy_climate_fund == -1|df$policy_climate_fund == -2 ~ paste0(df$lda_var, "oppose_policy_climate_fund "))
# Q18.4
# Governments can use the revenues from carbon taxes in different ways. Would you support or oppose introducing 
# a carbon tax that would raise gasoline prices by 40 cents per gallon, if the government used this revenue to finance...

# Cash transfers for HH with no alternatives

df$lda_var <- case_when(df$tax_transfer_constrained_hh == 2|df$tax_transfer_constrained_hh==1  ~ paste0(df$lda_var, "support_tax_transfer_constrained_hh "),
                        df$tax_transfer_constrained_hh == 0|is.pnr(df$tax_transfer_constrained_hh) ~ paste0(df$lda_var, "neutral_tax_transfer_constrained_hh "),
                        df$tax_transfer_constrained_hh == -1|df$tax_transfer_constrained_hh == -2 ~ paste0(df$lda_var, "oppose_tax_transfer_constrained_hh "))
# Cash transfers to poor

df$lda_var <- case_when(df$tax_transfer_poor == 2|df$tax_transfer_poor==1  ~ paste0(df$lda_var, "support_tax_transfer_poor "),
                        df$tax_transfer_poor == 0|is.pnr(df$tax_transfer_poor) ~ paste0(df$lda_var, "neutral_tax_transfer_poor "),
                        df$tax_transfer_poor == -1|df$tax_transfer_poor == -2 ~ paste0(df$lda_var, "oppose_tax_transfer_poor "))
# Equal cash transfers

df$lda_var <- case_when(df$tax_transfer_all == 2|df$tax_transfer_all==1  ~ paste0(df$lda_var, "support_tax_transfer_all "),
                        df$tax_transfer_all == 0|is.pnr(df$tax_transfer_all) ~ paste0(df$lda_var, "neutral_tax_transfer_all "),
                        df$tax_transfer_all == -1|df$tax_transfer_all == -2 ~ paste0(df$lda_var, "oppose_tax_transfer_all "))
# Reduction in PIT

df$lda_var <- case_when(df$tax_reduction_personal_tax == 2|df$tax_reduction_personal_tax==1  ~ paste0(df$lda_var, "support_tax_reduction_personal_tax "),
                        df$tax_reduction_personal_tax == 0|is.pnr(df$tax_reduction_personal_tax) ~ paste0(df$lda_var, "neutral_tax_reduction_personal_tax "),
                        df$tax_reduction_personal_tax == -1|df$tax_reduction_personal_tax == -2 ~ paste0(df$lda_var, "oppose_tax_reduction_personal_tax "))
# Reduction in CIP

df$lda_var <- case_when(df$tax_reduction_corporate_tax == 2|df$tax_reduction_corporate_tax==1  ~ paste0(df$lda_var, "support_tax_reduction_corporate_tax "),
                        df$tax_reduction_corporate_tax == 0|is.pnr(df$tax_reduction_corporate_tax) ~ paste0(df$lda_var, "neutral_tax_reduction_corporate_tax "),
                        df$tax_reduction_corporate_tax == -1|df$tax_reduction_corporate_tax == -2 ~ paste0(df$lda_var, "oppose_tax_reduction_corporate_tax "))
# Tax rebates for affected firms

df$lda_var <- case_when(df$tax_rebates_affected_firms == 2|df$tax_rebates_affected_firms==1  ~ paste0(df$lda_var, "support_tax_rebates_affected_firms "),
                        df$tax_rebates_affected_firms == 0|is.pnr(df$tax_rebates_affected_firms) ~ paste0(df$lda_var, "neutral_tax_rebates_affected_firms "),
                        df$tax_rebates_affected_firms == -1|df$tax_rebates_affected_firms == -2 ~ paste0(df$lda_var, "oppose_tax_rebates_affected_firms "))
# Funding environmental infrastructure

df$lda_var <- case_when(df$tax_investments == 2|df$tax_investments==1  ~ paste0(df$lda_var, "support_tax_investments "),
                        df$tax_investments == 0|is.pnr(df$tax_investments) ~ paste0(df$lda_var, "neutral_tax_investments "),
                        df$tax_investments == -1|df$tax_investments == -2 ~ paste0(df$lda_var, "oppose_tax_investments "))
# Subsidize low-carbon technologies

df$lda_var <- case_when(df$tax_subsidies == 2|df$tax_subsidies==1  ~ paste0(df$lda_var, "support_tax_subsidies "),
                        df$tax_subsidies == 0|is.pnr(df$tax_subsidies) ~ paste0(df$lda_var, "neutral_tax_subsidies "),
                        df$tax_subsidies == -1|df$tax_subsidies == -2 ~ paste0(df$lda_var, "oppose_tax_subsidies "))
# Reduction in public deficit

df$lda_var <- case_when(df$tax_reduction_deficit == 2|df$tax_reduction_deficit==1  ~ paste0(df$lda_var, "support_tax_reduction_deficit "),
                        df$tax_reduction_deficit == 0|is.pnr(df$tax_reduction_deficit) ~ paste0(df$lda_var, "neutral_tax_reduction_deficit "),
                        df$tax_reduction_deficit == -1|df$tax_reduction_deficit == -2 ~ paste0(df$lda_var, "oppose_tax_reduction_deficit "))
##### 7. International burden-sharing #####

# Q20.2
# At which level(s) do you think public policies to tackle climate change need to be put in place?

# Global

df$lda_var <- case_when(df$scale_global == T ~ paste0(df$lda_var, "yes_scale_global "),
                        df$scale_global == F~ paste0(df$lda_var, "no_scale_global "))
# Federal

df$lda_var <- case_when(df$scale_federal == T ~ paste0(df$lda_var, "yes_scale_federal "),
                        df$scale_federal == F~ paste0(df$lda_var, "no_scale_federal "))
# State

df$lda_var <- case_when(df$scale_state == T ~ paste0(df$lda_var, "yes_scale_state "),
                        df$scale_state == F~ paste0(df$lda_var, "no_scale_state "))
# Local

df$lda_var <- case_when(df$scale_local == T ~ paste0(df$lda_var, "yes_scale_local "),
                        df$scale_local == F~ paste0(df$lda_var, "no_scale_local "))
# Q20.3
# Do you agree or disagree with the following statement: "The U.S. should take measures to fight climate change."

df$lda_var <- case_when(df$should_fight_CC == 2|df$should_fight_CC==1  ~ paste0(df$lda_var, "agree_should_fight_CC "),
                        df$should_fight_CC == 0|is.pnr(df$should_fight_CC) ~ paste0(df$lda_var, "neutral_should_fight_CC "),
                        df$should_fight_CC == -1|df$should_fight_CC == -2 ~ paste0(df$lda_var, "disagree_should_fight_CC "))
# Q20.4
# How should U.S. climate policies depend on what other countries do?

# Other countries do more

df$lda_var <- case_when(df$if_other_do_more == 2|df$if_other_do_more==1  ~ paste0(df$lda_var, "more_if_other_do_more "),
                        df$if_other_do_more == 0|is.pnr(df$if_other_do_more) ~ paste0(df$lda_var, "neutral_if_other_do_more "),
                        df$if_other_do_more == -1|df$if_other_do_more == -2 ~ paste0(df$lda_var, "less_if_other_do_more "))
# Other countries do less

df$lda_var <- case_when(df$if_other_do_less == 2|df$if_other_do_less==1  ~ paste0(df$lda_var, "more_if_other_do_less "),
                        df$if_other_do_less == 0|is.pnr(df$if_other_do_less) ~ paste0(df$lda_var, "neutral_if_other_do_less "),
                        df$if_other_do_less == -1|df$if_other_do_less == -2 ~ paste0(df$lda_var, "less_if_other_do_less "))
# Q20.5
# To achieve a given reduction of greenhouse gas emissions globally, costly investments are needed.
# Ideally, how should countries bear the costs of fighting climate change?

# Proportion to income

df$lda_var <- case_when(df$burden_sharing_income == 2|df$burden_sharing_income==1  ~ paste0(df$lda_var, "agree_burden_sharing_income "),
                        df$burden_sharing_income == 0|is.pnr(df$burden_sharing_income) ~ paste0(df$lda_var, "neutral_burden_sharing_income "),
                        df$burden_sharing_income == -1|df$burden_sharing_income == -2 ~ paste0(df$lda_var, "disagree_burden_sharing_income "))
# Proportion to current emissions

df$lda_var <- case_when(df$burden_sharing_emissions == 2|df$burden_sharing_emissions==1  ~ paste0(df$lda_var, "agree_burden_sharing_emissions "),
                        df$burden_sharing_emissions == 0|is.pnr(df$burden_sharing_emissions) ~ paste0(df$lda_var, "neutral_burden_sharing_emissions "),
                        df$burden_sharing_emissions == -1|df$burden_sharing_emissions == -2 ~ paste0(df$lda_var, "disagree_burden_sharing_emissions "))
# Proportion to past emissions

df$lda_var <- case_when(df$burden_sharing_cumulative == 2|df$burden_sharing_cumulative==1  ~ paste0(df$lda_var, "agree_burden_sharing_cumulative "),
                        df$burden_sharing_cumulative == 0|is.pnr(df$burden_sharing_cumulative) ~ paste0(df$lda_var, "neutral_burden_sharing_cumulative "),
                        df$burden_sharing_cumulative == -1|df$burden_sharing_cumulative == -2 ~ paste0(df$lda_var, "disagree_burden_sharing_cumulative "))
# Richest pay all

df$lda_var <- case_when(df$burden_sharing_rich_pay == 2|df$burden_sharing_rich_pay==1  ~ paste0(df$lda_var, "agree_burden_sharing_rich_pay "),
                        df$burden_sharing_rich_pay == 0|is.pnr(df$burden_sharing_rich_pay) ~ paste0(df$lda_var, "neutral_burden_sharing_rich_pay "),
                        df$burden_sharing_rich_pay == -1|df$burden_sharing_rich_pay == -2 ~ paste0(df$lda_var, "disagree_burden_sharing_rich_pay "))
# Richest pay even more

df$lda_var <- case_when(df$burden_sharing_poor_receive == 2|df$burden_sharing_poor_receive==1  ~ paste0(df$lda_var, "agree_burden_sharing_poor_receive "),
                        df$burden_sharing_poor_receive == 0|is.pnr(df$burden_sharing_poor_receive) ~ paste0(df$lda_var, "neutral_burden_sharing_poor_receive "),
                        df$burden_sharing_poor_receive == -1|df$burden_sharing_poor_receive == -2 ~ paste0(df$lda_var, "disagree_burden_sharing_poor_receive "))
# Q20.6
# Do you support or oppose establishing a global democratic assembly whose role would be to draft international treaties against climate change? 
# Each adult across the world would have one vote to elect members of the assembly.

df$lda_var <- case_when(df$global_assembly_support == 2|df$global_assembly_support==1  ~ paste0(df$lda_var, "support_global_assembly_support "),
                        df$global_assembly_support == 0|is.pnr(df$global_assembly_support) ~ paste0(df$lda_var, "neutral_global_assembly_support "),
                        df$global_assembly_support == -1|df$global_assembly_support == -2 ~ paste0(df$lda_var, "oppose_global_assembly_support "))
# Q20.7
# Imagine the following policy: a global tax on greenhouse gas emissions funding a global basic income.
# Such a policy would progressively raise the price of fossil fuels (for example, the price of gasoline would increase by 40 cents per gallon in the first years). Higher prices would encourage people and companies to use less fossil fuels, reducing greenhouse gas emissions. Revenues from the tax would be used to finance a basic income of $30/month to each human adult, thereby lifting the 700 million people who earn less than $2/day out of extreme poverty. Most Americans would lose out financially as they would face price increases in excess of $30/month.
#Do you support or oppose such a policy?

df$lda_var <- case_when(df$global_tax_support == 2|df$global_tax_support==1  ~ paste0(df$lda_var, "support_global_tax_support "),
                        df$global_tax_support == 0|is.pnr(df$global_tax_support) ~ paste0(df$lda_var, "neutral_global_tax_support "),
                        df$global_tax_support == -1|df$global_tax_support == -2 ~ paste0(df$lda_var, "oppose_global_tax_support "))
# Q20.8
# Do you support or oppose a tax on all millionaires around the world to finance low-income countries which comply with international standards regarding climate action? 
# This would finance infrastructure and public services such as access to drinking water, healthcare, and education.

df$lda_var <- case_when(df$tax_1p_support == 2|df$tax_1p_support==1  ~ paste0(df$lda_var, "support_tax_1p_support "),
                        df$tax_1p_support == 0|is.pnr(df$tax_1p_support) ~ paste0(df$lda_var, "neutral_tax_1p_support "),
                        df$tax_1p_support == -1|df$tax_1p_support == -2 ~ paste0(df$lda_var, "oppose_tax_1p_support "))

##### 8. Bans vs Incentives #####
# Q246
# To reduce fuel consumption for heating and cooling, the U.S. federal government could subsidize half of the costs to renovate the insulation of residential buildings to meet a certain energy efficiency standard.
# Do you support or oppose such a policy?

df$lda_var <- case_when(df$insulation_subsidies_support == 2|df$insulation_subsidies_support==1  ~ paste0(df$lda_var, "support_insulation_subsidies_support "),
                        df$insulation_subsidies_support == 0|is.pnr(df$insulation_subsidies_support) ~ paste0(df$lda_var, "neutral_insulation_subsidies_support "),
                        df$insulation_subsidies_support == -1|df$insulation_subsidies_support == -2 ~ paste0(df$lda_var, "oppose_insulation_subsidies_support "))
# Q247
# Imagine that the U.S. federal government makes it mandatory for all residential buildings to have insulation that meets a certain energy efficiency standard before 2040. The government would subsidize half of the insulation costs to help households with the transition.
# Do you support or oppose such a policy?

df$lda_var <- case_when(df$insulation_mandatory_support == 2|df$insulation_mandatory_support==1  ~ paste0(df$lda_var, "support_insulation_mandatory_support "),
                        df$insulation_mandatory_support == 0|is.pnr(df$insulation_mandatory_support) ~ paste0(df$lda_var, "neutral_insulation_mandatory_support "),
                        df$insulation_mandatory_support == -1|df$insulation_mandatory_support == -2 ~ paste0(df$lda_var, "oppose_insulation_mandatory_support "))
# Q21.4
# Imagine that, in order to fight climate change, the U.S. federal government decides to limit the consumption of cattle products like beef and dairy.
# Do you support or oppose the following options?

# High tax on cattle products

df$lda_var <- case_when(df$beef_tax_support == 2|df$beef_tax_support==1  ~ paste0(df$lda_var, "support_beef_tax_support "),
                        df$beef_tax_support == 0|is.pnr(df$beef_tax_support) ~ paste0(df$lda_var, "neutral_beef_tax_support "),
                        df$beef_tax_support == -1|df$beef_tax_support == -2 ~ paste0(df$lda_var, "oppose_beef_tax_support "))
# Subsidies on organic and local vegies

df$lda_var <- case_when(df$beef_subsidies_vegetables_support == 2|df$beef_subsidies_vegetables_support==1  ~ paste0(df$lda_var, "support_beef_subsidies_vegetables_support "),
                        df$beef_subsidies_vegetables_support == 0|is.pnr(df$beef_subsidies_vegetables_support) ~ paste0(df$lda_var, "neutral_beef_subsidies_vegetables_support "),
                        df$beef_subsidies_vegetables_support == -1|df$beef_subsidies_vegetables_support == -2 ~ paste0(df$lda_var, "oppose_beef_subsidies_vegetables_support "))
# Removal of subsidies for cattle farming

df$lda_var <- case_when(df$beef_subsidies_removal_support == 2|df$beef_subsidies_removal_support==1  ~ paste0(df$lda_var, "support_beef_subsidies_removal_support "),
                        df$beef_subsidies_removal_support == 0|is.pnr(df$beef_subsidies_removal_support) ~ paste0(df$lda_var, "neutral_beef_subsidies_removal_support "),
                        df$beef_subsidies_removal_support == -1|df$beef_subsidies_removal_support == -2 ~ paste0(df$lda_var, "oppose_beef_subsidies_removal_support "))
# Ban of intensive cattle farming

df$lda_var <- case_when(df$beef_ban_intensive_support == 2|df$beef_ban_intensive_support==1  ~ paste0(df$lda_var, "support_beef_ban_intensive_support "),
                        df$beef_ban_intensive_support == 0|is.pnr(df$beef_ban_intensive_support) ~ paste0(df$lda_var, "neutral_beef_ban_intensive_support "),
                        df$beef_ban_intensive_support == -1|df$beef_ban_intensive_support == -2 ~ paste0(df$lda_var, "oppose_beef_ban_intensive_support "))
##### 9. Trust, perceptions of institutions, inequality, and the future #####

# Q22.1
# Do you agree or disagree with the following statement: "Most people can be trusted."

df$lda_var <- case_when(df$can_trust_people == 2|df$can_trust_people==1  ~ paste0(df$lda_var, "agree_can_trust_people "),
                        df$can_trust_people == 0|is.pnr(df$can_trust_people) ~ paste0(df$lda_var, "neutral_can_trust_people "),
                        df$can_trust_people == -1|df$can_trust_people == -2 ~ paste0(df$lda_var, "disagree_can_trust_people "))
# Q22.2
# Do you agree or disagree with the following statement: "Over the last decade the U.S. federal government could generally be trusted to do what is right."

df$lda_var <- case_when(df$can_trust_govt == 2|df$can_trust_govt==1  ~ paste0(df$lda_var, "agree_can_trust_govt "),
                        df$can_trust_govt == 0|is.pnr(df$can_trust_govt) ~ paste0(df$lda_var, "neutral_can_trust_govt "),
                        df$can_trust_govt == -1|df$can_trust_govt == -2 ~ paste0(df$lda_var, "disagree_can_trust_govt "))
# Q22.3
# Some people think the government is trying to do too many things that should be left to individuals and businesses. Others think that government should do more to solve our country's problems.
# Which come closer to your own view? 

df$lda_var <- case_when(df$view_govt==1  ~ paste0(df$lda_var, "more_view_govt "),
                        df$view_govt == 0|is.pnr(df$view_govt) ~ paste0(df$lda_var, "neutral_view_govt "),
                        df$view_govt == -1 ~ paste0(df$lda_var, "too_much_view_govt "))
# Q22.4
# How big of an issue do you think income inequality is in the U.S.?

df$lda_var <- case_when(df$problem_inequality == 2|df$problem_inequality==1  ~ paste0(df$lda_var, "serious_problem_inequality "),
                        df$problem_inequality == 0|is.pnr(df$problem_inequality) ~ paste0(df$lda_var, "neutral_problem_inequality "),
                        df$problem_inequality == -1|df$problem_inequality == -2 ~ paste0(df$lda_var, "small_problem_inequality "))
# Q22.5
# Do you think that overall people in the world will be richer or poorer in 100 years from now?

df$lda_var <- case_when(df$future_richness == 2|df$future_richness==1  ~ paste0(df$lda_var, "richer_future_richness "),
                        df$future_richness == 0|is.pnr(df$future_richness) ~ paste0(df$lda_var, "neutral_future_richness "),
                        df$future_richness == -1|df$future_richness == -2 ~ paste0(df$lda_var, "poorer_future_richness "))
###########################################################################
#################################################################################
##################################################################################
################################################################################
# 107 issues overall from 43 questions in the survey

df$WC_lda_var <- lengths(strsplit(df$lda_var, "\\W+"))
#hist(df$WC_lda_var)
#View(subset(df,WC_lda_var<90 ))

# Segment the 'sentence' into 'words/tokens', then transform it to DFM
toks<- quanteda::tokens(df$lda_var)
dfm <- dfm(toks)

# Convert for topicmodels package
dfm <- quanteda::convert(dfm, to ="topicmodels")

##### Test for optimal nbr of profiles #####
# Take several minutes to execute, decomment if needed
#topics <- c(5,10,15,20)
#profiles <-c(2:10)
# main function
#cohesion_matrix<-sapply(profiles,cohesion,dfm=dfm, nbr_topics=topics)
#colnames(cohesion_matrix) <- paste("Profiles", profiles, sep="_")
#rownames(cohesion_matrix) <- paste("Topics", topics, sep="_")
#cohesion_avg <- colSums(cohesion_matrix)/length(topics)
#
#cohesion_df <- data.frame(profiles, cohesion_avg)
#ggplot(cohesion_df, aes(x=profiles, y=cohesion_avg)) + geom_line() + xlab("Number of Profiles") + ylab("Cohesion Score")


###############################################################################
##############" FIT A LDA #####################################################
###############################################################################
### https://www.tidytextmining.com/topicmodeling.html
#https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
#https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf

#### 2 topics ####
lda_out2 <- LDA(dfm, method="Gibbs", k=2, control=list(seed=42))

terms(lda_out2, 20)

# Per-topic-per-word probabilities
lda_topics2 <- lda_out2 %>%
  tidy(matrix = "beta") %>%
  arrange(topic, desc(beta))

# Transform beta to have natural interpretation: overall probability that a feature.answer appears for a chosen profile
lda_topics2[,4] <- beta_transformation(lda_topics2[,3]) %>% rename(prob=beta)

# Per-document-per-topic probabilities (% of each profile for each respondent)
lda_gamma2 <- lda_out2 %>%
  tidy(matrix = "gamma")

# Sort matrix in terms of id of respondent and % of each profile
lda_gamma2$document <- substr(lda_gamma2$document, 5, 10)
lda_gamma2$document <- as.numeric(lda_gamma2$document)
lda_gamma2 <- lda_gamma2 %>%
  arrange(document)

# Profile assigned for each respondent
topic_assigned2 <- select(df,lda_var)
topic_assigned2$topic <- topics(lda_out2)



df$topic2 <- topics(lda_out2)

# Cross with variables
crosstab(df$topic2,df$vote_2020, prop.c =T) 
crosstab(df$topic2,df$CC_anthropogenic, prop.c =T) 

# Dummies for each profile
df$topic2_1 <- ifelse(df$topic2==1, 1,0)
df$topic2_2 <- ifelse(df$topic2==2, 1,0)

control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_quota", "vote_dum")
end_formula <- paste(control_variables, collapse = ' + ')
cov_lab <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 25-34", "age: 35-49", "age: 50-64", "age: 65+", "age: below 18", "vote: Biden", "vote: Trump")

reg_topic2_1 <- lm(as.formula(paste("topic2_1 ~", end_formula12)), data = df)
summary(reg_topic2_1)

reg_topic2_2 <- lm(as.formula(paste("topic2_2 ~", end_formula12)), data = df)
summary(reg_topic2_2)

desc_table(dep_vars = c("topic2_1", "topic2_2"), filename = "../LDA/topic2",
           dep.var.labels = c("Profile 1", "Profile 2"),
           dep.var.caption = c(""), data = df, indep_vars = control_variables, indep_labels = cov_lab)

#stargazer(reg_topic2_2,reg_topic2_1,type="html", out=file.path(getwd(),'/regressions_exploration.html') )


#rm(reg_topic2_1,reg_topic2_2, topic_assigned2, toks, dfm)


##################
#### 3 topics ####
##################

lda_out3 <- LDA(dfm, method="Gibbs", k=3, control=list(seed=42))

terms(lda_out3, 6)

# Per-topic-per-word probabilities
lda_topics3 <- lda_out3 %>%
  tidy(matrix = "beta") %>%
  arrange(topic, desc(beta))

# Transform beta to have natural interpretation: overall probability that a feature.answer appears for a chosen profile
lda_topics3[,4] <- beta_transformation(lda_topics3[,3]) %>% rename(prob=beta)


# Per-document-per-topic probabilities (% of each profile for each respondent)
lda_gamma3 <- lda_out3 %>%
  tidy(matrix = "gamma")

# Sort matrix in terms of id of respondent and % of each profile
lda_gamma3$document <- substr(lda_gamma3$document, 5, 10)
lda_gamma3$document <- as.numeric(lda_gamma3$document)
lda_gamma3 <- lda_gamma3 %>%
  arrange(document)

# Profile assigned for each respondent
topic_assigned3 <- select(df,lda_var)
topic_assigned3$topic <- topics(lda_out3)

df$topic3 <- topics(lda_out3)

# Cross with variables
crosstab(df$topic3,df$vote_2020, prop.c =T) 
crosstab(df$topic3,df$CC_anthropogenic, prop.c =T) 

# Dummies for each profile
df$topic3_1 <- ifelse(df$topic3==1, 1,0)
df$topic3_2 <- ifelse(df$topic3==2, 1,0)
df$topic3_3 <- ifelse(df$topic3==3, 1,0)

reg_topic3_1 <- lm(as.formula(paste("topic3_1 ~", end_formula12)), data = df)
summary(reg_topic3_1)

reg_topic3_2 <- lm(as.formula(paste("topic3_2 ~", end_formula12)), data = df)
summary(reg_topic3_2)

reg_topic3_3 <- lm(as.formula(paste("topic3_3 ~", end_formula12)), data = df)
summary(reg_topic3_3)

desc_table(dep_vars = c("topic3_1", "topic3_2", "topic3_3"), filename = "../LDA/topic3",
           dep.var.labels = c("Profile 1", "Profile 2", "Profile 3"),
           dep.var.caption = c(""), data = df, indep_vars = control_variables, indep_labels = cov_lab)


####################
#### 4   topics ####
###################
lda_out4 <- LDA(dfm, method="Gibbs", k=4, control=list(seed=42))

terms(lda_out4, 6)

# Per-topic-per-word probabilities
lda_topics4 <- lda_out4 %>%
  tidy(matrix = "beta") %>%
  arrange(topic, desc(beta))

# Transform beta to have natural interpretation: overall probability that a feature.answer appears for a chosen profile
lda_topics4[,4] <- beta_transformation(lda_topics4[,3]) %>% rename(prob=beta)

# Per-document-per-topic probabilities (% of each profile for each respondent)
lda_gamma4 <- lda_out4 %>%
  tidy(matrix = "gamma")

# Sort matrix in terms of id of respondent and % of each profile
lda_gamma4$document <- substr(lda_gamma4$document, 5, 10)
lda_gamma4$document <- as.numeric(lda_gamma4$document)
lda_gamma4 <- lda_gamma4 %>%
  arrange(document)

# Profile assigned for each respondent
topic_assigned4 <- select(df,lda_var)
topic_assigned4$topic <- topics(lda_out4)

df$topic4 <- topics(lda_out4)

# Cross with variables
crosstab(df$topic4,df$vote_2020, prop.c =T) 
crosstab(df$topic4,df$CC_anthropogenic, prop.c =T) 

# Dummies for each profile
df$topic4_1 <- ifelse(df$topic4==1, 1,0)
df$topic4_2 <- ifelse(df$topic4==2, 1,0)
df$topic4_3 <- ifelse(df$topic4==3, 1,0)
df$topic4_4 <- ifelse(df$topic4==4, 1,0)

reg_topic4_1 <- lm(as.formula(paste("topic4_1 ~", end_formula12)), data = df)
summary(reg_topic3_1)

reg_topic4_2 <- lm(as.formula(paste("topic4_2 ~", end_formula12)), data = df)
summary(reg_topic3_2)

reg_topic4_3 <- lm(as.formula(paste("topic4_3 ~", end_formula12)), data = df)
summary(reg_topic3_3)

reg_topic4_4 <- lm(as.formula(paste("topic4_4 ~", end_formula12)), data = df)
summary(reg_topic3_3)

desc_table(dep_vars = c("topic4_1", "topic4_2", "topic4_3", "topic4_4"), filename = "../LDA/topic4",
           dep.var.labels = c("Profile 1", "Profile 2", "Profile 3", "Profile 4"),
           dep.var.caption = c(""), data = df, indep_vars = control_variables, indep_labels = cov_lab)



