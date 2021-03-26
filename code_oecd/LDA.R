##### Functions #####
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
  ifelse(NPMI_matrix[1,2]==1, 1, log(NPMI_matrix[1,2]/(NPMI_matrix[1,1]*NPMI_matrix[2,2]))/(-log(NPMI_matrix[1,2])))
}

# Cohesion score based on Draca and Schwarz (2020)
# dfm: a dfm matrix for package topicmodels
# nbr_profiles: nbr of profiles for the LDA
# nbr_topics: nbr of top topics for the NPMI average
cohesion <- function(dfm, nbr_profiles=2, nbr_topics=c(5)){
  #Top answers for each profile
  lda_out <- LDA(dfm, method="Gibbs", k=nbr_profiles, control=list(seed=42))

  cohesion <- c()
  for(l in nbr_topics){
    terms<-terms(lda_out, l)

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
      dim(NPMI_matrix) <- c(l, l)
      
      # Avg of NPMIs within a profile
      NPMI_avg[k] <- (sum(NPMI_matrix*(upper.tri(NPMI_matrix)+lower.tri(NPMI_matrix)))/(l*(l-1)))
    }
    # Avg of NPMI_avg of all profiles
    cohesion <- c(cohesion, sum(NPMI_avg)/nbr_profiles)
  }
  return(cohesion)
}


# Function to get within-person concentration
gini_slant <- function(lda_gamma){
  gini_vect <- c()
  for(i in 1:max(lda_gamma$document)){
    #get the type shares for each types
    shares <- lda_gamma$gamma[lda_gamma$document==i]
    #take distance between those two
    matrix_diff <- abs(outer(shares,shares,FUN="-"))
    #compute Gini measure
    gini_vect[i]<- sum(matrix_diff)/(2*(max(lda_gamma$topic)-1)*sum(shares))
  }
  return(gini_vect)
}


#Input a LDA from Topicmodels package
polarisation <- function(lda_out, nu = 0.5){
  # Nbr of persons per type
  topics_vect <- topics(lda_out)
  
  nbr_profiles <- 1:max(topics_vect)
  pop_profile <- sapply(nbr_profiles, function(x) sum(topics_vect==x))
  
  #Get beta vectors
  lda_topics <- lda_out %>%
    tidy(matrix = "beta") %>%
    arrange(topic, desc(beta))
  
  #Get gamma matrix
  gamma_df <- lda_out %>%
    tidy(matrix = "gamma")
  # Sort matrix in terms of id of respondent and % of each profile
  gamma_df$document <- substr(gamma_df$document, 5, 10)
  gamma_df$document <- as.numeric(gamma_df$document)
  gamma_df <- gamma_df %>%
    arrange(document)
  #Add assigned group to gamma matrix
  gamma_df$assigned <- rep(topics_vect, each=max(topics_vect))
  #Share of each type for each type
  gamma_mean<- gamma_df %>%
    group_by(assigned, topic) %>% 
    summarise(gamma = mean(gamma))
  gamma_mean <- gamma_mean$gamma
  dim(gamma_mean) <- c(max(topics_vect),max(topics_vect))
  # [1,2]: share of type 1 for dominant group 2 (i.e on avg what share of type 1 do member of type 2 have)
  # [2,1]: share of type 2 for dominant group 1:
  # we can check this matrix to have a sense of polarization (do each column differ a lot?/
  # do a profile only has a few share of the others?)
  
  kappa<- sum(pop_profile)^(-(2+nu))
  polarisation <- 0
  # Compute Polarisation
  for(t in 1:max(topics_vect)){
    sum_j <- 0
    #Multiply by share
    for(j in 1:max(topics_vect)){
      diff_tk <- 0
      # Distance for each share + rho. k is second subscript in equation.
      for(k in 1:max(topics_vect)){
        beta_t<-lda_topics$beta[lda_topics$topic==t]
        beta_k<-lda_topics$beta[lda_topics$topic==k]
        rho_tk <- (3-cor(beta_t,beta_k))/2
        
        diff_tk <- diff_tk + abs(gamma_mean[t,k]-gamma_mean[j,k])*rho_tk
      }
      sum_j <- sum_j + diff_tk*pop_profile[j]*pop_profile[t]^(1+nu)
    }
    polarisation <- polarisation+sum_j
  }
  polarisation <- polarisation*kappa
  return(polarisation)
}

create_lda <- function(variables, data = e, nb_topic = NULL, compute_wtd_proba = F) {
  # if nb_topic = NULL, the number of topics is optimized
  e$concat_response <- ""
  for (v in variables_lda) ifelse(is.numeric(e[[v]]), e$concat_response <- paste(e$concat_response, paste(v, gsub(" ", "_", (e[[v]]+0)), sep="__")), 
                                  e$concat_response <- paste(e$concat_response, paste(v, gsub(" ", "_", gsub("[[:punct:]]", "", e[[v]])), sep="__")))
  e$concat_response <- tolower(e$concat_response)
  
  # e$WC_lda_var <- lengths(strsplit(e$concat_response, "\\W+")) # " "

  # Segment the 'sentence' into 'words/tokens', then transform it to DFM
  toks<- quanteda::tokens(e$concat_response) 
  # for (i in 1:nrow(e)) if(!all(sapply(toks[[i]], FUN = function(v) { grepl(v, e$concat_response[i])} ))) warning(paste("Some token in", i, "does not match concat_response"))
  dfm <- dfm(toks)
  
  # Convert for topicmodels package
  dfm <- quanteda::convert(dfm, to ="topicmodels")

  # Optimize number of topics
  if (missing(nb_topic)) {
    topics <- c(5,10,15,20)
    profiles <- 2:10
    nb_profile <- 2
    
    lda_out <- LDA(dfm, method="Gibbs", k=nb_profile, control=list(seed=42))
    
    cohesion_matrix <- sapply(profiles, cohesion, dfm = dfm, nbr_topics = topics)
    # colnames(cohesion_matrix) <- paste("Profiles", profiles, sep="_")
    # rownames(cohesion_matrix) <- paste("Topics", topics, sep="_")
    cohesion_avg <- colSums(cohesion_matrix)/length(topics)
    
    nb_topic <- profiles[which.max(cohesion_avg)]
    
    # if (return_plot) {
    cohesion_df <- data.frame(profiles, cohesion_avg)
    ggplot(cohesion_df, aes(x=profiles, y=cohesion_avg)) + geom_line() + xlab("Number of Profiles") + ylab("Cohesion Score")  # }
  } else { 
    nb_profile <- 0
    if (return_plot) warning("Plot of cohesion score per number of profile is not drawn when nb_topic is provided (as the number of topics is then not optimized)") }
  
  if (nb_profile != nb_topic) lda_out <- LDA(dfm, method="Gibbs", k=nb_topic, control=list(seed=42)) # TODO remove seed
  e$topic <- topics(lda_out)
  
  # Per-topic-per-word probabilities
  lda_topics <- lda_out %>% tidy(matrix = "beta") %>% arrange(topic, desc(beta))
  
  # Transform beta to have natural interpretation: overall probability that a feature.answer appears for a chosen profile
  lda_topics[,4] <- beta_transformation(lda_topics[,3], length(variables)) %>% rename(adj_beta=beta)
  lda_topics[,5] <- sapply(1:nrow(lda_topics), FUN = function(i) { mean(grepl(lda_topics$term[i], e$concat_response[e$topic == lda_topics$topic[i]]))} )
  if (compute_wtd_proba) lda_topics[,6] <- sapply(1:nrow(lda_topics), FUN = function(i) { wtd.mean(grepl(lda_topics$term[i], e$concat_response[e$topic == lda_topics$topic[i]]), weights = e$weight)} )
  if (compute_wtd_proba) names(lda_topics)[5:6] <- c("proba", "wtd.proba")
  else names(lda_topics)[5] <- "proba"
  
  # Per-document-per-topic probabilities (% of each profile for each respondent)
  lda_gamma <- lda_out %>% tidy(matrix = "gamma")
  
  # Sort matrix in terms of id of respondent and % of each profile
  lda_gamma$document <- substr(lda_gamma$document, 5, 10)
  lda_gamma$document <- as.numeric(lda_gamma$document)
  lda_gamma <- lda_gamma %>% arrange(document)
  
  for (i in 1:nb_topic) e[[paste0("topic", nb_topic, "_", i)]] <- e$topic == i
  
  if (nb_profile != 0) return(list(lda_out, lda_topics, lda_gamma, e, cohesion_df))
  else return(list(lda_out, lda_topics, lda_gamma, e))
}


##### Treatment ####  
variables_lda <- names(e)[11:308][!grepl("_field|^winner$|race_other|sector|excluded|race_hawaii|wtp_|race_native|attentive|language|race_pnr", names(e)[11:308])] # TODO! which variable to exclude
start <- Sys.time()
lda <- create_lda(variables_lda, data = e)
(duration_lda <- Sys.time() - start) # 1min40 with optimization / 30 seconds without, all without weighted probas
(terms <- terms(lda[[1]], 20)) # terms(lda_out, 20) lda_out <- lda[[1]]
(lda_topics <- lda[[2]]) # lda_topics <- lda[[2]] lda_gamma <- lda[[3]]
e <- lda[[4]]

decrit("topic", data = e)

# ggplot(lda[[5]], aes(x=profiles, y=cohesion_avg)) + geom_line() + xlab("Number of Profiles") + ylab("Cohesion Score")  # }

control_variables <- c("race_white_only", "gender_dum", "children", "college", "employment_agg", "income_factor", "age_quota", "vote_dum")
# TODO! define elsewhere
cov_lab <- c("race: White only", "Male", "Children", "No college", "status: Retired" ,"status: Student", "status: Working", "Income Q2", "Income Q3", "Income Q4","age: 25-34", "age: 35-49", "age: 50-64", "age: 65+", "age: below 18", "vote: Biden", "vote: Trump")

desc_table(dep_vars = c("topic2_1", "topic2_2"), filename = "../LDA/topic2",
           dep.var.labels = c("Profile 1", "Profile 2"),
           dep.var.caption = c(""), data = e, indep_vars = control_variables, indep_labels = cov_lab)
