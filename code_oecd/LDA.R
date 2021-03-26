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

lda <- function(variables, data = e, nb_topic = 4) {
  e$concat_response <- ""
  for (v in variables_lda) ifelse(is.numeric(e[[v]]), e$concat_response <- paste(e$concat_response, paste(v, gsub(" ", "_", (e[[v]]+0)), sep="__")), 
                                  e$concat_response <- paste(e$concat_response, paste(v, gsub(" ", "_", e[[v]]), sep="__")))

  # e$WC_lda_var <- lengths(strsplit(e$concat_response, "\\W+")) # " "

  # Segment the 'sentence' into 'words/tokens', then transform it to DFM
  toks<- quanteda::tokens(e$concat_response) # TODO! avoid "," as topic
  dfm <- dfm(toks)
  
  # Convert for topicmodels package
  dfm <- quanteda::convert(dfm, to ="topicmodels")
  
  # TODO! optimize number of profiles
  
  lda_out <- LDA(dfm, method="Gibbs", k=nb_topic, control=list(seed=42)) # TODO remove seed
  
  terms(lda_out, 20)
  
  # Per-topic-per-word probabilities
  lda_topics <- lda_out %>% tidy(matrix = "beta") %>% arrange(topic, desc(beta))
  
  # Transform beta to have natural interpretation: overall probability that a feature.answer appears for a chosen profile
  lda_topics[,4] <- beta_transformation(lda_topics[,3]) %>% rename(prob=beta)
  
  # Per-document-per-topic probabilities (% of each profile for each respondent)
  lda_gamma <- lda_out %>% tidy(matrix = "gamma")
  
  # Sort matrix in terms of id of respondent and % of each profile
  lda_gamma$document <- substr(lda_gamma$document, 5, 10)
  lda_gamma$document <- as.numeric(lda_gamma$document)
  lda_gamma <- lda_gamma %>% arrange(document)
  
  # Profile assigned for each respondent
  # topic_assigned <- select(e, concat_response)
  # topic_assigned$topic <- topics(lda_out)
  e$topic <- topics(lda_out)
  
  for (i in 1:nb_topic) e[[paste0("topic", i)]] <- e$topic == i
  
  return(list(lda_out, lda_topics, lda_gamma, e))
}


##### Treatment ####  
variables_lda <- names(e)[11:308][!grepl("_field", names(e)[11:308])]
lda <- lda(variables_lda, nb_topic = 2)
lda_out <- lda[[1]]
lda_topics <- lda[[2]]
lda_gamma <- lda[[3]]
e <- lda[[4]]

terms(lda_out, 20)
lda_topics
decrit("topic", data = e)
