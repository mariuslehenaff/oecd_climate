##### Functions LDA #####
# Function to scale-up probabilities from the beta vectors. Does not account for mutually exclusive answers.
# beta_transformation <- function(beta, nbr_question=107) {
#   prob <- 0
#   for (i in 1:nbr_question){
#     prob <- prob + beta*(1-beta)^(i-1)
#   }
#   return(prob)
# }

# Normalized PMI (pairwise)
NPMI <- function(NPMI_matrix){
  #NPMI_matrix needs to be a 2x2 matrix of joint distributions
  if (NPMI_matrix[1,2]==1){
    return(1)
  } else if (NPMI_matrix[1,2]==0) { #Case when two opposite answers appears as the top features
    return(0)
  } else {
    return(log(NPMI_matrix[1,2]/(NPMI_matrix[1,1]*NPMI_matrix[2,2]))/(-log(NPMI_matrix[1,2])))
  }
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
# gini_slant <- function(lda_gamma){
#   gini_vect <- c()
#   for(i in 1:max(lda_gamma$document)){
#     #get the type shares for each types
#     shares <- lda_gamma$gamma[lda_gamma$document==i]
#     #take distance between those two
#     matrix_diff <- abs(outer(shares,shares,FUN="-"))
#     #compute Gini measure
#     gini_vect[i]<- sum(matrix_diff)/(2*(max(lda_gamma$topic)-1)*sum(shares))
#   }
#   return(gini_vect)
# }


# #Input a LDA from Topicmodels package
# polarisation <- function(lda_out, nu = 0.5){
#   # Nbr of persons per type
#   topics_vect <- topics(lda_out)
#   
#   nbr_profiles <- 1:max(topics_vect)
#   pop_profile <- sapply(nbr_profiles, function(x) sum(topics_vect==x))
#   
#   #Get beta vectors
#   lda_topics <- lda_out %>%
#     tidy(matrix = "beta") %>%
#     arrange(topic, desc(beta))
#   
#   #Get gamma matrix
#   gamma_df <- lda_out %>%
#     tidy(matrix = "gamma")
#   # Sort matrix in terms of id of respondent and % of each profile
#   gamma_df$document <- substr(gamma_df$document, 5, 10)
#   gamma_df$document <- as.numeric(gamma_df$document)
#   gamma_df <- gamma_df %>%
#     arrange(document)
#   #Add assigned group to gamma matrix
#   gamma_df$assigned <- rep(topics_vect, each=max(topics_vect))
#   #Share of each type for each type
#   gamma_mean<- gamma_df %>%
#     group_by(assigned, topic) %>% 
#     summarise(gamma = mean(gamma))
#   gamma_mean <- gamma_mean$gamma
#   dim(gamma_mean) <- c(max(topics_vect),max(topics_vect))
#   # [1,2]: share of type 1 for dominant group 2 (i.e on avg what share of type 1 do member of type 2 have)
#   # [2,1]: share of type 2 for dominant group 1:
#   # we can check this matrix to have a sense of polarization (do each column differ a lot?/
#   # do a profile only has a few share of the others?)
#   
#   kappa<- sum(pop_profile)^(-(2+nu))
#   polarisation <- 0
#   # Compute Polarisation
#   for(t in 1:max(topics_vect)){
#     sum_j <- 0
#     #Multiply by share
#     for(j in 1:max(topics_vect)){
#       diff_tk <- 0
#       # Distance for each share + rho. k is second subscript in equation.
#       for(k in 1:max(topics_vect)){
#         beta_t<-lda_topics$beta[lda_topics$topic==t]
#         beta_k<-lda_topics$beta[lda_topics$topic==k]
#         rho_tk <- (3-cor(beta_t,beta_k))/2
#         
#         diff_tk <- diff_tk + abs(gamma_mean[t,k]-gamma_mean[j,k])*rho_tk
#       }
#       sum_j <- sum_j + diff_tk*pop_profile[j]*pop_profile[t]^(1+nu)
#     }
#     polarisation <- polarisation+sum_j
#   }
#   polarisation <- polarisation*kappa
#   return(polarisation)
# }

create_lda <- function(variables_lda, data = e, nb_topic = NULL, compute_wtd_proba = F, convert_5_to_3 = T) {
  # if nb_topic = NULL, the number of topics is optimized
  e$concat_response <- ""
  for (v in variables_lda) {
    if (is.numeric(e[[v]])){
      e$concat_response <- case_when(e[[v]] == 2 ~ paste(e$concat_response, paste0(v, "__1")),
                                     e[[v]] == -2 ~ paste(e$concat_response, paste0(v, "__-1")),
                                     abs(e[[v]]) != 2 | is.pnr(e[[v]]) ~ paste(e$concat_response, paste0(v, "__0")))
    } else {
      e$concat_response <- paste(e$concat_response, paste(v, gsub(" ", "_", gsub("[[:punct:]]", "", e[[v]])), sep="__"))
    }
  }
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
    start <- Sys.time()
    cohesion_matrix <- sapply(profiles, cohesion, dfm = dfm, nbr_topics = topics)
    (duration_cohesion <- Sys.time() - start) 
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
  # lda_topics[,4] <- beta_transformation(lda_topics[,3], length(variables)) %>% rename(adj_beta=beta)
  lda_topics[,4] <- sapply(1:nrow(lda_topics), FUN = function(i) { mean(grepl(lda_topics$term[i], e$concat_response[e$topic == lda_topics$topic[i]]))} )
  if (compute_wtd_proba) lda_topics[,5] <- sapply(1:nrow(lda_topics), FUN = function(i) { wtd.mean(grepl(lda_topics$term[i], e$concat_response[e$topic == lda_topics$topic[i]]), weights = e$weight)} )
  if (compute_wtd_proba) names(lda_topics)[4:5] <- c("proba", "wtd.proba")
  else names(lda_topics)[4] <- "proba"
  
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


##### Treatment LDA ####  
# variables_lda <- names(e)[72:272][!grepl("_field|^winner$|attention_test|duration_|_first|_last|_click|_First|_Last|_Click|_order|race_other|sector|excluded|race_hawaii|wtp_|race_native|attentive|language|race_pnr", names(e)[72:272])] # TODO! which variable to exclude / automatize exclusion of consensual variables?
variables_lda <- names(e)[45:272][!grepl("_field|^winner$|attention_test|duration_|_first|_last|_click|_First|_Last|_Click|_order|race_other|sector|excluded|race_hawaii|wtp_|race_native|attentive|language|race_pnr", names(e)[45:272])] # TODO! which variable to exclude / automatize exclusion of consensual variables?

variables_lda <- c(variables_lda, "wtp", "country")
start <- Sys.time()
lda_no_knowledge <- create_lda(variables_lda, data = e)
(duration_lda <- Sys.time() - start) # 42min for all with optimization / 30 seconds without, all without weighted probas
(terms <- terms(lda[[1]], 20)) # terms(lda_out, 20) lda_out <- lda[[1]]
(lda_topics <- lda[[2]]) # lda_topics <- lda[[2]] lda_gamma <- lda[[3]]
e <- lda[[4]]

decrit("topic", data = e)

# ggplot(lda[[5]], aes(x=profiles, y=cohesion_avg)) + geom_line() + xlab("Number of Profiles") + ylab("Cohesion Score")  # }

describe_profiles <- function(lda, nb_terms = 6) {
  terms <- terms(lda[[1]], nb_terms)
  lda_topics <- lda[[2]]
  nb_topic <- dim(terms)[2]
  text <- ""
  for (i in 1:nb_topic) {
    var_val_i <- strsplit(terms[,i], "__")
    text_i <- ""
    for (j in 1:nb_terms) {
      var_ij <- sub("__.*", "", terms[j,i])
      var_ij <- var_val_i[[j]][1]
      val_ij <- var_val_i[[j]][2]
      lab_ij <- paste0(sub("[^:]*: ", "", Label(e[[var_ij]]))) # var_name_ij <- sub(":.*", "", Label(e[[var_ij]]))
      # if (lab_ij=="") lab_ij <- var_ij # Uncomment to remove the variable name (except when there is no label)
      lab_ij <- ifelse(lab_ij=="", var_ij, paste0(var_ij, ": ``", lab_ij, "''")) 
      proba_ij <- round(100*lda_topics$proba[lda_topics$topic==i & lda_topics$term==terms[j,i]])
      text_i <- paste(text_i, paste0(" \\item \\textit{", val_ij, "} to ", lab_ij, " [\\textit{", proba_ij, "\\%}] "))
    }
    text <- paste0(text, "\\textbf{Profile ", i, "} \\\\ \\begin{itemize}", text_i, " \\end{itemize} \\\\")
  }
  return(gsub("_", "\\_", text, fixed = T))
}

describe_profiles(lda, nb_terms = 6)
write_clip(describe_profiles(lda, nb_terms = 6))


# control_variables, etc. are defined in preparation.R

desc_table(dep_vars = c("topic2_1", "topic2_2"), filename = "../LDA/topic2",
           dep.var.labels = c("Profile 1", "Profile 2"),
           dep.var.caption = c(""), data = e, indep_vars = control_variables, indep_labels = cov_lab)


##### Data preparation for PCA, EFA #####
variables_a <- variables_lda
kept_vars <- non_numeric_vars <- constant_vars <- c()
for (i in seq_along(variables_a)) {
  n <- length(Levels(e[[variables_a[i]]]))
  if (n > 7) { print(paste(n, variables_a[i]))
  } else  kept_vars <- c(kept_vars, i) }
e_a <- e[,variables_a[kept_vars]]
for (i in 1:length(e_a)) if (!is.numeric(e_a[[i]])) {
    non_numeric_vars <- c(non_numeric_vars, i)
    for (j in Levels(e_a[[i]])) e_a[[paste0(names(e_a)[i], "_", gsub(" ", "_", substr(j, 1, 15)))]] <- 1*(e_a[[i]] == j)
}
for (i in 1:length(e_a)) if (length(Levels(e_a[[i]]))==1) constant_vars <- c(constant_vars, i)
e_a <- e_a[,-union(non_numeric_vars, constant_vars)]
e_am <- e_a # imputes mean to missings
for (i in seq_along(names(e_am))) e_am[[i]] <- z_score_computation(group = c(names(e_am)[i], F, "", T), df = e_am, weight = T) 
# for (i in 1:length(e_a)) if (!is.numeric(e_a[[i]])) print(names(e_a[i]))


##### PCA #####
# prcomp uses a Singular value decomposition which examines the covariances / correlations between individuals
# princomp uses a Spectral decomposition which examines the covariances / correlations between variables
# prcomp has better numerical accuracy hence should be preferred
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#prcomp-and-princomp-functions
# /!\ Multiple correspondence analysis is the counterpart of PCA for categorical data: https://en.wikipedia.org/wiki/Multiple_correspondence_analysis
pca_all_missing_mean <- prcomp(e_am, scale = T, rank = 5) # prcomp doesn't handle missing values / alternative to rank = 5: tol = 0.03
pca_all <- prcomp(imputePCA(e_a, method = "EM", ncp = 5)$completeObs, scale = T, rank = 5) # 5 components

summary(pca_all)

export_pca <- function(pca, nb_vars = 10, filename = deparse(substitute(pca))) {
  if (class(pca)=="loadings") { 
    n <- ncol(pca)
    data <- matrix(as.numeric(pca), ncol = n)
    rownames(data) <- rownames(pca)
    vx <- colSums(pca^2)
    variance <- rbind(`SS loadings` = vx, `Proportion Var` = vx/nrow(pca), `Cumulative Var` = cumsum(vx/nrow(pca)))
  } else { 
    n <- ncol(pca$rotation)
    variance <- summary(pca)$importance[,1:n]
    data <- pca$rotation }
  tab <- matrix(NA, nrow = nb_vars+2, ncol = 2*n)
  for (j in 1:n) {
    order_j <- order(abs(data[, j]), decreasing = T)[1:nb_vars]
    for (i in 1:nb_vars) {
      tab[i+2, 2*j-1] <- rownames(data)[order_j[i]]
      tab[i+2, 2*j] <- round(data[order_j[i], j], 3) # 3 digits
    }
    tab[1, 2*j] <- round(variance[2, j], 3)
    tab[2, 2*j] <- round(variance[3, j], 3)
    tab[1:2, 2*j-1] <- ""
  }
  tab[1, 1] <- "Proportion of variance"
  tab[2, 1] <- "Cumulative proportion"
  # TODO: add caption, \scalebox{.6}{..} replace first line by  & \multicolumn{2}{c}{Component 1} & \multicolumn{2}{c}{Component 2} & \multicolumn{2}{c}{Component 3} & \multicolumn{2}{c}{Component 4} & \multicolumn{2}{c}{Component 5} \\ 
  print(xtable(tab, type = "latex"), file = paste0("../tables/PCA/", filename, ".tex"))
  return(tab)
}
export_pca(pca_all) # automatic handling of missing values
export_pca(pca_all_missing_mean) # mean imputed for missing values


#### EFA #####
# The different between PCA and EFA is that PCA explains each observation as a combination of components, while EFA explains each variable as a combination of factors.
#   PCA accounts for all the variance while EFA accounts just for the common variance between factors. 
#   PCA is more appropriate when there is no structure behind the data while EFA is more appropriate when looking for underlying causal factors.
# First: remove multicolinear variables
X <- as.matrix(e_am)
qr.X <- qr(X, tol=1e-9, LAPACK = FALSE)
(rnkX <- qr.X$rank)  
(keep <- qr.X$pivot[seq_len(rnkX)])
X <- X[,keep]

efa_all <- factanal(X, 5, lower = 0.01)$loadings 
# for (i in 1:length(e_a)) if (any(is.infinite(e_a[[i]]))) print(names(e_a)[i])
# as.numeric(factanal(data.frame(c(NA, 1)), 5)$loadings)
export_pca(efa_all)
