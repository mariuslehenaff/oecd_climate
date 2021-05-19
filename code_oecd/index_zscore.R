variables_knowledge2 <- c("score_footprint_transport", "score_footprint_elec", "score_footprint_food", "score_footprint_pc", "score_footprint_region", "CC_dynamic", "CC_anthropogenic", "CC_real", "score_CC_impacts", "CC_knowledgeable", "score_GHG")
"CC_anthropogenic"

z_score_computation <- function(variable_name, df=e, weight=T){
  
  variable_name_zscore <-  paste(variable_name,"zscore", sep = "_")
  
  # get mean and sd by treatment groups
  mean_sd <- as.data.frame(sapply(split(df, df$treatment), 
                                  function(x) { if (weight) weights <- x$weight
                                  else weights <- NULL
                                  return(c(wtd.mean(x[[variable_name]], w = weights, na.rm=T), sqrt(wtd.var(x[[variable_name]], w = weights, na.rm=T)))) } ))
  
  # compute z-score
  df[[variable_name_zscore]] <- ((df[[variable_name]])-mean_sd[1,1])/mean_sd[2,1]
  
  # replace missing values with its group mean
  df[[variable_name_zscore]][df$treatment == "None"  &  is.pnr(df[[variable_name]])] <- (mean_sd[1,1]-mean_sd[1,1])/mean_sd[2,1]
  df[[variable_name_zscore]][df$treatment == "Climate"  &  is.pnr(df[[variable_name]])] <- (mean_sd[1,2]-mean_sd[1,1])/mean_sd[2,1]
  df[[variable_name_zscore]][df$treatment == "Policy"  &  is.pnr(df[[variable_name]])] <- (mean_sd[1,3]-mean_sd[1,1])/mean_sd[2,1]
  df[[variable_name_zscore]][df$treatment == "Both"  &  is.pnr(df[[variable_name]])] <- (mean_sd[1,4]-mean_sd[1,1])/mean_sd[2,1]
  
  zscore <- as.numeric(df[[variable_name_zscore]])
  
  return(zscore)
}  

index_zscore <- function(variables, df=e, weight=T) {
  if (length(variables)==1) variables <- variables[1]
  zscores <- as.data.frame(sapply(variables, z_score_computation, df=df, weight=weight))
  #zscore_names<- as.vector(sapply(variables_list,function(x) paste(x,"zscore", sep = "_")))
  #colnames(zscore) <- zscore_names
  return(rowMeans(zscores))
}