library(utils)

# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p, version = NULL, remove = FALSE, github = '') {
  if (remove) {
    detach(paste0("package:", p), unload = T)
    remove.packages(p)
  }
  if (!is.element(p, installed.packages()[,1])) {
    if (missing(version)) {
      if (github != '') {
        package("devtools")
        install_github(paste0(github, '/', p))
      } else install.packages(p)
    } else {
      package("remotes")
      install_version(p, version = version, repos = "http://cran.us.r-project.org", upgrade = "never", dependencies = TRUE)
    }
  }
  else { if(!missing(version)) warning(paste("'", p, "' is already installed with a (potentially) newer version. You may want to install the required version (", version, ") to avoid bugs.", sep=""))}
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

Paths = c("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/code_oecd", "C:/Users/afabre/Documents/www/oecd_climate/code_oecd", "C:/Users/ans7406/Documents/GitHub/oecd_climate/code_oecd")
names(Paths) = c("Bluebii", "afabre", "ans7406")
setwd(Paths[Sys.info()[7]])
if (file.exists(Paths["afabre"])) .libPaths(c("C:/Users/afabre/R-4.1.1/library", "C:/Users/afabre/R-4.1.2/library", "\\\\nash/mtec-home/afabre/My Documents/R/win-library/4.0"))  # R-4.0.3/

chooseCRANmirror(ind = 1)
package("plyr")
package("tm") 
package("memisc", version = "0.99.22") # in case of bug (endless loop), copy/paste folder /memisc in library and: install.packages("memisc", method = "win.binary")
package('tidyverse')
package("xtable")
package("rms")
package('pwr')
package("foreign")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "/home/adrien/anaconda3/bin", sep = .Platform$path.sep))
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/Users/afabre/Anaconda3/pkgs/plotly-orca-1.3.1-1/orca_app", sep = .Platform$path.sep)) # to correct bug orca, add folder of orca.exe
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:/ProgramData/Anaconda3/pkgs/plotly-orca-1.3.1-1/orca_app", sep = .Platform$path.sep))
package("plotly") # in case of bug due to kaleido: "pip install kaleido" in the python console
# package("reticulate")
package('gdata')
package("Hmisc")
package("quantreg")
package("rcompanion")
package("DescTools")
package("VCA")
package("glmnet")
# package("installr") # not for linux
package("plotly")
package("processx")
package("readstata13")
package("permute")
package("AER")
package("ivmodel")
package("rattle")
package("data.table")
package("reshape2")
# package("rddtools") # not available
# package("rddapp") # not available
package("mets")
package("descr")
package("stargazer")
package("clipr")
package("ergm") # wtd.median
package("mfx")
package("margins")
package("plotrix")
package("grDevices")
package("colorspace")
package("RColorBrewer")
package("colorRamps")
package("ordinal")
package("oglmx")
package("logistf")
package("emmeans")
package("ggeffects")
package("snakecase")
package("rdd")
# detach("package:corrplot", unload = T)
# remove.packages("corrplot")
# package("prettydoc")
# package("seriation")
package("corrplot", github = 'taiyun')#, version = "0.88")
package("psy")
package("lavaan")
package("StatMatch")
package("np")
package("AMR")
package("KSgeneral")
package("dgof")
package("SnowballC")
package("wordcloud")
package("RCurl")
package("XML")
package("equivalence")
package("RMallow")
package("rpart")
package("readr")
package("caTools")
package("dplyr")
package("party")
package("partykit")
package("rpart.plot")
package("Peacock.test")
package("openxlsx")
package("devtools")
# LDA
package("quanteda") 
package("topicmodels")
package("broom")
package("tidytext")
package("modelsummary")
package("dplR")
package("ggpubr")
package("RStata")
package("relaimpo") # works well with 21 variables, not much more. install from: install.packages("https://prof.bht-berlin.de/fileadmin/prof/groemp/downloads/relaimpo_2.2-5.zip", repos = NULL)
package("missMDA") # PCA
package("maps")
package("forcats")
package("modi")
# package("estimatr")

# package("quanteda")
# package("haven")
# package("dplyr")
# package("textclean")
# package("tm")
# package("SnowballC")
# package("wordcloud")
# c("quanteda","forcats", "tidytext", "haven", "dplyr", "topicmodels", "textclean", "tm" , "SnowballC", "wordcloud", "quanteda", 
#   "lmtest", "sandwich","ggplot2", "stargazer", "DBI", "readstata13", "haven", "dplyr", "xlsx", "textstem", "readxl",  "stringr", "ggrepel")
# install_github("rstudio/webshot2")
# package("webshot2")
package("htmlwidgets")
# package("magick") # Bug sur Ubuntu, ne surtout pas décommenter sur Ubuntu
library(magick) # TODO
# install_github(repo = "MatthieuStigler/RCompAngrist", subdir = "RCompAngrist")
# package("RCompAngrist")

# package("psych") # library(psych, exclude = "describe")
# package("semTools")
# package("interplot")
# package("jtools")
# package("effects")
# package("sjplot")
# package("doMC") # for parallel computing, does not work on Windows

# Fs <- function(QID) { s[QID][[1]] }
# Vs <- function(QID) { as.vector(Fs(QID))  } 
n <- function(var) { as.numeric(as.vector(var)) }
NSPs <- function(QID) { length(V(QID)[V(QID) == "NSP (Je ne veux pas répondre)"])/length(V(QID)) }
nsps <- function(id) { length(v(id)[v(id) == "NSP (Je ne veux pas répondre)"])/length(v(id)) }
Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
}
is.pnr <- function(variable, empty_as_pnr = T) {
  if (empty_as_pnr) {
    if (is.null(annotation(variable))) return(is.na(variable)|variable=="")
    else return(is.missing(variable)|variable=="")
  } else {
    if (is.null(annotation(variable))) return(is.na(variable))
    else return(is.missing(variable))    
  }
}
decrit <- function(variable, data = e, miss = TRUE, weights = NULL, numbers = FALSE, which = NULL, weight = T) { # TODO!: allow for boolean weights
  # if (!missing(data)) variable <- data[[variable]]
  if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
  if (!missing(which)) variable <- variable[which] 
  if (weight) { 
    # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
    if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else { 
    if (!missing(which)) weights <- weights[which]
    if (length(weights)!=length(variable)) {
      warning("Lengths of weight and variable differ, non-weighted results are provided")
      weights <- NULL
    } }
  if (length(annotation(variable))>0 & !numbers) {
    if (!miss) {
      # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
    }
    else {
      if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
  }
  else {  
    if (length(annotation(variable))>0) {
      if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
      else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
    } else describe(variable[variable!=""], weights = weights[variable!=""])  }
}
Levels <- function(variable, data = e, miss = TRUE, numbers = FALSE) {
  Levs <- decrit(variable, miss = miss, numbers = numbers, data = data)$values$value
  if (is.null(Levs)) {
    if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
    Levs <- unique(variable) }
  return(Levs)
  # if (is.character(var) & length(var)==1) var <- data[[var]]
  # if (length(annotation(var))>0) { # works but cubmbersome and doesn't allow to get rid of missings
  #   if (is.character(var)) levels(as.factor(include.missings(var)))
  #   else return(as.vector(labels(var))) }
  # else return(levels(as.factor(var))) # as.factor may cause issues as it converts to string
}
# decrit <- function(variable, miss = FALSE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) { 
#   # if (!missing(data)) variable <- data[[variable]]
#   if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
#   if (!missing(which)) variable <- variable[which] 
#   if (weight) { 
#     # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
#     if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else { 
#     if (!missing(which)) weights <- weights[which]
#     if (length(weights)!=length(variable)) {
#       warning("Lengths of weight and variable differ, non-weighted results are provided")
#       weights <- NULL
#     } }
#   if (length(annotation(variable))>0 & !numbers) {
#     if (!miss) {
#       # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
#     }
#     else {
#       if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#   }
#   else {  
#     if (length(annotation(variable))>0) {
#       if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
#       else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
#     } else describe(variable[variable!=""], weights = weights[variable!=""])  }
# }
# decrit <- function(variable, miss = FALSE, weights = NULL, numbers = FALSE, data = e, which = NULL, weight = T) { 
#   # if (!missing(data)) variable <- data[[variable]]
#   if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
#   if (!missing(which)) variable <- variable[which] 
#   if (weight) { 
#     # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
#     if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else { 
#     if (!missing(which)) weights <- weights[which]
#     if (length(weights)!=length(variable)) {
#       warning("Lengths of weight and variable differ, non-weighted results are provided")
#       weights <- NULL
#     } }
#   if (length(annotation(variable))>0 & !numbers) {
#     if (!miss) {
#       # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#       if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
#     }
#     else {
#       if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
#       else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
#   }
#   else {  
#     if (length(annotation(variable))>0) {
#       if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
#       else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
#     } else describe(variable[variable!=""], weights = weights[variable!=""])  }
# }
export_stats_desc <- function(data, file, miss = TRUE, sorted_by_n = FALSE, return = FALSE, fill_extern = FALSE) {
  original_width <- getOption("width")
  options(width = 10000)
  des <- des_miss <- nb_miss <- labels <- n <- c()
  for (i in 1:length(data)) {
    decrit_i <- capture.output(print(decrit(data[[i]], miss=TRUE)))
    n <- c(n, as.numeric(sub('[[:blank:]]*([[:digit:]]*).*', '\\1', decrit_i[3])))
    des_i <- gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(capture.output(print(decrit(data[[i]]))), collapse='<br>')))))
    if (str_count(des_i, fixed("),"))>1) { des_i <- gsub("),",")<br>",des_i) }
    des_miss_i <-  gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(decrit_i, collapse='<br>')))))
    if (str_count(des_miss_i, fixed("),"))>1) { des_miss_i <- gsub("),",")<br>",des_miss_i) }
    nb_miss_i <- gsub(".*<br>        n","        n",gsub(".*<br>       n","       n",gsub(".*<br>      n","      n",gsub("<br><br>lowest.*","",paste(decrit_i[1:3], collapse='<br>')))))
    des <- c(des, des_i)
    des_miss <- c(des_miss, des_miss_i)
    nb_miss <- c(nb_miss, nb_miss_i)
    if (Label(data[[i]])=='') label_i <- colnames(data)[i]
    else label_i <- paste(colnames(data)[i], sub('[^:]*:', '', Label(data[[i]])), sep=':')
    labels <- c(labels, label_i)
  }
  
  if (miss=='both') output <- matrix(c(names(data), labels, des, des_miss), ncol=4)
  if (miss=='nb') output <- matrix(c(names(data), labels, nb_miss), ncol=3)
  else if (sorted_by_n) output <- matrix(c(names(data), labels, des_miss), ncol=3)[order(-n),]
  else if (miss) output <- matrix(c(names(data), labels, des_miss), ncol=3)
  else output <- matrix(c(names(data), labels, des), ncol=3)
  write.table(output, file=file, sep=";;;", row.names=FALSE, col.names=FALSE, quote=FALSE)
  options(width = original_width)
  if (fill_extern) {
    nb_reponses <<- n
    nb_manquants <<- des_miss     }
  if (return) return(output)
}
desc_table <- function(dep_vars, filename = NULL, data = e, indep_vars = control_variables, indep_labels = NULL, weights = data$weight,
                       save_folder = "../tables/", dep.var.labels = NULL, dep.var.caption = c(""), digits= 3, mean_control = FALSE, logit = FALSE, atmean = T, robust_SE = FALSE,
                       mean_above = T, only_mean = F, keep = indep_vars, nolabel = F, indep_vars_included = T, no.space = F) {
  # Wrapper for stargazer
  # /!\ always run first with nolabel = T to check that the order of indep_labels correspond to the one displayed
  # dep_vars: either a variable name (automatically repeated if needed) or a list of variable names (of length the number of columns)
  # (in)dep_vars accept expressions of type : var_name expression (e.g. "equal_quota %in% 0:1", but not "equal_quota == 0 | equal_quota==1)
  # /!\ To appear in the table, they should be written without parentheses and with due space, e.g. "var > 0" and not "(var>0)"
  # indep_vars is the list of potential covariates, they are by default all included by 
  # indep_vars_included can be set to a list (of length the number of columns) of booleans or variable names to specify which covariates to include in each column
  # keep is a vector of regular expressions allowing to specify which covariates to display (by default, all except the Constant)
  # mean_above=T displays the mean of the dependant var (for those which treatment=="control" if mean_control = T) at top rather than bottom of Table (only_mean=T only displays that)
  # logit is either a boolean or a boolean vector 
  if (missing(dep.var.labels) & !(is.character(dep_vars))) dep.var.labels <- dep_vars
  dep.var.labels.include <- ifelse(is.null(dep.var.labels), F, T)
  names(indep_vars) <- indep_vars
  if (class(indep_vars_included)=="list") { if (length(dep_vars)==1) dep_vars <- rep(dep_vars[1], length(indep_vars_included))  }
  else { indep_vars_included <- rep(list(rep(T, length(indep_vars))), length(dep_vars)) }
  if (length(logit) == 1) logit <- rep(logit, length(dep_vars))
  models <- coefs <- SEs <- list()
  means <- c()
  for (i in seq_along(dep_vars)) {
    formula_i <- as.formula(paste(dep_vars[i], "~", paste("(", indep_vars[indep_vars_included[[i]]], ")", collapse = ' + ')))
    if (logit[i]) { 
      models[[i]] <- glm(formula_i, data = data, family = binomial(link='logit'))
      logit_margin_i <- logitmfx(formula_i, data = data, robust = robust_SE, atmean = atmean)$mfxest # TODO! weights with logit; NB: logitmfx computes White/robust SE when robust_SE == T
      coefs[[i]] <- logit_margin_i[,1]
      SEs[[i]] <- logit_margin_i[,2] 
    }
    else { 
      models[[i]] <- lm(formula_i, data = data, weights = weights)
      coefs[[i]] <- models[[i]]$coefficients
      if (robust_SE) SEs[[i]] <- coeftest(models[[i]], vcov = vcovHC(models[[i]], "HC1"))[,2]
      else SEs[[i]] <- summary(models[[i]])$coefficients[,2]
    }
    if (mean_control==FALSE){
      means[i] <- round(wtd.mean(eval(parse(text = paste( "data$", parse(text = dep_vars[i]), sep=""))), weights = weights, na.rm = T), d = digits)
      mean_text <- "Mean"
    } else {
      means[i] <- round(wtd.mean(eval(parse(text = paste( "(data$", parse(text = dep_vars[i]), ")[data$treatment=='None']", sep=""))), weights = weights[data$treatment=='None'], na.rm = T), d = digits)
      mean_text <- "Control group mean"
    }
  }
  if (missing(filename)) file_path <- NULL
  else file_path <- paste(save_folder, filename, ".tex", sep="")
  keep <- gsub("(.*)", "\\\\\\Q\\1\\\\\\E", keep)
  if (only_mean) mean_above <- T
  if (mean_above) {
    if (nolabel) table <- do.call(stargazer, c(models,
                                               list(out=NULL, header=F, model.numbers = F, add.lines = list(c(mean_text, means)),
                                                    coef = coefs, se = SEs,
                                                    dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption, dep.var.labels.include = dep.var.labels.include,
                                                    multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep, no.space = no.space #, omit.stat = c("n")
                                               )))
    else  table <- do.call(stargazer, c(models,
                                        list(out=NULL, header=F, model.numbers = F,
                                             covariate.labels = indep_labels, add.lines = list(c(mean_text, means)),
                                             coef = coefs, se = SEs,
                                             dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption, dep.var.labels.include = dep.var.labels.include,
                                             multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep, no.space = no.space #, omit.stat = c("n")
                                        )))
    mean_line <- regmatches(table, regexpr('(Mean|Control group mean) &[^\\]*', table))
    first_lab <- ifelse(missing(indep_labels), latexify(indep_vars[1]), paste0(latexify(indep_labels[1]), " &")) # was: latexify(ifelse(missing(indep_labels), indep_vars[1], indep_labels[1]))
    if (only_mean) {
      table <- write_clip(gsub(paste(first_lab, ".*"), paste(mean_line, '\\\\\\\\'), table), collapse=' ')
      table <- table[c(1:grep('(Mean|Control group mean) &[^\\]*', table)[1], (length(table)-3):length(table))]
    } else table <- write_clip(sub(first_lab, paste(mean_line, '\\\\\\\\ \\\\hline \\\\\\\\[-1.8ex]', first_lab),
                                   gsub('(Mean|Control group mean) &.*', '', table)), collapse=' ')
    cat(paste(table, collapse="\n"), file = file_path)
  } else {
    if (nolabel) table <- do.call(stargazer, c(models,
                                               list(out=file_path, header=F, add.lines =list(c(mean_text, means)),
                                                    coef = coefs, se = SEs,
                                                    dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption, dep.var.labels.include = dep.var.labels.include,
                                                    multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep, no.space = no.space
                                               )))
    else table <- do.call(stargazer, c(models,
                                       list(out=file_path, header=F,
                                            covariate.labels = indep_labels, add.lines =list(c(mean_text, means)),
                                            coef = coefs, se = SEs,
                                            dep.var.labels = dep.var.labels, dep.var.caption = dep.var.caption, dep.var.labels.include = dep.var.labels.include,
                                            multicolumn = F, float = F, keep.stat = c("n"), omit.table.layout = "n", keep=keep, no.space = no.space
                                       ))) }
  return(table)
}
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) 
}
clean_number <- function(vec, high_numbers='') { 
  numeric_vec <- as.numeric(gsub(",", ".", gsub("[[:alpha:]  !#$%&')?/(@:;€_-]","",vec)))
  if (high_numbers=='remove') { is.na(numeric_vec) <- numeric_vec>10000 }
  else if (high_numbers=='divide') { numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12 }
  else if (high_numbers=='divide&remove') { 
    numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12
    is.na(numeric_vec) <- numeric_vec>6000 }
  return(numeric_vec)
}
uc <- function(nb_pers, nb_14_et_plus) {
  # https://www.insee.fr/fr/metadonnees/definition/c1802
  return(1 + 0.5 * pmax(0, nb_14_et_plus - 1) + 0.3 * pmax(0, nb_pers - nb_14_et_plus))
}
quotient <- function(nb_pers, nb_adultes) {
  # https://droit-finances.commentcamarche.com/contents/907-quotient-familial-calcul-du-nombre-de-parts-fiscales
  # nb de parts fiscales en fonction de la situation et de nb_pers = 1 / 2 / 3 / 4 / 5
  # marie: x / 2 / 2.5 / 3 / 4 --- en concubinage: x / 1 / 1.5 /2 / 3 (= marie - 1) --- seul: 1 / 2 / 2.5 / 3.5 / 4.5
  return((nb_pers == 1) + (nb_pers == 2)*2 + (nb_pers == 3)*2.5 + (nb_pers == 4)*3 + (nb_pers > 4)*pmin(6, nb_pers - 1) + (nb_adultes==1)*(nb_pers > 3)*0.5 )
}
irpp <- function(rev, nb_adultes, nb_pers) {
  # quotient <- (nb_pers < 2) + (nb_pers == 2) * 2 + (nb_pers == 3) * 2.5 + (nb_pers == 4) * 3 + (nb_pers > 4) * pmin(6, nb_pers - 1)
  income <- 0.9334 * rev / quotient(nb_pers, nb_adultes) # (1 + (0.029 * 1.28))*0.9 : passage au brut (+28% en moyenne), CSG+CRDS non déductibles (2,90%), puis abattement de 10%
  ir <- 0
  ir <- ir + (income - 12815.25*12) * 0.45 * (income > 12815.25*12)
  ir <- ir + (pmin(income, 12676*12) - 6051.42*12) * 0.41  * (income > 6051.42*12)
  ir <- ir + (pmin(income, 6051.42*12) - 2257.17*12) * 0.3  * (income > 2257.17*12)
  ir <- ir + (pmin(income, 2257.17*12) - 817.25*12) * 0.14  * (income > 817.25*12)
  
  ir <- quotient(nb_pers, nb_adultes) * ir
  seuil_decote <- (nb_adultes>1)*2585/12 + (nb_adultes<=1)*1569/12
  # decote <- (1920 - 0.75 * ir) * (marie & ir<2560) + (1165 - 0.75 * ir) * (!(marie) & ir<1553)
  decote <- (ir < seuil_decote) * 0.75 * (seuil_decote - ir)
  return(pmax((ir-decote),0)) # vrai calcul
}
representativity_index <- function(weights, digits = 3) { return(round(sum(weights)^2/(length(weights)*sum(weights^2)), 3)) }


##### Graphiques #####
stack_bars <- function(vars, data=s, miss=T, labels=NA, title=NA, accord=FALSE, en = FALSE, margin=c(2.5,17,0,3), cex=1, width=0.77/length(vars), weights=FALSE) {
  matrice <- c()
  colors <-   c(rainbow(4, end=4/15)[1:3], "green", "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]]) & !is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (is.na(labels)) { labels <- vars }
  if (accord) { values <- c("Pas du tout", "Pas vraiment d'accord", "Indifférent-e", "Assez", "Tout à fait d'accord")
  if (miss) { widths <- c(0.16,0.16,0.13,0.125,0.145,0.05) }
  else { widths <- c(0.18,0.185,0.15,0.14,0.2) } }
  else { values <- c("Baisser fortement", "légèrement", "Maintenir", "Augmenter légèrement", "fortement")
  if (miss) { widths <- c(0.153,0.14,0.14,0.15,0.083,0.05) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (en) {values <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
  if (accord) values <- c("Totally disagree", "Disagree", "Indifferent", "Agree", "Totally agree")
  if (miss) { widths <- c(0.16,0.15,0.14,0.13,0.12,0.06) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (miss) { 
    if (en) values <- c(values, "PNR")
    else values <- c(values, "NSP") }
  # if (accord) { values <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord") }
  # else { values <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau actuel", "Augmenter légèrement", "Augmenter fortement") }
  # if (miss) { values <- c(values, "NSP (Ne sait pas, ne se prononce pas)")} # TODO: trouver widths pour ceux-là et les mettre
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 1.5 }
  par(mar=margin, oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrice, width=width, horiz=TRUE, add=TRUE, col=colors, names.arg = labels, cex.names = cex, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
oui_non <- function(vars, file, labels = vars, data = s, display_value = T, sort=T, colors=color(2), weights=T, margin_r=0, margin_l=NA, title="", en=FALSE, NSP=FALSE) { # 250 l
  margin_t <- 30
  if (title!="") { margin_t <- 80 }
  if (grepl("<br>", title)) { margin_t <- 130 }
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  oui <- non <- nsp <- c()
  for (var in vars) {
    if (weights) {
      oui <- c(oui, sum(data[['weight']][which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      non <- c(non, sum(data[['weight']][which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      nsp <- c(nsp, sum(data[['weight']][which(data[[var]]=="NSP" | data[[var]]==-1)])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) ) #  | data[[var]]==-1 | data[[var]]=="NSP"
    }
    else {
      oui <- c(oui, length(which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      non <- c(non, length(which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      nsp <- c(nsp, length(which(data[[var]]=="NSP" | data[[var]]==-1))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
    }  
  }
  true_nsp <- round(100 * nsp*(oui+non))
  oui <- round(100 * oui)
  non <- round(100 * non)
  nsp <- round(100 * nsp)
  if (sort) order_as <- order(oui/(oui+non))
  else order_as <- 1:length(oui)
  y <- labels[order_as]
  non <- non[order_as]
  nsp <- nsp[order_as]
  true_nsp <- true_nsp[order_as]
  if (sort) oui <- sort(oui)
  o <- round(100 * oui / (oui + non))
  n <- round(100 * non / (oui + non))
  
  if (en==T) {
    hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
    hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
    hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')  
    Text <- c("Yes", "No", "PNR")      }
  else if (en==FALSE) {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- c("Oui", "Non", "NSP") }
  else {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- en
    if (length(Text) ==2) Text <- c(Text, 'PNR')}
  if (display_value) {
    hover_oui <- paste(oui, '%')
    hover_non <- paste(non, '%')
    hover_nsp <- paste(true_nsp, '%')
  }
  if (!(NSP)) Text[3] <- ''
  print(oui)
  print(non)
  print(nsp)
  print(o)
  print(n)
  data <- data.frame(y, oui, non, nsp, o, n)
  data$y <- factor(data$y, levels = data[["y"]])
  y <- c(y, '')
  bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
                  hoverinfo = 'text', marker = list(color = colors[1], line = list(color = 'white', width = 1))) %>%
    add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = colors[2])) %>%
    add_trace(x = ~nsp, text = hover_nsp, hoverinfo = 'text', marker = list(color = 'lightgrey')) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           hovermode = 'closest',
           barmode = 'stack',
           title = title,
           titlefont = list(color='black'),
           font = list(color='black'),
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = margin_l, r = margin_r, t = margin_t, b = 0),
           showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                    xanchor = 'right',
                    text = y,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = c(10, 90, 110),
                    y = 1.1,
                    text = Text,
                    font = list(family = 'Arial', size = 15, color = 'black'),
                    showarrow = FALSE) # %>%
  # labeling the percentages of each bar (x_axis)
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o / 2, y = y,
  #                 text = paste(data[,"oui"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n / 2, y = y,
  #                 text = paste(data[,"non"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n + nsp / 2, y = y,
  #                 text = paste(data[,"nsp"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # api_create(bars, filename=file, sharing="public")
  return(bars) # bugs most often than not
}
data5 <- function(vars, data=e, miss=T, weights=T, rev=FALSE) {
  matrice <- c()
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      if (is.null(annotation(data[[var]]))) {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.na(data[[var]])))/length(which(!is.missing(n(data[[var]]))))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
      else {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]])))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) } }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))))    
      if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (rev & !(miss)) return(matrice[5:1,])
  else if (rev & miss) return(matrice[c(5:1,6),])
  else return(matrice)
  # return(as.data.frame(matrice))
}
data1 <- function(vars, data=e, weights=T) {
  if (is.null(data[['weight']])) weights <- F # TODO? warning
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T))/length(which(data[[var]]==T | data[[var]]==FALSE))) }
  }
  return( matrix(res, ncol=length(vars)) )
}
dataN <- function(var, data=e, miss=T, weights = T, return = "", fr=F, rev=FALSE, rev_legend = FALSE) {
  missing_labels <- c("NSP", "PNR", "Non concerné·e", "Included", "Don't know", "PNR or other", "NSP ou autre", "PNR ou autre") # TODO: allow for non-standard PNR in a more straightforward way than adding the argument "fr" and putting its value below
  if (is.null(data[['weight']])) weights <- F # TODO? warning
  mat <- c()
  if (is.character(data[[var]]) | (is.numeric(data[[var]]) & !grepl("item", class(data[[var]]))) | is.logical(data[[var]])) v <- as.factor(data[[var]]) # before: no is.logical
  else v <- data[[var]]
  if (setequal(levels(v), c(T, F))) levels <- c(T) # before: not this line
  else if (is.null(annotation(v))) levels <- levels(v)
  else levels <- labels(v)@.Data
  levels <- levels[!(levels %in% missing_labels)]
  if (rev_legend) levels <- rev(levels) # new (05/20)
  if (weights) N <- sum(data[['weight']][!is.missing(v) & (!(v %in% missing_labels))]) # c("NSP", "Non concerné·e")
  else N <- length(which(!is.missing(v) & (!(v %in% missing_labels)))) # c("NSP", "Non concerné·e")
  for (val in levels) { # before: no %in% nowhere below
    if (weights) mat <- c(mat, sum(data[['weight']][which(v==val)])/N)
    else mat <- c(mat, length(which(v==val))/N) }
  if (rev) mat <- rev(mat)
  if (miss) {
    if (is.null(annotation(v))) {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.na(v) | v %in% missing_labels)])/N) # c("NSP", "Non concerné·e")
      else mat <- c(mat, length(which(is.na(v) | v %in% missing_labels))/N) # c("NSP", "Non concerné·e")
    } else  {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.missing(v) & !is.na(v))])/N) # was defined without " & (!(v %in% c("NSP", "Non concerné·e")))" here and line below
      else mat <- c(mat, length(which(is.missing(v) & !is.na(v)))/N) } } # mais ça semble équivalent pck les NSP sont missing dans ces cas-là
  if (max(nchar(levels))==3 & 'Oui' %in% levels & 'Non' %in% levels) { if (which(levels=='Non') < which(levels=='Oui')) mat[2:1] <- mat[1:2]; levels[c(which(levels=='Oui'),which(levels=='Non'))] <- c('Non', 'Oui') }
  if ((return %in% c("levels", "legend")) & miss & fr==TRUE) return(c(levels, 'NSP'))
  else if ((return %in% c("levels", "legend")) & miss & (fr==FALSE)) return(c(levels, 'PNR'))
  else if ((return %in% c("levels", "legend")) & miss & is.character(fr)) return(c(levels, fr))
  else if ((return %in% c("levels", "legend")) & (!(miss))) return(levels)
  else if (return == "N") return(N)
  else return(matrix(mat, ncol=1))
}
dataKN <- function(vars, data=e, miss=T, weights = T, return = "", fr=F, rev=FALSE) {
  if (is.logical(data[[vars[1]]])) return(data1(vars, data, weights))
  else {
    res <- c()
    for (var in vars) res <- c(res, dataN(var, data, miss, weights, return, fr, rev))
    return(matrix(res, ncol=length(vars))) }
}
dataN2 <- function(var, df = list(c, e), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev))) }
dataN3 <- function(var, df = list(e2, e, c), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[3]], miss = miss, weights = weights, fr = fr, rev = rev))) }
dataNK <- function(var, df = list(e2, e, c), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else return(do.call(cbind, lapply(df, function (d) {dataN(var, d, miss = miss, weights = weights, fr = fr, rev = rev)}))) }
data12 <- function(vars, df = list(e, e2), miss=T, weights = T, fr=F, rev=FALSE, return = "") {
  if (length(vars)==1) return(dataN2(var=vars, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return))
  else {
    init <- T 
    for (var in vars) {
      if (init) {
        data <- dataN2(var=var, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return)
        init <- F
      } else {
        data <- cbind(data, dataN2(var=var, df=list(df[[2]], df[[1]]), miss=miss, weights=weights, fr=fr, rev=rev, return=return))
      }
    }
    return(data)
  } }
barres12 <- function(vars, df=list(e, e2), labels, legend=hover, comp = "V2", orig = NULL, miss=T, weights = T, fr=F, rev=T, color=c(), rev_color = FALSE, hover=legend, sort=TRUE, thin=T, return="", showLegend=T, export_xls = F) {
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  data1 <- dataKN(vars, data=df[[1]], miss=miss, weights = weights, return = "", fr=fr, rev=rev)
  if (missing(legend) & missing(hover)) { 
    if (is.logical(df[[1]][[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights) # before: uncommented and "else" next line
    else hover <- legend <- dataN(var = vars[1], data=df[[1]], miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } 
  agree <- order_agree(data = data1, miss = miss)
  if (is.logical(df[[1]][[vars[1]]])) agree <- rev(agree)
  if (return=="data") return(data12(vars[agree], df = df, miss=miss, weights = weights, fr=fr, rev=rev, return = ""))
  else if (return=="labels") return(labels12(labels[agree], en = !fr, comp = comp, orig = orig))
  else if (return=="legend") return(legend)
  else return(barres(data = data12(vars[agree], df = df, miss=miss, weights = weights, fr=fr, rev=rev, return = ""), 
                     labels=labels12(labels[agree], en = !fr, comp = comp, orig = orig), legend=legend, 
                     miss=miss, weights = weights, fr=fr, rev=rev, color=color, rev_color = rev_color, hover=hover, sort=F, thin=thin, showLegend=showLegend, export_xls = export_xls))
}

labels12 <- function(labels, en=F, comp = "V2", orig = NULL) {
  new_labels <- c()
  lab2 <- ifelse(comp=="V2", ifelse(en, " Wave 2 (W2)", " Vague 2 (V2)"), comp)
  lab1 <- ifelse(missing(orig), ifelse(en, " (W1)", " (V1)"), orig)
  for (l in labels) {
    new_labels <- c(new_labels, lab2, paste(l, lab1, sep=""))
    lab2 <- paste("", lab2) }
  return(new_labels)
}
labelsN <- function(labels, levels, parentheses = T) {
  new_labels <- c()
  if (parentheses) {
    labs_other <- paste0("(", levels[1:(length(levels)-1)], ")")
    labs_main <- paste0(labels, " (", levels[length(levels)], ")")
  } else {
    double_dot <- ifelse(max(length(labels))>1, ": ", "")
    labs_other <- levels[1:(length(levels)-1)]
    labs_main <- paste0(labels, double_dot, levels[length(levels)])  }
  for (l in seq_along(labels)) new_labels <- c(new_labels, labs_other, labs_main[l])
  return(new_labels) # version var (lev1) / (lev2) / ...
  # return(sapply(labels, function(l) {return(paste(l, levels, sep=": "))})) # version var: lev1 / var: lev2 / ...
}
barresN <- function(vars, along = NULL, df=list(e), labels = NULL, legend=hover, miss=T, weights = T, fr=F, rev=T, color=c(), 
                    rev_color = FALSE, hover=legend, thin=T, return="", showLegend=T, export_xls = F, parentheses = T, nolabel = F, error_margin = F, alphabetical = FALSE) {
  if (nolabel & length(labels)==1) labels <- "" 
  if (is.data.frame(df)) df <- list(df)
  if (!missing(along)) {
    if (along == "country_name" & !alphabetical & exists("countries_names")) {
      levels <- c()
      for (l in countries_names) if (l %in% Levels(df[[1]][[along]])) levels <- c(levels, l)
    } else levels <- Levels(df[[1]][[along]])
    levels <- sub("^\\*", "", rev(levels))
  }
  if (!missing(along)) data <- lapply(seq_along(levels), function(l) return(df[[1]][df[[1]][[along]]==levels[l],]))
  if (!missing(along) & missing(labels)) labels <- paste(along, levels, sep=": ")
  if (!missing(along) & length(labels) < length(df)*length(levels)*length(vars)) labels <- labelsN(labels, levels, parentheses = parentheses) 
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  data1 <- dataKN(vars, data=df[[1]], miss=miss, weights = weights, return = "", fr=fr, rev=rev)
  if (missing(legend) & missing(hover)) { 
    if (is.logical(df[[1]][[vars[1]]])) { showLegend = F; legend <- "True"; hover <- labels; } # data1(var = vars[1], data=df, weights = weights) # before: uncommented and "else" next line
    else hover <- legend <- dataN(var = vars[1], data=df[[1]], miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } 
  agree <- order_agree(data = data1, miss = miss)
  if (is.logical(df[[1]][[vars[1]]])) agree <- rev(agree)
  if (return=="levels") {if (is.null(levels)) {return(labels)} else {return(levels)}}
  else if (return=="labels") return(labels) # labels12(labels[agree], en = !fr, comp = comp, orig = orig)
  else if (return=="legend") return(legend)
  else {
    plotted_data <- dataNK(vars[agree], df = data, miss=miss, weights = weights, fr=fr, rev=rev, return = "")
    if (return=="data") { return(plotted_data)
    } else {
      not_nan <- sapply(c(1:ncol(plotted_data)), function(j) any(!is.nan(plotted_data[,j])))
      plotted_data <- plotted_data[,not_nan]
      return(barres(data = plotted_data, labels=labels[not_nan], legend=legend, # labels12(labels[agree], en = !fr, comp = comp, orig = orig) # /!\ doesn't currently support multiple vars
                     miss=miss, weights = weights, fr=fr, rev=rev, color=color, rev_color = rev_color, hover=hover, sort=F, thin=thin, showLegend=showLegend, export_xls = export_xls, error_margin = error_margin))
  } }
}
color5 <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') { # TODO! whitout white
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else if (theme=='default') {
    cols <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
  } else {
    cols <- rev(brewer.pal(max(n, 3), theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (n > 10) cols <- colorRampPalette(cols)(n)
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}
# accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
oui_non5 <- c("Non, pas du tout", "Non, pas vraiment", "Indifférent-e/NSP", "Oui, plutôt", "Oui, tout à fait")
yes_no5 <- c("Not at all", "Not really", "Indifferent/PNR", "Rather yes", "Yes, completely")
# agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
# evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
# evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
order_agree <- function(data, miss, rev = T, n = ncol(data)) {
  agree <- c()
  if (!missing(miss)) {
    if (miss) for (i in 1:n) agree <- c(agree, sum(data[floor(nrow(data)/2+1):max(1,(nrow(data)-1)),i]))
    else for (i in 1:n) agree <- c(agree, sum(data[ifelse(nrow(data)==1,1,ceiling(nrow(data)/2+1)):nrow(data),i]))
  } else {
    if (nrow(data)==5 | nrow(data)==6) { for (i in 1:n) { agree <- c(agree, data[4, i] + data[5, i]) } }
    else if (nrow(data)==7) { for (i in 1:n) { agree <- c(agree, data[6, i] + data[7, i]) } }
    else { for (i in 1:n) { agree <- c(agree, data[1, i]) } } }
  return(order(agree, decreasing = rev)) }
barres <- function(data, vars, file, title="", labels, color=c(), rev_color = FALSE, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T, margin_r=0, margin_l=NA, online=FALSE, export_xls = F,
                   display_values=T, thin=T, legend_x=NA, show_ticks=T, xrange=NA, save = FALSE, df=e, miss=T, weights = T, fr=F, rev=T, grouped = F, error_margin = F, color_margin = '#00000033', N = NA) {
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  if (missing(data) & !missing(vars)) {
    data <- dataKN(vars, data=df, miss=miss, weights = weights, return = "", fr=fr, rev=rev)
    N <- dataN(vars[1], data=df, miss=miss, weights = weights, return = "N")
    if (missing(legend) & missing(hover)) { 
      if (is.logical(df[[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights)
      else hover <- legend <- dataN(var = vars[1], data=df, miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } }
  if (length(color)==0) color <- color(data, nsp, rev_color = rev_color)
  margin_t <- 0 + 25*(!(thin))
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 15 # 10, 13
  legendY <- 1.1  + 0.3*thin/(ncol(data)-1) # last term may be problematic
  legendX <- 0.2
  # legendFont <- 'Open Sans'
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  if (max(nchar(labels)) > 25) { legendSize <- 15 } # 9, 13
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 13 # 11
    # legendY = 1.2
    legendX= -0.2 # 1
    # if (ncol(data)>1) margin_t = 170
  }
  if (!is.na(legend_x)) legendX <- legend_x
  if (!showLegend) { margin_t <- max(0, margin_t - 70) }
  if (ncol(data)==1) legendY = 1.5 + 0.3*thin
  if (sort) {
    order <- order_agree(data = data, miss = miss, rev = rev, n = length(labels))
    labels <- labels[order]
    data <- matrix(data[, order], nrow=nrow(data))
  }
  if (nrow(data)==1 & (sort | !showLegend)) {  # new: add !showLegend to manage responsable_CC i.e. comparisons of a multiple answer question
    if (!sort) order <- 1:length(labels)
    hover <- hover[order]
    value <- c()
    for (i in 1:length(hover)) { 
      hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")
      value[i] <- paste(ifelse(data[1, i] > 0.01, round(100*data[1, i]), round(100*data[1, i], 1)), '%', sep='') 
      value[i] <- paste(round(100*data[1, i]), '%', sep='') } # '%  '
    hovers <- matrix(hover, nrow=length(hover))
    values <- matrix(value, nrow=length(hover))
  }
  else {
    hovers <- values <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          values <- c(values, paste(round(100*data[i, j]/(1+data[length(hover), j])), '%', sep='')) # '%  '
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
        values <- c(values, paste(round(100*data[length(hover), j]/(1+data[length(hover), j])), '%', sep='')) # '%  '
      }
    }
    else { 
      if (is.element(hover[length(hover)],c("PNR", "PNR or other", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "PNR or other", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
          values <- c(values, paste(round(100*data[i, j]), '%', sep='')) # '%  '
        }
      }  
    }
    hovers <- matrix(hovers, ncol=length(hover))
    values <- matrix(values, ncol=length(hover))
  }
  if (!(display_values)) values <- replace(values, T, '')
  
  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = values[,1], textposition = 'auto', 
                  error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[1,]*(1-data[1,])/(N-1)), color = color_margin), # sort=FALSE, 
                  hoverinfo = hovers[,1], name=legend[1], marker = list(color = color[1], line = list(color = 'white'))) %>% # , width = 0
    
    layout(xaxis = list(title = "",
                        showgrid = show_ticks,
                        showline = FALSE,
                        showticklabels = show_ticks,
                        gridcolor = toRGB("gray70"), # + noir, + proche de 0
                        gridwidth = 1,
                        griddash = "dot",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        ticklen = 5*show_ticks,
                        tickwidth = 1,
                        tickcolor = toRGB("gray70"),
                        zeroline = T, 
                        range = xrange,
                        domain = c(0.01 + 0.14*(!(" " %in% labels)), 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 # automargin = T,
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = ifelse(grouped, 'group', 'stack'),
    title = list(text = title, font = list(color = 'black')),
    # title = title,
    # titlefont = list(color='black'),
    font = list(color='black', size=legendSize-1),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 24, autoexpand = thin), # 21, autoexpand=FALSE removes useless margin at bottom but creates bug with legend
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, traceorder='normal', font=list(size=legendSize, color='black')), # family='Balto',  , family=legendFont
    # showlegend = (showLegend & !((("Yes" %in% legend) | ("Oui" %in% legend)) & (length(legend)<4)))) %>% 
    showlegend = (showLegend & !(setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP'))))) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') # %>%
  # Legend in the Yes/No case
  if ((setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')))) { 
    bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                                     x = c(0.1, 0.9, 1.1),
                                     y = 1.5,
                                     text = legend,
                                     font = list(family = 'Arial', size = 16, color = 'black'),
                                     showarrow = FALSE) } # %>%
  # print(nrow(data))
  # print(hover)
  # print(nrow(hovers))
  # print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) { # evaluate=TRUE, 
    bars <- add_trace(bars, x = data[i,], name=legend[i], text = values[,i], hoverinfo = 'text', hovertext = hovers[,i], marker = list(color = color[i]), 
                      error_x = list(visible = error_margin, array=qnorm(1-0.05/2)*sqrt(data[i,]*(1-data[i,])/(N-1)), color = color_margin)) # width thickness (in px)
  } } # /!\ When data and vars are not provided, N cannot be computed, but error_margin=T still returns a (zero) confidence interval
  if (online) { api_create(bars, filename=file, sharing="public") }
  if (!missing(file) & save) save_plotly(bars, filename = file) # new
  if (export_xls) {
    table <- as.data.frame(data, row.names = legend)
    names(table) <- labels
    return(table) }
  else return(bars)
}
# plot(1:3,1:3) # example
# dev.copy(png, filename="test.png") # save plot from R (not plotly)
# dev.off()
# orca(example, file = "image.png") # BEST METHOD, cf. below
fig_height <- function(nb_bars, large = F) return(ifelse(nb_bars == 1, 140, 220 + 30*(nb_bars - 2)) + 10*nb_bars*large) # 2 ~ 220, 3 ~ 250, 4 ~ 280, 5 ~ 325, 6 ~ 360, 7 ~ 380, TRUE ~ 400 # 2 ~ 200-240, 3 ~ 240-275, 4 ~ 270-340, 5 ~ 320-340, 6 ~ 400, 7 ~ 340-430, 
save_plot <- function(plot=NULL, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='dev', trim = T) {
  if (class(plot) %in% c("data.frame", "array")) {
    # file <- paste(folder, "xls/", filename, ".xlsx", sep='')
    file <- paste(sub("figures", "xlsx", folder), filename, ".xlsx", sep='')
    write.xlsx(plot, file, row.names = T)  
  } else {
    file <- paste(folder, filename, ".png", sep='')
    # print(file)
    if (grepl('dev', method)) { 
      dev.copy(png, filename=file, width = width, height = height) # save plot from R (not plotly)
      dev.off() }
    else {
      server <- orca_serve() # doesn't work within a function because requires admin rights
      server$export(plot, file = file, width = width, height = height)
      server$close()
    }
    if (trim) image_write(image_trim(image_read(file)), file) }
}
save_plotly <- function(plot, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='orca', trim = T) {
  if (class(plot)=="data.frame") {
    # file <- paste(folder, "xls/", filename, ".xlsx", sep='')
    file <- paste(sub("figures", "xlsx", folder), filename, ".xlsx", sep='')
    write.xlsx(plot, file, row.names = T)  
  } else {
    file <- paste(folder, filename, ".png", sep='')
    # print(file)
    if (grepl('webshot', method)) { # four times faster: 2.5s (vs. 10s) but saves useless widgets and doesn't exactly respect the display
      saveWidget(plot, 'temp.html')
      webshot('temp.html', file, delay = 0.1, vwidth = width, vheight = height)  
      file.remove('temp.html')}
    else orca(plot, file = file, width = width, height = height) # bug with encoding in Windows
    # else {
    #   server <- orca_serve() # doesn't work within a function because requires admin rights
    #   server$export(plot, file = file, width = width, height = height)
    #   server$close()
    # }
    if (trim) image_write(image_trim(image_read(file)), file) }
}
correlogram <- function(grep = NULL, vars = NULL, df = e) {
  if (missing(vars)) vars <- names(df)[grepl(grep, names(df)) & !grepl("_funding|_correct", names(df))]
  data <- df[,vars]
  names(data) <- vars
  corr <- cor(data, use="complete.obs")
  p.mat <- cor.mtest(data) # corrplot does not work when some packages are loaded before 'corrplot' => if it doesn't work, restart R and load only corrplot.
  corrplot(corr, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T, type='upper') #, order='hclust'
}
heatmap_plot <- function(data, type = "full", p.mat = NULL, proportion = T) { # type in full, upper, lower
  diag <- if(type=="full") T else F
  # color_lims <- if(proportion) c(0,1) else { if (min(data)>=2 & max(data)<= 2) c(-2,2) else c(min(0, data), max(data)) }
  color_lims <- if(proportion) c(0,1) else { if (min(data, na.rm=T)>=2 & max(data, na.rm=T)<= 2) c(-2,2) else c(min(0, data, na.rm=T), max(data, na.rm=T)) }
  nb_digits <- if(proportion) 0 else 1
  # color2 <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
  # col <- colorRampPalette(color2)(200)
  # # if (proportion) col <- colorRampPalette(c(rep("#67001F", 10), col2))(200)
  par(xpd=TRUE)
  return(corrplot(data, method='color', col = COL2('RdYlBu'), tl.cex = 1.3, na.label = "NA", number.cex = 1.3, mar = c(1,1,1.3,1.1), cl.pos = 'n', col.lim = color_lims, number.digits = nb_digits, p.mat = p.mat, sig.level = 0.01, diag=diag, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = proportion, type=type, is.corr = F) ) #  cl.pos = 'n' removes the scale # cex
}
heatmap_table <- function(vars, labels = vars, data = all, along = "country_name", special = c(), conditions = c("> 0"), on_control = T, alphabetical = FALSE) {
  # The condition must work with the form: "data$var cond", e.g. "> 0", "%in% c('a', 'b')" work
  e <- data
  if (on_control) e <- e[e$treatment=="None",]
  if (along == "country_name" & !alphabetical & exists("countries_names")) {
    levels <- c()
    for (l in countries_names) if (l %in% Levels(e[[along]])) levels <- c(levels, l)
  } else levels <- Levels(e[[along]])
  nb_vars <- length(vars)
  
  if (length(conditions)==1) conditions <- rep(conditions[1], nb_vars)
  up_labels <- c(special, levels)
  if (any(c('non-OECD', 'Non-OECD', 'non-oecd') %in% special) & levels[15]=="Brazil") up_labels <- c(special[!special %in% c('non-OECD', 'Non-OECD', 'non-oecd')], levels[1:14], "Non-OECD", levels[15:length(levels)])
  table <- array(NA, dim = c(nb_vars, length(c(special, levels))), dimnames = list(vars, up_labels))
  for (c in up_labels) {
    if (c %in% levels) { df_c <- e[e[[along]]==c,]
    } else if (c %in% c('World', 'world', 'total', 'all')) { df_c <- e 
    } else if (c %in% c('OECD', 'oecd')) { df_c <- e[which(oecd[e$country]),]
    } else if (c %in% c('non-OECD', 'Non-OECD', 'non-oecd')) { df_c <- e[which(!oecd[e$country]),]
    } else if (c %in% countries) { df_c <- e[e$country == c,] 
    } else if (c %in% countries_names) { df_c <- e[e$country_name == c,] }
    for (v in 1:nb_vars) {
      var_c <- df_c[[vars[v]]][!is.na(df_c[[vars[v]]])]
      table[v,c] <- eval(str2expression(paste("wtd.mean(var_c", conditions[v], ", na.rm = T, weights = df_c$weight[!is.na(df_c[[vars[v]]])])"))) 
    }
  }
  row.names(table) <- labels
  return(table)
}
heatmap_wrapper <- function(vars, labels = vars, name = deparse(substitute(vars)), along = "country_name", labels_along = NULL, special = c(), conditions = c("> 0"), df = all, width = NULL, height = NULL, alphabetical = T, on_control = T) {
  # width: 1770 to see Ukraine (for 20 countries), 1460 to see longest label (for 20 countries), 800 for four countries.
  # alternative solution to see Ukraine/labels: reduce height (e.g. width=1000, height=240 for 5 rows). Font is larger but picture of lower quality / more pixelized.
  # Longest label: "Richest countries should pay even more to help vulnerable ones" (62 characters, variables_burden_sharing_few). 
  # special can be c("World", "OECD")
  if (is.null(width)) width <- ifelse(length(labels) <= 3, 1000, ifelse(length(labels) <= 8, 1550, 1770)) # TODO! more precise than <= 3 vs. > 3
  if (is.null(height)) height <- ifelse(length(labels) <= 3, 163, ifelse(length(labels) <= 8, 400, 600))
  
  for (cond in conditions) {
    filename <- paste(sub("variables_", "", name), 
                      case_when(cond == "" ~ "mean", 
                                cond == "> 0" ~ "positive", 
                                cond == ">= 1" ~ "positive", 
                                cond == "< 0" ~ "negative", 
                                cond == "<= -1" ~ "negative", 
                                cond == ">= 0" ~ "non-negative", 
                                cond == "<= 0" ~ "non-positive", 
                                cond == "== 2" ~ "max", 
                                cond == "== -2" ~ "min", 
                                cond == "-" ~ "difference",
                                cond == "/" ~ "share",
                                TRUE ~ "unknown"), sep = "_")
    try({
      if (cond %in% c("/", "-")) {
        pos <- heatmap_table(vars = vars, labels = labels, data = df, along = along, special = special, conditions = ">= 1", on_control = on_control, alphabetical = alphabetical)
        neg <- heatmap_table(vars = vars, labels = labels, data = df, along = along, special = special, conditions = "<= -1", on_control = on_control, alphabetical = alphabetical)
        if (cond == "-") temp <- pos - neg else temp <- pos / (pos + neg)
        for (i in 1:length(vars)) if (is.logical(df[[vars[i]]])) temp[i, ] <- pos[i, ]
      } else {  temp <- heatmap_table(vars = vars, labels = labels, data = df, along = along, special = special, conditions = cond, on_control = on_control, alphabetical = alphabetical) }
      if (!missing(labels_along) & length(labels_along) == ncol(temp)) colnames(temp) <- labels_along
      heatmap_plot(temp, proportion = (cond != ""))
      save_plot(filename = paste0(folder, filename, replacement_text), width = width, height = height)
    })
  }
}

##### Other #####
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) }

# from http://pcwww.liv.ac.uk/~william/R/crosstab.r http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
Crosstab <- function (..., dec.places = NULL, type = NULL, style = "wide", row.vars = NULL, col.vars = NULL, percentages = TRUE,  addmargins = TRUE, subtotals=TRUE) {
  #Declare function used to convert frequency counts into relevant type of proportion or percentage
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars), 
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }
  
  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  
  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))    
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)
  
  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types
  
  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables
  
  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct  
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count  
    type <- "frequency"
  }
  
  
  
  #Check for integrity of requested analysis and adjust values of function arguments as required
  
  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }
  
  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }
  
  if ((length(type)>1) & (subtotals==FALSE)) { 
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }
  
  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }
  
  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE)) 
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")
  
  
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, Crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }
  
  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }
  
  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'
  
  args <- list(...)    
  
  if (length(args) > 1) {
    if (!all(sapply(args, is.factor))) 
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate Crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages", 
                    "addmargins", "subtotals")) if (is.null(get(opt))) 
                      assign(opt, eval(parse(text = paste("tbl$", opt, 
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }
  
  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))
  
  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }
  
  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (Crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency") 
    Crosstab <- tbl
  else 
    Crosstab <- mk.pcnt.tbl(tbl, type[1])
  
  
  #If multiple table types requested, create and add these to 
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(Crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency") 
        Crosstab <- tbl
      else Crosstab <- mk.pcnt.tbl(tbl, type[i])
      Crosstab <- as.data.frame.table(Crosstab)
      Crosstab[z] <- i
      tbldat <- rbind(tbldat, Crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    Crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(Crosstab))[z - 1] <- ""
  }
  
  
  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {
    
    vars <- c(row.vars,col.vars)
    
    if (length(type)==1) {
      if (type=="row.pct") 
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else 
      { if (type=="column.pct") 
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else 
        { if (type=="joint.pct") 
        { Crosstab <- addmargins(Crosstab,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        }
          else #must be total.pct OR frequency
          { Crosstab <- addmargins(Crosstab)
          tbl <- addmargins(tbl)
          }
        }
      } 
    }
    
    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }
    
  }  
  
  
  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    
    #Create version of Crosstab in ftable format
    t1 <- Crosstab 
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)
    
    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)
    
    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))
    
    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]    
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]
    
    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals) 
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]  
      }
    }
    
    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals
    
    t1 <- t1[((lab==0) | (lab==n.row.vars)),]
    
    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""
    
    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL
    
  }
  
  
  
  #Create output object 'result' [class: Crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals
  
  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$Crosstab <- Crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]  
  result$Crosstab.nosub <- t1  #Crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]  
  class(result) <- "Crosstab"    
  
  #Return 'result' as output of function
  result
  
}

print.Crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {
  
  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  if (length(x$type)>1) {
    z<-length(names(dimnames(x$Crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z) 
    } else {
      col.vars<-c(z,col.vars)
    }
  }
  
  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$Crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$Crosstab,x$dec.places))
    }
  }
  
  
  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$Crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {
    
    tbl <- ftable(x$Crosstab,row.vars=row.vars,col.vars=col.vars)
    
    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)
    
  }
  
  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$Crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {
    
    t1 <- x$Crosstab.nosub
    
    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))
    
    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])
    
    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }
    
    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)
    
  }
  
}

inflate_for_miss <- function(v) return(c(v[1:(length(v)-1)]/(1-v[length(v)]), v[length(v)]))

close <- function(x, y, prec = 0.0001) return(all(abs(x - y) < prec))

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
rquery.wordcloud <- function(x, type=c("text", "url", "file"), lang="english", excludeWords=NULL, 
                            textStemming=FALSE,  colorPalette="Dark2", min.freq=3, max.words=200) { 
  # http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0, #0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}

plot_world_map <- function(var, condition = "> 0", df = all, on_control = T, save = T, continuous = FALSE, width = dev.size('px')[1], height = dev.size('px')[2]) {
  table <- heatmap_table(vars = var, data = df, along = "country", conditions = c(condition), on_control = T)
  df_countries <- c(Country_Names[colnames(table)], "Wallis and Futuna", "Vatican", "Tobago", "Trinidad", "Sint Maarten", "Liechtenstein", "Saint Kitts", "Nevis", "Monaco", "Jersey", "Barbuda", "Antigua", "Saint Barthelemy", "Reunion", "Grenadines", "Virgin Islands", "Turks and Caicos Islands", "Saint Pierre and Miquelon", "Saint Helena", "Ascension Island", "Niue", "Palau", "Pitcairn Islands", "South Sandwich Islands")
  df <- data.frame(country = df_countries, mean = c(as.vector(table), seq(-1.84, 1.94, 0.2), seq(0.06, 0.86, 0.2)))
  
  if (condition != "") {
    breaks <- c(-Inf, .2, .35, .5, .65, .8, Inf)
    labels <- c("0-20%", "20-35%", "35-50%", "50-65%", "65-80%", "80-100%")
    legend <- paste("Share", condition)
    limits <- c(0, 1)
  } else {
    breaks <- c(-Inf, -1.2, -.8, -.4, 0, .4, .8, 1.2, Inf) # c(-Inf, -1, -.5, -.25, 0, .25, .5, 1, Inf)
    labels <- c("< -1.2", "-1.2 - -0.8", "-0.8 - -0.4", "-0.4 - 0", "0 - 0.4", "0.4 - 0.8", "0.8 - 1.2", "> 1.2")
    legend <- "Mean"
    limits <- c(-2, 2)
  }
  
  world_map <- map_data(map = "world")
  world_map <- world_map[world_map$region != "Antarctica",]
  # world_map$region <- iso.alpha(world_map$region)
  
  df_na <- data.frame(country = setdiff(world_map$region, df_countries), mean = NA)
  df <- merge(df, df_na, all = T)
  
  df$group <- cut(df$mean, breaks = breaks, labels = labels)
  
  if (!continuous) {
    (plot <- ggplot(df) + geom_map(aes(map_id = country, fill = fct_rev(group)), map = world_map) + #geom_sf() + # coord_proj("+proj=eck4") + #devtools::install_github("eliocamp/ggalt@new-coord-proj")
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + coord_fixed() +
    scale_fill_manual(name = legend, values = color(length(breaks)-1), na.value = "grey50")) # +proj=eck4 +proj=wintri
  } else {
    (plot <- ggplot(df) + geom_map(aes(map_id = country, fill = mean), map = world_map) + #geom_sf() + # coord_proj("+proj=eck4") + #devtools::install_github("eliocamp/ggalt@new-coord-proj")
          geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + coord_fixed() +
        scale_fill_distiller(palette = "RdBu", direction = 1, limits = limits, na.value = "grey50")) #scale_fill_viridis_c(option = "plasma", trans = "sqrt"))
  }
  
  if (save) save_plot(plot, filename = ifelse(continuous, paste0(var, "_cont"), var), folder = '../figures/maps/', width = width, height = height)
  # return(plot)
}

##### Plot along #####
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
    if ("double.item" %in% class(df[[y]])) dependent_var <- paste0("as.numeric(", y, ")") else dependent_var <- y
    formula <- as.formula(paste(dependent_var, " ~ ", paste(covariates, collapse = ' + ')))
    i <- i + 1
    if (logit[i]) {
      if (logit_margin) {
        reg <- logitmfx(formula, data = df, atmean = atmean) #$mfxest 
        if (summary) reg <- reg$mfxest
      } else reg <- glm(formula, family = binomial(link='logit'), data = df, weights = weight)
    } else reg <- lm(formula, data = df, weights = weight)
    if (summary) reg <- summary(reg)
    regs <- c(regs, list(reg))
  }
  return(regs)
}

# Given a set of regressions with one common variable (along), gives the coefs and CI of the levels of that variable.
mean_ci_along_regressions <- function(regs, along, labels, df = e, origin = 'others_at_mean', logit = c(FALSE), logit_margin = T, confidence = 0.95, factor_along = FALSE,
                                      subsamples = NULL, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), weight = 'weight') { # to handle numeric variables: levels_along = ifelse(is.numeric(df[[along]]), c(), Levels(df[[along]]))
  # names_levels[1] should correspond to the control group (concatenation of along and the omitted level)
  # origin can be 0, 'intercept': the intercept, the 'control_mean': true (or predicted) mean of the control group, or 'others_at_mean': all variables at their mean except along (at 0 i.e. the control group)
  # TODO: logit, origin
  if (factor_along) names_levels <- paste0("as.factor(", along, ")", levels_along)
  k <- length(names_levels)
  if (k < 2) warning("along must have several levels.")
  if (length(levels_along)!=length(names_levels)) warning("levels_along and names_levels must have same length.")
  mean_ci <- data.frame()
  i <- 0
  if (class(regs) != "list") regs <- list(regs)
  if (length(logit)==1) logit <- rep(logit, length(regs)) # TODO determine automatically whether logit through class(regs)
  for (r in regs) {
    i <- i+1
    label <- rep(labels[i], length(levels_along))
    if (!is.null(subsamples)) data_s <- df[df[[subsamples]] == Levels(df[[subsamples]])[i],] else data_s <- df
    if (!("weight" %in% names(data_s))) data_s$weight <- 1
    if (logit[i] & logit_margin) {
      regmxf <- r$mfxest
      reg <- r$fit }
    else reg <- r
    if (origin == 'intercept') origin_value <- reg$coefficients[["(Intercept)"]]
    else if (origin == 'others_at_mean') {
      if (names(reg$coefficients)[1] == "(Intercept)") origin_value <- reg$coefficients[["(Intercept)"]]
      else origin_value <- 0
      variables <- names(reg$model)[2:length(names(reg$model))]
      for (v in variables) {
        if (v %in% names(reg$xlevels) & v != along) for (j in reg$xlevels[[v]][2:length(reg$xlevels[[v]])]) {
          name_var <- sub("^as\\.factor\\((.*)\\)$", "\\1", "as.factor(income)")
          if (!name_var %in% names(data_s)) warning(paste(name_var, "not in names(df): its coefficient set to 0 to compute the origin value."))
          else origin_value <- origin_value + reg$coefficients[[paste0(v, j)]] * wtd.mean(data_s[[name_var]] == j, weights = data_s$weight, na.rm = T) }
        else if (v %in% names(reg$coefficients) & !(v %in% c(along, "(weights)"))) origin_value <- origin_value + reg$coefficients[[v]] * wtd.mean(data_s[[v]], weights = data_s$weight, na.rm = T)
      }
      if (logit[i]) origin_value <- 1/(1+exp(-origin_value)) # cf. https://www.princeton.edu/~otorres/LogitR101.pdf for an alternative coding
    } else if (origin == 'control_mean') {
      dependent_var <- if (logit[i] & logit_margin) reg$y else reg$model[[1]]
      origin_value <- wtd.mean(dependent_var[data_s[[along]] == levels_along[1]], weights = data_s$weight[data_s[[along]] == levels_along[1]])
    } else origin_value <- 0
    if (logit[i] & logit_margin) { # logit margins
      # if ("lm" %in% class(reg)) warning("Logit margins should be provided: logitmfx(..)")
      coefs <- origin_value + c(0, regmxf[names_levels[2:k],1])
      
      # SEs <- c(0, regmxf[names_levels[2:k],2])
      # z <- qnorm(1-(1-confidence)/2)
      # CI <- cbind(coefs - z*SEs, coefs + z*SEs) # CIs approximated using Standard Errors of logitmargin(...)$mfxest. Pb: no CI for omitted variable.
      
      n <- length(reg$fitted.values)
      t <- qt(1-(1-confidence)/2, n)
      sigma <- sqrt(wtd.mean((reg$y - reg$fitted.values)^2)) #, weights = data_s$weight)) TODO: handle weight for logit_margin (uncommenting this would only work when there is no missing value so that length(data_s$weight)==length(reg$y))
      SDs <- sapply(levels_along, function(i) return(sqrt(wtd.mean(((data_s[[along]] == i) - wtd.mean(data_s[[along]] == i, weights = data_s$weight, na.rm = T))^2, weights = data_s$weight, na.rm = T))))
      CI <- cbind(coefs - t*sqrt(1/n)*sigma/SDs, coefs + t*sqrt(1/n)*sigma/SDs) # CIs approximated as if the results were that of a linear regression. Stata uses a more appropriate method: the Delta method (which also have some issues: CIs can be outside [0; 1]), not easily implementable in R (despite msm::deltamethod)
    } else { # OLS
      if (logit[i]) warning("Are you sure you want the logit coefficients rather than the marginal effects? If not, set logit_margin = T.")
      #   mean_ci_origin <- binconf(x = sum(data_s[[weight]][data_s[[along]] == levels_along[1]], na.rm=T), n = sum(data_s[[weight]], na.rm=T), alpha = 1-confidence) # WRONG! This is the CI for the group mean, not the CI for the regression coefficient!
      #   CI_origin <- mean_ci_origin[2:3] - (origin_value == 0) * mean_ci_origin[1] # c(0, 0) # TODO! check that this confidence interval at the origin is correct (I doubt it)
      n <- length(reg$fitted.values)
      t <- qt(1-(1-confidence)/2, n)
      if (!("weight" %in% names(data_s))) data_s$weight <- 1
      if (!("weights" %in% names(reg))) reg$weights <- 1
      sigma <- sqrt(wtd.mean((reg$model[[1]] - reg$fitted.values)^2, weights = reg$weights))
      SD <- sqrt(wtd.mean(((data_s[[along]] == levels_along[1]) - wtd.mean(data_s[[along]] == levels_along[1], weights = data_s$weight, na.rm = T))^2, weights = data_s$weight, na.rm = T))
      CI_origin <- c(-1, 1)*t*sqrt(1/n)*sigma/SD # This computation is very close to confint(...), which we can't use for the omitted variable.
      coefs <- origin_value + c(0, reg$coefficients[names_levels[2:k]]) # what about emmeans(reg, ~ 1 | along) to compute e.g. CI_origin?
      # CI <- origin_value + rbind(CI_origin, confint(reg, names_levels[2:k], confidence)) # if simple OLS
      robust_SEs <- coeftest(reg, vcov = vcovHC(reg, "HC1")) # another way to get (non-robust) SEs is summary(reg)$coefficients[,2]
      CI <- origin_value + rbind(CI_origin, cbind(robust_SEs[names_levels[2:k], 1] - t*robust_SEs[names_levels[2:k], 2], robust_SEs[names_levels[2:k], 1] + t*robust_SEs[names_levels[2:k], 2]))
    }
    mean_ci_reg <- data.frame(y = label, mean = coefs, CI_low = CI[,1], CI_high = CI[,2], along = levels_along)
    mean_ci <- rbind(mean_ci, mean_ci_reg)    
  }
  row.names(mean_ci) <- NULL
  return(mean_ci)
}


# Two cases: with covariates (coefficients or marginal effects are shown, depending on origin = 0 or not) or without covariates (i.e. unconditional means of subgroups)
# Three configurations: a. one outcomes, two heterogeneities / b. and c. different outcomes, one heterogeneity (c. is invert_y_along = T)
# a. one outcome, y: subsamples (e.g. countries), along: heterogeneity; b. y: outcomes, along; c. y: heterogeneity, along: outcomes
# For numerical outcomes (i.e. not dummies), set conditions to rep("", length(outcomes))
mean_ci <- function(along, outcome_vars = outcomes, outcomes = paste0(outcome_vars, conditions), covariates = NULL, subsamples = NULL, conditions = c(" > 0"), invert_y_along = FALSE, df = e, labels = outcome_vars, factor_along = FALSE,
                    origin = 'others_at_mean', logit = c(FALSE), weight = 'weight', atmean = T, logit_margin = T, confidence = 0.95, 
                    labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), heterogeneity_condition = "", order_y = NULL, order_along = NULL) {
  z <- qnorm(1-(1-confidence)/2)
  if ((labels_along == levels_along) & is.logical(df[[along]])) { labels_along <- paste0(along, ": ", levels_along) }
  names(labels_along) <- as.character(levels_along) # setNames(levels_along, levels_along)
  if (!is.null(covariates)) { # If conditional (regressions)
    if (!(along %in% covariates)) print("ERROR: along must be in covariates")
    if (any(logit) & !logit_margin) print("Warning: Are you sure you want the logit coefficients rather than the marginal effects? If not, set logit_margin = T.")
    if (!is.null(subsamples) & (missing(labels) | labels == outcome_vars)) labels <- Levels(df[[subsamples]])
    regs <- regressions_list(outcomes = outcomes, covariates = covariates, subsamples = subsamples, df = df, logit = logit, weight = weight, atmean = atmean, logit_margin = logit_margin, summary = FALSE)
    mean_ci <- mean_ci_along_regressions(regs = regs, along = along, labels = labels, df = df, origin = origin, logit = logit, logit_margin = logit_margin, confidence = confidence, subsamples = subsamples, names_levels = names_levels, levels_along = levels_along, factor_along = factor_along, weight = weight)
  } else { # If unconditional (subgroup means)
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
      mean_ci_reg <- as.data.frame(t(apply(mean_ci_reg, 2, function(x) c(x[1],x[1]-z*x[2], x[1]+z*x[2])))) # /!\ This is a CI for a normal distribution, only an approximation in cases of binomial distributions
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
  
  mean_ci$along <- labels_along[as.character(mean_ci$along)]
  if (exists("countries_names")) {
    if (Levels(mean_ci$along)==sort(countries_names)) mean_ci$along <- factor(mean_ci$along, levels = countries_names)
    if (Levels(mean_ci$y)==sort(countries_names)) mean_ci$y <- factor(mean_ci$y, levels = rev(countries_names))
  }
  if (!missing(order_y)) if (sort(Levels(mean_ci$y))==sort(order_y)) mean_ci$y <- factor(mean_ci$y, levels = order_y)
  if (!missing(order_along)) if (sort(Levels(mean_ci$along))==sort(order_along)) mean_ci$along <- factor(mean_ci$along, levels = order_along)
  return(mean_ci)
}

plot_along <- function(along, mean_ci = NULL, vars = outcomes, outcomes = paste0(vars, conditions), covariates = NULL, subsamples = NULL, conditions = c(" > 0"), invert_y_along = FALSE, df = e, labels = vars, factor_along = FALSE,
                       origin = 'others_at_mean', logit = c(FALSE), atmean = T, logit_margin = T, labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]), 
                       confidence = 0.95, weight = "weight", heterogeneity_condition = "", return_mean_ci = FALSE, print_name = FALSE, # condition = "> 0", #country_heterogeneity = FALSE, along_labels,
                       legend_x = '', legend_y = '', plot_origin_line = FALSE, name = NULL, folder = '../figures/country_comparison/', width = dev.size('px')[1], height = dev.size('px')[2], save = T, order_y = NULL, order_along = NULL) {
  # TODO multiple conditions, show legend for 20 countries (display UA!) even if there is less than 4 variables
  # TODO: automatic values when missing(legend_x), legend_y
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
  } else if (missing(name)) name <- "temp"
  name <- sub("rev(", "", sub(")", "", sub("country_name", "country", name, fixed = T), fixed = T), fixed = T)
  if (print_name) print(name) # TODO: name with subsamples
  
  if (missing(folder) & deparse(substitute(df)) %in% tolower(countries)) folder <- paste0("../figures/", toupper(deparse(substitute(df))), "/")
  
  if (missing(mean_ci)) mean_ci <- mean_ci(along = along, outcome_vars = vars, outcomes = outcomes, covariates = covariates, subsamples = subsamples, conditions = conditions, invert_y_along = invert_y_along, df = df, labels = labels, factor_along = factor_along,
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
  
  if (plot_origin_line) {
    origins <- mean_ci$mean[mean_ci$along == levels_along[1]]
    names(origins) <- mean_ci$y[mean_ci$along == names_levels[1]]
  } else origins <- c()
  
  plot <- ggplot(mean_ci) + sapply(origins, function(xint) geom_vline(aes(xintercept = xint), linetype = "longdash", color = "grey")) + # For plot, we need mean_ci (cols: mean, CI_low,high, variable, along), legend_x, legend_y. For save, we need: name, folder, width, height.
    geom_pointrange( aes(x = mean, y = y, color = along, xmin = CI_low, xmax = CI_high), position = position_dodge(width = .5)) +
    labs(x = legend_x, y = legend_y, color="") + theme(legend.title = element_blank(), legend.position = "top") +
    theme_minimal() # + scale_color_manual(values = color(length(levels_along), theme='rainbow')) # can be theme = 'rainbow', 'RdBu', 'default' or any brewer theme, but the issue with RdBu/default is that the middle one is white for odd number of categories
  # scale_color_manual(labels = Levels(df[[along]]), values = color(length(Levels(df[[along]])), theme='rainbow'))# BUG when we specify labels: the legend does not correspond to the colors
  plot
  if (save) save_plotly(plot, filename = name, folder = folder, width = width, height = height, trim = T)
  if (return_mean_ci) return(mean_ci)
  else return(plot)
}


# var_to_decompose and group_of_interest: you need to input only one variable as a character
# controls and indices, can be a character vector
# Factor variables from control need to be in controls_factor
gelbach_decomposition <- function(var_to_decompose, group_of_interest, controls, controls_factor, indices, df=e, weights = "weight") {
  # We restrict the df to the variables we'll use, since there can be some incompatibilities
  # in using R dataframes in Stata
  df <- df %>%
    select(c(var_to_decompose, group_of_interest, controls, controls_factor, indices, weights))
  
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
  stata_cmd[12] <- paste("global local_weight [aw=", paste(weights), paste("]"), sep = "")
  stata_cmd[13] <- "do gelbach_stata.do"
  
  stata_cmd <- paste(stata_cmd, collapse = "\n")
  # We input df, and obtain the data frame with the share explained by each indice
  final <- stata(stata_cmd, data.in = df, data.out = T)
  
  return(final)
}
