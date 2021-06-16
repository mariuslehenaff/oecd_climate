library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("xlsx")

# Sources
# data (code insee/postal): https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/table/
# data2 (population): https://www.insee.fr/fr/information/2115011 (1 January 2020)
# data3 (aire urbaines): https://www.insee.fr/fr/information/2115011 sheet "ARM" + Wikipedia for population

# Population and code postal
data <-read.csv("../data/zipcodes/correspondance-code-insee-code-postal.csv", sep=';')
# Aires Urbaines (AU) and classification
data2<- read.csv2(file="../data/zipcodes/AU2020.csv", encoding = "UTF-8")

data <- data %>% 
  select(Code.INSEE, Code.Postal, Population) %>%  
  rename(CODGEO =  Code.INSEE, Code_Postal= Code.Postal, Population_commune = Population)

data2 <- merge(data2, data[,c("CODGEO", "Code_Postal", "Population_commune")], by="CODGEO")
data2<-select(data2, LIBGEO, LIBAU2010, CODGEO, Code_Postal, Population_commune, CATAEU2010)
# data2$Population_commune<-as.character(data2$Population_commune)
# data2$CATAEU2010<-as.character(data2$CATAEU2010)

# Data for Paris, Lyon, Marseille (missing in data2)
data3<- read.xlsx(file="../data/zipcodes/AU2020_2.xlsx", sheetIndex=1, header=TRUE)
data3[data3$LIBAU2010=="Marseille",2] <- "Marseille - Aix-en-Provence"
data3$Code_Postal <- as.character(data3$Code_Postal)

data2 <- bind_rows(data2, data3)
# data2$Population_commune <- as.double(data2$Population_commune)
# data2$CATAEU2010 <- as.integer(data2$CATAEU2010)

# Get population of AUs by summing the populations of communes within the same AU
Pop_AU<-data2 %>%
  group_by(LIBAU2010) %>%
  summarise_at(dplyr::vars(Population_commune),
    list(Population_AU = sum))

data2 <- merge(data2, Pop_AU[,c("Population_AU", "LIBAU2010")], by="LIBAU2010")
data2 <- data2 %>% rename(Aire_Urbaine =  LIBAU2010, Commune = LIBGEO, Code_INSEE = CODGEO, Zone_INSEE = CATAEU2010)

Pop_CP <- data2 %>%  group_by(Code_Postal) %>%  summarise_at(dplyr::vars(Population_commune), list(Pop_CP = sum))
data2 <- merge(data2, Pop_CP[,c("Pop_CP", "Code_Postal")], by="Code_Postal")
# sum(Pop_CP$Pop_CP)==sum(data2$Population_commune)
# data2 <- data2[,which(names(data2) != "Pop_CP")]

data2 <- data2 %>%
  mutate(Zone_GP = case_when(
    Zone_INSEE == 111 ~ "GP",
    Zone_INSEE == 112 ~ "Couronne_GP",
    TRUE ~ "Other"
  ))

data2$Pop_GP <- data2$Population_commune * (data2$Zone_GP == "GP")
data2$Pop_other <- data2$Population_commune * (data2$Zone_GP == "Other")
data2$Pop_Couronne_GP <- data2$Population_commune * (data2$Zone_GP == "Couronne_GP")
frac_GP <- data2 %>%  group_by(Code_Postal) %>%  summarise_at(dplyr::vars(Pop_GP), list(frac_GP = sum))
frac_Couronne_GP <- data2 %>%  group_by(Code_Postal) %>%  summarise_at(dplyr::vars(Pop_Couronne_GP), list(frac_Couronne_GP = sum))
frac_other <- data2 %>%  group_by(Code_Postal) %>%  summarise_at(dplyr::vars(Pop_other), list(frac_other = sum))
data2 <- merge(data2, frac_GP[,c("frac_GP", "Code_Postal")], by="Code_Postal")
data2 <- merge(data2, frac_Couronne_GP[,c("frac_Couronne_GP", "Code_Postal")], by="Code_Postal")
data2 <- merge(data2, frac_other[,c("frac_other", "Code_Postal")], by="Code_Postal")
data2$frac_Couronne_GP <- data2$frac_Couronne_GP/data2$Pop_CP
data2$frac_other <- data2$frac_other/data2$Pop_CP
data2$frac_GP <- data2$frac_GP/data2$Pop_CP


# Autres catégorisations possibles :
# data2 <- data2 %>%
#   mutate(Zone_Category4 = case_when(
#     Zone_INSEE %in% c(111, 211, 221)  ~ "Pôle",
#     Zone_INSEE %in% c(112, 212, 222) ~ "Couronne",
#     Zone_INSEE %in% c(120, 300) ~ "Multipolarisé",
#     Zone_INSEE == 400 ~ "Isolé"
#   ))
# 
# data2 <- data2 %>%
#   mutate(rural_urban = case_when(
#     Zone_INSEE %in% c(111)  ~ "Grande aire urbane",
#     Zone_INSEE %in% c(211, 221, 212, 222) ~ "Autre aire urbaine",
#     Zone_INSEE %in% c(112, 120) ~ "Péri-urbain",
#     Zone_INSEE %in% c(400, 300) ~ "Rural"
#   ))
# 
# data2 <- data2 %>%
#   mutate(Zone_Category = case_when(
#     Zone_INSEE %in% c(111, 211, 221)  ~ "Pôle",
#     Zone_INSEE %in% c(112, 212, 222) ~ "Couronne",
#     Zone_INSEE %in% c(120, 300, 400) ~ "Isolé ou multipolarisé"
#   ))
# 
# data2 <- data2 %>%
#   mutate(Grand_pole = case_when(
#     Zone_INSEE %in% c(111)  ~ TRUE,
#     TRUE ~ FALSE
#   ))

# Sépare communes avec plusieurs codes postaux 
# soit en une ligne par code postal avec les fractions du code postal dans chaque type d'aire urbaine (zones_GP_all) 
# soit en autant de lignes par codes postaux qu'il y a de types d'aire urbaine différents (zones_GP)
zones_GP <- data2 %>% select(Code_Postal, Zone_GP) %>% group_by(Code_Postal) %>% 
  summarise_at(dplyr::vars(Zone_GP), list(zone = unique))
zones_GP_all <- data2 %>% select(Code_Postal, Zone_GP, frac_GP, frac_Couronne_GP, frac_other) %>% group_by(Code_Postal) %>% 
  summarise_at(dplyr::vars(frac_GP, frac_Couronne_GP, frac_other), mean)
for (i in 1:nrow(zones_GP)) { 
  if (grepl("/", zones_GP$Code_Postal[i])) {
    code_postaux_i <- str_split(zones_GP$Code_Postal[i], "/")[[1]]
    zones_GP$Code_Postal[i] <- code_postaux_i[1]
    zones_GP <- rbind(zones_GP, as.data.frame(list("Code_Postal" = code_postaux_i[2:length(code_postaux_i)], "zone" = zones_GP$zone[i])))
  }
}
for (i in 1:nrow(zones_GP_all)) { 
  if (grepl("/", zones_GP_all$Code_Postal[i])) {
    code_postaux_i <- str_split(zones_GP_all$Code_Postal[i], "/")[[1]]
    zones_GP_all$Code_Postal[i] <- code_postaux_i[1]
    zones_GP_all <- rbind(zones_GP_all, as.data.frame(list("Code_Postal" = code_postaux_i[2:length(code_postaux_i)], 
                          "frac_GP" = zones_GP_all$frac_GP[i], "frac_Couronne_GP" = zones_GP_all$frac_Couronne_GP[i], "frac_other" = zones_GP_all$frac_other[i])))
  }
}
shareCP_GP <- zones_GP_all$frac_GP
shareCP_Couronne_GP <- zones_GP_all$frac_Couronne_GP
shareCP_Other <- zones_GP_all$frac_other
names(shareCP_GP) <- names(shareCP_Couronne_GP) <- names(shareCP_Other) <- as.character(zones_GP_all$Code_Postal)

(Pop_cat <- data2 %>%
    group_by(Zone_GP) %>% # Grand_pole Zone_Category Zone_Category4 Zone_GP
    summarise_at(dplyr::vars(Population_commune),
                 list(Pop_cat = sum)))

(pop_Zone_GP <- (Pop_cat <- data2 %>%
  group_by(Zone_GP) %>% # Grand_pole Zone_Category Zone_Category4 Zone_GP
  summarise_at(dplyr::vars(Population_commune),
               list(Pop_cat = sum)))$Pop_cat / 63566.16)

# /!\ Pb: 25% des codes postaux sont à cheval sur plusieurs types d'aires urbaines (et ont autant de lignes dans zones_GP ou data2)
# data2 a une ligne par commune (pour certaines communes la colonne code postal est donc de type "75012/75013")
# zones_GP n'a pas plus d'un code postal par ligne (par a des codes postaux en double ou triple quand ils recouvrent plusieurs types d'aires urbaines)
write.csv(zones_GP, "../data/zipcodes/FR_aires_urbaines_2020__5948_1836_2216.csv", row.names = FALSE, quote = FALSE)
write.csv(data2, "../data/zipcodes/FR_aires_urbaines_2020.csv", row.names = TRUE)
data2 <- read.csv("../data/zipcodes/FR_aires_urbaines_2020.csv")

nb_au <- rep(NA, length(unique(data2$Code_Postal)))
names(nb_au) <- unique(data2$Code_Postal)
for (c in unique(data2$Code_Postal)) nb_au[c] <- length(unique(data2$Zone_GP[data2$Code_Postal==c]))
decrit(nb_au)
data2$nb_au <- nb_au[data2$Code_Postal]
decrit(data2$nb_au)
data2$nb_au <- nb_au[data2$Code_Postal]
sum(data2$Population_AU[data2$nb_au==3])

e <- fr
e$nb_au <- nb_au[as.character(e$zipcode)]
decrit(e$urban_category[e$nb_au > 1])
decrit(e$urban_category[e$nb_au == 1])
e$shareCP_GP <- shareCP_GP[as.character(e$zipcode)]
e$shareCP_Couronne_GP <- shareCP_Couronne_GP[as.character(e$zipcode)]
e$shareCP_Other <- shareCP_Other[as.character(e$zipcode)]
e$error_urban_category <- (e$urban_category=="GP")*abs(e$shareCP_GP - 1*(e$urban_category=="GP")) + 
  (e$urban_category=="Couronne_GP")*abs(e$shareCP_Couronne_GP - 1*(e$urban_category=="Couronne_GP")) + 
  (e$urban_category=="Other")*abs(e$shareCP_Other - 1*(e$urban_category=="Other"))
mean(is.na(e$error_urban_category)) # 6% share of NA. Almost only the zipcodes at 4 digits (for which as.character makes it bug)
# e$zipcode[is.na(e$error_urban_category)]
mean(e$error_urban_category > 0, na.rm = T) # 22%: share where urban_category is arbitrary
mean(e$error_urban_category > .5, na.rm = T) # 8%: share where we are wrong more likely than not on urban_category type
decrit(e$error_urban_category) # 8.5%: expectancy of our error of attribution
mean(e$shareCP_GP, na.rm=T) # 62.9%
mean(e$shareCP_Couronne_GP, na.rm=T) # 18.3%
mean(e$shareCP_Other, na.rm=T) # 18.7%
decrit(e$urban_category) # 59/18/22%
fr <- e
# bon, j'ai recodé en R pour chaque code postal la fraction de population de ce code postal qui est dans chaque type d'aire urbaine, et comparé à l'aire urbaine qu'on lui a alloué arbitrairement
# Y a 22% des cas où on a alloué arbitrairement
# 8% où on s'est trompé avec une proba > 50%
# et (l'espérance de) la proportion des cas où on s'est trompé est de 8.5%
# au niveau agrégé, les erreurs se compensent partiellement, et au total on sous-estime la part d'urbains de 4 p.p.

c(0.5947513 , 0.1836103, 0.2216384) # pop_zone_GP GP, Couronne_GP, Other (Pôle, Couronne_Pôle, Multipolarisé, Isolé)
## Details for CATAEU2010
# 111 : Commune appartenant à un grand pôle (10 000 emplois ou plus)
# 112 : Commune appartenant à la couronne d'un grand pôle
# 120 : Commune multipolarisée des grandes aires urbaines
# 211 : Commune appartenant à un moyen pôle (5 000 à moins de 10 000 emplois)
# 212 : Commune appartenant à la couronne d'un moyen pôle
# 221 : Commune appartenant à un petit pôle (de 1 500 à moins de 5 000 emplois)
# 222 : Commune appartenant à la couronne d'un petit pôle
# 300 : Autre commune multipolarisée
# 400 : Commune isolée hors influence des pôles
# 
# L'espace des grandes aires urbaines est composé des communes dont la modalité vaut 111, 112, ou 120.
# L'espace des autres aires urbaines et composé des communes dont la modalité vaut 211, 212, 221 ou 222.
# 
# Par ailleurs, l'espace péri-urbain est composé des communes dont la modalité vaut 112 ou 120.