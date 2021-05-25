library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("xlsx")

# Sources
# Data1: https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/table/
# Data2: https://www.insee.fr/fr/information/2115011 (1 January 2020)
# Data3: same as above sheet "ARM" + Wikipedia for population

# Population and code postal
data <-read.csv("../data/correspondance-code-insee-code-postal.csv", sep=';')
# Aires Urbaines (AU) and classification
data2<- read.csv2(file="../data/AU2020.csv", encoding = "UTF-8")

data <- data %>% 
  select(Code.INSEE, Code.Postal, Population) %>%  
  rename(CODGEO =  Code.INSEE, Code_Postal= Code.Postal, Population_commune = Population)

data2 <- merge(data2, data[,c("CODGEO", "Code_Postal", "Population_commune")], by="CODGEO")
data2<-select(data2, LIBGEO, LIBAU2010, CODGEO, Code_Postal, Population_commune, CATAEU2010)
data2$Population_commune<-as.character(data2$Population_commune)
data2$CATAEU2010<-as.character(data2$CATAEU2010)

# Data for Paris, Lyon, Marseille (missing in data2)
data3<- read.xlsx2(file="../data/AU2020_2.xlsx", sheetIndex=1, header=TRUE)
data3[data3$LIBAU2010=="Marseille",2] <- "Marseille - Aix-en-Provence"

data2 <- bind_rows(data2, data3)
data2$Population_commune <- as.double(data2$Population_commune)
data2$CATAEU2010 <- as.integer(data2$CATAEU2010)

# Get population of AUs by summing the populations of communes within the same AU
Pop_AU<-data2 %>%
  group_by(LIBAU2010) %>%
  summarise_at(dplyr::vars(Population_commune),
    list(Population_AU = sum))

data2 <- merge(data2, Pop_AU[,c("Population_AU", "LIBAU2010")], by="LIBAU2010")
data2 <- data2 %>% 
  rename(Aire_Urbaine =  LIBAU2010, Commune = LIBGEO, Code_INSEE = CODGEO, Zone_INSEE = CATAEU2010)

data2 <- data2 %>%
  mutate(Zone_GP = case_when(
    Zone_INSEE == 111 ~ "GP",
    Zone_INSEE == 112 ~ "Couronne_GP",
    TRUE ~ "Other"
  ))

data2 <- data2 %>%
  mutate(Zone_Category4 = case_when(
    Zone_INSEE %in% c(111, 211, 221)  ~ "Pôle",
    Zone_INSEE %in% c(112, 212, 222) ~ "Couronne",
    Zone_INSEE %in% c(120, 300) ~ "Multipolarisé",
    Zone_INSEE == 400 ~ "Isolé"
  ))

data2 <- data2 %>%
  mutate(rural_urban = case_when(
    Zone_INSEE %in% c(111)  ~ "Grande aire urbane",
    Zone_INSEE %in% c(211, 221, 212, 222) ~ "Autre aire urbaine",
    Zone_INSEE %in% c(112, 120) ~ "Péri-urbain",
    Zone_INSEE %in% c(400, 300) ~ "Rural"
  ))

data2 <- data2 %>%
  mutate(Zone_Category = case_when(
    Zone_INSEE %in% c(111, 211, 221)  ~ "Pôle",
    Zone_INSEE %in% c(112, 212, 222) ~ "Couronne",
    Zone_INSEE %in% c(120, 300, 400) ~ "Isolé ou multipolarisé"
  ))

data2 <- data2 %>%
  mutate(Grand_pole = case_when(
    Zone_INSEE %in% c(111)  ~ TRUE,
    TRUE ~ FALSE
  ))

(zones_GP <- data2 %>%
    select(Code_Postal, Zone_GP) %>%
    group_by(Code_Postal) %>%
    summarise_at(dplyr::vars(Zone_GP), list(zone = unique)))
for (i in 1:nrow(zones_GP)) {
  if (grepl("/", zones_GP$Code_Postal[i])) {
    code_postaux_i <- str_split(zones_GP$Code_Postal[i], "/")[[1]]
    zones_GP$Code_Postal[i] <- code_postaux_i[1]
    zones_GP <- rbind(zones_GP, as.data.frame(list("Code_Postal" = code_postaux_i[2:length(code_postaux_i)], "zone" = zones_GP$zone[i])))
  }
}

(Pop_cat <- data2 %>%
    group_by(Zone_GP) %>% # Grand_pole Zone_Category Zone_Category4 Zone_GP
    summarise_at(dplyr::vars(Population_commune),
                 list(Pop_cat = sum)))

(pop_Zone_GP <- (Pop_cat <- data2 %>%
  group_by(Zone_GP) %>% # Grand_pole Zone_Category Zone_Category4 Zone_GP
  summarise_at(dplyr::vars(Population_commune),
               list(Pop_cat = sum)))$Pop_cat / 63566.16)

write.csv(zones_GP, "../data/zipcodes/FR_aires_urbaines_2020__5948_1836_2216.csv", row.names = FALSE, quote = FALSE)
write.csv(data2, "../data/zipcodes/FR_aires_urbaines_2020.csv", row.names = TRUE)
data2 <- read.csv("../data/zipcodes/FR_aires_urbaines_2020.csv")

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