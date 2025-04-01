library(dplyr) # /!\ BUG if plyr (or memisc?) loaded before => detach("package:plyr", unload = TRUE)
library("openxlsx")

#source https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/table/
data <-read.csv("C:/Users/ZBOOK/Downloads/correspondance-code-insee-code-postal.csv", sep=';')
data <- as.data.frame(data)

data <- data %>% 
  select(Code.INSEE, Code.Postal, Population) %>%  
  rename(CODGEO =  Code.INSEE, Code_Postal= Code.Postal, Population_commune = Population)

data <- data %>%
  mutate(Code_Postal = as.integer(substr(Code_Postal, 1, 5)))

#source : https://github.com/bixiou/robustness_global_redistr/blob/main/data_ext/zipcode_FR.csv
data2 <-read.csv("C:/Users/ZBOOK/Downloads/zipcode_FR.csv")

data2 <- data2 %>%
  mutate(zipcode=as.integer(zipcode)
  )

merge_1 <- merge(data,data2,by.x="Code_Postal",by.y="zipcode",all.x=TRUE)

population_by_urbanity <- merge_1 %>%
  group_by(urbanity) %>%
  summarise(total_population = sum(Population_commune, na.rm = TRUE))

print(population_by_urbanity)

total_population <- sum(data$Population_commune, na.rm = TRUE)
print(total_population)

na_urbanity <- merge_1 %>% filter(is.na(urbanity))
print(na_urbanity)

# Charger le fichier d'origine
data3 <- read.csv("C:/Users/ZBOOK/Downloads/zipcode_FR.csv", stringsAsFactors = FALSE)

na_urbanity_mod <- na_urbanity %>%
  filter(Population_commune > 20) %>%
  mutate(urbanity = 1) %>%
  select(zipcode = Code_Postal, urbanity)

# Ajouter les nouvelles lignes à data3
data3 <- bind_rows(data3, na_urbanity_mod)

data3 <- data3 %>%
  mutate(departement = substr(zipcode, 1, 2))


data3 <- data3 %>%
  mutate(region = case_when(
    departement %in% c("75", "77", "78", "91", "92", "93", "94", "95") ~ "idf",
    departement %in% c("59", "62", "80", "02", "60") ~ "nord",
    departement %in% c("08", "10", "51", "52", "54", "55", "57", "67", "68", "88", "21", "25", "39", "58", "70", "71", "89", "90") ~ "est",
    departement %in% c("09", "12", "19", "23", "24", "31", "32", "33", "40", "46", "47", "48", "64", "65", "81", "82", "87") ~ "sud-ouest",
    departement %in% c("01", "03", "07", "11", "13", "15", "26", "30", "34", "38", "42", "43", "48", "63", "66", "69", "73", "74", "83", "84") ~ "sud-est",
    TRUE ~ "reste"
  ))

write.csv(data3[, c("zipcode", "urbanity", "region")], "zipcode_FR_updated25.csv", row.names = FALSE)

#Joindre data et data3 sur Code_Postal
data_merged <- merge(data, data3, by.x = "Code_Postal", by.y = "zipcode",all.x=TRUE)

# Calculer la population totale par région
population_par_region <- data_merged %>%
  group_by(region) %>%
  summarise(total_population = sum(Population_commune, na.rm = TRUE))

print(population_par_region)
