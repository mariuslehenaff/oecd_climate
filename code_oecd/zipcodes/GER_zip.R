library("openxlsx")
library(dplyr)

# Data w/ area type
data2<- read.xlsx("/Users/Bluebii/Downloads/31122019_Auszug_GV (1).xlsx", sheet=1, colNames =TRUE)
# source: https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122019_Auszug_GV.html
sum(data2$pop.total[data2$density_type=="02"], na.rm=T)/sum(data2$pop.total, na.rm=T)
# 01 : 39.5%
# 02 : 40.3%
# 03 : 20.2%

# Get subset w/ all zipcodes (keep same number of total pop)
data2 <- data2 %>%
  subset(!(is.na(zipcode)))

data2$zipcode <- as.integer(data2$zipcode)

# Data w/ all zipcodes for each city
# source: https://gist.github.com/jbspeakr/4565964#file-german-zip-codes-csv
df_4 <- read.csv2("/Users/Bluebii/Downloads/German-Zip-Codes.csv")

df_4 <- df_4 %>%
  rename(zipcode = Plz, name = Ort)

df_4 <- df_4 %>%
  select(name, zipcode,Vorwahl,Bundesland)

df_4 <- df_4[!duplicated(df_4),]


# Merge to have density for zipcodes
merge_1 <- merge(x=df_4, y=data2[,c("zipcode", "pop.total", "density_type")], by=c("zipcode"), all.x=T)
merge_1$density_type <- as.integer(merge_1$density_type)

# For each name and Vorwahl (to uniquely identify a city) we give max zipcode
# This allows to allocate a density_type for each zip code
merge_1 <- merge_1 %>%
  group_by(name,Vorwahl) %>%
  mutate(density_type_2=max(density_type,na.rm=TRUE)) %>%
  ungroup()

# Then remove duplicates, keeping zipcode with largest density value
merge_1 <- merge_1[order(merge_1$zipcode, merge_1$density_type_2 ), ]
merge_1 <- merge_1[ !duplicated(merge_1$zipcode), ] 

# For remaining zip codes w/o a density we consider them as rural
merge_1$density_type_2[merge_1$density_type_2<0] <- 3

Northern <-	c("Bremen", "Hamburg", "Niedersachsen", "Mecklenburg-Vorpommern", "Schlewig-Holstein")
Western	<- c("Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland")
Central	<- c("Hesses", "Thüringen")
Eastern <- c("Berlin", "Brandenburg", "Sachsen", "Sachsen-Anhalt")
Southern <- c("Baden-Württemberg", "Bayern")

merge_1$Region[merge_1$Bundesland %in% Northern] <- "Northern"
merge_1$Region[merge_1$Bundesland %in% Western] <- "Western"
merge_1$Region[merge_1$Bundesland %in% Central] <- "Central"
merge_1$Region[merge_1$Bundesland %in% Eastern] <- "Eastern"
merge_1$Region[merge_1$Bundesland %in% Southern] <- "Southern"

merge_1$Urbanity[merge_1$density_type_2 < 3] <- "Urban"
merge_1$Urbanity[merge_1$density_type_2 == 3] <- "Rural"

merge_1 <- merge_1 %>%
  select(zipcode, Urbanity, Region)

write.csv(merge_1,"Germany_zipcode.csv", row.names=T)
