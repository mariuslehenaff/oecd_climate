library("xlsx")
library(dplyr)
library(data.table)
library(tidyr)

# IT

# Prepare data for merging
data2<- read.xlsx2(file="/Users/Bluebii/Downloads/Classificazioni-statistiche-Anni_2017-2021/Classificazioni statistiche-e-dimensione-dei-comuni_20_02_2021.xls", sheetIndex=1, header=TRUE, colClasses="character")
data1<- read.xlsx2(file="/Users/Bluebii/Downloads/Dati comunali e provinciali.xlsx", sheetIndex=2, header=TRUE, colClasses="character")

data1 <- rename(data1,Population = Popolazione.residente.al.Censimento.2011)
data2 <- rename(data2, Codice.Comune = Codice.Istat.del.Comune...numerico.)
data2$Codice.Comune <- as.integer(data2$Codice.Comune)
data1$Codice.Comune <- as.integer(data1$Codice.Comune)
data2 <- merge(data2, data1[,c("Codice.Comune", "Population")], by="Codice.Comune")

data2$Population <- as.integer(data2$Population)

# # Take figures for 2011 as it corresponds more to the actual figures
# sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.[data2$Grado.di.urbanizzazione==1]), na.rm=T)/ sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.), na.rm=T)
# sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.[data2$Grado.di.urbanizzazione==2]), na.rm=T)/ sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.), na.rm=T)
# sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.[data2$Grado.di.urbanizzazione==3]), na.rm=T)/ sum(as.integer(data2$Popolazione.legale.2011..09.10.2011.), na.rm=T)

data2$Istat <- data2$Codice.Comune

data3 <- read.csv2("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/xlsx/IT/IT_zipcode_pop.csv", sep=",")

# Merge
merge_1 <- merge(x=data3, y=data2[,c("Istat", "Grado.di.urbanizzazione")], all.x=T)
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==3]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==2]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==1]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[is.na(merge_1$Grado.di.urbanizzazione)]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

summary(merge_1$Abitanti[is.na(merge_1$Grado.di.urbanizzazione)])
summary(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==2])
sum(as.integer(merge_1$Abitanti[merge_1$Grado.di.urbanizzazione==3 | (is.na(merge_1$Grado.di.urbanizzazione) & merge_1$Abitanti<23000)]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

sum(as.integer(merge_1$Abitanti[is.na(merge_1$Grado.di.urbanizzazione) & merge_1$Abitanti<23000]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

# Set names as in our taxonomy

# Assign the two CP which are related to two different regions to a region (the one w/ more inhabitants)
# Insignificant btw, < 2k
merge_1$Regione[merge_1$CAP == "12071"] <- "PIE"
merge_1$Regione[merge_1$CAP == "18025"] <- "LIG"

merge_1$Region.quota <- ""
merge_1$Region.quota[merge_1$Regione %in% c("VDA", "LIG", "LOM", "PIE")] <- "North-West"
merge_1$Region.quota[merge_1$Regione %in% c("EMR", "FVG", "TAA", "VEN")] <- "North-East"
merge_1$Region.quota[merge_1$Regione %in% c("LAZ", "MAR", "TOS", "UMB")] <- "Center"
merge_1$Region.quota[merge_1$Regione %in% c("ABR", "PUG", "BAS", "CAL", "CAM", "MOL")] <- "South"
merge_1$Region.quota[merge_1$Regione %in% c("SAR", "SIC")] <- "Islands"

merge_1$Urban.rural.quota <- ""
merge_1$Urban.rural.quota[merge_1$Grado.di.urbanizzazione==1] <- "Cities"
merge_1$Urban.rural.quota[merge_1$Grado.di.urbanizzazione==2] <- "Small_Cities"
merge_1$Urban.rural.quota[merge_1$Grado.di.urbanizzazione==3] <- "Rural"

# Check quotas
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="North-West"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="North-East"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="Center"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="South"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)
sum(as.integer(merge_1$Abitanti[merge_1$Region.quota=="Islands"]), na.rm=T)/ sum(as.integer(merge_1$Abitanti), na.rm=T)

# Pop oK
check <- merge_1 %>%
  group_by(CAP) %>%
  summarise(pop.tot = sum(Abitanti, na.rm=T)) %>%
  ungroup()


# Not for category
## /!\
# Focus on subsample of duplicates
merge_1_pop <- merge_1 %>%
  group_by(CAP) %>%
  summarise(CAP.pop = sum(Abitanti, na.rm=T)) %>%
  ungroup()
merge_1 <- merge(merge_1, merge_1_pop, by="CAP")

duplicate.urbanity <- merge_1 %>%
  group_by(CAP) %>%
  summarise(dup.urbanity = length(unique(Urban.rural.quota))) %>%
  ungroup()
merge_1 <- merge(merge_1, duplicate.urbanity, by="CAP")

# Assign region if no duplicates
merge_1$Urbanity.CAP <- ""
merge_1$Urbanity.CAP[merge_1$dup.urbanity == 1] <- merge_1$Urban.rural.quota[merge_1$dup.urbanity == 1]

# Get share of each rural/urban category for each outcode
merge_1_urbanity <- merge_1[merge_1$dup.urbanity > 1,] %>%
  group_by(CAP, Urban.rural.quota) %>%
  summarise(CAP, Urban.rural.quota, share = sum(Abitanti, na.rm = T)/CAP.pop) %>%
  ungroup()

# Transform data
merge_1_urbanity <- merge_1_urbanity[!duplicated(merge_1_urbanity),]
merge_1_urbanity[merge_1_urbanity==""] <- "NA"
merge_1_urbanity <- merge_1_urbanity %>%
  pivot_wider(names_from = Urban.rural.quota, values_from = share)

merge_1_urbanity$Urbanity.CAP <- ""

# Replace NA with 0
merge_1_urbanity$Small_Cities[is.na(merge_1_urbanity$Small_Cities)] <- 0
merge_1_urbanity$Rural[is.na(merge_1_urbanity$Rural)] <- 0
merge_1_urbanity$Cities[is.na(merge_1_urbanity$Cities)] <- 0
merge_1_urbanity["NA"][is.na(merge_1_urbanity["NA"])] <- 0

# Assign value with greatest share
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity['NA'] > merge_1_urbanity$Rural & merge_1_urbanity['NA'] > merge_1_urbanity$Cities & merge_1_urbanity['NA'] > merge_1_urbanity$Small_Cities)] <- "Small_Cities"
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity$Cities > merge_1_urbanity$Rural & merge_1_urbanity$Cities > merge_1_urbanity$Small_Cities & merge_1_urbanity$Cities > merge_1_urbanity['NA'])] <- "Cities"
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity$Rural > merge_1_urbanity$Cities & merge_1_urbanity$Rural > merge_1_urbanity$Small_Cities & merge_1_urbanity$Rural > merge_1_urbanity['NA'])] <- "Rural"
merge_1_urbanity$Urbanity.CAP[(merge_1_urbanity$Small_Cities > merge_1_urbanity$Rural & merge_1_urbanity$Small_Cities > merge_1_urbanity$Cities & merge_1_urbanity$Small_Cities > merge_1_urbanity['NA'])] <- "Small_Cities"

# Merging
merge_1_urbanity <- merge_1_urbanity %>%
  select(CAP, Urbanity.CAP)

merge_1 <- merge(x=merge_1, y=merge_1_urbanity, by="CAP", all.x = T)

# Assign values of non-duplicates
merge_1$Urbanity.CAP.y[is.na(merge_1$Urbanity.CAP.y)] <- merge_1$Urbanity.CAP.x[is.na(merge_1$Urbanity.CAP.y)]
merge_1 <- merge_1 %>%
  rename(Urbanity.CAP = Urbanity.CAP.y)

sum(merge_1$Abitanti[merge_1$Urbanity.CAP == "Rural"], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)
sum(merge_1$Abitanti[merge_1$Urbanity.CAP == "Small_Cities"], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)
sum(merge_1$Abitanti[merge_1$Urbanity.CAP == "Cities"], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)
sum(merge_1$Abitanti[merge_1$Urbanity.CAP == ""], na.rm=T)/sum(merge_1$Abitanti, na.rm=T)

# Assign to rural values with inhbaitants < max of rural
merge_1$Urbanity.CAP[merge_1$Urbanity.CAP == "" & merge_1$Abitanti < 23036] <- "Rural"
merge_1$Urbanity.CAP[merge_1$Urbanity.CAP == "" & merge_1$Abitanti >= 23036] <- "Small_Cities"

data.CAP.list <- merge_1 %>%
  select(CAP, Urbanity.CAP, Region.quota)

# Generate additional ZIP for big cities

# Select zip for which we need to generate additional numbers
# Two different format ends with 'xx' or 'x'
data.CAP.list.xx <- data.CAP.list[substr(data.CAP.list$CAP, 5, 5) == 'x',]
data.CAP.list.x <- data.CAP.list.xx[substr(data.CAP.list.xx$CAP, 4, 5) != 'xx',]
data.CAP.list.xx <- data.CAP.list.xx[substr(data.CAP.list.xx$CAP, 4, 5) == 'xx',]

data.CAP.list.x$CAP <- substr(data.CAP.list.x$CAP, 1, 4)
data.CAP.list.xx$CAP <- substr(data.CAP.list.xx$CAP, 1, 3)

generate_zip <- function(vector, zipsize){
  interval.start <- as.numeric(vector[[1]])*zipsize
  interval.end <- (as.numeric(vector[[1]])+1)*zipsize-1

  zip <- c(interval.start:interval.end)
  
  df <- data.frame(zip, vector[[2]], vector[[3]]) %>%
    setNames(c("CAP", "Urbanity.CAP", "Region.quota"))
  
  return(df)
}

# Generate the zip
result.x <- apply(data.CAP.list.x, 1, generate_zip, zipsize=10)
result.x <- rbindlist(result.x, fill=T)

result.xx <- apply(data.CAP.list.xx, 1, generate_zip, zipsize=100)
result.xx <- rbindlist(result.xx, fill=T)
# Remove two zip generated that are duplicates of already existing CAP (with different information)
result.xx <- result.xx[!(result.xx$CAP %in% c(9170,34170)),]

# Select the remaining zip and append the two datasets
data.CAP.list.nox <- data.CAP.list[substr(data.CAP.list$CAP, 5, 5) != 'x',]
final <- data.CAP.list.nox[!duplicated(data.CAP.list.nox),]
final$CAP <- as.integer(final$CAP)
final <- rbind(final, result.x) %>%
  rbind(result.xx)

write.csv(final,"IT_zipcode_pop.csv", row.names=F)
