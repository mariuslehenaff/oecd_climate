library("xlsx")
library(dplyr)


# Postcode
data1 <- read.csv2("/Users/Bluebii/Downloads/archive (1)/Poland_complete_postal_codes_admin_div.csv", sep=",")
# Source: https://www.kaggle.com/ravikanth/poland-postal-codes-zip-state-counties-districts
data1 <- data1 %>%
  rename(stat.id = Ofc.Municipality.Key)

data1$small_zip <- substr(data1$Postal.Code,1,1)

duplicate.urbanity <- data1 %>%
  group_by(stat.id) %>%
  summarise(dup.urbanity = length(unique(small_zip))) %>%
  ungroup()

table(duplicate.urbanity$dup.urbanity)
View(duplicate.urbanity[duplicate.urbanity$dup.urbanity>1,])
## PART 1: Get Pop
# source: https://stat.gov.pl/en/topics/population/population/population-size-and-structure-and-vital-statistics-in-poland-by-territorial-division-as-of-december-31-2020,3,29.html
data.pop<- read.xlsx2(file="/Users/Bluebii/Downloads/LudnoòÜ.Stan i struktura_31.12.2020/Tabela_IV.xls", sheetIndex=1, header=TRUE, colClasses="character")

# Only keep G. and M. entries (as M-W. is the sum of the two)
data.pop <- subset(data.pop, startsWith(data.pop$Gminas, "G.")|startsWith(data.pop$Gminas, "M."))
# For stat.id need to delete last character (type of entity) and take integer form
data.pop$stat.id <- data.pop$Code %>%
  substr(1,nchar(data.pop$Code)-1) %>%
  as.integer()

# Sum population so that G. M. are aggregated
data.pop$Population <- as.integer(data.pop$Population)
data.pop <- data.pop %>% 
  group_by(stat.id) %>% 
  summarise(pop.total = sum(Population)) %>%
  ungroup()
# length(data.pop$stat.id) : 2477

data1 <- merge(data1, data.pop[,c("stat.id", "pop.total")], by="stat.id")

## PART 2: Get type of city
# Source: https://eteryt.stat.gov.pl/eTeryt/rejestr_teryt/udostepnianie_danych/baza_teryt/uzytkownicy_indywidualni/pobieranie/pliki_pelne.aspx?contrast=default
# NTS and SIMC statistical

# Type of commune
data.NTS <- read.csv2("/Users/Bluebii/Downloads/NTS_2021-06-29/NTS_2021-06-29.csv", sep=";")
data.NTS <- data.NTS[!is.na(data.NTS$GMI),]
data.NTS$stat.id <- (data.NTS$WOJ*10000+data.NTS$POW*100+data.NTS$GMI)
# length(unique(data.NTS$stat.id)) # 2516


data.SMIC <- read.csv2("/Users/Bluebii/Downloads/SIMC_Statystyczny_2021-06-29/SIMC_Statystyczny_2021-06-29.csv", sep=";")
data.SMIC$stat.id <- (data.SMIC$WOJ*10000+data.SMIC$POW*100+data.SMIC$GMI)
# length(unique(data.SMIC$stat.id)) # 2514



merge_1 <- merge(data1, data.NTS[,c("stat.id", "RODZ", "WOJ")], by="stat.id")
#merge_1_unique <- merge_1[!duplicated(merge_1$stat.id), ]


#merge_2 <- merge(data1, data.SMIC[,c("stat.id", "RODZ_GMI", "WOJ")], by="stat.id")
#merge_2_unique <- merge_2[!duplicated(merge_2$stat.id), ]
#sum(merge_2_unique$pop.total[merge_2_unique$min.RODZ == 1], na.rm=T)/sum(merge_2_unique$pop.total, na.rm=T)

merge_1$region <- ""
merge_1$region[merge_1$WOJ %in% c(22,32)] <-"North-West"   
merge_1$region[merge_1$WOJ %in% c(28, 20)] <-"North-East"   
merge_1$region[merge_1$WOJ %in% c(8, 30, 4)] <-"Central"
merge_1$region[merge_1$WOJ %in% c(2, 16)] <-"South-West"
merge_1$region[merge_1$WOJ %in% c(24, 12, 18)] <-"Central-East"   
merge_1$region[merge_1$WOJ %in% c(10, 14, 6, 26)] <-"South-East"
#sum(merge_1_unique$pop.total[merge_1_unique$region == "North-West"], na.rm=T)/sum(merge_1_unique$pop.total, na.rm=T)

# Take minimum RODZ by CP
df <- merge_1 %>% 
  group_by(Postal.Code) %>% 
  summarise(min.RODZ=min(RODZ), stat.id)

# Assign min RODZ to each obs.
merge_1 <- merge(merge_1, df[, c("Postal.Code", "min.RODZ")], by="Postal.Code")
merge_1_unique <- merge_1[!duplicated(merge_1$stat.id), ]
sum(merge_1_unique$pop.total[merge_1_unique$min.RODZ == 1], na.rm=T)/sum(merge_1_unique$pop.total, na.rm=T)

merge_1$Rural.urban <- ""
merge_1$Rural.urban[merge_1_unique$min.RODZ == 1] <- "Urban"
merge_1$Rural.urban[merge_1_unique$min.RODZ > 1] <- "Rural"

final <- merge_1 %>%
  select(Postal.Code, region, Rural.urban)

duplicate <- final %>%
  group_by(Postal.Code) %>%
  summarise(dup.urbanity = length(unique(region))) %>%
  ungroup()

table(duplicate$dup.urbanity)

final <- final[!duplicated(final$Postal.Code),]

write.csv(final,"Poland_zipcode.csv", row.names = T)
View(data.pop2[duplicated(data.pop2[c("Municipality.short","Prefecture")]),]) # Nbr duplicates

# TODO
# On pourrait peut-être faire ça:
#   - Chaque code commune, on peut lui associer une région au départ (la bonne!)
# - On fait la même avec les codes postaux.
# - On dégage tous les codes postaux qui ne matchent pas la région de leur code commune
# 
# Et d'un autre côté
# - On associe le code commune à la catégorie urbaine avec la plus grosse part d'habitants. Le problème c'est que les régions vont sûrement être trop grosses, et donc que les villes vont mécaniquement peser plus et donc qu'on ait du rural sous-représenté
#            