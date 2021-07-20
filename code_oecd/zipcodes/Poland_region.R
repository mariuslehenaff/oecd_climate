library("xlsx")
library(dplyr)

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

sum(data.pop$pop.total[data.pop$pop.total < 20000])/sum(data.pop$pop.total)

## PART 2: Get zip and region
data1 <- read.csv2("/Users/Bluebii/Downloads/archive (1)/Poland_complete_postal_codes_admin_div.csv", sep=",")
# Source: https://www.kaggle.com/ravikanth/poland-postal-codes-zip-state-counties-districts
data1 <- data1 %>%
  rename(stat.id = Ofc.Municipality.Key)

data1$zip_indicator <- substr(data1$Postal.Code,1,1)

# data1$zip_region <- ""
# data1$zip_region[data1$zip_indicator == 0] <- "Mazovia"
# data1$zip_region[data1$zip_indicator == 1] <- "Warmia-Masuria" "Podlasie"
# data1$zip_region[data1$zip_indicator == 2] <- "Lublin" "Swietokrzyskie"
# data1$zip_region[data1$zip_indicator == 3] <- "Lesser Poland" "Subcarpathia"
# data1$zip_region[data1$zip_indicator == 4] <- "Silesia" "Opole Voivodeship"
# data1$zip_region[data1$zip_indicator == 5] <- "Lower Silesia"
# data1$zip_region[data1$zip_indicator == 6] <- "Greater Poland" "Lubusz"
# data1$zip_region[data1$zip_indicator == 7] <- "West Pomerania"
# data1$zip_region[data1$zip_indicator == 8] <- "Pomerania" "Kujawsko-Pomorskie"
# data1$zip_region[data1$zip_indicator == 9] <- "Lódz Voivodeship"
# 2477 istat that merge, not a big deal as they correspond to the ones of the zipcode file

data1$zip_region <- ""
data1$zip_region[data1$zip_indicator %in% c(7, 8)] <- "North-West"
data1$zip_region[data1$zip_indicator %in% c(1)] <- "North-East"
data1$zip_region[data1$zip_indicator %in% c(6)] <- "Central"
data1$zip_region[data1$zip_indicator %in% c(4,5)] <- "South-West"
data1$zip_region[data1$zip_indicator %in% c(3)] <- "Central-East"
data1$zip_region[data1$zip_indicator %in% c(9,0,2)] <- "South-East"
