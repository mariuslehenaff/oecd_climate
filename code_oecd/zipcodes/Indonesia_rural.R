library(jsonlite)
library(dplyr)
# Source: https://github.com/digitalheir/Indonesia-Census-2010
data.pop <- read.csv2("/Users/Bluebii/Downloads/population_indonesian_villages.csv", sep=',')

data.pop <- data.pop[!(duplicated(data.pop[c("Province.Name", "Regency.Municipality.Code", "Village.Name", "District.Name")])),]

df_4 <- read.csv2("/Users/Bluebii/Downloads/ArrayAccess Indonesia-Postal-And-Area master data-csv_62/villages.csv", sep=',')
df_3 <- read.csv2("/Users/Bluebii/Downloads/ArrayAccess Indonesia-Postal-And-Area master data-csv_62/subDistricts.csv", sep=',')
df_3 <- read.csv2("/Users/Bluebii/Downloads/ArrayAccess Indonesia-Postal-And-Area master data-csv_62/provinces.csv", sep=',')
df_3 <- read.csv2("/Users/Bluebii/Downloads/ArrayAccess Indonesia-Postal-And-Area master data-csv_62/cities.csv", sep=',')
df_3 <- read.csv2("/Users/Bluebii/Downloads/ArrayAccess Indonesia-Postal-And-Area master data-csv_62/translations.csv", sep=',')

df_3 <- read.csv2("/Users/Bluebii/Downloads/ArrayAccess Indonesia-Postal-And-Area master data-csv_62/postalCodes.csv", sep=',')

df_3 <- df_3 %>%
  rename(Village.Name = Name)

merge_1 <- merge(x=df_3, y=df_2[,c("Population", "Village.Name")], by=c("Village.Name"), all.x = T)

sum(merge_1$Population, na.rm=T)
# 247 678 344

sum(data.pop$Population, na.rm=T)
# 237 689 702

nrow(merge_1[is.na(merge_1$Population),])
# 4420

# source: https://github.com/ekaputra07/kodepos
lines <- readLines("/Users/Bluebii/Downloads/kodepos.jsonl.txt")
lines <- lapply(lines, fromJSON)
lines <- lapply(lines, unlist)
data.zip <- bind_rows(lines)

data.zip <- data.zip %>%
  rename(zipcode = kodepos, District.Name = kecamatan, area = daerah_t2,
         region.code = kode_wil, Regency.Municipality.Name = kabupaten_kota,
         Province.Name = provinsi, Village.Name = desa)

data.zip$Province.Name <- toupper(data.zip$Province.Name)
data.zip$District.Name <- toupper(data.zip$District.Name)
data.zip$Regency.Municipality.Name <- toupper(data.zip$Regency.Municipality.Name)
data.zip$Village.Name <- toupper(data.zip$Village.Name)

# table(duplicated(data.pop[c("Regency.Municipality.Name", "Village.Name", "District.Name")]))
# 0 true
#data.zip$Province.Name[data.zip$Province.Name == "ACEH (NAD)"] <- "ACEH"
merge_1 <- merge(x=data.zip, y=data.pop[,c("Population", "Village.Name", "District.Name", "Regency.Municipality.Name")], by=c("District.Name", "Regency.Municipality.Name", "Village.Name"), all.x = T)

# Rename Provinces



data.kota.kabupaten <- read.csv2("/Users/Bluebii/Downloads/kabupaten_kota.csv", sep=";")
data.kota.kabupaten$Category <- word(data.kota.kabupaten$Name, 1)
sum(data.kota.kabupaten$Population[data.kota.kabupaten$Category=="Kota"])/sum(data.kota.kabupaten$Population)

