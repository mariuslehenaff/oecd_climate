library(dplyr)
library(tidyr)

# Source: https://www.doogal.co.uk/postcodedownloads.php
data <- read.csv2("/Users/Bluebii/Downloads/postcodes.csv", sep=',')

rural <- c("Rural village", "Rural hamlet and isolated dwellings", "Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural hamlet and isolated dwellings in a sparse setting", "Rural village in a sparse setting")
rural <- c(rural,"", "Accessible rural area", "Remote rural area", "Very remote rural area", "Very remote small town", "Accessible small town", "Remote small town")
urban_town <- c("Urban city and town", "Urban city and town in a sparse setting")
urban_large <- c("Urban major conurbation", "Urban minor conurbation", "Large urban area", "Other urban area")

sum(data$Population[data$Rural.urban %in% urban_town], na.rm=T)/sum(data$Population, na.rm=T)
sum(data$Population[data$Rural.urban %in% c(rural)], na.rm=T)/sum(data$Population, na.rm=T)
sum(data$Population[data$Rural.urban %in% urban_large], na.rm=T)/sum(data$Population, na.rm=T)


## PART 2

#data <- read.csv2(file="/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/xlsx/UK/UK_zip_pop.csv", header=T, sep=",")

# View(a)
# data.england <- data[data$Country=="England",]

southern.england <- c("South West", "South East", "East of England")
midlands <- c("West Midlands", "East Midlands")
northern.england <- c("North West", "Yorkshire and The Humber", "North East")

sum(data$Population[data$Region == "London"], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Region %in% southern.england], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Region %in% midlands | data$Country=="Wales"], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Region %in% northern.england], na.rm = T)/sum(data$Population, na.rm = T)
sum(data$Population[data$Country %in% c("Scotland", "Northern Ireland")], na.rm = T)/sum(data$Population, na.rm = T)

data$Rural.urban.quota[data$Rural.urban %in% urban_town] <-"City_Town"
data$Rural.urban.quota[data$Rural.urban %in% rural] <-"Rural"
data$Rural.urban.quota[data$Rural.urban %in% urban_large] <-"Large_urban"

data$Region.quota[data$Region =="London"] <- "London"
data$Region.quota[data$Region %in% southern.england] <- "Southern England"
data$Region.quota[data$Region %in% midlands | data$Country == "Wales"] <- "Central UK"
data$Region.quota[data$Region %in% northern.england] <- "Northern England"
data$Region.quota[data$Country %in% c("Scotland", "Northern Ireland")] <- "Northern UK"

data.outcode <- data %>%
  select(Population, Postcode, Region.quota, Rural.urban.quota)

data.outcode <- data.outcode %>%
  subset(Region.quota != "")
# Get the outcode
data.outcode$outcode <- sub(" .*", "", data.outcode$Postcode)

# Then since there are more then 2Mio postcodes, we want info at the outcode levels
# When there are multiples regions or rural/urban areas for the same outcode,
# we assign the one with the greatest share of population


# Step 1: Get total pop by outcode
data.outcode.pop <- data.outcode %>%
  group_by(outcode) %>%
  summarise(pop.outcode = sum(Population, na.rm=T)) %>%
  ungroup()
data.outcode <- merge(data.outcode, data.outcode.pop, by="outcode")

######################
## Step 2: Urbanity ##
######################
# Focus on subsample of duplicates
duplicate.urbanity <- data.outcode %>%
  group_by(outcode) %>%
  summarise(dup.urbanity = length(unique(Rural.urban.quota))) %>%
  ungroup()

# Assign region if no duplicates
data.outcode$Urbanity.outcode <- ""
data.outcode <- merge(data.outcode, duplicate.urbanity, by="outcode")
data.outcode$Urbanity.outcode[data.outcode$dup.urbanity == 1] <- data.outcode$Rural.urban.quota[data.outcode$dup.urbanity == 1]

# Get share of each rural/urban category for each outcode
data.outcode.urbanity <- data.outcode[data.outcode$dup.urbanity > 1,] %>%
  group_by(outcode, Rural.urban.quota) %>%
  summarise(outcode, Rural.urban.quota, share = sum(Population, na.rm = T)/pop.outcode) %>%
  ungroup()

# Transform data
data.outcode.urbanity <- data.outcode.urbanity[!duplicated(data.outcode.urbanity),]
data.outcode.urbanity <- data.outcode.urbanity %>%
  pivot_wider(names_from = Rural.urban.quota, values_from = share)

data.outcode.urbanity$Urbanity.outcode <- ""
# If no population at all, we assign it to rural, except for 3 exceptions (not related to rural in original data)
data.outcode.urbanity$Urbanity.outcode[(is.na(data.outcode.urbanity$Large_urban) & is.na(data.outcode.urbanity$Rural) & is.na(data.outcode.urbanity$City_Town)) == T] <- "Rural"
data.outcode.urbanity$Urbanity.outcode[data.outcode.urbanity$outcode %in% c("DH98", "NG80", "WV98")] <- "City_Town"

# Replace NA with 0
data.outcode.urbanity$Large_urban[is.na(data.outcode.urbanity$Large_urban)] <- 0
data.outcode.urbanity$Rural[is.na(data.outcode.urbanity$Rural)] <- 0
data.outcode.urbanity$City_Town[is.na(data.outcode.urbanity$City_Town)] <- 0

# Assign value with greatest share
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$Large_urban > data.outcode.urbanity$Rural & data.outcode.urbanity$Large_urban > data.outcode.urbanity$City_Town)] <- "Large_urban"
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$Rural > data.outcode.urbanity$Large_urban & data.outcode.urbanity$Rural > data.outcode.urbanity$City_Town)] <- "Rural"
data.outcode.urbanity$Urbanity.outcode[(data.outcode.urbanity$City_Town > data.outcode.urbanity$Rural & data.outcode.urbanity$City_Town > data.outcode.urbanity$Large_urban)] <- "City_Town"

# Merging
data.outcode.urbanity <- data.outcode.urbanity %>%
  select(outcode, Urbanity.outcode)

data.outcode <- merge(x=data.outcode, y=data.outcode.urbanity, by="outcode", all.x = T)

# Assign values of non-duplicates
data.outcode$Urbanity.outcode.y[is.na(data.outcode$Urbanity.outcode.y)] <- data.outcode$Urbanity.outcode.x[is.na(data.outcode$Urbanity.outcode.y)]
data.outcode <- data.outcode %>%
  rename(Urbanity.outcode = Urbanity.outcode.y)

sum(data.outcode$Population[data.outcode$Urbanity.outcode == "City_Town"], na.rm=T)/sum(data.outcode$Population, na.rm=T)
sum(data.outcode$Population[data.outcode$Urbanity.outcode == "Rural"], na.rm=T)/sum(data.outcode$Population, na.rm=T)
sum(data.outcode$Population[data.outcode$Urbanity.outcode == "Large_urban"], na.rm=T)/sum(data.outcode$Population, na.rm=T)


#############
## Step 3: Region ##
#############

# Focus on subsample of duplicates
duplicate.region <- data.outcode %>%
  group_by(outcode) %>%
  summarise(dup.region = length(unique(Region.quota))) %>%
  ungroup()

# Assign region if no duplicates
data.outcode$Region.outcode <- ""
data.outcode <- merge(data.outcode, duplicate.region, by="outcode")
data.outcode$Region.outcode[data.outcode$dup.region == 1] <- data.outcode$Region.quota[data.outcode$dup.region == 1]
# Get share of each region for each outcode
data.outcode.region <- data.outcode[data.outcode$dup.region > 1,] %>%
  group_by(outcode, Region.quota) %>%
  summarise(outcode, Region.quota, share = sum(Population, na.rm = T)/pop.outcode) %>%
  ungroup()

# Transform data
data.outcode.region <- data.outcode.region[!duplicated(data.outcode.region),]
data.outcode.region <- data.outcode.region %>%
  pivot_wider(names_from = Region.quota, values_from = share)

data.outcode.region$Region.outcode <- ""
# For values with only NA and NaN, we assign the NaN values
# with the following order of priority : London >> Central UK >> Rest
outcode.central <- c("CH28", "CH29", "CH34", "CH70", "NP5", "NP6", "PE17", "S19", "S30", "S31")
data.outcode.region$Region.outcode[data.outcode.region$outcode %in% outcode.central] <- "Central UK"
data.outcode.region$Region.outcode[data.outcode.region$outcode == "WD2"] <- "London"

# Replace NA with 0 to sum
region <- c("London", "Southern England", "Northern England", "Northern UK", "Central UK")
data.outcode.region[,region] <- replace(data.outcode.region[,region], is.na(data.outcode.region[region]), 0)

# Assign value with greatest share
data.outcode.region$Region.outcode[(data.outcode.region["Southern England"] > data.outcode.region["London"] & data.outcode.region["Southern England"] > data.outcode.region["Northern England"] & data.outcode.region["Southern England"] > data.outcode.region["Northern UK"] & data.outcode.region["Southern England"] > data.outcode.region["Central UK"])] <- "Southern England"
data.outcode.region$Region.outcode[(data.outcode.region["Northern England"] > data.outcode.region["London"] & data.outcode.region["Northern England"] > data.outcode.region["Southern England"] & data.outcode.region["Northern England"] > data.outcode.region["Northern UK"] & data.outcode.region["Northern England"] > data.outcode.region["Central UK"])] <- "Northern England"
data.outcode.region$Region.outcode[(data.outcode.region["Northern UK"] > data.outcode.region["London"] & data.outcode.region["Northern UK"] > data.outcode.region["Southern England"] & data.outcode.region["Northern UK"] > data.outcode.region["Northern England"] & data.outcode.region["Northern UK"] > data.outcode.region["Central UK"])] <- "Northern UK"
data.outcode.region$Region.outcode[(data.outcode.region["Central UK"] > data.outcode.region["London"] & data.outcode.region["Central UK"] > data.outcode.region["Southern England"] & data.outcode.region["Central UK"] > data.outcode.region["Northern England"] & data.outcode.region["Central UK"] > data.outcode.region["Northern UK"])] <- "Central UK"
data.outcode.region$Region.outcode[(data.outcode.region["London"] > data.outcode.region["Southern England"] & data.outcode.region["London"] > data.outcode.region["Northern England"] & data.outcode.region["London"] > data.outcode.region["Northern UK"] & data.outcode.region["London"] > data.outcode.region["Central UK"])] <- "London"

# Merging
data.outcode.region <- data.outcode.region %>%
  select(outcode, Region.outcode)

data.outcode <- merge(x=data.outcode, y=data.outcode.region, by="outcode", all.x = T)
# Assign values of non-duplicates
data.outcode$Region.outcode.y[is.na(data.outcode$Region.outcode.y)] <- data.outcode$Region.outcode.x[is.na(data.outcode$Region.outcode.y)]
data.outcode <- data.outcode %>%
  rename(Region.outcode = Region.outcode.y)


sum(data.outcode$Population[data.outcode$Region.outcode == "London"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Southern England"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Central UK"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Northern England"], na.rm = T)/sum(data.outcode$Population, na.rm = T)
sum(data.outcode$Population[data.outcode$Region.outcode == "Northern UK"], na.rm = T)/sum(data.outcode$Population, na.rm = T)

## Final Export
final <- data.outcode %>%
  select(outcode, Urbanity.outcode, Region.outcode)

final <- final[!duplicated(final),]


final$Urbanity.outcode[final$Urbanity.outcode=="Rural"] <- 1
final$Urbanity.outcode[final$Urbanity.outcode=="City_Town"] <- 2
final$Urbanity.outcode[final$Urbanity.outcode=="Large_urban"] <- 3

final$Region.outcode[final$Region.outcode == "London"] <- 1
final$Region.outcode[final$Region.outcode == "Southern England"] <- 2
final$Region.outcode[final$Region.outcode == "Central UK"] <- 3
final$Region.outcode[final$Region.outcode == "Northern England"] <- 4
final$Region.outcode[final$Region.outcode == "Northern UK"] <- 5
final$Region.outcode <- as.integer(final$Region.outcode)

write.csv(final,"UK_zipcode.csv", row.names=F)
