library(dplyr)
library(tidyr)

# States
# 1	New South Wales
# 2	Victoria
# 3	Queensland
# 4	South Australia
# 5	Western Australia
# 6	Tasmania
# 7	Northern Territory
# 8	Australian Capital Territory
# 9	Other Territories

## STEP 1: Get data from official sources and merge them
# source: (Remoteness Area (RA)ASGS Ed 2016 in .csv format) https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.005July%202016?OpenDocument
# table(data.zip$MB_CODE_2016 %in% data.RA$MB_CODE_2016)
data.RA <- read.csv2(file = "/Users/Bluebii/Downloads/RA_2016_AUST.csv", sep = ",")
data.RA$Rural.urban.quota <- "Rural"
data.RA$Rural.urban.quota[data.RA$RA_NAME_2016 == "Major Cities of Australia"] <- "Urban"
data.RA <- data.RA %>%
  select(MB_CODE_2016, Rural.urban.quota)

# POAs (Postal Areas ASGS Edition 2016 in .csv Format ): https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument
data.zip <- read.csv2(file = "/Users/Bluebii/Downloads/POA_2016_AUST.csv", sep = ",")

# census: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2074.02016?OpenDocument
data.census <- read.csv2(file = "/Users/Bluebii/Downloads/2016 census mesh block counts.csv", sep = ",")

# We get region from state
data.census$Region.quota[data.census$State %in% c(5,7)] <- "Western_Australia"
data.census$Region.quota[data.census$State %in% c(3)] <- "Queensland"
data.census$Region.quota[data.census$State %in% c(1,8)] <- "Broad_NSW"
data.census$Region.quota[data.census$State %in% c(4)] <- "South_Australia"
# Others added to this region (but only 1e-3 percent of the pop)
data.census$Region.quota[data.census$State %in% c(2,6)] <- "Victoria_Tasmania"

# Then we merge
data.zip <- merge(data.zip, data.RA, by = "MB_CODE_2016", all.x = T)

merge_1 <- merge(x = data.zip[, c("MB_CODE_2016", "POA_CODE_2016", "Rural.urban.quota")], y=data.census, by = c("MB_CODE_2016"), all.x = T)

## STEP 2: We assign RA with largest population share
# First we get total pop by POA
data.POA.pop <- merge_1 %>%
  group_by(POA_CODE_2016) %>%
  summarise(pop.POA = sum(Person, na.rm=T)) %>%
  ungroup()
merge_1 <- merge(merge_1, data.POA.pop, by="POA_CODE_2016")


# Focus on subsample of duplicates
duplicate.RA <- merge_1 %>%
  group_by(POA_CODE_2016) %>%
  summarise(dup.rural.urban = length(unique(Rural.urban.quota))) %>%
  ungroup()

# Assign RA if no duplicates
merge_1$Rural.urban.POA <- ""
merge_1 <- merge(merge_1, duplicate.RA, by="POA_CODE_2016")
merge_1$Rural.urban.POA[merge_1$dup.rural.urban == 1] <- merge_1$Rural.urban.quota[merge_1$dup.rural.urban == 1]

# Get share of each RA for each POA w/ duplicates
data.POA.RA <- merge_1[merge_1$dup.rural.urban > 1,] %>%
  group_by(POA_CODE_2016, Rural.urban.quota) %>%
  summarise(POA_CODE_2016, Rural.urban.quota, share = sum(Person, na.rm = T)/pop.POA) %>%
  ungroup()

# Transform data
data.POA.RA <- data.POA.RA[!duplicated(data.POA.RA),]
data.POA.RA <- data.POA.RA %>%
  pivot_wider(names_from = Rural.urban.quota, values_from = share)

data.POA.RA$Rural.urban.POA <- ""

# Assign value w/ greatest share
data.POA.RA$Rural.urban.POA[(data.POA.RA["Urban"] > data.POA.RA["Rural"])] <- "Urban"
data.POA.RA$Rural.urban.POA[(data.POA.RA["Rural"] > data.POA.RA["Urban"])] <- "Rural"

# Merging
data.POA.RA <- data.POA.RA %>%
  select(POA_CODE_2016, Rural.urban.POA)

merge_1 <- merge(x=merge_1, y=data.POA.RA, by="POA_CODE_2016", all.x = T)
# Assign values of non-duplicates
merge_1$Rural.urban.POA.y[is.na(merge_1$Rural.urban.POA.y)] <- merge_1$Rural.urban.POA.x[is.na(merge_1$Rural.urban.POA.y)]
merge_1 <- merge_1 %>%
  rename(Rural.urban.POA = Rural.urban.POA.y)


## STEP 3: We assign region with largest population share
# Focus on subsample of duplicates
duplicate.region <- merge_1 %>%
  group_by(POA_CODE_2016) %>%
  summarise(dup.region = length(unique(Region.quota))) %>%
  ungroup()

# Assign region if no duplicates
merge_1$Region.POA <- ""
merge_1 <- merge(merge_1, duplicate.region, by="POA_CODE_2016")
merge_1$Region.POA[merge_1$dup.region == 1] <- merge_1$Region.quota[merge_1$dup.region == 1]

# Get share of each region for each POA w/ duplicates
data.POA.region <- merge_1[merge_1$dup.region > 1,] %>%
  group_by(POA_CODE_2016, Region.quota) %>%
  summarise(POA_CODE_2016, Region.quota, share = sum(Person, na.rm = T)/pop.POA) %>%
  ungroup()

# Transform data
data.POA.region <- data.POA.region[!duplicated(data.POA.region),]
data.POA.region <- data.POA.region %>%
  pivot_wider(names_from = Region.quota, values_from = share)

data.POA.region$Region.POA <- ""

# Delete NA column
data.POA.region["NA"] <- NULL
# Replace NA w/ 0 to sum
data.POA.region[,c(2:7)] <- replace(data.POA.region[,c(2:7)], is.na(data.POA.region[c(2:7)]), 0)

# Assign value w/ greatest share
data.POA.region$Region.POA[(data.POA.region["Western_Australia"] > data.POA.region["South_Australia"] & data.POA.region["Western_Australia"] > data.POA.region["Broad_NSW"] & data.POA.region["Western_Australia"] > data.POA.region["Queensland"] & data.POA.region["Western_Australia"] > data.POA.region["Victoria_Tasmania"])] <- "Western_Australia"
data.POA.region$Region.POA[(data.POA.region["South_Australia"] > data.POA.region["Western_Australia"] & data.POA.region["South_Australia"] > data.POA.region["Broad_NSW"] & data.POA.region["South_Australia"] > data.POA.region["Queensland"] & data.POA.region["South_Australia"] > data.POA.region["Victoria_Tasmania"])] <- "South_Australia"
data.POA.region$Region.POA[(data.POA.region["Broad_NSW"] > data.POA.region["Western_Australia"] & data.POA.region["Broad_NSW"] > data.POA.region["South_Australia"] & data.POA.region["Broad_NSW"] > data.POA.region["Queensland"] & data.POA.region["Broad_NSW"] > data.POA.region["Victoria_Tasmania"])] <- "Broad_NSW"
data.POA.region$Region.POA[(data.POA.region["Queensland"] > data.POA.region["Western_Australia"] & data.POA.region["Queensland"] > data.POA.region["South_Australia"] & data.POA.region["Queensland"] > data.POA.region["Broad_NSW"] & data.POA.region["Queensland"] > data.POA.region["Victoria_Tasmania"])] <- "Queensland"
data.POA.region$Region.POA[(data.POA.region["Victoria_Tasmania"] > data.POA.region["Western_Australia"] & data.POA.region["Victoria_Tasmania"] > data.POA.region["South_Australia"] & data.POA.region["Victoria_Tasmania"] > data.POA.region["Broad_NSW"] & data.POA.region["Victoria_Tasmania"] > data.POA.region["Queensland"])] <- "Victoria_Tasmania"

# Merging
data.POA.region <- data.POA.region %>%
  select(POA_CODE_2016, Region.POA)

merge_1 <- merge(x=merge_1, y=data.POA.region, by="POA_CODE_2016", all.x = T)
# Assign values of non-duplicates
merge_1$Region.POA.y[is.na(merge_1$Region.POA.y)] <- merge_1$Region.POA.x[is.na(merge_1$Region.POA.y)]
merge_1 <- merge_1 %>%
  rename(Region.POA = Region.POA.y)

## STEP 4: Regroup data on pop, dwellings and size by POA
# We also compute the share of each Region for POA that are linked to several States
# We attribute the region with the largest share of inhabitants
merge_1$AREA_ALBERS_SQKM <- as.numeric(merge_1$AREA_ALBERS_SQKM)

data.POA <- merge_1 %>%
  group_by(POA_CODE_2016) %>%
  summarise(pop = sum(Person, na.rm = T), total_dwellings = sum(Dwelling, na.rm = T), tot.area = sum(AREA_ALBERS_SQKM, na.rm = T), Region.POA = Region.POA, Rural.urban.POA = Rural.urban.POA) %>%
  ungroup()
data.POA <- data.POA[!duplicated(data.POA),]


data.POA$Density.pop <- data.POA$pop/data.POA$tot.area
data.POA$Density.dwe <- data.POA$total_dwellings/data.POA$tot.area

# Check quotas rural urban
#sum(data.POA$pop[data.POA$Density.pop <= 200], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[data.POA$Rural.urban.POA == "Urban"], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[data.POA$Rural.urban.POA == "Rural"], na.rm = T)/sum(data.POA$pop)

# Some POA w/ NA region, we reassign them manually
data.POA$Region.POA[data.POA$POA_CODE_2016 %in% c(815, 6798, 6799)] <- "Western_Australia"
data.POA$Region.POA[data.POA$POA_CODE_2016 %in% c(2522, 2899)] <- "Broad_NSW"
data.POA$Region.POA[data.POA$POA_CODE_2016 %in% c(3800)] <- "Victoria_Tasmania"

# Check quota region
sum(data.POA$pop[data.POA$Region.POA == "Western_Australia"], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[is.na(data.POA$Region.POA)], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[data.POA$Region.POA == "Broad_NSW"], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[data.POA$Region.POA == "Victoria_Tasmania"], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[data.POA$Region.POA == "Queensland"], na.rm = T)/sum(data.POA$pop)
sum(data.POA$pop[data.POA$Region.POA == "South_Australia"], na.rm = T)/sum(data.POA$pop)

#https://www.abs.gov.au/ausstats/abs@.nsf/Lookup/2071.0main+features1132016
# Small towns (<10k): 9.7% pop

## STEP 5: Take info, on additional zip code (in particular Remoteness area and state)
# source zip: https://github.com/matthewproctor/australianpostcodes
zip.all <- read.csv2(file = "/Users/Bluebii/Downloads/australian_postcodes.csv", sep = ",")
zip.all <- zip.all %>%
  rename(POA_CODE_2016 = postcode)

zip.all <- zip.all[!duplicated(zip.all$POA_CODE_2016),]

zip.all$Region.POA <- ""
zip.all$Region.POA[zip.all$state %in% c("ACT", "NSW")] <- "Broad_NSW"
zip.all$Region.POA[zip.all$state %in% c("NT", "WA")] <- "Western_Australia"
zip.all$Region.POA[zip.all$state %in% c("QLD")] <- "Queensland"
zip.all$Region.POA[zip.all$state %in% c("SA")] <- "South_Australia"
zip.all$Region.POA[zip.all$state %in% c("VIC", "TAS")] <- "Victoria_Tasmania"

zip.all$Rural.urban.POA <- "Rural"
zip.all$Rural.urban.POA[zip.all$RA_2016 == 1] <- "Urban"

merge_2 <- merge(data.POA, zip.all, by = "POA_CODE_2016", all.y = T)

# Look at zip code that we do not have in data.zip and that are not administrative entities
# For those, we take Remoteness area for proxy of urban rural
additional.zip <- (merge_2[is.na(merge_2$pop & merge_2$POA_CODE_2016 %in% c(800:899, 2000:5799, 6000:6797, 7000:7799)),])
additional.zip <- additional.zip %>%
  rename(Region.POA = Region.POA.y, Rural.urban.POA = Rural.urban.POA.y)
## STEP 6: Final cleaning
final <- rbind(data.POA[, c("POA_CODE_2016", "Region.POA", "Rural.urban.POA")], additional.zip[, c("POA_CODE_2016", "Region.POA", "Rural.urban.POA")])
write.csv(final,"AU_zipcode.csv", row.names=F)
