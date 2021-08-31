library(dplyr)

# source: https://data.gov.in/node/6818285
data.zip <- read.csv2(file = "/Users/Bluebii/Downloads/pincode.csv", sep = ",")

data.zip$Region.Quota <- ""
data.zip$Region.Quota[data.zip$StateName %in% c("RAJASTHAN", "PUNJAB", "HIMACHAL PRADESH", "HARYANA", "DELHI",
                                                 "JAMMU AND KASHMIR", "LADAKH", "CHANDIGARH")] <- "Northern"
data.zip$Region.Quota[data.zip$StateName %in% c("ANDHRA PRADESH", "TELANGANA", "KARNATAKA", "TAMIL NADU", "KERALA",
                                                 "LAKSHADWEEP", "PUDUCHERRY", "ANDAMAN AND NICOBAR ISLANDS")] <- "Southern"
data.zip$Region.Quota[data.zip$StateName %in% c("UTTAR PRADESH", "MADHYA PRADESH", "CHHATTISGARH", "UTTARAKHAND")] <- "Central"
data.zip$Region.Quota[data.zip$StateName %in% c("BIHAR", "ASSAM", "JHARKHAND", "ODISHA", "WEST BENGAL",
                                                 "MEGHALAYA", "MANIPUR", "MIZORAM", "NAGALAND", "TRIPURA",
                                                 "ARUNACHAL PRADESH", "SIKKIM")] <- "Eastern"
data.zip$Region.Quota[data.zip$StateName %in% c("MAHARASHTRA", "GUJARAT", "THE DADRA AND NAGAR HAVELI AND DAMAN AND DIU", "GOA")] <- "Western"

# Check for duplicates (== different regions for same pincode)
duplicate.Region <- data.zip %>%
  group_by(Pincode) %>%
  summarise(dup.rural.urban = length(unique(Region.Quota))) %>%
  ungroup()

dup.pin <- duplicate.Region$Pincode[duplicate.Region$dup.rural.urban==2]

# Fix thte 2 pincodes with duplicates
data.zip$Region.Quota[data.zip$Pincode == 110025] <- "Northern"
data.zip$Region.Quota[data.zip$Pincode == 802131] <- "Eastern"

data.zip <- data.zip %>%
  select(Pincode, Region.Quota)

data.zip <- data.zip[!duplicated(data.zip),]

write.csv(data.zip,"IN_zipcode.csv", row.names=F)
