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


## Check indicator

data.zip$indicator <- as.numeric(substr(data.zip$Pincode, 1, 2))

data.zip$indicator[data.zip$StateName %in% c("ANDAMAN AND NICOBAR ISLANDS", "CHANDIGARH", "THE DADRA AND NAGAR HAVELI AND DAMAN AND DIU", "GOA", "LAKSHADWEEP", "PUDUCHERRY", "SIKKIM")] <- NA
#  as.numeric(substr(data.zip$Pincode, 1, 3)) <- NULL

## 2. MANUALLY GENERATE ZIPCODES
  
table(data.zip$indicator[data.zip$Region.Quota == "Northern"])
table(data.zip$indicator[data.zip$Region.Quota == "Southern"])
table(data.zip$indicator[data.zip$Region.Quota == "Central"])
table(data.zip$indicator[data.zip$Region.Quota == "Eastern"])
table(data.zip$indicator[data.zip$Region.Quota == "Western"])

zipcode <- c(110000:289999, 300000:349999, 360000:539999, 560000:649999, 670000:859999)
data.final <-data.frame(zipcode)
data.final$indicator <- as.numeric(substr(data.final$zipcode, 1, 2))

data.final$Region.Quota <- ""
data.final$Region.Quota[data.final$indicator %in% c(11:19, 30:34)] <- "Northern"
data.final$Region.Quota[data.final$indicator %in% c(50:53, 56:64, 67:69)] <- "Southern"
data.final$Region.Quota[data.final$indicator %in% c(20:28, 45:49)] <- "Central"
data.final$Region.Quota[data.final$indicator %in% c(36:44)] <- "Western"
data.final$Region.Quota[data.final$indicator %in% c(70:85)] <- "Eastern"

# Here we prepare data for the pincodes that do now follow the same logic as the other pincodes
data.merge <- data.zip[data.zip$StateName %in% c("ANDAMAN AND NICOBAR ISLANDS", "CHANDIGARH", "THE DADRA AND NAGAR HAVELI AND DAMAN AND DIU", "GOA", "LAKSHADWEEP", "PUDUCHERRY", "SIKKIM"),]
data.merge$zipcode <- as.numeric(data.merge$Pincode)
data.merge <- data.merge %>%
  select(c("zipcode", "Region.Quota"))

merge_1 <- merge(x = data.final, y = data.merge, by = "zipcode", all.x= T)
# Replace those values
merge_1$Region.Quota <- merge_1$Region.Quota.x
merge_1$Region.Quota[!is.na(merge_1$Region.Quota.y)] <- merge_1$Region.Quota.y[!is.na(merge_1$Region.Quota.y)]

merge_1 <- merge_1 %>%
  select(c("zipcode", "Region.Quota"))

write.csv(merge_1,"IN_zipcode.csv", row.names=F)
