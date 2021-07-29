library(dplyr)
# source: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Table.cfm?Lang=Eng&T=1201&SR=1&S=22&O=A&RPP=9999&PR=0
data.zip <- read.csv2(file = "/Users/Bluebii/Downloads/T120120210729115405.csv", sep = ",")
data.zip <- data.zip[c(2:(NROW(data.zip)-7)),]

# For rural we get check if second character of FSA is 0
data.zip$Rural.urban.quota <- "Urban"
data.zip$Rural.urban.quota[substr(data.zip$Geographic.code, 2, 2) == 0] <- "Rural"
sum(data.zip$Population..2016[data.zip$Rural.urban.quota == "Rural"])/sum(data.zip$Population..2016)

# Matches: https://environicsanalytics.com/resources/blogs/ea-blog/2017/02/08/first-data-from-the-2016-census#:~:text=In%202016%2C%2016.8%20percent%20of,percent%2C%20growth%20rates%20varied%20widely.
# 18.7% here: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Table.cfm?Lang=Eng&T=703&S=87&O=A

# More details, here: https://www.canadapost-postescanada.ca/cpc/en/support/articles/addressing-guidelines/postal-codes.page?
summary(data.zip$Population..2016[substr(data.zip$Geographic.code, 2, 2) == 0])
summary(data.zip$Population..2016)

data.zip$Region.quota <- ""
data.zip$Region.quota[substr(data.zip$Geographic.code, 1, 1) %in% c("T", "V", "X", "Y")] <-"North_West"
data.zip$Region.quota[substr(data.zip$Geographic.code, 1, 1) %in% c("R", "S")] <- "Central"
data.zip$Region.quota[substr(data.zip$Geographic.code, 1, 1) %in% c("K", "L", "M", "N", "P")] <-"Ontario"
data.zip$Region.quota[substr(data.zip$Geographic.code, 1, 1) %in% c("G", "H", "J")] <-"Quebec"
data.zip$Region.quota[substr(data.zip$Geographic.code, 1, 1) %in% c("A", "B", "C", "E")] <-"East"

sum(data.zip$Population..2016[data.zip$Region.quota == "North_West"])/sum(data.zip$Population..2016)
sum(data.zip$Population..2016[data.zip$Region.quota == "Central"])/sum(data.zip$Population..2016)
sum(data.zip$Population..2016[data.zip$Region.quota == "Ontario"])/sum(data.zip$Population..2016)
sum(data.zip$Population..2016[data.zip$Region.quota == "Quebec"])/sum(data.zip$Population..2016)
sum(data.zip$Population..2016[data.zip$Region.quota == "East"])/sum(data.zip$Population..2016)

data.zip$FSA <- data.zip$Geographic.code

data.zip <- data.zip %>%
  select(FSA, Rural.urban.quota, Region.quota)

write.csv(data.zip,"CA_zipcode.csv", row.names=F)
