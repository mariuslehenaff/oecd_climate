library(dplyr)
library(data.table)

## This script creates all zipcodes for countries for which we only have the starting number

# data <- read.csv(file = "/Users/Bluebii/Downloads/zip_spain.csv", header=T, sep=";")

# 2. CH
# data <- read.csv(file = "/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/data/zipcodes/china_region_quota_zip.csv", header=T, sep=";")
# data<-data %>%
#  select(Region_quota, zip_start)

# 3. ID
# data <- read.csv(file="/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/oecd_climate/data/zipcodes/Indonesia_zipcode.csv", header=T, sep=";")
# data$zip_start[data$zip_start>10] <- data$zip_start[data$zip_start>10]/10
# data <- data[c(2,1)]

# Change this var. in function of the format
zip.size = 10000
# 1,000 ESP; 10,000 CH/ID

#1.ESP


generate_zip <- function(vector){
  interval.start <- as.numeric(vector[[2]])*zip.size
#  if(vector[[2]] < 9){
  interval.end <- (as.numeric(vector[[2]])+1)*zip.size-1
#  }
#  else if(vector[[2]]>=9)
#  {
#    interval.end <- (as.numeric(vector[[2]]))*zip.size+999
#  }
  zip <- c(interval.start:interval.end)

  df <- data.frame(zip, vector[[1]]) %>%
    setNames(c("zip", "region"))
  
  return(df)
}

result <- apply(data, 1, generate_zip)
result <- rbindlist(result, fill=T)

#1.ESP
# write.csv(result,"ES_zipcode.csv", row.names=T)
#2. CH
# write.csv(result,"CH_zipcode.csv", row.names=T)
# 3. ID
# write.csv(result,"ID_zipcode.csv", row.names=T)
