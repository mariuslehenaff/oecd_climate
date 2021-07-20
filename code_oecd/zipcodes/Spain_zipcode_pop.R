library("xlsx")
library(dplyr)
library(rjson)

## Source: https://github.com/Zeeshanahmad4/Zip-code-of-all-countries-cities-in-the-world-CSV-TXT-SQL-DATABASE
## Source 2:(Download compressed file with the municipal Excel files for each year)
# https://www.ine.es/dynt3/inebase/en/index.htm?padre=517&capsel=525

df_2 <- read.csv2("/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/Climate Survey/Questionnaire/Spain/allCountriesCSV.csv", sep=',')
df_2 <- subset(df_2, COUNTRY == "ES")
df_2 <- select(df_2, POSTAL_CODE, CITY)



data2<- read.xlsx2(file="/Users/Bluebii/Library/Mobile Documents/com~apple~CloudDocs/TRAVAIL/Jobs/Stantcheva_2020:21/OECD/Climate Survey/Questionnaire/Spain/pobmun_en/pobmun20.xlsx", sheetIndex=1, header=TRUE, colClasses="character")
data3<- read.xlsx2(file="/Users/Bluebii/Downloads/DDW_PCA0000_2011_wardlevel.xlsx", sheetIndex=1)
data2$POSTAL_CODE <- (paste(data2$CPRO,data2$CMUN, sep=""))
data2$POB20 <- as.integer(data2$POB20)
data2 <- select(data2, NOMBRE, POB20, POSTAL_CODE)

library("openxlsx")
mydf <- read.xlsx("/Users/Bluebii/Downloads/DDW_PCA0000_2011_wardlevel.xlsx", sheet = 1, colNames = TRUE)

#sum(top_n(data2, 200, POB20)$POB20)/
#  sum(data2$POB20[data2$POB20 < 20000])/sum(data2$POB20)
data2 <- rename(data2, Place_Name = NOMBRE)
data <- left_join(df_2, data2[,c("CITY", "POB20")], by="CITY")

#Abla 4001

Spain.zip <- fromJSON(file="/Users/Bluebii/Downloads/Spain_Postal_Code.json")

do <- as.data.frame(do.call(rbind, lapply(Spain.zip$results, as.vector)))
do <- cbind(my.var=rownames(do), do)
do[do == "NULL"] <- NA

data2$Admin_Code3 <- (paste(data2$CPRO,data2$CMUN, sep=""))
data2$POB20 <- as.integer(data2$POB20)
data2 <- select(data2, NOMBRE, POB20, Admin_Code3)

do$Admin_Code3 <- as.character(do$Admin_Code3)
do$Place_Name <- as.character(do$Place_Name)
data <- left_join(do, data2[,c("Place_Name", "POB20")], by="Place_Name")



library(tiff)

library(raster)

str_name<-'MOD16A2_ET_0.05deg_GEO_2008M01.tif' 

read_file<-readTIFF("/Users/Bluebii/Downloads/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0.tif") 
