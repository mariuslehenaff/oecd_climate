library(dplyr)
library(xlsx)
library(stringr)


data1 <- read.csv2("/Users/Bluebii/Downloads/KEN_ALL_ROME.CSV", sep=",", fileEncoding = "cp932", header = F)
data1 <- data1 %>% 
  select(V1, V5, V6, V7)

data1 <- data1 %>%
  rename(Postal.Code = V1, Prefecture = V5,
         Municipality = V6, Town = V7)

data1$region <- ""


data1$Region[data1$Prefecture %in% c("HOKKAIDO", "AKITA KEN", "AOMORI KEN", 
                                     "FUKUSHIMA KEN", "IWATE KEN", "MIYAGI KEN",
                                     "YAMAGATA KEN")] <- "North"

data1$Region[data1$Prefecture %in% c("HIROSHIMA KEN", "OKAYAMA KEN", "SHIMANE KEN", 
                                     "TOTTORI KEN", "YAMAGUCHI KEN", "EHIME KEN", 
                                     "KAGAWA KEN", "KOCHI KEN", "TOKUSHIMA KEN", 
                                     "FUKUOKA KEN", "KAGOSHIMA KEN", "KUMAMOTO KEN",
                                     "MIYAZAKI KEN", "NAGASAKI KEN", "OITA KEN", 
                                     "OKINAWA KEN", "SAGA KEN")] <- "South"

data1$Region[data1$Prefecture %in% c("CHIBA KEN", "GUMMA KEN", "IBARAKI KEN", 
                                     "KANAGAWA KEN", "SAITAMA KEN", "TOCHIGI KEN",
                                     "TOKYO TO")] <- "Kanto"

data1$Region[data1$Prefecture %in% c("HYOGO KEN", "KYOTO FU", "MIE KEN", "NARA KEN",
                                     "OSAKA FU", "SHIGA KEN", "WAKAYAMA KEN")] <- "Kansai"

data1$Region[data1$Prefecture %in% c("AICHI KEN", "FUKUI KEN", "GIFU KEN",
                                     "ISHIKAWA KEN", "NAGANO KEN", "NIIGATA KEN",
                                     "SHIZUOKA KEN", "TOYAMA KEN", "YAMANASHI KEN")] <- "Chubu"

data1$Municipality.short <- (data1$Municipality)
# Source: https://www.post.japanpost.jp/zipcode/dl/roman-zip.html

# # Sourcepop: https://www.e-stat.go.jp/en/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&year=20200&month=24101210&tclass1=000001136465&tclass2=000001154388
# data.pop<- read.xlsx2(file="/Users/Bluebii/Downloads/japan_pop.xlsx", sheetIndex=1, header=TRUE, colClasses="character")
# data.pop <- data.pop %>%
#   rename(Id.area = Identification.Area.Codes)
# 
# data.pop$Population <- as.integer(data.pop$Population)
# # One NA with pop = "-"
# # Id.Area
# # a: Prefectures + Japan
# View(data.pop[data.pop$Identification.Area.Codes=="a",])
# length(unique(data.pop[data.pop$Identification.Area.Codes=="a",]$Prefectures))
# # 0: ku (198)
# # 1: shi (21)
# # 2: shi (772)
# # 3: cho/mura (926)
# 

# Source: https://www.e-stat.go.jp/en/regional-statistics/ssdsview/municipality
data.pop2 <- read.csv2("/Users/Bluebii/Downloads/FEI_CITY_210630173633.csv", sep=",", fileEncoding = "cp932", header = F)
data.pop2 <- data.pop2 %>%
  rename(Year = V2, Area.Code = V3,
         Area = V4, Pop = V6)

# Delete first blank line
data.pop2 <- data.pop2[c(2:15306),]
data.pop2$Pop.int <- as.integer(data.pop2$Pop)
data.pop2 <- data.pop2 %>%
  subset(Year == 2015)

data.pop2$Prefecture<- toupper(gsub("-"," ",word(data.pop2$Area, 1)))

# NB : "ken Takizawashi" is a "shi
data.pop2$Municipality.type <- gsub(".*-","",data.pop2$Area)

# Take two last words of the name
data.pop2$Municipality.short2 <- toupper(gsub("-", " ", data.pop2$Area %>%
                                               word(2)))

data.pop2$Municipality.short3 <- toupper(gsub("-", " ", data.pop2$Area %>%
                                                word(3)))

data.pop2$Municipality.short <- apply(cbind(data.pop2$Municipality.short2, data.pop2$Municipality.short3), 1, 
             function(x) paste(x[!is.na(x)], collapse = " "))

city.w.arrondissement <- c("CHIBA SHI","FUKUOKA SHI", "HAMAMATSU SHI", "HIROSHIMA SHI",
                           "KAWASAKI SHI", "KITAKYUSHU SHI", "KOBE SHI", 
                           "KUMAMOTO SHI", "KYOTO SHI", "NAGOYA SHI", "NIIGATA SHI",
                           "OKAYAMA SHI", "OSAKA SHI", "SAGAMIHARA SHI", "SAITAMA SHI",
                          "SAPPORO SHI", "SENDAI SHI", "SHIZUOKA SHI","YOKOHAMA SHI")

# SAKAI SHI, pb: 2 SAKAI SHI
# So need to account for prefecture as well
data.pop2.unique <- data.pop2[!(data.pop2$Municipality.short %in% city.w.arrondissement) & (data.pop2$Area!="Osaka-fu Sakai-shi"),]
sum(data.pop2.unique$Pop.int[data.pop2.unique$Municipality.type=="cho"])/sum(data.pop2.unique$Pop.int)
word(data.pop2$Municipality.short, 1)

merge_1 <- merge(data1, data.pop2[,c("Prefecture", "Municipality.short", "Pop.int")], by=c("Prefecture", "Municipality.short"))

merge_1 <- merge_1 %>%
  select(Postal.Code, Region, Pop.int)

merge_1 <- merge_1 %>%
  rename(Pop.Municipality = Pop.int)

write.csv(merge_1,"Japan_zipcode.csv", row.names=T)
