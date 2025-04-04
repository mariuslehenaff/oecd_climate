library(dplyr)
library(xlsx)
library(stringr)

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

# Do some cleaning for the matching
data.pop2$Municipality.short[data.pop2$Municipality.short == "MINOH SHI"] <- "MINO SHI"

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

### PART 2: POSTAL CODES

data1 <- read.csv2("/Users/Bluebii/Downloads/KEN_ALL_ROME 2.CSV", sep=",", fileEncoding = "cp932", header=F)

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

data1$Municipality.short[data1$Municipality.short == "KUMAMOTO SHI CHUO KU"] <- "KUMAMOTO SHI CHUUOU KU"

table(!(data1$Municipality.short %in% data.pop2$Municipality.short))
df <- data1[!(data1$Municipality.short %in% data.pop2$Municipality.short),]
data1 <- data1[(data1$Municipality.short %in% data.pop2$Municipality.short),]

df$N_word <- sapply(df$Municipality.short,function(x)length(unlist(gregexpr(" ",x)))+1)

# Correcting some typos
df$Municipality.short[df$Municipality=="MIYAKEJIMA MIYAKE MURA"] <- "MIYAKE MURA"
df$Municipality.short[df$Municipality=="HACHIJOJIMA HACHIJO MACHI"] <- "HACHIJO MACHI"
df$Municipality.short[df$Municipality=="NISHIYATSUSHIRO GUN ICHIKAWAMISATO"] <- "ICHIKAWAMISATO CHO"
df$Municipality.short[df$Municipality.short=="MATSURA SHI"] <- "MATSUURA SHI"
df$Municipality.short[df$Municipality.short=="TAKIZAWA SHI"] <- "TAKIZAWASHI"
df$Municipality.short[df$Municipality.short=="NIHOMMATSU SHI"] <- "NIHONMATSU SHI"
df$Municipality.short[df$Municipality.short=="OTAWARA SHI"] <- "OTAWARA SHI"
df$Municipality.short[df$Municipality.short=="KATSURA SHI"] <- "KATSUURA SHI"
df$Municipality.short[df$Municipality.short=="KOZUSHIMA MURA"] <- "KOUZUSHIMA MURA"
df$Municipality.short[df$Municipality.short=="TOYOKA SHI"] <- "TOYOOKA SHI"
df$Municipality.short[df$Municipality.short=="SETOCHI SHI"] <- "SETOUCHI SHI"
df$Municipality.short[df$Municipality.short=="NISHINOMOTE SHI"] <- "NISHINOOMOTE SHI"
df$Municipality.short[df$Municipality.short=="MINAMIKYUSHU SHI"] <- "MINAMIKYUSYU SHI"

# A bunch of them only have the two last words instead of all last 4
df$Municipality.short[df$N_word == 4] <- paste(word(df$Municipality[df$N_word==4], 3), word(df$Municipality[df$N_word==4], 4))



# Fix 21 remaining that still do not match
# list here: unique(df$Municipality.short[!(df$Municipality.short %in% data.pop2$Municipality.short)])
df$Municipality.short[df$Municipality.short=="NAMPORO CHO"] <- "NANPORO CHO"
df$Municipality.short[df$Municipality.short=="TOMA CHO"] <- "TOHMA CHO"
df$Municipality.short[df$Municipality.short=="TAKINOE CHO"] <- "TAKINOUE CHO"
df$Municipality.short[df$Municipality.short=="TOYORA CHO"] <- "TOYOURA CHO"
df$Municipality.short[df$Municipality.short=="HIRO CHO"] <- "HIROO CHO"
df$Municipality.short[df$Municipality.short=="HAPPO CHO"] <- "HAPPOU CHO"
df$Municipality.short[df$Municipality.short=="YAMATSURI MACH"] <- "YAMATSURI MACHI"
df$Municipality.short[df$Municipality.short=="OTAWARA SHI"] <- "OHTAWARA SHI"
df$Municipality.short[df$Municipality.short=="OI CHO"] <- "OHI CHO"
df$Municipality.short[df$Municipality.short=="FUJIKAWAGUCHIKO MAC"] <- "FUJIKAWAGUCHIKO MACHI"
df$Municipality.short[df$Municipality.short=="KOMI MACHI"] <- "KOUMI MACHI"
df$Municipality.short[df$Municipality.short=="TOYOKA MURA"] <- "TOYOOKA MURA"
df$Municipality.short[df$Municipality.short=="YAMANOCHI MACHI"] <- "YAMANOUCHI MACHI"
df$Municipality.short[df$Municipality.short=="WANOCHI CHO"] <- "WANOUCHI CHO"
df$Municipality.short[df$Municipality.short=="AMPACHI CHO"] <- "ANPACHI CHO"
df$Municipality.short[df$Municipality.short=="CHIHAYAAKASAKA MU"] <- "CHIHAYAAKASAKA MURA"
df$Municipality.short[df$Municipality.short=="NACHIKATSURA CHO"] <- "NACHIKATSUURA CHO"
df$Municipality.short[df$Municipality.short=="KOTORA CHO"] <- "KOTOURA CHO"
df$Municipality.short[df$Municipality.short=="KATSURA CHO"] <- "KATSUURA CHO"
df$Municipality.short[df$Municipality.short=="SANAGOCHI SON"] <- "SANAGOUCHI SON"
df$Municipality.short[df$Municipality.short=="SETOCHI CHO"] <- "SETOUCHI CHO"

df$N_word <- NULL
data1 <- rbind(data1, df)

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



merge_1 <- merge(data1, data.pop2[,c("Prefecture", "Municipality.short", "Pop.int")], by=c("Prefecture", "Municipality.short"))

# When take first number need to add a 0 for numbers less than 1,000,000. Otherwise taking first 3 numbers create duplicates
merge_1$small.zip <- ""
merge_1$small.zip[merge_1$Postal.Code >= 1000000] <- substr(merge_1$Postal.Code[merge_1$Postal.Code >= 1000000], 1, 5)
merge_1$small.zip[merge_1$Postal.Code %in% c(100000:999999)] <- paste("0",substr(merge_1$Postal.Code[merge_1$Postal.Code %in% c(100000:999999)], 1, 4), sep="")
merge_1$small.zip[merge_1$Postal.Code < 100000] <- paste("00",substr(merge_1$Postal.Code[merge_1$Postal.Code < 100000], 1, 3), sep="")

duplicate.region <- merge_1 %>%
  group_by(small.zip) %>%
  summarise(dup.region = length(unique(Region))) %>%
  ungroup()
table(duplicate.region$dup.region) # ok!

# merge_1$Rural.urban <- ""
# merge_1$Rural.urban[merge_1$Pop.int < 100000] <- "Rural"
# merge_1$Rural.urban[merge_1$Pop.int >= 100000] <- "Urban"
# 
# duplicate.urbanity <- merge_1 %>%
#   group_by(small.zip) %>%
#   summarise(dup.urbanity = length(unique(Rural.urban))) %>%
#   ungroup()
table(duplicate.urbanity$dup.urbanity)

pop.small.zip <- merge_1 %>%
  group_by(small.zip) %>%
  summarise(pop.max = max(Pop.int)) %>%
  ungroup()

# Assign urbal, if small zip contains at least one zip w/ more than 100k
merge_1$Rural.urban <- ""
merge_1$Rural.urban[merge_1$small.zip %in% pop.small.zip$small.zip[pop.small.zip$pop.max>=100000]] <- "Urban"
merge_1$Rural.urban[merge_1$small.zip %in% pop.small.zip$small.zip[pop.small.zip$pop.max<100000]] <- "Rural"

View(merge_1[merge_1$small.zip %in% duplicate.urbanity$small.zip[duplicate.urbanity$dup.urbanity>1],])

merge_2 <- merge_1[,c("Prefecture", "Municipality.short", "Rural.urban")]
merge_2 <- merge_2[!duplicated(merge_2),]
# Check pop in rural and urban
merge_2 <- merge(x=data.pop2.unique, y=merge_2, by=c("Prefecture", "Municipality.short"), all.x = T)
table(is.na(merge_2$Rural.urban))


merge_2$Rural.urban[duplicated(merge_2[c("Prefecture", "Municipality.short")])] <- "Urban"
merge_2 <- merge_2[!duplicated(merge_2[c("Prefecture", "Municipality.short")]), ]

sum(merge_2$Pop.int[merge_2$Rural.urban == "Urban"])/sum(merge_2$Pop.int)
sum(merge_2$Pop.int[merge_2$Rural.urban == "Rural"])/sum(merge_2$Pop.int)

duplicate.urbanity <- merge_1 %>%
  group_by(small.zip) %>%
  summarise(dup.urbanity = length(unique(Region))) %>%
  ungroup()

# Fix dup small codes
merge_1$small.zip[merge_1$Municipality.short== "KISOSAKI CHO"] <- "49808"
merge_1$small.zip[merge_1$Postal.Code== 3840097] <- "37716"
merge_1$small.zip[merge_1$Postal.Code== 3890121] <- "37903"

merge_1 <- merge_1 %>%
  select(small.zip, Region, Rural.urban)

merge_1 <- merge_1[!duplicated(merge_1),]

write.csv(merge_1,"Japan_zipcode.csv", row.names=F)
