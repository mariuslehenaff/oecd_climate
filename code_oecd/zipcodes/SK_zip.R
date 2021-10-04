library(xlsx)
library(dplyr)
library(stringr)

## STEP 1 : Get the zip code files and prepare the final csv

# 1.A : Get the files and clean the data
# source : https://www.epost.go.kr/search/zipcode/areacdAddressDown.jsp
# 시스템에 사용할 경우 지역별 주소 DB 사용을 권장 (2021.09.03)
files <- list.files(path = "/Users/Bluebii/Downloads/zipcode_DB-1", pattern = "\\.txt$")

# Append all the files downloaded into one dataframe
data.zip <- do.call("rbind", lapply(files, FUN = function(x) {
  read.table(file = paste("/Users/Bluebii/Downloads/zipcode_DB-1/", x, sep =""), sep = "|",
             header = T, encoding = "UTF-8", fill = T, quote = "")}))

colnames(data.zip)[c(1,3)] <- c("Postal.code", "Province")
data.zip$division.1 <- sub('.*-', '', data.zip[,5]) # Can either be "Gu" / "Gun" / 'Si' / Na if Sejong-Si
data.zip$division.1[is.na(data.zip$division.1)] <- "si"
data.zip$division.2 <- sub('.*-', '', data.zip[,7]) # Can either be "Myeon" / "Eup" / or blank;NA if a Dong
data.zip$division.2[is.na(data.zip$division.2) | data.zip$division.2 == ""] <- "dong"


data.zip$Region <- ""
data.zip$Region[data.zip$Province %in% c("Seoul")] <- "Seoul"
data.zip$Region[data.zip$Province %in% c("Gyeonggi-do", "Incheon", "Gangwon-do")] <- "North"
data.zip$Region[data.zip$Province %in% c("Chungcheongnam-do", "Chungcheongbuk-do", "Daejeon", "Sejong-si")] <- "Central-West" <- "West"
data.zip$Region[data.zip$Province %in% c("Jeollanam-do", "Jeollabuk-do", "Gwangju", "Jeju-do")] <- "South-West" <- "West"
data.zip$Region[data.zip$Province %in% c("Gyeongsangnam-do", "Gyeongsangbuk-do", "Daegu", "Busan", "Ulsan")] <- "East"

data.zip.short <- data.zip[, c("Postal.code", "Region", "division.2", "division.1")]

# 1.B : Check for duplicates
duplicates <- data.zip.short  %>%
  group_by(Postal.code) %>%
  summarise(dup.div1 = length(unique(division.1)), dup.div2 = length(unique(division.2)), dup.Reg = length(unique(Region))) %>%
  ungroup()

View(data.zip[data.zip$Postal.code %in% duplicates[duplicates$dup.div2 > 1,]$Postal.code,])
View(data.zip[data.zip$Postal.code %in% duplicates[duplicates$dup.div2 > 2,]$Postal.code,])

data.zip.short$Rural.urban <- ""
data.zip.short$Rural.urban[data.zip.short$division.1 == "gun"] <- "District"
data.zip.short$Rural.urban[data.zip.short$division.1 == "si"] <- "Town"
data.zip.short$Rural.urban[data.zip.short$division.1 == "gu"] <- "City"

data.final <- data.zip.short[, c("Postal.code", "Region", "Rural.urban")]
data.final <- data.final[!duplicated(data.final),]

write.csv(data.final,"SK_zipcode.csv", row.names=F)
## STEP 2 : Collect population data

# 2.A : Municipal level
# source : https://www.index.go.kr/potal/main/EachDtlPageDetail.do?idx_cd=1200
# file "2020년 도시지역 인구현황(시군구별).xls"
data.pop <- read.xlsx(file = "/Users/Bluebii/Downloads/2020년+도시지역+인구현황(시군구별).xls",
                      startRow = 7, header = F, sheetIndex = 1)
data.pop <- data.pop[1:NROW(data.pop)-1,]

# Get the last character that corresponds to the administrative level
data.pop$division <- str_sub(data.pop$X1, start = -1)

# Remove supra level
data.pop <- data.pop[data.pop$division != "계",]

# Change division to latin alphabet
data.pop$division[data.pop$division == "구"] <- "Gu"
data.pop$division[data.pop$division == "군"] <- "Gun"
data.pop$division[data.pop$division == "시"] <- "Si"

sum(data.pop$X2[data.pop$division == "Gun"])/sum(data.pop$X2)
sum(data.pop$X2[data.pop$division == "Gu"])/sum(data.pop$X2)
sum(data.pop$X2[data.pop$division == "Si"])/sum(data.pop$X2)


# 2.B : Sub-Municipal
# source : https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1B04005N
# Level 3; Total Population (2020-08)
data.pop2 <- read.xlsx(file = "/Users/Bluebii/Downloads/행정구역_읍면동_별_5세별_주민등록인구_2011년__20210907174420.xlsx",
                      startRow = 3, header = F, sheetIndex = 1)

data.pop2$division <- str_sub(data.pop2$X1, start = -1)

data.pop2$division[data.pop2$division == "국"] <- "National"
data.pop2$division[data.pop2$division == "시"] <- "Si"
data.pop2$division[data.pop2$division == "구"] <- "Gu"
data.pop2$division[data.pop2$division == "동"] <- "Dong"
data.pop2$division[data.pop2$division == "군"] <- "Gun"
data.pop2$division[data.pop2$division == "읍"] <- "Eup"
data.pop2$division[data.pop2$division == "면"] <- "Myeon"
data.pop2$division[data.pop2$division == "소"] <- "Admin"
data.pop2$division[data.pop2$division == "도"] <- "Do"

data.pop2$admin.level[data.pop2$division %in% c("National")] <- 0
data.pop2$admin.level[data.pop2$division %in% c("Do")] <- 1
data.pop2$admin.level[data.pop2$division %in% c("Si", "Gun", "Gu")] <- 2
data.pop2$admin.level[data.pop2$division %in% c("Dong", "Eup", "Myeon")] <- 3

sum(data.pop2$X3[data.pop2$division == "Dong"], na.rm =T)/sum(data.pop2$X3[data.pop2$admin.level == 3], na.rm =T)
sum(data.pop2$X3[data.pop2$division == "Eup"], na.rm =T)/sum(data.pop2$X3[data.pop2$admin.level == 3], na.rm =T)
sum(data.pop2$X3[data.pop2$division == "Myeon"], na.rm =T)/sum(data.pop2$X3[data.pop2$admin.level == 3], na.rm =T)
