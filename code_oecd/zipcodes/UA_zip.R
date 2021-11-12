library(openxlsx)
library(dplyr)

# Source : https://data.gov.ua/dataset/post-index-and-braches
# DEPRECATED file : Відомості за липень 2021
# file : "Перелік поштових індексів та відділень ... Популярні"
# Tips: it can be difficult to directly open the CSV as it is in Cyrillic. Therefore, we first import it to Excel,
# using the "Cyrillic (Windows)" encoding, save it as a xlsx, and then import it in R.
# data.zip <- read.xlsx( "/Users/Bluebii/Downloads/postindx.xlsx", sheet  = 1)
data.zip.10_09_2021 <- read.xlsx( "/Users/Bluebii/Downloads/postindex-10-09-2021.xlsx", sheet  = 1)
data.zip.Avril <- read.xlsx( "/Users/Bluebii/Downloads/post_code_UA_Avril.xlsx", sheet  = 1)
# The first word of the locality indicates the type of locality. We extract it.
data.zip.10_09_2021$type <- sub(" .*$", "", data.zip.10_09_2021$Locality)
data.zip.Avril$type <- sub(" .*$", "", data.zip.Avril$Locality)

data.zip.10_09_2021$Rural.urban <- ""
data.zip.10_09_2021$Rural.urban[data.zip.10_09_2021$type %in% c("City", "Urban")] <- "Urban"
data.zip.10_09_2021$Rural.urban[data.zip.10_09_2021$type %in% c("Village", "Settlement")] <- "Rural"

data.zip.Avril$Rural.urban <- ""
data.zip.Avril$Rural.urban[data.zip.Avril$type %in% c("City", "Urban")] <- "Urban"
data.zip.Avril$Rural.urban[data.zip.Avril$type %in% c("Village", "Settlement")] <- "Rural"


# We use this variable instead of the one in the Latin columns (which have missing values)
colnames(data.zip.10_09_2021)[4] <- "Postcode"
colnames(data.zip.Avril)[4] <- "Postcode"


data.zip.10_09_2021 <- data.zip.10_09_2021[,c("Postcode", "Rural.urban")]
data.zip.Avril <- data.zip.Avril[,c("Postcode", "Rural.urban")]
data.zip.Avril <- data.zip.Avril[!(data.zip.Avril$Postcode %in% data.zip.10_09_2021$Postcode),]

data.zip.Avril <- data.zip.Avril[!duplicated(data.zip.Avril),]
data.zip.10_09_2021 <- data.zip.10_09_2021[!duplicated(data.zip.10_09_2021),]
data.final <- rbind(data.zip.10_09_2021, data.zip.Avril)
# # There is one postcode, that is assigned to both rural and urban locations
# # It's in the suburbs of Odessa, therefore we assign it to Urban
# data.zip.10_09_2021$Rural.urban[data.zip.10_09_2021$Postcode == 67804] <- "Urban"
# 
# # Create the regions according to our quota
# data.zip$Region <- ""
# data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Chernihivska", "Poltavska", "Sumska")] <- "Center-East" <- "Center"
# data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Cherkaska", "Kirovohradska", "Kyiv", "Kyivska", "Vinnytska", "Zhytomyrska")] <- "Center-West" <- "Center"
# data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Donetska", "Kharkivska", "Luhanska")] <- "East"
# data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Dnipropetrovska", "Khersonska", "Mykolaivska", "Odeska", "Zaporizka")] <- "South"
# data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Chernivetska", "Ivano-Frankivska", "Khmelnytska", "Lvivska", "Rivnenska", "Ternopilska", "Volynska", "Zakarpatska")] <- "West"
# 
# data.zip$Char_zip <- ""
# data.zip$Char_zip[data.zip$Postcode < 10000] <- paste0(0, as.character(data.zip$Postcode[data.zip$Postcode<10000]))
# data.zip$Char_zip[data.zip$Postcode >= 10000] <- as.character(data.zip$Postcode[data.zip$Postcode>=10000])
# 
# data.zip$indicateur <- substr(data.zip$Char_zip, 1, 2)
# 
# # duplicates <- data.zip  %>%
# #   group_by(Postcode) %>%
# #   summarise(dup.rural = length(unique(Rural.urban)), dup.reg = length(unique(Region))) %>%
# #   ungroup()
# # table(duplicates$dup.rural)
# 
# data.final <- data.zip[,c("Postcode", "Region", "Rural.urban")]
# data.final <- data.final[!duplicated(data.final),]
# 
write.csv(data.final, "UA_zipcode.csv", row.names = F)
