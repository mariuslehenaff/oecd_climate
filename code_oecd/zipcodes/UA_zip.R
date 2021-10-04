library(openxlsx)
library(dplyr)

# Source : https://data.gov.ua/dataset/post-index-and-braches
# file : Відомості за липень 2021
# Tips: it can be difficult to directly open the CSV as it is in Cyrillic. Therefore, we first import it to Excel,
# using the "Cyrillic (Windows)" encoding, save it as a xlsx, and then import it in R.
data.zip <- read.xlsx( "/Users/Bluebii/Downloads/postindx.xlsx", sheet  = 1)

# The first word of the locality indicates the type of locality. We extract it.
data.zip$type <- sub(" .*$", "", data.zip$Locality)

data.zip$Rural.urban <- ""
data.zip$Rural.urban[data.zip$type %in% c("City", "Urban")] <- "Urban"
data.zip$Rural.urban[data.zip$type %in% c("Village", "Settlement")] <- "Rural"

# We use this variable instead of the one in the Latin columns (which have missing values)
colnames(data.zip)[4] <- "Postcode"

# There is one postcode, that is assigned to both rural and urban locations
# It's in the suburbs of Odessa, therefore we assign it to Urban
data.zip$Rural.urban[data.zip$Postcode == 67804] <- "Urban"

# Create the regions according to our quota
data.zip$Region <- ""
data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Chernihivska", "Poltavska", "Sumska")] <- "Center-East" <- "Center"
data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Cherkaska", "Kirovohradska", "Kyivska", "Vinnytska", "Zhytomyrska")] <- "Center-West" <- "Center"
data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Donetska", "Kharkivska", "Luhanska")] <- "East"
data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Dnipropetrovska", "Khersonska", "Mykolaivska", "Odeska", "Zaporizka")] <- "South"
data.zip$Region[data.zip$`Region.(Oblast)` %in% c("Chernivetska", "Ivano-Frankivska", "Khmelnytska", "Lvivska", "Rivnenska", "Ternopilska", "Volynska", "Zakarpatska")] <- "West"


# duplicates <- data.zip  %>%
#   group_by(Postcode) %>%
#   summarise(dup.rural = length(unique(Rural.urban)), dup.reg = length(unique(Region))) %>%
#   ungroup()
# table(duplicates$dup.rural)

data.final <- data.zip[,c("Postcode", "Region", "Rural.urban")]
data.final <- data.final[!duplicated(data.final),]

write.csv(data.final, "UA_zipcode.csv", row.names = F)
