library(dplyr)
library(readxl)

#source: https://www.correosdemexico.gob.mx/SSLServicios/ConsultaCP/CodigoPostal_Exportar.aspx

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Create list w/ all sheets. Then remove first one and append each one.
xl.sheets <- read_excel_allsheets("/Users/Bluebii/Downloads/CPdescarga.xls")
xl.sheets <- xl.sheets[2:length(xl.sheets)]
data.zip <- do.call(rbind, xl.sheets)

# Step 1: Assign regions
data.zip$Region.quota <- ""
data.zip$Region.quota[data.zip$d_estado %in% c("Aguascalientes", "Colima", "Jalisco", "Guanajuato", "Michoacán de Ocampo",
                                               "Nayarit", "San Luis Potosí", "Zacatecas")] <- "Central-Western"
data.zip$Region.quota[data.zip$d_estado %in% c("Ciudad de México", "Hidalgo", "México", "Morelos", "Puebla", "Querétaro",
                                               "Tlaxcala")] <- "Central-Eastern"
data.zip$Region.quota[data.zip$d_estado %in% c("Coahuila de Zaragoza", "Nuevo León", "Tamaulipas")] <- "North-East"
data.zip$Region.quota[data.zip$d_estado %in% c("Baja California", "Baja California Sur", "Chihuahua", "Durango", "Sinaloa",
                                               "Sonora")] <- "North-West"
data.zip$Region.quota[data.zip$d_estado %in% c("Campeche", "Chiapas", "Guerrero", "Oaxaca", "Quintana Roo", "Tabasco",
                                               "Veracruz de Ignacio de la Llave", "Yucatán")] <- "South"

# Check for duplicates (== different regions for same pincode)
data.zip$Rural.num[data.zip$d_zona == "Rural"] <- 1
data.zip$Rural.num[data.zip$d_zona == "Semiurbano"] <- 2
data.zip$Rural.num[data.zip$d_zona == "Urbano"] <- 3

duplicate.rural <- data.zip %>%
  group_by(d_codigo) %>%
  summarise(dup.rural.urban = length(unique(d_zona)), Rural.max = max(Rural.num)) %>%
  ungroup()

# We assign to each zip code, the most urban category
merge_1 <- merge(x=data.zip, y=duplicate.rural, by=c("d_codigo"), all.x = T)

# Transform numeric back to text
merge_1$Rural.quota <- ""
merge_1$Rural.quota[merge_1$Rural.max == 1] <- "Rural"
merge_1$Rural.quota[merge_1$Rural.max == 2] <- "Semiurbano"
merge_1$Rural.quota[merge_1$Rural.max == 3] <- 'Urbano'

merge_1 <- merge_1 %>%
  select(d_codigo, Region.quota, Rural.quota)

merge_1 <- merge_1[!duplicated(merge_1),]

write.csv(merge_1,"MX_zipcode.csv", row.names=F)
