library(rvest)
library(tidyverse)
library(data.table)

url <- "https://en.wikipedia.org/wiki/List_of_counties_in_China"

table <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill = T)
table <- table[[1]]

table <- table[2:NROW(table),]

table$pop <- as.numeric(gsub(",", "", table$`Population Census 2010`))

table$Rural.urban <- ""
# Include Autonomous county and county
table$Rural.urban[grepl("ounty", table$Type)] <- "Rural"
# Include District, City, District and New Area, District (Islands), Administrative committee, Banner, Autonomous banner
table$Rural.urban[grepl("committee|District|City|anner", table$Type)] <- "Urban"

sum(table$pop[table$Rural.urban=="Rural"], na.rm= T)/sum(table$pop, na.rm=T)
sum(table$pop[table$Rural.urban=="Urban"], na.rm= T)/sum(table$pop, na.rm=T)