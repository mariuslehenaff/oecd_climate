library(rvest)
library(tidyverse)
library(data.table)

url <- "http://www.citypopulation.de/en/china/townships/"

# Get links
links <- url %>%
  read_html() %>%
  html_nodes(xpath = '//div//div[@class="mcolboxes slimboxes"]') %>%
  html_nodes("a") %>%
  html_attr("href")

# Get names of Prefectures
names <- url %>%
  read_html() %>%
  html_nodes(xpath = '//div//div[@class="mcolboxes slimboxes"]') %>%
  html_nodes("a") %>%
  html_text()


urls <- links %>%
  sapply(function(x) paste("http://www.citypopulation.de/en/china/townships/", x, sep=""))

pages <- as.data.frame(cbind(urls[1:(length(urls)-1)], names[1:(length(names)-1)]))


# table <- url %>%
#   read_html() %>%
#   html_nodes(xpath = '//table[@class="data"]') %>%
#   html_nodes(xpath = '//tbody[@class="admin1"]') %>%
#   html_nodes("span") %>%
#   html_text
#   html_attr(xpath = '//span[@itemprop="name"]') %>%
#   html_text

# Function to extract table from each page and add the province
get_table <- function(row){
  url <- row[[1]]
  prefecture <- row[[2]]
  metropole <- c("Bĕijīng Shì","Chóngqìng Shì", "Shànghăi Shì", "Tiānjīn Shì", "Dōngguān Shì", "Zhōngshān Shì")
  
  if(prefecture %in% metropole){
  # Get the table from the URL
    table <- url %>%
      read_html() %>%
      html_nodes(xpath = '//table[@class="data"]') %>%
      html_table()
    table <- table[[1]]
    table$Prefecture <- prefecture
    if(!(prefecture %in% c("Dōngguān Shì", "Zhōngshān Shì"))){
      # delete last row as it contains province-wide info
      table <- table[c(2:nrow(table)-1),]
    }else{
      table <- table[c(3:nrow(table)-1),]
    }
  }else{
  # Get the table from the URL
    
  table <- url %>%
    read_html() %>%
    html_nodes(xpath = '//table[@class="data"]') %>% ## can try @id="ts" instead
    html_table()
  table <- table[[1]]
  table$Prefecture <- prefecture
  }
  
  
  return(table)
}


for (i in c(1:342)){
  html_table(html_nodes(read_html(pages[i,1]), xpath = '//table[@class="data"]'))[[1]]
}

final.table <- 1:nrow(pages) %>%
  sapply(function(x) get_table(pages[x,]))

final.table <- rbindlist(final.table, fill=T)
final.table$pop <- as.numeric(gsub(",", "", final.table$`PopulationCensus2010-11-01`))

# sum(final.table$pop, na.rm=T)
# sum(final.table$pop[final.table$Prefecture=="Tiānjīn Shì"], na.rm=T)

table.no.district <- final.table %>%
  subset(!(Status == "District" & Prefecture %in% c("Bĕijīng Shì","Chóngqìng Shì", "Shànghăi Shì", "Tiānjīn Shì")))

# sum(final.table$pop[!(final.table$Status %in% c("District"))], na.rm=T)

sum(table.no.district$pop, na.rm=T)
sum(table.no.district$pop[table.no.district$Status=="Rural Township"], na.rm=T)/sum(table.no.district$pop, na.rm=T)
sum(table.no.district$pop[table.no.district$pop<50000], na.rm=T)/sum(table.no.district$pop, na.rm=T)

true <- (table.no.district$Status %in% c("Rural Township", "Semi-urban Area")) | (table.no.district$Status=="Town" & table.no.district$pop<50000)
sum(table.no.district$pop[true], na.rm=T)/sum(table.no.district$pop, na.rm=T)

# TODO: put district as a variable instead of a row

table.no.district$category <- word(table.no.district$Name,-1)


### ZIP
#china.zip <- read.csv2(file="/Users/Bluebii/Downloads/cnarea_2020.csv",header=T, sep=",")
