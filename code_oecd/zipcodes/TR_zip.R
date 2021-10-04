library("openxlsx")
library(dplyr)
library(rjson)

## Step 1: Get zipcode
# source : https://github.com/muratgozel/turkey-neighbourhoods
data.zip <- fromJSON(file = "/Users/Bluebii/Downloads/index.json")
data.zip <-  as.data.frame(matrix(unlist(data.zip), ncol =length(unlist(data.zip[1])), byrow = T))

# Prepare variables
colnames(data.zip) <- c("Province", "District", "District.Main.Town", "Neighbourhood", "Postal.Code")
data.zip$District <- toupper(data.zip$District)
data.zip$Province <- toupper(data.zip$Province)

# Generate regions based on Provinces
aegan_provinces <- c("UŞAK","MUĞLA","MANİSA", "MANISA","KÜTAHYA","İZMİR", "İZMIR", "DENİZLİ", "DENIZLI",
                     "AFYONKARAHİSAR", "AFYONKARAHISAR", "AYDIN")
black_sea_provinces <- c("ZONGULDAK","TRABZON","TOKAT","SİNOP", "SINOP","SAMSUN","RİZE", "RIZE",
                         "ORDU","KASTAMONU","KARABÜK", "GÜMÜŞHANE", "GİRESUN", "GIRESUN",
                         "DÜZCE", "ÇORUM", "AMASYA", "ARTVİN", "ARTVIN", "BARTIN",
                         "BAYBURT", "BOLU")

central_anatolia_provinces <- c("ANKARA", "AKSARAY", "ÇANKIRI", "ESKİŞEHİR", "ESKIŞEHIR", "KARAMAN", "KAYSERİ", "KAYSERI",
                                "KIRIKKALE", "KIRŞEHİR", "KIRŞEHIR", "KONYA", "NEVŞEHİR", "NEVŞEHIR")

eastern_anatolia_provinces <- c("AĞRI", "ARDAHAN", "BİNGÖL", "BINGÖL", "BİTLİS", "BITLIS", "ELAZIĞ", "ERZİNCAN", "ERZINCAN", 
                                "ERZURUM", "HAKKARİ", "HAKKARI", "IĞDIR", "KARS", "MALATYA","MUŞ", "TUNCELİ", "TUNCELI", "VAN")

southeastern_anatolia_provinces <- c("ADIYAMAN", "BATMAN", "DİYARBAKIR", "DIYARBAKIR", "GAZİANTEP", "GAZIANTEP", "KİLİS", "KILIS",
                                     "MARDİN", "MARDIN", "ŞANLIURFA", "SİİRT", "SIIRT", "ŞIRNAK", "NİĞDE", "NIĞDE","SİVAS", "SIVAS","YOZGAT")

marmara_provinces <- c("BALIKESİR", "BALIKESIR", "BİLECİK", "BILECIK", "BURSA", "ÇANAKKALE", "EDİRNE", "EDIRNE",
                       "İSTANBUL", "KIRKLARELİ", "KIRKLARELI", "KOCAELİ", "KOCAELI", "SAKARYA", "TEKİRDAĞ", "TEKIRDAĞ",
                       "YALOVA")
mediterranean_provinces <- c("OSMANİYE", "OSMANIYE","MERSİN", "MERSIN","KAHRAMANMARAŞ", "ISPARTA", "HATAY",
                             "ADANA", "ANTALYA", "BURDUR")
data.zip$Region <- ""
data.zip$Region[data.zip$Province %in% c(aegan_provinces, mediterranean_provinces)] <- "West"
data.zip$Region[data.zip$Province %in% marmara_provinces] <- "Marmara"
data.zip$Region[data.zip$Province %in% c(black_sea_provinces, central_anatolia_provinces)] <- "Central"
data.zip$Region[data.zip$Province %in% c(eastern_anatolia_provinces, southeastern_anatolia_provinces)] <- "East"

## Step 2: Get population Data
# source: https://biruni.tuik.gov.tr/medas/?kn=95&locale=tr
# ADNKS-GK137474-O29001 İbbs-Düzey1, İbbs-Düzey2, İl Ve İlçe Nüfusları / 2020 / İlçe Düzeyi / HEPSI
data.pop <- read.csv(file = "/Users/Bluebii/Downloads/pivot_district.csv", sep = "|", skip = 4)

# Prepare variables
data.pop <- data.pop[1:NROW(data.pop)-1,]
data.pop$District <- sub('\\).*$', '', data.pop$X.1)
data.pop$District <- toupper(sub('.*\\(', '', data.pop$District))
data.pop$Province <- toupper(sub('\\(.*$', '', data.pop$X.1))

# Create population variables
data.pop$Pop.urban <- data.pop$X.3
data.pop$Pop.rural <- data.pop$X.2
data.pop$Pop.total <- rowSums(data.pop[,c("X.3", "X.2")], na.rm =T)

data.pop$share.rural <- data.pop$Pop.rural/data.pop$Pop.total
data.pop$share.rural[is.na(data.pop$share.rural)] <- 0

# Share of rural population according to our definition
sum(data.pop$Pop.total[data.pop$share.rural >= mean(data.pop$share.rural)])/sum(data.pop$Pop.total)
data.pop$Rural.urban <- "Urban"
data.pop$Rural.urban[data.pop$share.rural >= mean(data.pop$share.rural)] <- "Rural"


## Step 3: Merge the data together
merge_1 <- merge(x = data.zip, y=data.pop[,c(6:NCOL(data.pop))], by = c("Province", "District"), all.x = T)

data.final <- merge_1[, c("Postal.Code", "Region", "Rural.urban")]
data.final <- data.final[!duplicated(data.final),]

write.csv(data.final,"TR_zipcode.csv", row.names=F)
