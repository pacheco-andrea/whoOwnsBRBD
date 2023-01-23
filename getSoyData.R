#### get soy production data ####

# a script that reads and prepares the soy data production/export data
# to define/identify the areas/municipalities under most "threat" from trade
# output is a new dataset with animal/vegetable exports per muni during 1997-2022

# author: Andrea Pacheco
# first run: january 2023

# libraries
library(ggplot2)
library(dplyr)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# Comex stat - the ministry of economics "exterior commerce open data" ----
# downloand and unzip data 
comexData_url <- "https://balanca.economia.gov.br/balanca/bd/comexstat-bd/mun/EXP_COMPLETA_MUN.zip"
comexData_destination <- paste0(wdmain, "/data/raw/soy_production_exports/comex/EXP_COMPLETA_MUN.zip")
download.file(comexData_url, comexData_destination)
unzip(zipfile = paste0(wdmain, "/data/raw/soy_production_exports/comex/EXP_COMPLETA_MUN.zip"),
      exdir = paste0(wdmain, "/data/raw/soy_production_exports/comex/EXP_COMPLETA_MUN"), 
      overwrite = TRUE)

# read comex data
setwd(paste0(wdmain, "/data/raw/soy_production_exports/comex/"))
list.files()
expcompleta <- read.csv("EXP_COMPLETA_MUN/EXP_COMPLETA_MUN.csv", sep=";") 
head(expcompleta)
# SH4 code is the information on what kind of product is exported
length(unique(expcompleta$SH4)) 

# read translating table for SH4 (this was manually created from TABELAS AUXILIARES)
transTable <- read.csv("translatingTable_SH.csv")
# source: "TABLAS AUXILIARES" Sheet 1 which is relevant for the SH4 categories
head(transTable)
colnames(transTable)
sh4 <- data.frame("SH4" = transTable$CO_SH4, "name" = transTable$NO_SH4_ING)
sh4_2 <- sh4 %>% distinct(SH4, name, .keep_all = T)

# according to these categories, I believe the following are the relevant categories we should keep:
# 101-511: animal products
# 601-1404 vegetable prods
# 1501-1522 animal or veg fats
# (not keeping raw hides and skins)
# 4401-4602 is wood and articles of wood
# 4701-4911 wood pulp and other fibrous material

mySH4 <- sh4_2[which(sh4_2$SH4 >= 101 & sh4_2$SH4 <= 1522 |
                       sh4_2$SH4 >= 4401 & sh4_2$SH4 <= 4911),] 
grep("soy", mySH4$name, ignore.case = T) # can see only these two categories distinguish by soy
mySH4[c(106, 132),]

# write out this as a translating table in my processed data
setwd(paste0(wdmain, "/data/processed/soyExports"))
write.csv(mySH4, "translatingTable-SH4_biomassProds.csv", row.names = F)

# join comex data with translating table
# and only keep rows from comex which have values from my translating table 
myComex <- inner_join(expcompleta, mySH4, by = "SH4")
summary(myComex)
colnames(myComex)
myComex[,1:8]
length(unique(myComex$CO_MUN))

# write out export data with sh4 name
setwd(paste0(wdmain, "/data/processed/soyExports"))
write.csv(myComex, "comex-biomassExports_1997-2022.csv", row.names = FALSE)




