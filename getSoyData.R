#### get soy production data ####
# goals: 
# a script that reads soy production/export data from comex
# and figures out a way to link this as close as possible to properties
# output would ideally be number of soy produced per muni per diff years

# author: Andrea Pacheco
# first run: 

# libraries
library(ggplot2)
library(dplyr)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# read data from Comex stat - the ministry of economics "exterior commerce open data" ----
setwd(paste0(wdmain, "/data/raw/soy_production_exports/comex"))
list.files()
# source: https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta
# according to the website, columns are:
# ano, mês, código SH4, código de país de destino/origem do produto, código da UF do domicílio fiscal da empresa, código do município domicílio fiscal da empresa exportadora/importadora, quilograma líquido, valor dólar FOB (US$)
expcompleta <- read.csv("EXP_COMPLETA_MUN.csv", sep=";") 
head(expcompleta)
nrow(expcompleta) # 18272017
length(unique(expcompleta$SH4)) # SH4 will be translated using:

# read translating table
transTable <- read.csv("translatingTable_SH.csv")
# source: "TABLAS AUXILIARES" Sheet 1 which is relevant for the SH4 categories
head(transTable)
colnames(transTable)
sh4 <- data.frame("SH4" = transTable$CO_SH4, "name" = transTable$NO_SH4_ING)
sh4_2 <- sh4 %>% distinct(SH4, name, .keep_all = T)

# according to this, we should keep the following categories from SH4:
# 101-511: animal products
# 601-1404 vegetable prods
# 1501-1522 animal or veg fats
# (not keeping raw hids and skins)
# 4401-4602 is wood and articles of wood
# 4701-4911 wood pulp and other fibrous material

mySH4 <- sh4_2[which(sh4_2$SH4 >= 101 & sh4_2$SH4 <= 1522 |
                       sh4_2$SH4 >= 4401 & sh4_2$SH4 <= 4911),] 
grep("soy", mySH4$name, ignore.case = T)
mySH4[c(106, 132),]

biomass_prods <- expcompleta[which(expcompleta$SH4 >= 101 & expcompleta$SH4 <= 1522 |
                                        expcompleta$SH4 >= 4401 & expcompleta$SH4 <= 4911),] 
nrow(biomass_prods)
length(unique(biomass_prods$CO_MUN)) # would it be possible certain municipalities simply don't produce animal prods?


