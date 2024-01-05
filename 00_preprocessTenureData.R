#### Biodiversity and Tenure in Brazil ####

# this script brings together different sources of (updated) land tenure data and preprocesses them for use in analyses
# outputs are:
# folders with parcel-level tenure data for brazil, all harmonized in same projection 
# these are also simplified versions of the above with the minimum information of an identifier, a category, and the geometry

# author: Andrea Pacheco
# first run: 23.09.2022
# last run: 15.12.2023

# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)

# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data sources:

# 1. CAR properties sourced from CSR ----
# these data were provided by the CSR (Centro de Sensoramento Remoto) of UFMG and downloaded manually
# CSR informed me they are originally sources from the SFB (servico florestal brasilero)
# they include the cateogies: IRU, AST, PCT, (imovel rural, assentamentos, and povos e comunidades tradicionais)
# data is split into a .shp for each state, and in general, i keep it this way to facilitate parallelization

setwd(paste0(wdmain,"/data/raw/LandTenure/LandTenure_v20231009/"))
l <- list.files()
files <- l[grep(".shp", l)]

# read and write out simpler files (exclude extraneous columns)
# loop through reading and writing simplified shapefiles
for (i in 1:length(files))
{
  setwd(paste0(wdmain,"/data/raw/LandTenure/LandTenure_v20231009/"))
  # read in all the shapefiles (with only specific columns established in the read_my_shp funct above), bind rows into one df
  stateShp <- read_my_shp(files[i])
  name <- paste(files[i])
  test <- st_read("cf_imoveis_imaflora_ac.shp")
  # write out
  setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
  st_write(stateShp, name)
  print(i)
}

# 2. Conservation units and indigenous lands ----

# conservation units
# uc <- read_conservation_units(date = 201909, simplified = FALSE) # latest update 201909
# # need to crop these to only the extent of terrestrial brazil
# uc <- st_transform(uc, crs = my_crs_SAaea)
# uc <- uc[-grep("TRINDADE", uc$name_conservation_unit),] # manually removed marine PAs that I wasn't able to crop with raster
# uc2 <- select(uc, c("group", "category", "creation_year")) # note, best not to consider quality column, according to amanda
# setwd(paste0(wdmain,"/data/processed/landTenure_UC/"))
# st_write(uc2, "landTenure_ConservationUnits_201909_SAalbers.shp", append=FALSE) # no longer using this data as it's outdated

# conservation units sourced directly from MMA
setwd(paste0(wdmain,"/data/raw/landTenure/UCs_MMA"))
uc_mma <- st_read("drive-download-20231212T124407Z-001/ucstodas.shp")
st_crs(uc_mma) <- my_crs_SAaea
uc_mma <- uc_mma[-grep("TRINDADE", uc_mma$NOME_UC1),] # manually removed big marine PAs that I wasn't able to crop with raster
uc_mma2 <- select(uc_mma, c("GRUPO4", "CATEGORI3", "ANO_CRIA6"))
unique(uc_mma$CATEGORI3)
# should separately consider RPPN because these are private - but these are better sourced from ICMBIO
# should NOT include APAs because they have no real protection
uc_mma2 <- uc_mma2[which(uc_mma2$CATEGORI3 != "Reserva Particular do Patrim\xf4nio Natural"),]
uc_mma2 <- uc_mma2[which(uc_mma2$CATEGORI3 != "\xc1rea de Prote\xe7\xe3o Ambiental"),]
colnames(uc_mma2) <- c("group", "category", "yearCreat", "geometry")

# write out:
setwd(paste0(wdmain,"/data/processed/landTenure_UC/"))
st_write(uc_mma2, "landTenure_UCs_MMA_20231212_SAalbers.shp", append=FALSE)

# add RPPNs:
setwd(paste0(wdmain,"/data/raw/landTenure/RPPN"))
unzip("total.zip")
rppn <- st_read("total.shp")
rppn <- st_transform(rppn, crs = my_crs_SAaea)
rppn$group <- "US" # to categorize as sustainable use
rppn$category <- "RPPN"
rppn <- select(rppn, c("group", "category"))
setwd(paste0(wdmain,"/data/processed/landTenure_RPPN"))
st_write(rppn, "landTenure_RPPN_20231212_SAalbers.shp", append=FALSE)

# indigenous lands
setwd(paste0(wdmain,"/data/raw/landTenure/FUNAI"))
unzip("tis_poligonais.zip")
ind <- st_read("tis_poligonais.shp")
table(ind$fase_ti)
# keep only regularized and homologated indigenous lands because these are the legally official ones
ind <- ind[which(ind$fase_ti == "Homologada" | ind$fase_ti == "Regularizada" ),]
ind2 <- select(ind, (c( "uf_sigla", "fase_ti","modalidade")))
# plot(ind2$geometry)
setwd(paste0(wdmain,"/data/processed/landTenure_IPLC/"))
st_write(ind2, "landTenure_indigenous_20231212_SAalbers.shp", append=FALSE)
# according to this https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/demarcacao-de-terras-indigenas
# the following indigenous lands categories
# Terras IndÃ­genas Tradicionalmente Ocupadas: SÃ£o as terras habitadas pelos indÃ­genas em carÃ¡ter permanente, utilizadas para atividades produtivas, culturais, bem-estar e reproduÃ§Ã£o fÃ­sica, segundo seus usos, costumes e tradiÃ§Ãµes.
# Reservas IndÃ­genas: SÃ£o terras doadas por terceiros, adquiridas ou desapropriadas pela UniÃ£o, que se destinam Ã  posse permanente dos indÃ­genas. SÃ£o terras que tambÃ©m pertencem ao patrimÃ´nio da UniÃ£o, mas que nÃ£o se confundem com as terras de ocupaÃ§Ã£o tradicional.
# Terras Dominiais: SÃ£o as terras de propriedade das comunidades indÃ­genas, havidas por qualquer das formas de aquisiÃ§Ã£o do domÃ­nio, nos termos da legislaÃ§Ã£o civil.
# Interditadas: weren't found on this official site, but these refer to tribes in isolation

# 3. INCRA data: SIGEF, SNCI, assentamentos, and quilombos ----
setwd(paste0(wdmain, "data/raw/landTenure/INCRA"))
l <- list.files()
# zips <- grep(".zip", l)
# lapply(l[zips], unzip)

# SIGEF:
# SIGEF is the Sistema de Gestão Fundiaria: the electronic tool developed for the georeferenced certification of land properties/tenure in Brazil
sigef <- st_read("Sigef Brasil.shp")
plot(sigef$geometry)
summary(sigef$data_aprov) # indeed, the SIGEF only runs starting from 2013

# # explore frequency of certification from INCRA: more as the years went on, 
# ggplot(sigef, aes(x=data_aprov)) + geom_histogram(binwidth=30, colour="white") +
#   scale_x_date(#breaks = seq(min(sigef$data_aprov)-5, max(sigef$data_aprov)+5, 30),
#                 breaks = seq(min(sigef$data_aprov), max(sigef$data_aprov), 500),
#                limits = c(min(sigef$data_aprov), max(sigef$data_aprov))) +
#   ylab("Frequency") + xlab("Year and Month") +
#   theme_bw() 

sigef2 <- select(sigef, c("situacao_i", "status", "data_aprov"))
sigef2 <- st_transform(sigef2, crs = my_crs_SAaea)
sigef2 <- st_zm(sigef2, drop = TRUE, what = "ZM") # remove z dimension
sigef2$LTcateg <- "sigef"
setwd(paste0(wdmain,"/data/processed/landTenure_SIGEF/"))
st_write(sigef2, "landTenure_SIGEFproperties_20231312_SAalbers.shp", append = F)

# SNCI
setwd(paste0(wdmain, "data/raw/landTenure/INCRA"))
snci <- st_read("Imóvel certificado SNCI Brasil.shp")
summary(snci$data_certi) # goes from 2004-2021 which does hold up to what the Camara paper says (but has not been updated since, whereas sigef yes?)
# # check histogram of dates certified
# ggplot(snci, aes(x=data_certi)) + geom_histogram(binwidth=30, colour="white") +
#   scale_x_date(#breaks = seq(min(sigef$data_aprov)-5, max(sigef$data_aprov)+5, 30),
#                 breaks = seq(min(snci$data_certi, na.rm = TRUE), max(snci$data_certi, na.rm = TRUE), 500),
#                limits = c(min(snci$data_certi, na.rm = TRUE), max(snci$data_certi, na.rm = TRUE))) +
#   ylab("Frequency") + xlab("Year and Month") +
#   theme_bw()
snci2 <- st_transform(snci, crs = my_crs_SAaea)
snci2 <- select(snci2, c("data_certi"))
snci2$LTcateg <- "snci"
setwd(paste0(wdmain,"/data/processed/landTenure_SNCI/"))
st_write(snci2, "landTenure_SNCIproperties_20240105_SAalbers.shp", append = F)


# 4. Public forests ----
setwd(paste0(wdmain,"data/raw/landTenure/florestasPublicas/CNFP 2020 Shapefiles (5) (1)"))
l <- grep(".shp", list.files())
flp <- lapply(list.files()[l], st_read)
flp <- do.call(rbind, flp)
# select relevant columns
flp2 <- select(flp, c("estagio", "ano", "anocriacao", "uf", "protecao", "tipo", "comunitari",  "categoria", "sobreposic")) # note only keep category instead of class, which is similar
# "other uses" in category are indeed various uses from unis to other institutes it seems
# the "atolegal" column was also pretty useless as a proxy of legality, it was only for military areas
# determine which categories to keep from the protection, class, and category 
flp2 <- flp2[which(flp2$protecao != "PROTECAO INTEGRAL" & flp2$protecao != "USO SUSTENTAVEL"),] # these categories are better covered by the UC data above
# the challenge will likely be seeing the overlap between glebas and other assentamentos, etc. 
# NOTE: according to the map from the cadaster of public forests: "As florestas não destinadas ocorrem em glebas arrecadadas pela União ou Estados"
# which means these are essentially the undesignated lands
flp2 <- st_transform(flp2, crs = my_crs_SAaea)
flp2 <- st_zm(flp2, drop = T, what = "ZM") # remove Z dimension which doesn't work with sf package
setwd(paste0(wdmain,"/data/processed/landTenure_UND-OTH/"))
st_write(flp2, "landTenure_Undesignated-Other-Military_SAalbers.shp", append=FALSE)


# 5. I also explored the harmonized dataset produced by Camara et al 2023 (ERL) ----
# the data is only for the amazon, and not very reproducible in the sense that their scripts do not show the raw data from start to finish
# their prioritization levels could provide a justifiable basis for us, though
# setwd(paste0(wdmain, "data/raw/landTenure/amz_fc/zip"))
# l <- list.files()
# lapply(l, unzip)
# amz_lt <- st_read("amz_land_tenure.shp")
# plot(amz_lt$geometry) # just checking this is indeed only for the amazon - it is. 

