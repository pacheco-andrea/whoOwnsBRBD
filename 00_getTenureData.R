#### Biodiversity and Tenure in Brazil ####

# this script gathers different sources of (updated) land tenure data and pre-processes them for use in analyses
# outputs are:
# individual folders with parcel-level tenure data for brazil
# these are simplified versions of the data (filtering for only the minimum relevant information category, and geometry)

# author: Andrea Pacheco


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)

# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data sources:

# 1. CAR properties sourced from CSR ----
# these data were provided by the CSR (Centro de Sensoriamento Remoto) of UFMG and downloaded manually
# the original source of these data is the SFB (servico florestal brasilero), but they've been preprocessed by CSR and through their partnership with Imaflora
# categories are: IRU, AST, PCT, (imovel rural, assentamentos, and povos e comunidades tradicionais)
# after running exploratory analyses, i  decided to exclude the PCT category from the analysis as these were better covered by more specific categs found elsewhere
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
  # test <- st_read("cf_imoveis_imaflora_ac.shp")
  # test <- test[which(test$tipo == "PCT"),]
  # filter out PCTs
  stateShp <- stateShp[which(stateShp$tipo != "PCT"),]
  colnames(stateShp) <- gsub("tipo", "LTcateg", colnames(stateShp))
  # write out
  setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
  st_write(stateShp, name, append = FALSE)
  print(i)
}

# 2. Conservation units  ----
# sourced directly from MMA (rather than the IBGE package read_conservation_units function which is easier but may be outdated)
# this required a vpn from brazil, as the site was blocked from access otherwise
# additionally, i had to do some manual preprocessing in QGIS where i assigned the SAD 1969 and reprojected to WGS 84 in QGIS 
# because .shp in mma website was missing the .prj file
setwd(paste0(wdmain,"/data/raw/landTenure/UCs_MMA"))
uc_mma <- st_read("ucs_mma_qgispreprocess.shp")
# plot(uc_mma$geom)
uc_mma <- st_transform(uc_mma, my_crs_SAaea)
# manually remove big marine PAs to preemptively avoid extraction errors
uc_mma <- uc_mma[-grep("TRINDADE", uc_mma$NOME_UC1),] 
uc_mma <- uc_mma[-grep("20/03/2018", uc_mma$ATO_LEGA9),]
uc_mma <- uc_mma[-grep("NORONHA", uc_mma$NOME_UC1),]
uc_mma <- uc_mma[-grep("ATOL DAS ROCAS", uc_mma$NOME_UC1),]
# plot(uc_mma$geometry)
uc_mma2 <- select(uc_mma, c("GRUPO4", "CATEGORI3", "ANO_CRIA6"))
# should separately consider RPPN because these are private - but these are better sourced from ICMBIO
# should NOT include APAs because they have no real protection
uc_mma2 <- uc_mma2[-grep("^Reserva Particular do", uc_mma2$CATEGORI3),]
uc_mma2 <- uc_mma2[-grep("rea de Prote", uc_mma2$CATEGORI3),]
# colnames(uc_mma2) <- c("group", "LTcateg", "yearCreat", "geometry")
# write out:
setwd(paste0(wdmain,"/data/processed/landTenure_UC/"))
st_write(uc_mma2, "landTenure_UCs_MMA_20231212_SAalbers.shp", append=FALSE)

# # note, i run into the st_intersection GEOS exception issue even with the data downloaded from the geobr package 
# # the two features with an issue are: code 267, and 964
# # i'm keeping the following test as a reminder:
# test <- read_conservation_units()
# test <- test[which(test$category != "Reserva Particular do Patrim?nio Natural"),]
# nrow(test)
# test2 <- test[-grep("267", test$code_conservation_unit),]
# test2 <- test2[grep("964", test$code_conservation_unit),]
# testintersection <- st_intersection(test2)



# 3. Private Protected areas: RPPNs ----
setwd(paste0(wdmain,"/data/raw/landTenure/RPPN"))
# unzip("total.zip")
rppn <- st_read("total.shp")
rppn <- st_transform(rppn, crs = my_crs_SAaea)
rppn$group <- "US" # to categorize as sustainable use
rppn$LTcateg <- "RPPN"
rppn <- select(rppn, c("LTcateg", "group", "data_cadas"))
setwd(paste0(wdmain,"/data/processed/landTenure_RPPN"))
st_write(rppn, "landTenure_RPPN_20231212_SAalbers.shp", append=FALSE)

# 4. indigenous lands ----
# according to this https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/demarcacao-de-terras-indigenas
# the following indigenous lands categories
# Terras IndÃ­genas Tradicionalmente Ocupadas: SÃ£o as terras habitadas pelos indÃ­genas em carÃ¡ter permanente, utilizadas para atividades produtivas, culturais, bem-estar e reproduÃ§Ã£o fÃ­sica, segundo seus usos, costumes e tradiÃ§Ãµes.
# Reservas IndÃ­genas: SÃ£o terras doadas por terceiros, adquiridas ou desapropriadas pela UniÃ£o, que se destinam Ã  posse permanente dos indÃ­genas. SÃ£o terras que tambÃ©m pertencem ao patrimÃ´nio da UniÃ£o, mas que nÃ£o se confundem com as terras de ocupaÃ§Ã£o tradicional.
# Terras Dominiais: SÃ£o as terras de propriedade das comunidades indÃ­genas, havidas por qualquer das formas de aquisiÃ§Ã£o do domÃ­nio, nos termos da legislaÃ§Ã£o civil.
# Interditadas: weren't found on this official site, but these refer to tribes in isolation

setwd(paste0(wdmain,"/data/raw/landTenure/FUNAI"))
unzip("tis_poligonais.zip")
ind <- st_read("tis_poligonais.shp")
table(ind$fase_ti)
# keep only regularized and homologated indigenous lands because these are the legally official ones
ind <- ind[which(ind$fase_ti == "Homologada" | ind$fase_ti == "Regularizada" ),]
ind2 <- select(ind, (c( "uf_sigla", "fase_ti","modalidade")))
# plot(ind2$geometry)
setwd(paste0(wdmain,"/data/processed/landTenure_IND/"))
st_write(ind2, "landTenure_indigenous_20231212_SAalbers.shp", append=FALSE)

# 5.. INCRA data: SIGEF, SNCI, assentamentos, and quilombos ----
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
sigef2 <- st_buffer(sigef2, dist = 0)
setwd(paste0(wdmain,"/data/processed/landTenure_SIGEF/"))
st_write(sigef2, "landTenure_SIGEF_20231312_SAalbers.shp", append = F)

# SNCI
setwd(paste0(wdmain, "data/raw/landTenure/INCRA"))
snci <- st_read("Im?vel certificado SNCI Brasil.shp")
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
# fix self-intersection issue
snci2 <- st_buffer(snci2, dist = 0) 
setwd(paste0(wdmain,"/data/processed/landTenure_SNCI/"))
st_write(snci2, "landTenure_SNCI_20240105_SAalbers.shp", append = F)

# # how much is the overlap between sigef and snci?
# nrow(sigef2)
# nrow(snci2)
# tinter <- st_intersection(sigef2, snci2)
# nrow(tinter)/nrow(sigef2) # so, 12% of overlap


qui <- st_read("?reas de Quilombolas.shp")
plot(qui$geometry)
summary(qui)
qui <- select(qui, c("dt_publica", "fase" , "nr_familia"))
qui$LTcateg <- "quilombola"
setwd(paste0(wdmain,"/data/processed/landTenure_QUI/"))
st_write(qui, "landTenure_quilombo_20240105_SAalbers.shp", append = F)


# 6. Public forests ----
# NOTE: according to the map from the cadaster of public forests: 
#"As florestas não destinadas ocorrem em glebas arrecadadas pela União ou Estados"
# which means these are essentially the undesignated lands

setwd(paste0(wdmain,"data/raw/landTenure/florestasPublicas/CNFP 2020 Shapefiles (5) (1)"))
l <- grep(".shp", list.files())
flp <- lapply(list.files()[l], st_read)
flp <- do.call(rbind, flp)
# select relevant columns
flp2 <- select(flp, c("estagio", "ano", "anocriacao", "uf", "protecao", "tipo", "comunitari",  "categoria", "sobreposic")) # note only keep category instead of class, which is similar
# "other uses" in category are indeed various uses from unis to other institutes it seems
# the "atolegal" column was also pretty useless as a proxy of legality, it was only for military areas
flp2 <- flp2[which(flp2$protecao != "PROTECAO INTEGRAL" & flp2$protecao != "USO SUSTENTAVEL"),] # these categories are better covered by the UC data above
flp2 <- st_transform(flp2, crs = my_crs_SAaea)
flp2 <- st_zm(flp2, drop = T, what = "ZM") # remove Z dimension which doesn't work with sf package
setwd(paste0(wdmain,"/data/processed/landTenure_UND-OTH/"))
st_write(flp2, "landTenure_UND-OTH_SAalbers.shp", append=FALSE)


# NOTE: ALTERNATIVE DATA SOURCES ----
# I also explored the harmonized dataset produced by Camara et al 2023 (ERL) 
# the data is only for the amazon, and not very reproducible in the sense that their scripts do not show the raw data from start to finish
# their prioritization levels could provide a justifiable basis for us that could be repeated, however
# setwd(paste0(wdmain, "data/raw/landTenure/amz_fc/zip"))
# l <- list.files()
# lapply(l, unzip)
# amz_lt <- st_read("amz_land_tenure.shp")
# plot(amz_lt$geometry) # just checking this is indeed only for the amazon - it is. 

