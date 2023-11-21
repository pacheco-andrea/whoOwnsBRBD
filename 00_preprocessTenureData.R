#### Biodiversity and Tenure in Brazil ####

# this script brings together different sources of (updated) land tenure data and preprocesses them for use in analyses
# outputs are:
# folders with parcel-level tenure data for brazil, all harmonized in same projection 
# these are also simplified versions of the above with the minimum information of an identifier, a category, and the geometry

# author: Andrea Pacheco
# first run: 23.09.2022
# last run: 15.11.2023

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
  # write out
  setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
  st_write(stateShp, name)
  print(i)
}

# 2. Conservation units and indigenous lands ----

# conservation units
uc <- read_conservation_units(date = 201909, simplified = FALSE) # latest update 201909
# need to crop these to only the extent of terrestrial brazil
uc <- st_transform(uc, crs = my_crs_SAaea)
uc <- uc[-grep("TRINDADE", uc$name_conservation_unit),] # manually removed marine PAs that I wasn't able to crop with raster
uc2 <- select(uc, c("group", "category", "creation_year", "quality"))
# modify and translate the variable on quality
uc2$quality <- gsub("^Aproximado.*", "approximate", uc2$quality)
uc2$quality <- gsub("^Correto.*", "correct", uc2$quality)
uc2$quality <- gsub("^Esquem.*", "outlined", uc2$quality)
setwd(paste0(wdmain,"/data/processed/landTenure_UC/"))
st_write(uc2, "landTenure_ConservationUnits_201909_SAalbers.shp", append=FALSE)

# indigenous lands
ind <- read_indigenous_land(date = 201909, simplified = FALSE) # latest available 202103
ind <- st_transform(ind, crs = my_crs_SAaea)
ind2 <- select(ind, (c("abbrev_state", "fase_ti", "modalidade", "date")))
setwd(paste0(wdmain,"/data/processed/landTenure_IPLC/"))
st_write(ind2, "landTenure_IPLCS_201909_SAalbers.shp", append=FALSE)
# according to this https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/demarcacao-de-terras-indigenas
# the following indigenous lands categories
# Terras IndÃ­genas Tradicionalmente Ocupadas: SÃ£o as terras habitadas pelos indÃ­genas em carÃ¡ter permanente, utilizadas para atividades produtivas, culturais, bem-estar e reproduÃ§Ã£o fÃ­sica, segundo seus usos, costumes e tradiÃ§Ãµes.
# Reservas IndÃ­genas: SÃ£o terras doadas por terceiros, adquiridas ou desapropriadas pela UniÃ£o, que se destinam Ã  posse permanente dos indÃ­genas. SÃ£o terras que tambÃ©m pertencem ao patrimÃ´nio da UniÃ£o, mas que nÃ£o se confundem com as terras de ocupaÃ§Ã£o tradicional.
# Terras Dominiais: SÃ£o as terras de propriedade das comunidades indÃ­genas, havidas por qualquer das formas de aquisiÃ§Ã£o do domÃ­nio, nos termos da legislaÃ§Ã£o civil.
# Interditadas: weren't found on this official site, but these refer to tribes in isolation

# 3. Public forests ----
# from the above datasets, i realized that that public lands were missing. hence, 
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

# synthesis of these data: ----
nrow(uc2) # 1932
nrow(ind2)# 624
nrow(flp2) # 1841
# = 4397 observations + 6797557 properties from CSR = 6,801,954 (or, nearly 7 million parcels, which, btw is 3 million more than i had in the 2018 version of imaflora)

