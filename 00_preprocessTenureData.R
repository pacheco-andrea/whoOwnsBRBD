#### Biodiversity and Tenure in Brazil ####

# this script brings together different sources of (updated) land tenure data and preprocesses them for use in analyses
# outputs are:
  # folders with parcel-level tenure data for brazil, all harmonized 
  # is simplified versions of the above with the minimum information of an identifier, a category, and the geometry
  # rasterized versions of all these polygon categories (which is used for mapping not for accounting areas)

# author: Andrea Pacheco
# first run: 23.09.2022
# last run: 15.11.2023

# issues:
# resolving the overlaps in data!
# i suggest:
# one script to put together all tenure data
  # need to decide which public forests to keep
  # export these to have a look at overlaps in qgis
  # decide how to prioritize hierarqies of categories
  # describe overlaps (how to quantify these % wise?)


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
  name <- paste(files[shps[i]])
  # write out
  setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT/"))
  st_write(stateShp, name)
  print(i)
}

# 2. Conservation units and indigenous lands ----
# conservation units
uc <- read_conservation_units(date = 201909, simplified = FALSE) # latest update
# need to crop these to only the extent of terrestrial brazil
uc <- st_transform(uc, crs = my_crs_SAaea)
uc <- uc[-grep("TRINDADE", uc$name_conservation_unit),] # manually removed marine PAs that I wasn't able to crop with raster
uc2 <- select(uc, c("group", "category", "creation_year", "quality"))
qual <- c("Aproximado (O poligono representa uma estimativa dos limites da unidade)." = "approximate",
         "Correto (O poligono corresponde ao memorial descritivo do ato legal de criação)." = "correct",
         "Esquemático (O poligono é uma representação esquemática da dimensão da unidade)." = "outlined")
uc2$quality <- as.character(qual[uc2$quality])
setwd(paste0(wdmain,"/data/processed/landTenure_UC/"))
st_write(uc2, "landTenure_ConservationUnits_201909_SAalbers.shp")
# indigenous lands
ind <- read_indigenous_land(date = 202103, simplified = FALSE) # latest available
ind <- st_transform(ind, crs = my_crs_SAaea)
ind2 <- select(ind, (c("abbrev_state", "fase_ti", "modalidade", "date")))
setwd(paste0(wdmain,"/data/processed/landTenure_UC/"))
st_write(ind2, "landTenure_IPLCS_202103_SAalbers.shp")
# according to this https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/demarcacao-de-terras-indigenas
# the following indigenous lands categories
# Terras Indígenas Tradicionalmente Ocupadas: São as terras habitadas pelos indígenas em caráter permanente, utilizadas para atividades produtivas, culturais, bem-estar e reprodução física, segundo seus usos, costumes e tradições.
# Reservas Indígenas: São terras doadas por terceiros, adquiridas ou desapropriadas pela União, que se destinam à posse permanente dos indígenas. São terras que também pertencem ao patrimônio da União, mas que não se confundem com as terras de ocupação tradicional.
# Terras Dominiais: São as terras de propriedade das comunidades indígenas, havidas por qualquer das formas de aquisição do domínio, nos termos da legislação civil.
# Interditadas: weren't found on this official site, but these refer to tribes in isolation

setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(ind_r, filename = "landTenure_indigenous_SAalbers_1km.tif")

# 3. Public forests ----
# from the above datasets, i realized that that public lands were missing. hence, 
setwd(paste0(wdmain,"data/raw/florestasPublicas/CNFP 2020 Shapefiles (5) (1)" ))
list.files()
l <- grep(".shp", list.files())
list.files()[l]
flp <- lapply(list.files()[l], st_read)
flp <- do.call(rbind, flp)
flp
# explore these data on public forests
nrow(flp)
plot(flp$geometry)
table(flp$classe)
unique(flp$classe) # glebas, q e AUTARQ?
table(flp$categoria)
table(flp$sobreposic)
overlaps <- flp[which(flp$sobreposic == "SIM"),]
table(overlaps$classe)
table(flp$protecao)
flp[which(flp$classe == "ASSENEST"),]$protecao

undesignated <- flp[which(flp$protecao == "SEM DESTINACAO"),]
undesignated2 <- st_transform(undesignated, crs = my_crs)
plot(undesignated2$geometry)
table(undesignated2$classe)
table(undesignated2$tipo)
table(undesignated2$categoria)
table(undesignated2$sopreposic)
und <- rasterize(undesignated2, mask, "tipo")
plot(und)
setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(und, filename = "landTenure_Undes_SAalbers_1km.tif")


# rasterize tenure data ----
# THIS SHOULD BE FOR PLOTTING MAPS - NOT FOR CALCULATIONS!

# make mask
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
r <- rast(list.files()[grep("Richness", list.files())[1]])
r <- project(r, my_crs_SAaea)
mask <- r*0

setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023/"))
# bind all the state sfs into one in order to:
# 1. rasterize without raster changing categories across states
# 2. generate a unique ID for each polygon
s <- list()
for (i in 1:length(grep(".shp",list.files()))) 
{
  setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023/"))
  shps <- grep(".shp", list.files())  
  stateShp <- read_my_shp(list.files()[shps[i]])
  name <- list.files()[shps[i]]
  s[[i]] <- stateShp
  # also writing out shapefiles only of PCT because I want to more easily compare these against sustainable use PAs
  setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_PCT/"))
  st_write(stateShp[which(stateShp$tipo == "PCT"),], name)
}
# merge individual states into one whole map of brazil
s2 <- do.call(rbind, s)
s2$g_id <- 1:nrow(s2)
nrow(s2)
setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_allBR"))
write.csv(s2, "landTenureCategs_v2023_allBR.csv", row.names = F)

# rasterize and write out raster for map
r <- terra::rasterize(s2, mask, "tipo")
cols <- c("#FFD700","#8DA0CB", "#FC8D62")
plot(r, col=cols) 
setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(r, filename = "landTenure_AST-IRU-PCT_SAalbers_1km.tif")

# figure out the deal with PCT lands 
setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023_PCT/"))
l <- list.files()
grep(".shp", l)
pct <- lapply(l[grep(".shp", l)], st_read)
pct <- do.call(rbind, pct)
nrow(pct)
setwd(paste0(wdmain,"/data/processed/pct_lands"))
st_write(pct, "pct_BR.shp")
# so, importing this shapefile into qgis i can tell there's a lot, but not 100% overlap of PCTs and sustainable use areas. many are within SUs

# rasterize ucs and inds ----
uc_r <- rasterize(uc2, mask, "group")
plot(uc_r)
setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
writeRaster(uc_r, filename = "landTenure_PAs_SAalbers_1km.tif")

ind_r <- rasterize(ind2, mask, "modalidade") # CHECK what exactly i want to rasterize
plot(ind_r)
