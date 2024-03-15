#### Biodiversity and Tenure in Brazil ####

# private lands # 


# libraries
library(terra)
library(sf)
library(geos)
library(dplyr)
library(ggplot2)
library(tidyverse)


# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))

# private PAs
rppn <- st_read("landTenure_RPPN/landTenure_RPPN_20231212_SAalbers.shp")
rppn <- st_transform(rppn, my_crs_SAaea) # fix projection
rppn$id <- paste0("RPPN-", 1:nrow(rppn))
rppn <- select(rppn, c("LTcateg", "id", "geometry"))
setwd(paste0(wdmain,"/data/processed/processed2/private"))
st_write(rppn, "private_protectedAreas.shp", append = F)
# rppn.selfOVerlaps <- st_intersection(rppn)

# sigef properties (around 1M)
setwd(paste0(wdmain,"/data/processed/"))
sigef <- st_read("landTenure_SIGEF/landTenure_SIGEF_20231312_SAalbers.shp")
sigef <- st_transform(sigef, my_crs_SAaea)
sigef$id <- paste0("SIGEF-", 1:nrow(sigef))
sigef <- select(sigef, c("LTcateg", "id", "geometry"))
setwd(paste0(wdmain,"/data/processed/processed2/private"))
st_write(sigef, "SIGEF_properties.shp", append = F)

# snci properties
setwd(paste0(wdmain,"/data/processed/"))
snci <- st_read("landTenure_SNCI/")
snci <- st_transform(snci, my_crs_SAaea)
snci$id <- paste0("SNCI-", 1:nrow(snci))
snci <- select(snci, c("LTcateg", "id", "geometry"))
setwd(paste0(wdmain,"/data/processed/processed2/private"))
st_write(snci, "SNCI_properties.shp", append = F)


# quilombola lands
setwd(paste0(wdmain,"/data/processed/"))
quilombola <- st_read("landTenure_QUI/")
quilombola <- st_transform(quilombola, my_crs_SAaea)
quilombola$id <- paste0("QUI-", 1:nrow(quilombola))
quilombola <- select(quilombola, c("LTcateg", "id", "geometry"))
setwd(paste0(wdmain,"/data/processed/processed2/private"))
st_write(quilombola, "quilombolaLands.shp", append = F)


