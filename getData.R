# biodiversity_conservation dataset from ubirajara
wdmain <- "N:/eslu/priv/pacheco/biodivTenureBR"

list.files()

# install.packages("raster")
# install.packages("rgdal")
install.packages("sf")

library(raster)
library(rgdal)
library(sf)

# biodiversity data ----
setwd(paste0(wdmain,"/data/raw/biodiveristy_conservation"))
brc <- raster("biodiveristy_conservation.tif")
brc
plot(brc)
res(brc)
unique(brc)
# the 8 categs seem to correspond precisely with what they have in the paper... but in the same order?
# is this 500*500 m? - which means at 1km


# tenure data ----
# new 2021 version is now split into a .shp for each state-municipality. very annoying
setwd(paste0(wdmain,"/data/raw/pa_br_LandTernure.v202105"))
l <- list.files()
f <- lapply(l, unzip)

s <- st_read("centro_oeste/es_df_landternure.v202105_random_sirgas.shp")
s
plot(s$geometry)
colnames(s)

# steps for processing:
# what versions of the columns do I want to keep?
# gid, subclass, area, cd_mun, geometry, 
