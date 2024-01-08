#### Biodiversity and Tenure in Brazil ####

# this script brings together the pre-processed tenure data 
# and creates the outputs:
# 1) one version with no overlaps for rasterizing (or rather, with overlaps as one category)
# 2) one version with overlaps for extracting 

# is this a script to deal with the overlaps overall?


# libraries
library(terra)
library(sf)
library(dplyr)
library(geobr)

# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))
grep("^landTenure", list.files())
tenfolds <- list.files()[grep("^landTenure_", list.files())]

# get data inside each folder, print the column names
# why would they need to have the same column names though?

setwd(paste0(wdmain,"/data/processed/", tenfolds[i]))
s <- grep(".shp", list.files())
list.files()[s]

data <- st_read("landTenure_indigenous_20231212_SAalbers.shp")
colnames(data)

setwd(paste0(wdmain,"/data/processed/", tenfolds[2]))
data2 <- st_read("cf_imoveis_imaflora_ac.shp")
colnames(data2)

setwd(paste0(wdmain,"/data/processed/", tenfolds[3]))
data3 <- st_read("landTenure_RPPN_20231212_SAalbers.shp")
colnames(data3)

setwd(paste0(wdmain,"/data/processed/", tenfolds[4]))
data4 <- st_read("landTenure_SIGEFproperties_20231312_SAalbers.shp")
colnames(data4)

setwd(paste0(wdmain,"/data/processed/", tenfolds[5]))
data5 <- st_read("landTenure_SNCIproperties_20240105_SAalbers.shp")
colnames(data5)

setwd(paste0(wdmain,"/data/processed/", tenfolds[6]))
data6 <- st_read("landTenure_UCs_MMA_20231212_SAalbers.shp")
colnames(data6)

setwd(paste0(wdmain,"/data/processed/", tenfolds[7]))
data7 <- st_read("landTenure_Undesignated-Other-Military_SAalbers.shp")
colnames(data7)



# let me try subsetting indigenous that are inside PAs
tenfolds

testoverlap <- st_intersection(uc, ind) # returns those that intersect (248)
plot(testoverlap$geometry)

# ok, now test if in a raster, they overlap


