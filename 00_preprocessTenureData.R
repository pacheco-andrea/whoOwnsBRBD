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

testintersect <- st_intersects(ind, uc)
testintersection  <- st_intersection(uc, ind) # returns geometry of the shared portion of x and y
nrow(testintersection )
plot(testintersection $geometry)

testwithin <- st_snap(ind, uc, tolerance = 1000) # run time start ~10:45

testoverlap <- st_overlap(ind, uc # ids if x and y share space, are of the same dimension but are not completely contained

# ALL OF THIS FOR THE ONE WALL-TO-WALL with the minimum necessary of overlaps (as this will become one category)                       
# i need to come up with a series of rules
# if UCs and IND overlap: should resolve, prioritize UCs
# if UCs and IRU-AST-PCT overlap: should note these overlaps as private on public (but only keep IRU and AST, and if AST then it's public on public)
# if UCs and RPPN overlap
# UCs + SIGEF: note overlap as private on public
# Ucs + SNCI: note overlap as private on public
# UCs + UND-OTHER: note as public on public

# SIGEF + SNCI: merge as private from INCRA
# IRU-AST + SIGEF-SNCI: merge as private on private but not problematic - simply due to different systems # this would potentially take foreeever
# 



