#### Biodiversity and Tenure in Brazil ####
# script that counts the biodiversity indicators per different tenure regimes
# output: tables per state in br of the mean biodiversity indicator (richness, endemism) per polygon (by the X id) 
# note run time of the extraction should be ~50m on my cpu
# author: Andrea Pacheco
# first run: 17.10.2022
# last run: 11.10.2023

# libraries
library(raster)
library(sf)
library(exactextractr)
# load directories and etc.
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# things to consider:
# what is the best solution for polygons with an area <1km?

# biodiversity data ----
setwd(paste0(wdmain, "data/processed/BiodivIndicators_SA-albers"))
list.files()
richness <- rast("Species_Richness_All_groups_MARS_SAalbers.tif")
endemism <- rast("Weight_Endemism_All_groups_RF_SAalbers.tif") #index 0-1
# betaD <- rast("Beta_Diversity_GDM_SAalbers.tif") # idk what the 3 rgb bands are - so tackle this later

# conduct extractions using simple tenure data ----
setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023/"))
f <- grep(".shp", list.files())

for(i in 1:length(f))
{
  setwd(paste0(wdmain,"/data/processed/landTenureCategs_v2023/"))
  s <- st_read(list.files()[f][i])
  name <- gsub(".shp",".csv", list.files()[f][i])
  # check if there are empty geometries
  empties <- grep("TRUE", is.na(st_dimension(s)))
  if(length(empties) > 0) {s <- s[-empties,] }
  # mean species richness per polygon, weighed by the fraction of that polygon that is covered by the cell
  e_rich <- exactextractr::exact_extract(richness, s, "mean") # function should depend on variable that I am extracting
  e_endm <- exactextractr::exact_extract(endemism, s, "mean")
  # make table with biodiversity values and identifier
  t <- as.data.frame(cbind("X_uid_" = s$X_uid_, "richness"= e_rich, "endemism" = e_endm))
  # write out table
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  write.csv(t, file = paste0(name), row.names = FALSE)
  print(i)
}


