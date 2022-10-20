#### Biodiversity and Tenure in Brazil ####
# script that counts the areas with high biodiversity per different tenure regimes
# output is tables that count how many km2 (equal area pixels) of each tenure category have BD categories
# author: Andrea Pacheco
# first run: 17.10.2022

# libraries
library(raster)
library(rgdal)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"
rasterOptions(tmpdir = "N:/eslu/priv/pacheco/biodivTenureBR/tmp", chunksize = 524288, maxmemory = 134217728)
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}




# read raster data ----
# biodiversity
setwd(paste0(wdmain,"/data/raw/"))
bd <- raster("Biodiversity_model/Model_biodiverisity.tif")

setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
tenureList <- list.files()[grep("grd", list.files())]

# create table for reclassifying raster data
myReclass <- data.frame("orig" = 1:18)
myReclass$new <- c(0,9,8,8,6,0,7,7,5,8,4,3,7,8,0,2,1,0)

# join biodiversity and tenure data
for(i in 2:length(tenureList))
{
  setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
  
  rasterreg <- raster(tenureList[i])
  # reclassify raster
  rasterreg <- reclassify(rasterreg, myReclass)
  # join with biodiv
  bd2 <- crop(bd, extent(rasterreg)) # its not cropping!
  tenureBD <- (bd2*10) + rasterreg
  # get pixel counts
  countBDTen <- as.data.frame(freq(tenureBD))
  # write out
  setwd(paste0(wdmain, "/data/processed/BDTen_areaCount"))
  write.csv(countBDTen, paste0("countBD-tenure_",gsub(".grd", ".csv", tenureList[i])), row.names = F)
  
  print(i)
}





