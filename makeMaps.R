#### Biodiversity and Tenure in Brazil ####
# script that makes maps: 
# new version land tenure in Brazil
# homologated and nonhomologated indigenous lands
# author: Andrea Pacheco
# first run: 18.10.2022

# libraries
library(raster)
library(rgdal)
library(rasterVis)
# install.packages("rasterVis")

wdmain <- "N:/eslu/priv/pacheco/biodivTenureBR"
rasterOptions(tmpdir = "N:/eslu/priv/pacheco/biodivTenureBR/tmp", chunksize = 524288, maxmemory = 134217728)
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# upon first run, get tenure rasters and merge them into one:
# setwd(paste0(wdmain, "/data/processed/landTenureCategsRaster/"))
# l <- grep(".gri", list.files())
# r <- lapply(list.files()[l], raster)
# r$fun <- mode
# r2 <- do.call(mosaic, r)
# plot(r2)
# setwd(paste0(wdmain, "/data/processed/"))
# writeRaster(r2, "tenure_data_reclassified_1km_BR.tif")

setwd(paste0(wdmain, "/data/processed/"))
r <- raster("tenure_data_reclassified_1km_BR.tif")

# test to reclassify and check the difference bt homologated and nonhomologated indigenous
myReclass <- data.frame("orig" = 1:18)
myReclass$new <- c(0,0,0,0,0,0,0,0,0,0,5,10,0,0,0,0,0,0)

indi <- reclassify(r, myReclass)
plot(indi) # homologated should be 5, non homologated should be 10

# make tenure map
myReclass <- data.frame("orig" = 1:18)
myReclass$new <- c(0,9,8,8,6,0,7,7,6,8,3,3,7,8,0,2,1,0)
tenureBR <- reclassify(r, myReclass)
tenure_raster <- ratify(tenureBR)
rat <- levels(tenure_raster)[[1]]
rat$tenure <- c("other", "sustUse_PA", "strict_PA", "Indigenous", "Comm_Qui", "Undesignated", "Private", "RurSettlmt") 
levels(tenure_raster) <- rat

tenure_plot <- levelplot(tenure_raster,
         xlab = NULL, ylab = NULL,
         scales = list(draw = F), 
         par.settings = list(axis.line = list(col = "transparent")),
         colorkey = F,
         col.regions = c("#f0f0f0", "#8C7E5B", "#1b9e77", "#e78ac3", "#ffd700", "#1c9099", "#8da0cb", "#fc8d62 " ))# other options: #d8b365 #b88340 #a19f63
tenure_plot

setwd(paste0(wdmain, "/output/"))
pdf(file = "tenure_map.pdf", width = 1500, height = 1500, units = "px", res = 300)
tenure_plot
dev.off()


