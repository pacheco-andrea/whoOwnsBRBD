#### Biodiversity and Tenure in Brazil ####

# this script check the area of overlaps between different categories using a 1km2 raster 
# it also plots all of these overlaps across all categories in order to visualize these

# outputs are: 
# one giant plot of all the overlaps across categories ("overlap_plots_1kmg.png") 
# 
# a map of tenure categories (at this stage it should indicate overlaps!)

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
library(dplyr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# get tenure rasters: ----
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- list.files()
tifs <- grep(".tif$", l)
R1km <- l[tifs][grep("1km", l[tifs])] 
# exclude SIGEF and SNCI properties
t <- lapply(R1km[-grep("properties", R1km)], rast)
# terra::plot(t)
names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", R1km[-grep("properties", R1km)]))

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(mask, proj = T))
plot(biomes)
v <- vect(biomes)

# overlap all the possible combinations ----
names(t) # the number of categories to map

overlap_palette <- c("gray80", "red")
overlaps1km_results <- list()

# loop through all the combinations & plot at the same time
setwd(paste0(wdmain, "output/"))
png("overlap_plots_1km.png", width = 3000, height = 1500, units = "px", pointsize = 18)
par(mfrow = c(3,7))
for(i in 1:(length(t)-1)){
  for(j in (i+1):length(t)) {
    
    # check if there is an overlap
    overlap <- terra::intersect(t[[i]], t[[j]])
    
    # if there is overlap
    if(!is.null(overlap)) {
      overlap_area <- sum(values(overlap), na.rm = T) # i can just sum here bc using AEA proj at 1km res
      # write result
      overlaps1km_results[[paste0("overlap_", i, "_", j)]] <- list(
        "raster1" = names(t)[i],
        "raster2" = names(t)[j],
        "areakm2" = overlap_area
      )
      # plot the overlap
      plot(overlap, col = overlap_palette, main = paste("Overlap between", names(t)[i], "&", names(t)[j]),
           axes = F, mar = NA, plg = list(x = "bottomleft"))
      terra::lines(v, lwd=.01)
      
    }
  }
}
dev.off()

overlap1km_df <- do.call(rbind, lapply(overlaps1km_results, as.data.frame))
overlap1km_df

# alright, upon manual inspection this checks out pretty well against polygon intersections
# at 1km2 raster, these estimated intersections are consistently smaller than the polygons (likely bc they're less precise)
# someday i'll likely want to choose which plots to highlight for a presentation/paper
# 1) rural properties and undesignated lands
# 2) rural properties and PAs
# to a less extent:
# 3) rural settlements on PAs
# 4) indigenous - PAs
# 5) rural properties on indigenous lands


# visualize overlaps ----

