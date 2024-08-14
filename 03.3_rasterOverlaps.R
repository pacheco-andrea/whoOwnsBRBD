#### Biodiversity and Tenure in Brazil ####

# this script check the area of overlaps between different categories using a 1km2 raster 
# it also plots all of these overlaps across all categories in order to visualize these

# outputs are: 
# one giant plot of all the overlaps across categories ("overlap_plots_1kmg.png") 
# one barplot of the overlaps in km2

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
library(dplyr)
library(tidyr)
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


# make barplot with data I've prepared ----
# i pre-prepared this data in excel, just putting together the tables in a way that made sense 
# (bc i had made some of those calculations in earlier scripts)

# get data
setwd(paste0(wdmain, "data/"))
d <- read.csv("processed/LT_overlapsSummary.csv", sep = "")
# pivot table
d2 <- as.data.frame(d %>%
                pivot_longer(cols = -all_of(c("areakm2", "categ")),
                             names_to = "overlap",
                             values_to = "overlapkm2"))
head(d2) 
# d2$overlapkm2 <- as.numeric(d2$overlapkm2)
d2$overlapkm2 <- base::round(d2$overlapkm2, digits = 0)
# i need to add a row which gives the sum of the area that is not under overlap
d3 <- matrix(nrow = length(unique(d2$categ)), ncol = length(colnames(d2)))
d3 <- as.data.frame(d3)
colnames(d3) <- colnames(d2)
for(i in 1:length(unique(d2$categ)))
{
  category <- unique(d2$categ)[i]
  total_area_overlapped <- sum(d2$overlapkm2[grep(category, d2$categ)], na.rm = T)
  area_total <- d2$areakm2[grep(category, d2$categ)][1]
  # fill table
  d3[i,] <- c(category, area_total, "non-overlapped", (area_total-total_area_overlapped))
}
d3
d3$overlapkm2 <- as.numeric(d3$overlapkm2)
d3$overlapkm2 <- base::round(d3$overlapkm2, digits = 0)

# join to original wider table
d4 <- rbind(d2,d3)
d4$categ <- factor(d4$categ, levels = c("quilombola", "privatePAs", "indigenous" ,"PA_sustuse", "PA_strict", "ruralSettlements", "undesignated",  "ruralProperties"))

# make barplot to visualize magnitude of overlaps compared to total area under different tenure categories

tenureColors <- c("indigenous" = "#E78AC3",
                  "non-overlapped" = "gray70",   
                  "PA_strict" = "#1B9E77",       
                  "PA_sustuse" =  "#8C7E5B",
                  "privatePAs"  = "#99d8c9",  
                  "quilombola" =  "#FFD700",
                  "ruralProperties" = "#8DA0CB",
                  "ruralSettlements" = "#FC8D62",
                  "undesignated" ="#1d6c7d")

overlapsBarplot <- ggplot(d4, aes(fill = overlap, y = overlapkm2, x = categ)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = tenureColors) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,5000000, by = 500000))+
  ylab(expression(Area~(km^2))) +
  xlab("Land tenure category") + 
  coord_flip() +
  theme(legend.position = c(0.90, 0.45), panel.background = element_blank(), legend.title = element_blank()) 
overlapsBarplot

# write out barplot
setwd(paste0(wdmain, "/output"))
svg("OverlapsAreakm2_barplot.svg", width = 12, height = 4)
overlapsBarplot
dev.off()
