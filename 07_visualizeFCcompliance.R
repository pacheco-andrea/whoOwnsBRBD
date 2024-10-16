#### Biodiversity and Tenure in Brazil ####

# script that incroporates the Forest Code compliance (surplus or deficit of native vegetation) to our tenure + biodiversity data

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(cowplot)
library(classInt)
library(biscale)
library(pals)
library(hrbrthemes)
library(GGally)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")



# get data table tenure - BD - CSR ----
setwd(paste0(wdmain, "data/processed/"))
data_extra <- read.csv("finalDataset_Tenure-BD-CSR.csv")
head(data_extra)

# filter data for which we have the FC surplus/deficit information
FC_data <- data_extra[which(!is.na(data_extra$area_conv)),]
summary(FC_data)
# get geometry data that corresponds to these data above
# in which directories would i find IRU and AST?
# AST
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
ast <- st_read("ruralSettlements.shp")
# IRU
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
iru <- st_read("ruralProperties.shp")
# join BD-FC data with geometry data
FC_data <- left_join(FC_data, rbind(ast,iru), by = "id")
FC_data <- st_as_sf(FC_data) 

# as I'm only plotting for visualization purposes, i need to simplify these polygons
# if tocantins is missing then i should st_union before this?
FC_data2 <- FC_data[!st_is_empty(FC_data),] 
# FC_data2 <- st_simplify(FC_data2, dTolerance = 100) # at 1000, we lose ~60% of observations
# FC_data2 <- FC_data2[!st_is_empty(FC_data2),] # i see that this is now 99% of the data, so it takes a while but it is pretty worth it in plotting the map
FC_data2 <- st_transform(FC_data2, my_crs_SAaea)

# map test ----

# start with a simple choropleth
try1 <- ggplot(FC_data2) +
  geom_sf(aes(fill = Richness_2020), color = NA) +
  theme_void() + 
  scale_fill_viridis_c(
    trans = "log", breaks = c(100, 1000, 1500, 2000, 2500, 5000),
    )

# # plot
# setwd(paste0(wdmain, "/output"))
# png("CurrentSpRichness_choropleth.png", width = 2400, height = 2400, units = "px", res = 300)
# try1
# dev.off()

# also map FC compliance
try1 <- ggplot(FC_data2) +
  geom_sf(aes(fill = rl_def), color = NA) +
  # theme_void() + 
  scale_fill_viridis_c()

# # plot
# setwd(paste0(wdmain, "/output"))
# png("RLdeficit_choropleth.png", width = 2400, height = 2400, units = "px", res = 300)
# try1
# dev.off()

rm(try1)

# get the biomes to add them as a shape on top of the map
# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(mask, proj = T))
plot(biomes)

# Biodiversity declines that could be reversed with full compliance of the FC in Brazil ----
head(FC_data2)

# create new column
FC_data2$deficit <- "no deficit" 
# identify the observations with deforestation where there was a deficit for either RL or APPs
# note this could overwrite the places where there was rl ativo (surplus), because app deficit is still be potentially illegal
# summary(FC_data2[which(FC_data2$rl_def < 0 | FC_data2$app_def < 0),])
# overwrite 
FC_data2[which(FC_data2$rl_def < 0 | FC_data2$app_def < 0),]$deficit <- "deficit"
FC_data2$deficit <- as.factor(FC_data2$deficit)
summary(FC_data2)

# filter data for (potential) illegal biodiversity loss 
propsDeficit <- FC_data2[which(FC_data2$deficit == "deficit"),] 
summary(propsDeficit) 

# PLOT BIODIVERSITY LOSS IN AREAS WITH FOREST CODE DEFICIT (RESTORATION) ----
# actually, i was thinking it would be good to have a raster base of all the properties in brazil

sample_FC <- sample_frac(FC_data2, 0.1)

deficitCols <- c("no deficit" = "#f0f0f0", "deficit" = "#f03b20")

# base map of all properties + properties with forest code deficits ----
# read in raster of all the properties we have
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- grep("SAalbers_1km.tif$", list.files())
t <- lapply(list.files()[l], rast) # very weird, changing behavior with lists from terra
# terra::plot(t)
names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", list.files()[l]))
# get rid of sigef and snci properties
t$SIGEF_properties <- NULL
t$SNCI_properties <- NULL

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(mask, proj = T))
plot(biomes)
v <- vect(biomes)

# make very simple map of all the areas with deficit in the forest code
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas.png", width = 2400, height = 2400, units = "px", res = 300)
# plot first one
plot(t[[1]], axes = F, mar = NA, legend = F, col = "gray90")
# add others on top
for (i in 2:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
# add the properties with deficits 
plot(propsDeficit, add = T, col = "#f03b20", border = NA, alpha = .8)
terra::lines(v, lwd=.1)
dev.off()

# map of how much biodiversity declines could be improved with compliance with the forest code? ----
# how much biodiversity weighted by how much is "owed"
# add the properties with deficits 
# total deficit for both APP and RL
propsDeficit$totalDeficit <- propsDeficit$rl_def + propsDeficit$app_def


# richness
# create variable of richness lost weighed by total deficit
propsDeficit$BDFC <- abs(propsDeficit$Richness_loss*propsDeficit$totalDeficit)
myCols <- c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081')
breaks <- classIntervals(propsDeficit$BDFC, na.rm = T, n=9)
# create factor variable based on these bins
propsDeficit$myCol1 <- cut(propsDeficit$BDFC, breaks = breaks$brks, labels = myCols, include.lowest = T)

setwd(paste0(wdmain, "/output/maps"))
png("BD-Richness_x_DeficitAreas.png", width = 2400, height = 2400, units = "px", res = 300)
# plot first one
plot(t[[1]], axes = F, mar = NA, legend = F, col = "gray90")
# add others on top
for (i in 2:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
plot(propsDeficit, col = as.character(propsDeficit$myCol1), add = T, border = NA)
terra::lines(v, lwd=.1)
dev.off()

# endemism
# create variable of endemism lost weighed by total deficit
propsDeficit$EndeFC <- abs(propsDeficit$Endemism_loss*propsDeficit$totalDeficit)
myCols <- c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081')
breaks <- classIntervals(propsDeficit$EndeFC, na.rm = T, n=9)
# create factor variable based on these bins
propsDeficit$myCol2 <- cut(propsDeficit$EndeFC, breaks = breaks$brks, labels = myCols, include.lowest = T)

setwd(paste0(wdmain, "/output/maps"))
png("BD-Endemism_x_DeficitAreas.png", width = 2400, height = 2400, units = "px", res = 300)
# plot first one
plot(t[[1]], axes = F, mar = NA, legend = F, col = "gray90")
# add others on top
for (i in 2:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
plot(propsDeficit, col = as.character(propsDeficit$myCol2), add = T, border = NA)
terra::lines(v, lwd=.1)
dev.off()


# need to create legend for areas - what is a high amount of deficit, and, what is a high amount of biodiversity loss?
# actually i'm re-evaluating the possibility of doing a bivariate map
# where i identify the areas with high biodiversity losses and high biodivesity deficits - to identify the most potential for restoration
# and the places where there isn't as high biodiversity loss, but high deficit

# make quick map to visualize the deficit in a continuous way
myCols <- c('#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
propsDeficit$totalDeficit2 <- abs(propsDeficit$totalDeficit)
breaks <- classIntervals(propsDeficit$totalDeficit2, na.rm = T, n=8)
propsDeficit$myCol3 <- cut(propsDeficit$totalDeficit2, breaks = breaks$brks, labels = myCols, include.lowest = T)
setwd(paste0(wdmain, "/output/maps"))
png("map_DeficitAreas_continuous.png", width = 2400, height = 2400, units = "px", res = 300)
# plot first one
plot(t[[1]], axes = F, mar = NA, legend = F, col = "gray90")
# add others on top
for (i in 2:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
plot(propsDeficit, col = as.character(propsDeficit$myCol3), add = T, border = NA)
terra::lines(v, lwd=.1)
dev.off()






# ...relate this to forest/deforestation somehow?
# richness:
ggplot(sample_FC) +
  geom_sf(aes(fill = deficit), color = NA) +
  scale_fill_manual(values = deficitCols) +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(0.1, 0.1))



  geom_sf(aes(fill = Richness_loss), color = NA) +
  scale_fill_viridis_c(direction = -1) +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(0.1, 0.1))
finalrichLoss <- richLoss + 
  geom_sf(data = biomes, fill = NA, color = "gray10") +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
# save plot
setwd(paste0(wdmain, "/output/maps"))
png("RichnessLoss_deficitAreas.png", width = 2400, height = 2400, units = "px", res = 300)
finalrichLoss
dev.off()


# endemism 
endLoss <- ggplot(illegalBDloss) +
  geom_sf(aes(fill = Endemism_loss), color = NA) +
  scale_fill_viridis_c(option = "magma", direction =-1) +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(0.1, 0.1))
finalendLoss <- endLoss + 
  geom_sf(data = biomes, fill = NA, color = "gray10") +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
# save plot
setwd(paste0(wdmain, "/output/maps"))
png("EndemismLoss_deficitAreas.png", width = 2400, height = 2400, units = "px", res = 300)
finalendLoss
dev.off()



