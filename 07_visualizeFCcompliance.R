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
FC_data2 <- st_simplify(FC_data, dTolerance = 100) # at 1000, we lose ~60% of observations
FC_data2 <- FC_data2[!st_is_empty(FC_data2),] # i see that this is now 99% of the data, so it takes a while but it is pretty worth it in plotting the map
FC_data2 <- st_transform(FC_data2, my_crs_SAaea)

# map test ----

# start with a simple choropleth
try1 <- ggplot(FC_data2) +
  geom_sf(aes(fill = richness_current), color = NA) +
  theme_void() + 
  scale_fill_viridis_c(
    trans = "log", breaks = c(100, 1000, 1500, 2000, 2500, 5000),
    )

# plot
setwd(paste0(wdmain, "/output"))
png("CurrentSpRichness_choropleth.png", width = 2400, height = 2400, units = "px", res = 300)
try1
dev.off()

# also map FC compliance
try1 <- ggplot(FC_data2) +
  geom_sf(aes(fill = rl_def), color = NA) +
  # theme_void() + 
  scale_fill_viridis_c()

# plot
setwd(paste0(wdmain, "/output"))
png("RLdeficit_choropleth.png", width = 2400, height = 2400, units = "px", res = 300)
try1
dev.off()

rm(try1)

# make BD and FC compliance variables categories for choropleth map ----
# do i need to think about normalizing some of the BD variables?
# (e.g. species richness - is already the mean of the polygon, weighed by the % that the pixel covered. but this should still be divided by the area, no?)
# the loss variables are already normalized but will i be plotting this?
set.seed(123)
# classify BD vars
BDbreaks_rich <- classIntervals(FC_data2$richness_current, n = 3, style = "fisher")
BDbreaks_rich$brks
BD_richClasses <- cut(FC_data2$richness_current, 
                      breaks = BDbreaks_rich$brks, 
                      labels = c("low", "medium", "high"), 
                      include.lowest = TRUE)

# classify FC variables
# is there deficit
deficit <- abs(FC_data2$rl_def + FC_data2$app_def) # i take the absolute values so that high values equal high deficit
breaks_deficit <- classIntervals(deficit, n = 3, style = "fisher")
breaks_deficit$brks
deficit_classes <- cut(deficit,
                       breaks = breaks_deficit$brks, 
                       labels = c("low", "medium", "high"), 
                       include.lowest = TRUE)

summary(BD_richClasses)
summary(deficit_classes) 
nrow(FC_data2)

# add as a column to data
FC_data2$rich_deficit <- paste0("BD-", BD_richClasses, "_FC-", deficit_classes)
table(FC_data2$rich_deficit) # there are NO places where there is high biodiversity and high FC deficit nor high FC deficit and low biodiversity
# this, to me, signals a poor job of the class intervals of the FC deficit

unique(FC_data2$rich_deficit)
bivariateCols <- c("BD-low_FC-low" = "#E8e8e8",
     "BD-low_FC-medium" = "#ace4e4",   
     "BD-low_FC-high" = "#5ac8c8",       
     "BD-medium_FC-low" =  "#dfb0d6",
     "BD-medium_FC-medium" = "#a5add3",  
     "BD-medium_FC-high" =  "#5698b9",
     "BD-high_FC-low" = "#be64ac",
     "BD-high_FC-medium" = "#8c62aa",
     "BD-high_FC-high" ="#3b4994",
     "BD-NA_FC-low" = "#E8e8e8",
     "BD-NA_FC-medium" = "#ace4e4")

# get the biomes to add them as a shape on top of the map
# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(mask, proj = T))
plot(biomes)

ggplot(biomes) +
  geom_sf(fill = NA, color = "gray10") +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


# bivariate map richness
biMap <- ggplot(FC_data2) +
  geom_sf(aes(fill = rich_deficit), color = NA) +
  scale_colour_manual(values = bivariateCols, aesthetics = c("color", "fill")) +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
finalbiMap <- biMap + 
  geom_sf(data = biomes, fill = NA, color = "gray10") +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


# plot
setwd(paste0(wdmain, "/output"))
png("bivariateMapBiomes.png", width = 2400, height = 2400, units = "px", res = 300)
finalbiMap
dev.off()

# bivariate map for other categories???
biMap <- ggplot(FC_data2) +
  geom_sf(aes(fill = rich_deficit), color = NA) +
  scale_colour_manual(values = bivariateCols, aesthetics = c("color", "fill")) +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
finalbiMap <- biMap + 
  geom_sf(data = biomes, fill = NA, color = "gray10") +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


# plot
setwd(paste0(wdmain, "/output"))
png("bivariateMapBiomes.png", width = 2400, height = 2400, units = "px", res = 300)
finalbiMap
dev.off()

# # test other function - i hated this
# ?bi_class # this function automatizes the classification, which i'm never a fan of
# # it also seems to simplify the geometry, which makes for a bit faster plotting, but then the map is quite holey
# test <- bi_class(FC_data2, x = richness_current, y = rl_def, style = "fisher", dim = 3)
# testMap <- ggplot() +
#   geom_sf(data = test, mapping = aes(fill = bi_class), color = "white", size = 0.1) +
#   bi_scale_fill(pal = "GrPink", dim = 3) +
#   labs(title = "test richness and RL deficit") +
#   bi_theme() 
# setwd(paste0(wdmain, "/output"))
# png("RLdeficit_richness_bivariate_test.png", width = 2400, height = 2400, units = "px", res = 300)
# testMap
# dev.off() # remember that here high values == high deficit

# parallel coord plot ----
# where i want to plot the biodiversity variables (current and loss) and the forest compliance per tenure category

# would it make most sense for comparing just the current and the loss?

parcoordPlot <- ggparcoord(FC_data2,
                           columns = grep("richness", colnames(FC_data2)), groupColumn = grep("LTcateg2", colnames(FC_data2)),
                           showPoints = T,
                           title = "Parallel Coordinates plot test for species richness (baseline, current, loss)",
                           alphaLines = 0.3) +
  theme_ipsum() +
  theme(plot.title = element_text(size = 10))

setwd(paste0(wdmain, "/output"))
png("parallelCoord_test_richness.png", width = 2400, height = 2400, units = "px", res = 300)
parcoordPlot
dev.off()





