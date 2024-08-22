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

# classify BD vars
BDbreaks_rich <- classIntervals(FC_data2$richness_current, n = 3, style = "fisher")
BDbreaks_rich$brks
BD_richClasses <- cut(FC_data2$richness_current, 
                      breaks = BDbreaks_rich$brks, 
                      labels = c("low", "medium", "high"), 
                      include.lowest = TRUE)

# classify FC variables
# is there deficit
deficit <- abs(FC_data2$rl_def + FC_data2$app_def)
breaks_deficit <- classIntervals(deficit, n = 3, style = "fisher")
deficit_classes <- cut(deficit,
                       breaks = breaks_deficit$brks, 
                       labels = c("low", "medium", "high"), 
                       include.lowest = TRUE)

summary(BD_richClasses)
summary(deficit_classes) # because these are negative numbers, this would mean low, medium, and high COMPLIANCE
# in other words, low == high deficit. see:
deficit[which(deficit_classes == "low")]
deficit[which(deficit_classes == "medium")]
summary(deficit[which(deficit_classes == "high")]) # high compliance == a "debt" of < 680ha 
# instead, i know i want to present this as low, medium, and high deficit. 
# hence, i transform to positive

deficit <- abs(FC_data2$rl_def + FC_data2$app_def)
# deficit <- abs(FC_data2$rl_def)
breaks_deficit <- classIntervals(deficit, n = 3, style = "fisher", sampled = 1e6)
breaks_deficit$brks

# i need to figure out how this function works because it's resulting in something different each time
set.seed(123)
sampled_data <- sample(deficit, size = 100000, replace = F)
breaks <- classIntervals(sampled_data, n=3, style = "fisher")
breaks$brks

ggplot(as.data.frame(sampled_data), aes(x=sampled_data)) + geom_histogram()

deficit_classes <- cut(deficit,
                       breaks = breaks_deficit$brks, 
                       labels = c("low", "medium", "high"), 
                       include.lowest = TRUE)
summary(deficit_classes) 

paste0("BD-", BD_richclasses, "FC", deficit_classes)

FC_data2$rich_deficit



# parallel coord plot
# where i want to plot the biodiversity variables (current and loss) and the forest compliance per tenure category
