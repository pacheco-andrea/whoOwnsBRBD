#### Biodiversity and Tenure in Brazil ####

# script that incroporates the Forest Code compliance (surplus or deficit of native vegetation) to our tenure + biodiversity data

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(cowplot)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")



# get data table tenure - BD - CSR ----
setwd(paste0(wdmain, "data/processed/"))
data_extra <- read.csv("finalDataset_Tenure-BD-CSR.csv")
head(data_extra)

# filter data for which we have the FC surplus/deficit information
FC_data <- data_extra[which(!is.na(data_extra$area_conv)),]
summary(FC_data)

# get geometry data that corresponds to these data above ----
# in which directories would i find IRU and AST?

# AST
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
ast <- st_read("ruralSettlements.shp")

# IRU
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
iru <- st_read("ruralProperties.shp")

FC_data <- left_join(FC_data, rbind(ast,iru), by = "id")
FC_data <- st_as_sf(FC_data) # STOP HERE AND CHECK WHETHER ALL MY OBS HAVE GEOMETRIES

# what would actually make sense is a chloropleth map of the properties with high biodiversity + high deficit!!!!
# keep in mind that i might have to normalize some of the BD variables (e.g. species richness - is already the mean of the polygon, weighed by the % that the pixel covered. but this should still be divided by the area, no?)


# as I'm only plotting for visualization purposes, i need to simplify these polygons because they are wayy to fine to plot easily
FC_data2 <- st_simplify(FC_data, dTolerance = 1000)
FC_data2 <- FC_data2[!st_is_empty(FC_data2),] # however, this means we lose ~half our observations
FC_data2 <- st_transform(FC_data2, my_crs_SAaea)

# map ----
# start with a simple choropleth
ggplot(FC_data2) +
  geom_sf(aes(fill = mean.Species_Richness), linewidth = 0, alpha = 0.9) +
  theme_void() + 
  scale_fill_viridis_c(
    trans = "log", breaks = c(100, 1000, 1500, 2000, 2500, 5000),
    )

# ok but what i actually have to do is another column where i combine BD x FC compliance
