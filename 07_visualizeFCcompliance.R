#### Biodiversity and Tenure in Brazil ####

# script that incorporates the Forest Code compliance (surplus or deficit of native vegetation) to our tenure + biodiversity data

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
s <- rbind(ast, iru)
head(s)
s <- s %>% select(id)
FC_data2 <- left_join(FC_data, s, by = c("id"))
FC_data2 <- st_as_sf(FC_data2) 
# check for empty geometries?
FC_data2[which(st_is_empty(FC_data2)),]
# transform to AEA
FC_data2 <- st_transform(FC_data2, my_crs_SAaea)
# get the total deficit
FC_data2$total_deficit <- abs(FC_data2$rl_def) + abs(FC_data2$app_def)
# map test ----

# start with a simple choropleth
try1 <- ggplot(FC_data2) +
  geom_sf(aes(fill = Richness_2020), color = NA) +
  theme_void() + 
  scale_fill_viridis_c(
    trans = "log", breaks = c(100, 1000, 1500, 2000, 2500, 5000),
    )
# plot
setwd(paste0(wdmain, "/output/maps"))
png("CurrentSpRichness_choropleth.png", width = 2400, height = 2400, units = "px", res = 300)
try1
dev.off()

# also map FC compliance
try1 <- ggplot(FC_data2) +
  geom_sf(aes(fill = abs(total_deficit)), color = NA) +
  theme_void() + 
  scale_fill_viridis_c(trans = "log", breaks = c(10, 10000, 20000, 40000))
# plot
setwd(paste0(wdmain, "/output/maps"))
png("RLdeficit_choropleth.png", width = 2400, height = 2400, units = "px", res = 300)
try1
dev.off()

rm(try1)
gc()

# get the biomes to add them as a shape on top of the map
# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = my_crs_SAaea)
plot(biomes)

# before investigating biodiversity, what is the distribution of deficit? ----
# are really only 2% of properties responsible for most deficit/deforestation?
data <- FC_data2
deficitTotals <- data %>% 
  st_drop_geometry() %>%
  group_by(LTcateg) %>%
  summarize(total_deficit = sum(total_deficit), # IRU are responsible for 90-91% of total deficit and total deforestation
            desmat_p08 = sum(desmat_p08))
sum(deficitTotals$total_deficit)
# the exact statement from the rotten apples paper is:
# 2% of of all properties in both biomes, which are bigger than 4FM, are responsible for 62% of all potentially illegal deforestations...
# based on the mean bt deforestation thresholds of 6.25 ha and 12.5 ha (Table S18)??
# then, 18% of properties responsible for 80% of illegal deforestation in both biomes
data$size <- NA
data[which(data$n_mf <= 4),]$size <- "smallholders"
data[which(data$n_mf > 4),]$size <- "largeholders"
summary(data)

deficitTotals <- data %>% 
  st_drop_geometry() %>%
  group_by(size, LTcateg) %>%
  summarize(n = n(),
            total_deficit = sum(total_deficit), # IRU are responsible for 90-91% of total deficit and total deforestation
            desmat_p08 = sum(desmat_p08)) %>%
  ungroup() %>%
  mutate(
    total_deficit_pct = total_deficit / sum(total_deficit) * 100,
    desmat_p08_pct = desmat_p08 / sum(desmat_p08) * 100
  ) %>%
  bind_rows(
    summarize(
      .,
      size = "Total",
      LTcateg = "Summary",
      n = sum(n),
      total_deficit = sum(total_deficit),
      desmat_p08 = sum(desmat_p08),
      total_deficit_pct = 100,
      desmat_p08_pct = 100
    )
  )
deficitTotals
# ok this shows consistency with the rotten apples paper:
# ~83% of deforestation post 2008 is in largeholders (73% in private lands, specifically) 
# BUT the properties are many more than those reported in the original paper
# almost 400,000 properties are larger than 4 FM and responsible for 73% deforestation after 2008...
# the paper reported 17,557. is this because they only considered amazon and cerrado?
deficitTotals_AMCE <- data %>% 
  st_drop_geometry() %>%
  filter(biome %in% c("Amazonia", "Cerrado"))  %>%
  group_by(size, LTcateg) %>%
  summarize(n = n(),
            total_deficit = sum(total_deficit), # IRU are responsible for 90-91% of total deficit and total deforestation
            desmat_p08 = sum(desmat_p08)) %>%
  ungroup() %>%
  mutate(
    total_deficit_pct = total_deficit / sum(total_deficit) * 100,
    desmat_p08_pct = desmat_p08 / sum(desmat_p08) * 100
  ) %>%
  bind_rows(
    summarize(
      .,
      size = "Total",
      LTcateg = "Summary",
      n = sum(n),
      total_deficit = sum(total_deficit),
      desmat_p08 = sum(desmat_p08),
      total_deficit_pct = 100,
      desmat_p08_pct = 100
    )
  )
deficitTotals_AMCE
deficitTotals
# the pattern is a bit more distinguishable here - 

# what does this mean for biodiversity


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

# filter data to only observations with deficit
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

# make quick map to visualize the deficit in a continuous way
# total deficit for both APP and RL
propsDeficit$totalDeficit <- propsDeficit$rl_def + propsDeficit$app_def
propsDeficit$totalDeficit2 <- abs(propsDeficit$totalDeficit)
propsDeficit$totalDeficit2 <- propsDeficit$totalDeficit2*10 # convert to km2 for consistency
# make color palette
myCols <- c('#fff7bc','#fee391','#fec44f','#fe9929','#993404','#662506')

# create breaks that correspond with the palette
breaks <- classIntervals(propsDeficit$totalDeficit2, na.rm = T, n=6)
propsDeficit$myCol3 <- cut(propsDeficit$totalDeficit2, breaks = c(0,10,50,100,1000,10000,100000), labels = myCols, include.lowest = T)

# Legend parameters
legend_colors <- c('#fff7bc','#fee391','#fec44f','#fe9929','#993404','#662506')
legend_labels <- c("<10", "11-50", "51-1000", "1001-10,000", "10,001-100,000", ">100,000")

# write out
setwd(paste0(wdmain, "/output/maps"))
png("map_DeficitAreas_continuous_20241031.png", width = 2400, height = 2400, units = "px", res = 300)
# plot first one
plot(t[[1]], axes = F, mar = NA, legend = F, col = "gray90")
# add others on top
for (i in 2:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
plot(propsDeficit, col = as.character(propsDeficit$myCol3), add = T, border = NA)
terra::lines(v, lwd=.1)

# Add legend in the bottom left
legend("bottomleft", 
       legend = legend_labels, 
       fill = legend_colors, 
       border = "gray30",
       bty = "n",    # No box around legend
       title = "Deficit (km2)",
       cex = 0.8)    # Adjust text size if necessary

dev.off()


# map of how much biodiversity declines could be improved with compliance with the forest code? ----
# how much biodiversity weighted by how much is "owed"
# add the properties with deficits 



# richness
# create variable of richness lost weighed by total deficit
propsDeficit$BDFC <- (propsDeficit$Richness_loss*propsDeficit$totalDeficit2)
myCols <- c('#f7fcf0','#ccebc5','#a8ddb5','#7bccc4','#2b8cbe','#084081')
breaks <- classIntervals(propsDeficit$BDFC, na.rm = T, n=6)
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
propsDeficit$EndeFC <- (propsDeficit$Endemism_loss*propsDeficit$totalDeficit2)
myCols <- c('#f7fcf0','#ccebc5','#a8ddb5','#7bccc4','#2b8cbe','#084081')
breaks <- classIntervals(propsDeficit$BDFC, na.rm = T, n=6)
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



