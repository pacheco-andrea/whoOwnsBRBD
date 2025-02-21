#### Biodiversity and Tenure in Brazil ####

# script to directly relate the biodiversity data with the Forest Code compliance (as a percentage) 



# use new % compliance data & figure out how to pair it ----
setwd(paste0(wdmain, "data/raw/nivel_conformidade_imoveis_panorama2024"))
compliance <- rast("nivel_conformidade_imoveis_panorama2024.tif")
plot(compliance)
my_colors <- colorRampPalette(c('#c51b7d', "gray90",'#4d9221'))(100)
plot(compliance, col = my_colors)

# transform
compliance2 <- project(compliance, my_crs_SAaea)
plot(compliance2, col = my_colors)
# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = my_crs_SAaea)
plot(biomes, add = T)

plot(biomes)
plot(compliance2, col = my_colors, add =T)
plot(biomes, add =T)

# is there a relationship bt compliance and biodiversity?----
# in other words, are the ones that conserve the most, the ones under most compliance
# AND does this include the overlaps?


#identify overlaps
test2 <- test[which(test$LTcateg != test$overlapsWith),]
test2[,c(2,17)]
# map overlaps with UCs or indigenous lands which would be the confounding thing
test3 <- test2[which(test2$overlapsWith != "Undesignated lands"),]
test3[,c(2,17)]

overl_deficit_plot <- ggplot(test3, aes(x = total_deficit, y = Richness_2020)) +
  geom_point(color = "#d95f0e", alpha = 0.2) +
  labs(
    title = "Overlapping private lands",
    x = "Total Deficit",
    y = "Richness (2020)"
  ) +
  theme_minimal()
overl_surplus_plot <- ggplot(test3, aes(x = rl_ativo, y = Richness_2020)) +
  geom_point(color = "#43a2ca", alpha = 0.2) +
  labs(
    title = "Overlapping private lands",
    x = "Total Surplus",
    y = "Richness (2020)"
  ) +
  theme_minimal()

# only non overlaps
test4 <- test[which(test$LTcateg == test$overlapsWith),]
nonoverl_deficit_plot <- ggplot(test4, aes(x = total_deficit, y = Richness_2020)) +
  geom_point(color = "#d95f0e", alpha = 0.2) +
  labs(
    title = "Non-overlapping private lands & rural settlements",
    x = "Total Deficit",
    y = "Richness (2020)"
  ) +
  theme_minimal()
nonoverl_surplus_plot <- ggplot(test4, aes(x = rl_ativo, y = Richness_2020)) +
  geom_point(color = "#43a2ca", alpha = 0.2) +
  labs(
    title = "Non-overlapping private lands & rural settlements",
    x = "Total Surplus",
    y = "Richness (2020)"
  ) +
  theme_minimal()

setwd(paste0(wdmain, "/output"))
png("scatterplot_BD-deficit-surplus_overlappingProperties.png", width = 3000, height = 2000, units = "px", res = 300)
plot_grid(overl_deficit_plot, nonoverl_deficit_plot, 
          overl_surplus_plot, nonoverl_surplus_plot, nrow = 2)
dev.off()

# make maps with biodiversity ----

# filter out the properties which were smallholder-rural-settlements bc they were <1% of either deficit or surplus
mapData <- data %>%
  filter(!(size == "smallholders" & LTcateg == "Rural settlements"))

# plot areas with SURPLUS ---- THIS I CAN DELETE
# in other words, how much are properties currently conserving?
# make surplus data subset
surplusData <- mapData[which(mapData$rl_ativo > 0),]
# surplus per property area
surplusData$rl_ativo_area <- (surplusData$rl_ativo/100)/surplusData$areakm2
breaks <- classInt::classIntervals(surplusData$rl_ativo_area, n = 5, style = "fisher")$brks
# Define colors for each break
colors <- c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#00441b") # Green gradient

# continuous map of the surplus 
setwd(paste0(wdmain, "/output/maps"))
png("MapSurplusAreas_continuous.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(surplusData) + 
  geom_sf(aes(fill = rl_ativo_area), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), # Rescale breaks to fit between 0 and 1
    limits = range(breaks), # Ensures scale matches the breaks
    oob = scales::squish # Handles out-of-bounds values
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.1, 0.1)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()

# plot biodiversity weighed by amount of SURPLUS ---- THIS IS WHAT I NEED TO RECREATE
# make richness * surplus variable
surplusData$rich_x_rlat <- surplusData$Richness_2020*surplusData$rl_ativo_area
# Calculate natural breaks using the Jenks method
breaks <- classInt::classIntervals(surplusData$rich_x_rlat, n = 5, style = "fisher")$brks
# Define colors for each break
colors <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac') # Green gradient

setwd(paste0(wdmain, "/output/maps"))
png("MapSurplusAreas_rich_x_rlativo.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(surplusData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = rich_x_rlat), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), # Rescale breaks to fit between 0 and 1
    limits = range(breaks), # Ensures scale matches the breaks
    oob = scales::squish # Handles out-of-bounds values
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()

# save as svg
setwd(paste0(wdmain, "/output/maps"))
svg("MapSurplusAreas_rich_x_rlativo.svg", width = 5, height = 5, pointsize = 12)
ggplot(surplusData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = rich_x_rlat), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), # Rescale breaks to fit between 0 and 1
    limits = range(breaks), # Ensures scale matches the breaks
    oob = scales::squish # Handles out-of-bounds values
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()


# repeat for endemism
surplusData$ende_x_rlat <- surplusData$Endemism_2020*surplusData$rl_ativo_area
# Calculate natural breaks using the Jenks method
breaks <- classInt::classIntervals(surplusData$ende_x_rlat, n = 5, style = "fisher")$brks
# Define colors for each break
colors <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac') # Green gradient

setwd(paste0(wdmain, "/output/maps"))
png("MapSurplusAreas_endemism_x_rlativo.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(surplusData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = ende_x_rlat), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), # Rescale breaks to fit between 0 and 1
    limits = range(breaks), # Ensures scale matches the breaks
    oob = scales::squish # Handles out-of-bounds values
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()



# plot current DEFICIT per property ----
# (not really needed for the paper, just for me to see)

deficitData <- mapData[which(mapData$total_deficit > 0),] # remember this is abs() in ha
# note, it does not make sense to normalize deficit property area bc the calculation of deficit implicitly includes a property's size
breaks <- classInt::classIntervals(deficitData$total_deficit, n = 5, style = "fisher")$brks
# Define colors for each break
colors <- c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404')

# continuous map of the deficit 
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_continuous.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(deficitData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = total_deficit), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), 
    limits = range(breaks), 
    oob = scales::squish 
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()

# try out what a proportion would look like 
deficitData$deficit_perc <- ((deficitData$total_deficit/100)/deficitData$areakm2)*100
breaks <- classInt::classIntervals(deficitData$deficit_perc, n = 5, style = "fisher")$brks

# proportional map of the deficit 
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_proportional.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(deficitData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = deficit_perc), color = NA) + # this would map the % of that property that is under deficit
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), 
    limits = range(breaks), 
    oob = scales::squish 
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off() 

# plot potential RESTORATION (increase in biodiversity) in properties with DEFICIT ----
# restor variable
deficitData$restor_rich <- deficitData$Richness_baseline*deficitData$deficit_perc
summary(deficitData$restor_rich)
breaks <- classInt::classIntervals(deficitData$restor_rich, n = 5, style = "fisher")$brks
colors <- c('#fcc5c0','#fa9fb5','#f768a1','#c51b8a','#7a0177')
#  map of the potential species to be restored per km2 (without human LUC) in properties with deficit 
# weighed by the proportion of a property
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_richnessRestore_proportional.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(deficitData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = restor_rich), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), 
    limits = range(breaks), 
    oob = scales::squish 
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()

# svg version for the legend - figure out better way to do this
setwd(paste0(wdmain, "/output/maps"))
svg("MapDeficitAreas_richnessRestore.svg", width = 5, height = 5, pointsize = 12)
ggplot(deficitData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = restor_rich), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), 
    limits = range(breaks), 
    oob = scales::squish 
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()

# endemism
deficitData$restor_ende <- deficitData$Endemism_loss*(deficitData$total_deficit/100)
summary(deficitData$restor_ende)
breaks <- classInt::classIntervals(deficitData$restor_ende, n = 5, style = "fisher")$brks
#  map of the potential endemism to be restored per km2 (without human LUC) in properties with deficit 
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_endemismRestore.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(deficitData) + 
  geom_sf(data = biomes, fill = "grey90", color = NA, size = 1) +
  geom_sf(aes(fill = restor_ende), color = NA) +
  scale_fill_gradientn(
    colors = colors, 
    values = scales::rescale(breaks), 
    limits = range(breaks), 
    oob = scales::squish 
  ) +
  theme(
    panel.background = element_blank(), 
    legend.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = c(0.2, 0.2)
  ) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()


