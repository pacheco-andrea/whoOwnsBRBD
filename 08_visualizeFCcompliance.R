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
# add states for more specific descriptions
states <- read_state()
states <- states$geom
states <- st_transform(states, crs = my_crs_SAaea)

# (before mapping) what is the distribution of surplus and deficit with these two categories? ----
data <- FC_data2
deficitTotals <- data %>% 
  st_drop_geometry() %>%
  group_by(LTcateg) %>%
  summarize(total_deficit = sum(total_deficit), 
            desmat_p08 = sum(desmat_p08))
sum(deficitTotals$total_deficit)
# the exact statement from the rotten apples paper is:
# 2% of of all properties in both biomes, which are bigger than 4FM, are responsible for 62% of all potentially illegal deforestations...
# also, 18% of properties responsible for 80% of illegal deforestation in both biomes
data$size <- NA
data[which(data$n_mf <= 4),]$size <- "smallholders"
data[which(data$n_mf > 4),]$size <- "largeholders"
summary(data)

deficitTotals <- data %>% 
  st_drop_geometry() %>%
  group_by(size, LTcateg) %>%
  summarize(n = n(),
            total_deficit = sum(total_deficit), 
            desmat_p08 = sum(desmat_p08),
            total_surplus = sum(rl_ativo)) %>%
  ungroup() %>%
  mutate(
    total_deficit_pct = total_deficit / sum(total_deficit) * 100,
    desmat_p08_pct = desmat_p08 / sum(desmat_p08) * 100,
    total_surplus_pct = total_surplus / sum(total_surplus) * 100
  ) %>%
  bind_rows(
    summarize(
      .,
      size = "Total",
      LTcateg = "Summary",
      n = sum(n),
      total_deficit = sum(total_deficit),
      desmat_p08 = sum(desmat_p08),
      total_surplus = sum(total_surplus),
      total_deficit_pct = 100,
      desmat_p08_pct = 100,
      total_surplus_pct = 100
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
            desmat_p08 = sum(desmat_p08),
            total_surplus = sum(rl_ativo)) %>%
  ungroup() %>%
  mutate(
    total_deficit_pct = total_deficit / sum(total_deficit) * 100,
    desmat_p08_pct = desmat_p08 / sum(desmat_p08) * 100,
    total_surplus_pct = total_surplus / sum(total_surplus) * 100
  ) %>%
  bind_rows(
    summarize(
      .,
      size = "Total",
      LTcateg = "Summary",
      n = sum(n),
      total_deficit = sum(total_deficit),
      desmat_p08 = sum(desmat_p08),
      total_surplus = sum(total_surplus),
      total_deficit_pct = 100,
      desmat_p08_pct = 100,
      total_surplus_pct = 100
    )
  )
deficitTotals_AMCE
deficitTotals
# the pattern is a bit more distinguishable here - 
# amazon and cerrado properties account for about 37% of properties, but almost 50% of deforestation post 2008
# but the amount of largeholders responsible is closer to 150,000 (rather than the 17,000 that was reported in the rotten apples)
# write out this information into a table for easy reference
setwd(paste0(wdmain, "/output"))
write.csv(deficitTotals, "deficitTotals.csv", row.names = F)
write.csv(deficitTotals_AMCE, "deficitTotals_amazon-cerrado.csv", row.names = F)

# make barplot to illustrate proportions of surplus and deficit ----
# transform data
deficitTotals_long <- deficitTotals %>%
  filter(!LTcateg %in% c("Summary")) %>% # Exclude summary rows
  select(size, LTcateg, total_deficit, total_surplus, total_deficit_pct, total_surplus_pct) %>% 
  pivot_longer(cols = c(total_deficit, total_surplus),
               names_to = "type",
               values_to = "value") %>%
  mutate(
    value = ifelse(type == "total_deficit", -value, value), # Negative for deficits
    pct = ifelse(type == "total_deficit", total_deficit_pct, total_surplus_pct), # Add percentages
    color_group = paste(type, LTcateg, sep = "_") # Create unique levels for color mapping
 )
# Define separate color palettes - these labels don't make sense, but they are sorted and correct in the figure
palette_deficit <- c("private lands" = "#fdc086", 
                     "rural settlements" = "#ff7f00", 
                     "smallholders" = "#fb9a99", 
                     "largeholders" = "#b30000") # Light to dark red-orange

palette_surplus <- c("private lands" = "#b2df8a", 
                     "rural settlements" = "#238b45", 
                     "smallholders" = "#8dd3c7", 
                     "largeholders" = "red") # Light to dark green

# Assign colors based on type (deficit/surplus) and categories
deficitTotals_long <- deficitTotals_long %>%
  mutate(color_group = interaction(type, size, LTcateg, drop = TRUE)) # Unique group for colors
# Define a color mapping
color_mapping <- c(
  setNames(palette_deficit, unique(deficitTotals_long$color_group[deficitTotals_long$type == "total_deficit"])),
  setNames(palette_surplus, unique(deficitTotals_long$color_group[deficitTotals_long$type == "total_surplus"]))
)
# clean up labeling
colnames(deficitTotals_long) <- gsub("total_", "", colnames(deficitTotals_long))
deficitTotals_long$type <- gsub("total_", "", deficitTotals_long$type)
# convert to km2 
deficitTotals_long$value <- deficitTotals_long$value/100

# Create the stacked bar plot
vegDeficitSurplus <- ggplot(deficitTotals_long, aes(x = type, y = value, fill = color_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_mapping) + # Apply custom color palette
  theme_minimal() +
  geom_text(data = deficitTotals_long %>% filter(pct > 5), # Only plot labels for percentages > 5%
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.9),
    size = 4
  ) + # Add percentage labels
  labs(
    x = "",
    y = "",
    fill = "Category",
    title = "Vegetation requirements for FC compliance"
  ) +
  scale_y_continuous(labels = scales::comma) + # Format y-axis
  theme(
    text = element_text(size = 14),
    legend.position = "bottom"
  ) 
# write out - note minor manual edits are made in inkscape
setwd(paste0(wdmain, "/output"))
svg("vegDeficitSurplus_barplot2.svg", width = 3, height = 10, bg = "white", pointsize = 12)
vegDeficitSurplus
dev.off()

# is there a relationship bt compliance and biodiversity?----
# in other words, do the properties that conserve the most, the ones with most compliance
# AND does this include the overlaps?
deficitTotals

test <- data %>% 
  st_drop_geometry()
head(test)

ggplot(test, aes(x = total_deficit, y = Richness_2020)) +
  geom_point(color = "blue", alpha = 0.3) +
  labs(
    title = "Scatterplot of Total Deficit vs Richness (2020)",
    x = "Total Deficit",
    y = "Richness (2020)"
  ) +
  theme_minimal()

unique(test$overlapsWith)

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

# plot areas with SURPLUS ----
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

# plot biodiversity weighed by amount of SURPLUS ----
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

setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_richnessRestore_proportional_withStates.png", width = 2400, height = 2400, units = "px", res = 300)
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
  geom_sf(data = states, fill = NA, color = "black", size = 1)
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









