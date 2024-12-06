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

# make accompanying figure to illustrate proportions of surplus and deficit ----
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
  ) +
  coord_flip()
# write out - note minor manual edits are made in inkscape
setwd(paste0(wdmain, "/output"))
svg("vegDeficitSurplus_barplot.svg", width = 10, height = 3, bg = "white", pointsize = 12)
vegDeficitSurplus
dev.off()





# what does this mean for biodiversity?? ----

# filter out the properties which were smallholder-rural-settlements bc they were <1% of either deficit or surplus
mapData <- data %>%
  filter(!(size == "smallholders" & LTcateg == "Rural settlements"))

# 1) Current biodiversity in areas with SURPLUS 
# in other words, how much are properties currently conserving?
# make surplus data subset
surplusData <- mapData[which(mapData$rl_ativo > 0),]
# Calculate natural breaks 
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

# make richness * surplus variable
surplusData$rich_x_rlat <- surplusData$Richness_2020*surplusData$rl_ativo_area

# Calculate natural breaks using the Jenks method
breaks <- classInt::classIntervals(surplusData$rich_x_rlat, n = 5, style = "fisher")$brks
# Define colors for each break
colors <- c('#f6eff7','#bdc9e1','#67a9cf','#1c9099','#016c59') # Green gradient

setwd(paste0(wdmain, "/output/maps"))
png("MapSurplusAreas_rich_x_rlativo.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot(surplusData) + 
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
    legend.position = c(0.1, 0.1)
  ) +
  # i think i need to conduct an st_union for all rural settlements
  # geom_sf(
  #   data = rSett, 
  #   fill = NA, color = "#FC8D62", size = 0.01) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()

# repeat for endemism

# check whether surplus and deficit coexist?
check <- mapData[which(mapData$rl_ativo > 0),] # not for the RL but yes, some, for the total


# Current biodiversity in areas with DEFICIT (not needed)
# Rather, Biodiversity potential increase in areas with DEFICIT
# filter data to A) private lands - largeholders with deficit
IRU_Deficit <- data[which(data$size == "largeholders" & data$LTcateg == "Private lands" & data$total_deficit > 0),]
summary(IRU_Deficit) 
# filter data to B) rural-settlement-largeholders with deficit
AST_Deficit <- data[which(data$size == "largeholders" & data$LTcateg == "Rural settlements" & data$total_deficit > 0),]
summary(AST_Deficit) 

# plot areas with deficit (areas with potential restoration) 
# make base map of all properties + properties with forest code deficits
# read in raster of all the properties we have
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- grep("SAalbers_1km.tif$", list.files())
t <- lapply(list.files()[l], rast) 
names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", list.files()[l]))
# get rid of sigef and snci properties
t$SIGEF_properties <- NULL
t$SNCI_properties <- NULL
v <- vect(biomes)

# make very simple map of all the areas with deficit in the forest code
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_largeholders.png", width = 2400, height = 2400, units = "px", res = 300)
# start by plotting biomes to roraima isn't sliced off
plot(biomes)
# plot rasters of other tenure categories as a base
for (i in 1:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
# add the properties with deficits 
plot(IRU_Deficit, add = T, col = "#993404", border = NA, alpha = .8)
plot(AST_Deficit, add = T, col = "#fec44f", border = NA, alpha = .8)
terra::lines(v, lwd=.1)
dev.off() # remember, this shows the properties responsible for more than 80% of deforestation, and 90% of all deficit

# plot biodiversity in these areas

setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_largeholders_richness.png", width = 2400, height = 2400, units = "px", res = 300) # this one isn't working...
# start by plotting biomes to roraima isn't sliced off
plot(biomes)
# plot rasters of other tenure categories as a base
for (i in 1:length(t)) {
  plot(t[[i]], axes = F, , col = "gray90", mar = NA, legend = F, add = TRUE)  # Add each raster on top of the previous one
}
# add the properties with deficits 
plot(IRU_Deficit["Richness_2020"], add = T, col = hcl.colors(100, "Viridis"), border = NA, alpha = .8)
terra::lines(v, lwd=.1)
dev.off() # the colors are off from the data...

# try with ggplot
setwd(paste0(wdmain, "/output/maps"))
png("MapDeficitAreas_largeholders_richness_test.png", width = 2400, height = 2400, units = "px", res = 300)
ggplot() + 
  geom_sf(data = IRU_Deficit, aes(fill = Richness_2020), color = NA) +
  scale_fill_viridis_c() +
  theme(panel.background = element_blank(), legend.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(0.1, 0.1)) +
  geom_sf(data = biomes, fill = NA, color = "black", size = 1)
dev.off()






# 2) how much biodiversity could be restored potentially, with full compliance of the FC? ----
# option A): weigh biodiversity "loss/change" (the difference bt current and potential) by the amount owed (total_deficit)
IRU_Deficit
# option B): how much are RL's conserving current BD - that is, RL ativo

# richness
# create variable of richness loss weighed by total deficit
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








deficitCols <- c("no deficit" = "#f0f0f0", "deficit" = "#f03b20")


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



