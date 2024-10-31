#### Biodiversity and Tenure in Brazil ####

# script that visualizes biodiversity indicators per tenure categ that have previously been extracted 

# two main outcomes:
# 1) boxplots of all BD variables (with the option of seeing the categories that are overlapping)
# 2) bd+tenure dataset joined with original information from CSR on the forest code deficit variables

# now need to add comparisons to deforestation

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(cowplot)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# read data on biodiversity extractions (land tenure + biodiversity) ----

setwd(paste0(wdmain, "/data/processed/bdExtractions-perPolygon_v202410"))
l <- list.files()
tables <- lapply(l, read.csv)
lapply(tables, colnames)
names(tables) <- gsub(".csv","", l)
lapply(tables, colnames)

# the tables have different column names, need to make them consistent to join into one table
myColumns <- c("LTcateg", "id", "areakm2", "name_biome",
               "mean.Endemism_2020","mean.Endemism_baseline","mean.Endemism_loss",
               "mean.Phylodiversity_2020","mean.Phylodiversity_baseline","mean.Phylodiversity_loss",
               "mean.Richness_2020","mean.Richness_baseline","mean.Richness_loss")

data <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # select my columns
  df <- df[,myColumns]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
# lapply(data, colnames)
data1 <- do.call(rbind, data)
colnames(data) <- gsub("mean.", "", colnames(data))


# DEAL WITH DUPLICATES  ----

# deal with duplicated id's, i.e., id's with multiple observations 
# i've realized that i've got duplicate ids, across all categories
# there seem to be two sources of duplicates - the assignment of biomes, but also the splitting up across the overlaps
# most pragmatically (bc we're now ignoring the overlaps)
# is to go ahead and summarize this data by grouping by id, summing the area, averaging the bd variables, and rechecking if duplicates remain

data2 <- data1 %>%
  group_by(id) %>%
  summarize(
    LTcateg = first(LTcateg),
    myOverCat = first(myOverCat),
    areakm2 = sum(areakm2, na.rm = T), 
    Endemism_2020 = mean(Endemism_2020, na.rm = T),
    Endemism_baseline = mean(Endemism_baseline, na.rm = T),
    Endemism_loss = mean(Endemism_loss, na.rm = T),
    Phylodiversity_2020 = mean(Phylodiversity_2020, na.rm = T),
    Phylodiversity_baseline = mean(Phylodiversity_baseline, na.rm = T),
    Phylodiversity_loss = mean(Phylodiversity_loss, na.rm = T),
    Richness_2020 = mean(Richness_2020, na.rm = T),
    Richness_baseline = mean(Richness_baseline, na.rm = T),
    Richness_loss = mean(Richness_loss, na.rm = T)) %>%
  ungroup()
head(data2)
data2 %>%
  group_by(LTcateg) %>%
  summarize(n = n())

# NOTE - I SHOULD STILL deal with the duplicates before the extractions ...

# read data on deforestation extractions ----

setwd(paste0(wdmain, "/data/processed/forestExtractions-perPolygon"))
l <- list.files()
tables <- lapply(l, read.csv)
lapply(tables, colnames)
names(tables) <- gsub(".csv","", l)

myColumns <- c("LTcateg", "id", "for23", "defor")
fData <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # select my columns
  df <- df[,myColumns]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
fData <- do.call(rbind, fData)
head(fData)

# check if the id's are the same in the BD extractions as with the forest extractions
summary(data$id == fData$id) # ok, this means i canNOT simply cbind 

# similarly, i have to summarize by id, but sum the forest values (which are counts) 

# sum forest extraction data
forestData <- fData %>%
  group_by(id) %>%
  summarize(LTcateg = first(LTcateg),
            for23 = sum(for23, na.rm = T),
            defor = sum(defor, na.rm = T))
nrow(forestData)
head(forestData)
length(unique(forestData$id))

forestData %>%
  group_by(LTcateg) %>%
  summarize(n = n())

# join forest data with biodiversity data
forest_BD_data <- inner_join(data2, forestData, by = c("LTcateg", "id"))
nrow(forest_BD_data)
head(forest_BD_data)
length(unique(forest_BD_data$id))


# other data cleaning and organizing ----

data <- forest_BD_data
# filter out polygons that are < .9 km2 in area - the resolution of the BD data (0.0083 degrees == roughly 900m2)
data.1km <- data[which(data$areakm2 < .9),] 
nrow(data.1km)
# for sure - if the added up version doesn't add up to the minimum, then it should be filtered out

# quick visualization
# ggplot(data.1km, aes(x = LTcateg)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# note: 5806802 IRU properties were <.5km, out of the 7067703 total (= 82% of records)
# 5806802/7067703
# and 693756 properties are >1km, which is only 9% of records
# write out this data so that i can check it
# setwd(paste0(wdmain, "data/processed/"))
# write.csv(data.5km, "LT-BD_areaUnder.5km.csv", row.names = F)

#  only use the parcels with greater area than the BD raster's resolution
nrow(data[which(data$areakm2 >= .9),])
data <- data[which(data$areakm2 >= .9),]


# filter out these undesignated categories which are a bit ambiguous 
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]


# check how many observations per category i have in the original data
data %>%
  group_by(LTcateg) %>%
  summarize(n = n())

data.1km %>%
  group_by(LTcateg) %>%
  summarize(n = n())

# cleaning of the overlaps (creating specific columns): ----
unique(data$LTcateg)
unique(data$myOverCat)

# create new column
data$overlapsWith <- NA
# no overlaps
data$overlapsWith[grep("no-overlaps", data$myOverCat)] <- "none"
# overlaps with self
data$overlapsWith[grep("self", data$myOverCat)] <- "self"
# more complicated situations
head(data[which(is.na(data$overlapsWith)),])
unique(data$myOverCat[which(is.na(data$overlapsWith))])
data$overlapsWith[which(is.na(data$overlapsWith))] <- gsub("overlaps_", "", data$myOverCat[which(is.na(data$overlapsWith))])
unique(data$overlapsWith) 
data$overlapsWith <- gsub("pubxpri_", "", data$overlapsWith)
unique(data$overlapsWith)

overlapsToCheck <- unique(data$overlapsWith)[-c(grep("self", unique(data$overlapsWith)), grep("none", unique(data$overlapsWith)))]

# easiest to just replace manually each set
data$overlapsWith[which(data$overlapsWith == "indPAoverlap-ruralSettlements")] <- gsub("indPAoverlap-", "", data$overlapsWith[which(data$overlapsWith == "indPAoverlap-ruralSettlements")])
data$overlapsWith[which(data$overlapsWith == "PA_strict-indigenous")] <- gsub("PA_strict-", "", data$overlapsWith[which(data$overlapsWith == "PA_strict-indigenous")])
data$overlapsWith[which(data$overlapsWith == "PA_sustuse-indigenous")] <- gsub("PA_sustuse-", "", data$overlapsWith[which(data$overlapsWith == "PA_sustuse-indigenous")])
data$overlapsWith[which(data$overlapsWith == "privatePAs-ruralProperties")] <- gsub("privatePAs-ruralProperties", "none", data$overlapsWith[which(data$overlapsWith == "privatePAs-ruralProperties")])
data$overlapsWith[which(data$overlapsWith == "quilombola-ruralProperties")] <- gsub("quilombola-", "", data$overlapsWith[which(data$overlapsWith == "quilombola-ruralProperties")])
data$overlapsWith[which(data$overlapsWith == "sustUsePAs-ruralSettlements")] <- gsub("sustUsePAs-", "", data$overlapsWith[which(data$overlapsWith == "sustUsePAs-ruralSettlements")])
data$overlapsWith[which(data$overlapsWith == "undesignated-ruralSettlements")] <- gsub("undesignated-", "", data$overlapsWith[which(data$overlapsWith == "undesignated-ruralSettlements")])
data$overlapsWith[which(data$overlapsWith == "ruralProperties-PA_strict")] <- gsub("ruralProperties-", "", data$overlapsWith[which(data$overlapsWith == "ruralProperties-PA_strict")])
data$overlapsWith[which(data$overlapsWith == "ruralProperties-PA_sustuse")] <- gsub("ruralProperties-", "", data$overlapsWith[which(data$overlapsWith == "ruralProperties-PA_sustuse")])
data$overlapsWith[which(data$overlapsWith == "ruralProperties-undesignated")] <- gsub("ruralProperties-", "", data$overlapsWith[which(data$overlapsWith == "ruralProperties-undesignated")])
unique(data$overlapsWith)

# create variables for the proportion of loss
data$rLoss_prop <- data$Richness_loss/(data$Richness_baseline - data$Richness_loss)
# i wanted to compare the bottom part of this proportion to the "current" layer - which is what it should be
# through some testing i found there were very minor, slight differences
# these differences would all point to BD losses which were *not* due to human-driven LUC
# which means the safest - or the most accurate to human-driven changes - way is to stick to using the difference bt baseline and loss (rather than simply the current)
data$eLoss_prop <- data$Endemism_loss/(data$Endemism_baseline - data$Endemism_loss)
data$pLoss_prop <- data$Phylodiversity_loss/(data$Phylodiversity_baseline - data$Phylodiversity_loss)

# clean names for plotting -----

# make overlapsWith correspond with my naming of categories
unique(data$LTcateg)
data$LTcateg[which(data$LTcateg == "IRU")] <- "Private lands"
data$LTcateg[which(data$LTcateg == "SEM DESTINACAO")] <- "Undesignated lands"
data$LTcateg[which(data$LTcateg ==  "AST")] <- "Rural settlements"
data$LTcateg[which(data$LTcateg == "PI")] <- "PA strict protection"
data$LTcateg[which(data$LTcateg == "US")] <- "PA sustainable use"
data$LTcateg[which(data$LTcateg == "indigenous")] <- "Indigenous"
data$LTcateg[which(data$LTcateg == "RPPN")] <- "Private PA"
data$LTcateg[which(data$LTcateg == "quilombola")] <- "Quilombola lands"
data$LTcateg2 <- factor(data$LTcateg, levels = c("Private lands",
                                                 "Undesignated lands",
                                                 "Rural settlements",
                                                 "PA strict protection",
                                                 "PA sustainable use",
                                                 "Indigenous" ,
                                                 "Private PA",
                                                 "Quilombola lands"))
unique(data$overlapsWith)
# replace the self and none overlaps with the category itself
data$overlapsWith[which(data$overlapsWith == "self")] <- as.character(data$LTcateg2[which(data$overlapsWith == "self")])
data$overlapsWith[which(data$overlapsWith == "none")] <- as.character(data$LTcateg2[which(data$overlapsWith == "none")])
data$overlapsWith[which(data$overlapsWith == "ruralProperties")] <- "Private lands"
data$overlapsWith[which(data$overlapsWith == "undesignated")] <- "Undesignated lands"
data$overlapsWith[which(data$overlapsWith ==  "ruralSettlements")] <- "Rural settlements"
data$overlapsWith[which(data$overlapsWith == "PA_strict")] <- "PA strict protection"
data$overlapsWith[which(data$overlapsWith == "PA_sustuse")] <- "PA sustainable use"
data$overlapsWith[which(data$overlapsWith == "indigenous")] <- "Indigenous"
data$overlapsWith[which(data$overlapsWith == "RPPN")] <- "Private PA"
data$overlapsWith[which(data$overlapsWith == "quilombola")] <- "Quilombola lands"
unique(data$overlapsWith)

# actually remove private PAs from plotting
data <- data[which(data$LTcateg2 != "Private PA"),]


# make into a factor
data$overlapsWith2 <- factor(data$overlapsWith, levels = c("Private lands",
                                                           "Undesignated lands",
                                                           "Rural settlements",
                                                           "PA strict protection",
                                                           "PA sustainable use",
                                                           "Indigenous" ,
                                                           "Private PA",
                                                           "Quilombola lands"))

setwd(paste0(wdmain, "/output"))
write.csv(data, "finalTenure&BD&ForestDatasetforPlotting.csv", row.names = FALSE)


# PLOTS ----

tenureColors <- c("Indigenous" = "#E78AC3",
                  "non-overlapped" = "gray70",   
                  "PA strict protection" = "#1B9E77",       
                  "PA sustainable use" =  "#8C7E5B",
                  # "Private PA" = "#99d8c9",  
                  "Quilombola lands" =  "#FFD700",
                  "Private lands" = "#8DA0CB",
                  "Rural settlements" = "#FC8D62",
                  "Undesignated lands" ="#1d6c7d")
# create variable with amount of observations per category to include as a label in my plots
sample_sizes <- data %>%
  group_by(LTcateg2) %>%
  summarize(n = n())



# create function for plotting 
# this function plots a violin (for distribution) with a boxplot on top
# where showing the overlaps is optional 

violinplotBD <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL, fill) {
  
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = {{fill}})) +
    geom_violin(position = position_dodge(width = 0.9), alpha = 0.2) +
    geom_boxplot(width=0.2, color="grey20", position = position_dodge(width = 0.9), alpha = 0.8) +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    labs(y = BDvariableTitle) +
    theme(panel.background = element_blank(), panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
          legend.title = element_blank(), legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank()) +
    # facet_wrap(vars(LTcateg2), nrow = 1, scales = "free_x") +
    geom_text(data = sample_sizes, aes(x = LTcateg2, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)
  
  return(plot)
}


# current biodiversity under different tenure categories ----

# Version WITH overlaps
# ACTUALLY - NOW THAT I'VE SUMMARIZED THE DATA, AND KEEPING ONLY THE FIRST OVERLAP
# THIS ISN'T POSSIBLE BECAUSE I'VE REMOVED ALL THE SPECIFIC OVERLAPS
richness <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Richness_2020/areakm2), BDvariableTitle = "Species richness 2020 (density)", fill = overlapsWith2)
richness 
ende <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Endemism_2020/areakm2), BDvariableTitle = "Endemism 2020 (density)" , fill = overlapsWith2)
# ende <- ende + coord_cartesian(ylim = c(0,1))
phyl <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Phylodiversity_2020/areakm2), BDvariableTitle = "Phylodiversity 2020 (density)", fill = overlapsWith2)
phyl 
# to plot all together
currentBDviolin <- plot_grid(richness, 
                       ende, 
                       phyl, 
                       nrow = 3, labels = c("A", "B", "C"))
currentBDviolin
# # save plot
# setwd(paste0(wdmain, "/output"))
# png("CurrentBD_perTenureCategOverlaps_violinDensity_202410.png", width = 3300, height = 4000, units = "px", res = 300)
# currentBDviolin
# dev.off()

# flipped version WITHOUT overlaps side by side

data$LTcateg3 <- factor(data$LTcateg2, levels = rev(levels(data$LTcateg2)))
data$LTcateg3 <- droplevels(data$LTcateg3)
sample_sizes <- data %>%
  group_by(LTcateg3) %>%
  summarize(n = n())

myboxplots <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL, fill, sample_sizes) {
  
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = {{fill}})) +
    geom_violin(position = position_dodge(width = 0.9), alpha = 0.2) +
    geom_boxplot(width=0.3, color="grey20", position = position_dodge(width = 0.9), alpha = 0.8) +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
          legend.title = element_blank(), 
          legend.position = "none", 
          axis.title.y = element_blank(),
          text = element_text(size = 15)) +
    labs(y = BDvariableTitle) +
    expand_limits(y = max(data$Richness_2020 / data$areakm2)) + 
    coord_flip() +
    geom_text(data = sample_sizes, aes(x = {{tenureCategory}}, y = Inf, label = paste("n =", n)), hjust = 10,
              vjust = -2, size = 3.5, inherit.aes = F)
    return(plot)
}

currentRichness <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Richness_2020/areakm2, BDvariableTitle = "Species richness 2020 (per"~km^2~")", fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Endemism_2020/areakm2, BDvariableTitle = "Endemism 2020 (per"~km^2~")",fill = LTcateg3, sample_sizes = sample_sizes)
plot_grid(currentRichness, currentEndemism, nrow = 2)

# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentBD_perTenureCategNoOverlaps_flippedboxplot_20241030.png", width = 2800, height = 3000, units = "px", res = 300)
plot_grid(currentRichness, currentEndemism, nrow = 2)
dev.off()

# biodiversity losses across different categories ----

richness <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = (Richness_loss/areakm2), BDvariableTitle = "Decrease in richness (per"~km^2~")", fill = LTcateg3, sample_sizes = sample_sizes)
# richness <- richness + coord_cartesian(ylim = c(0, 100))
ende <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = (Endemism_loss/areakm2), BDvariableTitle = "Decrease in endemism (per"~km^2~")", fill = LTcateg3, sample_sizes = sample_sizes)
# ende <- ende + coord_cartesian(ylim = c(0,50))
# to plot all together
lossBD <- plot_grid(richness, ende, nrow = 2)
lossBD

setwd(paste0(wdmain, "/output"))
png("BDLossProportion_perTenureCategNoOVerlaps_flippedboxplot_20241030.png", width = 2800, height = 3000,   units = "px", res = 300)
lossBD
dev.off()

# deforestation ----

# first i should make sense of the deforestation data
# if each count == 30m2, then
data$defor2 <- data$defor*0.0009
# are there areas deforested larger than the property itself?
deforChecking <- data[which(data$defor2 > data$areakm2), c(1:4, 14,15,23)]
summary(deforChecking$areakm2 - deforChecking$defor2) # ok the largest this error happens for is 27 km2
data$defor3 <- data$defor2
# cap it at the area of the parcel
data$defor3[which(data$defor2 > data$areakm2)] <- data$areakm2[which(data$defor2 > data$areakm2)]

# check current forest
data$for23 <- data$for23*0.0009
forChecking <- data[which(data$for23 > data$areakm2), c(1:4, 14,15,23)]
summary(forChecking$areakm2 - forChecking$for23) # this is off by over 700km2 sometimes!
data$for23_2 <- data$for23
# cap it at the area of the parcel
data$for23_2[which(data$for23 > data$areakm2)] <- data$areakm2[which(data$for23 > data$areakm2)]

# plot deforestation 1985-2023
deforestationPlot <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = defor3/areakm2, BDvariableTitle = "Deforestation 1985-2023 (per"~km^2~")", fill = LTcateg3, sample_sizes = sample_sizes)
deforestationPlot # is this a percent per property??

# plot current forest cover
currentForest <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = (for23_2/areakm2), BDvariableTitle = "Forest cover 2023 (per"~km^2~")", fill = LTcateg3, sample_sizes = sample_sizes)
currentForest


# repeat boxplots - but distinguish across biomes ----

violinplotBD <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL, fill) {
  
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = {{fill}})) +
    geom_violin(position = position_dodge(width = 0.9), alpha = 0.5) +
    geom_boxplot(width=0.2, color="grey20", position = position_dodge(width = 0.9), alpha = 0.6) +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    labs(y = BDvariableTitle) +
    theme(panel.background = element_blank(), panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
          legend.title = element_blank(), legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank()) +
    facet_wrap(vars(LTcateg2), nrow = 1, scales = "free_x") +
    geom_text(data = sample_sizes, aes(x = LTcateg2, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)
  
  return(plot)
}

unique(data$name_biome)
data$biome <- factor(data$name_biome, levels = c("Amazonia",
                                                  "Cerrado",
                                                 "Caatinga",
                                                 "Mata Atlantica",
                                                 "Pampa",
                                                 "Pantanal"))
summary(data)
head(data)

ggplot(data, aes(x = LTcateg2, y = Richness_2020, fill = LTcateg2)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_boxplot(width=0.2, color="grey20", position = position_dodge(width = 0.9), alpha = 0.6) +
  scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
  labs(y = "biome richness test") +
  theme(panel.background = element_blank(), panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
        legend.title = element_blank(), legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank()) +
  facet_wrap(as.factor(data$name_biome), nrow = 6, scales = "free_x") #+
  # geom_text(data = sample_sizes, aes(x = LTcateg2, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)


# comparison against deforestation ----

test <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = log(defor/areakm2), BDvariableTitle = "(log) Deforestation per km2", fill = LTcateg)
test
setwd(paste0(wdmain, "/output"))
png("testDeforestation_20241028.png", width = 4500, height = 1500,  units = "px", res = 300)
test
dev.off()

# remaining forest in 2023 
test <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (for23/areakm2), BDvariableTitle = "Remaining forest in 2023 per km2", fill = LTcateg)
test
setwd(paste0(wdmain, "/output"))
png("testForest23_20241028.png", width = 4500, height = 1500,  units = "px", res = 300)
test
dev.off()


# biodiversity and FC compliance ----
# combine this cleaned bd+tenure data with forest deficit information 
# will actually be visualized in following script

# get orig data 
setwd(paste0(wdmain, "data/raw/landTenure/LandTenure_v20231009"))
l <- list.files()
shps <- grep(".shp", l)
s <- list()
for(i in 1:length(l[shps]))
{
  a <- st_read(l[shps][i])
  s[[i]] <- st_drop_geometry(a)
}
lapply(s, colnames)
s <- do.call(rbind, s)

# replicate process as when i created the id's
original_iru <- s[which(s$tipo == "IRU"),]
original_iru$id <- paste0("IRU-", 1:nrow(original_iru))
original_ast <- s[which(s$tipo == "AST"),]
original_ast$id <- paste0("AST-", 1:nrow(original_ast))
# join both datasets with FC info
original_csr <- rbind(original_iru, original_ast)
colnames(original_csr)

original_csr <- select(original_csr, c("uf", "n_mf", "area_conv", "area_veg", "rl_ativo", "rl_def", "app_def", "desmat_p08", "id"))

# join data with tenure+bd data
data_extra <- left_join(data, original_csr, by = "id")
head(data_extra)
summary(data_extra)

# write out this information
setwd(paste0(wdmain, "data/processed/"))
write.csv(data_extra, "finalDataset_Tenure-BD-CSR.csv", row.names = F)


