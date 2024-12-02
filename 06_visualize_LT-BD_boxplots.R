#### Biodiversity and Tenure in Brazil ####

# script that visualizes biodiversity indicators per tenure categ that have previously been extracted 

# two main outcomes:
# 1) boxplots of all BD variables (with the option of seeing the categories that are overlapping)
# 2) bd+tenure dataset joined with original information from CSR on the forest code deficit variables


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
lapply(tables, length)

data <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
# lapply(data, colnames)
data1 <- do.call(rbind, data)
colnames(data1) <- gsub("mean.", "", colnames(data1))


# Deal with duplicated id's  ----
# duplicates stem from splitting up properties across the overlaps, which is a problem bc my boxplots show biodiversity/property area
# at the same time, i want to keep the info on whether a property has overlaps
# but if i don't summarize per id, this biases the boxplots (particularly evident for quilombola lands) 
nrow(data1)
length(unique(data1$id))

# so here i need to summarize the data by id, 
# BUT i first sort by area so that i keep whichever observation had a larger area, meaning i note whether a property had significant overlaps
data2 <- data1 %>%
  group_by(id) %>%
  slice_max(order_by = areakm2, n = 1, with_ties = FALSE) %>% # prioritizes by the area/size of the polygon
  summarize(
    LTcateg = first(LTcateg),
    myOverCat = first(myOverCat), # this just takes the first overlap category
    areakm2 = sum(areakm2, na.rm = T),
    biome = first(biome),
    Endemism_2020 = mean(Endemism_2020, na.rm = T),
    Endemism_baseline = mean(Endemism_baseline, na.rm = T),
    Endemism_loss = mean(Endemism_loss, na.rm = T),
    Phylodiversity_2020 = mean(Phylodiversity_2020, na.rm = T),
    Phylodiversity_baseline = mean(Phylodiversity_baseline, na.rm = T),
    Phylodiversity_loss = mean(Phylodiversity_loss, na.rm = T),
    Richness_2020 = mean(Richness_2020, na.rm = T),
    Richness_baseline = mean(Richness_baseline, na.rm = T),
    Richness_loss = mean(Richness_loss, na.rm = T),
    ) %>%
  ungroup()
head(data2)
length(unique(data2$id))
data2 %>%
  group_by(LTcateg) %>%
  summarize(n = n())

# read data on deforestation extractions ----

setwd(paste0(wdmain, "/data/processed/forestExtractions-perPolygon"))
l <- list.files()
l[grep("1130", l)]
tables <- lapply(l[grep("1130", l)], read.csv)
lapply(tables, colnames)
names(tables) <- gsub(".csv","", l[grep("1130", l)])

myColumns <- c("LTcateg", "id", "areakm2", "for23", "defor", "total")
fData <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # select my columns
  df <- df[,myColumns]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
fData <- do.call(rbind, fData)
length(unique(fData$id))
# similarly, i have to summarize by id, but i can sum the forest/deforest/total values (bc these are counts) 
forestData <- fData %>%
  group_by(id) %>%
  summarize(LTcateg = first(LTcateg),
            areakm2 = sum(areakm2),
            for23 = sum(for23, na.rm = T),
            defor = sum(defor, na.rm = T),
            total = sum(total, na.rm = T))
nrow(forestData)
head(forestData)
length(unique(forestData$id))
# get proportions
forestData$p_for23 <- (forestData$for23/forestData$total)*100
forestData$p_defor <- (forestData$defor/forestData$total)*100

forestData %>%
  group_by(LTcateg) %>%
  summarize(n = n(),
            perForest = mean(p_for23),
            perDeforest = mean(p_defor))
forestData <- forestData %>%
  select(id, LTcateg, p_for23, p_defor)

# join forest data with biodiversity data
forest_BD_data <- inner_join(data2, forestData, by = c("LTcateg", "id"))
head(forest_BD_data)
length(unique(forest_BD_data$id))

# other data cleaning and organizing ----

data <- forest_BD_data

# filter out these undesignated categories which are a bit ambiguous 
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]
# check how many observations per category i have in the original data
data %>%
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
head(as.data.frame(data))
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
    facet_wrap(vars({{tenureCategory}}), nrow = 1, scales = "free_x") +
    geom_text(data = sample_sizes, aes(x = {{tenureCategory}}, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)
  
  return(plot)
}


# current biodiversity under different tenure categories ----

# Version WITH overlaps
richness <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Richness_2020/areakm2), BDvariableTitle = "Species richness 2020 (density)", fill = overlapsWith2)
richness 
ende <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Endemism_2020/areakm2), BDvariableTitle = "Endemism 2020 (density)" , fill = overlapsWith2)
phyl <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Phylodiversity_2020/areakm2), BDvariableTitle = "Phylodiversity 2020 (density)", fill = overlapsWith2)
phyl 
# to plot all together
currentBDviolin <- plot_grid(richness, 
                       ende, 
                       phyl, 
                       nrow = 3, labels = c("A", "B", "C"))
currentBDviolin
# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentBD_perTenureCategOverlaps_violinDensity_202410.png", width = 3300, height = 4000, units = "px", res = 300)
currentBDviolin
dev.off()

# flipped version WITHOUT overlaps side by side
data$LTcateg3 <- factor(data$LTcateg2, levels = rev(levels(data$LTcateg2)))
data$LTcateg3 <- droplevels(data$LTcateg3)

sample_sizes <- data %>%
  group_by(LTcateg3) %>%
  summarize(n = n())

myboxplots <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL, fill, sample_sizes) {
  
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = {{fill}})) +
    geom_violin(alpha = 0.2) +
    geom_boxplot(width=0.5, color="grey20", alpha = 0.7) +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
          legend.title = element_blank(), 
          legend.position = "none", 
          axis.title.y = element_blank(),
          text = element_text(size = 10)) +
    labs(y = BDvariableTitle) +
    expand_limits(y = max(data$Richness_2020 / data$areakm2)) + 
    coord_flip() +
    geom_text(data = sample_sizes, aes(x = {{tenureCategory}}, y = Inf, label = paste("n =", n)), hjust = 6,
              vjust = -2, size = 2.5, inherit.aes = F)
    return(plot)
}

currentRichness <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Richness_2020/areakm2, 
                              BDvariableTitle = "Species richness 2020 (per"~km^2~")", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Endemism_2020/areakm2, 
                              BDvariableTitle = "Endemism 2020 (per"~km^2~")",
                              fill = LTcateg3, sample_sizes = sample_sizes)
plot_grid(currentRichness, currentEndemism, nrow = 2)

# save plot

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_20241126.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_20241126.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism
dev.off()

data %>%
  group_by(LTcateg) %>%
  summarise(mArea = mean(areakm2),,
            minArea = min(areakm2),
            maxArea = max(areakm2))

# deforestation ----

# plot current forest cover
currentForest <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = p_for23, 
                            BDvariableTitle = "% forest cover 2023 (per property)", 
                            fill = LTcateg3, sample_sizes = sample_sizes)
currentForest

setwd(paste0(wdmain, "/output"))
png("Forest-2023_flippedboxplot_20241202.png", width = 2450, height = 970,   units = "px", res = 300)
currentForest
dev.off()


# Are results different for Brazil's different biomes? ----
# repeat boxplots - but distinguish across biomes 
colnames(data)
unique(data$biome)
data$biome2 <- factor(data$biome)
data$biome2
summary(data)

biomeData <- data %>% filter(!is.na(biome2))

sample_sizes <- biomeData %>%
  group_by(LTcateg3, biome2) %>%
  summarize(n = n(), .groups = 'drop')

myBiome_boxplots <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL, fill, sample_sizes) {
  
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = {{fill}})) +
    geom_violin(alpha = 0.2) +
    geom_boxplot(width=0.5, color="grey20", alpha = 0.7) +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
          legend.title = element_blank(), 
          legend.position = "none", 
          axis.title.y = element_blank(),
          text = element_text(size = 14)) +
    labs(y = BDvariableTitle) +
    expand_limits(y = max(data$Richness_2020 / data$areakm2)) + 
    coord_flip() +
    geom_text(data = sample_sizes, aes(x = {{tenureCategory}}, y = Inf, label = paste("n =", n)), hjust = 2,
              vjust = -2, size = 3.5, inherit.aes = F)
  return(plot)
}


currentRichness <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable = Richness_2020/areakm2, 
                              BDvariableTitle = "Species richness 2020 (per"~km^2~")", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentRichness + facet_wrap(~biome2, nrow = 2, scales = "fixed") 

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_perBiomes.png", width = 3300, height = 3000, units = "px", res = 300)
currentRichness + facet_wrap(~biome2, nrow = 2, scales = "fixed") 
dev.off()

currentEndemism <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable =  Endemism_2020/areakm2, 
                              BDvariableTitle = "Endemism 2020 (per"~km^2~")", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism + facet_wrap(~biome2, nrow = 2, scales = "fixed")

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_perBiomes.png", width = 3300, height = 3000, units = "px", res = 300)
currentEndemism + facet_wrap(~biome2, nrow = 2, scales = "fixed")
dev.off()

# disaggregated plot show that the pattern holds throughout biomes
# i.e., amazonia does not dominate - whereas this is likely different for forest 2023
# is this the same for Forest 2023?
currentForest <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable = p_for23, 
                            BDvariableTitle = "% forest cover 2023 (per property)", 
                            fill = LTcateg3, sample_sizes = sample_sizes)
currentForest + facet_wrap(~biome2, nrow = 2, scales = "fixed")

setwd(paste0(wdmain, "/output"))
png("CurrentForest_perBiomes.png", width = 3300, height = 3000, units = "px", res = 300)
currentForest + facet_wrap(~biome2, nrow = 2, scales = "fixed")
dev.off()



# biodiversity and FC compliance ----
# combine this cleaned bd+tenure data with forest deficit information 
# will actually be visualized in following script

# get orig data for getting the geometries
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

test <- data_extra %>%
  group_by(LTcateg) %>%
  filter(is.na(n_mf))
unique(test$LTcateg) # of course, i lose observations which are not IRU or AST

# write out this information
setwd(paste0(wdmain, "data/processed/"))
write.csv(data_extra, "finalDataset_Tenure-BD-CSR.csv", row.names = F)


