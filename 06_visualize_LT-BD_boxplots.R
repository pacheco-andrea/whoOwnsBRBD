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

# 08.01.2025: so here, instead of summarizing with dplyr, FIRST i need to account for the areas of overlap  ---
# filter out these undesignated categories 
data <- data1
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]
# rename categories:
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
# in order to clean/organize the overlaps 
# split data into lists of each tenure category
head(data)
# create columns for the overlaps i'm concerned about controlling for
data$ovl_PA_strict <- NA
data$ovl_PA_sustuse <- NA
data$ovl_undesignated <- NA
data$ovl_ruralSett <- NA

data_list <- split(data, data$LTcateg2)
names(data_list)
for(i in 1:length(data_list))
{
  # the the unique overlaps for this category
  unique(data_list[[i]]$myOverCat)
  # get the name of the category
  name <- names(data_list[i]) 
  # record areas of overlaps for private lands and rural settlements
  if(name == "Private lands" | name == "Rural settlements"){
    # how much area overlaps with strict PAs?
    pastr_ovlp <- grep("PA_strict", data_list[[i]]$myOverCat)
    data_list[[i]][pastr_ovlp,]$ovl_PA_strict <- data_list[[i]][pastr_ovlp,]$areakm2
    # how much area overlaps with sustainable use PAs?
    pasus_ovlp <- grep("PA_sustuse", data_list[[i]]$myOverCat)
    data_list[[i]][pasus_ovlp,]$ovl_PA_sustuse <- data_list[[i]][pasus_ovlp,]$areakm2
    # how much area overlaps with undesignated?
    undes_ovlp <- grep("undesignated", data_list[[i]]$myOverCat)
    data_list[[i]][undes_ovlp,]$ovl_undesignated <- data_list[[i]][undes_ovlp,]$areakm2
    # note, no private lands at this point overlap with indigenous lands, which might be bc the properties were <1km, 
    # and the BD was not extracted
  }
  # record areas of overlaps for PA sustainable use 
  if(name == "PA sustainable use"){
    # how much area overlaps with rural settlements?
    rsett_ovlp <- grep("sustUsePAs-ruralSettlements", data_list[[i]]$myOverCat)
    data_list[[i]][rsett_ovlp,]$ovl_ruralSett <- data_list[[i]][rsett_ovlp,]$areakm2
  }
  # IMPORTANT NOTE HERE:
  # i did in fact cross-check whether other PA categs or indigenous lands had overlaps we could account for
  # but these were limited to self overlaps, or just the overlaps bt PAs-Indigenous which i don't believe we need to account for
  # hence, these are the only columns that need to be recorded
}
data2 <- do.call(rbind, data_list)
head(data2)
summary(data2)

# now, i can:
# summarize the data by id ----
# without losing the information on areas of overlap
# check how many observations per category i have in the original data

unique(data2$LTcateg)
unique(data2$myOverCat)
colnames(data2)
# summarize the data by id in order to have one row per property
data3 <- data2 %>%
  group_by(id) %>%
  summarize(
    LTcateg = first(LTcateg),
    LTcateg2 = first(LTcateg2),
    # myOverCat = first(myOverCat), # do not need this column anymore 
    areakm2 = sum(areakm2, na.rm = T), # the total area of each individual property across polygons
    biome = first(biome), # whichever biome is first, not a huge issue
    # the average biodiversity across polygons:
    Endemism_2020 = mean(Endemism_2020, na.rm = T),
    Endemism_baseline = mean(Endemism_baseline, na.rm = T),
    Endemism_loss = mean(Endemism_loss, na.rm = T),
    Phylodiversity_2020 = mean(Phylodiversity_2020, na.rm = T),
    Phylodiversity_baseline = mean(Phylodiversity_baseline, na.rm = T),
    Phylodiversity_loss = mean(Phylodiversity_loss, na.rm = T),
    Richness_2020 = mean(Richness_2020, na.rm = T),
    Richness_baseline = mean(Richness_baseline, na.rm = T),
    Richness_loss = mean(Richness_loss, na.rm = T),
    # the sum of the overlapping areas
    ovl_PA_strict = sum(ovl_PA_strict, na.rm = T),
    ovl_PA_sustuse = sum(ovl_PA_sustuse, na.rm = T),
    ovl_undesignated = sum(ovl_undesignated, na.rm = T),
    ovl_ruralSett = sum(ovl_ruralSett, na.rm = T),
    ) %>%
  ungroup()
# examine:
head(data3)
nrow(data3)
length(unique(data3$id))
data3 %>%
  group_by(LTcateg2) %>%
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
# similarly, summarize by id, but i can sum the forest/deforest/total values (bc these are counts) over id's
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
  select(id, p_for23, p_defor)

# join forest data with biodiversity data
forest_BD_data <- inner_join(data3, forestData, by = c("id"))
head(as.data.frame(forest_BD_data))
length(unique(forest_BD_data$id))

# other data cleaning and organizing ----

data <- forest_BD_data
# create variables for the proportion of loss
data$rLoss_prop <- data$Richness_loss/(data$Richness_baseline - data$Richness_loss)
data$eLoss_prop <- data$Endemism_loss/(data$Endemism_baseline - data$Endemism_loss)
data$pLoss_prop <- data$Phylodiversity_loss/(data$Phylodiversity_baseline - data$Phylodiversity_loss)

# clean names for plotting -----

# actually remove private PAs from plotting
data <- data[which(data$LTcateg2 != "Private PA"),]
head(as.data.frame(data))
# write out updated final dataset
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
sample_sizes

# current biodiversity under different tenure categories ----
# flipped boxplot WITHOUT overlaps side by side
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
  summarise(mArea = mean(areakm2),
            minArea = min(areakm2),
            maxArea = max(areakm2))

# make version that filters out those properties with overlaps ----
# will need to do some data transformation
data_novl <- data %>%
  mutate(
    no_overlap = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) == 0
  )
head(as.data.frame(data_novl))
# Pivot the overlap columns into a long format
data_long <- data_novl %>%
  pivot_longer(
    cols = starts_with("ovl_"),
    names_to = "overlap",
    values_to = "value"
  ) %>%
  mutate(overlap_present = value > 0)
head(as.data.frame(data_long))
# Separate data for plotting
no_overlap_data <- data_novl %>% filter(no_overlap)
overlap_data <- data_long %>% filter(overlap_present)

my_overlapsBarplots <- function(no_overlap_data, overlap_data, fill_color = "blue") {
  # Create the barplot
  ggplot() +
    # Bar for no-overlap observations
    geom_bar(data = no_overlap_data,
             aes(x = "No Overlap", fill = "No Overlap"),
             position = "dodge", stat = "count") +
    # Bars for overlap observations
    geom_bar(data = overlap_data,
             aes(x = overlap, fill = overlap),
             position = "dodge", stat = "count") +
    scale_fill_manual(values = c("No Overlap" = "grey", "ovl_a" = "red", "ovl_b" = "green", "ovl_c" = "blue")) +
    labs(
      title = "Side-by-Side Barplot of Overlaps",
      x = "Category",
      y = "Count",
      fill = "Overlap Type"
    ) +
    theme_minimal()
}
my_overlapsBarplots(no_overlap_data, overlap_data)



currentRichness <- myboxplots(data_novl, tenureCategory = LTcateg3, BDvariable = Richness_2020/areakm2, 
                              BDvariableTitle = "Species richness 2020 (per"~km^2~")", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism <- myboxplots(data_novl, tenureCategory = LTcateg3, BDvariable = Endemism_2020/areakm2, 
                              BDvariableTitle = "Endemism 2020 (per"~km^2~")",
                              fill = LTcateg3, sample_sizes = sample_sizes)
plot_grid(currentRichness, currentEndemism, nrow = 2)

# save plot

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_noOverlaps_20250109.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_noOverlaps_20250109.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism
dev.off()



# Version WITH overlaps ----

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


