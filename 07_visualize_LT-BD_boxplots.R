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

# read pre-cleaned data
setwd(paste0(wdmain, "/output/"))
data <- read.csv("finalTenure&BD&ForestDatasetforPlotting.csv")
head(data)

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



# current biodiversity under different tenure categories ----
# including all observations
data$LTcateg2 <- data$LTcateg2 <- factor(data$LTcateg, levels = c("Private lands",
                                                                  "Undesignated lands",
                                                                  "Rural settlements",
                                                                  "PA strict protection",
                                                                  "PA sustainable use",
                                                                  "Indigenous" ,
                                                                  "Private PA",
                                                                  "Quilombola lands"))
data$LTcateg3 <- factor(data$LTcateg2, levels = rev(levels(data$LTcateg2)))
data$LTcateg3 <- droplevels(data$LTcateg3)
# create variable with amount of observations per category to include as a label in my plots
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
    # expand_limits(y = max(data$Richness_2020 * data$areakm2)) + 
    # expand_limits(y = mean(data$Richness_2020 * data$areakm2)) + 
    coord_flip() +
    geom_text(data = sample_sizes, aes(x = {{tenureCategory}}, y = Inf, label = paste("n =", n)), hjust = 6,
              vjust = -2, size = 2.5, inherit.aes = F) +
    geom_text(data = medianBD, aes(x = {{tenureCategory}}, y = medianBD + (0.02 * max(medianBD)), label = paste(medianBD)), 
              hjust = 0.6, vjust = 0, size = 2.5, inherit.aes = FALSE)
  return(plot)
}
# average richness of all properties without the artifice of dividing by area
medianBD <- data %>%
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))
currentRichness2 <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Richness_2020, 
                              BDvariableTitle = "Species richness 2020", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
medianBD <- data %>%
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))
currentEndemism2 <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Endemism_2020, 
                              BDvariableTitle = "Endemism 2020",
                              fill = LTcateg3, sample_sizes = sample_sizes)
plot_grid(currentRichness2, currentEndemism2, nrow = 2)

# if i divide by area, then i have the artifice effect: it appears that private lands have more BD than others bc they're smaller properties
# currentRichness <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = (Richness_2020/areakm2), 
#                               BDvariableTitle = "Species richness 2020 (per"~km^2~")", 
#                               fill = LTcateg3, sample_sizes = sample_sizes)
# currentEndemism <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Endemism_2020/areakm2, 
#                               BDvariableTitle = "Endemism 2020 (per"~km^2~")",
#                               fill = LTcateg3, sample_sizes = sample_sizes)
# plot_grid(currentRichness, currentEndemism, nrow = 2)

# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentRichness_20250224.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness2
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_20250224.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism2
dev.off()

# setwd(paste0(wdmain, "/output"))
# png("comparisonBD_mean-and-perArea.png", width = 2450, height = 2000, units = "px", res = 300)
# plot_grid(currentRichness, currentEndemism, currentRichness2, currentEndemism2, nrow = 2)
# dev.off()

# version accounting for the overlaps  ----
# this is essentially to see whether there is a difference in the private lands that overlap with conservation areas
# will need to do some data transformation
data <- data %>%
  mutate(ovl_exists = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) > 0)
head(as.data.frame(data))
data2 <- data
# create new column for overlaps
data2$ovlWith <- data2$LTcateg
# overwrite this column when there are overlaps
unique(data2[which(data2$LTcateg == "Private lands" & data2$ovl_exists == TRUE),]$LTcateg)
data2[which(data2$LTcateg == "Private lands" & data2$ovl_exists == TRUE),]$ovlWith <- "Private-land overlaps" # just to flag all overlaps
data2$ovlWith <- factor(data2$ovlWith, levels = c("Private lands",
                                          "Private-land overlaps",
                                          "Undesignated lands",
                                          "Rural settlements",
                                          "PA strict protection",
                                          "PA sustainable use",
                                          "Indigenous" ,
                                          "Private PA",
                                          "Quilombola lands"))
summary(data2)

tenureColors2 <- c("Indigenous" = "#E78AC3",
                  # "non-overlapped" = "gray70",   
                  "PA strict protection" = "#1B9E77",       
                  "PA sustainable use" =  "#8C7E5B",
                  "Private-land overlaps" = "#CCF66F",  
                  "Quilombola lands" =  "#FFD700",
                  "Private lands" = "#8DA0CB",
                  "Rural settlements" = "#FC8D62",
                  "Undesignated lands" ="#1d6c7d")

data2$ovlWith2 <- factor(data2$ovlWith, levels = rev(levels(data2$ovlWith)))
# label the n of the overlaps
sample_sizes2 <- data2 %>%
  group_by(ovlWith2) %>%
  summarize(n = n())
medianBD <- data2 %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))

currentRichness3 <- myboxplots(data2, tenureCategory = ovlWith2, BDvariable = Richness_2020, 
                              BDvariableTitle = "Species richness 2020", 
                              fill = ovlWith2, sample_sizes = sample_sizes2) +
  scale_colour_manual(values = tenureColors2, aesthetics = c("color", "fill"))
currentRichness3

medianBD <- data2 %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))
currentEndemism3 <- myboxplots(data2, tenureCategory = ovlWith2, BDvariable = Endemism_2020, 
                              BDvariableTitle = "Species richness 2020", 
                              fill = ovlWith2, sample_sizes = sample_sizes2) +
  scale_colour_manual(values = tenureColors2, aesthetics = c("color", "fill"))
currentEndemism3

# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentRichness_overlaps.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness3
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_overlaps.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism3
dev.off()


# deforestation ----

# # plot current forest cover
# currentForest <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = p_for23, 
#                             BDvariableTitle = "% forest cover 2023 (per property)", 
#                             fill = LTcateg3, sample_sizes = sample_sizes)
# currentForest
# 
# setwd(paste0(wdmain, "/output"))
# png("Forest-2023_flippedboxplot_20241202.png", width = 2450, height = 970,   units = "px", res = 300)
# currentForest
# dev.off()


# Are results different for Brazil's different biomes? ----
# repeat boxplots - but distinguish across biomes 
colnames(data)
unique(data$biome)
data$biome2 <- factor(data$biome)
data$biome2
summary(data)

biomeData <- data %>% filter(!is.na(biome2))

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

sample_sizes <- biomeData %>%
  group_by(LTcateg3, biome2) %>%
  summarize(n = n(), .groups = 'drop')

currentRichness <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable = Richness_2020, 
                              BDvariableTitle = "Species richness 2020 (per"~km^2~")", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentRichness + facet_wrap(~biome2, nrow = 2, scales = "fixed") 

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_perBiomes.png", width = 3300, height = 3000, units = "px", res = 300)
currentRichness + facet_wrap(~biome2, nrow = 2, scales = "fixed") 
dev.off()

currentEndemism <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable =  Endemism_2020, 
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


