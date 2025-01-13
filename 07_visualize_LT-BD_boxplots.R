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
# flipped boxplot WITHOUT overlaps side by side
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
head(data)

data <- data %>%
  mutate(ovl_exists = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) > 0)
head(as.data.frame(data))
class(data$ovl_exists)

data2 <- data
# create new column for overlaps
data2$ovlWith <- data2$LTcateg3
# overwrite this column when there are overlaps
data2[which(data2$ovl_exists == TRUE),]$ovlWith <- "PA strict protection"

ggplot(data2, aes(x = LTcateg2, y = Richness_2020/areakm2, fill = ovlWith)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.2) +
  geom_boxplot(width=0.2, color="grey20", position = position_dodge(width = 0.9), alpha = 0.8) +
  scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
  labs(y = "title") +
  theme(panel.background = element_blank(), panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
        legend.title = element_blank(), legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank()) +
  facet_wrap(vars(LTcateg2), nrow = 1, scales = "free_x") +
  geom_text(data = sample_sizes, aes(x = LTcateg3, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)


# Pivot the overlapdata# Pivot the overlap columns into a long format ----
# data_long <- data_novl %>%
#   pivot_longer(
#     cols = starts_with("ovl_"),
#     names_to = "overlap",
#     values_to = "value"
#   ) %>%
#   mutate(overlap_present = value > 0)
# head(as.data.frame(data_long))
# # Separate data for plotting
# no_overlap_data <- data_novl %>% filter(no_overlap)
# overlap_data <- data_long %>% filter(overlap_present)
# 
# my_overlapsBarplots <- function(no_overlap_data, overlap_data, fill_color = "blue") {
#   # Create the barplot
#   ggplot() +
#     # Bar for no-overlap observations
#     geom_bar(data = no_overlap_data,
#              aes(x = "No Overlap", fill = "No Overlap"),
#              position = "dodge", stat = "count") +
#     # Bars for overlap observations
#     geom_bar(data = overlap_data,
#              aes(x = overlap, fill = overlap),
#              position = "dodge", stat = "count") +
#     scale_fill_manual(values = c("No Overlap" = "grey", "ovl_a" = "red", "ovl_b" = "green", "ovl_c" = "blue")) +
#     labs(
#       title = "Side-by-Side Barplot of Overlaps",
#       x = "Category",
#       y = "Count",
#       fill = "Overlap Type"
#     ) +
#     theme_minimal()
# }
# my_overlapsBarplots(no_overlap_data, overlap_data)



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


