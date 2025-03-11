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
library(effsize)

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
    geom_text(data = medianBD, aes(x = {{tenureCategory}}, y = medianBD + (0.03 * max(medianBD)), label = paste(medianBD)), 
              hjust = 0.6, vjust = 0, size = 2.5, inherit.aes = FALSE)
  return(plot)
}
# average richness of all properties without the artifice of dividing by area
sample_sizes <- data %>%
  group_by(LTcateg3) %>%
  summarize(n = n())
medianBD <- data %>%
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))
currentRichness2 <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Richness_2020, 
                              BDvariableTitle = "Species richness 2020", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentRichness2 

medianBD <- data %>%
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))
currentEndemism2 <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = Endemism_2020, 
                              BDvariableTitle = "Endemism 2020",
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism2

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
png("CurrentRichness_20250226.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness2
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_20250226.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism2 
dev.off()

# summarize standardized differences between LT categories
category_list <- unique(data$LTcateg)
s <- list()
for(i in 1:length(category_list))
{
  r <- cohen.d(data$Richness_2020[data$LTcateg3 == paste0(category_list[i])], data$Richness_2020[data$LTcateg3 != paste0(category_list[i])], na.rm = T, 
                     conf.level = 0.99)
  e <- cohen.d(data$Endemism_2020[data$LTcateg3 == paste0(category_list[i])], data$Endemism_2020[data$LTcateg3 != paste0(category_list[i])], na.rm = T, 
                     conf.level = 0.99)
  s[[i]] <- data.frame("LTcateg" = paste0(category_list[i]),
                                      "cohen_d_R" = r$estimate, 
                                      "sig_R" = r$magnitude,
                                      "cohen_d_E" = e$estimate, 
                                      "sig_E" = e$magnitude)
}
s <- do.call(rbind, s)
s
setwd(paste0(wdmain, "/output"))
write.csv(s, "cohensD_withOverlaps.csv", row.names = F)

# double check diffs in groups with anova
anova_rich <- aov(Richness_2020 ~ LTcateg3, data = data)
summary(anova_rich)
tukey_rich <- TukeyHSD(anova_rich)
anova_ende <- aov(Endemism_2020 ~ LTcateg, data = data)
summary(anova_ende)
tukey_ende <- TukeyHSD(anova_ende)
tukey_rich
tukey_ende


# setwd(paste0(wdmain, "/output"))
# png("comparisonBD_mean-and-perArea.png", width = 2450, height = 2000, units = "px", res = 300)
# plot_grid(currentRichness, currentEndemism, currentRichness2, currentEndemism2, nrow = 2)
# dev.off()

# version accounting for the overlaps  ----
# this is essentially to see whether there is a difference in the private lands that overlap with conservation areas
# create a column that flags all the overlaps
data2 <- data %>%
  mutate(ovlExists = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) > 0)
head(as.data.frame(data2))
# data2 <- data2[which(data2$ovlExists == "TRUE"),]
# summary(data2)
# i looked into the percentage of overlapping area, and as in the overlapping analysis, the majority of overlaps are in undesignated lands
# but ultimately, since i dont think i can completely rely on these exact numbers, i dont incorporate this into calculations
# instead, i just want to verify the amount of BD in properties with overlaps in PAs/undesignated lands

# create another column to summarize which categories overlap
data2$ovlWith <- NA 
# one by one, overwrite this "overlaps with" column to flag which other category it overlaps with 
data2$ovlWith[which(data2$ovl_PA_strict > 0 & data2$ovl_PA_sustuse == 0 & data2$ovl_undesignated == 0)] <- "PA strict protection"
data2$ovlWith[which(data2$ovl_PA_sustuse > 0 & data2$ovl_PA_strict == 0 & data2$ovl_undesignated == 0)] <- "PA sustainable use"
data2$ovlWith[which(data2$ovl_undesignated > 0 & data2$ovl_PA_strict == 0 & data2$ovl_PA_sustuse == 0)] <- "Undesignated lands"
# also flag cases where there are multiple overlaps
data2$ovlWith[which(data2$ovl_undesignated == 0 & (data2$ovl_PA_strict > 0 & data2$ovl_PA_sustuse > 0))] <- "Both PAs"
data2$ovlWith[which(data2$ovl_undesignated > 0 & 
                      (data2$ovl_PA_strict > 0 | data2$ovl_PA_sustuse > 0))] <- "PAs & Undesignated lands"
# i have to switch the tenure and rural sett categories here
data2$LTcateg3[which(data2$ovl_ruralSett > 0)] <- "Rural settlements"
data2$ovlWith[which(data2$ovl_ruralSett > 0)] <- "PA sustainable use"
# fill in the NAs to indicate no overlaps
data2$ovlWith[is.na(data2$ovlWith)] <- paste0(data2$LTcateg3[is.na(data2$ovlWith)], " (no ovlp)")
summary(data2)
unique(data2$ovlWith)

data2 <- data2 %>%
  filter(LTcateg3 == "Private lands" | LTcateg3 == "Rural settlements") # i'm only plotting these two
summary(data2)
unique(data2$ovlWith)
# make into factor
data2$ovlWith2 <- factor(data2$ovlWith, levels = c("Undesignated lands", # remember this goes in reverse
                                                  "PAs & Undesignated lands",
                                                  "Both PAs",
                                                  "PA sustainable use",
                                                  "PA strict protection",
                                                  "Rural settlements (no ovlp)",
                                                  "Private lands (no ovlp)"))
summary(data2)

overlapColors <- c("Rural settlements (no ovlp)"= "#FC8D62",
                   "Private lands (no ovlp)" = "#8DA0CB",
                   "PA strict protection" = "#1B9E77",
                   "PA sustainable use" = "#8C7E5B",
                   "Both PAs" = "#548E69",
                   "Undesignated lands" ="#1d6c7d",
                   "PAs & Undesignated lands" = "#397D73")

# plot overlaps ----
# label the n of the overlaps
sample_sizes <- data2 %>%
  # filter(LTcateg3 == "Private lands") %>%
  group_by(ovlWith2) %>%
  summarize(n = n())
medianBD <- data2 %>%
  # filter(LTcateg3 == "Private lands") %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))

# richness
# private land overlaps
currentRichness3 <- myboxplots(data2[which(data2$LTcateg == "Private lands"),], tenureCategory = ovlWith2, BDvariable = Richness_2020, 
                              BDvariableTitle = "Species richness 2020", 
                              fill = ovlWith2, sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_overlapsPrivate.png",  width = 2450, height = 970, units = "px", res = 300, bg = "transparent")
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()
dev.off()

# rural settlement overlaps 

currentRichness3 <- myboxplots(data2[which(data2$LTcateg3 == "Rural settlements"),], tenureCategory = ovlWith2, BDvariable = Richness_2020, 
                               BDvariableTitle = "Species richness 2020", 
                               fill = ovlWith2, sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_overlapsRurSetts.png",  width = 2450, height = 970, units = "px", res = 300)
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()
dev.off()


# endemism
medianBD <- data2 %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))
currentEndemism3 <- myboxplots(data2[which(data2$LTcateg == "Private lands"),], tenureCategory = ovlWith2, BDvariable = Endemism_2020, 
                              BDvariableTitle = "Endemism 2020", 
                              fill = ovlWith2, sample_sizes = sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentEndemism3 + coord_cartesian(ylim = c(100,230)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_overlapsFocus_20250226.png", width = 2450, height = 970, units = "px", res = 300, bg = "transparent")
currentEndemism3 + coord_cartesian(ylim = c(100,230)) + coord_flip()
dev.off()


# rerun of entire first-figure analysis excluding the overlaps ----
dataNoOverlaps <- data %>% 
  mutate(ovlExists = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) > 0) %>% # note there's a small classification mistake here, which means that it excludes some PA sust use bc they overlap w rural settlemtns (instead of the other way around)
  filter(ovlExists == FALSE)
sample_sizes <- dataNoOverlaps %>% 
  group_by(LTcateg3) %>%
  summarize(n = n())
medianBD <- dataNoOverlaps %>% 
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))

currentRichness2 <- myboxplots(dataNoOverlaps, tenureCategory = LTcateg3, BDvariable = Richness_2020, 
                               BDvariableTitle = "Species richness 2020", 
                               fill = LTcateg3, sample_sizes = sample_sizes)
currentRichness2 

medianBD <- data %>%
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))
currentEndemism2 <- myboxplots(dataNoOverlaps, tenureCategory = LTcateg3, BDvariable = Endemism_2020, 
                               BDvariableTitle = "Endemism 2020",
                               fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism2

# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentRichness_20250226_noOverlaps.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness2
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_20250226_noOverlaps.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism2 
dev.off()
# do the summaries change?
# summarize standardized differences between LT categories
category_list <- unique(dataNoOverlaps$LTcateg)
s <- list()
for(i in 1:length(category_list))
{
  r <- cohen.d(dataNoOverlaps$Richness_2020[dataNoOverlaps$LTcateg3 == paste0(category_list[i])], 
               dataNoOverlaps$Richness_2020[dataNoOverlaps$LTcateg3 != paste0(category_list[i])], 
               na.rm = T, conf.level = 0.99)
  e <- cohen.d(dataNoOverlaps$Endemism_2020[dataNoOverlaps$LTcateg3 == paste0(category_list[i])], 
               dataNoOverlaps$Endemism_2020[dataNoOverlaps$LTcateg3 != paste0(category_list[i])], 
               na.rm = T, conf.level = 0.99)
  s[[i]] <- data.frame("LTcateg" = paste0(category_list[i]),
                       "cohen_d_R" = r$estimate, 
                       "sig_R" = r$magnitude,
                       "cohen_d_E" = e$estimate, 
                       "sig_E" = e$magnitude)
}
s <- do.call(rbind, s)
s

# write out this table
setwd(paste0(wdmain, "/output"))
write.csv(s, "cohensD_withoutOverlaps.csv", row.names = F)

# check cohen's d of overlapping areas compared to non-overlapping private lands ----
ovl_list <- unique(data2$ovlWith2)
diffs <- list()
for(i in 1:length(ovl_list)) # for each overlap
{
  # compare richness
  r <- cohen.d(data2$Richness_2020[data2$ovlWith2 == paste0(ovl_list[i])], # remember, data2 is the data that DOES overlap
               dataNoOverlaps$Richness_2020[dataNoOverlaps$LTcateg3 == "Private lands"], # so, we're comparing the private lands that overlap with xyz with the private lands that do not overlap
               na.rm = T, conf.level = 0.99)
  # compare endemism
  e <- cohen.d(data2$Endemism_2020[data2$ovlWith2 == paste0(ovl_list[i])], # remember, data2 is the data that DOES overlap
               dataNoOverlaps$Endemism_2020[dataNoOverlaps$LTcateg3 == "Private lands"], # so, we're comparing the private lands that overlap with xyz with the private lands that do not overlap
               na.rm = T, conf.level = 0.99)
  
  diffs[[i]] <- data.frame("LTcateg" = paste0(ovl_list[i]),
                           "cohen_d_R" = r$estimate, 
                           "sig_R" = r$magnitude,
                           "cohen_d_E" = e$estimate, 
                           "sig_E" = e$magnitude)
}
diffs <- do.call(rbind, diffs)
diffs


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
                              BDvariableTitle = "Species richness 2020", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentRichness + facet_wrap(~biome2, nrow = 3, scales = "fixed") 

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_perBiomes.png", width = 3500, height = 4000, units = "px", res = 300)
currentRichness + facet_wrap(~biome2, nrow = 3, scales = "fixed") 
dev.off()

currentEndemism <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable =  Endemism_2020, 
                              BDvariableTitle = "Endemism 2020", 
                              fill = LTcateg3, sample_sizes = sample_sizes)
currentEndemism + facet_wrap(~biome2, nrow = 3, scales = "fixed")

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_perBiomes.png", width = 3500, height = 4000, units = "px", res = 300)
currentEndemism + facet_wrap(~biome2, nrow = 3, scales = "fixed")
dev.off()

# disaggregated plot shows that the pattern holds throughout biomes


# deforestation ----

# plot current forest cover
sample_sizes <- data %>%
  group_by(LTcateg3) %>%
  summarize(n = n())
currentForest <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = p_for23,
                            BDvariableTitle = "% forest cover 2023 (per property)",
                            fill = LTcateg3, sample_sizes = sample_sizes)
currentForest

# DO NOT OVERWRITE bc the original one i made remains correct and the new boxplot function messes with the median label 
# setwd(paste0(wdmain, "/output"))
# png("Forest-2023_flippedboxplot_20241202.png", width = 2450, height = 970,   units = "px", res = 300)
# currentForest
# dev.off()

# biomes disag for forest 2023?
sample_sizes <- biomeData %>%
  group_by(LTcateg3, biome2) %>%
  summarize(n = n(), .groups = 'drop')
currentForest <- myBiome_boxplots(biomeData, tenureCategory = LTcateg3, BDvariable = p_for23, 
                            BDvariableTitle = "% forest cover 2023", 
                            fill = LTcateg3, sample_sizes = sample_sizes)
currentForest + facet_wrap(~biome2, nrow = 3, scales = "fixed")

setwd(paste0(wdmain, "/output"))
png("CurrentForest_perBiomes.png", width = 3500, height = 4000, units = "px", res = 300)
currentForest + facet_wrap(~biome2, nrow = 3, scales = "fixed")
dev.off()

# add cohen's d for biome disaggregation ----
colnames(biomeData)
# 1) add how much tenure categories differ from each other within biomes
category_list <- unique(biomeData$LTcateg)
biome_list <- unique(biomeData$biome)

biomeData2 <- split(biomeData, biomeData$biome2)
biome_summary <- list()
for(j in 1:length(biomeData2))
{
  # for every biome, create a list of the tenure categories (which will vary)
  summary <- list()
  tenure_categories <- unique(biomeData2[[j]]$LTcateg)
  for(i in 1:length(tenure_categories))
  {
    r <- cohen.d(biomeData2[[j]]$Richness_2020[biomeData2[[j]]$LTcateg3 == paste0(tenure_categories[i])], 
                       biomeData2[[j]]$Richness_2020[biomeData2[[j]]$LTcateg3 != paste0(tenure_categories[i])], 
                       na.rm = T, conf.level = 0.99)
    e <- cohen.d(biomeData2[[j]]$Endemism_2020[biomeData2[[j]]$LTcateg3 == paste0(tenure_categories[i])], 
                      biomeData2[[j]]$Endemism_2020[biomeData2[[j]]$LTcateg3 != paste0(tenure_categories[i])], 
                      na.rm = T, conf.level = 0.99)
    
    summary[[i]] <- data.frame("LTcateg" = paste0(tenure_categories[i]),
                                        "biome" = paste0(names(biomeData2[j])),
                                        "cohen_d_R" = r$estimate, 
                                        "sig_R" = r$magnitude,
                                        "cohen_d_E" = e$estimate, 
                                        "sig_E" = e$magnitude)
  }
  summary <- do.call(rbind, summary)

biome_summary[[j]] <- summary
}
biome_summary <- do.call(rbind, biome_summary)

# write out
setwd(paste0(wdmain, "/output"))
write.csv(biome_summary, "cohensD_biomes.csv", row.names = F)


# 2) how much biomes BD varies?
diffsBiomes <- list()
biomes <- unique(biomeData$biome2)
for(i in 1:length(biomes))
{
  r <- cohen.d(biomeData$Richness_2020[biomeData$biome == paste0(biomes[i])], biomeData$Richness_2020[biomeData$biome != paste0(biomes[i])], na.rm = T, conf.level = 0.95)
  e <- cohen.d(biomeData$Endemism_2020[biomeData$biome == paste0(biomes[i])], biomeData$Endemism_2020[biomeData$biome != paste0(biomes[i])], na.rm = T, conf.level = 0.95)
  diffsBiomes[[i]] <- data.frame("biome" = paste0(biomes[i]),
                                 "cohen_d_R" = r$estimate, 
                                 "sig_R" = r$magnitude,
                                 "cohen_d_E" = e$estimate, 
                                 "sig_E" = e$magnitude)
}
do.call(rbind, diffsBiomes) # this is interesting because some things flip, but it doesnt get at the magnitude of differences   
    


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


