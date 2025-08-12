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

# clean pixel-based data for double checking analysis using the pixels (versus polygon extractions)----
setwd(paste0(wdmain, "/data/processed/BD_perPixel_tenureMasks/"))
l <- list.files()
data_pixel <- lapply(l, read.csv)
data_pixel <- do.call(rbind, data_pixel)
summary(data_pixel)
unique(data_pixel$LTcateg)

# PLOTS ----
# edit labels of categories:
unique(data$LTcateg)
unique(data$LTcateg2) 
data$LTcateg <- gsub("Private lands", "Private lands & claims", data$LTcateg)
data$LTcateg <- gsub("Indigenous", "Indigenous lands", data$LTcateg)
data$LTcateg2 <- data$LTcateg

# repeat renaming for pixel data
data_pixel$LTcateg[which(data_pixel$LTcateg == "ruralProperties")] <- "Private lands"
data_pixel$LTcateg[which(data_pixel$LTcateg == "undesignated")] <- "Undesignated lands"
data_pixel$LTcateg[which(data_pixel$LTcateg ==  "ruralSettlements")] <- "Rural settlements"
data_pixel$LTcateg[which(data_pixel$LTcateg == "PA_strict")] <- "PA strict protection"
data_pixel$LTcateg[which(data_pixel$LTcateg == "PA_strict_selfOverlaps")] <- "PA strict protection"
data_pixel$LTcateg[which(data_pixel$LTcateg == "PA_sustuse_selfOverlaps")] <- "PA sustainable use"
data_pixel$LTcateg[which(data_pixel$LTcateg == "PA_sustuse")] <- "PA sustainable use"
data_pixel$LTcateg[which(data_pixel$LTcateg == "sustUsePAs-ruralSettlements")] <- "PA sustainable use"
data_pixel$LTcateg[which(data_pixel$LTcateg == "indigenous")] <- "Indigenous"
data_pixel$LTcateg[which(data_pixel$LTcateg == "indigenous_selfOverlaps")] <- "Indigenous"
data_pixel$LTcateg[which(data_pixel$LTcateg == "PA_strict-indigenous")] <- "Indigenous"
data_pixel$LTcateg[which(data_pixel$LTcateg == "PA_sustuse-indigenous")] <- "Indigenous"
data_pixel$LTcateg[which(data_pixel$LTcateg == "privatePAs")] <- "Private PA"
data_pixel$LTcateg[which(data_pixel$LTcateg == "quilombola")] <- "Quilombola lands"
data_pixel$LTcateg[which(data_pixel$LTcateg == "quilombola_selfOverlaps")] <- "Quilombola lands"
data_pixel$LTcateg[which(data_pixel$LTcateg == "quilombola-ruralProperties")] <- "Quilombola lands" # important bc otherwise we'd "lose" a bunch of quilombola observations
data_pixel$LTcateg[which(data_pixel$LTcateg == "ruralProperties-PA_strict")] <- "Private overlapping PA strict"
data_pixel$LTcateg[which(data_pixel$LTcateg == "ruralProperties-PA_sustuse")] <- "Private overlapping PA sustainable use"
data_pixel$LTcateg[which(data_pixel$LTcateg == "ruralProperties-undesignated")] <- "Private overlapping undesignated"
unique(data_pixel$LTcateg)
# drop observations of self overlaps i dont need
data_pixel <- data_pixel %>%
  filter(LTcateg != "indPAoverlap-ruralSettlements" &
           LTcateg != "privatePAs-ruralProperties" &
           LTcateg != "privatePAs_selfOverlaps" &
           LTcateg != "undesignated-ruralSettlements" &
           LTcateg != "Private PA")
unique(data_pixel$LTcateg)

data_pixel$LTcateg <- gsub("Private lands", "Private lands & claims", data_pixel$LTcateg)
data_pixel$LTcateg <- gsub("Indigenous", "Indigenous lands", data_pixel$LTcateg)

# make into factor?
data_pixel$LTcateg2 <- factor(data_pixel$LTcateg, levels = c("Private lands & claims",
                                                 "Undesignated lands",
                                                 "Rural settlements",
                                                 "PA strict protection",
                                                 "PA sustainable use",
                                                 "Indigenous lands" ,
                                                 "Private PA",
                                                 "Quilombola lands",
                                                 "Private overlapping PA strict",
                                                 "Private overlapping PA sustainable use",
                                                 "Private overlapping undesignated"))

summary(data_pixel)


# establish colors
tenureColors <- c("Indigenous lands" = "#E78AC3",
                  "non-overlapped" = "gray70",   
                  "PA strict protection" = "#1B9E77",       
                  "PA sustainable use" =  "#8C7E5B",
                  # "Private PA" = "#99d8c9",  
                  "Quilombola lands" =  "#FFD700",
                  "Private lands & claims" = "#8DA0CB",
                  "Rural settlements" = "#FC8D62",
                  "Undesignated lands" ="#1d6c7d")



# current biodiversity under different tenure categories ----
# including all observations
data$LTcateg2 <- data$LTcateg2 <- factor(data$LTcateg, levels = c("Private lands & claims",
                                                                  
                                                                  "Rural settlements",
                                                                  "PA strict protection",
                                                                  "PA sustainable use",
                                                                  "Indigenous lands" ,
                                                                  "Undesignated lands",
                                                                  "Private PA",
                                                                  "Quilombola lands"))
data$LTcateg3 <- factor(data$LTcateg2, levels = rev(levels(data$LTcateg2)))
data$LTcateg3 <- droplevels(data$LTcateg3)
# there's a small issue where the overlap between sust use and rural setts is flipped:
# the main category should be rural settlements
data$LTcateg3[which(data$ovl_ruralSett > 0)] <- "Rural settlements" # replace category 
# and the overlap category should be the PA sust use
data$ovl_PA_sustuse[which(data$ovl_ruralSett > 0)] # replace value which is currently 0
data$ovl_PA_sustuse[which(data$ovl_ruralSett > 0)] <- data$ovl_ruralSett[which(data$ovl_ruralSett > 0)] # which the overlapping value


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

# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentRichness_20250701.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness2
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_20250701.png", width = 2450, height = 970, units = "px", res = 300)
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

# checking what the boxplot looks like when based on the pixel counts
sample_sizes <- data_pixel %>%
  group_by(LTcateg2) %>%
  summarize(n = n())
medianBD <- data %>%
  group_by(LTcateg2) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))
currentRichness2 <- myboxplots(data, tenureCategory = LTcateg2, BDvariable = Richness_2020, 
                               BDvariableTitle = "Species richness 2020", 
                               fill = LTcateg2, sample_sizes = sample_sizes)
currentRichness2 





# data wrangling for id'ing overlaps  ----
# this is  to see whether there is a difference in the private lands that overlap with conservation areas
# create a column that flags all the overlaps
data2 <- data %>%
  mutate(ovlExists = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) > 0)
head(as.data.frame(data2))

# i looked into the percentage of overlapping AREA, and as in the overlapping analysis, the majority of overlaps are in undesignated lands
# but ultimately, since i dont think i can completely rely on these exact numbers, i dont incorporate this into calculations
# instead, i just want to verify the amount of BD in properties with overlaps in PAs/undesignated lands

# create another column to summarize which categories overlap
data2$ovlWith <- NA 
# one by one, overwrite this "overlaps with" column to flag which other category it overlaps with 
data2$ovlWith[which(data2$ovl_PA_strict > 0 & data2$ovl_PA_sustuse == 0 & data2$ovl_undesignated == 0)] <- "PAs"
data2$ovlWith[which(data2$ovl_PA_sustuse > 0 & data2$ovl_PA_strict == 0 & data2$ovl_undesignated == 0)] <- "PAs"
data2$ovlWith[which(data2$ovl_undesignated > 0 & data2$ovl_PA_strict == 0 & data2$ovl_PA_sustuse == 0)] <- "Undesignated lands"
# also flag cases where there are multiple overlaps
data2$ovlWith[which(data2$ovl_undesignated == 0 & (data2$ovl_PA_strict > 0 & data2$ovl_PA_sustuse > 0))] <- "PAs"
data2$ovlWith[which(data2$ovl_undesignated > 0 & 
                      (data2$ovl_PA_strict > 0 | data2$ovl_PA_sustuse > 0))] <- "Both"

# fill in the NAs to indicate no overlaps
# but still indicate another column with no overlaps bc i need it later
data2$ovlWith3 <- data2$ovlWith
data2$ovlWith3[is.na(data2$ovlWith3)] <- paste0(data2$LTcateg3[is.na(data2$ovlWith)], " (no ovl)")
data2$ovlWith[is.na(data2$ovlWith)] <- paste0(data2$LTcateg3[is.na(data2$ovlWith)])
unique(data2$ovlWith)
# but still indicate another column with no overlaps bc i need it later

data2 <- data2 %>%
  filter(LTcateg3 == "Private lands & claims" | LTcateg3 == "Rural settlements") # i'm only plotting these two
summary(data2)
unique(data2$ovlWith)
# make into factor
data2$ovlWith2 <- factor(data2$ovlWith, levels = c("Both",
                                                   "Undesignated lands", # remember this goes in reverse
                                                   "PAs",
                                                   "Rural settlements",
                                                   "Private lands & claims"))
summary(data2)
unique(data2$ovlWith2)
# plot overlaps ----
overlapColors <- c("Rural settlements"= "#FC8D62",
                   "Private lands & claims" = "#8DA0CB",
                   "PAs" = "#1B9E77",
                   "Both" = "#548E69",
                   "Undesignated lands" ="#1d6c7d")


# richness
# private land overlaps
sample_sizes <- data2 %>%
  filter(LTcateg3 == "Private lands & claims") %>%
  group_by(ovlWith2) %>%
  summarize(n = n())
medianBD <- data2 %>%
  filter(LTcateg3 == "Private lands & claims") %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))
currentRichness3 <- myboxplots(data2[which(data2$LTcateg == "Private lands & claims"),], tenureCategory = ovlWith2, BDvariable = Richness_2020, 
                              BDvariableTitle = "Species richness 2020", 
                              fill = ovlWith2, sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_overlapsPrivate.png",  width = 2000, height = 750, units = "px", res = 300, bg = "transparent")
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()
dev.off()

# rural settlement overlaps 
sample_sizes <- data2 %>%
  filter(LTcateg3 == "Rural settlements") %>%
  group_by(ovlWith2) %>%
  summarize(n = n())
medianBD <- data2 %>%
  filter(LTcateg3 == "Rural settlements") %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Richness_2020, na.rm = T),0))

currentRichness3 <- myboxplots(data2[which(data2$LTcateg3 == "Rural settlements"),], tenureCategory = ovlWith2, BDvariable = Richness_2020, 
                               BDvariableTitle = "Species richness 2020", 
                               fill = ovlWith2, sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentRichness_overlapsRurSetts.png",  width = 2000, height = 400, units = "px", res = 300, bg = "transparent")
currentRichness3 + coord_cartesian(ylim = c(400,1000)) + coord_flip()
dev.off()


# endemism
# private lands
sample_sizes <- data2 %>%
  filter(LTcateg3 == "Private lands & claims") %>%
  group_by(ovlWith2) %>%
  summarize(n = n())
medianBD <- data2 %>%
  filter(LTcateg3 == "Private lands & claims") %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))

currentEndemism3 <- myboxplots(data2[which(data2$LTcateg == "Private lands & claims"),], tenureCategory = ovlWith2, BDvariable = Endemism_2020, 
                              BDvariableTitle = "Endemism 2020", 
                              fill = ovlWith2, sample_sizes = sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentEndemism3 + coord_cartesian(ylim = c(100,230)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_overlapsFocus_20250226.png", width = 2000, height = 750, units = "px", res = 300, bg = "transparent")
currentEndemism3 + coord_cartesian(ylim = c(100,230)) + coord_flip()
dev.off()
# rural setts
sample_sizes <- data2 %>%
  filter(LTcateg3 == "Rural settlements") %>%
  group_by(ovlWith2) %>%
  summarize(n = n())
medianBD <- data2 %>%
  filter(LTcateg3 == "Rural settlements") %>%
  group_by(ovlWith2) %>%
  summarize(medianBD = round(median(Endemism_2020, na.rm = T),0))

currentEndemism3 <- myboxplots(data2[which(data2$LTcateg3 == "Rural settlements"),], tenureCategory = ovlWith2, BDvariable = Endemism_2020, 
                               BDvariableTitle = "Endemism 2020", 
                               fill = ovlWith2, sample_sizes) +
  scale_colour_manual(values = overlapColors, aesthetics = c("color", "fill"))
currentEndemism3 + coord_cartesian(ylim = c(100,230)) + coord_flip()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_overlapsRurSetts.png",  width = 2000, height = 400, units = "px", res = 300, bg = "transparent")
currentEndemism3 + coord_cartesian(ylim = c(100,230)) + coord_flip()
dev.off()

# cohen's d of overlaps ----
category_list <- unique(data2$ovlWith3)[grep("no ovl", unique(data2$ovlWith3))]

s <- list()
for(i in 1:length(category_list)) # for the 2 no overlapping categs
{
  # test whether no overlapping private lands are different than the overlapping
  r <- cohen.d(data2$Richness_2020[data2$ovlWith3 == paste0(category_list[i])], # this will be the "no ovl"
               data2$Richness_2020[!grepl("\\(no ovl\\)", data2$ovlWith3)], # this needs to be neither of "no ovl"
               na.rm = T, conf.level = 0.99)
  e <- cohen.d(data2$Endemism_2020[data2$ovlWith3 == paste0(category_list[i])], 
               data2$Endemism_2020[!grepl("\\(no ovl\\)", data2$ovlWith3)],
               na.rm = T, conf.level = 0.99)
  s[[i]] <- data.frame("LTcateg" = paste0(category_list[i]),
                       "cohen_d_R" = r$estimate, 
                       "sig_R" = r$magnitude,
                       "cohen_d_E" = e$estimate, 
                       "sig_E" = e$magnitude)
}
s <- do.call(rbind, s)
s

# and where are these overlaps found?
data2 %>%
  group_by(biome) %>%
  summarize(n = n(),
            area = sum(areakm2))

# rerun of entire first-figure analysis excluding the overlaps ----
dataNoOverlaps <- data %>% 
  mutate(ovlExists = rowSums(select(., starts_with("ovl_")), na.rm = TRUE) > 0) %>% 
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
png("CurrentRichness_20250703_noOverlaps.png", width = 2450, height = 970, units = "px", res = 300)
currentRichness2
dev.off()

setwd(paste0(wdmain, "/output"))
png("CurrentEndemism_20250703_noOverlaps.png", width = 2450, height = 970, units = "px", res = 300)
currentEndemism2 
dev.off()

# do the Cohen's d summaries change?
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
do.call(rbind, diffsBiomes) # this is interesting because some things flip, but it doesnt get at the magnitude of differences of bd among biomes - which is in any case not so relevant   
    

# Forest cover ----

# plot current forest cover
sample_sizes <- data %>%
  group_by(LTcateg3) %>%
  summarize(n = n())
medianBD <- data %>%
  group_by(LTcateg3) %>%
  summarize(medianBD = round(median(p_for23, na.rm = T),0))

currentForest <- myboxplots(data, tenureCategory = LTcateg3, BDvariable = p_for23,
                            BDvariableTitle = "% forest cover 2023 (per property)",
                            fill = LTcateg3, sample_sizes = sample_sizes)
currentForest

setwd(paste0(wdmain, "/output"))
png("Forest-2023_boxplot.png", width = 2450, height = 970,   units = "px", res = 300)
currentForest
dev.off()

# cohen's d of forest cover
category_list <- unique(data$LTcateg)
s <- list()
for(i in 1:length(category_list))
{
  r <- cohen.d(data$p_for23[data$LTcateg3 == paste0(category_list[i])], 
               data$p_for23[data$LTcateg3 != paste0(category_list[i])], 
               na.rm = T, conf.level = 0.99)
  s[[i]] <- data.frame("LTcateg" = paste0(category_list[i]),
                       "cohen_d_for" = r$estimate, 
                       "sig_for" = r$magnitude)
}
s <- do.call(rbind, s)
s

# biomes disag for forest 2023 ----
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

# cohens d for forest cover x biomes ----
names(biomeData2)
biome_summary <- list()
for(j in 1:length(biomeData2))
{
  # for every biome, create a list of the tenure categories (which will vary)
  summary <- list()
  tenure_categories <- unique(biomeData2[[j]]$LTcateg)
  # for each of these categories, make the comparisons of forest cover
  for(i in 1:length(tenure_categories))
  {
    r <- cohen.d(biomeData2[[j]]$p_for23[biomeData2[[j]]$LTcateg3 == paste0(tenure_categories[i])], 
                 biomeData2[[j]]$p_for23[biomeData2[[j]]$LTcateg3 != paste0(tenure_categories[i])], 
                 na.rm = T, conf.level = 0.99)
    summary[[i]] <- data.frame("LTcateg" = paste0(tenure_categories[i]),
                               "biome" = paste0(names(biomeData2[j])),
                               "cohen_d_for" = r$estimate, 
                               "sig_for" = r$magnitude)
  }
  summary <- do.call(rbind, summary)
  
  biome_summary[[j]] <- summary
}
biome_summary <- do.call(rbind, biome_summary)
biome_summary
# write out
setwd(paste0(wdmain, "/output"))
write.csv(biome_summary, "cohensD_biomes_forest.csv", row.names = F)

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


