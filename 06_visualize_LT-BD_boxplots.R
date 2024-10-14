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
myColumns <- c("LTcateg", "id", "areakm2", 
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
data <- do.call(rbind, data)
colnames(data) <- gsub("mean.", "", colnames(data))

test <- data %>% distinct(LTcateg, id, myOverCat, .keep_all = T)
nrow(test)
nrow(data)
test2 <- data %>% filter(duplicated(paste(LTcateg, id, myOverCat)))
head(test2)
# read data on deforestation extractions 

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

# join data
data_new <- inner_join(data, fData, by = c("LTcateg", "id", "myOverCat"))
nrow(data_new)

# data cleaning and organizing ----
# summary(data)
# unique(data$LTcateg) 
# unique(data$myOverCat)


# filter out polygons that are < .5 km2 in area - the resolution of the BD data
data.1km <- data[which(data$areakm2 <= .9),] 
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
# henceforth use only the data that is under .5 km2
data <- data[which(data$areakm2 >= .9),]
rm(data.1km)
# filter out these undesignated categories which are a bit ambiguous 
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]


# examine the NAs (places where there was no value extracted)
summary(data[which(is.na(data$Endemism_2020 )),])
unique(data[which(is.na(data$Endemism_2020)),]$myOverCat)
ggplot(data[which(is.na(data$Endemism_2020)),], aes(x = myOverCat)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  coord_cartesian(ylim = c(0,500))
# in sum, some losses across the board 

# CLEAN UP THE OVERLAP CATEGORIES: ----
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
unique(data$overlapsWith)

overlapsToCheck <- unique(data$overlapsWith)[-c(grep("self", unique(data$overlapsWith)), grep("none", unique(data$overlapsWith)))]

# just replace manually each set?
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


# PLOTS ----

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
data$overlapsWith2 <- factor(data$overlapsWith, levels = c("Private lands",
                                                 "Undesignated lands",
                                                 "Rural settlements",
                                                 "PA strict protection",
                                                 "PA sustainable use",
                                                 "Indigenous" ,
                                                 "Private PA",
                                                 "Quilombola lands"))

tenureColors <- c("Indigenous" = "#E78AC3",
                  "non-overlapped" = "gray70",   
                  "PA strict protection" = "#1B9E77",       
                  "PA sustainable use" =  "#8C7E5B",
                  "Private PA" = "#99d8c9",  
                  "Quilombola lands" =  "#FFD700",
                  "Private lands" = "#8DA0CB",
                  "Rural settlements" = "#FC8D62",
                  "Undesignated lands" ="#1d6c7d")

# create function for plotting the biodiversity boxplots across biodiversity variables

# create variable with amount of observations per category
sample_sizes <- data %>%
  group_by(LTcateg2) %>%
  summarize(n = n())

# test violin plot
ggplot(data, aes(x = LTcateg2, y = (richness_current/areakm2), fill = overlapsWith2)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_boxplot(width=0.2, color="grey20", alpha=0.2, position = position_dodge(width = 0.9)) +
  scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
  labs(y = "test") +
  theme(panel.background = element_blank(), panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid', colour = "gray70"),
        legend.title = element_blank(), legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank()) +
  facet_wrap(vars(LTcateg2), nrow = 1, scales = "free_x") +
  geom_text(data = sample_sizes, aes(x = LTcateg2, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)

boxplotBD <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL){
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = LTcateg2)) +
    geom_boxplot() +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    labs(y = BDvariableTitle) +
    theme(panel.background = element_blank(), legend.title = element_blank(), legend.position = "none")
  return(plot)
}

violinplotBD <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL, fill){
  
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

# isolate overlaps to plot these alongside the main boxplots
no <- grep("no-overlaps", data$myOverCat)
dataOverlaps <- data[-no,]

# current biodiversity under different tenure categories ----
# richness <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = richness_current, BDvariableTitle = "Current species richness")
# richness <- richness + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# ende <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = Ende_current, BDvariableTitle = "Current endemism")
# ende <- ende + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# phyl <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = phylodiversity_current, BDvariableTitle = "Current phylodiversity")
# phyl <- phyl + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# # to plot all together
# currentBD <- plot_grid(richness, 
#                        ende, 
#                        phyl, 
#                        nrow = 1)
# currentBD
# # save plot
# setwd(paste0(wdmain, "/output"))
# png("CurrentBD_perTenureCateg.png", width = 3600, height = 2400, units = "px", res = 300)
# currentBD
# dev.off()

# violin version with overlaps side by side
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
# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentBD_perTenureCategOverlaps_violinDensity_202410.png", width = 3300, height = 4000, units = "px", res = 300)
currentBDviolin
dev.off()

# violin version WITHOUT overlaps side by side
richness <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Richness_2020/areakm2), BDvariableTitle = "Species richness 2020 (density)", fill = LTcateg2)
richness 
ende <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Endemism_2020/areakm2), BDvariableTitle = "Endemism 2020 (density)", fill = LTcateg2)
# ende <- ende + coord_cartesian(ylim = c(0,1))
phyl <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (Phylodiversity_2020/areakm2), BDvariableTitle = "Phylodiversity 2020 (density)", fill = LTcateg2)
phyl 
# to plot all together
currentBDviolin <- plot_grid(richness, 
                             ende, 
                             phyl, 
                             nrow = 3, labels = c("A", "B", "C"))
currentBDviolin
# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentBD_perTenureCategNoOverlaps_violinDensity_202410.png", width = 3300, height = 4000, units = "px", res = 300)
currentBDviolin
dev.off()

# biodiversity losses across different categories ----

# this should now plot the PROPORTION OF LOSS (scaling for how much there was to begin with) over the AREA
# which should indicate the DENSITY OF LOSS
richness <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (rLoss_prop*areakm2), BDvariableTitle = "Area (km2) with potential sp. richness loss", fill = LTcateg)
richness <- richness + coord_cartesian(ylim = c(0, 50))
ende <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (eLoss_prop*areakm2), BDvariableTitle = "Area (km2) with potential endemism loss", fill = LTcateg)
ende <- ende + coord_cartesian(ylim = c(0,100))
phyl <- violinplotBD(data, tenureCategory = LTcateg2, BDvariable = (pLoss_prop*areakm2), BDvariableTitle = "Area (km2) with potential phylodiversity loss", fill = LTcateg)
phyl <- phyl + coord_cartesian(ylim = c(0,50))
# to plot all together
lossBD <- plot_grid(richness, 
                    ende, 
                    phyl, 
                    nrow = 3, labels = c("A", "B", "C"))
lossBD

setwd(paste0(wdmain, "/output"))
png("BDLossProportion_perTenureCategNoOVerlaps_violinArea.png",width = 3300, height = 4000,  units = "px", res = 300)
lossBD
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
original_csr <- select(original_csr, c("uf", "n_mf", "area_conv", "area_veg", "rl_ativo", "rl_def", "app_def", "desmat_p08", "id"))

# join data with tenure+bd data
data_extra <- left_join(data, original_csr, by = "id")
head(data_extra)
summary(data_extra)

# write out this information
setwd(paste0(wdmain, "data/processed/"))
write.csv(data_extra, "finalDataset_Tenure-BD-CSR.csv", row.names = F)

