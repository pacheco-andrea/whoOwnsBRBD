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

# deal with duplicated id's, i.e., id's with multiple observations 
# i've realized that i've got duplicate ids, across all categories
test <- data %>% distinct(LTcateg, id, myOverCat, .keep_all = T)
nrow(test)
nrow(data)
# these are the duplicate ids:
test2 <- data %>% filter(duplicated(paste(LTcateg, id, myOverCat)))
nrow(test2)
head(test2)
# but i know that ONE id has multiple values extracted, meaning these aren't duplicated numbers, there is no actual double counting of values
# this happened due to all the overlaps i was processing, with for instance, many rural properties overlapping with one property
# then, they turn out spatially separated
# if i want to merge them i know that the area should be summed, but the rest of the biodiversity values should be averaged
# the only shame is that then i'll be "losing" observations when i have to filter out (for the BD analysis) parcels that are <900m2, 
# this might not have originally been the case for some of the polygons, but it happens when it's fractioned over the overlaps




# if the extraction actually happened on these smaller fractions - WHAT SHOULD I DO???




# first i need to cbind the forest info

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
summary(data$id == fData$id) # ok, this means i can simply cbind instead of joining

data$for23 <- fData$for23
data$defor <- fData$defor

# data cleaning and organizing ----

# deal with the repeated id's:
# examine duplicates
duplicates <- data %>%
  group_by(id) %>%
  filter(n() >1) %>%
  ungroup
head(duplicates)
duplicates <- arrange(duplicates, id)
head(duplicates)

data2 <- data %>%
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
    Richness_loss = mean(Richness_loss, na.rm = T),
    for23  = sum(for23, na.rm = T),
    defor = sum(defor, na.rm = T)) %>%
  ungroup()

summary(data2)
length(unique(data$id))


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
data <- data[which(data$areakm2 >= .9),]
nrow(data)

# filter out these undesignated categories which are a bit ambiguous 
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]

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
                                                           # "Private PA",
                                                           "Quilombola lands"))

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
png("CurrentBD_perTenureCategNoOverlaps_violinDensity_20241028.png", width = 3300, height = 4000, units = "px", res = 300)
currentBDviolin
dev.off()


# TESTING/ CHECKING THESE VALUES

test <- data[which(data$LTcateg2 == "Indigenous"),]
summary(test$Richness_2020/test$areakm2)
ggplot(test, aes(x = LTcateg2, y = Richness_2020/areakm2, fill = LTcateg2)) +
    geom_violin(position = position_dodge(width = 0.9), alpha = 0.5) +
    geom_boxplot(width=0.2, color="grey20", position = position_dodge(width = 0.9), alpha = 0.6) +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    labs(y = "BDvariableTitle") +
    theme(panel.background = element_blank(), panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray70"),
          legend.title = element_blank(), legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank()) +
    facet_wrap(vars(LTcateg2), nrow = 1, scales = "free_x") +
    geom_text(data = sample_sizes, aes(x = LTcateg2, y = Inf, label = paste("n =", n)), vjust = 2, size = 3.5, inherit.aes = F)
  
data %>%
  group_by(LTcateg2) %>%
  summarize(rmin = min(Richness_2020, na.rm = T),
            rmax = max(Richness_2020, na.rm = T),
            rmean = mean(Richness_2020, na.rm = T),
            amin = min(areakm2, na.rm = T),
            amax = max(areakm2, na.rm = T),
            amean = mean(areakm2, na.rm = T),
            min = min(Richness_2020/areakm2, na.rm = T),
            max = max(Richness_2020/areakm2, na.rm = T),
            mean = mean(Richness_2020/areakm2, na.rm = T)) # OK im convinced they actually make sense


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
png("BDLossProportion_perTenureCategNoOVerlaps_violinArea_20241028.png",width = 3300, height = 4000,  units = "px", res = 300)
lossBD
dev.off()

# i want to repeat these boxplits - but distinguish across biomes



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

# idk, these boxplots seem pretty meaningless... should the forest be barplots?


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

# split up boxplots by biome -----

# get biome data
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
st_crs(biomes)

setwd(paste0(wdmain,"/data/processed/LT_no-overlaps/"))
f <- grep(".shp", list.files())
b1 <- lapply(list.files()[f], st_read)
lapply(b1, colnames)
lapply(b1, st_crs)

b1 <- do.call(rbind, b1)

# extractiocolnames# extractions OVERLAPS  ----
setwd(paste0(wdmain,"/data/processed/LT_overlaps/"))
f <- grep(".shp", list.files())

# extractions OVERLAPS across public & private categs ----

setwd(paste0(wdmain,"/data/processed/LT_pubxpri_overlaps/"))
f <- grep(".shp", list.files())


# i've left this one for last because it takes the longest
# extractions PRIVATE no overlaps  ----
setwd(paste0(wdmain,"/data/processed/LT_no-overlaps_private/"))
f <- grep(".shp", list.files())

