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
