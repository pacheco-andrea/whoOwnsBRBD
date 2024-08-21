#### Biodiversity and Tenure in Brazil ####

# script that visualizes biodiversity indicators per tenure categ that have been extracted previously
# two main outcomes:
# 1) boxplots of all BD variables, alongside boxplots of the categories that are overlapping
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
# setwd(paste0(wdmain, "/data/processed/bdExtractions-perPolygon"))
setwd(paste0(wdmain, "/data/processed/bdExtractions-perPolygon_v202408"))
l <- list.files()
tables <- lapply(l, read.csv)
lapply(tables, colnames)
names(tables) <- gsub(".csv","", l)

# the tables have different column names, need to make them consistent to join into one table
myColumns <- c("LTcateg", "id", "areakm2", "mean.phylodiversity_baseline", "mean.phylodiversity_current", "mean.phylodiversity_loss", 
               "mean.richness_baseline", "mean.richness_current", "mean.richness_loss", 
               "mean.Ende_baseline", "mean.Ende_current", "mean.Ende_loss")

data <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # select my columns
  df <- df[, myColumns]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
# lapply(data, colnames)
data <- do.call(rbind, data)
colnames(data) 
colnames(data) <- gsub("mean.", "", colnames(data))

# data cleaning and organizing ----
# summary(data)
# unique(data$LTcateg) 
# unique(data$myOverCat)


# filter out polygons that are < .5 km2 in area - the resolution of the BD data
data.5km <- data[which(data$areakm2 <= .5),] 
# quick visualization
# ggplot(data.5km, aes(x = LTcateg)) + 
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# note: 5806802 IRU properties were <.5km, out of the 7067703 total (= 82% of records)
# 5806802/7067703
# write out this data so that i can check it
setwd(paste0(wdmain, "data/processed/"))
write.csv(data.5km, "LT-BD_areaUnder.5km.csv", row.names = F)
# henceforth use only the data that is under .5 km2
data <- data[which(data$areakm2 >= .5),]

# filter out these undesignated categories which are a bit ambiguous 
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]


# examine the NAs (places where there was no value extracted)
summary(data[which(is.na(data$phylodiversity_current )),])
unique(data[which(is.na(data$phylodiversity_current )),]$myOverCat)
ggplot(data[which(is.na(data$phylodiversity_current )),], aes(x = myOverCat)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  coord_cartesian(ylim = c(0,500))
# in sum, some losses across the board, but let's see how this changes with the new data

# make LTcateg correspond with my naming of categories
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

# CLEAN UP THE OVERLAP CATEGORIES:
unique(data$myOverCat)
# create new column
data$overlapsWith <- NA
# overlaps are public-public ()
unique(data$myOverCat)[grep("no-overlaps", unique(data$myOverCat))]
data$overlapsWith[grep("no-overlaps", data$myOverCat)] <- "none"

# overlaps with self
unique(data$myOverCat)[grep("self", unique(data$myOverCat))]
data$overlapsWith[grep("self", data$myOverCat)] <- "self"

# overlaps public-private (i.e., IRU on PAs and undesignated)
unique(data$myOverCat)[grep("pubxpri", unique(data$myOverCat))]
data$overlapsWith[grep("pubxpri", data$myOverCat)] <- "IRU on public"

# private - private (i.e., private PAs on rural Properties, quilombola on ruralProperties)
unique(data$myOverCat)[grep("privatePAs-ruralProperties", unique(data$myOverCat))]
unique(data$myOverCat)[grep("quilombola-ruralProperties", unique(data$myOverCat))]
data$overlapsWith[grep("privatePAs-ruralProperties", data$myOverCat)] <- "IRU & private PAs"
data$overlapsWith[grep("quilombola-ruralProperties", data$myOverCat)] <- "IRU on quilombos"

# public-public (i.e., )
unique(data[which(is.na(data$overlapsWith)),]$myOverCat)
data$overlapsWith[which(is.na(data$overlapsWith))] <- "public on public"
unique(data$myOverCat[which(data$overlapsWith == "public on public")])

summary(data)
colnames(data)

# create variables for the proportion of loss

data$rLoss_prop <- data$richness_loss/(data$richness_baseline - data$richness_loss)
# i wanted to compare the bottom part of this proportion to the "current" layer - which is what it should be
# through some testing i found there were very minor, slight differences
# these differences would all point to BD losses which were *not* due to human-driven LUC
# which means the safest - or the most accurate to human-driven changes - way is to stick to using the difference bt baseline and loss (rather than simply the current)
data$eLoss_prop <- data$Ende_loss/(data$Ende_baseline - data$Ende_loss)
data$pLoss_prop <- data$phylodiversity_loss/(data$phylodiversity_baseline - data$phylodiversity_loss)


# PLOTS ----
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
boxplotBD <- function(data, tenureCategory, BDvariable, BDvariableTitle = NULL){
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = LTcateg2)) +
    geom_boxplot() +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    labs(y = BDvariableTitle) +
    theme(panel.background = element_blank(), legend.title = element_blank(), legend.position = "none")
  return(plot)
}

# isolate overlaps to plot these alongside the main boxplots
no <- grep("no-overlaps", data$myOverCat)
dataOverlaps <- data[-no,]

# current biodiversity under different tenure categories ----
richness <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = richness_current, BDvariableTitle = "Current species richness")
richness <- richness + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ende <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = Ende_current, BDvariableTitle = "Current endemism")
ende <- ende + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
phyl <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = phylodiversity_current, BDvariableTitle = "Current phylodiversity")
phyl <- phyl + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# to plot all together
currentBD <- plot_grid(richness, 
                       ende, 
                       phyl, 
                       nrow = 1)
currentBD
# save plot
setwd(paste0(wdmain, "/output"))
png("CurrentBD_perTenureCateg.png", width = 3600, height = 2400, units = "px", res = 300)
currentBD
dev.off()


# biodiversity losses across different categories ----
# to properly show this loss it should be shown as the proportion: loss / original-loss
# because this scales the loss of average richness, endemism, and phylodiversity 

# just check, what does it mean that many endemism losses are higher than 100%?
data[which(data$eLoss_prop >= 1),] # probably need to check this with Bira - the loss was greater than the original endemism value

# note, if i want to facet_wrap, then i'd need to make my data longer

richness <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = rLoss_prop*100, BDvariableTitle = "Loss in species richness (%)")
richness <- richness + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ende <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = eLoss_prop*100, BDvariableTitle = "Loss in endemism (%)")
ende <- ende + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+ coord_cartesian(ylim = c(0,100))
phyl <- boxplotBD(data, tenureCategory = LTcateg2, BDvariable = pLoss_prop*100, BDvariableTitle = "Loss in phylodiversity (%)")
phyl <- phyl + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# to plot all together
lossBD <- plot_grid(richness, 
                       ende, 
                       phyl, 
                       nrow = 1)
lossBD
setwd(paste0(wdmain, "/output"))
png("BDLossProportion_perTenureCateg.png", width = 3600, height = 2400, units = "px", res = 300)
lossBD
dev.off()
# current biodiversity and losses in OVERLAP categories ----
# ideally, i'd want two boxplots side by side the category, and then the overlap
# this would imply specifying the fill as a 1) category, 2) overlaps... tackle this visualization later
head(data)
richness <- boxplotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = richness_current, BDvariableTitle = "Current species richness" )
richness <- richness + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ende <- boxplotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = Ende_current, BDvariableTitle = "Current endemism")
ende <- ende + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
phyl <- boxplotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = phylodiversity_current, BDvariableTitle = "Current phylodiversity")
phyl <- phyl + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# to plot all together
currentBD_overlaps <- plot_grid(richness, 
                       ende, 
                       phyl, 
                       nrow = 1)
currentBD_overlaps
# write out
setwd(paste0(wdmain, "/output"))
png("Biodiversity&Tenure&overlaps_boxplots_v202408.png", width = 4000, height = 1700, units = "px", res = 300)
currentBD_overlaps
dev.off()

# LOSSES
richness <- boxplotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = rLoss_prop*100, BDvariableTitle = "Loss in species richness (%)" )
richness <- richness + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ende <- boxplotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = eLoss_prop*100, BDvariableTitle = "Loss in endemism (%)")
ende <- ende + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
phyl <- boxplotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = pLoss_prop*100, BDvariableTitle = "Loss in phylodiversity (%)")
phyl <- phyl + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# to plot all together
lossBD_overlaps <- plot_grid(richness, 
                                ende, 
                                phyl, 
                                nrow = 1)
lossBD_overlaps
# write out
setwd(paste0(wdmain, "/output"))
png("Biodiversity&Tenure&overlaps_losses_boxplots_v202408.png", width = 4000, height = 1700, units = "px", res = 300)
lossBD_overlaps
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

