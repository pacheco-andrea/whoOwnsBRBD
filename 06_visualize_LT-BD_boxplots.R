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
setwd(paste0(wdmain, "/data/processed/bdExtractions-perPolygon"))
l <- list.files()
tables <- lapply(l, read.csv)
lapply(tables, colnames)
names(tables) <- gsub(".csv","", l)

# the tables have different column names, need to make them consistent to join into one table
myColumns <- c("LTcateg", "id", "areakm2", "mean.Phylogenetic_Diversity", "mean.Phylogenetic_Endemism", "mean.Species_Richness", "mean.Weight_Endemism")
tables <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # select my columns
  df <- df[,myColumns, drop = F]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
data <- do.call(rbind, tables)

# data cleaning and organizing ----
summary(data)
unique(data$LTcateg) 
unique(data$myOverCat)


# filter out polygons that are < 1km in area - done in previous script
data_1km <- data[which(data$areakm2 < 1),]
summary(data_1km)
# quick visualization
ggplot(data_1km, aes(x = LTcateg)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# note: 6387509 IRU properties were <1km, out of the 7067703 total (= 90% of records)
6387509/7067703
# data <1km2 == data that i will use henceforth
data <- data[which(data$areakm2 >= 1),]

# filter out these undesignated categories which are a bit ambiguous - note other uses had lowest sp richness values - but what are these other uses?
data <- data[which(data$LTcateg != "OUTROS USOS"),]
data <- data[which(data$LTcateg != "USO MILITAR"),]


# figure out the NAs (places where there was no value extracted)
summary(data[which(is.na(data$mean.Species_Richness)),])
unique(data[which(is.na(data$mean.Species_Richness)),]$myOverCat)
ggplot(data[which(is.na(data$mean.Species_Richness)),], aes(x = myOverCat)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  coord_cartesian(ylim = c(0,50))
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

# PENDING: CLEAN UP THE OVERLAP CATEGORIES SOMEHOW?
table(dataOverlaps$myOverCat)
# maybe i should be creating a new column of overlaps
# 1) self
# 2) overlap is public-public
# 3) overlap is public-private
# 4) overlap is private-private

# BOXPLOTS ----
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
plotBD <- function(data, tenureCategory, BDvariable){
  plot <- ggplot(data, aes(x = {{tenureCategory}}, y = {{BDvariable}}, fill = LTcateg2)) +
    geom_boxplot() +
    scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill")) +
    theme(panel.background = element_blank(), legend.title = element_blank(), legend.position = "none")
  return(plot)
}

# isolate overlaps to plot these alongside the main boxplots
no <- grep("no-overlaps", data$myOverCat)
dataNoOverlaps <- data[no,]
dataOverlaps <- data[-no,]

# richness
richness <- plotBD(data, tenureCategory = LTcateg2, BDvariable = mean.Species_Richness)
richness

richness_noOverlaps <- plotBD(dataNoOverlaps, LTcateg2, mean.Species_Richness)
richness_noOverlaps
plot_grid(richness, richness_noOverlaps) # realized here that the parts that don't overlap are quite minimal and don't make a huge diff

richness_overlaps <- plotBD(dataOverlaps, tenureCategory = myOverCat, BDvariable = mean.Species_Richness)
richness_overlaps <- richness_overlaps + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                               legend.position = "none", legend.title = element_blank()) # clearly, some issues with the colors plotted
r <- plot_grid(richness, richness_overlaps)

# endemism
ende <- plotBD(data, tenureCategory = LTcateg2, BDvariable = mean.Weight_Endemism)
ende
ende_overlaps <- plotBD(dataOverlaps, tenureCategory = myOverCat, BDvariable = mean.Weight_Endemism)
ende_overlaps <- ende_overlaps + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                       legend.position = "none", legend.title = element_blank()) 
e <- plot_grid(ende, ende_overlaps)

# phylogenetic div
phyl <- plotBD(data, tenureCategory = LTcateg2, BDvariable = mean.Phylogenetic_Diversity)
phyl
phyl_overlaps <- plotBD(dataOverlaps, tenureCategory = myOverCat, BDvariable = mean.Phylogenetic_Diversity)
phyl_overlaps <- phyl_overlaps+ theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                      legend.position = "none", legend.title = element_blank()) 

p <- plot_grid(phyl, phyl_overlaps)

# phylogenetic endemism
phyl_ende <- plotBD(data, tenureCategory = LTcateg2, BDvariable = mean.Phylogenetic_Endemism)
phyl_ende
phyl_ende_overlaps <- plotBD(dataOverlaps, tenureCategory = myOverCat, BDvariable = mean.Phylogenetic_Endemism)
phyl_ende_overlaps <- phyl_ende_overlaps + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                                 legend.position = "none", legend.title = element_blank()) 

pe <- plot_grid(phyl_ende, phyl_ende_overlaps)

# plot all together
plot_grid (r, e, p, pe, nrow = 4)

# write out ----
# plot
setwd(paste0(wdmain, "/output"))
svg("Biodiversity&Tenure&overlaps_boxplots.svg", width = 20, height = 20)
plot_grid (r, e, p, pe, nrow = 4)
dev.off()


# combine this cleaned bd+tenure data with forest deficit information ----

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

