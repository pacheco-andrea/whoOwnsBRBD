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
myColumns <- c("LTcateg", "id", "areakm2", "mean.phylodiversity_current", "mean.phylodiversity_loss", 
               "mean.richness_current", "mean.richness_loss", "mean.Ende_current", "mean.Ende_loss")

tables <- lapply(names(tables), function(name){
  df <- tables[[name]]
  # select my columns
  df <- df[,myColumns, drop = F]
  # create column that identifies the overlap category
  df$myOverCat <- name
  return(df)
})
data <- do.call(rbind, tables)
colnames(data) 
colnames(data) <- gsub("mean.", "", colnames(data))

# data cleaning and organizing ----
summary(data)
unique(data$LTcateg) 
unique(data$myOverCat)


# filter out polygons that are < .5 km2 in area - the resolution of the BD data
data.5km <- data[which(data$areakm2 <= .5),] 
# quick visualization
ggplot(data.5km, aes(x = LTcateg)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
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

# richness ----
richness <- plotBD(data, tenureCategory = LTcateg2, BDvariable = richness_current)
richness
richness_loss <- plotBD(data, tenureCategory = LTcateg2, BDvariable = richness_loss)
richness_loss

richness_overlaps <- plotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = richness_current)
richness_overlaps <- richness_overlaps + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                               legend.position = "none", legend.title = element_blank()) # clearly, some issues with the colors plotted
r <- plot_grid(richness, richness_overlaps)

# endemism
ende <- plotBD(data, tenureCategory = LTcateg2, BDvariable = Ende_current)
ende
ende_overlaps <- plotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = Ende_current)
ende_overlaps <- ende_overlaps + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                       legend.position = "none", legend.title = element_blank()) 
e <- plot_grid(ende, ende_overlaps)

# phylogenetic div
phyl <- plotBD(data, tenureCategory = LTcateg2, BDvariable = phylodiversity_current)
phyl
phyl_overlaps <- plotBD(dataOverlaps, tenureCategory = overlapsWith, BDvariable = phylodiversity_current)
phyl_overlaps <- phyl_overlaps+ theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                                      legend.position = "none", legend.title = element_blank()) 

p <- plot_grid(phyl, phyl_overlaps)

# # phylogenetic endemism
# phyl_ende <- plotBD(data, tenureCategory = LTcateg2, BDvariable = mean.Phylogenetic_Endemism)
# phyl_ende
# phyl_ende_overlaps <- plotBD(dataOverlaps, tenureCategory = myOverCat, BDvariable = mean.Phylogenetic_Endemism)
# phyl_ende_overlaps <- phyl_ende_overlaps + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
#                                                  legend.position = "none", legend.title = element_blank()) 
# 
# pe <- plot_grid(phyl_ende, phyl_ende_overlaps)

# plot all together
# plot_grid (r, e, p, pe, nrow = 4)
plot_grid (r, e, p, nrow = 3)


# write out ----
# plot
setwd(paste0(wdmain, "/output"))
svg("Biodiversity&Tenure&overlaps_boxplots_v202408.svg", width = 20, height = 15)
# plot_grid (r, e, p, pe, nrow = 4)
plot_grid (r, e, p, nrow = 3)
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

