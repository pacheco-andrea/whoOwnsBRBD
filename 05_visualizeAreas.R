#### Biodiversity and Tenure in Brazil ####

# script that visualizes biodiversity indicators per tenure categ

library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# i can start with bar and boxplots - which will likely show outliers
# in a second step need to count the deficit data 

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

# make factors
data$LTcateg <- as.factor(data$LTcateg)
data$myOverCat <- as.factor(data$myOverCat)

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
levels(data$LTcateg)[levels(data$LTcateg) == "PI"] <- "PA_strict"
levels(data$LTcateg)[levels(data$LTcateg) == "US"] <- "PA_sustuse"
levels(data$LTcateg)[levels(data$LTcateg) == "RPPN"] <- "privatePAs"
levels(data$LTcateg)[levels(data$LTcateg) == "SEM DESTINACAO"] <- "undesignated"
levels(data$LTcateg)[levels(data$LTcateg) == "IRU"] <- "ruralProperties"
levels(data$LTcateg)[levels(data$LTcateg) == "AST"] <- "ruralSettlements"
unique(data$LTcateg)
# who owns BRBD boxplot ----
tenureColors <- c("indigenous" = "#E78AC3",
                  "non-overlapped" = "gray70",   
                  "PA_strict" = "#1B9E77",       
                  "PA_sustuse" =  "#8C7E5B",
                  "privatePAs"  = "#99d8c9",  
                  "quilombola" =  "#FFD700",
                  "ruralProperties" = "#8DA0CB",
                  "ruralSettlements" = "#FC8D62",
                  "undesignated" ="#1d6c7d")


richness <- ggplot(data, aes(x = LTcateg, y = mean.Species_Richness, fill = LTcateg)) +
  geom_boxplot() +
  scale_colour_manual(values = tenureColors, aesthetics = c("color", "fill"))
richness




# plot from before that could be useful
whoOwnsPlot <- ggplot(table2.2, aes(tenCateg, (sum/1000), fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,4000, by = 500))+
  ylab(bquote("Area in 1000 km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(5,5,5,5), "mm"),
        legend.position = "none", legend.title = element_blank())+ # probably REMOVE LEGEND
  coord_flip()
whoOwnsPlot 

setwd(paste0(wdmain, "/output"))
svg("whoOwnsBRBD_barplot.svg", width = 8.3, height = 2.5)
whoOwnsPlot
dev.off()

# write out information
setwd(paste0(wdmain, "/output/"))
write.csv(table2.2, "whoOwnsBR-BDinKm2.csv", row.names = FALSE)

# create subplots of percentages of total areas per category ----



myTenCols <- c("Other"= "#F0F0F0", 
               "PA sustainable use" = "#8C7E5B", 
               "PA strict protection" = "#1B9E77", 
               "Indigenous" = "#E78AC3",
               "Communal/Quilombo" = "#FFD700",
               "Undesignated" = "#1d6c7d",
               "Private lands" = "#8DA0CB",
               "Rural settlements" = "#FC8D62")




