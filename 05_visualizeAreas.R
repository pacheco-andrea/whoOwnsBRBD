#### Biodiversity and Tenure in Brazil ####

# script that visualizes biodiversity indicators per tenure categ

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# i can start with bar and boxplots - which will likely show outliers
# in a second step need to count the deficit data 

# read data on biodiversity extractions (land tenure + biodiversity) ----
setwd(paste0(wdmain, "/data/processed/bdExtractions-perPolygon"))
l <- list.files()
shps <- grep(".shp", l)
l[shps]

tables <- list()
for(i in 4:8)
{
  tables[[i]] <- st_read(l[shps][i]) # read in just the information without the geometry
  tables[[i]] <- st_drop_geometry(tables[[i]])
  print(colnames(tables[[i]]))
}

test <- do.call(rbind, tables)
head(test)

# filter out polygons that are < 1km in area - done in previous script

# figure out the NAs (places where there was no value extracted)
summary(test)
test[which(is.na(test$mn_Sp_R)),]

# who owns BRBD boxplot ----
ggplot(test, aes(x = LTcateg, y = mn_Sp_R, color = LTcateg)) +
  geom_boxplot()



# last version could be useful for the raster overlaps
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




