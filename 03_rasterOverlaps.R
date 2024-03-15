# Map tenure overlaps using rasters ----

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
library(dplyr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# get data ----
tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")

# get tenure rasters:
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- grep(".tif$", list.files())
t <- lapply(list.files()[l], rast) # very weird, changing behavior with lists from terra
# terra::plot(t)
names(t) <- gsub("landTenure_", "", gsub("_SAalbers_1km.tif","", list.files()[l]))

# add biomes 
biomes <- read_biomes(year=2019)
biomes <- biomes[-7,]$geom
biomes <- st_transform(biomes, crs = crs(mask, proj = T))
plot(biomes)
v <- vect(biomes)

# overlap all the possible combinations ----
names(t) # the number of categories to map

# NOTE: IF THERE IS A CHANGE IN THE AMOUNT OF DATA THE CODE BELOW NEEDS CAREFUL MANUAL ATTENTION TO ENSURE ALL COMBINATIONS AND NAMES MAKE SENSE
overlaps <- list()
for(i in 1:(length(t)-1))
{
  overlaps[[i]] <- terra::intersect(t[[i]], t[[i+1]]) # 1+2, 2+3, 3+4, 4+5 BUT ALSO NEED
}
length2 <- length(overlaps)
for(i in length2:6)
{
  overlaps[[i+1]] <- terra::intersect(t[[1]], t[[i-1]]) # 1+3 and 1+4 and 1+5 Now missing 
}
overlaps[[8]] <- terra::intersect(t[[2]], t[[4]]) # 2+4 
overlaps[[9]] <- terra::intersect(t[[2]], t[[5]]) # 2+5
overlaps[[10]] <- terra::intersect(t[[3]], t[[5]]) # 3+5
names(t)

names(overlaps) <- c("indigenous-IRU", "IRU-PAs", "PAS-ruralSettlements", "ruralSettlements-undesignated",
                     "indigenous-PAs", "indigenous-ruralSettlements", "indigenous-undesignated",
                     "IRU-ruralSettlements", "IRU-undesignated", "PAs-undesignated")

# join using mosaic in order to see overall overlaps
# myOverlaps <- mosaic(sprc(overlaps), fun = sum)
# plot(myOverlaps) 

# However, what is more important is to identify which specific layers overlap

tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")

# PLOT THE OVERLAPS OF PRIVATE ON PUBLIC LANDS ----
par(mfrow = c(1,1))

terra::plot(overlaps$`indigenous-IRU`,
            col = c("transparent", "#E78AC3"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            plg = list(legend = c("", "Rural properties in indigenous lands"), x = "bottomright"))

terra::plot(overlaps$`IRU-undesignated`,
     col = c("transparent", "#1d6c7d"),
     type = "classes",
     mar=NA,
     box = F,
     axes = F,
     add=T,
     plg = list(legend = c("", "Rural properties in undesignated lands"), x = "bottomright"))

terra::plot(overlaps$`IRU-PAs`,
            col = c("transparent", "#1B9E77"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("", "Rural properties in PAs"), x = "bottomright"))

terra::plot(overlaps$`IRU-ruralSettlements`,
            col = c("transparent", "#FC8D62"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("", "Rural properties in rural settlements"), x = "bottomright"))
terra::lines(v, lwd=.1)

dev.off()

# PLOT THE OVERLAPS OF PUBLIC ON PUBLIC ----
tenureColors = c("#FC8D62", "#8DA0CB", "#8C7E5B", "#1B9E77", "#E78AC3", "#FFD700", "#1d6c7d") #, "#F0F0F0")
par(mfrow = c(1,1))
terra::plot(overlaps$`indigenous-PAs`,
            col = c("transparent", "#E78AC3"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            plg = list(legend = c("Indigenous lands in PAs", " "), x = "bottomright"))

terra::plot(overlaps$`PAS-ruralSettlements`,
            col = c("transparent", "#1B9E77"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("Rural settlements in PAs", " "), x = "bottomright"))

terra::plot(overlaps$`ruralSettlements-undesignated`,
            col = c("transparent", "#FC8D62"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("Rural settlements in undesignated", " "), x = "bottomright"))

terra::plot(overlaps$`indigenous-ruralSettlements`,
            col = c("transparent", "#F28C93"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("Rural settlements in indigenous", " "), x = "bottomright"))

terra::plot(overlaps$`PAs-undesignated`,
            col = c("transparent", "#1d6c7d"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("Rural settlements in PAs", " "), x = "bottomright"))

terra::plot(overlaps$`indigenous-undesignated`,
            col = c("transparent", "#E78AC3"),
            type = "classes",
            mar=NA,
            box = F,
            axes = F,
            add=T,
            plg = list(legend = c("Rural settlements in PAs", " "), x = "bottomright"))
terra::lines(v, lwd=.1)
dev.off()

# PRIVATE ON PRIVATE? would this be figuring out the snci and sigef data?



# Quantify these overlaps in amounts of km 
count <- data.frame("tenure" = c(names(overlaps)),
                    "non-overlaps" = 0, 
                    "overlaps" = 0)
for(i in 1:length(overlaps))
{
  table <- table(values(overlaps[[i]]))
  count[i,2] <- as.numeric(table[1])
  count[i,3] <- as.numeric(table[2])
  
}
count
count <- arrange(count, desc(overlaps))
count$tenure <- factor(count$tenure, levels = count$tenure)

# visualize as a barplot:
ggplot(count, aes(tenure, overlaps, fill = tenure)) +
  geom_bar(stat = "identity") +
  labs(title = " Overlaps in categories (in km2)") +
  theme(panel.background = element_rect(fill = "transparent"), legend.position = "inside")

# next step: get the areas of all the individual categories in order to calculate percentages