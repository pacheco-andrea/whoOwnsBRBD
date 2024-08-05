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
setwd(paste0(wdmain, "output/maps/"))
svg("ruralPropertiesOverlappingPublicLands.svg", width = 7, height = 7)

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

setwd(paste0(wdmain, "output/maps/"))
svg("PublicLandsOverlappingPublicLands.svg", width = 7, height = 7)

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

# i think there are basically no overlaps in the two categories below - but should double check

# terra::plot(overlaps$`PAs-undesignated`, 
#             col = c("transparent", "#1d6c7d",),
#             type = "classes",
#             mar=NA,
#             box = F,
#             axes = F,
#             add=T,
#             plg = list(legend = c("Rural settlements in PAs", " "), x = "top"))

# terra::plot(overlaps$`indigenous-undesignated`,
#             col = c("transparent", "#E78AC3"),
#             type = "classes",
#             mar=NA,
#             box = F,
#             axes = F,
#             add=T,
#             plg = list(legend = c("Rural settlements in PAs", " "), x = "bottomright"))
terra::lines(v, lwd=.1)
dev.off()


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

# VISUALIZE OVERLAPS VERSION 2: ----

# read 30m2 rasterizations of data
setwd(paste0(wdmain,"/data/processed/raster_landTenureCategs/"))
l <- list.files()
r <- grep("30m.tif$", l)
l[r]

tenureRast <- lapply(l[r], rast)
names(tenureRast) <- gsub("_SAalbers_30m.tif", "", l[r])
names(tenureRast) <- gsub("_30m.tif", "", names(tenureRast))
names(tenureRast) 

# i want to check:
#1) whether the problem is just the RPPNs 
#2) whether the problem is the IRU

tenureRast$private_protectedAreas <- NULL

# tests outside of this messy loop
# make categorical rast a numerical one:
ast <- tenureRast$ruralSettlements 
ast <- as.numeric(ast)

ast <- terra::subst(tenureRast$ruralSettlements, from = "AST", to = 10)

par(mfrow = c(1,2))
plot(ast)
plot(tenureRast$ruralSettlements, col = "green")
dev.off()

# sum ast + iru
ast_iru <- ast + tenureRast$ruralProperties

test <- mosaic(ast, tenureRast$ruralProperties, fun = "sum")

# create output file
setwd(paste0(wdmain, "output/"))
output_file <- "30mraster_overlap_results.txt"
writeLines("Raster Overlap Results\n", con = output_file)

# identify overlaps
overlap_matrix <- matrix(FALSE, nrow = length(tenureRast), ncol = length(tenureRast))
rownames(overlap_matrix) <- colnames(overlap_matrix) <- names(tenureRast)

# write function to calculate the area in km2 
calculate_area <- function(raster, resolution) {
  cell_area_m2 <- resolution^2
  total_area_km2 <- sum(!is.na(values(raster)))*(cell_area_m2 /1e6)
  return(total_area_km2)
}
i=4
j=5
# loop through each pair of rasters and check for overlaps
for (i in 1:(length(tenureRast) - 1)) {
  for (j in (i + 1):length(tenureRast)) {
    result <- try({
      # 1. Check for overlap using logical comparison
      print(tenureRast[[i]])
      print(tenureRast[[j]])
      # overlap <- !is.na(tenureRast[[i]]) & !is.na(tenureRast[[j]]) # this never works, computationally
      # has_overlap <- any(values(overlap), na.rm = TRUE)

      if (has_overlap) {
        # 2. If there is an overlap, run an intersection
        overlap_raster <- terra::intersect(tenureRast[[i]], tenureRast[[j]])

        # 3. Quantify the area of the intersection in km²
        resolution <- res(tenureRast[[i]])[1]  # Assume square pixels, resolution in meters
        overlap_area_km2 <- calculate_area(overlap_raster, resolution)

        # Calculate total areas of the rasters
        area_raster_i <- calculate_area(tenureRast[[i]], resolution)
        area_raster_j <- calculate_area(tenureRast[[j]], resolution)

        # Calculate percentage of the total area
        overlap_percent_i <- (overlap_area_km2 / area_raster_i) * 100
        overlap_percent_j <- (overlap_area_km2 / area_raster_j) * 100

        # Record the overlap information in the matrix (optional)
        overlap_matrix[i, j] <- TRUE
        overlap_matrix[j, i] <- TRUE  # Symmetric matrix


      # Write the result to the file
      writeLines(paste("Overlap between", names(tenureRast)[i], "and", names(tenureRast)[j], ":\n",
                       " - Overlap Area: ", overlap_area_km2, "km²\n",
                       " - Overlap as % of Raster", names(tenureRast)[i], ": ", overlap_percent_i, "%\n",
                       " - Overlap as % of Raster", names(tenureRast)[j], ": ", overlap_percent_j, "%\n"),
                 con = output_file, append = TRUE)
      }
    }, silent = TRUE)
    
    # If an error occurs, print a message and move on
    if (inherits(result, "try-error")) {
      message(paste("Error occurred while processing rasters:", names(tenureRast)[i], "and", names(tenureRast)[j]),
              con = output_file, append = TRUE)
    }
  }
}

overlap_matrix

# terra::expanse should calculate the area of the rasters