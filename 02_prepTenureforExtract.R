#### Biodiversity and Tenure in Brazil ####

# this script prepare land tenure data for extractions
# i need to create:
# a variable which tells me whether each analyzed polygon overlaps with some other category 
  # what the overlap is
  # the amount of overlap 
# an easily splittable dataset that can be sent to the extractions (should i split the CAR by states?)

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# get tenure data
setwd(paste0(wdmain, "data/processed/"))
l <- list.files()
tenDataFolds <- l[grep("landTenure_", l)]

# should join...


# ok so to actually conduct the prioritization of one categ over the other, it would be through a spatial join
# https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-join.html#case-2-polygons-target-vs-points-source

# testing/learning:
#--- create polygons ---#
polygon_1 <- st_polygon(list(
  rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)) 
))

polygon_2 <- st_polygon(list(
  rbind(c(0.5, 1.5), c(0.5, 3.5), c(2.5, 3.5), c(2.5, 1.5), c(0.5, 1.5)) 
))

polygon_3 <- st_polygon(list(
  rbind(c(0.5, 2.5), c(0.5, 3.2), c(2.3, 3.2), c(2, 2), c(0.5, 2.5)) 
))

#--- combine the polygons to make an sf of polygons ---#
(
  polygons <- list(polygon_1, polygon_2, polygon_3) %>% 
    st_sfc() %>% 
    st_as_sf() %>% 
    mutate(polygon_name = c("polygon 1", "polygon 2", "polygon 3"))
)

ggplot() +
  geom_sf(data = polygons, aes(fill = polygon_name), alpha = .3) 

st_intersects(polygons, polygons) # whether the intersect exists: either by touching or sharing area
st_intersects(polygon_1, polygon_3) # these only touch the corners


# let me try subsetting indigenous that are inside PAs
uc <- st_read("landTenure_UC/landTenure_ConservationUnits_201909_SAalbers.shp")
ind <- st_read("landTenure_IPLC/landTenure_IPLCS_202103_SAalbers.shp")
ind <- ind[which(ind$fase_ti == "Regularizada" | ind$fase_ti == "Homologada"),]
overlap <- st_intersection(uc,ind)
plot(overlap$geometry)

# so I just need to do a series of st_intersections





#for(i in 1:length(ten))