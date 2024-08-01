#### Biodiversity and Tenure in Brazil ####

# this script was the origin of me carrying out these complicated series of polygon intersections across categories 
# i think just because it was the most accurate way to get the overlaps across polygon-based data (and keeping the individual property data) 

# what should it do now?
# i need a script that puts together all the diff datasets that i've produced/preprocessed
# maybe this should be a script that takes stock of the diff processed datasets and provides somem summary?
# KEEP IN MIND: THE WHOLE POINT OF THIS WAS TO TREAT THE OVERLAPS AS SEPARATE CATEGORIES
# THIS JUSTIFIES ALL THE MESS OF DATA CLEANING I HAD TO DO

# libraries
library(terra)
library(ggplot2)
library(sf)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# get tenure data
setwd(paste0(wdmain, "data/processed/"))
l <- list.files()

# get biodiversity data

# are these the data sets that i use from now on?
# if i consider the overaps a separate category to conduct extractions on - then YES. 
tenDataFolds <- l[grep("LT_", l)]

# extractions Public datasets: ----
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
l <- list.files() 
shps <- grep(".shp", l)

for(i in 1:length(shps))
{
  tenureData <- st_read(l[shps[i]])
  
}


# extractions Private datasets: ----


# THINK THROUGH WHAT IS THE MOST EFFICIENT WAY TO DO THIS - IN PARTICULAR FOR THE RURAL PROPERTIES