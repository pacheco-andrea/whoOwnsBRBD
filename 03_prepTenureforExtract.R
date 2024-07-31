#### Biodiversity and Tenure in Brazil ####

# this script was the origin of me carrying out these complicated series of polygon intersections across categories 
# i think just because it was the most accurate way to get the overlaps across polygon-based data (and keeping the individual property data) 

# what should it do now?
# i need a script that puts together all the diff datasets that i've produced/preprocessed
# maybe this should be a script that takes stock of the diff processed datasets and provides somem summary?

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

# 