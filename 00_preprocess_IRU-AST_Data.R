#### Biodiversity and Tenure in Brazil ####

# Rural properties and rural settlements # 

# this script brings together the pre-processed tenure data and
# 1) cleans and fixes self-overlaps of data
# 2) identifies the overlapping areas across datasets: NOTE, should do this one at a time in order to keep track of overlaps
# 3) creates the categories for these overlaps themselves

# this creates the outputs:

# 1) one version with no overlaps for rasterizing (or rather, with overlaps as one category)
# 2) one version with overlaps for extracting 


# libraries
library(terra)
library(sf)
library(geos)
library(dplyr)
library(ggplot2)
library(tidyverse)


# load directories and other settings
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# Land tenure data folders ----
setwd(paste0(wdmain,"/data/processed/"))
