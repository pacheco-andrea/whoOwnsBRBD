#### Biodiversity and Tenure in Brazil ####

# script that incroporates the Forest Code compliance (surplus or deficit of native vegetation) to our tenure + biodiversity data

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(cowplot)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")


# visualize forest surplus/deficit + biodiversity ----

# get data
setwd(paste0(wdmain, "data/processed/"))
data_extra <- read.csv("finalDataset_Tenure-BD-CSR.csv")
head(data_extra)

# filter data for which we have the FC surplus/deficit information
FC_data <- data_extra[which(!is.na(data_extra$area_conv)),]
summary(FC_data)

# make data long format
FCdata_long <- FC_data %>%
  pivot_longer(
    cols = -c(LTcateg, id, uf, LTcateg2, myOverCat),
    names_to = "Vars",
    values_to = "value"
  )
head(FCdata_long)

# maybe this is too many variables?
# start with richness and endemism
# veg deficit and surplus

FC_data2 <- select(FCdata, c(id, LTcateg2, myOverCat, mean.SpeciesRichness, mean.Weight_Endemism, rl_ativo, rl_def, app_def))
FC_data2_long <- FC_data2 %>%
  pivot_longer(
    cols = -c(id, LTcateg2, myOverCat)
    names_to = "Vars",
    values_to = "value"
  )

# make heatmap
heatmap <- ggplot(FCdata_long, aes(x = Vars, y = LTcateg2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_minimal()
heatmap
