#### ares with most biomass/non-food biomass exports ####
# goals: 
# a script that reads comex export data
# pairs this with spatial data to id the municipalities under most "threat" from trade

# first run: January 23, 2023

#libraries:
library(dplyr)
library(sf)
library(geobr)
install.packages("geobr")

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# get soy export data ----
setwd(paste0(wdmain, "/data/processed/soyExports"))
myComex <- read.csv("comex-biomassExports_1997-2022.csv")
soyplace <- grep("soy", myComex$name, ignore.case = TRUE)
myComexSoy <- myComex[soyplace,]

woodplace <- grep("wood", myComex$name, ignore.case = TRUE)
unique(myComex[woodplace,]$name)

pulpplace <- grep("pulp", myComex$name, ignore.case = TRUE)
unique(myComex[pulpplace,]$name)

# get municipalities data from the IBGE package ----
mun <- read_municipality(code_muni = "all", year = 2020)
nrow(mun)
ggplot(mun)+
  geom_sf()
mun$CO_MUN <- mun$code_muni

# join data ----
soymunis <- inner_join(myComexSoy, mun, by = "CO_MUN")
soymunis2 <- soymunis[which(soymunis$CO_ANO == 2020),]

# plot
# soy 2020
ggplot(soymunis)+
  geom_sf(fill = "gray90", color = "transparent", aes(geometry = geom))+
  geom_sf(data = soymunis[which(soymunis$CO_ANO == 2020),], 
          aes(fill = KG_LIQUIDO, geometry = geom),
          color = "transparent")
