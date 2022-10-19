#### Biodiversity and Tenure in Brazil ####
# script that reads ubirajara's high biodiversity data
# and the new version of Imaflora's land tenure data set
# output is simplified column versions of land tenure
# and raster versions of the land tenure data in the projection of the biodiv data
# author: Andrea Pacheco
# first run: 23.09.2022

# libraries

# install.packages("raster")
# install.packages("rgdal")
# install.packages("sf")
# install.packages("fasterize")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("RPostgreSQL")

library(raster)
library(rgdal)
library(sf)
library(fasterize)
library(ggplot2)
library(dplyr)

wdmain <- "N:/eslu/priv/pacheco/biodivTenureBR"
rasterOptions(tmpdir = "N:/eslu/priv/pacheco/biodivTenureBR/tmp", chunksize = 524288, maxmemory = 134217728)
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# biodiversity_conservation dataset from ubirajara ----
setwd(paste0(wdmain,"/data/raw/"))
#unzip("biodiveristy_conservation.zip", exdir = paste0(wdmain, "data/raw/BD")) # this was necessary upon first version of data
list.files()
brc <- raster("Biodiversity_model/Model_biodiverisity.tif")
brc
plot(brc)
res(brc) # 500*500 m? - which means at 1km, and it's already in albers equal area
unique(brc) # the definitions for 8 categs are found in the metadata here: https://csr.ufmg.br/bioconservation/#

# tenure data ----
# new 2021 version is now split into a .shp for each state. very annoying
# unfortunately, I couldn't download directly from Dropbox using R because the files ended up corrupted and un-unzippable...?
# this meant i downloaded each folder manually here: (https://www.dropbox.com/sh/cvtrj35w6hzehhb/AAAHPVR4Sdk6jDXjUJH78TBja/MalhaFundiaria_LandTenure/MalhaFundiaria_LandTenure_v.202105/pa_br_LandTernure.v202105?dl=0&subfolder_nav_tracking=1)

setwd(paste0(wdmain,"/data/raw/pa_br_LandTernure.v202105"))

# steps for processing:
# choose specific columns to keep lighter versions of these data: gid, subclass, area, cd_mun, geometry

# get list of files i want to process
l <- list.files()
# f <- lapply(l[grep(".zip", l)], unzip) # only upon first run
x <- grep(".zip|.txt", list.files())
myfiles <- list.files()[-x]

# read and write out simpler files # HERE FIGURE OUT IF I SHOULD WRITE OUT AS JSON OR SQLITE????
# function for reading in only specific columns
read_my_json = function(f){
  s = st_read(f)
  return(s[,c("gid","sub_class", "area", "cd_mun")])
}

# upon first run, read files and write out simpler versions:
for (i in 1:length(myfiles))
{
  setwd(paste0(wdmain,"/data/raw/pa_br_LandTernure.v202105/", myfiles[i],"/"))
  shps <- grep(".shp", list.files())
  # read in all the shapefiles (with only specific columns established in the read_my_json funct above), bind rows into one df
  region <- do.call(rbind, lapply(list.files()[shps], read_my_json))
  # test <- read_my_json("es_sc_landternure.v202105_random_sirgas.shp")
  # keep info on region
  region$region <- as.character(paste0(myfiles[i]))
  setwd(paste0(wdmain,"/data/processed/landTenureCategs/"))
  # st_write(test, "test.shp")
  st_write(region, paste0(myfiles[i], ".shp"))
}

# rasterize tenure data ----

# make mask
mask <- brc*0
# rasterize each region
for(i in 1:length(myfiles))
{
  setwd(paste0(wdmain,"/data/processed/landTenureCategs/"))
  shps <- grep(".shp", list.files())
  region <- st_read(list.files()[shps[i]])
  # recat tenure categories to numerical
  region <- region %>% mutate(tenureCateg=recode(sub_class,
                                                      "AG"=1,
                                                      "ARU"=2,
                                                      "CARpr"=3,
                                                      "CARpo"=4,
                                                      "COM"=5,
                                                      "ML"=6,
                                                      "ND_B"=7,
                                                      "ND_I"=8,
                                                      "QL"=9,
                                                      "SIGEF"=10,
                                                      "TI_H"=11,
                                                      "TI_N"=12,
                                                      "TLPC"=13,
                                                      "TLPL"=14,
                                                      "TRANS"=15,
                                                      "UCPI"=16,
                                                      "UCUS"=17,
                                                      "URB"=18))
  # transform projection to match biodiversity mask
  region <- st_transform(region, crs = crs(mask))
  # crop mask to region 
  mask2 <- crop(mask, extent(region))
  # rasterize
  region2 <- fasterize(region, mask2, field = "tenureCateg")
  # write out raster file
  setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
  terra::writeRaster(region2, paste0(myfiles[i], ".tif", overwrite=TRUE))
}



