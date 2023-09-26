#### Biodiversity and Tenure in Brazil ####
# script that reads ubirajara's high biodiversity data
# and the new version of Imaflora's land tenure data set
# output is simplified column versions of land tenure
# and raster versions of the land tenure data in the projection of the biodiv data
# author: Andrea Pacheco
# first run: 23.09.2022
# second run: 25.09.2023

# libraries
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(exactextractr)
library(cowplot)
library(rasterVis)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"
#rasterOptions(tmpdir = "N:/eslu/priv/pacheco/biodivTenureBR/tmp", chunksize = 524288, maxmemory = 134217728)
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


# NEW DATA PENDING biodiversity_conservation dataset from ubirajara ----
setwd(paste0(wdmain,"/data/raw/"))
#unzip("biodiveristy_conservation.zip", exdir = paste0(wdmain, "data/raw/BD")) # this was necessary upon first version of data
list.files()
brc <- rast("Biodiversity_model/Model_biodiverisity.tif")
brc
plot(brc)
res(brc) # 500*500 m? - which means at 1km, and it's already in albers equal area
unique(brc) # the definitions for 8 categs are found in the metadata here: https://csr.ufmg.br/bioconservation/#


# NEW DATA FROM AMANDA PENDNG tenure data ----
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

# upon first run, read files and write out all of these into one folder to simplify future extractions through looping:
for (i in 1:length(myfiles))
{
  setwd(paste0(wdmain,"/data/raw/pa_br_LandTernure.v202105/", myfiles[i],"/"))
  shps <- grep(".shp", list.files())
  # read in all the shapefiles (with only specific columns established in the read_my_json funct above), bind rows into one df
  
  # region <- do.call(rbind, lapply(list.files()[shps], read_my_json))
  # region <- read_my_json(list.files()[shps][1])
  
  # creo q hay q cambiar esto y hacerlo por estado para que la extracción sea más rápida y paralelizable
  for(j in 1:length(shps))
  {
    setwd(paste0(wdmain,"/data/raw/pa_br_LandTernure.v202105/", myfiles[i],"/"))
    state_file <- read_my_json(list.files()[shps][j])
    nameOfStateFile <- gsub(".shp", "", list.files()[shps][j]) # this name will prob be different with amanda's version anyway
    setwd(paste0(wdmain,"/data/processed/landTenureCategs/"))
    st_write(state_file, dsn = paste0(nameOfStateFile, ".geojson"), layer = paste0(nameOfStateFile, ".geojson"))
  }
  print(i)
}

# test extractions of biodiv data per property/state
setwd(paste0(wdmain,"/data/processed/landTenureCategs/"))
l <- list.files()
brc

for(i in 1:length(l))
{
  setwd(paste0(wdmain,"/data/processed/landTenureCategs/"))
  f <- st_read(list.files()[i])
  nameOfStateFile <- list.files()[i]
  # e <- f[1:100,] # as a test
  e <- st_transform(f, crs = crs(brc)) # convert to same projection for extraction to work
  # ext <- terra::extract(brc, e, fun=NULL) # function from terra package
  # ok, getting an error here which i think may be because of empty geometries. hence, i might need to run a filtering such as:
  # dplyr::filter(st_is(sf_object,c("POLYGON","MULTIPOLYGON")))
  
  ext <- exact_extract(brc, e) # other separate function which works excellent! it is able to give a list of tables for each polygon with the raster value and the coverage fraction of it!
  # apparently value default is the max of the intersecting cells. so this should be specified if raster is categorical. but they will be continuous in the near future, so i think the following works:
  
  # the manual way of doing the above:
  # ext <- exact_extract(brc, e)
  # bind values to id of property 
  # val_combined <- bind_rows(ext2, .id = "id")
  # val_by_id <- val_combined %>%
  #   mutate(id = as.numeric(id)) %>%
  #   group_by(id) %>%
  #   summarize(val = sum(value * coverage_fraction)/sum(coverage_fraction))
  
  e$brc <- ext # add column to sf
  # head(e)
  # plot(e["brc"])
  setwd(paste0(wdmain,"/data/processed/bdExtractions-perPolygon/"))
  st_write(e, dsn = paste0(nameOfStateFile), layer = paste0(nameOfStateFile))
}

# rasterize tenure data ----
# THIS SHOULD BASICALLY BE FOR MAPS - NOT FOR CALCULATIONS!
# make mask
mask <- brc*0
# rasterize each region
for(i in 1:length(myfiles))
{
  setwd(paste0(wdmain,"/data/processed/landTenureCategs/"))
  # this is a list of shapefiles for each region in Brazil
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
  mask2 <- crop(mask, ext(region))
  # rasterize
  region2 <- rasterize(region, mask2, field = "tenureCateg")
  # plot(region2)
  # write out raster file
  setwd(paste0(wdmain,"/data/processed/landTenureCategsRaster/"))
  terra::writeRaster(region2, paste0(myfiles[i], ".tif", overwrite=TRUE))
}

# next script: extract biodiversity values per property


