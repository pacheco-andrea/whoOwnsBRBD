# Biodiversity indicator data ----
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
list.files()
richness <- rast("Species_Richness_All_groups_MARS.tif")
richness
plot(richness)
res(richness) # .01*.01degrees - which means at ~1km. Now it's in WGS84. 

# transform to south america albers equal area for making calculations 
my_crs <- terra::crs(stateShp, proj = TRUE)
# richness_eq <- project(richness, my_crs)
# plot(richness_eq)
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
rastFiles <- grep(".tif$", list.files())
list.files()[rastFiles]
for(i in 1:length(rastFiles))
{
  setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009"))
  r <- rast(paste0(list.files()[rastFiles][i]))
  r2 <- project(r, my_crs)
  name <- gsub(".tif", "_SAalbers.tif", paste0(list.files()[rastFiles][i]))
  print(i)
  setwd(paste0(wdmain,"/data/processed/BiodivIndicators_SA-albers"))
  writeRaster(r2, filename = name)
}
