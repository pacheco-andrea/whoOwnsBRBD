#### Biodiversity and Tenure in Brazil ####

# this script:
# bring together all the diff polygon datasets that i've preprocessed, takes stock of these and:
# 1) summarizes their area in a table
# 2) intersects the areas of overlap across public and private categories

# note, here, does this mean that i write out the polygons as a separate dataset?


# 3) writes them out in a file format this is easy to handle for further BD extractions
# KEEP IN MIND: THE WHOLE POINT OF THIS WAS TO TREAT THE OVERLAPS AS SEPARATE CATEGORIES
# (which was the point of all the public and private preprocessing i did in step 02 so the folder structure should reflect these distinct categories)

# libraries
library(sf)
library(terra)
library(ggplot2)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")
rm(mask)

# preprocessed tenure data
setwd(paste0(wdmain, "data/processed/"))
l <- list.files()
l[grep("LT_", l)]

list.files()
# A) area calculations ----

# Public lands: no overlaps (LT_no-overlaps) ----
setwd(paste0(wdmain, "data/processed/LT_no-overlaps"))
l <- list.files()
shps <- l[grep(".shp", l)]
# create data frame 
df_noOverlapsPublic <- data.frame(categ = gsub(".shp", "", shps), areakm2 = NA)
for(i in 1:length(shps))
{
  # read shape
  s <- st_read(shps[i])
  # calculate area
  a <- st_area(s)
  # place in data.frame and divide to convert to km2
  df_noOverlapsPublic[i,"areakm2"] <- sum(a)/1000000
}
df_noOverlapsPublic

# Private lands: no overlaps (LT_no-overlaps_private) -----
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private"))
l <- list.files()
shps <- l[grep(".shp", l)]
df_noOverlapsPrivate <- data.frame(categ = gsub(".shp", "", shps), areakm2 = NA)
# create data frame 
for(i in 1:length(shps))
{
  # read shape
  s <- st_read(shps[i])
  # calculate area
  a <- st_area(s)
  # place in data.frame and divide to convert to km2
  df_noOverlapsPrivate[i,"areakm2"] <- sum(a)/1000000
}
df_noOverlapsPrivate

df_noOverlaps <- rbind(df_noOverlapsPublic, df_noOverlapsPrivate)
sum(df_noOverlaps$areakm2) # yes, 7,932,964 km2 seems right, as all BR is ~8,500,000 km2
# quick visualization:
ggplot(df_noOverlaps, aes(x=categ, y=areakm2)) +
  geom_bar(stat = "identity")
rm(s)
gc()

# Overlaps across categories (but within public and private lands) LT_overlaps ----
setwd(paste0(wdmain, "data/processed/LT_overlaps"))
l <- list.files()

# self overlaps
self <- l[grep("selfOverlaps.shp", l)]

# make matrix for results
df_self <- matrix(nrow = 1, ncol = length(self))
colnames(df_self) <- gsub("_selfOverlaps.shp", "", self)
df_self <- as.data.frame(df_self)
# loop for calcs
for(i in 1:length(self))
{
  s <- st_read(l[grep("selfOverlaps.shp", l)][i])
  a <- st_area(s)
  df_self[1,i] <- sum(a)/1000000
}
df_self

# across categories
across_shps <- l[-grep("selfOverlaps", l)]
across_shps <- across_shps[grep(".shp", across_shps)]
df_across <- matrix(nrow = 1, ncol = length(across_shps))
colnames(df_across) <- gsub(".shp", "", across_shps)
df_across <- as.data.frame(df_across)
# loop calcs
for(i in 1:length(across_shps))
{
  s <- st_read(across_shps[i])
  a <- st_area(s)
  df_across[1,i] <- sum(a)/1000000
}
df_across

rm(s)
# PUT INDIVIDUAL DATA FRAMES TOGETHER HERE!
df_noOverlaps
df_self
df_across

# place in right place:
df <- df_noOverlaps
for(i in 1:length(df_self))
{
  # get column name: needs to start with the df_self rather than the total categs
  newCol <- colnames(df_self)[i]
  # creates the new column
  df[[newCol]] <- NA
  # it needs to find the place in the df$categ
  colPlace <- grep(paste0(newCol), df$categ)
  df[colPlace,2+i] <- df_self[,grep(newCol, colnames(df_self))]
}
df_across
# for(i in 2:length(df_across)) # note, starting at 2, going to skip the ind-PA-rural-sett overlap bc that would be a 3-dimensional table
# {
#   nameOverlap <- colnames(df_across)[i]
#   rowPlace <- grep(df_across[i])
# }
# OK - adding the df_across gets quite complicated, and unnecessary energy right now... so i just inputted these manually in an excel table
# just remember the important ones are 1) PAs-indigenous and 2) sustUse PAs and rural settlements
setwd(paste0(wdmain, "output/"))
write.csv(df, "OverlapsAcrossCategs_20240806.csv", row.names = F)

# Overlaps ACROSS PUBLIC AND PRIVATE categories ----

# create lists of public and private files i want to overlap
setwd(paste0(wdmain, "data/processed/LT_no-overlaps/"))
l <- list.files()
pubList <- l[grep(".shp", l)]

setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private/"))
l <- list.files()
priList <- l[grep(".shp", l)]

# make matrix for results
overlap_matrix <- matrix(FALSE, nrow = length(priList), ncol = length(pubList))
rownames(overlap_matrix) <- gsub(".shp", "", priList)
colnames(overlap_matrix) <- gsub(".shp", "", pubList)
overlap_matrix  


# loop through each pair to intersect
for(i in 1:length(priList)) 
{
  # start with private data so that it doesn't have to read the rural properties 4 times
  # get private data:
  setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private/"))
  priData <- st_read(priList[i])
  priData <- st_transform(priData, my_crs_SAaea)

  for(j in 1:length(pubList))
  {
    # get public data:
    setwd(paste0(wdmain, "data/processed/LT_no-overlaps/"))
    pubData <- st_read(pubList[j])
    pubData <- st_transform(pubData, my_crs_SAaea)
    
    # fix bug in strict PAs
    if(pubList[j] == "PA_strict.shp"){
      pubData <- st_make_valid(pubData)
    }
    
    # intersection
    overlap <- st_intersection(priData, pubData)
    print(overlap)
    a <- st_area(overlap)
    
    # place in matrix
    overlap_matrix[i,j] <- sum(a)/1000000
    print(overlap_matrix)
    
    # depending on how much they overlap, i can decide whether it's worth actually analyzing the overlap
    # it turns out the overlaps between IRU and 1) PA_strict, 2) PA_sustuse, 3) undesignated are so significant
    # i need to write these out in the folder LT_pubxpri_overlaps
    
    if(priList[i] == "ruralProperties.shp" & pubList[j] == "PA_strict.shp" | pubList[j] == "PA_sustuse.shp"| pubList[j] == "undesignated.shp"){
      # unique(st_geometry_type(overlap))
      print(overlap)
      extract.pgeometries <- st_collection_extract(overlap, "POLYGON")
      overlap <- extract.pgeometries %>% group_by(LTcateg, id, LTcateg.1, id.1) %>% summarize(geometry = st_union(geometry)) %>% st_as_sf()
      print(overlap)
      
      setwd(paste0(wdmain, "data/processed/LT_pubxpri_overlaps"))
      st_write(overlap, paste0("ruralProperties-", pubList[j]), append = F)
      
    }
  }
}

setwd(paste0(wdmain, "output/"))
write.csv(as.data.frame(overlap_matrix), "overlapMatrix-Pri-Pub.csv")
rm(priData)

# missing comparisons:
# private PAs x quilombos
setwd(paste0(wdmain, "data/processed/LT_no-overlaps_private/"))
priPAS <- st_read(priList[1])
qui <- st_read(priList[2])
# intersection
overlap <- st_intersection(priPAS, qui)
print(overlap)
a <- st_area(overlap)
sum(a)/1000000
# rural settlements x indigenous, x PA_strict - this was already done in previous script 01 preprocess public




# B) folder structuring for extractions??? ----

