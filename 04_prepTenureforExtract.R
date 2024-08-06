#### Biodiversity and Tenure in Brazil ####

# this script was the origin of me carrying out these complicated series of polygon intersections across categories 
# i think  because it was the most accurate way to get the overlaps across polygon-based data (and keeping the individual property data) 

# this script should now:
# bring together all the diff datasets that i've preprocessed, takes stock of these and:
# 1) summarizes their area in a table
# 2) writes them out in a file format this is easy to handle for further BD extractions
# KEEP IN MIND: THE WHOLE POINT OF THIS WAS TO TREAT THE OVERLAPS AS SEPARATE CATEGORIES
# THIS JUSTIFIES ALL THE MESS OF DATA CLEANING I HAD TO DO - so the folder structure should reflect these distinct categories

# libraries
library(sf)
library(terra)
library(ggplot2)
library(geobr)
source("N:/eslu/priv/pacheco/whoOwnsBRBD/code/000_gettingStarted.R")

# preprocessed tenure data
setwd(paste0(wdmain, "data/processed/"))
l <- list.files()
l[grep("LT_", l)]

list.files()
# A) area calculations ----
# create a matrix for these calculations:
overlap_matrix <- matrix(FALSE, nrow = length(tenureRast), ncol = length(tenureRast))
rownames(overlap_matrix) <- colnames(overlap_matrix) <- names(tenureRast)


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
sum(df_noOverlaps$areakm2) # yes, 7,932,964 km2 seems right, as all BR is ~8,500,000km2
# quick visualization:
ggplot(df_noOverlaps, aes(x=categ, y=areakm2)) +
  geom_bar(stat = "identity")

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
# OK - adding the df_across gets quite complicated, and unnecessary energy right now...
# just remember the important ones are 1) PAs-indigenous and 2) sustUse PAs and rural settlements
setwd(paste0(wdmain, "output/"))
write.csv(df, "OverlapsAcrossCategs_20240806.csv", row.names = F)

# Overlaps ACROSS PUBLIC AND PRIVATE categories ----
# this is the part that I haven't really done yet
# should i be deciding based on the information above?


# B) folder structuring for extractions
