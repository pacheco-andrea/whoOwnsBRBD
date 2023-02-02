# script for fine tuning plots
# output is final figures
# who owns
# bd map
# soy map
# join map: where is most BD threatened?

# libraries
library(raster)
rasterOptions(tmpdir = "N:/eslu/priv/pacheco/biodivTenureBR/tmp", chunksize = 524288, maxmemory = 134217728)
library(ggplot2)
library(geobr)
library(rasterVis)
library(sf)
# library(rgdal)

# directory

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# main plot who owns in km2 ----
setwd(paste0(wdmain, "/output/"))
who <- read.csv("whoOwnsBR-BDinKm2.csv")

# MAKE FONT SIZE BIGGER

who$BDCateg <- factor(who$BDCateg, levels = c("high priority high knowledge fragmented",
                                               "high priority high knowledge continuous", 
                                               "high priority low knowledge fragmented",
                                               "high priority low knowledge continuous",
                                               "low priority good knowledge fragmented",
                                               "low priority good knowledge continuous",
                                               "insufficient knowledge fragmented",
                                               "insufficient knowledge continuous"))
who
myCols <- c("insufficient knowledge continuous" = "#e0e0e0",
            "insufficient knowledge fragmented" = "#878787",
            "low priority good knowledge continuous" = "#e6f5d0",
            "low priority good knowledge fragmented" = "#7fbc41", 
            "high priority low knowledge continuous" = "#fde0ef",
            "high priority low knowledge fragmented" = "#f1b6da",  
            "high priority high knowledge continuous" = "#de77ae",  
            "high priority high knowledge fragmented" = "#c51b7d")
whoOwnsPlot <- ggplot(who, aes(tenCateg, (sum), fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab(bquote("Area in 10,000 km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+ # probably REMOVE LEGEND
  coord_flip()
whoOwnsPlot 

# plots where disagg categories by percentages ----




# MAPS ----

# biodiversity map
setwd(paste0(wdmain, "/data/raw/Biodiversity_model"))
bd <- raster("Model_biodiverisity.tif")
bd # maybe should recategorize eventually? bc i want just priority categories...?

biomshp <- read_biomes()
biomshp <- biomshp[1:6,]
plot(biomshp$geom)
biomshp <- st_transform(biomshp, crs = crs(bd))
biome_outline <- as(st_geometry(biomshp), Class = "Spatial")
plot(biome_outline, col = "gray80", lwd=1)

bd <- ratify(bd)
rat <- levels(bd)[[1]]
rat$bdcateg <- c("insufficient knowledge continuous",
                 "insufficient knowledge fragmented",
                 "high priority low knowledge continuous",
                 "high priority low knowledge fragmented",  
                 "low priority good knowledge continuous",
                 "low priority good knowledge fragmented", 
                 "high priority high knowledge continuous",  
                 "high priority high knowledge fragmented")
levels(bd) <- rat
bd

bdplot <- levelplot(bd,
                    xlab=NULL, ylab=NULL,
                    scales = list(draw=F),
                    par.settings = list(axis.line = list(col = "transparent")),
                    margin = FALSE,
                    #colorkey = FALSE,
                    col.regions = c("#e0e0e0", "#878787", "#fde0ef","#f1b6da",
                                    "#e6f5d0","#7fbc41","#de77ae","#c51b7d"))
bdplot + latticeExtra::layer(sp.lines(biome_outline, col = "gray20", lwd = 1))


# soy map!
setwd(paste0(wdmain, "/data/processed/soyExports"))


# plot all together ----
# Reclassify bd priority map so that they're fewer categories:
# high priority
# high priority low knowledge
# low priority
# insufficient knowledge

# discretize soy production data
# no threat (everything NA)
# low threat (everything around 0?)
# middle threat
# high threat
 