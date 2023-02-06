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
library(fasterize)
library(cowplot)
# library(rgdal)

# directory

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# main plot who owns in km2 ----
setwd(paste0(wdmain, "/output/"))
who <- read.csv("whoOwnsBR-BDinKm2.csv")

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

# don't plot the "other category bc its confusing people
who <- who[which(who$tenCateg != "Other"),]

whoOwnsPlot <- ggplot(who, aes(tenCateg, (sum), fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = scales::comma, expand = c(0,0))+
  ylab(bquote("Area in 10,000 km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(5,5,5,5), "mm"),
        legend.position = "none", legend.title = element_blank())+ # probably REMOVE LEGEND
  coord_flip()
whoOwnsPlot 

# setwd(paste0(wdmain, "/output"))
# svg("whoOwnsBRBD_barplot.svg", width = 8.3, height = 2.5)
# whoOwnsPlot
# dev.off()

# plots where disagg categories by percentages ----




# MAPS ----

# biodiversity map ----
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
                    colorkey = FALSE,
                    col.regions = c("#e0e0e0", "#878787", "#fde0ef","#f1b6da",
                                    "#e6f5d0","#7fbc41","#de77ae","#c51b7d"))
bdplot + latticeExtra::layer(sp.lines(biome_outline, col = "gray20", lwd = 1))


setwd(paste0(wdmain, "/output"))
png("mapBRBiodiversity.png", width = 1500, height = 1500, units = "px", res = 300, bg = "transparent")
bdplot + latticeExtra::layer(sp.lines(biome_outline, col = "gray20", lwd = 1))
dev.off()



# soy map ----
setwd(paste0(wdmain, "/data/processed/soyExports"))
soy <- st_read("trase_soyProduction_2016-2020.shp")
head(soy)

biomshp <- read_biomes()
biomshp <- biomshp[1:6,]


meanSoy20162020 <- ggplot(soy) +
  geom_sf(data = soy, 
          aes(fill = mn_sypr),
          color = "transparent") +
  scale_fill_gradient(na.value="gray90", 
                      low="#f6e8c3", high="#543005", guide="colorbar", 
                      aesthetics = "fill",
                      labels = scales::comma,) +
  geom_sf(data = biomshp, fill = "transparent", color = "gray20") +
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.2,.2), legend.title = element_blank(), legend.background = element_rect(colour = "transparent")) +
  labs(title = paste0("Mean Annual Soy Production 2016-2020 (t)"))
meanSoy20162020

sumSoy20162020 <- ggplot(soy) +
  geom_sf(data = soy, 
          aes(fill = sm_sypr),
          color = "transparent") +
  scale_fill_gradient(na.value="gray90", 
                      low="#f6e8c3", high="#543005", guide="colorbar", 
                      aesthetics = "fill",
                      labels = scales::comma,) +
  geom_sf(data = biomshp, fill = "transparent", color = "gray20") +
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.2,.2), legend.title = element_blank(), legend.background = element_rect(colour = "transparent")) +
  labs(title = paste0("Sum of Soy Production 2016-2020 (t)"))
sumSoy20162020

plot_grid(meanSoy20162020, sumSoy20162020)

# plot all together ----

# 1 Reclassify bd priority map so that they're fewer categories:
bd
myReclass <- data.frame("bdo" = 1:8)
myReclass$bdn <- c(10,10,30,30,20,20,40,40)
bd2 <- reclassify(bd, myReclass)
bd2

# 4 = high priority
# 3 = high priority low knowledge
# 2 = low priority
# 1 = insufficient knowledge
# ratify this bd map
bd2 <- ratify(bd2)
rat <- levels(bd2)[[1]]
rat$bdcateg <- c("insufficient knowledge",
                 "low priority good knowledge", 
                 "high priority low knowledge",  
                 "high priority high knowledge")
levels(bd2) <- rat


# 2 discretize soy production data
# no threat (everything NA)
# low threat (everything around 0?)
# middle threat
# high threat
soy$pcat <- NA
soy$pcat[which(soy$mn_sypr >= 1181.33)] <- 2
soy$pcat[which(soy$mn_sypr < 1181.33)] <- 1
soy$pcat[which(is.na(soy$mn_sypr))] <- 0
# create mask
mask <- bd2*0
# rasterize this sf
soyR <- st_transform(soy, crs = crs(mask))
soyR <- fasterize(soyR, mask, field = "pcat")
# now ratify this raster as well
soyR <- ratify(soyR)
rat <- levels(soyR)[[1]]
rat$scateg <- c("no soy production",
                "low soy production",
                "high soy production")
levels(soyR) <- rat
# plot 2 maps overlayed


# bdcols <- c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e")
# soycols <- c("#ece2f0", "#a6bddb", "#1c9099")

# biodiversity simplified
a <- levelplot(bd2,
          xlab=NULL, ylab=NULL,
          scales = list(draw=F),
          par.settings = list(axis.line = list(col = "transparent")),
          margin = FALSE,
          colorkey = FALSE,
          alpha.regions =0.6,
          col.regions = c("#feebe2", "#fbb4b9", "#f768a1", "#ae017e"))
a
# soy categorized
b <- levelplot(soyR,
               xlab=NULL, ylab=NULL,
               scales = list(draw=F),
               par.settings = list(axis.line = list(col = "transparent")),
               margin = FALSE,
               colorkey = FALSE,
               # alpha.regions =0.6,
               col.regions = c("#ece2f0", "#a6bddb", "#1c9099"))

plot_grid(a, b)

b + a + latticeExtra::layer(sp.lines(biome_outline, col = "gray20", lwd = 1))
# a + b + latticeExtra::layer(sp.lines(biome_outline, col = "gray20", lwd = 1))



setwd(paste0(wdmain, "/output"))
png("mapBRBDxSoy.png", width = 1500, height = 1500, units = "px", res = 500, bg = "transparent")
b + a + latticeExtra::layer(sp.lines(biome_outline, col = "gray20", lwd = .1))
dev.off()

# now, quantify how much this is in area x TENURE ----
soyR2 <- soyR*10
bd3 <- bd2*10

setwd(paste0(wdmain, "/output"))
ten <- raster("tenure_data_BR.tif")

all <- bd3+soyR2+ten
plot(all)

countAll <- as.data.frame(freq(all))

setwd(paste0(wdmain, "/output"))
write.csv(countAll, "countBD-tenure-soyProd.csv", row.names = F)



