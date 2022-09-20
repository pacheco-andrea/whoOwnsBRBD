# biodiversity_conservation dataset from ubirajara
wdmain <- "N:/eslu/priv/pacheco/biodivTenureBR"
setwd(paste0(wdmain,"/data/raw/biodiveristy_conservation"))
list.files()

# install.packages("raster")
# install.packages("rgdal")

library(raster)
library(rgdal)

brc <- raster("biodiveristy_conservation.tif")
brc
plot(brc)
res(brc)
unique(brc)

# the 8 categs seem to correspond precisely with what they have in the paper... but in the same order?
# is this 500*500 m? - which means at 1km