#### ares with most biomass/non-food biomass exports ####
# goals: 
# a script that reads comex export data
# pairs this with spatial data to id the municipalities under most "threat" from trade

# first run: January 23, 2023

#libraries:
# install.packages("geobr")
# install.packages("cowplot")

library(dplyr)
library(sf)
library(geobr)
library(ggplot2)
library(cowplot)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# get soy export data ----
setwd(paste0(wdmain, "/data/processed/soyExports"))
myComex <- read.csv("comex-biomassExports_1997-2022.csv")
soyplace <- grep("soy", myComex$name, ignore.case = TRUE)
myComexSoy <- myComex[soyplace,]

# get municipalities data from the IBGE package ----
mun <- read_municipality(code_muni = "all", year = 2020)
nrow(mun)
ggplot(mun)+
  geom_sf()
mun$CO_MUN <- mun$code_muni

# test join data ----
soymunis <- left_join(mun,myComexSoy, by = "CO_MUN")
head(soymunis)
nrow(soymunis[is.na(soymunis$code_muni),]) 
soymunis[is.na(soymunis$code_muni),] 
unique(soymunis[is.na(soymunis$code_muni),]$SG_UF_MUN)
soymunis[which(soymunis$SG_UF_MUN == "EX"),] # EX means exterior

# PROBLEM TO SOLVE: in 17982 cases the code for the municipality didn't match 
# some comex munis are not recognized from the muni package
# the vast majority from SP, GO, MS, DF, and EX (wx which is exterior)
# when i do this the other way around and bind the comex to the municipalities
# then when i ask if any municipalities which are missing comex information
nrow(soymunis[is.na(soymunis$CO_ANO),])
# plot
# soy 2020
test <- soymunis[which(soymunis$CO_ANO == 2020),]
nrow(test[is.na(test$KG_LIQUIDO),])
# still need to find out if i'm missing any key municipalities for soy (for which i don't have recognizable spatial data)


# create function for plotting different exports

# comexData = myComex
# commodity = "soy"
# year = 2020
# year = c(2010, 2020)
# munYear = 2020
# title = "Soy Exports (kg)"

# 1 establish the commodity of interest
data <- comexData

# 1.1 to summarize (sum/mean) exports over certain time periods
print(paste0(commodity, " is found under the categories:"))
commodity_loc <- grep(paste0(commodity), data$name, ignore.case = TRUE)
print(unique(data[commodity_loc,]$name))
data <- data[commodity_loc,]

if(munYear>2020){stop("maximum year available from municipality data is 2020")}
if(length(year==1)){
  data <- data[which(data$CO_ANO >= year),]
}
if(length(year>1)){
  
  # subset data to the years of interest
  data <- data[which(data$CO_ANO >= year[1] & data$CO_ANO <= year[2]),]
  
  if(is.null(yearFUNC) == FALSE){ # THIS PART MIGHT NOT BE NECESSARY BECAUSE I NEED TO SUMMARIZE THESE BY THE MONTH ANYWAY...
    # if i want the sum of exports over the years that I've asked
    print(paste0("calculating ", yearFUNC, " for the years:"))
    print(unique(data$CO_ANO))
    # because this data is already subset for the years of interest, i only need to summarize by municipality
    data_sum <- as.data.frame(data %>%
                                group_by(CO_MUN, SG_UF_MUN) %>%
                                summarise(KG_LIQUIDO_sum = sum(KG_LIQUIDO), KG_LIQUIDO_mean = mean(KG_LIQUIDO)))
    data_sum$years = paste0(year[1], "-", year[2])
  }
}

# first, establish the year #NOTE, WHAT IF I WANT TO DO AVERAGES OVER YEARS?
data <- comexData[which(comexData$CO_ANO == paste0(year)),]





# 2. function to bind comex data with spatial data

# 3. function to plot municipalities' exports 

plotExports <- function(comexData, commodity, year=2020, munYear = 2020, title, yearFUNC=NULL) {
  
  
  else{
    
    
    
    
    
    # third, bind comex data with municipality data
    munis_geo <- read_municipality(code_muni = "all", year = munYear) # here add if condition
    munis_geo$CO_MUN <- munis_geo$code_muni
    data <- left_join(munis_geo, data, by = "CO_MUN")
    
    # third, plot
    plot <- ggplot(data) +
      geom_sf(fill = "gray90", color = "transparent", size = .1, aes(geometry = geom))+
      geom_sf(data = data[which(data$CO_ANO == year),], 
              aes(fill = log(KG_LIQUIDO), geometry = geom),
              color = "transparent") +
      scale_fill_continuous(low="#ffffe5", high="#662506", guide="colorbar", na.value="#78c679", labels = scales::comma) +
      theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
            legend.position = c(.2,.35), legend.title = element_blank()) +
      labs(title = paste0(title))
    
    return(plot)
  }
  
}

# create plots of commodity exports

mysoyplot <- plotExports(myComex, "soy", year=2020, munYear=2020, title = "(log) Soy exports 2020 (kg)")
mywoodpulpplot <- plotExports(myComex, "wood pulp", year = 2020, title = "(log) Wood Pulp exports 2020 (kg)")

plot_grid(mysoyplot, mywoodpulpplot)















