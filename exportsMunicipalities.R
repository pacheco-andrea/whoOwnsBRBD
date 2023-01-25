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
# soyplace <- grep("soy", myComex$name, ignore.case = TRUE)
# myComexSoy <- myComex[soyplace,]

# # get municipalities data from the IBGE package ----
# this is now incorporated in the function below
# mun <- read_municipality(code_muni = "all", year = 2020)
# nrow(mun)
# ggplot(mun)+
#   geom_sf()
# mun$CO_MUN <- mun$code_muni



# create function for plotting different exports ----

plotCommodityExports <- function(data, year, commodity, munYear, title, plotFUN){
  
  if(munYear>2020){stop("maximum year available from municipality data is 2020")}
  comexData <- data
  # 1. function to subset data to the commodity and years of interest
  subsetCommodityYears <- function(comexData, year, commodity){
    # establish the commodity of interest
    data <- comexData
    print(paste0(commodity, " is found under the categories:"))
    commodity_loc <- grep(paste0(commodity), data$name, ignore.case = TRUE)
    print(unique(data[commodity_loc,]$name))
    data <- data[commodity_loc,]
    
    # summarize exports over chosen time periods (sum or average)
    
    if(length(year) == 1){
      # subset data to year established
      data <- data[which(data$CO_ANO == year),]
      data_sum <- as.data.frame(data %>%
                                  group_by(CO_MUN, SG_UF_MUN) %>%
                                  summarize(KG_LIQUIDO_sum = sum(KG_LIQUIDO), KG_LIQUIDO_mean = mean(KG_LIQUIDO)))
      data_sum$years = paste0(year)
    }
    if(length(year) > 1){
      
      # subset data to the years of interest established
      data <- data[which(data$CO_ANO >= year[1] & data$CO_ANO <= year[2]),]
      # print message to check you have the right years
      print("function calculated for the years:")
      print(unique(data$CO_ANO))
      # because this data is already subset for the years of interest, i only need to summarize by municipality
      # create new data frame with this summarized information
      data_sum <- as.data.frame(data %>%
                                  group_by(CO_MUN, SG_UF_MUN) %>%
                                  summarise(KG_LIQUIDO_sum = sum(KG_LIQUIDO), KG_LIQUIDO_mean = mean(KG_LIQUIDO)))
      data_sum$years = paste0(year[1], "-", year[2])
    }
    return(data_sum)
  }
  
  mydata <- subsetCommodityYears(comexData = comexData, commodity = commodity, year = year)
  
  # 2. function to bind comex data with spatial data
  # data must be in format of the output of previous function
  joinComexToGeo <- function(data, munYear){
    
    munis_geo <- read_municipality(code_muni = "all", year = munYear) 
    munis_geo$CO_MUN <- munis_geo$code_muni
    
    # some problem solving: 
    # i found out the comex data had the wrong code for the states of SP, MS, GO, and DF (the code for EX should be ignored bc it means exterior)
    # but the mistake is just the first two digits, which should be 35 (instead of 34), 50 (instead of 52), 52 (instead of 53), and 53 (instead of 54) 
    # check here that all the SP observations are indeed starting with 34 
    SPcomex <- data[which(data$SG_UF_MUN == "SP"),]
    SPibge <- munis_geo[which(munis_geo$abbrev_state == "SP"),] # here we see they should all start with 35
    # replace the first two digits
    data[which(data$SG_UF_MUN == "SP"),]$CO_MUN <- as.numeric(gsub("^[34]{1,2}", "35", data[which(data$SG_UF_MUN == "SP"),]$CO_MUN))
    # now for MS
    MScomex <- data[which(data$SG_UF_MUN == "MS"),] # these are 52 and should be 50
    MSibge <- munis_geo[which(munis_geo$abbrev_state == "MS"),] # here we see they should all start with 50
    data[which(data$SG_UF_MUN == "MS"),]$CO_MUN <- as.numeric(gsub("^[52]{1,2}", "50", data[which(data$SG_UF_MUN == "MS"),]$CO_MUN))
    # now for GO
    GOcomex <- data[which(data$SG_UF_MUN == "GO"),] # these are 53 and should be 52
    GOibge <- munis_geo[which(munis_geo$abbrev_state == "GO"),] # here we see they should all start with 52
    data[which(data$SG_UF_MUN == "GO"),]$CO_MUN <- as.numeric(gsub("^[53]{1,2}", "52", data[which(data$SG_UF_MUN == "GO"),]$CO_MUN))
    # now for DF
    DFcomex <- data[which(data$SG_UF_MUN == "DF"),] # this is 53 and should be 54
    DFibge <- munis_geo[which(munis_geo$abbrev_state == "DF"),] # here we see they should all start with 52
    data[which(data$SG_UF_MUN == "DF"),]$CO_MUN <- as.numeric(gsub("^[54]{1,2}", "53", data[which(data$SG_UF_MUN == "DF"),]$CO_MUN))
    # *NOTE*, i also manually check these with the master translating table from comex which has the municipality names
    
    # join comex data with geographical ibge data
    data <- full_join(data, munis_geo, by = "CO_MUN") # full join because i want to see what's missing from both sides
    # 203 (out of 642 municipalities in SP)
    summary(data)
    data[is.na(data$code_muni),] # this should only be EX for "exterior"
    
    return(data)
  } 
  
  mydata_sf <- joinComexToGeo(data = mydata, munYear = munYear)
  
  # 3. function to plot municipalities' exports 
  plotExports <- function(data, title, plotFUN) {
    
    if(plotFUN == "sum"){
      
      plot <- ggplot(data) +
        geom_sf(data = data, 
                aes(fill = (KG_LIQUIDO_sum), geometry = geom),
                color = "transparent") +
        scale_fill_continuous(low="#ffffe5", high="#662506", guide="colorbar", na.value="gray90", labels = scales::comma) +
        theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
              legend.position = c(.2,.2), legend.title = element_blank(), legend.background = element_rect(colour = "transparent")) +
        labs(title = paste0(title))
    }
    if(plotFUN == "mean"){
      plot <- ggplot(data) +
        geom_sf(data = data, 
                aes(fill = (KG_LIQUIDO_mean), geometry = geom),
                color = "transparent") +
        scale_fill_continuous(low="#ffffe5", high="#662506", guide="colorbar", na.value="gray90", labels = scales::comma) +
        theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
              legend.position = c(.2,.2), legend.title = element_blank(), legend.background = element_rect(colour = "transparent")) +
        labs(title = paste0(title))
    }
    
    return(plot)
  }
  myplot <- plotExports(data = mydata_sf, title=title, plotFUN = plotFUN)
 
  return(myplot)
}

# create plots of commodity exports ----

mysoyplot <- plotCommodityExports(data = myComex, commodity = "soy", 
                             year = c(2006,2022), munYear = 2020, title = "Average soy exports 2006-2022 (kg)", plotFUN = "mean")

mywoodpulpplot <- plotCommodityExports(data = myComex, commodity = "beef", 
                                       year = c(2006,2022), munYear = 2020, title = "Average wood pulp exports 2006-2022 (kg)", plotFUN = "mean")

plotCommodityExports(data = myComex, commodity = "soy", 
                     year = c(1997,2022), munYear = 2020, title = "Sum of soy exports 1997-2022 (kg)", plotFUN = "sum")

plot_grid(mysoyplot, mywoodpulpplot)















