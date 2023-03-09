#### Biodiversity and Tenure in Brazil ####
# script that graphs how much each tenure categ has of BD
# author: Andrea Pacheco
# first run: 17.10.2022

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD"

# read data on frequency counts (land tenure + biodiversity) ----
setwd(paste0(wdmain, "/data/processed/BDTen_areaCount"))
tables <- lapply(list.files(), read.csv)

t1 <- full_join(tables[[1]], tables[[2]], by="value")
t2 <- full_join(t1, tables[[3]], by="value")
t3 <- full_join(t2, tables[[4]], by="value")
t4 <- full_join(t3, tables[[5]], by="value")

# make table nice
table <- t4
colnames(table)[2:6] <- gsub(".csv", "", gsub("countBD-tenure_", "", list.files()))
table$sum <- rowSums(table[2:6], na.rm = T)

# create categories i'll need for labels
BDcategs <- c("insufficient knowledge continuous",
              "insufficient knowledge fragmented",
              "high priority low knowledge continuous",
              "high priority low knowledge fragmented",
              "low priority good knowledge continuous",
              "low priority good knowledge fragmented",
              "high priority high knowledge continuous",
              "high priority high knowledge fragmented")

TenCategs <- c("PA sustainable use",
               "PA strict protection",
               "Indigenous H",
               "Indigenous NH",
               "Quilombola",
               "Comunitaria",
               "Undesignated",
               "Private lands",
               "Rural settlements")
# manually categorize tenure categories
table$tenCateg <- NA
table[grep("1$", table$value),]$tenCateg <- TenCategs[1]
table[grep("2$", table$value),]$tenCateg <- TenCategs[2]
table[grep("3$", table$value),]$tenCateg <- TenCategs[3]
table[grep("4$", table$value),]$tenCateg <- TenCategs[4]
table[grep("5$", table$value),]$tenCateg <- TenCategs[5]
table[grep("6$", table$value),]$tenCateg <- TenCategs[6]
table[grep("7$", table$value),]$tenCateg <- TenCategs[7]
table[grep("8$", table$value),]$tenCateg <- TenCategs[8]
table[grep("9$", table$value),]$tenCateg <- TenCategs[9]
table[grep("0$", table$value),]$tenCateg <- "Other"

table$BDCateg <- NA
table[grep("^1", table$value),]$BDCateg <- BDcategs[1]
table[grep("^2", table$value),]$BDCateg <- BDcategs[2]
table[grep("^3", table$value),]$BDCateg <- BDcategs[3]
table[grep("^4", table$value),]$BDCateg <- BDcategs[4]
table[grep("^5", table$value),]$BDCateg <- BDcategs[5]
table[grep("^6", table$value),]$BDCateg <- BDcategs[6]
table[grep("^7", table$value),]$BDCateg <- BDcategs[7]
table[grep("^8", table$value),]$BDCateg <- BDcategs[8]

# keep only rows for which there was BD to count
table <- table[!is.na(table$value),]

table2 <- table

table2[which(table2$tenCateg == "Indigenous H"),]$tenCateg <- "Indigenous"
table2[which(table2$tenCateg == "Indigenous NH"),]$tenCateg <- "Indigenous"
table2[which(table2$tenCateg == "Comunitaria"),]$tenCateg <- "Communal/Quilombo"
table2[which(table2$tenCateg == "Quilombola"),]$tenCateg <- "Communal/Quilombo"


table2$BDCateg <- factor(table2$BDCateg, levels = c("high priority high knowledge fragmented",
                                              "high priority high knowledge continuous", 
                                              "high priority low knowledge fragmented",
                                              "high priority low knowledge continuous",
                                              "low priority good knowledge fragmented",
                                              "low priority good knowledge continuous",
                                              "insufficient knowledge fragmented",
                                              "insufficient knowledge continuous"))

# create bar plot
myCols <- c("insufficient knowledge continuous" = "#e0e0e0",
            "insufficient knowledge fragmented" = "#878787",
            "high priority low knowledge continuous" = "#fde0ef",
            "high priority low knowledge fragmented" = "#f1b6da",  
            "low priority good knowledge continuous" = "#e6f5d0",
            "low priority good knowledge fragmented" = "#7fbc41", 
            "high priority high knowledge continuous" = "#de77ae",  
            "high priority high knowledge fragmented" = "#c51b7d")

# make sure repeated indigenous/communal categories are summed

table2 <- table2[which(table2$tenCateg != "Other"),]
table2.2 <- as.data.frame(table2 %>%
  group_by(tenCateg, BDCateg) %>%
  summarize(sum = sum(sum)))

# who owns plot ----

whoOwnsPlot <- ggplot(table2.2, aes(tenCateg, (sum), fill = BDCateg))+
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

# write out information
setwd(paste0(wdmain, "/output/"))
write.csv(table2.2, "whoOwnsBR-BDinKm2.csv", row.names = FALSE)

# create subplots of percentages of total areas per category ----

# sum all areas by total of tenure category
mySumsByTenure <- table2.2 %>%
  group_by(tenCateg) %>%
  summarize(sumAT = sum(sum))
# sum all areas by total of biodiversity category
mySumsbyBD <- table2.2 %>%
  group_by(BDCateg) %>%
  summarize(sumBD = sum(sum))

table3 <- left_join(table2.2, mySumsByTenure, by = "tenCateg")
table3 <- left_join(table3, mySumsbyBD, by = "BDCateg")


table3$tenCateg <- factor(table3$tenCateg, levels = c("Undesignated",
                                   "Rural settlements", 
                                   "Private lands",
                                   "PA strict protection",
                                   "PA sustainable use",
                                   "Indigenous",
                                   "Communal/Quilombo"))
# % of total tenure's areas
percentTotalAreas <- ggplot(table3, aes(tenCateg, (sum/sumAT)*100, fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab("% total area") +
  xlab(element_blank()) +
  scale_x_discrete(guide = guide_axis(angle = 55)) +
  theme(panel.background = element_blank(), 
        plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = "none", 
        legend.title = element_blank())

percentTotalAreas
setwd(paste0(wdmain, "/output"))
svg("BRBD_percentTotalTenureArea.svg", width = 4.15, height = 3)
percentTotalAreas
dev.off()

# % of total BD category areas
# this should answer: out of all the highly biodiverse areas, what percent is in x lands?

myTenCols <- c("Other"= "#F0F0F0", 
               "PA sustainable use" = "#8C7E5B", 
               "PA strict protection" = "#1B9E77", 
               "Indigenous" = "#E78AC3",
               "Communal/Quilombo" = "#FFD700",
               "Undesignated" = "#1d6c7d",
               "Private lands" = "#8DA0CB",
               "Rural settlements" = "#FC8D62")

# wrap labels of bd but keep factor levels
table3$BDCateg_str <- factor(str_wrap(table3$BDCateg, width = 18), 
                             levels = c("high priority\nhigh knowledge\nfragmented",
                                        "high priority\nhigh knowledge\ncontinuous",
                                        "high priority\nlow knowledge\nfragmented",
                                        "high priority\nlow knowledge\ncontinuous",
                                        "low priority\ngood knowledge\nfragmented",
                                        "low priority\ngood knowledge\ncontinuous",
                                        "insufficient\nknowledge\nfragmented",
                                        "insufficient\nknowledge\ncontinuous"))
levels(table3$BDCateg)
levels(table3$BDCateg_str)
# x <- unique(table3$BDCateg_str)
# levels(table3$BDCateg_str) <- x[c(8,7,4,3,6,5,2,1,2)]

percentBDareas <- ggplot(table3, aes(BDCateg_str,(sum/sumBD)*100, fill = tenCateg)) +
  geom_col() +
  scale_colour_manual(values = myTenCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab("% total area") +
  xlab(element_blank()) + 
  scale_x_discrete(guide = guide_axis(angle = 55)) +
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,0,2), "mm"),
        legend.position = "none", legend.title = element_blank())

percentBDareas

setwd(paste0(wdmain, "/output/"))
png("BRBD_percentTotalBDArea.png", width = 4.15, height = 3, units = "in", res = 300, bg = "transparent")
percentBDareas
dev.off()


