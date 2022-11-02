#### Biodiversity and Tenure in Brazil ####
# script that graphs how much each tenure categ has of BD
# author: Andrea Pacheco
# first run: 17.10.2022

library(dplyr)
library(ggplot2)
library(sf)

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
               "Private farms",
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

table2$BDCateg <- as.factor(table2$BDCateg)
levels(table2$BDCateg) <- c( "high priority high knowledge continuous",
                            "high priority high knowledge fragmented",
                            "high priority low knowledge continuous",
                            "high priority low knowledge fragmented",
                            "low priority good knowledge continuous",
                            "low priority good knowledge fragmented",
                            "insufficient knowledge fragmented",
                            "insufficient knowledge continuous")

# create bar plot
myCols <- c("insufficient knowledge fragmented" = "#e0e0e0",
            "insufficient knowledge continuous" = "#878787",
            "low priority good knowledge fragmented" = "#7fbc41",
            "low priority good knowledge continuous" = "#b8e186",
            "high priority low knowledge fragmented" = "#dfc27d",
            "high priority low knowledge continuous" = "#f6e8c3",
            "high priority high knowledge fragmented" = "#de77ae",
            "high priority high knowledge continuous" = "#c51b7d")

# convert area to 1000 km2
table2$sum2 <- (table2$sum)/1000

whoOwnsPlot <- ggplot(table2, aes(tenCateg, sum2, fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab(bquote("Area in 10,000 km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+
  coord_flip()
whoOwnsPlot 

# write out information
setwd(paste0(wdmain, "/output/"))
write.csv(table2[,c(8,1,9,2:7)], "whoOwnsBR-BDinKm2.csv", row.names = FALSE)
# save plot
ggsave("whoOwnsBR-BDinKm2_barplot.png", whoOwnsPlot, width = 9, height = 4.5)


# how much area does each category cover? ----
setwd(paste0(wdmain, "/data/processed/landTenureCategs"))
l <- list.files()
grep(".shp", l)
files <- lapply(l[grep(".shp", l)], st_read)
head(files[[1]])

areas <- list()
for(i in 1:length(files)){
  areas[[i]] <- aggregate(files[[i]]$area, by=list(tenure=files[[i]]$sub_class), FUN=sum)
}
a <- left_join(areas[[1]], areas[[2]], by = "tenure")
b <- left_join(a, areas[[3]], by = "tenure")
c <- left_join(b, areas[[4]], by = "tenure")
d <- left_join(c, areas[[5]], by = "tenure")
d$sumAT <- (rowSums(d[2:6], na.rm = T))/10 # to make ha into km2

# now repeat barplot by percentage of total area owned per tenure category
d <- d %>% mutate(tenCateg=recode(tenure,
                                     "AG"="Other",
                                     "ARU"= "Rural settlements",
                                     "CARpr"= "Private farms",
                                     "CARpo"= "Private farms",
                                     "COM"="Communal/Quilombo",
                                     "ML"="Other",
                                     "ND_B" = "Undesignated",
                                     "ND_I" = "Undesignated",
                                     "QL"= "Communal/Quilombo",
                                     "SIGEF"= "Private farms",
                                     "TI_H"= "Indigenous",
                                     "TI_N"= "Indigenous",
                                     "TLPC"= "Undesignated",
                                     "TLPL"= "Private farms",
                                     "TRANS"= "Other",
                                     "UCPI"= "PA strict protection",
                                     "UCUS"= "PA sustainable use",
                                     "URB"= "Other"))

d2 <- as.data.frame(d %>% 
                      group_by(tenCateg) %>%
                      summarize(sumAT = sum(sumAT)))

table3 <- left_join(table2, d2, by = "tenCateg")


# remake plot
whoOwnsPlot2 <- ggplot(table3, aes(tenCateg, (sum/sumAT), fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab("% total area") +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+
  coord_flip()
whoOwnsPlot2

round((table3$sum/table3$sumAT)*100, digits=2)

# okay this doesn't make sense and does not provide much different information. 
# i'm assuming there's something wrong with the total calculation of area per tenure category which could be traced back to the parcels themselves
# but i'm very annoyed at how slow the server is right now

mySumsByTenure <- table2 %>%
  group_by(tenCateg) %>%
  summarize(sumAT = sum(sum))

mySumsbyBD <- table2%>%
  group_by(BDCateg) %>%
  summarize(sumBD = sum(sum))

table3 <- left_join(table2, mySumsByTenure, by = "tenCateg")
table3 <- left_join(table3, mySumsbyBD, by = "BDCateg")

# % of total tenure's areas
percentTotalAreas <- ggplot(table3, aes(tenCateg, (sum/sumAT)*100, fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab("% total area") +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = "none", legend.title = element_blank())+
  coord_flip()
percentTotalAreas
setwd(paste0(wdmain, "/output/"))
ggsave("BRBD_percentTotalTenureArea.png", percentTotalAreas, width = 9, height = 4.5)

# % of total BD category areas
# this should answer: out of all the highly biodiverse areas, what percent is in x lands?

myTenCols <- c("Other"= "#F0F0F0", 
               "PA sustainable use" = "#8C7E5B", 
               "PA strict protection" = "#1B9E77", 
               "Indigenous" = "#E78AC3",
               "Communal/Quilombo" = "#FFD700",
               "Undesignated" = "#1d6c7d",
               "Private farms" = "#8DA0CB",
               "Rural settlements" = "#FC8D62")

percentBDareas <- ggplot(table3, aes(BDCateg, (sum/sumBD)*100, fill = tenCateg))+
  geom_col() +
  scale_colour_manual(values = myTenCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab("% total area") +
  xlab("Biodiversity relevance") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = "none", legend.title = element_blank())+
  coord_flip()
percentBDareas
setwd(paste0(wdmain, "/output/"))
ggsave("BRBD_percentTotalBDArea.png", percentBDareas, width = 9, height = 4.5)

