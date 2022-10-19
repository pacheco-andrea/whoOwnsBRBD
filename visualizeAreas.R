#### Biodiversity and Tenure in Brazil ####
# script that graphs how much each tenure categ has of BD
# author: Andrea Pacheco
# first run: 17.10.2022

library(dplyr)
library(ggplot2)

wdmain <- "N:/eslu/priv/pacheco/biodivTenureBR"

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

table$BDCateg <- as.factor(table$BDCateg)
levels(table$BDCateg) <- c( "high priority high knowledge continuous",
                            "high priority high knowledge fragmented",
                            "high priority low knowledge continuous",
                            "high priority low knowledge fragmented",
                            "low priority good knowledge continuous",
                            "low priority good knowledge fragmented",
                            "insufficient knowledge fragmented",
                            "insufficient knowledge continuous")

# bar plot
myCols <- c("insufficient knowledge fragmented" = "#e0e0e0",
            "insufficient knowledge continuous" = "#878787",
            "low priority good knowledge fragmented" = "#7fbc41",
            "low priority good knowledge continuous" = "#b8e186",
            "high priority low knowledge fragmented" = "#fde0ef",
            "high priority low knowledge continuous" = "#f1b6da",
            "high priority high knowledge fragmented" = "#de77ae",
            "high priority high knowledge continuous" = "#c51b7d")

whoOwnsPlot <- ggplot(table, aes(tenCateg, (sum), fill = BDCateg))+
  geom_col() +
  scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), expand = c(0,0))+
  ylab(bquote("Area in km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+
  coord_flip()
# coord_flip(ylim = c(0,5000000))
whoOwnsPlot + guides(col = guide_legend(ncol=2))

# write out information
setwd(paste0(wdmain, "/output/"))
write.csv(table[,c(8,1,9,2:7)], "whoOwnsBR-BDinKm2.csv", row.names = FALSE)
ggsave("whoOwnsBR-BDinKm2_plot.png", whoOwnsPlot, width = 9, height = 4.5)

# to-do:
# recreate teure map 
# check this categorization ubirajara has done

