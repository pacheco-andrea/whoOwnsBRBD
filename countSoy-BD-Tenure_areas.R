# count the area of all the categories together


library(dplyr)
library(ggplot2)
library(stringr)

setwd(paste0(wdmain, "/output"))

data <- read.csv("countBD-tenure-soyProd.csv")

# make columns for categories
data$BD <- as.integer(as.data.frame(str_split_fixed(data$value, "",3))[,1])
data$soy <- as.integer(as.data.frame(str_split_fixed(data$value, "",3))[,2])
data$ten <- as.integer(as.data.frame(str_split_fixed(data$value, "",3))[,3])

data$BD_c <- NA
data$BD_c[which(data$BD == 1)] <- "insufficient knowledge"
data$BD_c[which(data$BD == 2)] <- "low priority good knowledge"
data$BD_c[which(data$BD == 3)] <- "high priority low knowledge"
data$BD_c[which(data$BD == 4)] <- "high priority high knowledge"

data$soy_c <- NA
data$soy_c[which(data$soy == 0)] <- "no soy production"
data$soy_c[which(data$soy == 1)] <- "low soy production"
data$soy_c[which(data$soy == 2)] <- "high soy production"

data$ten_c <- NA
data$ten_c[which(data$ten == 0)] <- "no tenure data"
data$ten_c[which(data$ten == 1)] <- "PA sustainable use"
data$ten_c[which(data$ten == 2)] <- "PA strict protection"
data$ten_c[which(data$ten == 3)] <- "Indigenous H"
data$ten_c[which(data$ten == 4)] <- "Indigenous NH"
data$ten_c[which(data$ten == 5)] <- "Quilombola"
data$ten_c[which(data$ten == 6)] <- "Comunitaria"
data$ten_c[which(data$ten == 7)] <- "Undesignated"
data$ten_c[which(data$ten == 8)] <- "Private farms"
data$ten_c[which(data$ten == 9)] <- "Rural settlements"


# who owns most BD , most threatened?
hBD_hthr <- data[which(data$BD == 4 & data$soy >=2),]

a <- ggplot(hBD_hthr, aes(ten_c, (count), fill = BD_c))+
  geom_col() +
  # scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = scales::comma, expand = c(0,0))+
  ylab(bquote("Area km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+
  coord_flip()

# who owns high priority low knowledge, most threatened?
lk_hthr <- data[which(data$BD ==3  & data$soy >=2),]
b <- ggplot(lk_hthr, aes(ten_c, (count), fill = BD_c))+
  geom_col() +
  # scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = scales::comma, expand = c(0,0))+
  ylab(bquote("Area km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+
  coord_flip()

# who owns insufficient knowledge, most threatened
ik_hthr <- data[which(data$BD == 1  & data$soy >=2),]

c <- ggplot(ik_hthr, aes(ten_c, (count), fill = BD_c))+
  geom_col() +
  # scale_colour_manual(values = myCols, aesthetics = c("color", "fill"))+
  scale_y_continuous(labels = scales::comma, expand = c(0,0))+
  ylab(bquote("Area km"^2)) +
  xlab("Land tenure category") + 
  theme(panel.background = element_blank(), plot.margin=grid::unit(c(2,2,2,2), "mm"),
        legend.position = c(.8,.3), legend.title = element_blank())+
  coord_flip()

plot_grid(a, b, c)
