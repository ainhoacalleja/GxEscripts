# Remove everything from the working environment
rm(list=ls())

#setting working directory
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/connectivity_graph")

library(gplots)
library(RColorBrewer)

#new package heatmaply (from january 2018). This was the best option.
#install.packages("heatmaply")
#webshot::install_phantomjs()
#create directory to save the image map.
#dir.create("graphs")
library(heatmaply)



#HEATMAP FOR FAMILY CONECTIONS
conect_graph <- as.matrix(read.csv('connections.csv', header = T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(conect_graph)

is.numeric(conect_graph)
heatmaply(conect_graph, dendrogram="none", main = "% of family connections", margins = c(60,100,40,20),
          draw_cellnote = TRUE, cellnote_textposition="middle center", limits = c(0,100))

ggsave("fam_connections_1.pdf", width=12, height=6, units = "cm")

library(ggpubr)
library(magrittr)
library(gridExtra)

ggexport(filename="family_con_2.pdf")

#another heatmap: but the names of trials do not appear
install.packages("spatstat") #installing package
library(spatstat) # "im" function 
plot(im(cor_ht10[nrow(cor_ht10):1,]), main="Correlation Matrix Map")
plot(im(cor_ht10))

