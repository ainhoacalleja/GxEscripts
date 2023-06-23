#setting working directory
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/correlations_graphs")

library(gplots)
library(RColorBrewer)
#reading data file
cor_ht10 <- as.matrix(read.csv('cor_ht10.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_ht10)
is.numeric(cor_ht10)
heatmap.2(cor_ht10, Rowv = TRUE, Colv = TRUE)


#new package heatmaply (from january 2018). This was the best option.
#install.packages("heatmaply")
#webshot::install_phantomjs()
#create directory to save the image map.
#dir.create("graphs")
library(heatmaply)

#heatmap correlation map for ht10
heatmaply(cor_ht10, main = "Correlation Heatmap for Ht10", margins = c(60,100,40,20),limits = c(-1,1), file = "graphs/heatmaply_ht10.png")

#heatmap cor map for ht20
cor_ht20 <- as.matrix(read.csv('cor_ht20.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_ht20)
is.numeric(cor_ht20)
#heatmaply(cor_ht20, Colv = "Rowv")
heatmaply(cor_ht20, main = "Correlation Heatmap for Ht20", limits = c(-1,1), margins = c(60,100,40,20), file = "graphs/heatmaply_ht20.png")

#heatmap vit10
cor_vt10 <- as.matrix(read.csv('cor_vt10.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_vt10)
is.numeric(cor_vt10)
heatmaply(cor_vt10, main = "Correlation Heatmap for Vt10", limits = c(-1,1), margins = c(60,100,40,20), file = "graphs/heatmaply_vt10.png")

#heatmap vit20
cor_vt20 <- as.matrix(read.csv('cor_vt20.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_vt20)
is.numeric(cor_vt20)
heatmaply(cor_vt20, main = "Correlation Heatmap for Vt20", limits = c(-1,1), margins = c(60,100,40,20), file = "graphs/heatmaply_vt20.png")

#heatmap ht10 vs others
multi_ht10 <- as.matrix(read.csv('multiv_ht10.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(multi_ht10)
is.numeric(multi_ht10)
heatmaply(multi_ht10, main = "Multi ht10", limits = c(-1,1), margins = c(60,100,40,20))
#heatmaply(multi_ht10, dendrogram="none", main = "Multi ht10", limits = c(-1,1), margins = c(60,100,40,20))
########################################################################################################


#multivariate correlation plots

setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/correlations_graphs/cluster_multi")
#type A/B genetic correlations for group 1
library(heatmaply)
multi_gr1 <- as.matrix(read.csv('cor_gr1.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(multi_gr1)
is.numeric(multi_gr1)
heatmaply(multi_gr1, dendrogram="none", main = "genetic correlations group 1", margins = c(60,100,40,20), 
          draw_cellnote = TRUE, cellnote_textposition="middle center",
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "green", high = "red", mid = "black", limits = c(-1,1)))

#type A genetic correlations for group 2
multi_gr2 <- as.matrix(read.csv('multi_gr2.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(multi_gr2)
is.numeric(multi_gr2)
heatmaply(multi_gr2, dendrogram="none", main = "type A genetic correlations group 2", margins = c(60,100,40,20),
          draw_cellnote = TRUE, cellnote_textposition="middle center",
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "green", 
                                                                  limits = c(-1,1)))
#type A genetic correlations for group 3
multi_gr3 <- as.matrix(read.csv('multi_gr3.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(multi_gr3)
is.numeric(multi_gr3)
heatmaply(multi_gr3, dendrogram="none", main = "type A genetic correlations group 3", margins = c(60,100,40,20),
          draw_cellnote = TRUE, cellnote_textposition="middle center",
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "green", 
                                                                  limits = c(-1,1)))
#type A genetic correlations for group 4
multi_gr4 <- as.matrix(read.csv('multi_gr4.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(multi_gr4)
is.numeric(multi_gr4)
heatmaply(multi_gr4, dendrogram="none", main = "type A genetic correlations group 4", margins = c(60,100,40,20),
          draw_cellnote = TRUE, cellnote_textposition="middle center",
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "green", 
                                                                  limits = c(-1,1)))


#HEATMAP FOR FAMILY CONECTIONS
conect_graph <- as.matrix(read.csv('connections.csv', header = T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(conect_graph)

is.numeric(conect_graph)
heatmaply(conect_graph, dendrogram="none", main = "% of family conections", margins = c(60,100,40,20),
          draw_cellnote = TRUE, cellnote_textposition="middle center", limits = c(0,100))


#another heatmap: but the names of trials do not appear
install.packages("spatstat") #installing package
library(spatstat) # "im" function 
plot(im(cor_ht10[nrow(cor_ht10):1,]), main="Correlation Matrix Map")
plot(im(cor_ht10))

