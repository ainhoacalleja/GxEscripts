# Remove everything in the working environment.
rm(list=ls())

#setting working directory
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/correlations_graphs")

library(gplots)
library(RColorBrewer)

############## height10 #####################################################################
#reading a data file
cor_ht10 <- as.matrix(read.csv('cor_ht10.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_ht10)
is.numeric(cor_ht10)

#Use correlations between variables "as distance" for cluster analysis
dd_ht10 <- as.dist((1 - cor_ht10))
head(dd_ht10)
round(1000 * dd_ht10) # (prints more nicely)
plot(hclust(dd_ht10, method = "ward.D2"), cex = 0.6,  main="Tree height 1")  #PLOTTING the cluster analysis

save.image("corr_graphs_UV.RData")
#creating heatmap with dendrogram and cluster for height 10

rd<-as.dist((1 - cor_ht10))
rc<-hclust(rd)
cd<-as.dist(t(1-cor_ht10))
cc<-hclust(cd)

heatmap_ht10<-heatmap.2(cor_ht10,  Rowv=as.dendrogram(rc), Colv=as.dendrogram(cc),
                        revC=T,density.info="none", trace="none", main="Tree height 1")
save.image("corr_graphs_UV.RData")

############## height20 #####################################################################
#reading a data file
cor_ht20 <- as.matrix(read.csv('cor_ht20.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_ht20)
is.numeric(cor_ht20)

#Use correlations between variables "as distance" for cluster analysis
dd_ht20 <- as.dist((1 - cor_ht20))
head(dd_ht20)
round(1000 * dd_ht20) # (prints more nicely)
plot(hclust(dd_ht20, method = "ward.D2"), cex = 0.6, main="Tree height 2")  #PLOTTING the cluster analysis

#creating heatmap with dendrogram and cluster for height 20
rd<-as.dist((1 - cor_ht20))
rc<-hclust(rd)
cd<-as.dist(t(1-cor_ht20))
cc<-hclust(cd)

heatmap_ht20<-heatmap.2(cor_ht20,  Rowv=as.dendrogram(rc), Colv=as.dendrogram(cc),
                        revC=T,density.info="none", trace="none", main= "Tree height 2")

############## vt10 #####################################################################
#reading a data file
cor_vt10 <- as.matrix(read.csv('cor_vt10.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_vt10)
is.numeric(cor_vt10)

#Use correlations between variables "as distance" for cluster analysis
dd_vt10 <- as.dist((1 - cor_vt10))
head(dd_vt10)
round(1000 * dd_vt10) # (prints more nicely)
plot(hclust(dd_vt10, method = "ward.D2"), cex = 0.6, main="Tree vitality 1")  #PLOTTING the cluster analysis

#Creating heatmap with dendrogram and cluster for vt10
rd<-as.dist((1 - cor_vt10))
rc<-hclust(rd)
cd<-as.dist(t(1-cor_vt10))
cc<-hclust(cd)

heatmap_vt10<-heatmap.2(cor_vt10,  Rowv=as.dendrogram(rc), Colv=as.dendrogram(cc),
                        revC=T,density.info="none", trace="none", main= "Tree vitality 1")

############## vt20 #####################################################################
#reading a data file
cor_vt20 <- as.matrix(read.csv('cor_vt20.csv', header=T, sep = ",", row.names=1, stringsAsFactors = FALSE))
head(cor_vt20)
is.numeric(cor_vt20)

#Use correlations between variables "as distance" for cluster analysis
dd_vt20 <- as.dist((1 - cor_vt20))
head(dd_vt20)
round(1000 * dd_vt20) # (prints more nicely)
plot(hclust(dd_vt20, method = "ward.D2"), cex = 0.6, main="Tree vitality 2")  #PLOTTING the cluster analysis


#Creating heatmap with dendrogram and cluster for vt20
rd<-as.dist((1 - cor_vt20))
rc<-hclust(rd)
cd<-as.dist(t(1-cor_vt20))
cc<-hclust(cd)

heatmap_vt20<-heatmap.2(cor_vt20,  Rowv=as.dendrogram(rc), Colv=as.dendrogram(cc),
                        revC=T,density.info="none", trace="none", main= "Tree vitality 2")


save.image("corr_graphs_UV.RData")


################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)
library(gridExtra)
ggarrange(clustH1, clustH2, clustV1, clustV2,labels = c("a", "b", "c","d"),
          common.legend = TRUE, legend = "bottom")


###Just testing how to use cluster and dendrogram
#rd<-dist(cor_ht10)
#rc<-hclust(rd)
#cd<-dist(t(cor_ht10))
#cc<-hclust(cd)
#heatmap_ht10<-heatmap.2(cor_ht10,  Rowv=as.dendrogram(rc), Colv=as.dendrogram(cc),
                        #revC=T,density.info="none", trace="none")
