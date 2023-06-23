# Remove everything in the working environment.
rm(list=ls())

#Setting working directory
getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/h2_graphs/MET_h2/presentations")

#getting packages
library(ggplot2)
library(MASS)



######################Plot of heritability for ht10 for all sites ###################################
sites_hjd10<-read.csv('sites_hjd10.csv',header=T, sep=",") #reading the data file
head(sites_hjd10)
tail(sites_hjd10)

ht10_sites <- ggplot(sites_hjd10, aes(x=site, y=h2i))+
  geom_bar(stat = "identity",position="dodge",fill="lightblue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Narrow sense heritability") + ggtitle("Tree height 1") +
  scale_y_continuous(limits=c(-0.025, 0.60)) + xlab(NULL) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=12)) +
  theme(axis.text.x = element_text(angle=60, hjust=1, size=12)) +
  theme(plot.title = element_text(colour = "black",size = 15, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))


######################Plot of heritability for ht20 for all sites ###################################
sites_hjd20<-read.csv('sites_hjd20.csv',header=T, sep=",") #reading the data file
head(sites_hjd20)
tail(sites_hjd20)

ht20_sites <- ggplot(sites_hjd20, aes(x=site, y=h2i))+
  geom_bar(stat = "identity",position="dodge",fill="lightblue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Narrow sense heritability") + ggtitle("Tree height 2") +
  scale_y_continuous(limits=c(-0.025, 0.60)) + xlab(NULL) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=12)) +
  theme(axis.text.x = element_text(angle=60, hjust=1, size=12)) +
  theme(plot.title = element_text(colour = "black",size = 15, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)
library(gridExtra)
figure <- ggarrange(ht10_sites, ht20_sites, 
                    common.legend = TRUE, legend = "bottom")

annotate_figure(figure,top = text_grob("Univariate multi-environment analyses: narrow-sense heritabilities", 
                                       color = "black", face = "bold", size = 20))
