rm(list=ls())

#Setting working directory
getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/h2_graphs_multi")

#getting packages
library(ggplot2)
library(MASS)

#load("MV_heritabilities.RData")

######################Plot of heritability for ht10 for MV ###################################
sites_h10<-read.csv('sites_h10.csv',header=T, sep=",") #reading the data file
head(sites_h10)
tail(sites_h10)

mvh10_h2 <- ggplot(sites_h10, aes(x=Trials, y=h2i))+
  geom_bar(stat = "identity",position="dodge", fill="blue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  scale_y_continuous(limits=c(0.00, 0.60)) + xlab(NULL) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Narrow sense heritability") + ggtitle("Tree height 1") +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_Ht10_mv.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_Ht10_mv.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of heritability for ht20 for MV ###################################
sites_h20<-read.csv('sites_h20.csv',header=T, sep=",") #reading the data file
head(sites_h20)
tail(sites_h20)

mvh20_h2 <- ggplot(sites_h20, aes(x=Trials, y=h2i))+
  geom_bar(stat = "identity",position="dodge", fill="blue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  scale_y_continuous(limits=c(0.00, 0.60)) + xlab(NULL) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Narrow sense heritability") + ggtitle("Tree height 2") +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_Ht20_mv.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_Ht20_mv.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of heritability for vt10 for MV ###################################
sites_v10<-read.csv('sites_v10.csv',header=T, sep=",") #reading the data file
head(sites_v10)
tail(sites_v10)

mvv10_v2 <- ggplot(sites_v10, aes(x=Trials, y=h2i))+
  geom_bar(stat = "identity",position="dodge", fill="blue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  scale_y_continuous(limits=c(0.00, 0.60)) + xlab(NULL) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Narrow sense heritability") + ggtitle("Tree vitality 1") +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_v10_mv.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_v10_mv.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of heritability for vt20 for MV ###################################
sites_v20<-read.csv('sites_v20.csv',header=T, sep=",") #reading the data file
head(sites_v20)
tail(sites_v20)

mvv20_v2 <- ggplot(sites_v20, aes(x=Trials, y=h2i))+
  geom_bar(stat = "identity",position="dodge", fill="blue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  scale_y_continuous(limits=c(0.00, 0.60)) + xlab(NULL) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Narrow sense heritability") + ggtitle("Tree vitality 2") +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_v20_mv.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_v20_mv.png", width=12, height=6, units = "cm",dpi = 300)



################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)
library(gridExtra)
library(ggpubr)
library(magrittr)
library(gridExtra)
figure2 <- ggarrange(mvh10_h2, mvh20_h2, mvv10_v2, mvv20_v2,
          common.legend = TRUE, legend = "bottom") 

annotate_figure(figure2,top = text_grob("Multivariate multi-environment analyses", 
                                       color = "black", face = "bold", size = 14))

#%>% 
 # ggexport(filename="MV_heritabilities.pdf")

save.image("MV_heritabilities.RData")
