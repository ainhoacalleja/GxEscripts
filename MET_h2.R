# Remove everything in the working environment.
rm(list=ls())

#Setting working directory
getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/h2_graphs/MET_h2")

#getting packages
library(ggplot2)
library(MASS)

load("heritabilities_UV.RData")

######################Plot of narrow sense h2 for MET ###################################
h2indiv<-read.csv('MET_h2i.csv',header=T, sep=",") #reading the data file
head(h2indiv)
tail(h2indiv)

h2narrow <- ggplot(h2indiv, aes(x=trait, y=h2i, fill=trait))+
  geom_bar(stat = "identity",position="dodge") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) + 
  facet_wrap( ~ group, ncol=45) + ylab("individual narrow sense heritability") + ggtitle("UV MET analysis") +
  scale_y_continuous(limits=c(0, 0.3)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2i_MET.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2i_MET.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of famili mean h2 for MET ###################################
h2fam<-read.csv('MET_h2f.csv',header=T, sep=",") #reading the data file
head(h2fam)
tail(h2fam)

h2f <- ggplot(h2fam, aes(x=trait, y=h2f, fill=trait))+
  geom_bar(stat = "identity",position="dodge") + 
  geom_errorbar(aes(ymin=h2f-se, ymax=h2f+se),position=position_dodge(0.9), width=0.1) + 
  facet_wrap( ~ group, ncol=45) + ylab("heritability") + ggtitle("MET family mean heritability") +
  scale_y_continuous(limits=c(0, 0.9)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2f_MET.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2f_MET.png", width=12, height=6, units = "cm",dpi = 300)



################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)
ggarrange(ht10_group4,ht20_group4,nvit10S_group4, nvit20S_group4, labels = c("A", "B", "C","D"),
          common.legend = TRUE, legend = "bottom") %>% 
  ggexport(filename="graph1_gr4.pdf")

######################Plot of narrow sense h2 for sites and ht10 ###################################
#Setting working directory
getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/h2_graphs/MET_h2")

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
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Individual narrow sense heritability") + ggtitle("Tree height 1") +
  scale_y_continuous(limits=c(-0.025, 0.60)) + xlab(NULL) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_H1_UV.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_H1_uv.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of heritability for ht20 for all sites ###################################
sites_hjd20<-read.csv('sites_hjd20.csv',header=T, sep=",") #reading the data file
head(sites_hjd20)
tail(sites_hjd20)

ht20_sites <- ggplot(sites_hjd20, aes(x=site, y=h2i))+
  geom_bar(stat = "identity",position="dodge",fill="lightblue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Individual narrow sense heritability") + ggtitle("Tree height 2") +
  scale_y_continuous(limits=c(-0.025, 0.60)) + xlab(NULL) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_H2_UV.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_H2_UV.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of heritability for vit10 for all sites ###################################
sites_vit10<-read.csv('sites_vit10.csv',header=T, sep=",") #reading the data file
head(sites_vit10)
tail(sites_vit10)

vit10_sites <- ggplot(sites_vit10, aes(x=site, y=h2i))+
  geom_bar(stat = "identity",position="dodge",fill="lightblue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Individual narrow sense heritability") + ggtitle("Tree vitality 1") +
  scale_y_continuous(limits=c(-0.025, 0.60)) + xlab(NULL) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_vit10_sites.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_vit10_sites.png", width=12, height=6, units = "cm",dpi = 300)

######################Plot of heritability for vit20 for all sites ###################################
sites_vit20<-read.csv('sites_vit20.csv',header=T, sep=",") #reading the data file
head(sites_vit20)
tail(sites_vit20)

vit20_sites <- ggplot(sites_vit20, aes(x=site, y=h2i))+
  geom_bar(stat = "identity",position="dodge",fill="lightblue") + 
  geom_errorbar(aes(ymin=h2i-se, ymax=h2i+se),position=position_dodge(0.9), width=0.1) +
  facet_wrap( ~ group, scales = "free_x", ncol=4) + ylab("Individual narrow sense heritability") + ggtitle("Tree vitality 2") +
  scale_y_continuous(limits=c(-0.025, 0.60)) + xlab(NULL) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_vit20_sites.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_vit20_sites.png", width=12, height=6, units = "cm",dpi = 300)

################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)
library(gridExtra)
figure <- ggarrange(ht10_sites, ht20_sites, vit10_sites, vit20_sites,
          common.legend = TRUE, legend = "bottom")

annotate_figure(figure,top = text_grob("Univariate multi-environment analyses", 
                                       color = "black", face = "bold", size = 14))
# %>% 
  # ggexport(filename="UV_heritabilities_2.pdf")



save.image("heritabilities_UV.RData")
load("heritabilities_UV.RData")






