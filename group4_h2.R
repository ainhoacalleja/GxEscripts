#Setting working directory
getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/h2_graphs/group4_h2")

#getting packages
library(ggplot2)
library(MASS)

######################Plot of heritability for ht10 at group3 ###################################
hjd10<-read.csv('hjd10.csv',header=T, sep=";") #reading the data file
head(hjd10)
tail(hjd10)

ht10_group4 <- ggplot(hjd10, aes(x=Model, y=h2, fill=Model))+
  geom_bar(stat = "identity",position="dodge") + 
  geom_errorbar(aes(ymin=h2-se, ymax=h2+se),position=position_dodge(0.9), width=0.1) + 
  facet_wrap( ~ Trial, ncol=6) + ylab("heritability") + ggtitle("Height 10 group 4") +
  scale_y_continuous(limits=c(0, 0.4)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_Ht10_gr4.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_Ht10_gr4.png", width=12, height=6, units = "cm",dpi = 300)

#ggplot(hjd10, aes(x=Trial, y=h2, fill=Model))+geom_bar(stat = "identity",position="dodge") + 
#  geom_errorbar(aes(ymin=h2-se, ymax=h2+se),position=position_dodge(0.9), width=0.2) 
#############################################################################################
######################  Plot of heritability for ht20 at group3  ###################################
hjd20<-read.csv('hjd20.csv',header=T, sep=",") #reading the data file
head(hjd20)
tail(hjd20)

ht20_group4 <- ggplot(hjd20, aes(x=Model, y=h2, fill=Model))+
  geom_bar(stat = "identity",position="dodge") + 
  geom_errorbar(aes(ymin=h2-se, ymax=h2+se),position=position_dodge(0.9), width=0.1) + 
  facet_wrap( ~ Trial, ncol=6) + ylab("heritability") + ggtitle("Height 20 group 4") +
  scale_y_continuous(limits=c(0, 0.4)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))
  

ggsave("plot_h2_Ht20_gr4.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_Ht20_gr4.png", width=12, height=6, units = "cm",dpi = 300)

#############################################################################################

#############################################################################################
######################  Plot of heritability for nvit10 spatial at group3  ###################################
nvit10S<-read.csv('nvit10.csv',header=T, sep=",") #reading the data file
head(nvit10S)
tail(nvit10S)

nvit10S_group4 <- ggplot(nvit10S, aes(x=Model, y=h2, fill=Model))+
  geom_bar(stat = "identity",position="dodge") + 
  geom_errorbar(aes(ymin=h2-se, ymax=h2+se),position=position_dodge(0.9), width=0.2) + 
  facet_wrap( ~ Trial, ncol=6) + ylab("heritability") + ggtitle("Vitality age 10 group 4") +
  scale_y_continuous(limits=c(0, 0.35)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_nvit10S_gr4.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_nvit10S_gr4.png", width=12, height=6, units = "cm",dpi = 300)


######################  Plot of heritability for nvit20 spatial at group2  ###################################
nvit20S<-read.csv('nvit20.csv',header=T, sep=";") #reading the data file
head(nvit20S)
tail(nvit20S)

nvit20S_group4 <- ggplot(nvit20S, aes(x=Model, y=h2, fill=Model))+
  geom_bar(stat = "identity",position="dodge") + 
  geom_errorbar(aes(ymin=h2-se, ymax=h2+se),position=position_dodge(0.9), width=0.2) + 
  facet_wrap( ~ Trial, ncol=6) + ylab("heritability") + ggtitle("Vitality age 20 group 4") +
  scale_y_continuous(limits=c(0, 0.35)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6))

ggsave("plot_h2_nvit20S_gr4.pdf", width=12, height=6, units = "cm")
ggsave("plot_h2_nvit20S_gr4.png", width=12, height=6, units = "cm",dpi = 400)



################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)
ggarrange(ht10_group4,ht20_group4,nvit10S_group4, nvit20S_group4, labels = c("A", "B", "C","D"),
          common.legend = TRUE, legend = "bottom") %>% 
  ggexport(filename="graph1_gr4.pdf")









