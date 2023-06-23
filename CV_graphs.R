getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/MET_graphs/CV_graphs")

#getting packages
library(ggplot2)
library(MASS)
library(plyr)
library(scales)


############### graph with both CVa and CVp for height 10 #########################
#reading data file
cv2_ht10<-read.csv('CV2_ht10.csv',header=T, sep=",") #reading the data file
head(cv2_ht10)
tail(cv2_ht10)


ggplot(cv2_ht10, aes(x=CV, y=percentage, fill=CV)) +
  geom_bar(stat="identity", width = 0.9, position="dodge") +
  facet_wrap( ~ Site, ncol=19) + scale_y_continuous(limits=c(0, 40)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(axis.title.y = element_text(face="italic", colour="black", size=8)) +
  theme(plot.title = element_text(colour = "black",size = 10, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "grey",fill = NA, size = 0.5)) +
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.title = element_text(face = "italic", colour = "black", size = 6)) +
  geom_text(aes(label=percentage), position=position_dodge(.9),size=3)+
  ggtitle("Coefficient of variation for height at 10 years") + ylab("CV%")
 # coord_flip()

ggplot(cv2_ht10, aes(x=Site, y=percentage, fill=CV)) +
  geom_bar(stat="identity", width = 0.9, position="dodge") +
  geom_text(aes(label=percentage), position=position_dodge(.9),size=3)+
  ggtitle("Coefficient of variation for height at 10 years") + ylab("CV%")+ 
  theme(plot.title = element_text(colour = "black", hjust = 0.5)) + coord_flip() 


##### additive CV plots #############
#reading file 10 
cv_ht10<-read.csv('CV_ht10.csv', header=T, sep = ",")
head(cv_ht10)

#creating CV plot for height 10
ht10_cva<- ggplot(cv_ht10, aes(x=Site, y=CVa)) +
  geom_bar(stat="identity", fill="lightblue", width = 0.9, position="dodge") +
  geom_text(aes(label=CVa), vjust=1.5, position=position_dodge(.9),size=3)+
  ggtitle("Additive CV for height at 10 years") + ylab("percentage")+ 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(plot.title = element_text(colour = "black", hjust = 0.5)) #+ coord_flip()  

ggsave("ht10_cva.pdf", width=15, height=12, units = "cm")
ggsave("ht10_cva.png", width=15, height=12, units = "cm",dpi = 400)

#reading file
cv_ht20<-read.csv('CV_ht20.csv', header=T, sep = ",")
head(cv_ht20)

#creating CV plot for height 20
ht20_cva<- ggplot(cv_ht20, aes(x=Site, y=CVa)) +
  geom_bar(stat="identity", fill="#00CC66", width = 0.9, position="dodge") +
  geom_text(aes(label=CVa),vjust=1.5, position=position_dodge(.9),size=3)+
  ggtitle("Additive CV for height at 20 years") + ylab("percentage")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(plot.title = element_text(colour = "black", hjust = 0.5)) #+ coord_flip()

ggsave("ht20_cva.pdf", width=15, height=12, units = "cm")
ggsave("ht20_cva.png", width=15, height=12, units = "cm",dpi = 400)

#reading file
cv_vt10 <- read.csv('CV_vt10.csv', header=T, sep=",")
head(cv_vt10) 

#creating CV plot for vitality 10
vt10_cva<- ggplot(cv_vt10, aes(x=Site, y=CVa)) +
  geom_bar(stat="identity", fill="#FF9999", width = 0.9, position="dodge") +
  geom_text(aes(label=CVa), vjust=1.5, position=position_dodge(.9),size=3)+
  ggtitle("Additive CV for survival at 10 years") + ylab("percentage")+ 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(plot.title = element_text(colour = "black", hjust = 0.5)) #+ coord_flip() 

ggsave("vt10_cva.pdf", width=15, height=12, units = "cm")
ggsave("vt10_cva.png", width=15, height=12, units = "cm",dpi = 400)

#reading file
cv_vt20 <- read.csv('CV_vt20.csv', header=T, sep=",")
head(cv_vt20) 

#creating CV plot for vitality 20
vt20_cva<- ggplot(cv_vt20, aes(x=Site, y=CVa)) +
  geom_bar(stat="identity", fill="#CCCC66", width = 0.9, position="dodge") +
  geom_text(aes(label=CVa), vjust=1.5, position=position_dodge(.9),size=3)+
  ggtitle("Additive CV for survival at 20 years") + ylab("percentage")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(plot.title = element_text(colour = "black", hjust = 0.5)) #+ coord_flip()

ggsave("vt20_cva.pdf", width=15, height=12, units = "cm")
ggsave("vt20_cva.png", width=15, height=12, units = "cm",dpi = 400)

################################ arranging  plots in only one plot #############################
library(ggpubr)
library(magrittr)

ggarrange(ht10_cva,ht20_cva,vt10_cva,vt20_cva, labels = c("A", "B", "C", "D"),
          common.legend = TRUE, legend = "bottom") %>% 
  ggexport(filename="graph1_cva.pdf")

ggarrange(ht10_cva,ht20_cva,vt10_cva,vt20_cva, labels = c("A", "B", "C", "D"),
          common.legend = TRUE, legend = "bottom") %>% 
  ggexport(filename="graph1_cva.png", dpi= 400)






