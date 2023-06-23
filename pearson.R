# Remove everything in the working environment.
rm(list=ls())

setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/regression")
#save.image("regression.RData")
#getwd()
load("regression.RData")

reg1 <- read.csv("reg1.csv", header= T,stringsAsFactors = F, row.names =1)
head(reg1)

#correlations between typeA and Tsum
mod1 <- lm(Tsum ~ typeA, data=reg1)  # build linear regression model on full data
print(mod1)
summary(mod1)

scatter.smooth(x=reg1$typeA, y=reg1$Tsum, main="typeA ~ Tsum")
save.image("regression.RData")
cor(reg1$Tsum, reg1$typeA)

#library(Hmisc)
#rcorr(reg1$typeA,reg1$Tsum, type=c("pearson","spearman"))
install.packages("Cairo")
setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(type='cairo'))
options(device='x11')

library(ggpubr)
require(ggplot2)
require(magrittr)
require("Cairo")
 
typeA <- ggscatter(reg1, x = "Tsum", y = "typeA", size=4,
          add = "reg.line", conf.int = TRUE, conf.int.level = 0.95, cor.coef.size = 7 ,
          cor.coef = TRUE, cor.method = "pearson", 
          cor.coeff.args = list(label.x.npc = 0.7, label.y.npc = 1),
          xlab = "Tsum", ylab = "Type-A genetic correlations\n", 
          ggtheme = theme_linedraw(base_line_size=0.75,base_rect_size = 1.5))
        
          
#base_line_size = 0.25, base_rect_size = 0.5)

Acor<-typeA + font("xlab", size = 30) + font("ylab", size = 30) + font("xy.text", size = 20)

ggsave("TypeA_Tsum.pdf", dpi=320)


#######################################################################
###########################Type B for height##########################
library(ggpubr)
typeB_Ht<-read.csv("typeB_ht.csv", header = T, sep = ",", stringsAsFactors = F)
str(typeB_Ht)
typeB_Ht$Group<-as.factor(typeB_Ht$Group)
head(typeB_Ht)

typeB_Ht<- ggscatter(typeB_Ht, x = "Tsum_dif", y = "typeB", size = 6,
          add = "reg.line", conf.int = TRUE, conf.int.level = 0.95, cor.coef.size = 7,
          cor.coef = TRUE, cor.method = "pearson", shape = "Trait", 
          cor.coeff.args = list(label.x.npc = 0.7, label.y.npc = 1),
          label.x = 8, legend = "top",
          xlab = "Tsum difference", ylab = "Type-B genetic correlations\n",
          ggtheme = theme_linedraw(base_line_size = 0.75, base_rect_size = 1.5))

Bcor<-typeB_Ht + font("xlab", size = 30) + font("ylab", size = 30) + font("xy.text", size = 20) +
  theme(legend.position =  "none")
  #font("legend.title", size = 18) #+ font("legend.text", size = 16)

ggsave("TypeB_Tsumdiff_2.eps", dpi=320)


# eps(filename="~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/regression/TypeA_Tsum", 
#     width=1140, height=703, units=mm, res=600)

#ggpar(Acor,  ylim = c(0,0.8))

#rm(Acor)

###With ggplot
typeB_col <- ggscatter(typeB_Ht, x = "Tsum_dif", y = "typeB", size = 6,
                       color = "Trait", palette = "jco",
                       add = "reg.line", conf.int = TRUE, conf.int.level = 0.95, cor.coef.size = 7,
                       cor.coef = TRUE, cor.method = "pearson", shape = "Trait", 
                       cor.coeff.args = list(label.x.npc = 0.5, label.y.npc = 1),
                       label.x = 8, legend = "top",
                       xlab = "Tsum difference", ylab = "Type-B genetic correlations\n",
                       ggtheme = theme_linedraw(base_line_size = 0.75, base_rect_size = 1.5))
Bcor_col <- typeB_col + font("xlab", size = 25) + font("ylab", size = 25) + font("xy.text", size = 20) +
  theme(legend.position = c(0.9, 0.15)) + 
  theme(legend.text = element_text(size = 25)) +
  theme(legend.title = element_text(colour = "black", size = 25)) +
  theme(legend.background = element_rect(fill="white",
                                    size=0.5, linetype="solid", 
                                    colour ="black"))
  
  
typeA_col <- ggscatter(reg1, x = "Tsum", y = "typeA", size=4,
                   add = "reg.line", conf.int = TRUE, conf.int.level = 0.95, cor.coef.size = 7 ,
                   cor.coef = TRUE, cor.method = "pearson", 
                   cor.coeff.args = list(label.x.npc = 0.5, label.y.npc = 1),
                   xlab = "Tsum", ylab = "Type-A genetic correlations", 
                   ggtheme = theme_linedraw(base_line_size=0.75,base_rect_size = 1.5))

Acor_col<-typeA_col + font("xlab", size = 25) + font("ylab", size = 25) + font("xy.text", size = 20)

library(ggpubr) 
library(magrittr)
library(gridExtra)
library(ggpubr)
library(magrittr)
library(gridExtra)
#ggarrange(Bcor_col, Acor_col = c("A", "B"))
grid.arrange(Bcor_col, Acor_col, ncol=2, top=" \n ")   


# ggscatter(reg1, x = "Lat", y = "typeA",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Latitude", ylab = "typeA")

save.image("regression.RData")


# reg2_ht10 <- read.csv("reg2_ht10.csv", header= T,stringsAsFactors = F, row.names =1)
# head(reg2_ht10)
# summary(reg2_ht10)
# 
# ggscatter(reg2_ht10, x = "Tsum_dif", y = "h10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "Height 10 typeB correlation")
# 
# reg2 <- read.csv("reg2.csv", header= T,stringsAsFactors = F, row.names =1)
# head(reg2)
# summary(reg2)
# 
# ggscatter(reg2, x = "Tsum_dif", y = "h10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "Height 10 typeB correlation")
# 
# ggscatter(reg2, x = "Tsum_dif", y = "v10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "vitality 10 typeB correlation")
# 
# ggscatter(reg2, x = "Tsum_dif", y = "H10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "Height 20 typeB correlation")
# 
# ggscatter(reg2, x = "Tsum_dif", y = "V10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "Vitality 20 typeB correlation")
# 
# mod2 <- lm(Tsum_dif ~ v10, data=reg2)  # build linear regression model on full data
# print(mod2)
# summary(mod2)
# 
# gr3 <- read.csv("gr3.csv", header= T,stringsAsFactors = F, row.names =1)
# head(gr3)
# 
# gr2<-read.csv("gr2.csv", header=T, stringsAsFactors = F, row.names = 1)
# head(gr2)
# summary(gr2)
# 
# gr2<-read.csv("gr2.csv", header=T, stringsAsFactors = F, row.names = 1)
# head(gr2)
# summary(gr2)
# ggscatter(gr2, x = "Tsum_dif", y = "V20", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "Height 10 typeB correlation", title = "Group 2")
# 
# 
# gr2Ht<-read.csv("gr2_Ht.csv", header=T,sep = ",", stringsAsFactors = F)
# head(gr2Ht)
# summary(gr2Ht)
# 
# gr2Vt<- read.csv("gr2_Vt.csv", header=T, sep = ",", stringsAsFactors = F)
# head(gr2Vt)

# ggscatter(gr2Ht, x = "Tsum_dif", y = "typeB",
#           add = "reg.line",                         # Add regression line
#           conf.int = TRUE,                          # Add confidence interval
#           color = "Trait", palette = "jco",           # Color by groups "cyl"
#           shape = "Trait") + stat_cor(aes(color = Trait), label.x = 2 )                            # Change point shape by groups "cyl"
# 
# ggscatter(gr2Ht, x = "Tsum_dif", y = "typeB", size = 0.3,
#           color = "Trait", palette = "jco",
#           facet.by = "Trait", #scales = "free_x",
#           add = "reg.line", conf.int = TRUE, title = "Group 2") +
#   stat_cor(aes(color = Trait), method = "spearman", label.y = 1)

#Pearsson correlation

##### GROUP 2 ############
# gr2_ht<-read.csv("gr2_Ht.csv",header=T,sep = ",", stringsAsFactors = F)
# head(gr2_ht)
# 
# ggscatter(gr2_ht, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Height Group 2",
#           xlab = "Tsum", ylab = "typeB")
# 
# gr2_vt<-read.csv("gr2_Vt.csv", header = T, sep = ",", stringsAsFactors = F)
# head(gr2_vt)
# ggscatter(gr2_vt, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "trait", main="Vitality Group 2",
#           xlab = "Tsum", ylab = "typeB")
# 
# ##### GROUP 1 ############
# gr1_ht<-read.csv("gr1_Ht.csv",header=T,sep = ",", stringsAsFactors = F)
# head(gr1_ht)
# 
# ggscatter(gr1_ht, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Height Group 1",
#           xlab = "Tsum", ylab = "typeB")
# 
# gr1_vt<-read.csv("gr1_Vt.csv", header = T, sep = ",", stringsAsFactors = F)
# head(gr1_vt)
# ggscatter(gr1_vt, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Vitality Group 1",
#           xlab = "Tsum", ylab = "typeB")
# 
# ##### GROUP 4 ############
# gr4_ht<-read.csv("gr4_Ht.csv",header=T,sep = ",", stringsAsFactors = F)
# head(gr4_ht)
# 
# ggscatter(gr4_ht, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Height Group 4",
#           xlab = "Tsum", ylab = "typeB")
# 
# gr4_vt<-read.csv("gr4_Vt.csv", header = T, sep = ",", stringsAsFactors = F)
# head(gr4_vt)
# ggscatter(gr4_vt, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Vitality Group 4",
#           xlab = "Tsum", ylab = "typeB")





# ##### GROUP 3 ############
# gr3Ht<-read.csv("gr3_Ht.csv", header=T,sep = ",", stringsAsFactors = F)
# head(gr3Ht)
# summary(gr3Ht)
# 
# ggscatter(gr3Ht, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Height Group 3",
#           xlab = "Tsum", ylab = "typeB")
# 
# gr3Vt<- read.csv("gr3_Vt.csv", header=T, sep = ",", stringsAsFactors = F)
# head(gr3Vt)
# 
# ggscatter(gr3Vt, x = "Tsum_dif", y = "typeB",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson", shape = "Trait", main="Vitality Group 3",
#           xlab = "Tsum", ylab = "typeB")
# 
# gr3_heights<-ggscatter(gr3Ht, x = "Tsum_dif", y = "typeB",
#                        add = "reg.line",                                 # Add regression line
#                        conf.int = TRUE,                                  # Add confidence interval
#                        shape = "Trait",
#                        add.params = list(color = "blue",
#                                          fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(gr3_heights, title= "Group 3", ylab="type-B genetic correlation", xlab = "Tsum difference")
# 
# gr3_vit<-ggscatter(gr3Vt, x = "Tsum_dif", y = "typeB",
#                        add = "reg.line",                                 # Add regression line
#                        conf.int = TRUE,                                  # Add confidence interval
#                        shape = "Trait",
#                        add.params = list(color = "blue",
#                                          fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(gr3_vit, title= "Group 3", ylab="type-B genetic correlation", xlab = "Tsum difference")
# 
# ##### GROUP 1 ############
# gr1Ht<-read.csv("gr1_Ht.csv", header=T,sep = ",", stringsAsFactors = F)
# head(gr1Ht)
# summary(gr1Ht)
# 
# gr1Vt<- read.csv("gr1_Vt.csv", header=T, sep = ",", stringsAsFactors = F)
# head(gr1Vt)
# 
# gr1_heights<-ggscatter(gr1Ht, x = "Tsum_dif", y = "typeB",
#                        add = "reg.line",                                 # Add regression line
#                        conf.int = TRUE,                                  # Add confidence interval
#                        shape = "Trait",
#                        add.params = list(color = "blue",
#                                          fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(gr1_heights, title= "Group 1", ylab="type-B genetic correlation", xlab = "Tsum difference")
# 
# gr1_vit<-ggscatter(gr1Vt, x = "Tsum_dif", y = "typeB",
#                    add = "reg.line",                                 # Add regression line
#                    conf.int = TRUE,                                  # Add confidence interval
#                    shape = "Trait",
#                    add.params = list(color = "blue",
#                                      fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(gr1_vit, title= "Group 1", ylab="type-B genetic correlation", xlab = "Tsum difference")
# 
# ##### GROUP 4 ############
# gr4Ht<-read.csv("gr4_Ht.csv", header=T,sep = ",", stringsAsFactors = F)
# head(gr4Ht)
# summary(gr4Ht)
# 
# gr4Vt<- read.csv("gr4_Vt.csv", header=T, sep = ",", stringsAsFactors = F)
# head(gr4Vt)
# 
# gr4_heights<-ggscatter(gr4Ht, x = "Tsum_dif", y = "typeB",
#                        add = "reg.line",                                 # Add regression line
#                        conf.int = TRUE,                                  # Add confidence interval
#                        shape = "Trait",
#                        add.params = list(color = "blue",
#                                          fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(gr4_heights, title= "Group 4", ylab="type-B genetic correlation", xlab = "Tsum difference")
# 
# gr4_vit<-ggscatter(gr4Vt, x = "Tsum_dif", y = "typeB",
#                    add = "reg.line",                                 # Add regression line
#                    conf.int = TRUE,                                  # Add confidence interval
#                    shape = "Trait",
#                    add.params = list(color = "blue",
#                                      fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(gr4_vit, title= "Group 4", ylab="type-B genetic correlation", xlab = "Tsum difference")
# 
# ###############################
# #          TYPE AB            #
# ###############################
# AB_gr2<-read.csv("typeAB_gr2.csv", header=T, sep = ",", stringsAsFactors = F)
# head(AB_gr2)
# 
# AB2<-ggscatter(AB_gr2, x = "Tsum_diff", y = "typeAB",
#                    add = "reg.line",                                 # Add regression line
#                    conf.int = TRUE,                                  # Add confidence interval
#                    shape = "age",
#                    add.params = list(color = "blue",
#                                      fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(AB2, title= "Group 2", ylab="type-AB genetic correlation", xlab = "Tsum difference")
# 
# 
# ab_all<-read.csv("typeAB_all.csv", header = T, sep = ",", stringsAsFactors = F)
# head(ab_all)
# 
# ab_graph<-ggscatter(ab_all, x = "Tsum_diff", y = "typeAB",
#                        add = "reg.line",                                 # Add regression line
#                        conf.int = TRUE,                                  # Add confidence interval
#                        shape = "age",
#                        add.params = list(color = "blue",
#                                          fill = "lightgray"),
# )+ stat_cor(method = "pearson", label.y = 1)  # Add correlation coefficient
# 
# ggpar(ab_graph, title= "Groups 2 and 3", ylab="type-AB genetic correlations", xlab = "Tsum difference")
# 
# ggscatter(ab_all, x = "Tsum_diff", y = "typeAB",
#           add = "reg.line",                         # Add regression line
#           conf.int = TRUE,                          # Add confidence interval
#           color = "group", palette = "jco",           # Color by groups "cyl"
#           shape = "age"                             # Change point shape by groups "cyl"
# )+
#   stat_cor(aes(color = group), label.x = 3)  
# 
# 
# ab_all2<-read.csv("typeAB_all2.csv", header = T, sep = ",", stringsAsFactors = F)
# head(ab_all2)
# 
# ggscatter(ab_all2, x = "Tsum_diff", y = "typeAB",
#           add = "reg.line",                         # Add regression line
#           conf.int = TRUE,                          # Add confidence interval
#           color = "group", palette = "jco",           # Color by groups "cyl"
#           shape = "age"                             # Change point shape by groups "cyl"
# )+
#   stat_cor(aes(color = group), label.x = 3)  
# 
# ab_all3<-read.csv("typeAB_all3.csv", header = T, sep = ",", stringsAsFactors = F)
# head(ab_all3)
# 
# ggscatter(ab_all3, x = "Tsum_diff", y = "typeAB",
#           add = "reg.line",                         # Add regression line
#           conf.int = TRUE,                          # Add confidence interval
#           color = "group", palette = "jco",           # Color by groups "cyl"
#           shape = "age"                             # Change point shape by groups "cyl"
# )+
#   stat_cor(aes(color = group), label.x = 3) 

#######################################################################
###########################Type B for height##########################
library(ggpubr)
typeB_Ht<-read.csv("typeB_ht.csv", header = T, sep = ",", stringsAsFactors = F)
str(typeB_Ht)
typeB_Ht$Group<-as.factor(typeB_Ht$Group)
head(typeB_Ht)

ggscatter(typeB_Ht, x = "Tsum_dif", y = "typeB",
          add = "reg.line", conf.int = TRUE, conf.int.level = 0.95,
          cor.coef = TRUE, cor.method = "pearson", shape = "Trait", 
          xlab = "Tsum difference", ylab = "Type-B genetic correlations")


# ggscatter(typeB_Ht, x = "Tsum_dif", y = "typeB",
#           add = "reg.line",                         # Add regression line
#           conf.int = TRUE,                          # Add confidence interval
#           color = "Group", palette = "jco",           # Color by groups "cyl"
#           shape = "Trait"                             # Change point shape by groups "cyl"
# )+
#   stat_cor(aes(color = Group), label.x = 3) 

typeB_all<-read.csv("typeB_all.csv", header = T, sep = ",", stringsAsFactors = F)
str(typeB_all)
typeB_all$Group<-as.factor(typeB_all$Group)

ggscatter(typeB_all, x = "Tsum_dif", y = "typeB",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", shape = "Trait", 
          xlab = "Tsum", ylab = "typeB")

typeB_vt<-read.csv("typeB_vt_all.csv", header = T, sep = ",", stringsAsFactors = F)
str(typeB_vt)
head(typeB_vt)
typeB_vt$Group<-as.factor(typeB_vt$Group)

ggscatter(typeB_vt, x = "Tsum_dif", y = "typeB",
          add = "reg.line",                         # Add regression line
          conf.int = TRUE,                          # Add confidence interval
          color = "Group", palette = "jco",           # Color by groups "cyl"
          shape = "Trait"                             # Change point shape by groups "cyl"
)+
  stat_cor(aes(color = Group), label.x = 3)

ggscatter(typeB_vt, x = "Tsum_dif", y = "typeB",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", shape = "Trait", 
          xlab = "Tsum", ylab = "typeB")

# reg3 <- read.csv("reg3.csv", header= T,stringsAsFactors = F, row.names=1)
# head(reg3)
# summary(reg3)
# 
# 
# 
# ggscatter(reg3, x = "Tsum_dif", y = "typeB", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "typeB correlation")

# gr1 <- read.csv("gr1.csv", header= T,stringsAsFactors = F, row.names =1)
# head(gr1)
# summary(gr1)
# 
# 
# gr1 <- read.csv("gr1.csv", header= T,stringsAsFactors = F, row.names =1)
# head(gr1)
# summary(gr1)
# 
# ggscatter(gr1, x = "Tsum_dif", y = "h10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "Height 10 typeB correlation",title="Group 1")
# 
# ggscatter(gr1, x = "Tsum_dif", y = "v10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "vitality 10 typeB correlation", title = "Group 1")
# ggscatter(gr1, x = "Tsum_dif", y = "V10", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, 
#           cor.method = "pearson",xlab = "Tsum difference", ylab = "vitality 20 typeB correlation", title = "Group 1")



save.image("regression.RData")
