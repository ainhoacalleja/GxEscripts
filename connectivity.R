#Setting working directory
getwd()
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity")

#packagens I will probably need
library(MASS)
library(dplyr)
library(gmodels)

#Reading the data
#connections<-read.csv('connection.csv',header=T, sep=";")
#as.data.frame(connections, row.names = NULL, stringsAsFactors=T)
#head(connections)
#str(connections)
#summary(connections)
#tableconect<-CrossTable(connections$Site, connections$Mum_id, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
#write.csv(tableconect, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/tableconect.csv")

#fam<-select(connections, Site,Family_id)
#head(fam)

#connections<-read.csv('allsites.csv',header=T, sep=",")
connections <- read.delim("all.txt",  header = TRUE, sep = ",", dec = ".")
as.data.frame(connections, row.names = NULL, stringsAsFactors=T)
head(connections)
tail(connections)
str(connections)
summary(connections)
#tableconect<-CrossTable(connections$Site, connections$Mum_id, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
#write.csv(tableconect, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/tableconect.csv")

fam<-select(connections, Site,Family_id)
head(fam)
tail(fam)



#with MASS library

conect1<-with(connections, table(Mum_id, Site))
head(conect1)
prop.table(conect1)

conect2<-with(connections, table(Mum_id,Site))
head(conect2)
tail(conect2)

write.csv(conect1, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/conect1.csv")
write.csv(conect2, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/conect2.csv",col.names = TRUE)


conect3<- with(connections, table(Family_id, Site))
head(conect3)
tail(conect3)
write.csv(conect3, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/conect3.csv")

#giving values 0 and 1
data3 <-read.csv('conect3.csv',header=T,row.names=1,  sep=",")
head(data3)
str(data3)
data3[]<-lapply(data3, function(x) ifelse(x>0, 1, x)) ##this way I keep the rownames
head(data3)
tail(data3)


write.csv(data3, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/data3.csv")
#to change the 0 values to NA values

##################correlation plot with ggplot heatmap########################
library(ggplot2)
cormat <- round(cor(conect2),2)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
melted_cormat
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


########################################################################################

#One way of Giving value=1 to columns with values higher than 0.
data1<-read.csv('conect2.csv',header=T,  sep=",")
as.data.frame(data1)
#as.factor(data1$X)
head(data1)
tail(data1)


crosstab(data1, row.vars="Mum_id", col.vars="Site", type=t)

#data1[2:15]<-lapply(data1[2:15], function(x) ifelse(x>0, 1, x))
#head(data1)
#write.csv(data1, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/data1.csv")

#Another way
#data1 <- cbind(apply(data1[ ,2:16], 2, function(x) ifelse(x > 0, 1, x)), data1[ ,1])
data2 <-read.csv('conect2.csv',header=T,row.names=1,  sep=",")
head(data2)
str(data2)
data2[]<-lapply(data2, function(x) ifelse(x>0, 1, x)) ##this way I keep the rownames
head(data2)
tail(data2)


write.csv(data2, "~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis/connectivity/data2.csv")
#to change the 0 values to NA values


######Correlation plot with corrplot

library(corrplot)
library(RColorBrewer)
cor2<-cor(data2) 
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
plotcor1<-corrplot(cor2, type="full", order="hclust",  method="color", addgrid.col = "white",  tl.col = "black",
                   tl.srt=45, tl.cex= 0.7, number.cex = .7, addCoef.col = "black", diag = FALSE, 
                   col=brewer.pal(n=8, name="RdYlBu"), is.corr=TRUE)
plotcor2<-corrplot(cor2, order="hclust", method="number")

col3 <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
M <- cor(data2[1:15])
head(M)
heatmap(x = M, symm = TRUE, col = cm.colors(256))




#with missing values
data4<-read.csv('conect1.csv',header=T,row.names=1,  sep=",")
as.data.frame(data4)
head(data4)

cor3<-cor(data4)
head(cor3)
library(corrplot)
library(RColorBrewer)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
plotcor3<-corrplot(cor3, type="full", order="hclust",  method="color", addgrid.col = "white",  tl.col = "black",
                   tl.srt=45, tl.cex= 0.7, number.cex = .7, addCoef.col = "black", diag = FALSE, 
                   col=brewer.pal(n=8, name="RdYlBu"), is.corr=TRUE)
