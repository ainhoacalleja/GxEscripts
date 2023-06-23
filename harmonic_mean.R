#compute the harmonic mean of number of trees per site in each group 
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis")
group1 <- read.csv('group1.csv', header=T, sep="")
head(group1)
#create a data frame with the counts of trees within each plot
trees_per_plot <- aggregate(density ~ MALE + FEMALE + CROSS + SITE + BLOCK, data = diallel, length)
#harmonic mean of number of trees per plot
(nh = 1/mean(1/trees_per_plot$density)) 



