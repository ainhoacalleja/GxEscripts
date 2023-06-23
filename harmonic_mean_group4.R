#compute the harmonic mean of number of trees per site in each group 
#setting working directory
setwd("~/Documents/PhD_UPSC/Project_2_GxE/GxE_analysis") 

#read the data file
group4 <- read.csv('group4.csv', header=T, sep=";")
head(group4)
summary(group4)
group44 <- read.csv('group44.csv', header=T, sep=",")
head(group44)

group45 <- read.csv('group45.csv', header=T, sep=",")
head(group45)


#create a data frame with the counts of trees within each site for hjd_10
trees_per_site4_hjd10 <- aggregate(hjd_10 ~ Family_id + site, data = group4, length)
head(trees_per_site4_hjd10)
#harmonic mean of number of trees per site for hjd_10
(nh4_hjd10 = 1/mean(1/trees_per_site4_hjd10$hjd_10)) 

#create a data frame with the counts of trees within each site for hjd_10_only 4 sites
trees_per_site44_hjd10 <- aggregate(hjd_10 ~ Family_id + site, data = group44, length)
head(trees_per_site44_hjd10)
#harmonic mean of number of trees per site for hjd_10
(nh44_hjd10 = 1/mean(1/trees_per_site44_hjd10$hjd_10)) 


#create a data frame with the counts of trees within each site for hjd_20
trees_per_site4_hjd20 <- aggregate(hjd_20 ~ Family_id + site, data = group4, length)
head(trees_per_site4_hjd20)
#harmonic mean of number of trees per site for hjd_20
(nh4_hjd20 = 1/mean(1/trees_per_site4_hjd20$hjd_20)) 

#create a data frame with the counts of trees within each site for vit_10
trees_per_site4_vit10 <- aggregate(vit_10 ~ Family_id + site, data = group4, length)
#harmonic mean of number of trees per site for vit_10
(nh4_vt10 = 1/mean(1/trees_per_site4_vit10$vit_10))



#create a data frame with the counts of trees within each site for vit_20
trees_per_site4_vit20 <- aggregate(vit_20 ~ Family_id + site, data = group4, length)
#harmonic mean of number of trees per site for vit_20
(nh4_vt20 = 1/mean(1/trees_per_site4_vit20$vit_20))

#create a data frame with the counts of trees within each site for nVit_10
trees_per_site4_vitnsc10 <- aggregate(vitnsc_10 ~ Family_id + site, data = group4, length)
#harmonic mean of number of trees per site for nVit_10
(nh4_nvt10 = 1/mean(1/trees_per_site4_vitnsc10$vitnsc_10))

#create a data frame with the counts of trees within each site for nVit_10, 4 sites
trees_per_site45_vitnsc10 <- aggregate(vitnsc_10 ~ Family_id + site, data = group45, length)
#harmonic mean of number of trees per site for nVit_10
(nh45_nvt10 = 1/mean(1/trees_per_site45_vitnsc10$vitnsc_10))

#create a data frame with the counts of trees within each site for nVit_20
trees_per_site4_vitnsc20 <- aggregate(vitnsc_20 ~ Family_id + site, data = group4, length)
#harmonic mean of number of trees per site for nVit_20
(nh4_nvt20 = 1/mean(1/trees_per_site4_vitnsc20$vitnsc_20))





