# Remove everything in the working environment.
rm(list=ls())

#Working directory
setwd("~/Documents/PhD_UPSC/Project_2_GxE/DATAPLAN_data")

#Trial F560. Reading the datafile
trial1 <- read.csv("F560_D.csv", header= T,stringsAsFactors = T, row.names =NULL, sep = ";")
head(trial1)
str(trial1)
summary(trial1)

# mean(trial1$Hjd_7, na.rm = TRUE)
# sd(trial1$Hjd_7, na.rm = TRUE)

# using subset function  to select dead and alive trees
t1_dead <- subset(trial1, Vit_19 == 0, select=c(Genotype_id, Hjd_12, Vit_19))
head(t1_dead)
summary(t1_dead)

t1_alive <- subset(trial1, Vit_19 >= 1, select =c(Genotype_id, Hjd_12, Vit_19))
head(t1_alive)
summary(t1_alive)

############################################
#Trial F561. Reading the datafile
trial2 <- read.csv("F561_D.csv", header= T,stringsAsFactors = T, row.names =NULL, sep = ";")
head(trial2)
str(trial2)
summary(trial2)


# using subset function  to select dead and alive trees
t2_dead <- subset(trial2, Vit_19 == 0, select=c(Genotype_id, Hjd_12, Vit_19))
head(t2_dead)
summary(t2_dead)

t2_alive <- subset(trial2, Vit_19 >= 1, select =c(Genotype_id, Hjd_12, Vit_19))
head(t2_alive)
summary(t2_alive)

############################################
#Trial F565. Reading the datafile
trial3 <- read.csv("F565_D.csv", header= T,stringsAsFactors = T, row.names =NULL, sep = ";")
head(trial3)
str(trial3)
summary(trial3)


# using subset function  to select dead and alive trees
t3_dead <- subset(trial3, Vit_19 == 0, select=c(Genotype_id, Hjd_11, Vit_19))
head(t3_dead)
summary(t3_dead)

t3_alive <- subset(trial3, Vit_19 >= 1, select =c(Genotype_id, Hjd_11, Vit_19))
head(t3_alive)
summary(t3_alive)

############################################
#Trial F566. Reading the datafile
trial4 <- read.csv("F566_D.csv", header= T,stringsAsFactors = T, row.names =NULL, sep = ";")
head(trial4)
str(trial4)
summary(trial4)


# using subset function  to select dead and alive trees
t4_dead <- subset(trial4, Vit_19 == 0, select=c(Genotype_id, Hjd_11, Vit_19))
head(t4_dead)
summary(t4_dead)

t4_alive <- subset(trial4, Vit_19 >= 1, select =c(Genotype_id, Hjd_11, Vit_19))
head(t4_alive)
summary(t4_alive)

############################################
#Trial F567. Reading the datafile
trial5 <- read.csv("F567_D.csv", header= T,stringsAsFactors = T, row.names =NULL,sep = ";")
head(trial5)
str(trial5)
summary(trial5)


# using subset function  to select dead and alive trees
t5_dead <- subset(trial5, Vit_19 == 0, select=c(Genotype_id, Hjd_11, Vit_19))
head(t5_dead)
summary(t5_dead)


t5_alive <- subset(trial5, Vit_19 >= 1, select =c(Genotype_id, Hjd_11, Vit_19))
head(t5_alive)
summary(t5_alive)

save.image("trialseries4.RData")

