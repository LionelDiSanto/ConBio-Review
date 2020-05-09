########################
# Results Simulation 2 #
########################

###
# Aim
###

# Test whether sampling of populations based on environmental/geographic 
# distance matrices perform better than random sampling of populations 
# under a realistic within-population sampling scenario. 
# Variables tested: Fst and % allelic diversity captured. Specifically,
# this script performs all calculations necessary to create Figure 3.

################################
# Libraries and import of data #
################################
library(simpleboot)
library(boot)

### B_maximo ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/B_maximo/Simulation2')
temp = list.files(pattern = '*.csv');temp
B_maximo = lapply(temp, read.csv)

### C_solstitialis ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/C_solstitialis/Simulation2')
temp = list.files(pattern = '*.csv');temp
C_solstitialis = lapply(temp, read.csv)

### H_annuus ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_annuus/Simulation2')
temp = list.files(pattern = '*.csv');temp
H_annuus = lapply(temp, read.csv)

### M_guttatus ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_guttatus/Simulation2')
temp = list.files(pattern = '*.csv');temp
M_guttatus = lapply(temp, read.csv)

### N_papyraceus ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus/Simulation2')
temp = list.files(pattern = '*.csv');temp
N_papyraceus = lapply(temp, read.csv)

### N_alpina ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_alpina/Simulation2')
temp = list.files(pattern = '*.csv');temp
N_alpina = lapply(temp, read.csv)

### N_glauca ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_glauca/Simulation2')
temp = list.files(pattern = '*.csv');temp
N_glauca = lapply(temp, read.csv)

### N_obliqua ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_obliqua/Simulation2')
temp = list.files(pattern = '*.csv');temp
N_obliqua = lapply(temp, read.csv)

### P_balsamifera ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera/Simulation2')
temp = list.files(pattern = '*.csv');temp
P_balsamifera = lapply(temp, read.csv)

### P_sitchensis ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis/Simulation2')
temp = list.files(pattern = '*.csv');temp
P_sitchensis = lapply(temp, read.csv)

### P_tremula ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula/Simulation2')
temp = list.files(pattern = '*.csv');temp
P_tremula = lapply(temp, read.csv)

################################
# EnvBased - Random comparison #
################################
##### Fst ####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rG <-N_glauca[[2]]-N_glauca[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,3,5,7)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_alpina[[2]]-N_alpina[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_obliqua[[2]]-N_obliqua[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(5,9,14,19)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-H_annuus[[2]]-H_annuus[[11]] 
rGm <- apply(rG,2,mean)
y=rGm[c(4,7,10,14)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-M_guttatus[[2]]-M_guttatus[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,4,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_papyraceus[[2]]-N_papyraceus[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,11,18,24)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-C_solstitialis[[2]]-C_solstitialis[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,10,16,22)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rG <-B_maximo[[2]]-B_maximo[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(13,22,34,46)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_balsamifera[[3]]-P_balsamifera[[21]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_sitchensis[[3]]-P_sitchensis[[21]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[4]]-P_tremula[[31]] # Ctr
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[5]]-P_tremula[[32]] # Def
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rG <-P_balsamifera[[4]]-P_balsamifera[[22]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_tremula[[6]]-P_tremula[[33]] # Selected
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_sitchensis[[4]]-P_sitchensis[[22]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

### Create Summary File ###
neutral <- matrix(nrow=(nrow(grpneutral)*ncol(grpneutral)), ncol = 3)
c <- 0
for(j in 1:ncol(grpneutral)){
  for(i in 1:nrow(grpneutral)){
    neutral[c+1,1] <- grpneutral[i,j]
    c <- c+1
  }
}
neutral <- data.frame(neutral)
neutral[,2] <- rep(c('1','2','3','4'), each=nrow(grpneutral)); neutral[,2] <- as.numeric(neutral[,2])
neutral[,3] <- rep('Neutral', times=nrow(grpneutral))
colnames(neutral) <- c('values','interval','Category');neutral

functional <- matrix(nrow=(nrow(grpfunctional)*ncol(grpfunctional)), ncol = 3)
c <- 0
for(j in 1:ncol(grpfunctional)){
  for(i in 1:nrow(grpfunctional)){
    functional[c+1,1] <- grpfunctional[i,j]
    c <- c+1
  }
}
functional <- data.frame(functional)
functional[,2] <- rep(c('1','2','3','4'), each=nrow(grpfunctional)); functional[,2] <- as.numeric(functional[,2])
functional[,3] <- rep('Functional', times=nrow(grpfunctional))
colnames(functional) <- c('values','interval','Category');functional

selected <- matrix(nrow=(nrow(grpselected)*ncol(grpselected)), ncol = 3)
c <- 0
for(j in 1:ncol(grpselected)){
  for(i in 1:nrow(grpselected)){
    selected[c+1,1] <- grpselected[i,j]
    c <- c+1
  }
}
selected <- data.frame(selected)
selected[,2] <- rep(c('1','2','3','4'), each=nrow(grpselected)); selected[,2] <- as.numeric(selected[,2])
selected[,3] <- rep('Selected', times=nrow(grpselected))
colnames(selected) <- c('values','interval','Category');selected

temp <- matrix(ncol=5, nrow=12);temp[,1]<- rep(c('Neutral','Functional','Selected'),each=4); temp[,2] <-rep(c(1,2,3,4), times=3)
for(i in 1:4){
  neut <- one.boot(neutral$values[neutral$interval==i], mean, R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean, R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean, R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='Fst_mean_EnvRand.csv', row.names = F)
write.csv(result, file='Fst_EnvRand.csv', row.names = F)

##### Genetic Diversity Captured #####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rG <-N_glauca[[3]]-N_glauca[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,3,5,7)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_alpina[[3]]-N_alpina[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_obliqua[[3]]-N_obliqua[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(5,9,14,19)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-H_annuus[[3]]-H_annuus[[12]] 
rGm <- apply(rG,2,mean)
y=rGm[c(4,7,10,14)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-M_guttatus[[3]]-M_guttatus[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,4,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_papyraceus[[3]]-N_papyraceus[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,11,18,24)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-C_solstitialis[[3]]-C_solstitialis[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,10,16,22)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rG <-B_maximo[[3]]-B_maximo[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(13,22,34,46)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_balsamifera[[5]]-P_balsamifera[[23]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_sitchensis[[5]]-P_sitchensis[[23]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[7]]-P_tremula[[34]] # Ctr
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[8]]-P_tremula[[35]] # Def
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rG <-P_balsamifera[[6]]-P_balsamifera[[24]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_tremula[[9]]-P_tremula[[36]] # Selected
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_sitchensis[[6]]-P_sitchensis[[24]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

### Create Summary File ###
neutral <- matrix(nrow=(nrow(grpneutral)*ncol(grpneutral)), ncol = 3)
c <- 0
for(j in 1:ncol(grpneutral)){
  for(i in 1:nrow(grpneutral)){
    neutral[c+1,1] <- grpneutral[i,j]
    c <- c+1
  }
}
neutral <- data.frame(neutral)
neutral[,2] <- rep(c('1','2','3','4'), each=nrow(grpneutral)); neutral[,2] <- as.numeric(neutral[,2])
neutral[,3] <- rep('Neutral', times=nrow(grpneutral))
colnames(neutral) <- c('values','interval','Category');neutral

functional <- matrix(nrow=(nrow(grpfunctional)*ncol(grpfunctional)), ncol = 3)
c <- 0
for(j in 1:ncol(grpfunctional)){
  for(i in 1:nrow(grpfunctional)){
    functional[c+1,1] <- grpfunctional[i,j]
    c <- c+1
  }
}
functional <- data.frame(functional)
functional[,2] <- rep(c('1','2','3','4'), each=nrow(grpfunctional)); functional[,2] <- as.numeric(functional[,2])
functional[,3] <- rep('Functional', times=nrow(grpfunctional))
colnames(functional) <- c('values','interval','Category');functional

selected <- matrix(nrow=(nrow(grpselected)*ncol(grpselected)), ncol = 3)
c <- 0
for(j in 1:ncol(grpselected)){
  for(i in 1:nrow(grpselected)){
    selected[c+1,1] <- grpselected[i,j]
    c <- c+1
  }
}
selected <- data.frame(selected)
selected[,2] <- rep(c('1','2','3','4'), each=nrow(grpselected)); selected[,2] <- as.numeric(selected[,2])
selected[,3] <- rep('Selected', times=nrow(grpselected))
colnames(selected) <- c('values','interval','Category');selected

temp <- matrix(ncol=5, nrow=12);temp[,1]<- rep(c('Neutral','Functional','Selected'),each=4); temp[,2] <-rep(c(1,2,3,4), times=3)
for(i in 1:4){
  neut <- one.boot(neutral$values[neutral$interval==i], mean, R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean, R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean, R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='GDcapt_mean_EnvRand.csv', row.names = F)
write.csv(result, file='GDcapt_EnvRand.csv', row.names = F)

################################
# GeoBased - Random comparison #
################################
##### Fst ####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rG <-N_glauca[[5]]-N_glauca[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,3,5,7)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_alpina[[5]]-N_alpina[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_obliqua[[5]]-N_obliqua[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(5,9,14,19)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-H_annuus[[5]]-H_annuus[[11]] 
rGm <- apply(rG,2,mean)
y=rGm[c(4,7,10,14)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-M_guttatus[[5]]-M_guttatus[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,4,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_papyraceus[[5]]-N_papyraceus[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,11,18,24)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-C_solstitialis[[5]]-C_solstitialis[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,10,16,22)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rG <-B_maximo[[5]]-B_maximo[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(13,22,34,46)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_balsamifera[[9]]-P_balsamifera[[21]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_sitchensis[[9]]-P_sitchensis[[21]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[13]]-P_tremula[[31]] # Ctr
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[14]]-P_tremula[[32]] # Def
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rG <-P_balsamifera[[10]]-P_balsamifera[[22]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_tremula[[15]]-P_tremula[[33]] # Selected
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_sitchensis[[10]]-P_sitchensis[[22]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

### Create Summary File ###
neutral <- matrix(nrow=(nrow(grpneutral)*ncol(grpneutral)), ncol = 3)
c <- 0
for(j in 1:ncol(grpneutral)){
  for(i in 1:nrow(grpneutral)){
    neutral[c+1,1] <- grpneutral[i,j]
    c <- c+1
  }
}
neutral <- data.frame(neutral)
neutral[,2] <- rep(c('1','2','3','4'), each=nrow(grpneutral)); neutral[,2] <- as.numeric(neutral[,2])
neutral[,3] <- rep('Neutral', times=nrow(grpneutral))
colnames(neutral) <- c('values','interval','Category');neutral

functional <- matrix(nrow=(nrow(grpfunctional)*ncol(grpfunctional)), ncol = 3)
c <- 0
for(j in 1:ncol(grpfunctional)){
  for(i in 1:nrow(grpfunctional)){
    functional[c+1,1] <- grpfunctional[i,j]
    c <- c+1
  }
}
functional <- data.frame(functional)
functional[,2] <- rep(c('1','2','3','4'), each=nrow(grpfunctional)); functional[,2] <- as.numeric(functional[,2])
functional[,3] <- rep('Functional', times=nrow(grpfunctional))
colnames(functional) <- c('values','interval','Category');functional

selected <- matrix(nrow=(nrow(grpselected)*ncol(grpselected)), ncol = 3)
c <- 0
for(j in 1:ncol(grpselected)){
  for(i in 1:nrow(grpselected)){
    selected[c+1,1] <- grpselected[i,j]
    c <- c+1
  }
}
selected <- data.frame(selected)
selected[,2] <- rep(c('1','2','3','4'), each=nrow(grpselected)); selected[,2] <- as.numeric(selected[,2])
selected[,3] <- rep('Selected', times=nrow(grpselected))
colnames(selected) <- c('values','interval','Category');selected

temp <- matrix(ncol=5, nrow=12);temp[,1]<- rep(c('Neutral','Functional','Selected'),each=4); temp[,2] <-rep(c(1,2,3,4), times=3)
for(i in 1:4){
  neut <- one.boot(neutral$values[neutral$interval==i], mean, R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean, R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean, R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='Fst_mean_GeoRand.csv', row.names = F)
write.csv(result, file='Fst_GeoRand.csv', row.names = F)

##### Genetic Diversity Captured #####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rG <-N_glauca[[6]]-N_glauca[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,3,5,7)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_alpina[[6]]-N_alpina[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_obliqua[[6]]-N_obliqua[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(5,9,14,19)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-H_annuus[[6]]-H_annuus[[12]] 
rGm <- apply(rG,2,mean)
y=rGm[c(4,7,10,14)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-M_guttatus[[6]]-M_guttatus[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,4,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_papyraceus[[6]]-N_papyraceus[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,11,18,24)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-C_solstitialis[[6]]-C_solstitialis[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,10,16,22)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rG <-B_maximo[[6]]-B_maximo[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(13,22,34,46)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_balsamifera[[11]]-P_balsamifera[[23]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_sitchensis[[11]]-P_sitchensis[[23]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[16]]-P_tremula[[34]] # Ctr
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[17]]-P_tremula[[35]] # Def
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rG <-P_balsamifera[[12]]-P_balsamifera[[24]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_tremula[[18]]-P_tremula[[36]] # Selected
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_sitchensis[[12]]-P_sitchensis[[24]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

### Create Summary File ###
neutral <- matrix(nrow=(nrow(grpneutral)*ncol(grpneutral)), ncol = 3)
c <- 0
for(j in 1:ncol(grpneutral)){
  for(i in 1:nrow(grpneutral)){
    neutral[c+1,1] <- grpneutral[i,j]
    c <- c+1
  }
}
neutral <- data.frame(neutral)
neutral[,2] <- rep(c('1','2','3','4'), each=nrow(grpneutral)); neutral[,2] <- as.numeric(neutral[,2])
neutral[,3] <- rep('Neutral', times=nrow(grpneutral))
colnames(neutral) <- c('values','interval','Category');neutral

functional <- matrix(nrow=(nrow(grpfunctional)*ncol(grpfunctional)), ncol = 3)
c <- 0
for(j in 1:ncol(grpfunctional)){
  for(i in 1:nrow(grpfunctional)){
    functional[c+1,1] <- grpfunctional[i,j]
    c <- c+1
  }
}
functional <- data.frame(functional)
functional[,2] <- rep(c('1','2','3','4'), each=nrow(grpfunctional)); functional[,2] <- as.numeric(functional[,2])
functional[,3] <- rep('Functional', times=nrow(grpfunctional))
colnames(functional) <- c('values','interval','Category');functional

selected <- matrix(nrow=(nrow(grpselected)*ncol(grpselected)), ncol = 3)
c <- 0
for(j in 1:ncol(grpselected)){
  for(i in 1:nrow(grpselected)){
    selected[c+1,1] <- grpselected[i,j]
    c <- c+1
  }
}
selected <- data.frame(selected)
selected[,2] <- rep(c('1','2','3','4'), each=nrow(grpselected)); selected[,2] <- as.numeric(selected[,2])
selected[,3] <- rep('Selected', times=nrow(grpselected))
colnames(selected) <- c('values','interval','Category');selected

temp <- matrix(ncol=5, nrow=12);temp[,1]<- rep(c('Neutral','Functional','Selected'),each=4); temp[,2] <-rep(c(1,2,3,4), times=3)
for(i in 1:4){
  neut <- one.boot(neutral$values[neutral$interval==i], mean, R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean, R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean, R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='GDcapt_mean_GeoRand.csv', row.names = F)
write.csv(result, file='GDcapt_GeoRand.csv', row.names = F)

###################################
# GeoEnvBased - Random comparison #
###################################
##### Fst ####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rG <-N_glauca[[8]]-N_glauca[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,3,5,7)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_alpina[[8]]-N_alpina[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_obliqua[[8]]-N_obliqua[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(5,9,14,19)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-H_annuus[[8]]-H_annuus[[11]] 
rGm <- apply(rG,2,mean)
y=rGm[c(4,7,10,14)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-M_guttatus[[8]]-M_guttatus[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,4,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_papyraceus[[8]]-N_papyraceus[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,11,18,24)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-C_solstitialis[[8]]-C_solstitialis[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,10,16,22)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rG <-B_maximo[[8]]-B_maximo[[11]]
rGm <- apply(rG,2,mean)
y=rGm[c(13,22,34,46)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_balsamifera[[15]]-P_balsamifera[[21]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_sitchensis[[15]]-P_sitchensis[[21]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[22]]-P_tremula[[31]] # Ctr
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[23]]-P_tremula[[32]] # Def
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rG <-P_balsamifera[[16]]-P_balsamifera[[22]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_tremula[[24]]-P_tremula[[33]] # Selected
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_sitchensis[[16]]-P_sitchensis[[22]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

### Create Summary File ###
neutral <- matrix(nrow=(nrow(grpneutral)*ncol(grpneutral)), ncol = 3)
c <- 0
for(j in 1:ncol(grpneutral)){
  for(i in 1:nrow(grpneutral)){
    neutral[c+1,1] <- grpneutral[i,j]
    c <- c+1
  }
}
neutral <- data.frame(neutral)
neutral[,2] <- rep(c('1','2','3','4'), each=nrow(grpneutral)); neutral[,2] <- as.numeric(neutral[,2])
neutral[,3] <- rep('Neutral', times=nrow(grpneutral))
colnames(neutral) <- c('values','interval','Category');neutral

functional <- matrix(nrow=(nrow(grpfunctional)*ncol(grpfunctional)), ncol = 3)
c <- 0
for(j in 1:ncol(grpfunctional)){
  for(i in 1:nrow(grpfunctional)){
    functional[c+1,1] <- grpfunctional[i,j]
    c <- c+1
  }
}
functional <- data.frame(functional)
functional[,2] <- rep(c('1','2','3','4'), each=nrow(grpfunctional)); functional[,2] <- as.numeric(functional[,2])
functional[,3] <- rep('Functional', times=nrow(grpfunctional))
colnames(functional) <- c('values','interval','Category');functional

selected <- matrix(nrow=(nrow(grpselected)*ncol(grpselected)), ncol = 3)
c <- 0
for(j in 1:ncol(grpselected)){
  for(i in 1:nrow(grpselected)){
    selected[c+1,1] <- grpselected[i,j]
    c <- c+1
  }
}
selected <- data.frame(selected)
selected[,2] <- rep(c('1','2','3','4'), each=nrow(grpselected)); selected[,2] <- as.numeric(selected[,2])
selected[,3] <- rep('Selected', times=nrow(grpselected))
colnames(selected) <- c('values','interval','Category');selected

temp <- matrix(ncol=5, nrow=12);temp[,1]<- rep(c('Neutral','Functional','Selected'),each=4); temp[,2] <-rep(c(1,2,3,4), times=3)
for(i in 1:4){
  neut <- one.boot(neutral$values[neutral$interval==i], mean, R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean, R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean, R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='Fst_mean_GeoEnvRand.csv', row.names = F)
write.csv(result, file='Fst_GeoEnvRand.csv', row.names = F)

##### Genetic Diversity Captured #####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rG <-N_glauca[[9]]-N_glauca[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,3,5,7)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_alpina[[9]]-N_alpina[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_obliqua[[9]]-N_obliqua[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(5,9,14,19)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-H_annuus[[9]]-H_annuus[[12]] 
rGm <- apply(rG,2,mean)
y=rGm[c(4,7,10,14)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-M_guttatus[[9]]-M_guttatus[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(3,4,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-N_papyraceus[[9]]-N_papyraceus[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,11,18,24)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rG <-C_solstitialis[[9]]-C_solstitialis[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(6,10,16,22)]
# colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rG <-B_maximo[[9]]-B_maximo[[12]]
rGm <- apply(rG,2,mean)
y=rGm[c(13,22,34,46)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_balsamifera[[17]]-P_balsamifera[[23]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_sitchensis[[17]]-P_sitchensis[[23]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[25]]-P_tremula[[34]] # Ctr
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rG <-P_tremula[[26]]-P_tremula[[35]] # Def
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rG <-P_balsamifera[[18]]-P_balsamifera[[24]]
rGm <- apply(rG,2,mean)
y=rGm[c(9,15,22,30)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_tremula[[27]]-P_tremula[[36]] # Selected
rGm <- apply(rG,2,mean)
y=rGm[c(3,5,8,11)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rG <-P_sitchensis[[18]]-P_sitchensis[[24]]
rGm <- apply(rG,2,mean)
y=rGm[c(2,4,7,9)]
# colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

### Create Summary File ###
neutral <- matrix(nrow=(nrow(grpneutral)*ncol(grpneutral)), ncol = 3)
c <- 0
for(j in 1:ncol(grpneutral)){
  for(i in 1:nrow(grpneutral)){
    neutral[c+1,1] <- grpneutral[i,j]
    c <- c+1
  }
}
neutral <- data.frame(neutral)
neutral[,2] <- rep(c('1','2','3','4'), each=nrow(grpneutral)); neutral[,2] <- as.numeric(neutral[,2])
neutral[,3] <- rep('Neutral', times=nrow(grpneutral))
colnames(neutral) <- c('values','interval','Category');neutral

functional <- matrix(nrow=(nrow(grpfunctional)*ncol(grpfunctional)), ncol = 3)
c <- 0
for(j in 1:ncol(grpfunctional)){
  for(i in 1:nrow(grpfunctional)){
    functional[c+1,1] <- grpfunctional[i,j]
    c <- c+1
  }
}
functional <- data.frame(functional)
functional[,2] <- rep(c('1','2','3','4'), each=nrow(grpfunctional)); functional[,2] <- as.numeric(functional[,2])
functional[,3] <- rep('Functional', times=nrow(grpfunctional))
colnames(functional) <- c('values','interval','Category');functional

selected <- matrix(nrow=(nrow(grpselected)*ncol(grpselected)), ncol = 3)
c <- 0
for(j in 1:ncol(grpselected)){
  for(i in 1:nrow(grpselected)){
    selected[c+1,1] <- grpselected[i,j]
    c <- c+1
  }
}
selected <- data.frame(selected)
selected[,2] <- rep(c('1','2','3','4'), each=nrow(grpselected)); selected[,2] <- as.numeric(selected[,2])
selected[,3] <- rep('Selected', times=nrow(grpselected))
colnames(selected) <- c('values','interval','Category');selected

temp <- matrix(ncol=5, nrow=12);temp[,1]<- rep(c('Neutral','Functional','Selected'),each=4); temp[,2] <-rep(c(1,2,3,4), times=3)
for(i in 1:4){
  neut <- one.boot(neutral$values[neutral$interval==i], mean, R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean, R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean, R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='GDcapt_mean_GeoEnvRand.csv', row.names = F)
write.csv(result, file='GDcapt_GeoEnvRand.csv', row.names = F)
