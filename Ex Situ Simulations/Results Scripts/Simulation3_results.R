########################
# Results Simulation 3 #
########################

###
# Aim
###

# Test whether sampling of populations based on environmental/geographic 
# distance matrices perform better than random sampling of populations 
# under an idealized within-population sampling scenario. 
# Variables tested: Fst and % allelic diversity captured. Specifically,
# this script performs all calculations necessary to create Figure 3.

################################
# Libraries and import of data #
################################
library(simpleboot)
library(boot)

### B_maximo ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/B_maximo/Simulation3')
temp = list.files(pattern = '*.csv');temp
B_maximo = lapply(temp, read.csv)

### C_solstitialis ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/C_solstitialis/Simulation3')
temp = list.files(pattern = '*.csv');temp
C_solstitialis = lapply(temp, read.csv)

### H_annuus ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_annuus/Simulation3')
temp = list.files(pattern = '*.csv');temp
H_annuus = lapply(temp, read.csv)

### M_guttatus ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_guttatus/Simulation3')
temp = list.files(pattern = '*.csv');temp
M_guttatus = lapply(temp, read.csv)

### N_papyraceus ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus/Simulation3')
temp = list.files(pattern = '*.csv');temp
N_papyraceus = lapply(temp, read.csv)

### N_alpina ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_alpina/Simulation3')
temp = list.files(pattern = '*.csv');temp
N_alpina = lapply(temp, read.csv)

### N_glauca ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_glauca/Simulation3')
temp = list.files(pattern = '*.csv');temp
N_glauca = lapply(temp, read.csv)

### N_obliqua ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_obliqua/Simulation3')
temp = list.files(pattern = '*.csv');temp
N_obliqua = lapply(temp, read.csv)

### P_balsamifera ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera/Simulation3')
temp = list.files(pattern = '*.csv');temp
P_balsamifera = lapply(temp, read.csv)

### P_sitchensis ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis/Simulation3')
temp = list.files(pattern = '*.csv');temp
P_sitchensis = lapply(temp, read.csv)

### P_tremula ###
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula/Simulation3')
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
rGm <-N_glauca[[2]]-apply(N_glauca[[11]],2,mean)
y=rGm[c(2,3,5,7)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_alpina[[2]]-apply(N_alpina[[11]],2,mean)
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_obliqua[[2]]-apply(N_obliqua[[11]],2,mean)
y=rGm[c(5,9,14,19)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-H_annuus[[2]]-apply(H_annuus[[11]],2,mean)
y=rGm[c(4,7,10,14)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-M_guttatus[[2]]-apply(M_guttatus[[11]],2,mean)
y=rGm[c(3,4,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_papyraceus[[2]]-apply(N_papyraceus[[11]],2,mean)
y=rGm[c(6,11,18,24)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-C_solstitialis[[2]]-apply(C_solstitialis[[11]],2,mean)
y=rGm[c(6,10,16,22)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rGm <-B_maximo[[2]]-apply(B_maximo[[11]],2,mean)
y=rGm[c(13,22,34,46)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_balsamifera[[3]]-apply(P_balsamifera[[21]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_sitchensis[[3]]-apply(P_sitchensis[[21]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[4]]-apply(P_tremula[[31]],2,mean) # Ctr
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[5]]-apply(P_tremula[[32]],2,mean) # Def
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rGm <-P_balsamifera[[4]]-apply(P_balsamifera[[22]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_tremula[[6]]-apply(P_tremula[[33]],2,mean) # Selected
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_sitchensis[[4]]-apply(P_sitchensis[[22]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
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
  neut <- one.boot(neutral$values[neutral$interval==i], mean,R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean,R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean,R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='Fst_mean_EnvRandWP.csv', row.names = F)
write.csv(result, file='Fst_EnvRandWP.csv', row.names = F)

##### Genetic Diversity Captured #####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rGm <-N_glauca[[3]]-apply(N_glauca[[12]],2,mean)
y=rGm[c(2,3,5,7)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_alpina[[3]]-apply(N_alpina[[12]],2,mean)
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_obliqua[[3]]-apply(N_obliqua[[12]],2,mean)
y=rGm[c(5,9,14,19)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-H_annuus[[3]]-apply(H_annuus[[12]],2,mean)
y=rGm[c(4,7,10,14)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-M_guttatus[[3]]-apply(M_guttatus[[12]],2,mean)
y=rGm[c(3,4,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_papyraceus[[3]]-apply(N_papyraceus[[12]],2,mean)
y=rGm[c(6,11,18,24)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-C_solstitialis[[3]]-apply(C_solstitialis[[12]],2,mean)
y=rGm[c(6,10,16,22)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rGm <-B_maximo[[3]]-apply(B_maximo[[12]],2,mean)
y=rGm[c(13,22,34,46)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_balsamifera[[5]]-apply(P_balsamifera[[23]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_sitchensis[[5]]-apply(P_sitchensis[[23]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[7]]-apply(P_tremula[[34]],2,mean) # Ctr
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[8]]-apply(P_tremula[[35]],2,mean) # Def
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rGm <-P_balsamifera[[6]]-apply(P_balsamifera[[24]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_tremula[[9]]-apply(P_tremula[[36]],2,mean) # Selected
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_sitchensis[[6]]-apply(P_sitchensis[[24]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
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
  neut <- one.boot(neutral$values[neutral$interval==i], mean,R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean,R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean,R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='GDcapt_mean_EnvRandWP.csv', row.names = F)
write.csv(result, file='GDcapt_EnvRandWP.csv', row.names = F)

################################
# GeoBased - Random comparison #
################################
##### Fst ####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rGm <-N_glauca[[5]]-apply(N_glauca[[11]],2,mean)
y=rGm[c(2,3,5,7)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_alpina[[5]]-apply(N_alpina[[11]],2,mean)
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_obliqua[[5]]-apply(N_obliqua[[11]],2,mean)
y=rGm[c(5,9,14,19)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-H_annuus[[5]]-apply(H_annuus[[11]],2,mean)
y=rGm[c(4,7,10,14)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-M_guttatus[[5]]-apply(M_guttatus[[11]],2,mean)
y=rGm[c(3,4,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_papyraceus[[5]]-apply(N_papyraceus[[11]],2,mean)
y=rGm[c(6,11,18,24)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-C_solstitialis[[5]]-apply(C_solstitialis[[11]],2,mean)
y=rGm[c(6,10,16,22)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rGm <-B_maximo[[5]]-apply(B_maximo[[11]],2,mean)
y=rGm[c(13,22,34,46)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_balsamifera[[9]]-apply(P_balsamifera[[21]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_sitchensis[[9]]-apply(P_sitchensis[[21]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[13]]-apply(P_tremula[[31]],2,mean) # Ctr
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[14]]-apply(P_tremula[[32]],2,mean) # Def
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rGm <-P_balsamifera[[10]]-apply(P_balsamifera[[22]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_tremula[[15]]-apply(P_tremula[[33]],2,mean) # Selected
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_sitchensis[[10]]-apply(P_sitchensis[[22]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
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
  neut <- one.boot(neutral$values[neutral$interval==i], mean,R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean,R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean,R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='Fst_mean_GeoRandWP.csv', row.names = F)
write.csv(result, file='Fst_GeoRandWP.csv', row.names = F)

##### Genetic Diversity Captured #####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rGm <-N_glauca[[6]]-apply(N_glauca[[12]],2,mean)
y=rGm[c(2,3,5,7)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_alpina[[6]]-apply(N_alpina[[12]],2,mean)
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_obliqua[[6]]-apply(N_obliqua[[12]],2,mean)
y=rGm[c(5,9,14,19)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-H_annuus[[6]]-apply(H_annuus[[12]],2,mean)
y=rGm[c(4,7,10,14)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-M_guttatus[[6]]-apply(M_guttatus[[12]],2,mean)
y=rGm[c(3,4,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_papyraceus[[6]]-apply(N_papyraceus[[12]],2,mean)
y=rGm[c(6,11,18,24)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-C_solstitialis[[6]]-apply(C_solstitialis[[12]],2,mean)
y=rGm[c(6,10,16,22)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rGm <-B_maximo[[6]]-apply(B_maximo[[12]],2,mean)
y=rGm[c(13,22,34,46)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_balsamifera[[11]]-apply(P_balsamifera[[23]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_sitchensis[[11]]-apply(P_sitchensis[[23]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[16]]-apply(P_tremula[[34]],2,mean) # Ctr
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[17]]-apply(P_tremula[[35]],2,mean) # Def
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rGm <-P_balsamifera[[12]]-apply(P_balsamifera[[24]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_tremula[[18]]-apply(P_tremula[[36]],2,mean) # Selected
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_sitchensis[[12]]-apply(P_sitchensis[[24]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
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
  neut <- one.boot(neutral$values[neutral$interval==i], mean,R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean,R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean,R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='GDcapt_mean_GeoRandWP.csv', row.names = F)
write.csv(result, file='GDcapt_GeoRandWP.csv', row.names = F)

###################################
# GeoEnvBased - Random comparison #
###################################
##### Fst ####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rGm <-N_glauca[[8]]-apply(N_glauca[[11]],2,mean)
y=rGm[c(2,3,5,7)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_alpina[[8]]-apply(N_alpina[[11]],2,mean)
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_obliqua[[8]]-apply(N_obliqua[[11]],2,mean)
y=rGm[c(5,9,14,19)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-H_annuus[[8]]-apply(H_annuus[[11]],2,mean)
y=rGm[c(4,7,10,14)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-M_guttatus[[8]]-apply(M_guttatus[[11]],2,mean)
y=rGm[c(3,4,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_papyraceus[[8]]-apply(N_papyraceus[[11]],2,mean)
y=rGm[c(6,11,18,24)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-C_solstitialis[[8]]-apply(C_solstitialis[[11]],2,mean)
y=rGm[c(6,10,16,22)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rGm <-B_maximo[[8]]-apply(B_maximo[[11]],2,mean)
y=rGm[c(13,22,34,46)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_balsamifera[[15]]-apply(P_balsamifera[[21]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_sitchensis[[15]]-apply(P_sitchensis[[21]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[22]]-apply(P_tremula[[31]],2,mean) # Ctr
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[23]]-apply(P_tremula[[32]],2,mean) # Def
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rGm <-P_balsamifera[[16]]-apply(P_balsamifera[[22]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_tremula[[24]]-apply(P_tremula[[33]],2,mean) # Selected
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_sitchensis[[16]]-apply(P_sitchensis[[22]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
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
  neut <- one.boot(neutral$values[neutral$interval==i], mean,R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean,R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean,R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='Fst_mean_GeoEnvRandWP.csv', row.names = F)
write.csv(result, file='Fst_GeoEnvRandWP.csv', row.names = F)

##### Genetic Diversity Captured #####
grpneutral=NULL
grpfunctional=NULL
grpselected=NULL

# Neutral #
rGm <-N_glauca[[9]]-apply(N_glauca[[12]],2,mean)
y=rGm[c(2,3,5,7)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_alpina[[9]]-apply(N_alpina[[12]],2,mean)
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_obliqua[[9]]-apply(N_obliqua[[12]],2,mean)
y=rGm[c(5,9,14,19)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-H_annuus[[9]]-apply(H_annuus[[12]],2,mean)
y=rGm[c(4,7,10,14)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-M_guttatus[[9]]-apply(M_guttatus[[12]],2,mean)
y=rGm[c(3,4,8,11)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-N_papyraceus[[9]]-apply(N_papyraceus[[12]],2,mean)
y=rGm[c(6,11,18,24)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

rGm <-C_solstitialis[[9]]-apply(C_solstitialis[[12]],2,mean)
y=rGm[c(6,10,16,22)]
colnames(y) <- c('25','50','75', '100')
grpneutral<-rbind(grpneutral,y)

# Functional #
rGm <-B_maximo[[9]]-apply(B_maximo[[12]],2,mean)
y=rGm[c(13,22,34,46)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_balsamifera[[17]]-apply(P_balsamifera[[23]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_sitchensis[[17]]-apply(P_sitchensis[[23]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[25]]-apply(P_tremula[[34]],2,mean) # Ctr
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

rGm <-P_tremula[[26]]-apply(P_tremula[[35]],2,mean) # Def
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpfunctional<-rbind(grpfunctional,y)

# Selected #
rGm <-P_balsamifera[[18]]-apply(P_balsamifera[[24]],2,mean)
y=rGm[c(9,15,22,30)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_tremula[[27]]-apply(P_tremula[[36]],2,mean) # Selected
y=rGm[c(3,5,8,11)]
colnames(y) <- c('25','50','75', '100')
grpselected<-rbind(grpselected,y)

rGm <-P_sitchensis[[18]]-apply(P_sitchensis[[24]],2,mean)
y=rGm[c(2,4,7,9)]
colnames(y) <- c('25','50','75', '100')
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
  neut <- one.boot(neutral$values[neutral$interval==i], mean,R=2000)
  neutci <- boot.ci(neut, conf=.95, type = 'basic')
  temp[i,3] <- mean(neutral$values[neutral$interval==i])
  temp[i,4]<-neutci$basic[4]; temp[i,5]<-neutci$basic[5]
  fun <- one.boot(functional$values[functional$interval==i], mean,R=2000)
  funci <- boot.ci(fun, conf=.95, type='basic')
  temp[i+4,3] <- mean(functional$values[functional$interval==i])
  temp[i+4,4]<-funci$basic[4]; temp[i+4,5]<-funci$basic[5]
  sel <- one.boot(selected$values[selected$interval==i], mean,R=2000)
  selci <- boot.ci(sel, conf=.95, type='basic')
  temp[i+8,3] <- mean(selected$values[selected$interval==i])
  temp[i+8,4]<-selci$basic[4]; temp[i+8,5]<-selci$basic[5]
}
temp<-data.frame(temp)
colnames(temp) <- c('Category','Interval', 'Mean', 'lower','upper')
temp

result <- rbind(neutral, functional, selected);result

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')
write.csv(temp, file='GDcapt_mean_GeoEnvRandWP.csv', row.names = F)
write.csv(result, file='GDcapt_GeoEnvRandWP.csv', row.names = F)
