###                        ###
# Run simulation to set Nind #
###                        ###

###
# Aim 
###

# Run setNind function for every dataset and estimate the number of individuals
# to be used in the resampling simulation (N80%).

source('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation\ Scripts/setNind.R')
library(RColorBrewer)

#############
# B. maximo #
#############
B_maximo <- list()
for (i in 1:12){
  run <- setNind(data='B_maximo.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/B_maximo')
  B_maximo[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/B_maximo')
jpeg('B_maximo.jpg')
plot(1:48,ylim=c(0,1), xlim=c(1,48), type='n', main='B_maximo',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =B_maximo[[i]]
  points(1:48, temp[1,], pch=20, col=col[i])
  segments(1:48,temp[1,], 1:48,temp[1,]+temp[2,], col=col[i])
  segments(1:48,temp[1,], 1:48,temp[1,]-temp[2,], col=col[i])
  arrows(1:48,temp[1,],1:48,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:48,temp[1,],1:48,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

##################
# C_solstitialis #
##################
C_solstitialis <- list()
for (i in 1:2){
  run <- setNind(data='C_solstitialis.csv', Nind=i, it=500,
                 directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/C_solstitialis')
  C_solstitialis[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/C_solstitialis')
jpeg('C_solstitialis.jpg')
plot(1:25,ylim=c(0,1), xlim=c(1,25), type='n', main='C_solstitialis',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(2,'Paired')

for(i in 1:2){
  temp =C_solstitialis[[i]]
  points(1:25, temp[1,], pch=20, col=col[i])
  segments(1:25,temp[1,], 1:25,temp[1,]+temp[2,], col=col[i])
  segments(1:25,temp[1,], 1:25,temp[1,]-temp[2,], col=col[i])
  arrows(1:25,temp[1,],1:25,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:25,temp[1,],1:25,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:2)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()
############
# H_annuus #
############
H_annuus <- list()
for (i in 1:12){
  run <- setNind(data='H_annuus.csv', Nind=i, it=500,
                 directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_annuus')
  H_annuus[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_annuus')
jpeg('H_annuus.jpg')
plot(1:15,ylim=c(0,1), xlim=c(1,15), type='n', main='H_annuus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =H_annuus[[i]]
  points(1:15, temp[1,], pch=20, col=col[i])
  segments(1:15,temp[1,], 1:15,temp[1,]+temp[2,], col=col[i])
  segments(1:15,temp[1,], 1:15,temp[1,]-temp[2,], col=col[i])
  arrows(1:15,temp[1,],1:15,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:15,temp[1,],1:15,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#################
# H_argophyllus #
#################
H_argophyllus <- list()
for (i in 1){
  run <- setNind(data='H_argophyllus.csv', Nind=i, it=500,
                 directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_argophyllus')
  H_argophyllus[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_argophyllus')
jpeg('H_argophyllus.jpg')
plot(1:51,ylim=c(0,1), xlim=c(1,51), type='n', main='H_argophyllus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(1,'Paired')

for(i in 1){
  temp =H_argophyllus[[i]]
  points(1:51, temp[1,], pch=51, col=col[i])
  segments(1:51,temp[1,], 1:51,temp[1,]+temp[2,], col=col[i])
  segments(1:51,temp[1,], 1:51,temp[1,]-temp[2,], col=col[i])
  arrows(1:51,temp[1,],1:51,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:51,temp[1,],1:51,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

##############
# M_guttatus #
##############
M_guttatus <- list()
for (i in 1:6){
  run <- setNind(data='M_guttatus.csv', Nind=i, it=500,
                 directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_guttatus')
  M_guttatus[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_guttatus')
jpeg('M_guttatus.jpg')
plot(1:14,ylim=c(0,1), xlim=c(1,24), type='n', main='M_guttatus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(6,'Paired')

for(i in 1:6){
  temp =M_guttatus[[i]]
  points(1:14, temp[1,], pch=20, col=col[i])
  segments(1:14,temp[1,], 1:14,temp[1,]+temp[2,], col=col[i])
  segments(1:14,temp[1,], 1:14,temp[1,]-temp[2,], col=col[i])
  arrows(1:14,temp[1,],1:14,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:14,temp[1,],1:14,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:6)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

###############
# M_lacinatus #
###############
M_lacinatus <- list()
for (i in 1:12){
  run <- setNind(data='M_lacinatus.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_lacinatus')
  M_lacinatus[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_lacinatus')
jpeg('M_lacinatus.jpg')
plot(1:23,ylim=c(0,1), xlim=c(1,23), type='n', main='M_lacinatus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp = M_lacinatus[[i]]
  points(1:23, temp[1,], pch=20, col=col[i])
  segments(1:23,temp[1,], 1:23,temp[1,]+temp[2,], col=col[i])
  segments(1:23,temp[1,], 1:23,temp[1,]-temp[2,], col=col[i])
  arrows(1:23,temp[1,],1:23,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:23,temp[1,],1:23,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

############
# N_alpina #
############
N_alpina <- list()
for (i in 1:12){
  run <- setNind(data='N_alpina.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_alpina')
  N_alpina[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_alpina')
jpeg('N_alpina.jpg')
plot(1:12,ylim=c(0,1), xlim=c(1,12), type='n', main='N_alpina',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =N_alpina[[i]]
  points(1:12, temp[1,], pch=20, col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]+temp[2,], col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]-temp[2,], col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

############
# N_glauca #
############
N_glauca <- list()
for (i in 1:12){
  run <- setNind(data='N_glauca.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_glauca')
  N_glauca[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_glauca')
jpeg('N_glauca.jpg')
plot(1:8,ylim=c(0,1), xlim=c(1,8), type='n', main='N_glauca',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =N_glauca[[i]]
  points(1:8, temp[1,], pch=20, col=col[i])
  segments(1:8,temp[1,], 1:8,temp[1,]+temp[2,], col=col[i])
  segments(1:8,temp[1,], 1:8,temp[1,]-temp[2,], col=col[i])
  arrows(1:8,temp[1,],1:8,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:8,temp[1,],1:8,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#############
# N_obliqua #
#############
N_obliqua <- list()
for (i in 1:12){
  run <- setNind(data='N_obliqua.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_obliqua')
  N_obliqua[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_obliqua')
jpeg('N_obliqua.jpg')
plot(1:20,ylim=c(0,1), xlim=c(1,20), type='n', main='N_obliqua',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =N_obliqua[[i]]
  points(1:20, temp[1,], pch=20, col=col[i])
  segments(1:20,temp[1,], 1:20,temp[1,]+temp[2,], col=col[i])
  segments(1:20,temp[1,], 1:20,temp[1,]-temp[2,], col=col[i])
  arrows(1:20,temp[1,],1:20,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:20,temp[1,],1:20,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

################
# N_papyraceus #
################
N_papyraceus <- list()
for (i in 1:12){
  run <- setNind(data='N_papyraceus.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus')
  N_papyraceus[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus')
jpeg('N_papyraceus.jpg')
plot(1:26,ylim=c(0,1), xlim=c(1,26), type='n', main='N_papyraceus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =N_papyraceus[[i]]
  points(1:26, temp[1,], pch=20, col=col[i])
  segments(1:26,temp[1,], 1:26,temp[1,]+temp[2,], col=col[i])
  segments(1:26,temp[1,], 1:26,temp[1,]-temp[2,], col=col[i])
  arrows(1:26,temp[1,],1:26,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:26,temp[1,],1:26,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#################
# P_balsamifera #
#################
P_balsamifera <- list()
for (i in 1:10){
  run <- setNind(data='P_balsamifera.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera')
  P_balsamifera[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera')
jpeg('P_balsamifera.jpg')
plot(1:31,ylim=c(0,1), xlim=c(1,31), type='n', main='P_balsamifera',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(10,'Paired')

for(i in 1:10){
  temp =P_balsamifera[[i]]
  points(1:31, temp[1,], pch=20, col=col[i])
  segments(1:31,temp[1,], 1:31,temp[1,]+temp[2,], col=col[i])
  segments(1:31,temp[1,], 1:31,temp[1,]-temp[2,], col=col[i])
  arrows(1:31,temp[1,],1:31,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:31,temp[1,],1:31,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:10)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

##########################
# P_balsamifera selected #
##########################
P_balsamifera_selected <- list()
for (i in 1:10){
  run <- setNind(data='P_balsamifera_selected.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera')
  P_balsamifera_selected[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera')
jpeg('P_balsamifera_selected.jpg')
plot(1:31,ylim=c(0,1), xlim=c(1,31), type='n', main='P_balsamifera_selected',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(10,'Accent')

for(i in 1:10){
  temp =P_balsamifera_selected[[i]]
  points(1:31, temp[1,], pch=20, col=col[i])
  segments(1:31,temp[1,], 1:31,temp[1,]+temp[2,], col=col[i])
  segments(1:31,temp[1,], 1:31,temp[1,]-temp[2,], col=col[i])
  arrows(1:31,temp[1,],1:31,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:31,temp[1,],1:31,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:10)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

################
# P_sitchensis #
################
P_sitchensis <- list()
for (i in 1:12){
  run <- setNind(data='P_sitchensis.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis')
  P_sitchensis[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis')
jpeg('P_sitchensis.jpg')
plot(1:10,ylim=c(0,1), xlim=c(1,10), type='n', main='P_sitchensis',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =P_sitchensis[[i]]
  points(1:10, temp[1,], pch=20, col=col[i])
  segments(1:10,temp[1,], 1:10,temp[1,]+temp[2,], col=col[i])
  segments(1:10,temp[1,], 1:10,temp[1,]-temp[2,], col=col[i])
  arrows(1:10,temp[1,],1:10,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:10,temp[1,],1:10,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#########################
# P_sitchensis selected #
#########################
P_sitchensis_selected <- list()
for (i in 1:12){
  run <- setNind(data='P_sitchensis_selected.csv', Nind=i,it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis')
  P_sitchensis_selected[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis')
jpeg('P_sitchensis_selected.jpg')
plot(1:10,ylim=c(0,1), xlim=c(1,10), type='n', main='P_sitchensis_selected',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(12,'Paired')

for(i in 1:12){
  temp =P_sitchensis_selected[[i]]
  points(1:10, temp[1,], pch=20, col=col[i])
  segments(1:10,temp[1,], 1:10,temp[1,]+temp[2,], col=col[i])
  segments(1:10,temp[1,], 1:10,temp[1,]-temp[2,], col=col[i])
  arrows(1:10,temp[1,],1:10,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:10,temp[1,],1:10,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:12)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#################
# P_tremula ctr #
#################
P_tremula_ctr <- list()
for (i in 1:6){
  run <- setNind(data='P_tremula_ctr.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
  P_tremula_ctr[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
jpeg('P_tremula_ctr.jpg')
plot(1:12,ylim=c(0,1), xlim=c(1,12), type='n', main='P_tremula_ctr',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(6,'Paired')

for(i in 1:6){
  temp =P_tremula_ctr[[i]]
  points(1:12, temp[1,], pch=20, col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]+temp[2,], col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]-temp[2,], col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:6)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#####################
# P_tremula defense #
#####################
P_tremula_defense <- list()
for (i in 2:6){
  run <- setNind(data='P_tremula_def.csv', Nind=i,it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
  P_tremula_defense[[i-1]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
jpeg('P_tremula_defense.jpg')
plot(1:12,ylim=c(0,1), xlim=c(1,12), type='n', main='P_tremula_defense',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(5,'Paired')

for(i in 1:5){
  temp =P_tremula_defense[[i]]
  points(1:12, temp[1,], pch=20, col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]+temp[2,], col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]-temp[2,], col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',2:6)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

######################
# P_tremula selected #
######################
P_tremula_selected <- list()
for (i in 2:6){
  run <- setNind(data='P_tremula_selected.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
  P_tremula_selected[[i-1]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
jpeg('P_tremula_selected.jpg')
plot(1:12,ylim=c(0,1), xlim=c(1,12), type='n', main='P_tremula_selected',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(5,'Paired')

for(i in 1:5){
  temp =P_tremula_selected[[i]]
  points(1:12, temp[1,], pch=20, col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]+temp[2,], col=col[i])
  segments(1:12,temp[1,], 1:12,temp[1,]-temp[2,], col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:12,temp[1,],1:12,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',2:6)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

##############
# R_oldhamii #
##############
R_oldhamii <- list()
for (i in 1:8){
  run <- setNind(data='R_oldhamii.csv', Nind=i, it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/R_oldhamii')
  R_oldhamii[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/R_oldhamii')
jpeg('R_oldhamii.jpg')
plot(1:18,ylim=c(0,1), xlim=c(1,18), type='n', main='R_oldhamii',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(8,'Paired')

for(i in 1:8){
  temp =R_oldhamii[[i]]
  points(1:18, temp[1,], pch=20, col=col[i])
  segments(1:18,temp[1,], 1:18,temp[1,]+temp[2,], col=col[i])
  segments(1:18,temp[1,], 1:18,temp[1,]-temp[2,], col=col[i])
  arrows(1:18,temp[1,],1:18,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:18,temp[1,],1:18,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:8)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

###############
# S_leprosula #
###############
S_leprosula <- list()
for (i in 1:10){
  run <- setNind(data='S_leprosula.csv', Nind=i,it=500,
          directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/S_leprosula')
  S_leprosula[[i]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/S_leprosula')
jpeg('S_leprosula.jpg')
plot(1:24,ylim=c(0,1), xlim=c(1,24), type='n', main='S_leprosula',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(10,'Paired')

for(i in 1:10){
  temp =S_leprosula[[i]]
  points(1:24, temp[1,], pch=20, col=col[i])
  segments(1:24,temp[1,], 1:24,temp[1,]+temp[2,], col=col[i])
  segments(1:24,temp[1,], 1:24,temp[1,]-temp[2,], col=col[i])
  arrows(1:24,temp[1,],1:24,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:24,temp[1,],1:24,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',1:10)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

