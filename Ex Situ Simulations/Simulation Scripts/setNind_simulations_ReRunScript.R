##################
# ReRuns setNind #
##################

###
# Aim
###

# Rerun two datasets with increased number of individuals sampled (N) to see if the
# 0.8 threshold can be (reasonably) met.

source('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation\ Scripts/setNind.R')
library(RColorBrewer)

################
# M. lacinatus #
################
M_lacinatus <- list()
for (i in 25:30){
  run <- setNind(data='M_lacinatus.csv', Nind=i, it=500,
                 directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_lacinatus')
  M_lacinatus[[i-24]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_lacinatus')
jpeg('M_lacinatus_ReRun.jpg')
plot(1:23,ylim=c(0,1), xlim=c(1,23), type='n', main='M_lacinatus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col=brewer.pal(6,'Paired')

for(i in 1:6){
  temp = M_lacinatus[[i]]
  points(1:23, temp[1,], pch=20, col=col[i])
  segments(1:23,temp[1,], 1:23,temp[1,]+temp[2,], col=col[i])
  segments(1:23,temp[1,], 1:23,temp[1,]-temp[2,], col=col[i])
  arrows(1:23,temp[1,],1:23,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:23,temp[1,],1:23,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',25:30)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

#################
# N. papyraceus #
#################
N_papyraceus <- list()
for (i in 13){
  run <- setNind(data='N_papyraceus.csv', Nind=i, it=500,
                 directory='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus')
  N_papyraceus[[i-12]] <- run
}

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus')
jpeg('N_papyraceus_ReRun.jpg')
plot(1:26,ylim=c(0,1), xlim=c(1,26), type='n', main='N_papyraceus',
     xlab='Npop', ylab='% Genetic Diveristy Captured')
abline(h=.8,lty=2, col='red')
col='black'

for(i in 1){
  temp =N_papyraceus[[i]]
  points(1:26, temp[1,], pch=20, col=col[i])
  segments(1:26,temp[1,], 1:26,temp[1,]+temp[2,], col=col[i])
  segments(1:26,temp[1,], 1:26,temp[1,]-temp[2,], col=col[i])
  arrows(1:26,temp[1,],1:26,temp[1,]+temp[2,], length=0.05, angle=90, col=col[i])
  arrows(1:26,temp[1,],1:26,temp[1,]-temp[2,], length=0.05, angle=90, col=col[i])
}
legend('bottomright', legend=c(paste('Nind=',13)), col=col,pch = 20, 
       horiz = F, cex = 0.8, bty='n')
dev.off()

