#########################
# Simulation 1: Results #
#########################

###
# Aim
###

# To create Appendix S6.

###############################
# Import datasets & Libraries #
###############################
library(ggplot2)
library(ggthemes)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/B_maximo/Simulation1')
B_maximo <- read.csv('Results_Nind11_B_maximo.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/C_solstitialis/Simulation1')
C_solstitialis <- read.csv('Results_Nind2_C_solstitialis.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_annuus/Simulation1')
H_annuus <- read.csv('Results_Nind3_H_annuus.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_argophyllus/Simulation1')
H_argophyllus <- read.csv('Results_Nind1_H_argophyllus.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_guttatus/Simulation1')
M_guttatus <- read.csv('Results_Nind3_M_guttatus.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_lacinatus/Simulation1')
M_lacinatus <- read.csv('Results_Nind30_M_lacinatus.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_alpina/Simulation1')
N_alpina <- read.csv('Results_Nind10_N_alpina.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_glauca/Simulation1')
N_glauca <- read.csv('Results_Nind10_N_glauca.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_obliqua/Simulation1')
N_obliqua <- read.csv('Results_Nind10_N_obliqua.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus/Simulation1')
N_papyraceus <- read.csv('Results_Nind13_N_papyraceus.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera/Simulation1')
P_balsamifera <- read.csv('Results_Nind3_P_balsamifera.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera/Simulation1')
P_balsamifera_selected <- read.csv('Results_Nind4_P_balsamifera_selected.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis/Simulation1')
P_sitchensis <- read.csv('Results_Nind4_P_sitchensis.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis/Simulation1')
P_sitchensis_selected <- read.csv('Results_Nind5_P_sitchensis_selected.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula/Simulation1')
P_tremula_ctr <- read.csv('Results_Nind2_P_tremula_ctr.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula/Simulation1')
P_tremula_defense <- read.csv('Results_Nind3_P_tremula_def.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula/Simulation1')
P_tremula_selected <- read.csv('Results_Nind3_P_tremula_selected.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/R_oldhamii/Simulation1')
R_oldhamii <- read.csv('Results_Nind8_R_oldhamii.csv',h=T)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/S_leprosula/Simulation1')
S_leprosula <- read.csv('Results_Nind10_S_leprosula.csv',h=T)

######################
# Create Appendix S6 #
######################

graph =list()
graph[[1]] <- B_maximo[,-1]
graph[[2]] <- C_solstitialis[,-1]
graph[[3]] <- H_annuus[,-1]
graph[[4]] <- M_guttatus[,-1]
graph[[5]] <- N_alpina[,-1]
graph[[6]] <- N_glauca[,-1]
graph[[7]] <- N_obliqua[,-1]
graph[[8]] <- P_balsamifera[,-1]
graph[[9]] <- P_balsamifera_selected[,-1]
graph[[10]] <- P_sitchensis[,-1]
graph[[11]] <- P_sitchensis_selected[,-1]
graph[[12]] <- P_tremula_ctr[,-1]
graph[[13]] <- P_tremula_defense[,-1]
graph[[14]] <- P_tremula_selected[,-1]
graph[[15]] <- N_papyraceus[,-1]
graph[[16]] <- H_argophyllus[,-1]
graph[[17]] <- M_lacinatus[,-1]
graph[[18]] <- R_oldhamii[,-1]
graph[[19]] <- S_leprosula[,-1]

names <- c('B_maximo (11)', 'C_solstitialis (2)', 'H_annuus (3)', 'M_guttatus (3)', 
           'N_alpina (10)', 'N_glauca (10)', 'N_obliqua (10)', 'P_balsamifera (3)', 
           'P_balsamifera_selected (4)', 'P_sitchensis (4)','P_sitchensis_selected (5)',
           'P_tremula_ctr (2)', 'P_tremula_defense (3)', 'P_tremula_selected (3)',
           'N_papyraceus (13)', 'H_argophyllus (1)', 'M_lacinatus (30)', 'R_oldhamii (8)', 
           'S_leprosula (10)')

for(i in 1:19){
  temp=graph[[i]]
  temp=t(temp[])
  temp <- data.frame(temp)
  temp$PopID <- 1:nrow(temp)
  temp$Datasets <- rep(names[i], times=max(temp$PopID))
  colnames(temp) <- c('Mean','SE', 'PopID' ,'Datasets')
  if(i<16){
    temp$status <- rep('Included in Realistic Simulations', times=max(temp$PopID))
  }
  if(i>15){
    temp$status <- rep('Discarded from Realistic Simulations', times=max(temp$PopID))
  }
  if(i==1){
    sim1 <- temp
  }
  if(i!=1){
    sim1 <- rbind(sim1, temp)
  }
  if(i==15){
    rownames(sim1) <- 1:nrow(sim1)
  }
}

sim1_D <- subset(sim1, sim1$status=="Discarded from Realistic Simulations")
discarded <- ggplot(data=sim1_D, aes(x=PopID, y=Mean, color=Datasets)) + 
  geom_hline(yintercept = .8, linetype='dashed', color='red') +
  geom_point() + ylim(.5,1) + geom_line() + theme_classic() +
  xlab('Population ID') + ylab(expression(paste("A" ["c"],"/A" ["d"])))+
  geom_errorbar(data=sim1_D, aes(ymin=Mean-1.96*SE, ymax=Mean+1.96*SE), width=.5)+
  scale_color_manual(values=c('blue','grey', 'red', 'orange'))+
  theme(panel.grid = element_blank(),axis.text = element_text(size=12), 
        axis.title = element_text(size=14), strip.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=12))

sim1_K <- subset(sim1, sim1$status=="Included in Realistic Simulations")
kept <- ggplot(data=sim1_K, aes(x=PopID, y=Mean, color=Datasets)) + 
  geom_hline(yintercept = .8, linetype='dashed', color='red') +
  geom_point() + ylim(.5,1) + geom_line() + theme_classic() +
  xlab('') + ylab(expression(paste("A" ["c"],"/A" ["d"])))+
  geom_errorbar(data=sim1_K, aes(ymin=Mean-1.96*SE, ymax=Mean+1.96*SE), width=.5)+
  scale_color_manual(values=c('green', 'lightblue','lightgrey',
                              'darkgreen','darkblue','lightgreen', 'darkgrey', 'darkred',
                              'darkorange', 'turquoise', 'purple', 'brown', 'yellow', 'black',
                              'magenta'))+
  theme(panel.grid = element_blank(),axis.text = element_text(size=12), 
        axis.title = element_text(size=14), strip.text = element_text(size=12),
        legend.text = element_text(size=12), legend.title = element_text(size=12))

library(cowplot)
plot_grid(kept, discarded, labels = c('a','b'), nrow = 2, ncol = 1, align="v")

