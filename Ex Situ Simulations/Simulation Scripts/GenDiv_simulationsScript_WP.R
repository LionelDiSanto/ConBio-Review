###################
# Simulation Runs #
###################

###                          ###
# Load the simulation function #
###                          ###

source('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation\ Scripts/GeneticDiversityCaptured.R')

### ###
# SSR #
### ###

########################
# Narcissus papyraceus #
########################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='N_papyraceus', method = names[i],
                Npop = c(2,3,seq(5,26,1)),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_papyraceus')
}

#####################
# Nothofagus glauca #
#####################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='N_glauca', method = names[i],
                Npop = seq(2,8,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_glauca')
}

######################
# Nothofagus obliqua #
######################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='N_obliqua', method = names[i],
                Npop = seq(2,20,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_obliqua')
}

#####################
# Nothofagus alpina #
#####################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='N_alpina', method = names[i],
                Npop = seq(2,12,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/N_alpina')
}
####################################################################################
###     ###
# EST-SSR #
###     ###

#########################
# Betula maximowicziana #
#########################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='B_maximo', method = names[i],
                Npop = c(2,seq(4,48,1)),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/B_maximo')
}

####################################################################################
### ###
# SNP #
### ###

#####################
# Helianthus annuus #
#####################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='H_annuus', method = names[i],
                Npop = seq(2,15,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/H_annuus')
}

####################
# Mimulus guttatus #
####################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='M_guttatus', method = names[i],
                Npop = c(2,3,5,seq(7,14,1)),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/M_guttatus')
}

##########################
# Centaurea solstitialis #
##########################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='C_solstitialis', method = names[i],
                Npop = c(2:4,seq(6,9,1),seq(11,25,1)),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/C_solstitialis')
}

####################################################################################
###     ###
# EST-SNP #
###     ###

#######################
# Populus balsamifera #
#######################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_balsamifera', method = names[i],
                Npop = seq(2,31,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera')
}

####################
# Picea sitchensis #
####################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_sitchensis', method = names[i],
                Npop = seq(2,10,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis')
}

#######################
# Populus tremula CTR #
#######################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_tremula_ctr', method = names[i],
                Npop = seq(2,12,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
}

#######################
# Populus tremula DEF #
#######################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_tremula_def', method = names[i],
                Npop = seq(2,12,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
}

####################################################################################
###          ###
# Selected SNP #
###          ###

#######################
# Populus balsamifera #
#######################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_balsamifera_selected', method = names[i],
                Npop = seq(2,31,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_balsamifera')
}

#######################
# Populus tremula DEF #
#######################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_tremula_selected', method = names[i],
                Npop = seq(2,12,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_tremula')
}

####################
# Picea sitchensis #
####################
names=c('Random', 'GeoBased', 'EnvBased', 'GeoEnvBased'); iteration=c(500,1,1,1)
for(i in 1:4){
  gendiv.sample(species='P_sitchensis_selected', method = names[i],
                Npop = seq(2,10,1),it=iteration[i], WP='Yes',
                dir='/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/RData/P_sitchensis')
}