###
# Quantification of distance matrices
###

###
# Aim
###
# The objective of this script is to estimate genetic distance (Nei's Fst) between populations of
# all 19 genetic and genomic datasets.

####################################################################################
# Main Piepline: Quantification of genetic, environmental and geographic distances #
####################################################################################

###
# Functions
###

### Install dependecies ###
install.dependencies <- function(){
  install.packages("hierfstat")
  install.packages("adegenet")
  install.packages("ade4")
}

###
# Import dependencies
###
library(hierfstat)
library(adegenet)
library(ade4)

#####
# Single Sequence Repeat datasets (SSRs)
#####
###########################
# Mimulus lacinatus (SSR) #
###########################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Mimulus lacinatus')

##### Genetic Assessment #####
mimgen <- read.fstat('M_lacinatus_fstat.dat')
mimfst <- genet.dist(method='Nei87',mimgen)

##############################
# Narcissus papyraceus (SSR) #
##############################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Narcissus papyraceus')

##### Genetic Assessment #####
nargen <- read.fstat('N_papyraceus_filtered_fstat.dat')
narfst <- genet.dist(method='Nei87',nargen)

###########################
# Nothofagus glauca (SSR) #
###########################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Nothofagus glauca')

##### Genetic Assessment #####
notgen <- read.fstat('N_glauca_fstat.dat')
notfst <- genet.dist(method='Nei87',notgen)

############################
# Nothofagus obliqua (SSR) #
############################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Nothofagus obliqua')

##### Genetic Assessment #####
nobgen <- read.fstat('N_obliqua_fstat.dat')
nobfst <- genet.dist(method='Nei87',nobgen)

###########################
# Nothofagus alpina (SSR) #
###########################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Nothofagus alpina')

##### Genetic Assessment #####
nalgen <- read.fstat('N_alpina_fstat.dat')
nalfst <- genet.dist(method='Nei87',nalgen)

#####
# Expressed Sequence Tage SSRs (EST-SSR) 
#####
##############################
# Shorea leprosula (EST-SSR) #
##############################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Shorea leprosula')

##### Genetic Assessment #####
leprogen <- read.fstat('S_leprosula_fstat.dat')
leprofst <- genet.dist(method='Nei87',leprogen)

###################################
# Betula maximowicziana (EST-SSR) #
###################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Betula maximowicziana')

##### Genetic Assessment #####
betgen <- read.fstat('B_maximo_fstat.dat')
betfst <- genet.dist(method='Nei87',betgen)

###################################
# Rhododendron oldhamii (EST-SSR) #
###################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Rhododendron oldhamii')

##### Genetic Assessment #####
oldgen <- read.fstat('R_oldhamii_fstat.dat')
oldfst <- genet.dist(method='Nei87',oldgen)

#####
# Single-Nucleotide Polymorphism (SNPs) 
#####
###########################
# Helianthus annuus (SNP) #
###########################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Helianthus annuus')

##### Genetic Assessment #####
helgen <- read.genetix('H_annuus_genetix.gtx')
helfst <- genet.dist(method='Nei87',helgen)

##########################
# Mimulus guttatus (SNP) #
##########################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Mimulus guttatus')

##### Genetic Assessment #####
mgugen <- read.genetix('M_guttatus_genetix.gtx')
mgufst <- genet.dist(method='Nei87',mgugen)

################################
# Centaurea solstitialis (SNP) #
################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Centaurea solstitialis')

##### Genetic Assessment #####
centgen <- read.genetix('C_solstitialis_native_filtered_genetix.gtx')
centfst <- genet.dist(method='Nei87',centgen)

#####
# Functional SNPs
#####
#################################
# Populus balsamifera (EST-SNP) #
#################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus balsamifera')

##### Genetic Assessment #####
pbalgen <- read.genetix('P_balsamifera_genetix.gtx')
pbalfst <- genet.dist(method='Nei87',pbalgen)

##############################
# Picea sitchensis (EST-SNP) #
##############################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Picea sitchensis')

##### Genetic Assessment #####
psitgen <- read.genetix('P_sitchensis_filtered_genetix.gtx')
psitfst <- genet.dist(method='Nei87',psitgen)

#############################################
# Populus tremula (EST-SNP) [Control genes] #
#############################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus tremula')

##### Genetic Assessment #####
ptrecgen <- read.genetix('P_tremula_ctr_genetix.gtx')
ptrecfst <- genet.dist(method='Nei87',ptrecgen)

#############################################
# Populus tremula (EST-SNP) [Defense genes] #
#############################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus tremula')

##### Genetic Assessment #####
ptremgen <- read.genetix('P_tremula_def_genetix.gtx')
ptremfst <- genet.dist(method='Nei87',ptremgen)

####################################
# Helianthus argophyllus (EST-SNP) #
####################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Helianthus argophyllus')

##### Genetic Assessment #####
hargen <- read.genetix('H_argophyllus_genetix.gtx')
harfst <- genet.dist(method='Nei87',hargen)

#####
# Putatively selected SNPs (Adaptive SNPS)
#####

######################################
# Populus balsamifera (Selected SNP) #
######################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus balsamifera')

##### Genetic Assessment #####
pbalsgen <- read.genetix('P_balsamifera_selected_genetix.gtx')
pbalsfst <- genet.dist(method='Nei87',pbalsgen)

##################################
# Populus tremula (Selected SNP) #
##################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus tremula')

##### Genetic Assessment #####
ptresgen <- read.genetix('P_tremula_selected_genetix.gtx')
ptresfst <- genet.dist(method='Nei87',ptresgen)

###################################
# Picea sitchensis (Selected SNP) #
###################################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Picea sitchensis')

##### Genetic Assessment #####
psitsgen <- read.genetix('P_sitchensis_selected_filtered_genetix.gtx')
psitsfst <- genet.dist(method='Nei87',psitsgen)

################
# Save R image #
################
setwd("/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/R\ Scripts")
save.image(file="Quantification_of_distance_matrices.RData")
