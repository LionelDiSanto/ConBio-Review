##################################################################################
# Estimate genetic diversity and differentiation captured in ex situ collections #
##################################################################################

###
# Aim
###
# This script aims at coding for a simulation experiment to estimate genetic diversity captured using four
# different sampling strategies: (1) Random sampling of populations (NULL), (2) Sample populations with the
# the greatest geographical distance (ALTERNATIVE 1), (3) Sample populations with the greatest
# environmental distance (ALTERNATIVE 2) and Sample populations with the greatest geographic and 
# envrionmental distances (ALTERNATIVE 3).

###
# Notes
###

# Running the function with WP="Yes" will simulate data under the idealized within-population
# sampling scenario.

# Running the function with WP="No" will simulate data under the realistic within-population
# sampling scenario.

###
# Function
###

gendiv.sample <- function(species=NULL, method = c('Random', 'GeoBased', 'EnvBased', "GeoEnvBased"), Nind=NULL, Npop=NULL,
                          it=500, header=F, WP='No', dir=NULL){
  
  ###########
  # PreWork #
  ###########
  
  setwd(dir)
  library(usdm)
  library(adegenet)
  library(hierfstat)
  library(data.table)
  
  ###################
  # Import the data #
  ###################
  
  ##### Genotypes #####
  geno <- read.csv(paste(species,'.csv',sep=""),h=header, colClasses = 'character')
  geno[,1] <- as.integer(geno[,1])
  
  orig <- geno
  ori.popname <-as.factor(orig[,1])
  orig <- orig[,-1]
  count<-0
  ori <- data.frame(matrix(nrow = nrow(orig), ncol=(ncol(orig)/2)))
  for(j in 1:(ncol(orig)/2)){
    for(i in 1:nrow(orig)){
      ori[i,j] <- paste(orig[i,(j+count)],',',orig[i,(j+count+1)], sep='') 
    }
    count<-count+1
  }
  original <- df2genind(ori, pop = ori.popname, sep=',', NA.char='-9', ploidy = 2, type='codom')

  ##### Environmental distance #####
  env <- read.csv(paste(species, '_envir.csv', sep=""), h=T)
  rownames(env) <- env[,1]
  dat <- env[,4:ncol(env)]
  dat <- round(dat, digits = 1)
  selzero <- apply(dat,2,sd)
  sel <- which(selzero==0)
  if(length(sel)!=0){dat <- dat[,-sel]}
  dat <- scale(dat, scale = T, center = T)
  dat <- prcomp(dat)
  env <- dist(dat$x[,1:2], method = 'euclidean')
  
  ##### Geographic distance #####
  geo <- read.csv(paste(species, '_envir.csv', sep=""), h=T)
  geo <- geo[,2:3]
  geo <- dist(geo, method = 'euclidean')
  
  ############################
  # Simaltion-based Sampling #
  ############################
  
  if(method=='Random'){
    population <- geno[which(duplicated(geno[,1])==F), 1]
    save <- matrix(nrow = it, ncol=length(Npop))
    Fst <- matrix(nrow = it, ncol=length(Npop))
    colnames(save) <- paste('Npop',Npop, sep='')
    colnames(Fst) <- paste('Npop',Npop, sep='')

    ### Select populations to sample ###
    for(p in 1:length(Npop)){
      for(t in 1:it){
        sampop <- sample(population, Npop[p])
        for(r in 1:length(sampop)){
          if(r==1){genosub <- subset(geno, geno[,1]==sampop[r])}
          if(r!=1){
            genosubset <- subset(geno, geno[,1]==sampop[r])
            genosub <- rbind(genosub, genosubset)
          }
        }
        genosub$ID <- 1:nrow(genosub)
        
        ### Selection of individuals to sample ### 
        if(WP=='No'){
          samind = NULL
          for(k in 1:length(sampop)){
            samindividuals <- sample(genosub$ID[genosub[,1]==sampop[k]], Nind)
            samind <- append(samind, samindividuals)
          }
          genosample <- genosub[samind,]
        }
        if(WP=='Yes'){
          genosample <- genosub
        }
        
        ### Calculate genetic diversity captured ###
        popname <-as.factor(genosample[,1])
        genosample <- genosample[,-1]
        genosample <- genosample[,-ncol(genosample)]
        count<-0
        genosamp <- data.frame(matrix(nrow = nrow(genosample), ncol=(ncol(genosample)/2)))
        for(j in 1:(ncol(genosample)/2)){
          for(i in 1:nrow(genosample)){
            genosamp[i,j] <- paste(genosample[i,(j+count)],',',genosample[i,(j+count+1)], sep='') 
          }
          count<-count+1
        }
        genoal <- df2genind(genosamp, pop = popname, sep=',', NA.char='-9', ploidy = 2, type='codom')
        stat <- basic.stats(genoal)
        Fst[t,p] <- stat$overall[8]; save[t,p] <- ncol(genoal@tab)/ncol(original@tab)
        print(paste('Iteration ', t, '/', 'Npop ', Npop[p], '/', method, '/', species))
      }
    }
    if(WP=='No'){
      write.csv(save, file = paste('Random_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('Random_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
    if(WP=='Yes'){
      write.csv(save, file = paste('RandomWP_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('RandomWP_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
  }
  
  if(method == 'GeoBased'){
    geo.m <- as.matrix(geo)
    geo.m2 <- melt(geo.m)[melt(upper.tri(geo.m))$value,]
    names(geo.m2) <- c("InputID", "TargetID", "Distance")
    I <- order(geo.m2$Distance, decreasing = T)
    geoord <- geo.m2[I,]
    save <- matrix(nrow = it, ncol=length(Npop))
    Fst <- matrix(nrow = it, ncol=length(Npop))
    colnames(save) <- paste('Npop',Npop, sep='')
    colnames(Fst) <- paste('Npop',Npop, sep='')

    ### Select populations to sample ###
    for(p in 1:length(Npop)){
      for(t in 1:it){
        sampop=NULL
        rm(sampop)
        for (w in 1:nrow(geoord)){
          pop <- geoord[1:w,]
          temp <- append(pop$InputID, pop$TargetID)
          if(length(which(duplicated(temp)==F))==Npop[p]){
            sampop <- temp[which(duplicated(temp)==F)]
            break
          }
        }
        for(r in 1:length(sampop)){
          if(r==1){genosub <- subset(geno, geno[,1]==sampop[r])}
          if(r!=1){
            genosubset <- subset(geno, geno[,1]==sampop[r])
            genosub <- rbind(genosub, genosubset)
          }
        }
        genosub$ID <- 1:nrow(genosub)
        
        ### Selection of individuals to sample ### 
        if(WP=='No'){
          samind = NULL
          for(k in 1:length(sampop)){
            samindividuals <- sample(genosub$ID[genosub[,1]==sampop[k]], Nind)
            samind <- append(samind, samindividuals)
          }
          genosample <- genosub[samind,]
        }
        if(WP=='Yes'){
          genosample <- genosub
        }
        
        ### Calculate genetic diversity captured ###
        popname <-as.factor(genosample[,1])
        genosample <- genosample[,-1]
        genosample <- genosample[,-ncol(genosample)]
        count<-0
        genosamp <- data.frame(matrix(nrow = nrow(genosample), ncol=(ncol(genosample)/2)))
        for(j in 1:(ncol(genosample)/2)){
          for(i in 1:nrow(genosample)){
            genosamp[i,j] <- paste(genosample[i,(j+count)],',', genosample[i,(j+count+1)], sep='') 
          }
          count<-count+1
        }
        genoal <- df2genind(genosamp, pop = popname, sep=',', NA.char='-9', ploidy = 2, type='codom')
        stat <- basic.stats(genoal) 
        Fst[t,p] <- stat$overall[8]; save[t,p] <- ncol(genoal@tab)/ncol(original@tab)
        print(paste('Iteration ', t, '/', 'Npop ', Npop[p], '/', method, '/', species))
      }
    }
    if(WP=='No'){
      write.csv(save, file = paste('GeoBased_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('GeoBased_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
    if(WP=='Yes'){
      write.csv(save, file = paste('GeoBasedWP_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('GeoBasedWP_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
  }
  
  if(method == 'EnvBased'){
    env.m <- as.matrix(env)
    env.m2 <- melt(env.m)[melt(upper.tri(env.m))$value,]
    names(env.m2) <- c("InputID", "TargetID", "Distance")
    I <- order(env.m2$Distance, decreasing = T)
    envord <- env.m2[I,]
    save <- matrix(nrow = it, ncol=length(Npop))
    Fst <- matrix(nrow = it, ncol=length(Npop))
    colnames(save) <- paste('Npop',Npop, sep='')
    colnames(Fst) <- paste('Npop',Npop, sep='')

    ### Select populations to sample ###
    for(p in 1:length(Npop)){
      for(t in 1:it){
        sampop=NULL
        rm(sampop)
        for (w in 1:nrow(envord)){
          pop <- envord[1:w,]
          temp <- append(pop$InputID, pop$TargetID)
          if(length(which(duplicated(temp)==F))==Npop[p]){
            sampop <- temp[which(duplicated(temp)==F)]
            break
          }
        }
        for(r in 1:length(sampop)){
          if(r==1){genosub <- subset(geno, geno[,1]==sampop[r])}
          if(r!=1){
            genosubset <- subset(geno, geno[,1]==sampop[r])
            genosub <- rbind(genosub, genosubset)
          }
        }
        genosub$ID <- 1:nrow(genosub)
        
        ### Selection of individuals to sample ### 
        if(WP=='No'){
          samind = NULL
          for(k in 1:length(sampop)){
            samindividuals <- sample(genosub$ID[genosub[,1]==sampop[k]], Nind)
            samind <- append(samind, samindividuals)
          }
          genosample <- genosub[samind,]
        }
        if(WP=='Yes'){
          genosample <- genosub
        }
        
        ### Calculate genetic diversity captured ###
        popname <-as.factor(genosample[,1])
        genosample <- genosample[,-1]
        genosample <- genosample[,-ncol(genosample)]
        count<-0
        genosamp <- data.frame(matrix(nrow = nrow(genosample), ncol=(ncol(genosample)/2)))
        for(j in 1:(ncol(genosample)/2)){
          for(i in 1:nrow(genosample)){
            genosamp[i,j] <- paste(genosample[i,(j+count)],',', genosample[i,(j+count+1)], sep='') 
          }
          count<-count+1
        }
        genoal <- df2genind(genosamp, pop = popname, sep=',', NA.char='-9', ploidy = 2, type='codom')
        stat <- basic.stats(genoal)
        Fst[t,p] <- stat$overall[8]; save[t,p] <- ncol(genoal@tab)/ncol(original@tab)
        print(paste('Iteration ', t, '/', 'Npop ', Npop[p], '/', method, '/', species))
      }
    }
    if(WP=='No'){
      write.csv(save, file = paste('EnvBased_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('EnvBased_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
    if(WP=='Yes'){
      write.csv(save, file = paste('EnvBasedWP_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('EnvBasedWP_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
  }
  
  if(method == 'GeoEnvBased'){
    
    ### Processing pairwise geographic distances ###
    geo.m <- as.matrix(geo)
    geo.m2 <- melt(geo.m)[melt(upper.tri(geo.m))$value,]
    names(geo.m2) <- c("InputID", "TargetID", "Distance")
    I <- order(geo.m2$InputID, decreasing = F)
    geoord <- geo.m2[I,]
    geoord$Distance <- scale(geoord$Distance, scale = T, center = T)
    
    ### Processing pairwise environmental distances ###
    env.m <- as.matrix(env)
    env.m2 <- melt(env.m)[melt(upper.tri(env.m))$value,]
    names(env.m2) <- c("InputID", "TargetID", "Distance")
    I <- order(env.m2$InputID, decreasing = F)
    envord <- env.m2[I,]
    envord$Distance <- scale(envord$Distance, scale = T, center = T)
    
    ### Metric considering both environmental and geographic distance ###
    geoenvord <- cbind(envord[,1:2], (geoord[,3]+envord[,3]))
    colnames(geoenvord)[3] <- "Distance"
    I <- order(geoenvord$Distance, decreasing = T)
    geoenvord <- geoenvord[I,]
    
    ### Create storage objects ###
    save <- matrix(nrow = it, ncol=length(Npop))
    Fst <- matrix(nrow = it, ncol=length(Npop))
    colnames(save) <- paste('Npop',Npop, sep='')
    colnames(Fst) <- paste('Npop',Npop, sep='')

    ### Select populations to sample ###
    for(p in 1:length(Npop)){
      for(t in 1:it){
        sampop=NULL
        rm(sampop)
        for (w in 1:nrow(geoenvord)){
          pop <- geoenvord[1:w,]
          temp <- append(pop$InputID, pop$TargetID)
          if(length(which(duplicated(temp)==F))==Npop[p]){
            sampop <- temp[which(duplicated(temp)==F)]
            break
          }
        }
        for(r in 1:length(sampop)){
          if(r==1){genosub <- subset(geno, geno[,1]==sampop[r])}
          if(r!=1){
            genosubset <- subset(geno, geno[,1]==sampop[r])
            genosub <- rbind(genosub, genosubset)
          }
        }
        genosub$ID <- 1:nrow(genosub)
        
        ### Selection of individuals to sample ### 
        if(WP=='No'){
          samind = NULL
          for(k in 1:length(sampop)){
            samindividuals <- sample(genosub$ID[genosub[,1]==sampop[k]], Nind)
            samind <- append(samind, samindividuals)
          }
          genosample <- genosub[samind,]
        }
        if(WP=='Yes'){
          genosample <- genosub
        }
        
        ### Calculate genetic diversity captured ###
        popname <-as.factor(genosample[,1])
        genosample <- genosample[,-1]
        genosample <- genosample[,-ncol(genosample)]
        count<-0
        genosamp <- data.frame(matrix(nrow = nrow(genosample), ncol=(ncol(genosample)/2)))
        for(j in 1:(ncol(genosample)/2)){
          for(i in 1:nrow(genosample)){
            genosamp[i,j] <- paste(genosample[i,(j+count)],',', genosample[i,(j+count+1)], sep='') 
          }
          count<-count+1
        }
        genoal <- df2genind(genosamp, pop = popname, sep=',', NA.char='-9', ploidy = 2, type='codom')
        stat <- basic.stats(genoal)
        Fst[t,p] <- stat$overall[8]; save[t,p] <- ncol(genoal@tab)/ncol(original@tab)
        print(paste('Iteration ', t, '/', 'Npop ', Npop[p], '/', method, '/', species))
      }
    }
    if(WP=='No'){
      write.csv(save, file = paste('GeoEnvBased_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('GeoEnvBased_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
    if(WP=='Yes'){
      write.csv(save, file = paste('GeoEnvBasedWP_results_',species,'.csv', sep=""), row.names = F)
      write.csv(Fst, file=paste('GeoEnvBasedWP_Fst_results_',species,'.csv', sep=""), row.names = F)
    }
  }
  return(save)
}
