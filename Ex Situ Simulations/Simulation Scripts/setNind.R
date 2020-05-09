###                                       ###
# Set the fix value of Nind (N80%) for simulations #
###                                       ###

###
# Aim
###

# Run genetic resampling simulation to determine the fix value of 
# Nind (N80%) to use in realistic simulations.

#######################
# Simulation function #
#######################

setNind <- function(data=NULL, Nind=NULL, header=F, it=500, directory=NULL){
  
  ####################
  # Libraries & Data #
  ####################
  library(adegenet)
  setwd(directory)
  df <- read.csv(data, h=header, colClasses = 'character')
  df[,1] <- as.integer(df[,1])
  colnames(df) <- c('PopID', paste('Col', 2:ncol(df), sep=''))
  df$FID <- 1:nrow(df)
  Npop <- df$PopID[which(duplicated(df$PopID)==F)]
  saved=NULL
  res <- matrix(nrow=2,ncol=length(Npop))
  colnames(res) <- paste('Pop', 1:length(Npop), sep='')
  rownames(res) <- c('Mean', 'SE')
  
  ############
  # Sampling #
  ############
  for(d in 1:length(Npop)){
    temp <- subset(df, df$PopID==Npop[d])
    for(r in 1:it){
      sampind <- sample(temp$FID, Nind, replace = F)
      genosample <- df[sampind,]
      genosample <- genosample[,-c(1,ncol(genosample))]
      count<-0
      genosamp <- data.frame(matrix(nrow = nrow(genosample), ncol=(ncol(genosample)/2)))
      for(j in 1:(ncol(genosample)/2)){
        for(i in 1:nrow(genosample)){
          genosamp[i,j] <- paste(genosample[i,(j+count)],',',genosample[i,(j+count+1)], sep='') 
        }
        count<-count+1
      }
      genoal <- df2genind(genosamp, sep=',', NA.char='-9', ploidy = 2, type='codom')
      
      temprm <- temp[,-c(1,ncol(temp))]
      count <- 0
      tempgenind <- data.frame(matrix(nrow = nrow(temprm), ncol=(ncol(temprm)/2)))
      for(j in 1:(ncol(temprm)/2)){
        for(i in 1:nrow(temprm)){
          tempgenind[i,j] <- paste(temprm[i,(j+count)],',',temprm[i,(j+count+1)], sep='') 
        }
        count<-count+1
      }
      tempal <- df2genind(tempgenind, sep=',', NA.char='-9', ploidy = 2, type='codom')
      saved <- append(saved,ncol(genoal@tab)/(ncol(tempal@tab)))
      print(paste('Iteration',r, '/', 'Nind', Nind,'/','Npop', Npop[d]))
    }
    res[1,d] <- mean(saved)
    res[2,d] <- sd(saved)/sqrt(length(saved))
    saved=NULL
  }
  write.csv(res, file=paste('Results_Nind',Nind,'_',data, sep=''))
  return(res)
}
