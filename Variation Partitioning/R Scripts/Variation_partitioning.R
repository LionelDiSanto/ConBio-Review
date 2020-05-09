###
# Variance Partioning analyses
###

###
# Aim
###

# For each of our species, estimate the proportion of genetic divergence explained by geographic and environmental
# variables independently and together using the 'vegan' package.

### Functions ###
envdist_varpart <- function(data){
  
  ### Data Organization ###
  dat <- data[,4:ncol(data)]
  dat <- round(dat, digits = 1)
  selzero <- apply(dat,2,sd)
  sel <- which(selzero==0)
  if(length(sel)!=0){dat <- dat[,-sel]}
  dat <- scale(dat, scale = T, center = T)
  return(dat)
}

### Loading dependencies ###
library('vegan')
library("adegenet")
setwd("/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/R\ Scripts")
load("Quantification_of_distance_matrices.RData")

#####
# SSRs
#####
##### Mimulus lacinatus #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Mimulus lacinatus')

mimenv <- read.csv("M_lacinatus_envir.csv", h=T); head(mimenv)
mimenvir <- envdist_varpart(mimenv); mimenvir; pcadata <- mimenvir
mimenvir <- prcomp(mimenvir); summary(mimenvir)
envDist <- as.matrix(mimenvir$x[,1:2])

mimgeogr <- mimenv[,2:3]; mimgeogr
geoDist <- as.matrix(mimgeogr)

genDist <- mimfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(mimenvir$x[,1:2], method = 'euclidean')
geo <- dist(mimgeogr, method = 'euclidean')
mantel <- mantel.randtest(mimfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(mimfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(mimfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(mimfst~env); abline(model, col="darkred")
plot(mimfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(mimfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1), mai=c(1,1,.5,1))

##### Narcissus papyraceus #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Narcissus papyraceus')

narenv <- read.csv("N_papyraceus_envir_filtered.csv", h=T); head(narenv)
narenvir <- envdist_varpart(narenv); narenvir; pcadata <- narenvir
narenvir <- prcomp(narenvir); summary(narenvir)
envDist <- as.matrix(narenvir$x[,1:2])

nargeogr <- narenv[,2:3]; nargeogr
geoDist <- as.matrix(nargeogr)

genDist <- narfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(narenvir$x[,1:2], method = 'euclidean')
geo <- dist(nargeogr, method = 'euclidean')
mantel <- mantel.randtest(narfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(narfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(narfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(narfst~env); abline(model, col="darkred")
plot(narfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(narfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Nothofagus glauca #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Nothofagus glauca')

notenv <- read.csv("N_glauca_envir.csv", h=T); head(notenv)
notenvir <- envdist_varpart(notenv); notenvir; pcadata <- notenvir
notenvir <- prcomp(notenvir); summary(notenvir)
envDist <- as.matrix(notenvir$x[,1:2])

notgeogr <- notenv[,2:3]; notgeogr
geoDist <- as.matrix(notgeogr)

genDist <- notfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(notenvir$x[,1:2], method = 'euclidean')
geo <- dist(notgeogr, method = 'euclidean')
mantel <- mantel.randtest(notfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(notfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(notfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(notfst~env); abline(model, col="darkred")
plot(notfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(notfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Nothofagus obliqua #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Nothofagus obliqua')

nobenv <- read.csv("N_obliqua_envir.csv", h=T); head(nobenv)
nobenvir <- envdist_varpart(nobenv); nobenvir; pcadata <- nobenvir
nobenvir <- prcomp(nobenvir); summary(nobenvir)
envDist <- as.matrix(nobenvir$x[,1:2])

nobgeogr <- nobenv[,2:3]; nobgeogr
geoDist <- as.matrix(nobgeogr)

genDist <- nobfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(nobenvir$x[,1:2], method = 'euclidean')
geo <- dist(nobgeogr, method = 'euclidean')
mantel <- mantel.randtest(nobfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(nobfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(nobfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(nobfst~env); abline(model, col="darkred")
plot(nobfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(nobfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Nothofagus alpina #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Nothofagus alpina')

nalenv <- read.csv("N_alpina_envir.csv", h=T); head(nalenv)
nalenvir <- envdist_varpart(nalenv); nalenvir; pcadata <- nalenvir
nalenvir <- prcomp(nalenvir); summary(nalenvir)
envDist <- as.matrix(nalenvir$x[,1:2])

nalgeogr <- nalenv[,2:3]; nalgeogr
geoDist <- as.matrix(nalgeogr)

genDist <- nalfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(nalenvir$x[,1:2], method = 'euclidean')
geo <- dist(nalgeogr, method = 'euclidean')
mantel <- mantel.randtest(nalfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(nalfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(nalfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(nalfst~env); abline(model, col="darkred")
plot(nalfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(nalfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

#####
# EST-SSRs
#####
##### Shorea leprosula #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Shorea leprosula')

leproenv <- read.csv("S_leprosula_envir.csv", h=T); head(leproenv)
leproenvir <- envdist_varpart(leproenv); leproenvir; pcadata <- leproenvir
leproenvir <- prcomp(leproenvir); summary(leproenvir)
envDist <- as.matrix(leproenvir$x[,1:2])

leprogeogr <- leproenv[,2:3]; leprogeogr
geoDist <- as.matrix(leprogeogr)

genDist <- leprofst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(leproenvir$x[,1:2], method = 'euclidean')
geo <- dist(leprogeogr, method = 'euclidean')
mantel <- mantel.randtest(leprofst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(leprofst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(leprofst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(leprofst~env); abline(model, col="darkred")
plot(leprofst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(leprofst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Betula maximowicziana #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Betula maximowicziana')

betenv <- read.csv("B_maximo_envir.csv", h=T); head(betenv)
betenvir <- envdist_varpart(betenv); betenvir; pcadata <- betenvir
betenvir <- prcomp(betenvir); summary(betenvir)
envDist <- as.matrix(betenvir$x[,1:2])

betgeogr <- betenv[,2:3]; betgeogr
geoDist <- as.matrix(betgeogr)

genDist <- betfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(betenvir$x[,1:2], method = 'euclidean')
geo <- dist(betgeogr, method = 'euclidean')
mantel <- mantel.randtest(betfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(betfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(betfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(betfst~env); abline(model, col="darkred")
plot(betfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(betfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Rhododendron oldhamii #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Rhododendron oldhamii')

oldenv <- read.csv("R_oldhamii_envir.csv", h=T); head(oldenv)
oldenvir <- envdist_varpart(oldenv); oldenvir; pcadata <- oldenvir
oldenvir <- prcomp(oldenvir); summary(oldenvir)
envDist <- as.matrix(oldenvir$x[,1:2])

oldgeogr <- oldenv[,2:3]; oldgeogr
geoDist <- as.matrix(oldgeogr)

genDist <- oldfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(oldenvir$x[,1:2], method = 'euclidean')
geo <- dist(oldgeogr, method = 'euclidean')
mantel <- mantel.randtest(oldfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(oldfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(oldfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(oldfst~env); abline(model, col="darkred")
plot(oldfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(oldfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

#####
# SNPs
#####
##### Helianthus annuus #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Helianthus annuus')

helenv <- read.csv("H_annuus_envir.csv", h=T); head(helenv)
helenvir <- envdist_varpart(helenv); helenvir; pcadata <- helenvir
helenvir <- prcomp(helenvir); summary(helenvir)
envDist <- as.matrix(helenvir$x[,1:2])

helgeogr <- helenv[,2:3]; helgeogr
geoDist <- as.matrix(helgeogr)

genDist <- helfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(helenvir$x[,1:2], method = 'euclidean')
geo <- dist(helgeogr, method = 'euclidean')
mantel <- mantel.randtest(helfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(helfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(helfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(helfst~env); abline(model, col="darkred")
plot(helfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(helfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Miumulus guttatus #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Mimulus guttatus')

mguenv <- read.csv("M_guttatus_envir.csv", h=T); head(mguenv)
mguenvir <- envdist_varpart(mguenv); mguenvir; pcadata <- mguenvir
mguenvir <- prcomp(mguenvir); summary(mguenvir)
envDist <- as.matrix(mguenvir$x[,1:2])

mgugeogr <- mguenv[,2:3]; mgugeogr
geoDist <- as.matrix(mgugeogr)

genDist <- mgufst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(mguenvir$x[,1:2], method = 'euclidean')
geo <- dist(mgugeogr, method = 'euclidean')
mantel <- mantel.randtest(mgufst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(mgufst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(mgufst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(mgufst~env); abline(model, col="darkred")
plot(mgufst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(mgufst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Centaurea solstitialis #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Centaurea solstitialis')

centenv <- read.csv("C_solstitialis_native_envir_filtered.csv", h=T); head(centenv)
centenvir <- envdist_varpart(centenv); centenvir; pcadata <- centenvir
centenvir <- prcomp(centenvir); summary(centenvir)
envDist <- as.matrix(centenvir$x[,1:2])

centgeogr <- centenv[,2:3]; centgeogr
geoDist <- as.matrix(centgeogr)

genDist <- centfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(centenvir$x[,1:2], method = 'euclidean')
geo <- dist(centgeogr, method = 'euclidean')
mantel <- mantel.randtest(centfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(centfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(centfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(centfst~env); abline(model, col="darkred")
plot(centfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(centfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

#####
# Gen-SNPs
#####
##### Populus balsamifera #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus balsamifera')

pbalenv <- read.csv("P_balsamifera_envir.csv", h=T); head(pbalenv)
pbalenvir <- envdist_varpart(pbalenv); pbalenvir; pcadata <- pbalenvir
pbalenvir <- prcomp(pbalenvir); summary(pbalenvir)
envDist <- as.matrix(pbalenvir$x[,1:2])

pbalgeogr <- pbalenv[,2:3]; pbalgeogr
geoDist <- as.matrix(pbalgeogr)

genDist <- pbalfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(pbalenvir$x[,1:2], method = 'euclidean')
geo <- dist(pbalgeogr, method = 'euclidean')
mantel <- mantel.randtest(pbalfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(pbalfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(pbalfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(pbalfst~env); abline(model, col="darkred")
plot(pbalfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(pbalfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Picea sitchensis #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Picea sitchensis')

psitenv <- read.csv("P_sitchensis_envir_filtered.csv", h=T); head(psitenv)
psitenvir <- envdist_varpart(psitenv); psitenvir; pcadata <- psitenvir
psitenvir <- prcomp(psitenvir); summary(psitenvir)
envDist <- as.matrix(psitenvir$x[,1:2])

psitgeogr <- psitenv[,2:3]; psitgeogr
geoDist <- as.matrix(psitgeogr)

genDist <- psitfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(psitenvir$x[,1:2], method = 'euclidean')
geo <- dist(psitgeogr, method = 'euclidean')
mantel <- mantel.randtest(psitfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(psitfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.25, 0.1))
plot(psitfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(psitfst~env); abline(model, col="darkred")
plot(psitfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(psitfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Populus tremula [ctr] #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus tremula')

ptrecenv <- read.csv("P_tremula_envir.csv", h=T); head(ptrecenv)
ptrecenvir <- envdist_varpart(ptrecenv); ptrecenvir; pcadata <- ptrecenvir
ptrecenvir <- prcomp(ptrecenvir); summary(ptrecenvir)
envDist <- as.matrix(ptrecenvir$x[,1:2])

ptrecgeogr <- ptrecenv[,2:3]; ptrecgeogr
geoDist <- as.matrix(ptrecgeogr)

genDist <- ptrecfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(ptrecenvir$x[,1:2], method = 'euclidean')
geo <- dist(ptrecgeogr, method = 'euclidean')
mantel <- mantel.randtest(ptrecfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(ptrecfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(ptrecfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(ptrecfst~env); abline(model, col="darkred")
plot(ptrecfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(ptrecfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Populus tremula [def] #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus tremula')

ptremenv <- read.csv("P_tremula_envir.csv", h=T); head(ptremenv)
ptremenvir <- envdist_varpart(ptremenv); ptremenvir; pcadata <- ptremenvir
ptremenvir <- prcomp(ptremenvir); summary(ptremenvir)
envDist <- as.matrix(ptremenvir$x[,1:2])

ptremgeogr <- ptremenv[,2:3]; ptremgeogr
geoDist <- as.matrix(ptremgeogr)

genDist <- ptremfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(ptremenvir$x[,1:2], method = 'euclidean')
geo <- dist(ptremgeogr, method = 'euclidean')
mantel <- mantel.randtest(ptremfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(ptremfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(ptremfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(ptremfst~env); abline(model, col="darkred")
plot(ptremfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(ptremfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Helianthus argophyllus #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Helianthus argophyllus')

harenv <- read.csv("H_argophyllus_envir.csv", h=T); head(harenv)
harenvir <- envdist_varpart(harenv); harenvir; pcadata <- harenvir
harenvir <- prcomp(harenvir); summary(harenvir)
envDist <- as.matrix(harenvir$x[,1:2])

hargeogr <- harenv[,2:3]; hargeogr
geoDist <- as.matrix(hargeogr)

genDist <- harfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(harenvir$x[,1:2], method = 'euclidean')
geo <- dist(hargeogr, method = 'euclidean')
mantel <- mantel.randtest(harfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(harfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(harfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(harfst~env); abline(model, col="darkred")
plot(harfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(harfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

#####
# Sel-SNPs
#####
##### Populus balsamifera #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus balsamifera')

pbalsenv <- read.csv("P_balsamifera_envir.csv", h=T); head(pbalsenv)
pbalsenvir <- envdist_varpart(pbalsenv); pbalsenvir; pcadata <- pbalsenvir
pbalsenvir <- prcomp(pbalsenvir); summary(pbalsenvir)
envDist <- as.matrix(pbalsenvir$x[,1:2])

pbalsgeogr <- pbalsenv[,2:3]; pbalsgeogr
geoDist <- as.matrix(pbalsgeogr)

genDist <- pbalsfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(pbalsenvir$x[,1:2], method = 'euclidean')
geo <- dist(pbalsgeogr, method = 'euclidean')
mantel <- mantel.randtest(pbalsfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(pbalsfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(pbalsfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(pbalsfst~env); abline(model, col="darkred")
plot(pbalsfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(pbalsfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Populus tremula #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Populus tremula')

ptresenv <- read.csv("P_tremula_envir.csv", h=T); head(ptresenv)
ptresenvir <- envdist_varpart(ptresenv); ptresenvir; pcadata <- ptresenvir
ptresenvir <- prcomp(ptresenvir); summary(ptresenvir)
envDist <- as.matrix(ptresenvir$x[,1:2])

ptresgeogr <- ptresenv[,2:3]; ptresgeogr
geoDist <- as.matrix(ptresgeogr)

genDist <- ptresfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(ptresenvir$x[,1:2], method = 'euclidean')
geo <- dist(ptresgeogr, method = 'euclidean')
mantel <- mantel.randtest(ptresfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(ptresfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(ptresfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(ptresfst~env); abline(model, col="darkred")
plot(ptresfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(ptresfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))

##### Picea sitchensis #####
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/RDATA/Picea sitchensis')

psitsenv <- read.csv("P_sitchensis_envir_filtered.csv", h=T); head(psitsenv)
psitsenvir <- envdist_varpart(psitsenv); psitsenvir; pcadata <- psitsenvir
psitsenvir <- prcomp(psitsenvir); summary(psitsenvir)
envDist <- as.matrix(psitsenvir$x[,1:2])

psitsgeogr <- psitsenv[,2:3]; psitsgeogr
geoDist <- as.matrix(psitsgeogr)

genDist <- psitsfst; genDist
model <- varpart(Y=genDist,geoDist,envDist);model
plot(model, bg=2:4)

rda <- dbrda(genDist~geoDist+Condition(envDist)) #IBD only
anova(rda)
rda <- dbrda(genDist~envDist+Condition(geoDist)) # IBE only
anova(rda)
rda <- dbrda(genDist~geoDist) # IBD
anova(rda)
rda <- dbrda(genDist~envDist) # IBE
anova(rda)
rda <- dbrda(genDist~envDist+geoDist) # IBD+IBE
anova(rda)

### "Covariance" ###
env <- dist(psitsenvir$x[,1:2], method = 'euclidean')
geo <- dist(psitsgeogr, method = 'euclidean')
mantel <- mantel.randtest(psitsfst, env); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(psitsfst, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))
mantel <- mantel.randtest(env, geo); print(paste("r=",round(mantel$obs, digits = 2), "| P=",round(mantel$pvalue,digits = 3)))

### Supp plots ###
par(mfrow=c(1,3), mai=c(0.7, 0.7, 0.1, 0.1))
plot(psitsfst~env, xlab="Environmental Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkred"); model <- lm(psitsfst~env); abline(model, col="darkred")
plot(psitsfst~geo, xlab="Geographic Distance", ylab=expression("F"["ST"]), pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5, col="darkblue"); model <- lm(psitsfst~geo); abline(model, col="darkblue")
plot(env~geo, xlab="Geographic Distance", ylab="Environmental Distance", pch=19,cex=0.5, cex.axis=1.5, cex.lab=1.5); model <- lm(env~geo); abline(model)
par(mfrow=c(1,1))
