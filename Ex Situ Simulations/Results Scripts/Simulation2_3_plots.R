############################
# Simulation 2 and 3 plots #
############################

###
# Aim
###

# Get the final plots for genetic indices assessed during simulation 2 and 3 as well as 
# statistical results to assess whether environmental or geographic distance can increase
# genetic diversity and differentiation captured within (simulated) ex situ collections
# (Figure 3).

slope.analysis <- function(data, filter){
  dat <- subset(data, data$Category==filter[1] & data$Simulations==filter[2])
  save <- matrix(ncol = 6, nrow=3)
  colnames(save) <- c("Model","Slope", "Slope.SE", "Tstat", "R.squared", "P.value")
  if(is.factor(dat$Relation)==F){dat$Relation <- as.factor(dat$Relation)}
  for(i in 1:length(levels(dat$Relation))){
    temp <- subset(dat, dat$Relation==levels(dat$Relation)[i])
    model <- summary(lm(temp$values~temp$interval))
    save[i,1] <- as.character(levels(dat$Relation)[i])
    save[i,2] <- round(model$coefficients[2,1], digits = 3)
    save[i,3] <- round(model$coefficients[2,2], digits = 3)
    save[i,4] <- round(model$coefficients[2,3], digits = 3)
    save[i,5] <- round(model$adj.r.squared, digits = 2)
    save[i,6] <- round(model$coefficients[2,4], digits = 3)
  }
  save <- data.frame(save)
  print(save)
  cat(paste("\n\nCategory:", filter[1], ", Simulation:", filter[2]))
}

getSE <- function(data, name, model){
  R <- data[1:(nrow(data)/2),]
  I <- data[((nrow(data)/2)+1):nrow(data),]
  category <- unique(as.character(data[,3]))
  interval <- unique(data[,2])
  for(j in 1:length(category)){
    for(i in 1:length(interval)){
      df <- R[which(R[,3]==category[j] & R[,2]==interval[i]),]
      mean <- mean(df[,1]); SE <- sd(df[,1])/sqrt(nrow(df))
      write.table(data.frame(cbind(mean, SE, interval[i], category[j]), stringsAsFactors = F), 
                  file = paste(name, ".txt", sep = ""), append = T, row.names = F, col.names = F, quote = F) 
    }
  }
  for(j in 1:length(category)){
    for(i in 1:length(interval)){
      df <- I[which(I[,3]==category[j] & I[,2]==interval[i]),]
      mean <- mean(df[,1]); SE <- sd(df[,1])/sqrt(nrow(df))
      write.table(data.frame(cbind(mean, SE, interval[i], category[j]), stringsAsFactors = F), 
                  file = paste(name, "_WP.txt", sep = ""), append = T, row.names = F, col.names = F, quote = F) 
    }
  }
  datR <- read.table(paste(name, ".txt", sep = ""), h=F)
  datI <- read.table(paste(name, "_WP.txt", sep = ""), h=F)
  dat <- rbind(datR, datI); colnames(dat) <- c("mean", "SE", "interval", "Category")
  dat$Simulations <- rep(c("Realistic", "Idealized"), each=(nrow(dat)/2))
  dat$Relation <- rep(model, times=nrow(dat))
  if(file.exists(paste(name, ".txt", sep = ""))==T){file.remove(paste(name, ".txt", sep = ""))}
  if(file.exists(paste(name, "_WP.txt", sep = ""))==T){file.remove(paste(name, "_WP.txt", sep = ""))}
  return(dat)
}

#############################
# Import data and libraries #
#############################
library(ggplot2)
library(cowplot)

setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Ex\ Situ\ Simulations/Simulation_results_R_files')

### EnvRand ###
temp_env = list.files(pattern = '*_EnvRand*');temp_env
data_env = lapply(temp_env, read.csv)

### GeoRand ###
temp_geo = list.files(pattern = '*_GeoRand*');temp_geo
data_geo = lapply(temp_geo, read.csv)

### GeoEnvRand ###
temp_geoenv = list.files(pattern = '*_GeoEnvRand*');temp_geoenv
data_geoenv = lapply(temp_geoenv, read.csv)

###########################################
# Combined plots and Statistical analyses #
###########################################
##### Fst #####

df <- rbind(data_env[[3]], data_env[[4]])
df2 <- rbind(data_geo[[3]], data_geo[[4]])
df3 <- rbind(data_geoenv[[3]], data_geoenv[[4]])

dfm <- getSE(df, "Fst_EnvBased", "Env - Rand")
df2m <- getSE(df2, "Fst_GeoBased", "Geo - Rand")
df3m <- getSE(df3, "Fst_GeoEnvBased", "Env & Geo - Rand")

df <- rbind(df, df2, df3); df$Category <- as.character(df$Category)
df$Category[which(df$Category=="Selected")] <- "Adaptive"; df$Category <- as.factor(df$Category)
df$Simulations <- rep(rep(c('Realistic', 'Idealized'), each=60), times=3)
df$Relation <- rep(c('Env - Rand', 'Geo - Rand', "Env & Geo - Rand"), each=120)
df$Relation <- factor(df$Relation, levels = c('Env - Rand', 'Geo - Rand', "Env & Geo - Rand"))
df$Category <- factor(df$Category, levels = c("Functional","Neutral", "Adaptive"))

dfm <- rbind(dfm, df2m, df3m); dfm$Category <- as.character(dfm$Category)
dfm$Category[which(dfm$Category=="Selected")] <- "Adaptive"; dfm$Category <- as.factor(dfm$Category)
dfm$Relation <- factor(dfm$Relation, levels = c('Env - Rand', 'Geo - Rand', "Env & Geo - Rand"))
dfm$Category <- factor(dfm$Category, levels = c("Functional","Neutral", "Adaptive"))

Fst <- ggplot()+geom_hline(yintercept = 0, linetype='dashed', col='black')+facet_grid(Category~Relation, scales = "free")+theme_bw()+
  geom_smooth(data=df, aes(x=interval, y=values, colour=Simulations), method="lm", se=F,position = position_dodge(width = 1))+
  geom_point(data=dfm, aes(x=interval, y=mean, colour=Simulations), position = position_dodge(width = 1), size=3)+
  geom_errorbar(data=dfm, aes(x=interval, ymax=mean+SE, ymin=mean-SE, colour=Simulations), position = position_dodge(width = 1), width=0)+
  scale_x_continuous(labels = c('0.25-\n0.3','0.3-\n0.4','0.5-\n0.6', '0.7-\n0.8', '0.9-\n1','1-\n1.5'))+
  theme_classic()+scale_color_manual(values=c('black','grey50'))+
  xlab('')+ylab(expression(paste(delta, 'F' ['ST'])))+
  theme(panel.grid = element_blank(), legend.position = 'none', axis.text = element_text(size=12), axis.title = element_text(size=14), strip.text = element_text(size=12), strip.background = element_blank())+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
print(Fst)

### Linear Regression analysis
slope.analysis(data = df, filter = c("Neutral", "Realistic"))
slope.analysis(data = df, filter = c("Neutral", "Idealized"))
slope.analysis(data = df, filter = c("Functional", "Realistic"))
slope.analysis(data = df, filter = c("Functional", "Idealized"))
slope.analysis(data = df, filter = c("Adaptive", "Realistic"))
slope.analysis(data = df, filter = c("Adaptive", "Idealized"))

##### GDcapt #####

df <- rbind(data_env[[5]], data_env[[6]])
df2 <- rbind(data_geo[[5]], data_geo[[6]])
df3 <- rbind(data_geoenv[[5]], data_geoenv[[6]])

dfm <- getSE(df, "GDcapt_EnvBased", "Env - Rand")
df2m <- getSE(df2, "GDcapt_GeoBased", "Geo - Rand")
df3m <- getSE(df3, "GDcapt_GeoEnvBased", "Env & Geo - Rand")

df <- rbind(df, df2, df3); df$Category <- as.character(df$Category)
df$Category[which(df$Category=="Selected")] <- "Adaptive"; df$Category <- as.factor(df$Category)
df$Simulations <- rep(rep(c('Realistic', 'Idealized'), each=60), times=3)
df$Relation <- rep(c('Env - Rand', 'Geo - Rand', "Env & Geo - Rand"), each=120)
df$Relation <- factor(df$Relation, levels = c('Env - Rand', 'Geo - Rand', "Env & Geo - Rand"))
df$Category <- factor(df$Category, levels = c("Functional","Neutral", "Adaptive"))

dfm <- rbind(dfm, df2m, df3m); dfm$Category <- as.character(dfm$Category)
dfm$Category[which(dfm$Category=="Selected")] <- "Adaptive"; dfm$Category <- as.factor(dfm$Category)
dfm$Relation <- factor(dfm$Relation, levels = c('Env - Rand', 'Geo - Rand', "Env & Geo - Rand"))
dfm$Category <- factor(dfm$Category, levels = c("Functional","Neutral", "Adaptive"))

GDcapt <- ggplot()+geom_hline(yintercept = 0, linetype='dashed', col='black')+facet_grid(Category~Relation, scales = "free")+theme_bw()+
  geom_smooth(data=df, aes(x=interval, y=values, colour=Simulations), method="lm", se=F,position = position_dodge(width = 1))+
  geom_point(data=dfm, aes(x=interval, y=mean, colour=Simulations), position = position_dodge(width = 1), size=3)+
  geom_errorbar(data=dfm, aes(x=interval, ymax=mean+SE, ymin=mean-SE, colour=Simulations), position = position_dodge(width = 1), width=0)+
  scale_x_continuous(labels = c('0.25-\n0.3','0.3-\n0.4','0.5-\n0.6', '0.7-\n0.8', '0.9-\n1','1-\n1.5'))+
  theme_classic()+scale_color_manual(values=c('black','grey50'))+
  xlab('Proportion of populations sampled')+ylab(expression(paste(delta, "A" ["c"],"/A" ["d"])))+
  theme(panel.grid = element_blank(), legend.position = 'none', axis.text = element_text(size=12), axis.title = element_text(size=14), strip.text = element_text(size=12), strip.background = element_blank())+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
print(GDcapt)

### Linear Regression analysis
slope.analysis(data = df, filter = c("Neutral", "Realistic"))
slope.analysis(data = df, filter = c("Neutral", "Idealized"))
slope.analysis(data = df, filter = c("Functional", "Realistic"))
slope.analysis(data = df, filter = c("Functional", "Idealized"))
slope.analysis(data = df, filter = c("Adaptive", "Realistic"))
slope.analysis(data = df, filter = c("Adaptive", "Idealized"))

############
# Figure 3 #
############
plot_grid(Fst,GDcapt, labels = c('a','b'), nrow = 2, ncol = 1, align = "hv")
