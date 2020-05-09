###
# Analysis of variation partitioning results
###

###
# Aim
###

# To perform bootstrapping on each variance fraction for all genetic marker classes.

#####
# Import dependencies
#####
library(ggplot2)
library(cowplot)
library(boot)
library(simpleboot)

#################
# Bootstrapping #
#################
setwd('/Volumes/LACIE\ SHARE/Ph.D/6_Writings/Manuscripts/Chapter\ 3/Manuscript/Review/Datasets\ and\ Codes/Variation\ Partitioning/R\ Scripts')
varpart <- read.csv("Varpart_dataset.csv", h=T); head(varpart)

## IBD ##
IBDboot <- matrix(ncol=4, nrow=3)
colnames(IBDboot) <- c("category","statistic", "lower", "upper")
IBDboot[,1] <- c("Neutral","Functional", "Selected"); IBDboot

neut <- one.boot(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBD"], median, R=2000)
neutci <- boot.ci(neut, conf=.95, type = 'basic')
IBDboot[1,2] <- median(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBD"])
IBDboot[1,3]<-neutci$basic[4]; IBDboot[1,4]<-neutci$basic[5]
fun <- one.boot(varpart$value[varpart$category=="Functional" & varpart$variable=="IBD"], median, R=2000)
funci <- boot.ci(fun, conf=.95, type='basic')
IBDboot[2,2] <- median(varpart$value[varpart$category=="Functional" & varpart$variable=="IBD"])
IBDboot[2,3]<-funci$basic[4]; IBDboot[2,4]<-funci$basic[5]
sel <- one.boot(varpart$value[varpart$category=="Selected" & varpart$variable=="IBD"], median, R=2000)
selci <- boot.ci(sel, conf=.95, type='basic')
IBDboot[3,2] <- median(varpart$value[varpart$category=="Selected" & varpart$variable=="IBD"])
IBDboot[3,3]<-selci$basic[4]; IBDboot[3,4]<-selci$basic[5]
IBDboot <- data.frame(IBDboot);IBDboot$statistic <- as.character(IBDboot$statistic); IBDboot$lower <- as.character(IBDboot$lower); IBDboot$upper <- as.character(IBDboot$upper); str(IBDboot)
IBDboot <- data.frame(IBDboot);IBDboot$statistic <- as.numeric(IBDboot$statistic); IBDboot$lower <- as.numeric(IBDboot$lower); IBDboot$upper <- as.numeric(IBDboot$upper); str(IBDboot)
IBDboot

## IBE ##
IBEboot <- matrix(ncol=4, nrow=3)
colnames(IBEboot) <- c("category","statistic", "lower", "upper")
IBEboot[,1] <- c("Neutral","Functional", "Selected"); IBEboot

neut <- one.boot(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBE"], median, R=2000)
neutci <- boot.ci(neut, conf=.95, type = 'basic')
IBEboot[1,2] <- median(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBE"])
IBEboot[1,3]<-neutci$basic[4]; IBEboot[1,4]<-neutci$basic[5]
fun <- one.boot(varpart$value[varpart$category=="Functional" & varpart$variable=="IBE"], median, R=2000)
funci <- boot.ci(fun, conf=.95, type='basic')
IBEboot[2,2] <- median(varpart$value[varpart$category=="Functional" & varpart$variable=="IBE"])
IBEboot[2,3]<-funci$basic[4]; IBEboot[2,4]<-funci$basic[5]
sel <- one.boot(varpart$value[varpart$category=="Selected" & varpart$variable=="IBE"], median, R=2000)
selci <- boot.ci(sel, conf=.95, type='basic')
IBEboot[3,2] <- median(varpart$value[varpart$category=="Selected" & varpart$variable=="IBE"])
IBEboot[3,3]<-selci$basic[4]; IBEboot[3,4]<-selci$basic[5]
IBEboot <- data.frame(IBEboot);IBEboot$statistic <- as.character(IBEboot$statistic); IBEboot$lower <- as.character(IBEboot$lower); IBEboot$upper <- as.character(IBEboot$upper); str(IBEboot)
IBEboot <- data.frame(IBEboot);IBEboot$statistic <- as.numeric(IBEboot$statistic); IBEboot$lower <- as.numeric(IBEboot$lower); IBEboot$upper <- as.numeric(IBEboot$upper); str(IBEboot)
IBEboot

## IBDE ##
IBDEboot <- matrix(ncol=4, nrow=3)
colnames(IBDEboot) <- c("category","statistic", "lower", "upper")
IBDEboot[,1] <- c("Neutral","Functional", "Selected"); IBDEboot

neut <- one.boot(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBD_IBE"], median, R=2000)
neutci <- boot.ci(neut, conf=.95, type = 'basic')
IBDEboot[1,2] <- median(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBD_IBE"])
IBDEboot[1,3]<-neutci$basic[4]; IBDEboot[1,4]<-neutci$basic[5]
fun <- one.boot(varpart$value[varpart$category=="Functional" & varpart$variable=="IBD_IBE"], median, R=2000)
funci <- boot.ci(fun, conf=.95, type='basic')
IBDEboot[2,2] <- median(varpart$value[varpart$category=="Functional" & varpart$variable=="IBD_IBE"])
IBDEboot[2,3]<-funci$basic[4]; IBDEboot[2,4]<-funci$basic[5]
sel <- one.boot(varpart$value[varpart$category=="Selected" & varpart$variable=="IBD_IBE"], median, R=2000)
selci <- boot.ci(sel, conf=.95, type='basic')
IBDEboot[3,2] <- median(varpart$value[varpart$category=="Selected" & varpart$variable=="IBD_IBE"])
IBDEboot[3,3]<-selci$basic[4]; IBDEboot[3,4]<-selci$basic[5]
IBDEboot <- data.frame(IBDEboot);IBDEboot$statistic <- as.character(IBDEboot$statistic); IBDEboot$lower <- as.character(IBDEboot$lower); IBDEboot$upper <- as.character(IBDEboot$upper); str(IBDEboot)
IBDEboot <- data.frame(IBDEboot);IBDEboot$statistic <- as.numeric(IBDEboot$statistic); IBDEboot$lower <- as.numeric(IBDEboot$lower); IBDEboot$upper <- as.numeric(IBDEboot$upper); str(IBDEboot)
IBDEboot

## IBD and IBE ##
IBDandIBEboot <- matrix(ncol=4, nrow=3)
colnames(IBDandIBEboot) <- c("category","statistic", "lower", "upper")
IBDandIBEboot[,1] <- c("Neutral","Functional", "Selected"); IBDandIBEboot

neut <- one.boot(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBDandIBE"], median, R=2000)
neutci <- boot.ci(neut, conf=.95, type = 'basic')
IBDandIBEboot[1,2] <- median(varpart$value[varpart$category=="Neutral" & varpart$variable=="IBDandIBE"])
IBDandIBEboot[1,3]<-neutci$basic[4]; IBDandIBEboot[1,4]<-neutci$basic[5]
fun <- one.boot(varpart$value[varpart$category=="Functional" & varpart$variable=="IBDandIBE"], median, R=2000)
funci <- boot.ci(fun, conf=.95, type='basic')
IBDandIBEboot[2,2] <- median(varpart$value[varpart$category=="Functional" & varpart$variable=="IBDandIBE"])
IBDandIBEboot[2,3]<-funci$basic[4]; IBDandIBEboot[2,4]<-funci$basic[5]
sel <- one.boot(varpart$value[varpart$category=="Selected" & varpart$variable=="IBDandIBE"], median, R=2000)
selci <- boot.ci(sel, conf=.95, type='basic')
IBDandIBEboot[3,2] <- median(varpart$value[varpart$category=="Selected" & varpart$variable=="IBDandIBE"])
IBDandIBEboot[3,3]<-selci$basic[4]; IBDandIBEboot[3,4]<-selci$basic[5]
IBDandIBEboot <- data.frame(IBDandIBEboot);IBDandIBEboot$statistic <- as.character(IBDandIBEboot$statistic); IBDandIBEboot$lower <- as.character(IBDandIBEboot$lower); IBDandIBEboot$upper <- as.character(IBDandIBEboot$upper); str(IBDandIBEboot)
IBDandIBEboot <- data.frame(IBDandIBEboot);IBDandIBEboot$statistic <- as.numeric(IBDandIBEboot$statistic); IBDandIBEboot$lower <- as.numeric(IBDandIBEboot$lower); IBDandIBEboot$upper <- as.numeric(IBDandIBEboot$upper); str(IBDandIBEboot)
IBDandIBEboot

###########
# Figures #
###########
boot <- rbind(IBDboot, IBEboot, IBDEboot, IBDandIBEboot)
boot$variable <- rep(c("IBD", "IBE", "IBDuIBE", "IBDnIBE"), each=3)
boot$variable <- factor(boot$variable, levels = c("IBD", "IBE", "IBDnIBE", "IBDuIBE"));str(boot)
boot$category <- as.character(boot$category)
for(i in 1:nrow(boot)){
  if(boot[i,1]=="Selected"){boot[i,1] <- "Adaptive"}
}
boot$category <- factor(boot$category, levels = c("Neutral", "Functional", "Adaptive"));str(boot)

B <- ggplot()+geom_point(data=boot, aes(x=category, y=statistic), size=1.5)+facet_grid(~variable)+
  geom_errorbar(data=boot, aes(x=category, ymin=lower, ymax=upper), width=0.2)+
  theme_classic()+theme(panel.grid = element_blank(), axis.text = element_text(size=11), 
                   axis.title = element_text(size=14), strip.text = element_text(size=12),
                   strip.background = element_blank())+
  xlab("")+ylab("Proportion of genetic differentiation")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
print(B)

boot <- boot[-c(7:9),]; boot
A <- ggplot()+geom_point(data=boot, aes(x=variable, y=statistic), size=1.5)+facet_grid(~category)+
  geom_errorbar(data=boot, aes(x=variable, ymin=lower, ymax=upper), width=0.2)+
  theme_classic()+theme(panel.grid = element_blank(), axis.text = element_text(size=11), 
                   axis.title = element_text(size=14), strip.text = element_text(size=12),
                   strip.background = element_blank())+
  xlab("")+ylab("Proportion of genetic differentiation")+  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
print(A)

plot_grid(A,B, nrow = 2, ncol = 1, labels = c("a","b"))
