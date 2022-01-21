#Ionocyte Data Analysis
#January 21, 2022


#Load data for each life stage and save as an environment so I can easily load it again
#all_emb<-read.csv('Ionocyte/All embryo ionocyte data.csv',header=TRUE)
#all_1dph<-read.csv('Ionocyte/All 1dph ionocyte data.csv',header=TRUE)
#all_10mm<-read.csv('Ionocyte/All 10mm ionocyte data.csv',header=TRUE)


#Examine the data and create factors when necessary
str(all_emb)
all_emb$Experiment<-factor(all_emb$Experiment,levels=c("exp1","exp3","exp4"))

str(all_1dph)
all_1dph$Experiment<-factor(all_1dph$Experiment,levels=c("exp1","exp2","exp3","exp4"))

str(all_10mm)
all_10mm$Experiment<-factor(all_10mm$Experiment,levels=c("exp2","exp3"))

#Standard deviation analysis of duplicates - embryo
hist(all_emb$SDYolkmm,breaks=50)
abline(v=mean(all_emb$SDYolkmm)+(3*sd(all_emb$SDYolkmm)),col="red",lwd=3)

hist(all_emb$SDBodymm,breaks=50)
abline(v=mean(all_emb$SDBodymm)+(3*sd(all_emb$SDBodymm)),col="blue",lwd=3)

#Find the sample numbers of the most extreme ones