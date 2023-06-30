#Calculating Q10 for Chapter 2 data so that Exp. 2 can be multiplied by Q10 and converted to 24C

#Calculating the Q10 from average MO2 at each temperature (within CO2 treatments or all averaged together?)
Q10emball<-(mean(emb_dana$RMR,na.rm=TRUE)/mean(emb_flaxo$RMR,na.rm=TRUE))^(10/(23.8-22.3)) #The temperatures are from Table 1
Q10emb_amb<-(mean(emb_dana$RMR[emb_dana$CO2_level=="amb"],na.rm=TRUE)/mean(emb_flaxo$RMR[emb_flaxo$CO2_level=="amb"],na.rm=TRUE))^(10/(23.8-22.3))
Q10emb_med<-(mean(emb_dana$RMR[emb_dana$CO2_level=="med"],na.rm=TRUE)/mean(emb_flaxo$RMR[emb_flaxo$CO2_level=="med"],na.rm=TRUE))^(10/(23.8-22.3))
Q10emb_high<-(mean(emb_dana$RMR[emb_dana$CO2_level=="high"],na.rm=TRUE)/mean(emb_flaxo$RMR[emb_flaxo$CO2_level=="high"],na.rm=TRUE))^(10/(23.8-22.3))

#Multiply the RMR at 22C by the Q10 to get what it would be at 24C
#If Q10 tells you how much it changes for every 10 degrees, and I want to know how much it changes for 1.5 degrees...?
#I need to solve for R2 in the equation
#Is it incest to use the Q10 calculated by these data to convert the same data? Or should I use an externally calculated Q10 (such as from Ch. 1)
#And should I be using an average Q10 made from calculating it from every possible combination of RMRs from each temperature? Or is it okay to use the one from the average RMRs?

emb_flaxo$RMRcorr<-emb_flaxo$RMR*(2.47^((23.8-22.3)/10))

plot(emb_flaxo$RMRcorr~emb_flaxo$RMR)
plot(emb_flaxo$RMRcorr)
plot(emb_dana$RMR)


#Now repeat for 2dph and 5dph larvae
Q10larall<-(mean(lar_dana$RMR,na.rm=TRUE)/mean(lar_flaxo$RMR,na.rm=TRUE))^(10/(23.8-22.3)) #The temperatures are from Table 1
lar_flaxo$RMRcorr<-lar_flaxo$RMR*(2.65^((23.8-22.3)/10))

plot(lar_flaxo$RMRcorr~lar_flaxo$RMR)
plot(lar_flaxo$RMRcorr)
plot(lar_dana$RMR)


Q10lrvall<-(mean(lrv_danao$RMR,na.rm=TRUE)/mean(lrv_flaxo$RMR,na.rm=TRUE))^(10/(23.8-22.3)) #The temperatures are from Table 1
lrv_flaxo$RMRcorr<-lrv_flaxo$RMR*(2.65^((23.8-22.3)/10))

plot(lrv_flaxo$RMRcorr~lrv_flaxo$RMR)
plot(lrv_flaxo$RMRcorr)
plot(lrv_danao$RMR)


#And then was the goal to pool them and do a linear mixed effects model with experiment as the random effect?

#Embryos
emball<-data.frame("Experiment"<-c(rep("dana",times=28),rep("flax",times=37)),
                   "Tank"<-c(emb_dana$Tank,emb_flaxo$Tank),
                   "RMR"<-c(emb_dana$RMR,emb_flaxo$RMRcorr),
                   "CO2_level"<-c(emb_dana$CO2_level,emb_flaxo$CO2_level),
                   "CO2mean"<-c(emb_dana$CO2mean,emb_flaxo$CO2mean))
names(emball)<-c("Experiment","Tank","RMR","CO2_level","CO2mean")

emblmer<-lmer(RMR~CO2_level+(1|Experiment),data=emball)
anova(emblmer)
ranova(emblmer)
TukeyHSD(aov(emball$RMR~emball$CO2_level))
emblm<-lm(RMR~CO2mean*Experiment,data=emball)
summary(emblm)
anova(emblmer,lm(RMR~CO2_level,data=emball))
plot(emball$RMR~emball$CO2mean)

embaov<-aov(emball$RMR~emball$CO2_level/factor(emball$Experiment))
summary(embaov)

shapiro.test(emball$RMR) #data are not normal, right skewed - try log or sqrt transformation
shapiro.test(residuals(emblm)) #residuals of lm are normal
hist(emball$RMR)
hist(residuals(emblm))
library(car)
leveneTest(emball$RMR,emball$CO2_level)

emblmer2<-lmer(sqrt(RMR)~CO2_level+(1|Experiment),data=emball)
anova(emblmer2)
ranova(emblmer2)
TukeyHSD(aov(sqrt(emball$RMR)~emball$CO2_level))
emblm2<-lm(sqrt(RMR)~CO2mean,data=emball)
summary(emblm2)

shapiro.test(sqrt(emball$RMR)) #data are normal
shapiro.test(residuals(emblm2)) #residuals of lm are normal
hist(sqrt(emball$RMR))
hist(residuals(emblm2))
library(car)
leveneTest(sqrt(emball$RMR),emball$CO2_level)


#2dph Larvae
larall<-data.frame("Experiment"<-c(rep("dana",times=29),rep("flax",times=37)),
                   "Tank"<-c(lar_dana$Tank,lar_flaxo$Tank),
                   "RMR"<-c(lar_dana$RMR,lar_flaxo$RMRcorr),
                   "CO2_level"<-c(lar_dana$CO2_level,lar_flaxo$CO2_level),
                   "CO2mean"<-c(lar_dana$CO2mean,lar_flaxo$CO2mean))
names(larall)<-c("Experiment","Tank","RMR","CO2_level","CO2mean")

larlmer<-lmer(RMR~CO2_level+(1|Experiment),data=larall)
anova(larlmer)
ranova(larlmer)
larlm<-lm(RMR~CO2mean*Experiment,data=larall)
summary(larlm) #Experiment is significant but CO2 is not
plot(larall$RMR~larall$CO2mean)

shapiro.test(larall$RMR) #data are not normal, right skewed - try log or sqrt transformation
shapiro.test(residuals(larlm)) #residuals of lm are not normal, right skewed - try log or sqrt transformation
hist(larall$RMR)
hist(residuals(larlm))
leveneTest(larall$RMR,larall$CO2_level)

larlmer2<-lmer(sqrt(RMR)~CO2_level+(1|Experiment),data=larall)
anova(larlmer2)
ranova(larlmer2)
larlm2<-lm(sqrt(RMR)~CO2mean*Experiment,data=larall)
summary(larlm2)

shapiro.test(sqrt(larall$RMR)) #data are not normal, right skewed - try log transformation
shapiro.test(residuals(larlm2)) #residuals of lm are not normal, right skewed - try log transformation
hist(sqrt(larall$RMR))
hist(residuals(larlm2))
leveneTest(sqrt(larall$RMR),larall$CO2_level)


#5dph Larvae
lrvall<-data.frame("Experiment"<-c(rep("dana",times=27),rep("flax",times=33)),
                   "Tank"<-c(lrv_danao$Tank,lrv_flaxo$Tank),
                   "RMR"<-c(lrv_danao$RMR,lrv_flaxo$RMRcorr),
                   "CO2_level"<-c(lrv_danao$CO2_level,lrv_flaxo$CO2_level),
                   "CO2mean"<-c(lrv_danao$CO2mean,lrv_flaxo$CO2mean))
names(lrvall)<-c("Experiment","Tank","RMR","CO2_level","CO2mean")

lrvlmer<-lmer(RMR~CO2_level+(1|Tank),data=lrvall)
anova(lrvlmer)
lrvlmer3<-aov(lrvall$RMR~lrvall$CO2_level)
ranova(lrvlmer)
lrvlm<-lm(RMR~CO2mean*Experiment,data=lrvall)
summary(lrvlm)
plot(lrvall$RMR~lrvall$CO2mean)

shapiro.test(lrvall$RMR) #data are normal
shapiro.test(residuals(lrvlm)) #residuals of lm are normal
hist(lrvall$RMR)
hist(residuals(lrvlm))
leveneTest(lrvall$RMR,lrvall$CO2_level)
#no transformation needed

#nest fish within tank within experiment 
#send to everyone still



#summary tables and plotting
library(plyr)
emb_sum<-ddply(emball,"CO2_level",summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
emb_sum
lar_sum<-ddply(larall,"CO2_level",summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
lar_sum
lrv_sum<-ddply(lrvall,"CO2_level",summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
lrv_sum

#plot group means by CO2 only
library(ggplot2)
library(grid)
library(gridExtra)
embmeansplot<-ggplot(emb_sum,aes(x=CO2_level,y=MeanRMR))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanRMR-SE,ymax=MeanRMR+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("Embryos",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.006))+
  theme_classic()
print(embmeansplot)

larmeansplot<-ggplot(lar_sum,aes(x=CO2_level,y=MeanRMR))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanRMR-SE,ymax=MeanRMR+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("2dph Larvae",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.35))+
  theme_classic()
print(larmeansplot)

lrvmeansplot<-ggplot(lrv_sum,aes(x=CO2_level,y=MeanRMR))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanRMR-SE,ymax=MeanRMR+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.35))+
  theme_classic()
print(lrvmeansplot)

grid.arrange(embmeansplot,larmeansplot,lrvmeansplot,ncol=3)


#plot all points with respect to pCO2
embplot<-ggplot(emball,aes(x=CO2mean,y=RMR,color=Experiment))+
  geom_point(size=2,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  annotation_custom(grobTree(textGrob("Embryos",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.012))+
  theme_classic()
print(embplot)

larplot<-ggplot(larall,aes(x=CO2mean,y=RMR,color=Experiment))+
  geom_point(size=2,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  annotation_custom(grobTree(textGrob("2dph Larvae",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()
print(larplot)

lrvplot<-ggplot(lrvall,aes(x=CO2mean,y=RMR,color=Experiment))+
  geom_point(size=2,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  annotation_custom(grobTree(textGrob("5dph Larvae",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()
print(lrvplot)

grid.arrange(embplot,larplot,lrvplot,ncol=3)



#Plot with respect to pH
emball$pH<-c(7.94,7.94,7.13,7.41,7.13,7.13,7.41,7.13,7.41,7.13,
             7.94,7.94,7.41,7.13,7.94,7.41,7.94,7.94,7.13,7.13,7.13,
             7.94,7.94,7.41,7.41,7.41,7.41,7.94,
             8.08,8.08,8.08,8.08,7.39,7.39,7.39,7.39,8.08,7.39,7.39,7.39,7.39,
             8.08,7.09,7.09,7.09,7.09,8.08,8.08,8.08,8.08,8.08,
             7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.39,7.39,7.39,7.39)

larall$pH<-c(7.41,7.41,7.41,7.41,7.94,7.94,7.94,7.94,7.13,7.13,7.13,7.13,
             7.94,7.94,7.94,7.41,7.41,7.41,7.13,7.13,7.13,7.13,7.94,7.94,7.94,7.13,7.41,7.41,7.41,
             8.08,8.08,8.08,8.08,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,
             8.08,8.08,8.08,8.08,8.08,7.39,7.39,7.39,7.39,7.39,7.39,7.39,7.39,7.39,
             7.09,7.09,7.09,7.09,7.09,7.39,7.39,7.39,7.39)

lrvall$pH<-c(7.13,7.13,7.13,7.13,7.13,7.41,7.94,7.94,7.94,7.13,7.13,7.13,7.13,
             7.41,7.41,7.41,7.41,7.94,7.94,7.94,7.94,7.94,7.94,7.94,7.41,7.41,7.41,
             7.39,7.39,7.39,7.09,7.09,7.09,7.39,7.39,7.39,7.39,8.08,8.08,8.08,8.08,8.08,8.08,8.08,8.08,
             7.39,7.39,7.39,7.39,7.39,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09,7.09)

embplotph<-ggplot(emball,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=2,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  annotation_custom(grobTree(textGrob("Embryos",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.012))+
  theme_classic()
print(embplotph)

larplotph<-ggplot(larall,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=2,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  annotation_custom(grobTree(textGrob("2dph Larvae",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()
print(larplotph)

lrvplotpH<-ggplot(lrvall,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=2,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  annotation_custom(grobTree(textGrob("5dph Larvae",x=0.5,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()
print(lrvplotpH)

grid.arrange(embplotph,larplotph,lrvplotpH,ncol=3)

#Redo the models with pH too
#But first need to enter the correct CO2 values for each individual tank in Exp 1
emball$CO2<-c()