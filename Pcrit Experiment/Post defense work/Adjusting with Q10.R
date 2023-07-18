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

emball2<-emball[emball$Experiment=="dana",]
e1tank2<-e1tank[,c(2,9)]
library(dplyr)
emball2<-full_join(emball2,e1tank2,by="Tank")
emball$pCO2<-c(emball2$pCO2,emball$CO2mean[29:65])

emblmer<-lmer(RMR~CO2_level+(1|Experiment),data=emball)
anova(emblmer)
ranova(emblmer)
TukeyHSD(aov(emball$RMR~emball$CO2_level))
emblm<-lm(sqrt(RMR)~pCO2*Experiment,data=emball)
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
library(olsrr)
ols_test_breusch_pagan(emblm)

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

larall2<-larall[larall$Experiment=="dana",]
larall2<-full_join(larall2,e1tank2,by="Tank")
larall$pCO2<-c(larall2$pCO2,larall$CO2mean[30:66])

larlmer<-lmer(RMR~CO2_level+(1|Experiment),data=larall)
anova(larlmer)
ranova(larlmer)
larlm<-lm(sqrt(RMR)~pCO2*Experiment,data=larall)
summary(larlm) #Experiment is significant but CO2 is not
plot(larall$RMR~larall$CO2mean)

shapiro.test(larall$RMR) #data are not normal, right skewed - try log or sqrt transformation
shapiro.test(residuals(larlm)) #residuals of lm are not normal, right skewed - try log or sqrt transformation
hist(larall$RMR)
hist(residuals(larlm))
leveneTest(larall$RMR,larall$CO2_level)
ols_test_breusch_pagan(larlm)

larlmer2<-lmer(sqrt(RMR)~CO2_level+(1|Experiment),data=larall)
anova(larlmer2)
ranova(larlmer2)
larlm2<-lm(sqrt(RMR)~pCO2*Experiment,data=larall)
summary(larlm2)

shapiro.test(sqrt(larall$RMR)) #data are not normal, right skewed - try log transformation
shapiro.test(residuals(larlm2)) #residuals of lm are normal
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

lrvall2<-lrvall[lrvall$Experiment=="dana",]
lrvall2<-full_join(lrvall2,e1tank2,by="Tank")
lrvall$pCO2<-c(lrvall2$pCO2,lrvall$CO2mean[28:60])

lrvlmer<-lmer(RMR~CO2_level+(1|Tank),data=lrvall)
anova(lrvlmer)
lrvlmer3<-aov(lrvall$RMR~lrvall$CO2_level)
ranova(lrvlmer)
lrvlm<-lm(sqrt(RMR)~pCO2*Experiment,data=lrvall)
summary(lrvlm)
plot(lrvall$RMR~lrvall$CO2mean)

shapiro.test(lrvall$RMR) #data are normal
shapiro.test(residuals(lrvlm)) #residuals of lm are normal
hist(lrvall$RMR)
hist(residuals(lrvlm))
leveneTest(lrvall$RMR,lrvall$CO2_level)
ols_test_breusch_pagan(lrvlm)

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
embplot<-ggplot(emball,aes(x=pCO2,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("RMR (",mu,"mol O"[2]," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.012))+
  theme_classic()+
  theme(legend.position="none")
print(embplot)

larplot<-ggplot(larall,aes(x=pCO2,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()+
  theme(legend.position="none")
print(larplot)

lrvplot<-ggplot(lrvall,aes(x=pCO2,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()+
  theme(legend.position="none")
print(lrvplot)

co2figlegend<-get_legend(embplot)

grid.arrange(embplot,larplot,lrvplot,co2figlegend,ncol=4,widths=c(2,2,2,1))



#Analyze and plot with respect to pH
e1tank3<-e1tank[,c(2,8)]

emball2$pH<-NULL
emball2<-full_join(emball2,e1tank3,by="Tank")
larall2$pH<-NULL
larall2<-full_join(larall2,e1tank3,by="Tank")
lrvall2$pH<-NULL
lrvall2<-full_join(lrvall2,e1tank3,by="Tank")

emball$pH<-c(emball2$pH_m,emball$pH[29:65])
larall$pH<-c(larall2$pH_m,larall$pH[30:66])
lrvall$pH<-c(lrvall2$pH_m,lrvall$pH[28:60])

emblmpH<-lm(RMR~pH*Experiment,data=emball)
summary(emblmpH)

larlmpH<-lm(RMR~pH*Experiment,data=larall)
summary(larlmpH)

lrvlmpH<-lm(RMR~pH*Experiment,data=lrvall)
summary(lrvlmpH)



embplotph<-ggplot(emball,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x="pH",y=expression(paste("RMR (",mu,"mol O"[2]," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.012),xlim=c(7.00,8.15))+
  theme_classic()+
  theme(legend.position="none")
print(embplotph)

larplotph<-ggplot(larall,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x="pH",y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65),xlim=c(7.00,8.15))+
  theme_classic()+
  theme(legend.position="none")
print(larplotph)

lrvplotpH<-ggplot(lrvall,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x="pH",y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65),xlim=c(7.00,8.15))+
  theme_classic()+
  theme(legend.position="none")
print(lrvplotpH)

grid.arrange(embplotph,larplotph,lrvplotpH,co2figlegend,ncol=4,widths=c(2,2,2,1))

#summary tables by CO2 and Experiment
library(plyr)
emb_sum<-ddply(emball,c("CO2_level","Experiment"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
emb_sum
lar_sum<-ddply(larall,c("CO2_level","Experiment"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
lar_sum
lrv_sum<-ddply(lrvall,c("CO2_level","Experiment"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
lrv_sum



#Redo Pcrit analysis to match RMR analysis
emball$Pcrit<-emb_both$Pcrit_break
larall$Pcrit<-lar_both$Pcrit_break
lrvall$Pcrit<-lrv_both$Pcrit_break

#Embryos, pCO2
emblmpcrit<-lm(sqrt(Pcrit)~pCO2*Experiment,data=emball)
summary(emblmpcrit)

shapiro.test(residuals(emblmpcrit)) #residuals of lm are normal
hist(residuals(emblmpcrit))
ols_test_breusch_pagan(emblmpcrit)

#Embryos, pH
emblmpcrit2<-lm(sqrt(Pcrit)~pH*Experiment,data=emball)
summary(emblmpcrit2)

shapiro.test(residuals(emblmpcrit2)) #residuals of lm are normal
hist(residuals(emblmpcrit2))
ols_test_breusch_pagan(emblmpcrit2)

#2dph Larvae, pCO2
larlmpcrit<-lm(sqrt(Pcrit)~pCO2*Experiment,data=larall)
summary(larlmpcrit)

shapiro.test(residuals(larlmpcrit)) #residuals of lm are normal
hist(residuals(larlmpcrit))
ols_test_breusch_pagan(larlmpcrit)

#2dph Larvae, pH
larlmpcrit2<-lm(sqrt(Pcrit)~pH*Experiment,data=larall)
summary(larlmpcrit2)

shapiro.test(residuals(larlmpcrit2)) #residuals of lm are normal
hist(residuals(larlmpcrit2))
ols_test_breusch_pagan(larlmpcrit2)

#5dph Larvae, pCO2
lrvlmpcrit<-lm(sqrt(Pcrit)~pCO2*Experiment,data=lrvall)
summary(lrvlmpcrit)

shapiro.test(residuals(lrvlmpcrit)) #residuals of lm are normal
hist(residuals(lrvlmpcrit))
ols_test_breusch_pagan(lrvlmpcrit)

#5dph Larvae, pH
lrvlmpcrit2<-lm(sqrt(Pcrit)~pH*Experiment,data=lrvall)
summary(lrvlmpcrit2)

shapiro.test(residuals(lrvlmpcrit2)) #residuals of lm are normal
hist(residuals(lrvlmpcrit2))
ols_test_breusch_pagan(lrvlmpcrit2)


#Make summaries and figures
emb_sump<-ddply(emball,c("CO2_level","Experiment"),summarise,N=length(na.omit(Pcrit)),MeanPcrit=mean(Pcrit,na.rm=TRUE),SE=sd(Pcrit,na.rm=TRUE)/sqrt(N))
emb_sump
lar_sump<-ddply(larall,c("CO2_level","Experiment"),summarise,N=length(na.omit(Pcrit)),MeanPcrit=mean(Pcrit,na.rm=TRUE),SE=sd(Pcrit,na.rm=TRUE)/sqrt(N))
lar_sump
lrv_sump<-ddply(lrvall,c("CO2_level","Experiment"),summarise,N=length(na.omit(Pcrit)),MeanPcrit=mean(Pcrit,na.rm=TRUE),SE=sd(Pcrit,na.rm=TRUE)/sqrt(N))
lrv_sump

#plot with respect to pCO2
embplotp<-ggplot(emball,aes(x=pCO2,y=Pcrit,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("P"["crit"]," (mg L"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,4.5))+
  theme_classic()+
  theme(legend.position="none")
print(embplotp)

larplotp<-ggplot(larall,aes(x=pCO2,y=Pcrit,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("P"["crit"]," (mg L"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,4.5))+
  theme_classic()+
  theme(legend.position="none")
print(larplotp)

lrvplotp<-ggplot(lrvall,aes(x=pCO2,y=Pcrit,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("P"["crit"]," (mg L"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,4.5))+
  theme_classic()+
  theme(legend.position="none")
print(lrvplotp)

co2figlegend<-get_legend(embplot)

grid.arrange(embplotp,larplotp,lrvplotp,co2figlegend,ncol=4,widths=c(2,2,2,1))


#plot with respect to pH
embplotp2<-ggplot(emball,aes(x=pH,y=Pcrit,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste("P"["crit"]," (mg L"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,4.5))+
  theme_classic()+
  theme(legend.position="none")
print(embplotp2)

larplotp2<-ggplot(larall,aes(x=pH,y=Pcrit,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste("P"["crit"]," (mg L"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,4.5))+
  theme_classic()+
  theme(legend.position="none")
print(larplotp2)

lrvplotp2<-ggplot(lrvall,aes(x=pH,y=Pcrit,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste("P"["crit"]," (mg L"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,4.5))+
  theme_classic()+
  theme(legend.position="none")
print(lrvplotp2)

co2figlegend<-get_legend(embplot)

grid.arrange(embplotp2,larplotp2,lrvplotp2,co2figlegend,ncol=4,widths=c(2,2,2,1))


#More edits_____________________________________________________________________________________________

#UGJHHHH somehow the pCO2 values are still wrong! Use the values from the dataframes e1tank and e2tank
emball$pCO2[emball$Experiment=="flax"&emball$CO2_level=="amb"]<-441.6598
emball$pCO2[emball$Experiment=="flax"&emball$CO2_level=="med"]<-2299.1721
emball$pCO2[emball$Experiment=="flax"&emball$CO2_level=="high"]<-4530.8646

larall$pCO2[larall$Experiment=="flax"&larall$CO2_level=="amb"]<-441.6598
larall$pCO2[larall$Experiment=="flax"&larall$CO2_level=="med"]<-2299.1721
larall$pCO2[larall$Experiment=="flax"&larall$CO2_level=="high"]<-4530.8646

lrvall$pCO2[lrvall$Experiment=="flax"&lrvall$CO2_level=="amb"]<-441.6598
lrvall$pCO2[lrvall$Experiment=="flax"&lrvall$CO2_level=="med"]<-2299.1721
lrvall$pCO2[lrvall$Experiment=="flax"&lrvall$CO2_level=="high"]<-4530.8646


#Make figures with all data but one regression line
#plot all points with respect to pCO2
embplot3<-ggplot(emball,aes(x=pCO2,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(mapping=aes(x=pCO2,y=RMR),formula=y~x,data=emball,method="lm",se=FALSE,inherit.aes=FALSE,color="black",lwd=0.5)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  annotation_custom(grobTree(textGrob(expression(paste("R"^2,"=0.11")),x=0.8,y=0.90,gp=gpar(fontsize=12))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("RMR (",mu,"mol O"[2]," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.012))+
  theme_classic()+
  theme(legend.position="none")
print(embplot3)

larplot3<-ggplot(larall,aes(x=pCO2,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(mapping=aes(x=pCO2,y=RMR),formula=y~x,data=larall,method="lm",se=FALSE,inherit.aes=FALSE,color="black",lwd=0.5)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  annotation_custom(grobTree(textGrob(expression(paste("R"^2,"=0.013")),x=0.8,y=0.90,gp=gpar(fontsize=12))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()+
  theme(legend.position="none")
print(larplot3)

lrvplot3<-ggplot(lrvall,aes(x=pCO2,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(mapping=aes(x=pCO2,y=RMR),formula=y~x,data=larall,method="lm",se=FALSE,inherit.aes=FALSE,color="black",lwd=0.5)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  annotation_custom(grobTree(textGrob(expression(paste("R"^2,"=0.12")),x=0.8,y=0.90,gp=gpar(fontsize=12))))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()+
  theme(legend.position="none")
print(lrvplot3)


grid.arrange(embplot3,larplot3,lrvplot3,co2figlegend,ncol=4,widths=c(2,2,2,1))



#Analyze and plot with respect to pH
embplotph3<-ggplot(emball,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(mapping=aes(x=pH,y=RMR),formula=y~x,data=emball,method="lm",se=FALSE,inherit.aes=FALSE,color="black",lwd=0.5)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  annotation_custom(grobTree(textGrob("D",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  annotation_custom(grobTree(textGrob(expression(paste("R"^2,"=0.069")),x=0.8,y=0.90,gp=gpar(fontsize=12))))+
  labs(color=NULL,x="pH",y=expression(paste("RMR (",mu,"mol O"[2]," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.012))+
  theme_classic()+
  theme(legend.position="none")
print(embplotph3)

larplotph3<-ggplot(larall,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(mapping=aes(x=pH,y=RMR),formula=y~x,data=larall,method="lm",se=FALSE,inherit.aes=FALSE,color="black",lwd=0.5)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  annotation_custom(grobTree(textGrob("E",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  annotation_custom(grobTree(textGrob(expression(paste("R"^2,"=0.0017")),x=0.8,y=0.90,gp=gpar(fontsize=12))))+
  labs(color=NULL,x="pH",y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()+
  theme(legend.position="none")
print(larplotph3)

lrvplotpH3<-ggplot(lrvall,aes(x=pH,y=RMR,color=Experiment))+
  geom_point(size=1.5,shape=1)+
  geom_smooth(mapping=aes(x=pH,y=RMR),formula=y~x,data=lrvall,method="lm",se=FALSE,inherit.aes=FALSE,color="black",lwd=0.5)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  annotation_custom(grobTree(textGrob("F",x=0.1,y=0.95,gp=gpar(fontsize=16,fontface="bold"))))+
  annotation_custom(grobTree(textGrob(expression(paste("R"^2,"=0.085")),x=0.8,y=0.90,gp=gpar(fontsize=12))))+
  labs(color=NULL,x="pH",y=expression(paste("RMR (",mu,"mol O"[2]," mg"^"-1"," h"^"-1",")")))+
  coord_cartesian(ylim=c(0.0,0.65))+
  theme_classic()+
  theme(legend.position="none")
print(lrvplotpH3)

grid.arrange(embplot3,larplot3,lrvplot3,co2figlegend,embplotph3,larplotph3,lrvplotpH3,ncol=4,widths=c(2,2,2,1))

#Redo the linear model with just pCO2 as independent variable
embmodpco2<-lm(RMR~pCO2,data=emball)
summary(embmodpco2)
shapiro.test(residuals(embmodpco2))
ols_test_breusch_pagan(embmodpco2)

larmodpco2<-lm(RMR~pCO2,data=larall)
summary(larmodpco2)
shapiro.test(residuals(larmodpco2))
ols_test_breusch_pagan(larmodpco2)

lrvmodpco2<-lm(RMR~pCO2,data=lrvall)
summary(lrvmodpco2)
shapiro.test(residuals(lrvmodpco2))
ols_test_breusch_pagan(lrvmodpco2)

#Redo the linear model with just pH as independent variable
embmodpH<-lm(RMR~pH,data=emball)
summary(embmodpH)
shapiro.test(residuals(embmodpH))
ols_test_breusch_pagan(embmodpH)

larmodpH<-lm(RMR~pH,data=larall)
summary(larmodpH)
shapiro.test(residuals(larmodpH))
ols_test_breusch_pagan(larmodpH)

lrvmodpH<-lm(RMR~pH,data=lrvall)
summary(lrvmodpH)
shapiro.test(residuals(lrvmodpH))
ols_test_breusch_pagan(lrvmodpH)


#Bonferoni test to compare all 6 CO2 levels
#First need to organize data so that treatments are "e1amb", "e2amb", etc. and ordered. 
emball$CatTrmt<-rep(NA,times=65)
emball$CatTrmt[emball$Experiment=="dana"&emball$CO2_level=="amb"]<-"680.0uatm"
emball$CatTrmt[emball$Experiment=="dana"&emball$CO2_level=="med"]<-"1683.0uatm"
emball$CatTrmt[emball$Experiment=="dana"&emball$CO2_level=="high"]<-"3609.1uatm"
emball$CatTrmt[emball$Experiment=="flax"&emball$CO2_level=="amb"]<-"441.7uatm"
emball$CatTrmt[emball$Experiment=="flax"&emball$CO2_level=="med"]<-"2299.2uatm"
emball$CatTrmt[emball$Experiment=="flax"&emball$CO2_level=="high"]<-"4530.9uatm"

emball$CatTrmt<-factor(emball$CatTrmt,levels=c("441.7uatm","680.0uatm","1683.0uatm","2299.2uatm","3609.1uatm","4530.9uatm"))

larall$CatTrmt<-rep(NA,times=66)
larall$CatTrmt[larall$Experiment=="dana"&larall$CO2_level=="amb"]<-"680.0uatm"
larall$CatTrmt[larall$Experiment=="dana"&larall$CO2_level=="med"]<-"1683.0uatm"
larall$CatTrmt[larall$Experiment=="dana"&larall$CO2_level=="high"]<-"3609.1uatm"
larall$CatTrmt[larall$Experiment=="flax"&larall$CO2_level=="amb"]<-"441.7uatm"
larall$CatTrmt[larall$Experiment=="flax"&larall$CO2_level=="med"]<-"2299.2uatm"
larall$CatTrmt[larall$Experiment=="flax"&larall$CO2_level=="high"]<-"4530.9uatm"

larall$CatTrmt<-factor(larall$CatTrmt,levels=c("441.7uatm","680.0uatm","1683.0uatm","2299.2uatm","3609.1uatm","4530.9uatm"))

lrvall$CatTrmt<-rep(NA,times=60)
lrvall$CatTrmt[lrvall$Experiment=="dana"&lrvall$CO2_level=="amb"]<-"680.0uatm"
lrvall$CatTrmt[lrvall$Experiment=="dana"&lrvall$CO2_level=="med"]<-"1683.0uatm"
lrvall$CatTrmt[lrvall$Experiment=="dana"&lrvall$CO2_level=="high"]<-"3609.1uatm"
lrvall$CatTrmt[lrvall$Experiment=="flax"&lrvall$CO2_level=="amb"]<-"441.7uatm"
lrvall$CatTrmt[lrvall$Experiment=="flax"&lrvall$CO2_level=="med"]<-"2299.2uatm"
lrvall$CatTrmt[lrvall$Experiment=="flax"&lrvall$CO2_level=="high"]<-"4530.9uatm"

lrvall$CatTrmt<-factor(lrvall$CatTrmt,levels=c("441.7uatm","680.0uatm","1683.0uatm","2299.2uatm","3609.1uatm","4530.9uatm"))


#test for significant differences and calculate group means
pairwise.t.test(emball$RMR,emball$CatTrmt,p.adj='bonferroni')
embcatsum<-ddply(emball,c("CatTrmt"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
embcatsum

pairwise.t.test(larall$RMR,larall$CatTrmt,p.adj='bonferroni')
larcatsum<-ddply(larall,c("CatTrmt"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
larcatsum

pairwise.t.test(lrvall$RMR,lrvall$CatTrmt,p.adj='bonferroni')
lrvcatsum<-ddply(lrvall,c("CatTrmt"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
lrvcatsum


#Look at the oxyconformity and presence of low O2 spike
#First make tables of the percentages vs pCO2, pH, and saturation state for each treatment group
percents<-data.frame("pCO2"<-c(680.0,1683.0,3609.1,441.7,2299.2,4530.9),
                    "pH"<-c(7.94,7.41,7.13,8.08,7.39,7.09),
                    "Omega"<-c(1.92,0.96,0.45,2.37,0.62,0.33),
                    "oxyconf"<-c(40.0,22.2,33.3,0.00,8.33,71.4),
                    "embspike"<-c(10.0,22.2,11.1,18.2,16.7,35.7),
                    "larspike"<-c(100.0,90.0,77.8,66.7,83.3,53.8),
                    "lrvspike"<-c(55.6,37.5,33.3,85.7,75.0,40.0),
                    "Experiment"<-c("Experiment 1","Experiment 1","Experiment 1","Experiment 2","Experiment 2","Experiment 2"))
names(percents)<-c("pCO2","pH","Omega","oxyconf","embspike","larspike","lrvspike","Experiment")

#plot them
oxyconfplotpco2<-ggplot(percents,aes(x=pCO2,y=oxyconf,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("% Oxyconformity")))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(oxyconfplotpco2)

oxyconfplotpH<-ggplot(percents,aes(x=pH,y=oxyconf,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(oxyconfplotpH)

oxyconfplotomega<-ggplot(percents,aes(x=Omega,y=oxyconf,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(2.5,0))+
  labs(color=NULL,x=expression(paste("Aragonite Saturation State")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(oxyconfplotomega)

oxyconflegend<-get_legend(oxyconfplotpco2)

grid.arrange(oxyconfplotpco2,oxyconfplotpH,oxyconfplotomega,oxyconflegend,ncol=4,widths=c(2,2,2,1))
