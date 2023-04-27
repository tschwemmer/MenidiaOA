#Figures for Ch 2

#Need the dataframes emb_dana, emb_flax, lar_dana, lar_flax, lrv_dana, lrv_flax all in the same environment 
#Saved as AllPcritRMR
#Then add a column for mean CO2 (by treatment group, not tank) and target temperature (same temperature for all tanks within an experiment)
#Then for each life stage, plot Pcrit with mean CO2 on the x-axis (numerical) and different color/shape for each Experiment/Temp category
#Then repeat for RMR, to get 6 figures in total
#Use grid.arrange() to line up the three life stages so there is one 3-panel figure for each response variable. 

emb_dana$CO2mean<-rep(NA)
emb_dana$CO2mean[emb_dana$CO2_level=="amb"]<-680.1
emb_dana$CO2mean[emb_dana$CO2_level=="med"]<-1682.8
emb_dana$CO2mean[emb_dana$CO2_level=="high"]<-3609.8

emb_flaxo$CO2mean<-rep(NA)
emb_flaxo$CO2mean[emb_flaxo$CO2_level=="amb"]<-441.7
emb_flaxo$CO2mean[emb_flaxo$CO2_level=="med"]<-2299.2
emb_flaxo$CO2mean[emb_flaxo$CO2_level=="high"]<-4530.9

lar_dana$CO2mean<-rep(NA)
lar_dana$CO2mean[lar_dana$CO2_level=="amb"]<-680.1
lar_dana$CO2mean[lar_dana$CO2_level=="med"]<-1682.8
lar_dana$CO2mean[lar_dana$CO2_level=="high"]<-3609.8

lar_flaxo$CO2mean<-rep(NA)
lar_flaxo$CO2mean[lar_flaxo$CO2_level=="amb"]<-441.7
lar_flaxo$CO2mean[lar_flaxo$CO2_level=="med"]<-2299.2
lar_flaxo$CO2mean[lar_flaxo$CO2_level=="high"]<-4530.9

lrv_danao$CO2mean<-rep(NA)
lrv_danao$CO2mean[lrv_danao$CO2_level=="amb"]<-680.1
lrv_danao$CO2mean[lrv_danao$CO2_level=="med"]<-1682.8
lrv_danao$CO2mean[lrv_danao$CO2_level=="high"]<-3609.8

lrv_flaxo$CO2mean<-rep(NA)
lrv_flaxo$CO2mean[lrv_flaxo$CO2_level=="amb"]<-441.7
lrv_flaxo$CO2mean[lrv_flaxo$CO2_level=="med"]<-2299.2
lrv_flaxo$CO2mean[lrv_flaxo$CO2_level=="high"]<-4530.9

emb_dana$TargTemp<-rep("24C")
lar_dana$TargTemp<-rep("24C")
lrv_danao$TargTemp<-rep("24C")

emb_flaxo$TargTemp<-rep("22C")
lar_flaxo$TargTemp<-rep("22C")
lrv_flaxo$TargTemp<-rep("22C")

#need to make columns match then rbind into one dataframe per life stage
emb_dana$ID<-rep(NA)
emb_dana$probs<-rep(NA)
emb_dana$Pcrit_NLR<-rep(NA)
lar_dana$ID<-rep(NA)
lar_dana$probs<-rep(NA)
lrv_danao$ID<-rep(NA)

emb_both<-rbind(emb_dana,emb_flaxo)
lar_both<-rbind(lar_dana,lar_flaxo)
lrv_both<-rbind(lrv_danao,lrv_flaxo)

#Make the figures
library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(ggpubr)

#Embryo Pcrit
embpcritsum<-ddply(emb_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),se=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
embpcritsum

embpcritfig<-ggplot(embpcritsum,aes(x=CO2mean,y=MeanPcrit,shape=TargTemp,group=TargTemp))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanPcrit-se,ymax=MeanPcrit+se),width=100)+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("P"["crit"]," (mg l"^" -1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,3.5))+
  theme_classic()
print(embpcritfig)

pcritlegend<-get_legend(embpcritfig)
embpcritfig<-embpcritfig+theme(legend.position="none")

#2dph Larvae Pcrit
larpcritsum<-ddply(lar_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),se=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
larpcritsum

larpcritfig<-ggplot(larpcritsum,aes(x=CO2mean,y=MeanPcrit,shape=TargTemp,group=TargTemp))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanPcrit-se,ymax=MeanPcrit+se),width=100)+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("P"["crit"]," (mg l"^" -1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,3.5))+
  theme_classic()+
  theme(legend.position="none")
print(larpcritfig)

#5dph Larvae Pcrit
lrvpcritsum<-ddply(lrv_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),se=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
lrvpcritsum

lrvpcritfig<-ggplot(lrvpcritsum,aes(x=CO2mean,y=MeanPcrit,shape=TargTemp,group=TargTemp))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanPcrit-se,ymax=MeanPcrit+se),width=100)+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("P"["crit"]," (mg l"^" -1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,3.5))+
  theme_classic()+
  theme(legend.position="none")
print(lrvpcritfig)

grid.arrange(embpcritfig,larpcritfig,lrvpcritfig,pcritlegend,ncol=4)

ggarrange(embpcritfig,larpcritfig,lrvpcritfig,ncol=3,common.legend=TRUE,legend="right")

#Embryo RMR
embrmrsum<-ddply(emb_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))
embrmrsum

embrmrfig<-ggplot(embrmrsum,aes(x=CO2mean,y=MeanRMR,shape=TargTemp,group=TargTemp))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanRMR-se,ymax=MeanRMR+se),width=100)+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("Routine MO"[2]," (",mu,"mol ind."^"-1"," h"^"-1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,0.0063))+
  theme_classic()+
  theme(legend.position="none")
print(embrmrfig)

#2dph Larvae RMR
larrmrsum<-ddply(lar_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))
larrmrsum

larrmrfig<-ggplot(larrmrsum,aes(x=CO2mean,y=MeanRMR,shape=TargTemp,group=TargTemp))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanRMR-se,ymax=MeanRMR+se),width=100)+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("Routine MO"[2]," (",mu,"mol mg"^"-1"," h"^"-1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,0.35))+
  theme_classic()+
  theme(legend.position="none")
print(larrmrfig)

#5dph Larvae RMR
lrvrmrsum<-ddply(lrv_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))
lrvrmrsum

lrvrmrfig<-ggplot(lrvrmrsum,aes(x=CO2mean,y=MeanRMR,shape=TargTemp,group=TargTemp))+
  geom_point(size=2)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanRMR-se,ymax=MeanRMR+se),width=100)+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("Routine MO"[2]," (",mu,"mol mg"^"-1"," h"^"-1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,0.35))+
  theme_classic()+
  theme(legend.position="none")
print(lrvrmrfig)

grid.arrange(embrmrfig,pcritlegend,larrmrfig,lrvrmrfig,ncol=2)


#####################################################################################
#Calculating means using both experiments combined

#RMR
embmeanrmr<-ddply(emb_both, "CO2_level", summarise, N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))
larmeanrmr<-ddply(lar_both, "CO2_level", summarise, N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))
lrvmeanrmr<-ddply(lrv_both, "CO2_level", summarise, N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))

#Pcrit
embmeanpcrit<-ddply(emb_both, "CO2_level", summarise, N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),se=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
larmeanpcrit<-ddply(lar_both, "CO2_level", summarise, N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),se=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
lrvmeanpcrit<-ddply(lrv_both, "CO2_level", summarise, N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),se=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))

######################################################################################
#Plotting distribution of Pcrit and RMR at each life stage

hist(emb_both$Pcrit_break,breaks=10) #reciprocal transformed
hist(lar_both$Pcrit_break,breaks=10)
hist(lrv_both$Pcrit_break,breaks=10)

hist(emb_both$RMR,breaks=10) #square-root transformed
hist(lar_both$RMR,breaks=10) #reciprocal transformed
hist(lrv_both$RMR,breaks=10)

boxplot(emb_dana$RMR~emb_dana$CO2_level)
boxplot(lar_dana$RMR~lar_dana$CO2_level)
boxplot(lrv_dana$RMR~lrv_dana$CO2_level)
boxplot(emb_flax$RMR~emb_flax$CO2_level)
boxplot(lar_flax$RMR~lar_flax$CO2_level)
boxplot(lrv_flax$RMR~lrv_flax$CO2_level)


#Plot sample curves for a typical curve, one with a spike, and an oxyconforming one
typseg<-segmented(lm(msmrs~O2_MEAN,data=E2P2B3lrv),seg.Z=~O2_MEAN)
plot(msmrs~O2_MEAN,data=E2P2B3lrv)
plot(typseg,add=T)

spikeseg<-selgmented(lm(msmrs~O2_MEAN,data=E1P1C1lar),seg.Z=~O2_MEAN,type='bic',Kmax=6,msg=F)
plot(msmrs~O2_MEAN,data=E1P1C1lar)
plot(spikeseg,add=T)

ocline<-lm(MO2b~O2_MEAN,data=E2P2B2emb)
ocseg<-selgmented(lm(MO2b~O2_MEAN,data=E2P2B2emb),seg.Z=~O2_MEAN,type='bic',Kmax=2,msg=F)
plot(MO2b~O2_MEAN,data=E2P2B2emb)
abline(ocline,col="black")

