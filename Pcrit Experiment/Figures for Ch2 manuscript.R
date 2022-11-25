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

emb_flax$CO2mean<-rep(NA)
emb_flax$CO2mean[emb_flax$CO2_level=="amb"]<-358.8
emb_flax$CO2mean[emb_flax$CO2_level=="med"]<-2114.7
emb_flax$CO2mean[emb_flax$CO2_level=="high"]<-4324.5

lar_dana$CO2mean<-rep(NA)
lar_dana$CO2mean[lar_dana$CO2_level=="amb"]<-680.1
lar_dana$CO2mean[lar_dana$CO2_level=="med"]<-1682.8
lar_dana$CO2mean[lar_dana$CO2_level=="high"]<-3609.8

lar_flax$CO2mean<-rep(NA)
lar_flax$CO2mean[lar_flax$CO2_level=="amb"]<-358.8
lar_flax$CO2mean[lar_flax$CO2_level=="med"]<-2114.7
lar_flax$CO2mean[lar_flax$CO2_level=="high"]<-4324.5

lrv_dana$CO2mean<-rep(NA)
lrv_dana$CO2mean[lrv_dana$CO2_level=="amb"]<-680.1
lrv_dana$CO2mean[lrv_dana$CO2_level=="med"]<-1682.8
lrv_dana$CO2mean[lrv_dana$CO2_level=="high"]<-3609.8

lrv_flax$CO2mean<-rep(NA)
lrv_flax$CO2mean[lrv_flax$CO2_level=="amb"]<-358.8
lrv_flax$CO2mean[lrv_flax$CO2_level=="med"]<-2114.7
lrv_flax$CO2mean[lrv_flax$CO2_level=="high"]<-4324.5

emb_dana$TargTemp<-rep("24C")
lar_dana$TargTemp<-rep("24C")
lrv_dana$TargTemp<-rep("24C")

emb_flax$TargTemp<-rep("22C")
lar_flax$TargTemp<-rep("22C")
lrv_flax$TargTemp<-rep("22C")

#need to make columns match then rbind into one dataframe per life stage
emb_dana$ID<-rep(NA)
emb_dana$probs<-rep(NA)
emb_dana$Pcrit_NLR<-rep(NA)
lar_dana$ID<-rep(NA)
lar_dana$probs<-rep(NA)
lrv_dana$ID<-rep(NA)

emb_both<-rbind(emb_dana,emb_flax)
lar_both<-rbind(lar_dana,lar_flax)
lrv_both<-rbind(lrv_dana,lrv_flax)

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
  geom_point(size=3)+
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
  geom_point(size=3)+
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
  geom_point(size=3)+
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
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanRMR-se,ymax=MeanRMR+se),width=100)+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("Routine MO"[2]," (",mu,"mol ind."^"-1"," h"^"-1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,0.0055))+
  theme_classic()+
  theme(legend.position="none")
print(embrmrfig)

#2dph Larvae RMR
larrmrsum<-ddply(lar_both,c("CO2mean","TargTemp"),summarise,N=length(na.omit(RMR)),MeanRMR=mean(RMR,na.rm=TRUE),se=sd(RMR,na.rm=TRUE)/sqrt(N))
larrmrsum

larrmrfig<-ggplot(larrmrsum,aes(x=CO2mean,y=MeanRMR,shape=TargTemp,group=TargTemp))+
  geom_point(size=3)+
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
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16),labels=c("22 (Experiment 2)","24 (Experiment 1)"))+
  geom_errorbar(aes(ymin=MeanRMR-se,ymax=MeanRMR+se),width=100)+
  annotation_custom(grobTree(textGrob("C",x=0.1,y=0.96,gp=gpar(fontsize=14,fontface="bold"))))+
  labs(shape=expression(paste("Temperature (",degree,"C)")),y=expression(paste("Routine MO"[2]," (",mu,"mol mg"^"-1"," h"^"-1",")")),x=expression(paste("pCO"[2], " (",mu,"atm)")))+
  coord_cartesian(ylim=c(0,0.35))+
  theme_classic()+
  theme(legend.position="none")
print(lrvrmrfig)

grid.arrange(embrmrfig,pcritlegend,larrmrfig,lrvrmrfig,ncol=2)

