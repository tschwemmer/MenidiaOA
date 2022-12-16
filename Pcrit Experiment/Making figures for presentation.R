#Figures for presentation - Pcrit experiment

#Figure of MO2-O2 curve showing a typical one vs one with a bump at the end. August 2022.
#Trial used: Flax 5dph larvae. Environment: Flax5dphAnalysisEnv2
library(segmented)
#typical MO2-O2 curve examples: P2A5 (ambient) and P2C6 (high)
plot(P2A5$msmrs~P2A5$O2_MEAN,ylab="Mass-Specific Metabolic Rate (umol O2 / mg*h)",xlab="Dissolved Oxygen (umol/L)",
     main="Exp. 2 5dph, P2A5",pch=16,col="steelblue")
segmented(lm(msmrs~O2_MEAN,data=P2A5))
segP2A5<-selgmented(lm(msmrs~O2_MEAN,data=P2A5))
plot(segP2A5,add=T,lwd=2)

plot(P2C6$msmrs~P2C6$O2_MEAN,ylab="Mass-Specific Metabolic Rate (umol O2 / mg*h)",xlab="Dissolved Oxygen (umol/L)",
     main="Exp. 2 5dph, P2C6",pch=16,col="steelblue")
segmented(lm(msmrs~O2_MEAN,data=P2C6))
segP2C6<-selgmented(lm(msmrs~O2_MEAN,data=P2C6))
plot(segP2C6,add=T,lwd=2)

#MO2-O2 curve with bump at end exammples: P2B4 and P2B5 (both medium)
plot(P2B4$msmrs~P2B4$O2_MEAN,ylab="Mass-Specific Metabolic Rate (umol O2 / mg*h)",xlab="Dissolved Oxygen (umol/L)",
     main="Exp. 2 5dph, P2B4",pch=16,col="steelblue")
segmented(lm(msmrs~O2_MEAN,data=P2B4))
segP2B4<-selgmented(lm(msmrs~O2_MEAN,data=P2B4))
segP2B4
plot(segP2B4,add=T,lwd=2)

plot(P2B5$msmrs~P2B5$O2_MEAN,ylab="Mass-Specific Metabolic Rate (umol O2 / mg*h)",xlab="Dissolved Oxygen (umol/L)",
     main="Exp. 2 5dph, P2B5",pch=16,col="steelblue")
segmented(lm(msmrs~O2_MEAN,data=P2B5))
segP2B5<-selgmented(lm(msmrs~O2_MEAN,data=P2B5))
segP2B5
plot(segP2B5,add=T,lwd=2)


######################################
#For manuscript

#Embryos
#Dana
danaembplot<-ggplot(dana_emb_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2])),y=expression(paste("MO"[2]," (umol individual"^-1," h"^-1,")")))+
  coord_cartesian(ylim=c(0,0.011))+
  theme_classic()
print(danaembplot)

#Flax
flaxembplot<-ggplot(flax_emb_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2])),y=expression(paste("MO"[2]," (umol individual"^-1," h"^-1,")")))+
  coord_cartesian(ylim=c(0,0.011))+
  theme_classic()
print(flaxembplot)

#both
library(gridExtra)
grid.arrange(danaembplot,flaxembplot,ncol=2)


#2dph Larvae
#Dana
danalarplot<-ggplot(dana_lar_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2])),y=expression(paste("MO"[2]," (umol mg"^-1," h"^-1,")")))+
  coord_cartesian(ylim=c(0,0.37))+
  theme_classic()
print(danalarplot)

#Flax
flaxlarplot<-ggplot(flax_lar_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2])),y=expression(paste("MO"[2]," (umol mg"^-1," h"^-1,")")))+
  coord_cartesian(ylim=c(0,0.37))+
  theme_classic()
print(flaxlarplot)

#both
library(gridExtra)
grid.arrange(danalarplot,flaxlarplot,ncol=2)


#5dph Larvae
#Dana
danalrvplot<-ggplot(dana_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("A",x=0.1,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2])),y=expression(paste("MO"[2]," (umol mg"^-1," h"^-1,")")))+
  coord_cartesian(ylim=c(0,0.31))+
  theme_classic()
print(danalrvplot)

#Flax
flaxlrvplot<-ggplot(flax_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("B",x=0.1,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2])),y=expression(paste("MO"[2]," (umol mg"^-1," h"^-1,")")))+
  coord_cartesian(ylim=c(0,0.31))+
  theme_classic()
print(flaxlrvplot)

#both
library(gridExtra)
grid.arrange(danalrvplot,flaxlrvplot,ncol=2)
