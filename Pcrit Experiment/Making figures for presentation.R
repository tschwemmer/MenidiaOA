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
