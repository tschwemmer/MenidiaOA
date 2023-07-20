#Rerunning all of the stats and figures with temperature-corrected pCO2

#Delete the pCO2 column and rename the pCO2c column as pCO2
#MAKE SURE TO SAVE AS A DIFFERENT ENV
emball<-emball[,-c(7)]
larall<-larall[,-c(7)]
lrvall<-lrvall[,-c(7)]

names(emball)<-c("Experiment","Tank","RMR","CO2_level","CO2mean","pH","Pcrit","CatTrmt","pCO2")
names(larall)<-c("Experiment","Tank","RMR","CO2_level","CO2mean","pH","Pcrit","CatTrmt","pCO2")
names(lrvall)<-c("Experiment","Tank","RMR","CO2_level","CO2mean","pH","Pcrit","CatTrmt","pCO2")


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


#Now plot the RMR spike for each stage and explanatory variables
#Embryos
embspikeplotpco2<-ggplot(percents,aes(x=pCO2,y=embspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("% Prevalence in Embryos")))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(embspikeplotpco2)

embspikeplotpH<-ggplot(percents,aes(x=pH,y=embspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(embspikeplotpH)

embspikeplotomega<-ggplot(percents,aes(x=Omega,y=embspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(2.5,0))+
  labs(color=NULL,x=expression(paste("Aragonite Saturation State")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(embspikeplotomega)

grid.arrange(embspikeplotpco2,embspikeplotpH,embspikeplotomega,oxyconflegend,ncol=4,widths=c(2,2,2,1))

#2dph Larvae
larspikeplotpco2<-ggplot(percents,aes(x=pCO2,y=larspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("% Prevalence in 2dph Larvae")))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(larspikeplotpco2)

larspikeplotpH<-ggplot(percents,aes(x=pH,y=larspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(larspikeplotpH)

larspikeplotomega<-ggplot(percents,aes(x=Omega,y=larspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(2.5,0))+
  labs(color=NULL,x=expression(paste("Aragonite Saturation State")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(larspikeplotomega)

grid.arrange(larspikeplotpco2,larspikeplotpH,larspikeplotomega,oxyconflegend,ncol=4,widths=c(2,2,2,1))


#5dph Larvae
lrvspikeplotpco2<-ggplot(percents,aes(x=pCO2,y=lrvspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  labs(color=NULL,x=expression(paste("pCO"[2]," (",mu,"atm)")),y=expression(paste("% Prevalence in 5dph Larvae")))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(lrvspikeplotpco2)

lrvspikeplotpH<-ggplot(percents,aes(x=pH,y=lrvspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(8.15,7.00))+
  labs(color=NULL,x=expression(paste("pH")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(lrvspikeplotpH)

lrvspikeplotomega<-ggplot(percents,aes(x=Omega,y=lrvspike,color=Experiment))+
  geom_point(size=2,shape=16)+
  scale_color_discrete(labels=c("Experiment 1","Experiment 2"))+
  scale_x_reverse(limits=c(2.5,0))+
  labs(color=NULL,x=expression(paste("Aragonite Saturation State")),y=expression(paste(NULL)))+
  coord_cartesian(ylim=c(0,100))+
  theme_classic()+
  theme(legend.position="none")
print(lrvspikeplotomega)

grid.arrange(lrvspikeplotpco2,lrvspikeplotpH,lrvspikeplotomega,oxyconflegend,ncol=4,widths=c(2,2,2,1))



