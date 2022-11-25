#Respirometry analysis from 2021 experiment

#Run 2: Embryos, Flax Pond, 6-15-21

#load data sheets and treatments
emb_p1orig<-read.csv(file.choose(),header=TRUE)
emb_p2orig<-read.csv(file.choose(),header=TRUE)
emb_trmt_p1<-read.csv(file.choose(),header=TRUE)
emb_trmt_p2<-read.csv(file.choose(),header=TRUE)

#
emb_p1<-emb_p1orig[32:6965,]
emb_p2<-emb_p2orig[32:6965,]
row.names(emb_p1)<-NULL
row.names(emb_p2)<-NULL

#check structure
str(emb_p1)
str(emb_p2)
str(emb_trmt_p1)
str(emb_trmt_p2)

#make the treatment variables factors
emb_trmt_p1$Well<-factor(emb_trmt_p1$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
emb_trmt_p2$Well<-factor(emb_trmt_p2$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
emb_trmt_p1$CO2_level<-factor(emb_trmt_p1$CO2_level,levels=c("amb","med","high","blankamb","blankmed","blankhigh"))
emb_trmt_p2$CO2_level<-factor(emb_trmt_p2$CO2_level,levels=c("amb","med","high","blankamb","blankmed","blankhigh"))
emb_trmt_p1$Tank<-factor(emb_trmt_p1$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))
emb_trmt_p2$Tank<-factor(emb_trmt_p2$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))


#calculate slope for each well using all untrimmed data. Units will be umol consumed per hour
slopes_r1p1<-data.frame(names(emb_p1)[4:27], sapply(emb_p1[4:27],function(x) ((-coef(summary(lm(x~emb_p1$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(emb_p1[4:27],function(x) summary(lm(x~emb_p1$Time.Sec.))$r.squared))
names(slopes_r1p1)<-c("Well","MO2","Rsquared")
slopes_r1p2<-data.frame(names(emb_p2)[4:27], sapply(emb_p2[4:27],function(x) ((-coef(summary(lm(x~emb_p2$Time.Sec.)))[2])/31.9988)*1800))
names(slopes_r1p2)<-c("Well","MO2")

#plot data for all live wells
par(mfrow=c(2,5))
plot(emb_p1$A1~emb_p1$Time.Min.)
plot(emb_p1$A2~emb_p1$Time.Min.)
plot(emb_p1$A4~emb_p1$Time.Min.)
plot(emb_p1$A5~emb_p1$Time.Min.)
plot(emb_p1$A6~emb_p1$Time.Min.)
plot(emb_p1$B1~emb_p1$Time.Min.)
plot(emb_p1$B3~emb_p1$Time.Min.)
plot(emb_p1$B4~emb_p1$Time.Min.)
plot(emb_p1$B5~emb_p1$Time.Min.)
plot(emb_p1$B6~emb_p1$Time.Min.)
plot(emb_p1$C1~emb_p1$Time.Min.)
plot(emb_p1$C2~emb_p1$Time.Min.)
plot(emb_p1$C4~emb_p1$Time.Min.)
plot(emb_p1$C5~emb_p1$Time.Min.)
plot(emb_p1$C6~emb_p1$Time.Min.)
plot(emb_p1$D1~emb_p1$Time.Min.)
plot(emb_p1$D2~emb_p1$Time.Min.)
plot(emb_p1$D3~emb_p1$Time.Min.)
plot(emb_p1$D4~emb_p1$Time.Min.)
abline(v=c(100,200,300,400))

#blanks
par(mfrow=c(2,3))
plot(emb_p1$A3~emb_p1$Time.Min.)
plot(emb_p1$B2~emb_p1$Time.Min.)
plot(emb_p1$C3~emb_p1$Time.Min.)
plot(emb_p1$D5~emb_p1$Time.Min.)
plot(emb_p1$D6~emb_p1$Time.Min.)

#plate 2
par(mfrow=c(2,5))
plot(emb_p2$A2~emb_p2$Time.Min.)
plot(emb_p2$A3~emb_p2$Time.Min.)
plot(emb_p2$A4~emb_p2$Time.Min.)
plot(emb_p2$A5~emb_p2$Time.Min.)
plot(emb_p2$A6~emb_p2$Time.Min.)
plot(emb_p2$B1~emb_p2$Time.Min.)
plot(emb_p2$B2~emb_p2$Time.Min.)
plot(emb_p2$B4~emb_p2$Time.Min.)
plot(emb_p2$B5~emb_p2$Time.Min.)
plot(emb_p2$B6~emb_p2$Time.Min.)
plot(emb_p2$C1~emb_p2$Time.Min.)
plot(emb_p2$C2~emb_p2$Time.Min.)
plot(emb_p2$C3~emb_p2$Time.Min.)
plot(emb_p2$C4~emb_p2$Time.Min.)
plot(emb_p2$C5~emb_p2$Time.Min.)
plot(emb_p2$D2~emb_p2$Time.Min.)
plot(emb_p2$D3~emb_p2$Time.Min.)
plot(emb_p2$D4~emb_p2$Time.Min.)
plot(emb_p2$D5~emb_p2$Time.Min.)
plot(emb_p2$D6~emb_p2$Time.Min.)
abline(v=c(100,400,600,1000))

#blanks
par(mfrow=c(2,2))
plot(emb_p2$A1~emb_p2$Time.Min.)
plot(emb_p2$B3~emb_p2$Time.Min.)
plot(emb_p2$C6~emb_p2$Time.Min.)
plot(emb_p2$D1~emb_p2$Time.Min.)

#Trim the data and calculate the overall slopes again
#For RMR try using initial ~30 min, or average 3 20 min chunks
#emb_p1_rmr<-data.frame(emb_p1$Time.Min.[180:270],emb_p1$Time.Sec.[180:270],emb_p1$A1[180:270],emb_p1$A4[180:270],emb_p1$A5[180:270],
#                       emb_p1$A6[180:270],emb_p1$B1[180:270],emb_p1$B3[180:270],emb_p1$B4[180:270],emb_p1$B5[180:270],emb_p1$B6[180:270],emb_p1$C1[180:270],
#                       emb_p1$C2[180:270],emb_p1$C4[180:270],emb_p1$C5[180:270],emb_p1$C6[180:270],emb_p1$D1[180:270],emb_p1$D2[180:270],emb_p1$D3[180:270],
#                       emb_p1$D4[180:270],
#                       emb_p1$A3[330:420],emb_p1$B2[330:420],emb_p1$C3[330:420],emb_p1$D5[330:420],emb_p1$D6[330:420])
#names(emb_p1_rmr)<-c("Time.Min.","Time.Sec.","A1","A4","A5","A6","B1","B3","B4","B5","B6","C1","C2","C4","C5","C6","D1","D2","D3","D4","A3","B2","C3","D5","D6")
#row.names(emb_p1_rmr)<-NULL

#emb_p2_rmr<-emb_p2[180:270,-c(1,23,28)] #This excludes D5 and the temperature and date columns
#row.names(emb_p2_rmr)<-NULL

#Trim the data and calculate the overall slopes again, this time using data after spikes when applicable
#For RMR try using initial ~30 min, or average 3 20 min chunks
emb_p1_rmr<-data.frame(emb_p1$Time.Min.[750:840],emb_p1$Time.Sec.[750:840],emb_p1$A1[750:840],emb_p1$A4[750:840],emb_p1$A5[750:840],
                       emb_p1$A6[750:840],emb_p1$B1[750:840],emb_p1$B3[750:840],emb_p1$B4[750:840],emb_p1$B5[1320:1410],emb_p1$B6[1320:1410],emb_p1$C1[960:1050],
                       emb_p1$C2[750:840],emb_p1$C4[1320:1410],emb_p1$C5[750:840],emb_p1$C6[1320:1410],emb_p1$D1[360:450],emb_p1$D2[360:450],emb_p1$D3[360:450],
                       emb_p1$D4[360:450],
                       emb_p1$A3[570:660],emb_p1$B2[570:660],emb_p1$C3[570:660],emb_p1$D5[570:660],emb_p1$D6[570:660])
names(emb_p1_rmr)<-c("Time.Min.","Time.Sec.","A1","A4","A5","A6","B1","B3","B4","B5","B6","C1","C2","C4","C5","C6","D1","D2","D3","D4","A3","B2","C3","D5","D6")
row.names(emb_p1_rmr)<-NULL

emb_p2_rmr<-data.frame(emb_p2$Time.Min.[330:420],emb_p2$Time.Sec.[330:420],emb_p2$A2[330:420],emb_p2$A3[1499:1589],emb_p2$A4[1829:1919],emb_p2$A5[330:420],
                       emb_p2$A6[330:420],emb_p2$B1[330:420],emb_p2$B2[330:420],emb_p2$B4[330:420],emb_p2$B5[330:420],emb_p2$B6[330:420],emb_p2$C1[330:420],
                       emb_p2$C2[330:420],emb_p2$C3[330:420],emb_p2$C4[330:420],emb_p2$C5[330:420],emb_p2$D2[330:420],emb_p2$D3[330:420],emb_p2$D4[330:420],
                       emb_p2$D6[690:780],
                       emb_p2$A3[330:420],emb_p2$B2[330:420],emb_p2$C3[330:420],emb_p2$D5[330:420],emb_p2$D6[330:420])
names(emb_p2_rmr)<-c("Time.Min.","Time.Sec.","A2","A3","A4","A5","A6","B1","B2","B4","B5","B6","C1","C2","C3","C4","C5","D2","D2","D4","D6","A1","B3","C6","D1")
row.names(emb_p2_rmr)<-NULL

#calculate slopes
slopes_r1p1_rmr<-data.frame(names(emb_p1_rmr)[3:25], sapply(emb_p1_rmr[3:25],function(x) ((-coef(summary(lm(x~emb_p1_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(emb_p1_rmr[3:25],function(x) summary(lm(x~emb_p1_rmr$Time.Sec.))$r.squared))
names(slopes_r1p1_rmr)<-c("Well","MO2","Rsquared")

slopes_r1p2_rmr<-data.frame(names(emb_p2_rmr)[3:25], sapply(emb_p2_rmr[3:25],function(x) ((-coef(summary(lm(x~emb_p2_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(emb_p2_rmr[3:25],function(x) summary(lm(x~emb_p2_rmr$Time.Sec.))$r.squared))
names(slopes_r1p2_rmr)<-c("Well","MO2","Rsquared")


#calculate the blanks
blank1<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A3"],slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D6"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A1"])
blank2<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B2"],slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B3"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D1"])
blank3<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D5"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B3"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C6"])



#Subtract the blanks
emb_p1b<-data.frame(c("A1","A4","A5","A6","B1","B3","B4","B5","B6","C1","C2","C4","C5","C6","D1","D2","D3","D4"),
                    c(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A1"]-blank1,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A4"]-blank1,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A5"]-blank1,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A6"]-blank1,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B1"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B3"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B4"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B5"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B6"]-blank1,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C1"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C2"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C4"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C5"]-blank2,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C6"]-blank1,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D1"]-blank3,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D2"]-blank3,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D3"]-blank3,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D4"]-blank3))
names(emb_p1b)<-c("Well","MO2")
emb_p2b<-data.frame(c("A2","A3","A4","A5","A6","B1","B2","B4","B5","B6","C1","C2","C3","C4","C5","D2","D3","D4","D6"),
                    c(slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A2"]-blank1,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A3"]-blank1,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A4"]-blank1,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A5"]-blank1,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A6"]-blank1,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B1"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B2"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B4"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B5"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B6"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C1"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C2"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C3"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C4"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C5"]-blank3,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D2"]-blank2,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D3"]-blank2,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D4"]-blank2,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D6"]-blank2))
names(emb_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataseet and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
emb_p1b<-dplyr::full_join(emb_p1b,emb_trmt_p1,by="Well")
emb_p1b<-emb_p1b[1:18,] #remove the blanks
emb_p2b<-dplyr::full_join(emb_p2b,emb_trmt_p2,by="Well")
emb_p2b<-emb_p2b[1:19,] #remove the blanks
emb_flax<-rbind(emb_p1b,emb_p2b) #combine the two plates into one dataset.
row.names(emb_flax)<-NULL #renumber the rows so it's easy to index if need be.

#analyze the MO2 with respect to CO2
flax_emb_model<-lm(emb_flax$MO2~emb_flax$CO2_level)
anova(flax_emb_model) #

#calculate the group means and do a post hoc Tukey test
TukeyHSD(aov(emb_flax$MO2~emb_flax$CO2_level)) #ambient is significantly different than both high CO2 treatments

#linear mixed effects model
library(lme4)
library(lmerTest)
flax_emb_mod<-lmer(MO2~CO2_level+(1|Tank),data=emb_flax) 
anova(flax_emb_mod) #
ranova(flax_emb_mod) #

#try it the other way
flax_emb_mdl<-aov(emb_flax$MO2~emb_flax$CO2_level/factor(emb_flax$Tank))
summary(flax_emb_mdl) #



library(plyr)
flax_emb_sum<-ddply(emb_flax,"CO2_level",summarise,N=length(MO2),MeanMO2=mean(MO2),SE=sd(MO2)/sqrt(N))
flax_emb_sum #elevated CO2 slightly increases MO2, similar to previous results. 

flax_emb_sum_tanks<-ddply(emb_flax,"Tank",summarise,N=length(MO2),MeanMO2=mean(MO2),SE=sd(MO2)/sqrt(N))
flax_emb_sum_tanks

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxembplot<-ggplot(flax_emb_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  #annotation_custom(grobTree(textGrob("Embryos, Exp. 2",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  labs(x=expression(paste("pCO"[2]," (uatm)")),y=expression(paste("MO"[2]," (umol individual"^-1," h"^-1,")")))+
  theme_classic()
print(flaxembplot)

##############################################################################################################
##############################################################################################################
#remove first 10 minutes and parts with extremely high temperature change
#In emb_p1 the first 10 minutes have already been removed, but the blips/spikes haven't been removed

#first check temperature in 10 min bins
library(tibble)
library(zoo)
emb_p1temp<-data.frame(emb_p1$T_internal,emb_p1$Time.Min.)
names(emb_p1temp)<-c("T_internal","Time.Min.")
Coef<-function(Z) coef(lm(T_internal~Time.Min., as.data.frame(Z)))[2]
emb_p1slope<-rollapplyr(zoo(emb_p1temp), width=30, Coef,by=30,by.column=FALSE, align="center")
emb_p1slope1<-as.data.frame(emb_p1slope)
emb_p1slope1<-rownames_to_column(emb_p1slope1, "Index")

emb_p2temp<-data.frame(emb_p2$T_internal,emb_p2$Time.Min.)
names(emb_p2temp)<-c("T_internal","Time.Min.")
emb_p2slope<-rollapplyr(zoo(emb_p2temp), width=30, Coef,by=30,by.column=FALSE, align="center")
emb_p2slope1<-as.data.frame(emb_p2slope)
emb_p2slope1<-rownames_to_column(emb_p2slope1, "Index")


#now identify the sections that have greater than 0.5C/h slope (0.0083C/min)
#convert to minutes by dividing by 3 and adding 10 (for the first 10 min being cut out)
emb_p1slope1$Index[abs(emb_p1slope1$emb_p1slope)>0.0083] #15, 2295:2535, 6645:6855 are too high so remove 10-20 min, 775-855, and 2225-2295
emb_p2slope1$Index[abs(emb_p2slope1$emb_p2slope)>0.0083] #15, 2325:2535, 6585:6855 are too high so remove 10-20 min, 785-855, and 2205-2295

emb_p1orig<-emb_p1
emb_p2orig<-emb_p2

#Trim the unusable parts from the original datasets for calc_mo2 analysis
emb_p1orig[c(1:30,2294:2533,6641:6849),c(4:27)]<-NA
emb_p2orig[c(1:30,2324:2533,6581:6849),c(4:27)]<-NA


emb_p1orig$A1[480:645]<-NA
emb_p1orig$A4[c(480:645,2533:2968)]<-NA
emb_p1orig$A5[c(570:795,2219:2968)]<-NA
emb_p1orig$A6[c(570:795,2219:2968)]<-NA
emb_p1orig$B1[1095:1395]<-NA
emb_p1orig$B4[c(1469:1769,2533:2968)]<-NA
emb_p1orig$B5[c(1020:1320,2533:2968)]<-NA
emb_p1orig$B6[c(945:1095,2219:2968)]<-NA
emb_p1orig$C1[1469:2533]<-NA
emb_p1orig$C4[3119:3268]<-NA
emb_p1orig$C5[2533:3119]<-NA
emb_p1orig$C6[c(720:1170,2819:2968)]<-NA
emb_p1orig$D1[1469:3268]<-NA
emb_p1orig$D2[1769:3119]<-NA
emb_p1orig$D3[2069:3119]<-NA
emb_p1orig$D4[2219:3268]<-NA

emb_p2orig$A2[1469:2669]<-NA
emb_p2orig$A3[1170:1619]<-NA
emb_p2orig$A4[1320:1769]<-NA
emb_p2orig$A5[2219:2819]<-NA
emb_p2orig$A6[1919:2893]<-NA
emb_p2orig$B1[2534:2969]<-NA
emb_p2orig$B2[2069:2969]<-NA
emb_p2orig$B4[2534:2669]<-NA
emb_p2orig$B5[2534:2893]<-NA
emb_p2orig$B6[2534:2969]<-NA
emb_p2orig$C1[1769:2969]<-NA
emb_p2orig$C2[2534:3118]<-NA
emb_p2orig$C3[2534:4318]<-NA
emb_p2orig$C4[2534:2669]<-NA
emb_p2orig$D2[2534:2969]<-NA
emb_p2orig$D3[c(1919:2744,3044:3193)]<-NA
emb_p2orig$D4[1919:2969]<-NA
emb_p2orig$D6[c(495:720,1320:1619,1769:1919,2534:3119)]<-NA




#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)

#Use make_bins() to set bin width but use different rows based on when it bottoms out. Use increments of 50. 
P1A1bin<-make_bins(o2=emb_p1orig$A1[1:5368],duration=emb_p1orig$Time.Min.[1:5368],max_o2_width=1/10,min_o2_width=1/30)
#P1A2bin<-make_bins(o2=emb_p1orig$A2[1:500],duration=emb_p1orig$Time.Min.[1:500],max_o2_width=1/10,min_o2_width=1/30)
#P1A3bin<-make_bins(o2=emb_p1orig$A3[1:500],duration=emb_p1orig$Time.Min.[1:500],max_o2_width=1/10,min_o2_width=1/30)
P1A4bin<-make_bins(o2=emb_p1orig$A4[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1A5bin<-make_bins(o2=emb_p1orig$A5[1:6266],duration=emb_p1orig$Time.Min.[1:6266],max_o2_width=1/10,min_o2_width=1/30)
P1A6bin<-make_bins(o2=emb_p1orig$A6[1:5368],duration=emb_p1orig$Time.Min.[1:5368],max_o2_width=1/10,min_o2_width=1/30)
P1B1bin<-make_bins(o2=emb_p1orig$B1[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
#P1B2bin<-make_bins(o2=emb_p1orig$B2[1:700],duration=emb_p1orig$Time.Min.[1:700],max_o2_width=1/10,min_o2_width=1/30)
P1B3bin<-make_bins(o2=emb_p1orig$B3[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1B4bin<-make_bins(o2=emb_p1orig$B4[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1B5bin<-make_bins(o2=emb_p1orig$B5[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1B6bin<-make_bins(o2=emb_p1orig$B6[1:6266],duration=emb_p1orig$Time.Min.[1:6266],max_o2_width=1/10,min_o2_width=1/30)
P1C1bin<-make_bins(o2=emb_p1orig$C1[1:6266],duration=emb_p1orig$Time.Min.[1:6266],max_o2_width=1/10,min_o2_width=1/30)
P1C2bin<-make_bins(o2=emb_p1orig$C2[1:6266],duration=emb_p1orig$Time.Min.[1:6266],max_o2_width=1/10,min_o2_width=1/30)
#P1C3bin<-make_bins(o2=emb_p1orig$C3[1:550],duration=emb_p1orig$Time.Min.[1:550],max_o2_width=1/10,min_o2_width=1/30)
P1C4bin<-make_bins(o2=emb_p1orig$C4[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1C5bin<-make_bins(o2=emb_p1orig$C5[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1C6bin<-make_bins(o2=emb_p1orig$C6[1:5966],duration=emb_p1orig$Time.Min.[1:5966],max_o2_width=1/10,min_o2_width=1/30)
P1D1bin<-make_bins(o2=emb_p1orig$D1[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1D2bin<-make_bins(o2=emb_p1orig$D2[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1D3bin<-make_bins(o2=emb_p1orig$D3[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P1D4bin<-make_bins(o2=emb_p1orig$D4[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
#P1D5bin<-make_bins(o2=emb_p1orig$D5[1:6566],duration=emb_p1orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
#P1D6bin<-make_bins(o2=emb_p1orig$D6[1:600],duration=emb_p1orig$Time.Min.[1:600],max_o2_width=1/10,min_o2_width=1/30)

#P2A1bin<-make_bins(o2=emb_p2orig$A1[1:500],duration=emb_p2orig$Time.Min.[1:500],max_o2_width=1/10,min_o2_width=1/30)
P2A2bin<-make_bins(o2=emb_p2orig$A2[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2A3bin<-make_bins(o2=emb_p2orig$A3[1:6266],duration=emb_p2orig$Time.Min.[1:6266],max_o2_width=1/10,min_o2_width=1/30)
P2A4bin<-make_bins(o2=emb_p2orig$A4[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2A5bin<-make_bins(o2=emb_p2orig$A5[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2A6bin<-make_bins(o2=emb_p2orig$A6[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2B1bin<-make_bins(o2=emb_p2orig$B1[1:6266],duration=emb_p2orig$Time.Min.[1:6266],max_o2_width=1/10,min_o2_width=1/30)
P2B2bin<-make_bins(o2=emb_p2orig$B2[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
#P2B3bin<-make_bins(o2=emb_p2orig$B3[1:650],duration=emb_p2orig$Time.Min.[1:650],max_o2_width=1/10,min_o2_width=1/30)
P2B4bin<-make_bins(o2=emb_p2orig$B4[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2B5bin<-make_bins(o2=emb_p2orig$B5[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2B6bin<-make_bins(o2=emb_p2orig$B6[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2C1bin<-make_bins(o2=emb_p2orig$C1[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2C2bin<-make_bins(o2=emb_p2orig$C2[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2C3bin<-make_bins(o2=emb_p2orig$C3[1:6416],duration=emb_p2orig$Time.Min.[1:6416],max_o2_width=1/10,min_o2_width=1/30)
P2C4bin<-make_bins(o2=emb_p2orig$C4[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2C5bin<-make_bins(o2=emb_p2orig$C5[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
#P2C6bin<-make_bins(o2=emb_p2orig$C6[1:850],duration=emb_p2orig$Time.Min.[1:850],max_o2_width=1/10,min_o2_width=1/30)
#P2D1bin<-make_bins(o2=emb_p2orig$D1[1:650],duration=emb_p2orig$Time.Min.[1:650],max_o2_width=1/10,min_o2_width=1/30)
P2D2bin<-make_bins(o2=emb_p2orig$D2[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2D3bin<-make_bins(o2=emb_p2orig$D3[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2D4bin<-make_bins(o2=emb_p2orig$D4[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
#P2D5bin<-make_bins(o2=emb_p2orig$D5[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)
P2D6bin<-make_bins(o2=emb_p2orig$D6[1:6566],duration=emb_p2orig$Time.Min.[1:6566],max_o2_width=1/10,min_o2_width=1/30)

#Now run the calc_MO2 code:
P1A1<-data.frame(calc_MO2(duration=emb_p1orig[1:5368,]$Time.Min.,o2=emb_p1orig[1:5368,]$A1,o2_unit="mg_per_l",bin_width=P1A1bin,vol=0.0006,temp=emb_p1orig[1:5368,]$T_internal,sal=27.3),rep("A1"),rep("1A"),rep("amb"))
#P1A2<-data.frame(calc_MO2(duration=emb_p1orig[1:1166,]$Time.Min.,o2=emb_p1orig[1:1166,]$A2,o2_unit="mg_per_l",bin_width=P1A2bin,vol=0.0006,temp=emb_p1orig[1:1166,]$T_internal,sal=27.3),rep("A2"),rep("1A"),rep("blank1"))
#P1A3<-data.frame(calc_MO2(duration=emb_p1orig[1:500,]$Time.Min.,o2=emb_p1orig[1:500,]$A3,o2_unit="mg_per_l",bin_width=P1A3bin,vol=0.0006,temp=emb_p1orig[1:500,]$T_internal,sal=27.3),rep("A3"),rep("1A"),rep("amb"))
P1A4<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$A4,o2_unit="mg_per_l",bin_width=P1A4bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("A4"),rep("1A"),rep("amb"))
P1A5<-data.frame(calc_MO2(duration=emb_p1orig[1:6266,]$Time.Min.,o2=emb_p1orig[1:6266,]$A5,o2_unit="mg_per_l",bin_width=P1A5bin,vol=0.0006,temp=emb_p1orig[1:6266,]$T_internal,sal=27.3),rep("A5"),rep("1A"),rep("amb"))
P1A6<-data.frame(calc_MO2(duration=emb_p1orig[1:5368,]$Time.Min.,o2=emb_p1orig[1:5368,]$A6,o2_unit="mg_per_l",bin_width=P1A6bin,vol=0.0006,temp=emb_p1orig[1:5368,]$T_internal,sal=27.3),rep("A6"),rep("1A"),rep("amb"))
P1B1<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$B1,o2_unit="mg_per_l",bin_width=P1B1bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("B1"),rep("2C"),rep("med"))
#P1B2<-data.frame(calc_MO2(duration=emb_p1orig[1:700,]$Time.Min.,o2=emb_p1orig[1:700,]$B2,o2_unit="mg_per_l",bin_width=P1B2bin,vol=0.0006,temp=emb_p1orig[1:700,]$T_internal,sal=27.3),rep("B2"),rep("2C"),rep("high"))
P1B3<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$B3,o2_unit="mg_per_l",bin_width=P1B3bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("B3"),rep("2C"),rep("med"))
P1B4<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$B4,o2_unit="mg_per_l",bin_width=P1B4bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("B4"),rep("2C"),rep("med"))
P1B5<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$B5,o2_unit="mg_per_l",bin_width=P1B5bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("B5"),rep("2C"),rep("med"))
P1B6<-data.frame(calc_MO2(duration=emb_p1orig[1:6266,]$Time.Min.,o2=emb_p1orig[1:6266,]$B6,o2_unit="mg_per_l",bin_width=P1B6bin,vol=0.0006,temp=emb_p1orig[1:6266,]$T_internal,sal=27.3),rep("B6"),rep("1B"),rep("amb"))
P1C1<-data.frame(calc_MO2(duration=emb_p1orig[1:6266,]$Time.Min.,o2=emb_p1orig[1:6266,]$C1,o2_unit="mg_per_l",bin_width=P1C1bin,vol=0.0006,temp=emb_p1orig[1:6266,]$T_internal,sal=27.3),rep("C1"),rep("2B"),rep("med"))
P1C2<-data.frame(calc_MO2(duration=emb_p1orig[1:6266,]$Time.Min.,o2=emb_p1orig[1:6266,]$C2,o2_unit="mg_per_l",bin_width=P1C2bin,vol=0.0006,temp=emb_p1orig[1:6266,]$T_internal,sal=27.3),rep("C2"),rep("2B"),rep("med"))
#P1C3<-data.frame(calc_MO2(duration=emb_p1orig[1:550,]$Time.Min.,o2=emb_p1orig[1:550,]$C3,o2_unit="mg_per_l",bin_width=P1C3bin,vol=0.0006,temp=emb_p1orig[1:550,]$T_internal,sal=27.3),rep("C3"),rep("3C"),rep("blank3"))
P1C4<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$C4,o2_unit="mg_per_l",bin_width=P1C4bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("C4"),rep("2B"),rep("med"))
P1C5<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$C5,o2_unit="mg_per_l",bin_width=P1C5bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("C5"),rep("2B"),rep("med"))
P1C6<-data.frame(calc_MO2(duration=emb_p1orig[1:5966,]$Time.Min.,o2=emb_p1orig[1:5966,]$C6,o2_unit="mg_per_l",bin_width=P1C6bin,vol=0.0006,temp=emb_p1orig[1:5966,]$T_internal,sal=27.3),rep("C6"),rep("1B"),rep("amb"))
P1D1<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$D1,o2_unit="mg_per_l",bin_width=P1D1bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("D1"),rep("3A"),rep("high"))
P1D2<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$D2,o2_unit="mg_per_l",bin_width=P1D2bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("D2"),rep("3A"),rep("high"))
P1D3<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$D3,o2_unit="mg_per_l",bin_width=P1D3bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("D3"),rep("3A"),rep("high"))
P1D4<-data.frame(calc_MO2(duration=emb_p1orig[1:6566,]$Time.Min.,o2=emb_p1orig[1:6566,]$D4,o2_unit="mg_per_l",bin_width=P1D4bin,vol=0.0006,temp=emb_p1orig[1:6566,]$T_internal,sal=27.3),rep("D4"),rep("3A"),rep("high"))
#P1D5<-data.frame(calc_MO2(duration=emb_p1orig[1:850,]$Time.Min.,o2=emb_p1orig[1:850,]$D5,o2_unit="mg_per_l",bin_width=P1D5bin,vol=0.0006,temp=emb_p1orig[1:850,]$T_internal,sal=27.3),rep("D5"),rep("1C"),rep("amb"))
#P1D6<-data.frame(calc_MO2(duration=emb_p1orig[1:1166,]$Time.Min.,o2=emb_p1orig[1:1166,]$D6,o2_unit="mg_per_l",bin_width=P1D6bin,vol=0.0006,temp=emb_p1orig[1:1166,]$T_internal,sal=27.3),rep("D6"),rep("1C"),rep("blank1"))

#P2A1<-data.frame(calc_MO2(duration=emb_p2orig[1:1166,]$Time.Min.,o2=emb_p2orig[1:1166,]$A1,o2_unit="mg_per_l",bin_width=P2A1bin,vol=0.0005,temp=emb_p2orig[1:1166,]$T_internal,sal=27.3),rep("A1"),rep("2A"),rep("blank2"))
P2A2<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$A2,o2_unit="mg_per_l",bin_width=P2A2bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("A2"),rep("1C"),rep("amb"))
P2A3<-data.frame(calc_MO2(duration=emb_p2orig[1:6266,]$Time.Min.,o2=emb_p2orig[1:6266,]$A3,o2_unit="mg_per_l",bin_width=P2A3bin,vol=0.0005,temp=emb_p2orig[1:6266,]$T_internal,sal=27.3),rep("A3"),rep("1C"),rep("amb"))
P2A4<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$A4,o2_unit="mg_per_l",bin_width=P2A4bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("A4"),rep("1C"),rep("amb"))
P2A5<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$A5,o2_unit="mg_per_l",bin_width=P2A5bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("A5"),rep("1C"),rep("amb"))
P2A6<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$A6,o2_unit="mg_per_l",bin_width=P2A6bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("A6"),rep("1C"),rep("amb"))
P2B1<-data.frame(calc_MO2(duration=emb_p2orig[1:6266,]$Time.Min.,o2=emb_p2orig[1:6266,]$B1,o2_unit="mg_per_l",bin_width=P2B1bin,vol=0.0005,temp=emb_p2orig[1:6266,]$T_internal,sal=27.3),rep("B1"),rep("3B"),rep("high"))
P2B2<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$B2,o2_unit="mg_per_l",bin_width=P2B2bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("B2"),rep("3B"),rep("high"))
#P2B3<-data.frame(calc_MO2(duration=emb_p2orig[1:650,]$Time.Min.,o2=emb_p2orig[1:650,]$B3,o2_unit="mg_per_l",bin_width=P2B3bin,vol=0.0005,temp=emb_p2orig[1:650,]$T_internal,sal=27.3),rep("B3"),rep("2C"),rep("med"))
P2B4<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$B4,o2_unit="mg_per_l",bin_width=P2B4bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("B4"),rep("3B"),rep("high"))
P2B5<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$B5,o2_unit="mg_per_l",bin_width=P2B5bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("B5"),rep("3B"),rep("high"))
P2B6<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$B6,o2_unit="mg_per_l",bin_width=P2B6bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("B6"),rep("3B"),rep("high"))
P2C1<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$C1,o2_unit="mg_per_l",bin_width=P2C1bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("C1"),rep("3C"),rep("high"))
P2C2<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$C2,o2_unit="mg_per_l",bin_width=P2C2bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("C2"),rep("3C"),rep("high"))
P2C3<-data.frame(calc_MO2(duration=emb_p2orig[1:6416,]$Time.Min.,o2=emb_p2orig[1:6416,]$C3,o2_unit="mg_per_l",bin_width=P2C3bin,vol=0.0005,temp=emb_p2orig[1:6416,]$T_internal,sal=27.3),rep("C3"),rep("3C"),rep("high"))
P2C4<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$C4,o2_unit="mg_per_l",bin_width=P2C4bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("C4"),rep("3C"),rep("high"))
P2C5<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$C5,o2_unit="mg_per_l",bin_width=P2C5bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("C5"),rep("3C"),rep("high"))
#P2C6<-data.frame(calc_MO2(duration=emb_p2orig[1:850,]$Time.Min.,o2=emb_p2orig[1:850,]$C6,o2_unit="mg_per_l",bin_width=P2C6bin,vol=0.0005,temp=emb_p2orig[1:850,]$T_internal,sal=27.3),rep("C6"),rep("3B"),rep("high"))
#P2D1<-data.frame(calc_MO2(duration=emb_p2orig[1:650,]$Time.Min.,o2=emb_p2orig[1:650,]$D1,o2_unit="mg_per_l",bin_width=P2D1bin,vol=0.0005,temp=emb_p2orig[1:650,]$T_internal,sal=27.3),rep("D1"),rep("2B"),rep("med"))
P2D2<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$D2,o2_unit="mg_per_l",bin_width=P2D2bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("D2"),rep("2A"),rep("med"))
P2D3<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$D3,o2_unit="mg_per_l",bin_width=P2D3bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("D3"),rep("2A"),rep("med"))
P2D4<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$D4,o2_unit="mg_per_l",bin_width=P2D4bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("D4"),rep("2A"),rep("med"))
#P2D5<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$D5,o2_unit="mg_per_l",bin_width=P2D5bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("D5"),rep("2B"),rep("med"))
P2D6<-data.frame(calc_MO2(duration=emb_p2orig[1:6566,]$Time.Min.,o2=emb_p2orig[1:6566,]$D6,o2_unit="mg_per_l",bin_width=P2D6bin,vol=0.0005,temp=emb_p2orig[1:6566,]$T_internal,sal=27.3),rep("D6"),rep("2A"),rep("med"))

names(P1A1)[9:11]<-c("Well","Tank","CO2")
names(P1A2)[9:11]<-c("Well","Tank","CO2")
names(P1A3)[9:11]<-c("Well","Tank","CO2")
names(P1A4)[9:11]<-c("Well","Tank","CO2")
names(P1A5)[9:11]<-c("Well","Tank","CO2")
names(P1A6)[9:11]<-c("Well","Tank","CO2")
names(P1B1)[9:11]<-c("Well","Tank","CO2")
names(P1B2)[9:11]<-c("Well","Tank","CO2")
names(P1B3)[9:11]<-c("Well","Tank","CO2")
names(P1B4)[9:11]<-c("Well","Tank","CO2")
names(P1B5)[9:11]<-c("Well","Tank","CO2")
names(P1B6)[9:11]<-c("Well","Tank","CO2")
names(P1C1)[9:11]<-c("Well","Tank","CO2")
names(P1C2)[9:11]<-c("Well","Tank","CO2")
names(P1C3)[9:11]<-c("Well","Tank","CO2")
names(P1C4)[9:11]<-c("Well","Tank","CO2")
names(P1C5)[9:11]<-c("Well","Tank","CO2")
names(P1C6)[9:11]<-c("Well","Tank","CO2")
names(P1D1)[9:11]<-c("Well","Tank","CO2")
names(P1D2)[9:11]<-c("Well","Tank","CO2")
names(P1D3)[9:11]<-c("Well","Tank","CO2")
names(P1D4)[9:11]<-c("Well","Tank","CO2")
names(P1D5)[9:11]<-c("Well","Tank","CO2")
names(P1D6)[9:11]<-c("Well","Tank","CO2")

names(P2A1)[9:11]<-c("Well","Tank","CO2")
names(P2A2)[9:11]<-c("Well","Tank","CO2")
names(P2A3)[9:11]<-c("Well","Tank","CO2")
names(P2A4)[9:11]<-c("Well","Tank","CO2")
names(P2A5)[9:11]<-c("Well","Tank","CO2")
names(P2A6)[9:11]<-c("Well","Tank","CO2")
names(P2B1)[9:11]<-c("Well","Tank","CO2")
names(P2B2)[9:11]<-c("Well","Tank","CO2")
names(P2B3)[9:11]<-c("Well","Tank","CO2")
names(P2B4)[9:11]<-c("Well","Tank","CO2")
names(P2B5)[9:11]<-c("Well","Tank","CO2")
names(P2B6)[9:11]<-c("Well","Tank","CO2")
names(P2C1)[9:11]<-c("Well","Tank","CO2")
names(P2C2)[9:11]<-c("Well","Tank","CO2")
names(P2C3)[9:11]<-c("Well","Tank","CO2")
names(P2C4)[9:11]<-c("Well","Tank","CO2")
names(P2C5)[9:11]<-c("Well","Tank","CO2")
names(P2C6)[9:11]<-c("Well","Tank","CO2")
names(P2D1)[9:11]<-c("Well","Tank","CO2")
names(P2D2)[9:11]<-c("Well","Tank","CO2")
names(P2D3)[9:11]<-c("Well","Tank","CO2")
names(P2D4)[9:11]<-c("Well","Tank","CO2")
names(P2D5)[9:11]<-c("Well","Tank","CO2")
names(P2D6)[9:11]<-c("Well","Tank","CO2")

#Subtract corresponding blank from each well
P1A1$MO2b<-P1A1$MO2-blank1
P1A2$MO2b<-P1A2$MO2-blank1
P1A3$MO2b<-P1A3$MO2-blank1
P1A4$MO2b<-P1A4$MO2-blank1
P1A5$MO2b<-P1A5$MO2-blank1
P1A6$MO2b<-P1A6$MO2-blank1
P1B1$MO2b<-P1B1$MO2-blank2
P1B2$MO2b<-P1B2$MO2-blank2
P1B3$MO2b<-P1B3$MO2-blank2
P1B4$MO2b<-P1B4$MO2-blank2
P1B5$MO2b<-P1B5$MO2-blank2
P1B6$MO2b<-P1B6$MO2-blank1
P1C1$MO2b<-P1C1$MO2-blank2
P1C2$MO2b<-P1C2$MO2-blank2
P1C3$MO2b<-P1C3$MO2-blank2
P1C4$MO2b<-P1C4$MO2-blank2
P1C5$MO2b<-P1C5$MO2-blank2
P1C6$MO2b<-P1C6$MO2-blank1
P1D1$MO2b<-P1D1$MO2-blank3
P1D2$MO2b<-P1D2$MO2-blank3
P1D3$MO2b<-P1D3$MO2-blank3
P1D4$MO2b<-P1D4$MO2-blank3
P1D5$MO2b<-P1D5$MO2-blank3
P1D6$MO2b<-P1D6$MO2-blank1
P2A1$MO2b<-P2A1$MO2-blank1
P2A2$MO2b<-P2A2$MO2-blank1
P2A3$MO2b<-P2A3$MO2-blank1
P2A4$MO2b<-P2A4$MO2-blank1
P2A5$MO2b<-P2A5$MO2-blank1
P2A6$MO2b<-P2A6$MO2-blank1
P2B1$MO2b<-P2B1$MO2-blank3
P2B2$MO2b<-P2B2$MO2-blank3
P2B3$MO2b<-P2B3$MO2-blank3
P2B4$MO2b<-P2B4$MO2-blank3
P2B5$MO2b<-P2B5$MO2-blank3
P2B6$MO2b<-P2B6$MO2-blank3
P2C1$MO2b<-P2C1$MO2-blank3
P2C2$MO2b<-P2C2$MO2-blank3
P2C3$MO2b<-P2C3$MO2-blank3
P2C4$MO2b<-P2C4$MO2-blank3
P2C5$MO2b<-P2C5$MO2-blank3
P2C6$MO2b<-P2C6$MO2-blank3
P2D1$MO2b<-P2D1$MO2-blank2
P2D2$MO2b<-P2D2$MO2-blank2
P2D3$MO2b<-P2D3$MO2-blank2
P2D4$MO2b<-P2D4$MO2-blank2
P2D5$MO2b<-P2D5$MO2-blank2
P2D6$MO2b<-P2D6$MO2-blank2

#There are no mass measurements to get msmr

#Remove the slopes where the number of data points was less than 4. 
P1A1<-P1A1[c(P1A1$N>4),]
P1A2<-P1A2[c(P1A2$N>4),]
P1A3<-P1A3[c(P1A3$N>4),]
P1A4<-P1A4[c(P1A4$N>4),]
P1A5<-P1A5[c(P1A5$N>4),]
P1A6<-P1A6[c(P1A6$N>4),]
P1B1<-P1B1[c(P1B1$N>4),]
P1B2<-P1B2[c(P1B2$N>4),]
P1B3<-P1B3[c(P1B3$N>4),]
P1B4<-P1B4[c(P1B4$N>4),]
P1B5<-P1B5[c(P1B5$N>4),]
P1B6<-P1B6[c(P1B6$N>4),]
P1C1<-P1C1[c(P1C1$N>4),]
P1C2<-P1C2[c(P1C2$N>4),]
P1C3<-P1C3[c(P1C3$N>4),]
P1C4<-P1C4[c(P1C4$N>4),]
P1C5<-P1C5[c(P1C5$N>4),]
P1C6<-P1C6[c(P1C6$N>4),]
P1D1<-P1D1[c(P1D1$N>4),]
P1D2<-P1D2[c(P1D2$N>4),]
P1D3<-P1D3[c(P1D3$N>4),]
P1D4<-P1D4[c(P1D4$N>4),]
P1D5<-P1D5[c(P1D5$N>4),]
P1D6<-P1D6[c(P1D6$N>4),]

P2A1<-P2A1[c(P2A1$N>4),]
P2A2<-P2A2[c(P2A2$N>4),]
P2A3<-P2A3[c(P2A3$N>4),]
P2A4<-P2A4[c(P2A4$N>4),]
P2A5<-P2A5[c(P2A5$N>4),]
P2A6<-P2A6[c(P2A6$N>4),]
P2B1<-P2B1[c(P2B1$N>4),]
P2B2<-P2B2[c(P2B2$N>4),]
P2B3<-P2B3[c(P2B3$N>4),]
P2B4<-P2B4[c(P2B4$N>4),]
P2B5<-P2B5[c(P2B5$N>4),]
P2B6<-P2B6[c(P2B6$N>4),]
P2C1<-P2C1[c(P2C1$N>4),]
P2C2<-P2C2[c(P2C2$N>4),]
P2C3<-P2C3[c(P2C3$N>4),]
P2C4<-P2C4[c(P2C4$N>4),]
P2C5<-P2C5[c(P2C5$N>4),]
P2C6<-P2C6[c(P2C6$N>4),]
P2D1<-P2D1[c(P2D1$N>4),]
P2D2<-P2D2[c(P2D2$N>4),]
P2D3<-P2D3[c(P2D3$N>4),]
P2D4<-P2D4[c(P2D4$N>4),]
P2D5<-P2D5[c(P2D5$N>4),]
P2D6<-P2D6[c(P2D6$N>4),]

#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES??
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
allemb<-rbind(P1A1,P1A4,P1A5,P1A6,P1B1,P1B3,P1B4,P1B5,P1B6,P1C1,P1C2,P1C4,P1C5,P1C6,P1D1,P1D2,P1D3,P1D4,
              P2A2,P2A3,P2A4,P2A5,P2A6,P2B1,P2B2,P2B4,P2B5,P2B6,P2C1,P2C2,P2C3,P2C4,P2C5,P2D2,P2D3,P2D4,P2D6)

#plot the curves
library(ggplot2)
allplot<-ggplot(allemb, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4"))
print(allplot)


hist(P1A1$MO2b,breaks=10) #two around -0.004 - fine
hist(P1A2$MO2b,breaks=10)
hist(P1A3$MO2b,breaks=10) 
hist(P1A4$MO2b,breaks=10) #one around -0.006 and five below -0.002
hist(P1A5$MO2b,breaks=10) #four below -0.002
hist(P1A6$MO2b,breaks=10) #six below -0.002
hist(P1B1$MO2b,breaks=10)
hist(P1B2$MO2b,breaks=10)
hist(P1B3$MO2b,breaks=10) #a couple below -0.002
hist(P1B4$MO2b,breaks=10) #one below -0.002
hist(P1B5$MO2b,breaks=10)
hist(P1B6$MO2b,breaks=10)
hist(P1C1$MO2b,breaks=10) #one below -0.002
hist(P1C2$MO2b,breaks=10) #two below -0.002
hist(P1C3$MO2b,breaks=10) 
hist(P1C4$MO2b,breaks=10)
hist(P1C5$MO2b,breaks=10)
hist(P1C6$MO2b,breaks=10) #nine below -0.002
hist(P1D1$MO2b,breaks=10) #one below -0.004
hist(P1D2$MO2b,breaks=10) #one below -0.002
hist(P1D3$MO2b,breaks=10) #one below -0.002
hist(P1D4$MO2b,breaks=10) #one below -0.004
hist(P1D5$MO2b,breaks=10)
hist(P1D6$MO2b,breaks=10) 
hist(P2A1$MO2b,breaks=10)
hist(P2A2$MO2b,breaks=10)
hist(P2A3$MO2b,breaks=10) #seven below -0.002
hist(P2A4$MO2b,breaks=10) #many that are slightly negative
hist(P2A5$MO2b,breaks=10)
hist(P2A6$MO2b,breaks=10)
hist(P2B1$MO2b,breaks=10)
hist(P2B2$MO2b,breaks=10)
hist(P2B3$MO2b,breaks=10)
hist(P2B4$MO2b,breaks=10) #two below -0.002
hist(P2B5$MO2b,breaks=10)
hist(P2B6$MO2b,breaks=10)
hist(P2C1$MO2b,breaks=10)
hist(P2C2$MO2b,breaks=10)
hist(P2C3$MO2b,breaks=10)
hist(P2C4$MO2b,breaks=10)
hist(P2C5$MO2b,breaks=10) #some are a little high check it out
hist(P2C6$MO2b,breaks=10)
hist(P2D1$MO2b,breaks=10)
hist(P2D2$MO2b,breaks=10)
hist(P2D3$MO2b,breaks=10)
hist(P2D4$MO2b,breaks=10)
hist(P2D5$MO2b,breaks=10)
hist(P2D6$MO2b,breaks=10)

#remove extreme points, and rename the row numbers before and after
row.names(P1A4)<-NULL
row.names(P1A5)<-NULL
row.names(P1A6)<-NULL
row.names(P1B3)<-NULL
row.names(P1B4)<-NULL
row.names(P1C1)<-NULL
row.names(P1C2)<-NULL
row.names(P1C6)<-NULL
row.names(P1D1)<-NULL
row.names(P1D2)<-NULL
row.names(P1D3)<-NULL
row.names(P1D4)<-NULL
row.names(P2A3)<-NULL
row.names(P2B4)<-NULL
row.names(P2C5)<-NULL

P1A4<-P1A4[-c(23,31),]
P1A5<-P1A5[-c(14),]
P1A6<-P1A6[-c(4:6),]
P1B3<-P1B3[-c(21:23),]
P1B4<-P1B4[-c(10,14:16),]
P1C1<-P1C1[-c(16),]
P1C2<-P1C2[-c(22,23),]
P1C6<-P1C6[-c(7,26:29),]
P1D1<-P1D1[-c(20),]
P1D2<-P1D2[-c(3),]
P1D3<-P1D3[-c(14),]
P1D4<-P1D4[-c(26),]
P2A3<-P2A3[-c(16),]
P2B4<-P2B4[-c(24,25),]
P2C5<-P2C5[-c(40),]

row.names(P1A4)<-NULL
row.names(P1A5)<-NULL
row.names(P1A6)<-NULL
row.names(P1B3)<-NULL
row.names(P1B4)<-NULL
row.names(P1C1)<-NULL
row.names(P1C2)<-NULL
row.names(P1C6)<-NULL
row.names(P1D1)<-NULL
row.names(P1D2)<-NULL
row.names(P1D3)<-NULL
row.names(P1D4)<-NULL
row.names(P2A3)<-NULL
row.names(P2B4)<-NULL
row.names(P2C5)<-NULL

emb_flax$Pcrit_alpha<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$MO2b)['Alpha'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$MO2b)['Alpha'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['Alpha'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['Alpha'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$MO2b)['Alpha'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['Alpha'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['Alpha'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$MO2b)['Alpha'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$MO2b)['Alpha'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['Alpha'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['Alpha'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['Alpha'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$MO2b)['Alpha'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['Alpha'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['Alpha'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['Alpha'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['Alpha'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['Alpha'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$MO2b)['Alpha'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['Alpha'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['Alpha'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$MO2b)['Alpha'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['Alpha'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['Alpha'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['Alpha'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$MO2b)['Alpha'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$MO2b)['Alpha'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['Alpha'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['Alpha'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['Alpha'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['Alpha'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$MO2b)['Alpha'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$MO2b)['Alpha'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$MO2b)['Alpha'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['Alpha'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['Alpha'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['Alpha'])
emb_flax$Pcrit_break<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$MO2b)['Breakpoint'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$MO2b)['Breakpoint'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['Breakpoint'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['Breakpoint'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$MO2b)['Breakpoint'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['Breakpoint'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['Breakpoint'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$MO2b)['Breakpoint'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$MO2b)['Breakpoint'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['Breakpoint'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['Breakpoint'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['Breakpoint'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$MO2b)['Breakpoint'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['Breakpoint'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['Breakpoint'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['Breakpoint'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['Breakpoint'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['Breakpoint'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$MO2b)['Breakpoint'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['Breakpoint'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['Breakpoint'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$MO2b)['Breakpoint'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['Breakpoint'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['Breakpoint'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['Breakpoint'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$MO2b)['Breakpoint'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$MO2b)['Breakpoint'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['Breakpoint'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['Breakpoint'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['Breakpoint'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['Breakpoint'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$MO2b)['Breakpoint'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$MO2b)['Breakpoint'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$MO2b)['Breakpoint'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['Breakpoint'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['Breakpoint'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['Breakpoint'])
emb_flax$Pcrit_subPI<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$MO2b)['Sub_PI'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$MO2b)['Sub_PI'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['Sub_PI'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['Sub_PI'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$MO2b)['Sub_PI'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['Sub_PI'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['Sub_PI'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$MO2b)['Sub_PI'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$MO2b)['Sub_PI'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['Sub_PI'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['Sub_PI'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['Sub_PI'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$MO2b)['Sub_PI'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['Sub_PI'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['Sub_PI'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['Sub_PI'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['Sub_PI'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['Sub_PI'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$MO2b)['Sub_PI'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['Sub_PI'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['Sub_PI'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$MO2b)['Sub_PI'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['Sub_PI'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['Sub_PI'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['Sub_PI'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$MO2b)['Sub_PI'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$MO2b)['Sub_PI'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['Sub_PI'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['Sub_PI'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['Sub_PI'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['Sub_PI'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$MO2b)['Sub_PI'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$MO2b)['Sub_PI'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$MO2b)['Sub_PI'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['Sub_PI'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['Sub_PI'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['Sub_PI'])
emb_flax$Pcrit_NLR<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$MO2b)['NLR'],
                      calc_pcrit(P1A4$O2_MEAN,P1A4$MO2b)['NLR'],
                      calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['NLR'],
                      calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['NLR'],
                      calc_pcrit(P1B1$O2_MEAN,P1B1$MO2b)['NLR'],
                      calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['NLR'],
                      calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['NLR'],
                      calc_pcrit(P1B5$O2_MEAN,P1B5$MO2b)['NLR'],
                      calc_pcrit(P1B6$O2_MEAN,P1B6$MO2b)['NLR'],
                      calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['NLR'],
                      calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['NLR'],
                      calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['NLR'],
                      calc_pcrit(P1C5$O2_MEAN,P1C5$MO2b)['NLR'],
                      calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['NLR'],
                      calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['NLR'],
                      calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['NLR'],
                      calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['NLR'],
                      calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['NLR'],
                      calc_pcrit(P2A2$O2_MEAN,P2A2$MO2b)['NLR'],
                      calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['NLR'],
                      calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['NLR'],
                      calc_pcrit(P2A5$O2_MEAN,P2A5$MO2b)['NLR'],
                      calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['NLR'],
                      calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['NLR'],
                      calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['NLR'],
                      calc_pcrit(P2B4$O2_MEAN,P2B4$MO2b)['NLR'],
                      calc_pcrit(P2B5$O2_MEAN,P2B5$MO2b)['NLR'],
                      calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['NLR'],
                      calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['NLR'],
                      calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['NLR'],
                      calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['NLR'],
                      calc_pcrit(P2C4$O2_MEAN,P2C4$MO2b)['NLR'],
                      calc_pcrit(P2C5$O2_MEAN,P2C5$MO2b)['NLR'],
                      calc_pcrit(P2D2$O2_MEAN,P2D2$MO2b)['NLR'],
                      calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['NLR'],
                      calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['NLR'],
                      calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['NLR'])
emb_flax$alpha<-c(calc_alpha(P1A1$O2_MEAN,P1A1$MO2b)$alpha,
                  calc_alpha(P1A4$O2_MEAN,P1A4$MO2b)$alpha,
                  calc_alpha(P1A5$O2_MEAN,P1A5$MO2b)$alpha,
                  calc_alpha(P1A6$O2_MEAN,P1A6$MO2b)$alpha,
                  calc_alpha(P1B1$O2_MEAN,P1B1$MO2b)$alpha,
                  calc_alpha(P1B3$O2_MEAN,P1B3$MO2b)$alpha,
                  calc_alpha(P1B4$O2_MEAN,P1B4$MO2b)$alpha,
                  calc_alpha(P1B5$O2_MEAN,P1B5$MO2b)$alpha,
                  calc_alpha(P1B6$O2_MEAN,P1B6$MO2b)$alpha,
                  calc_alpha(P1C1$O2_MEAN,P1C1$MO2b)$alpha,
                  calc_alpha(P1C2$O2_MEAN,P1C2$MO2b)$alpha,
                  calc_alpha(P1C4$O2_MEAN,P1C4$MO2b)$alpha,
                  calc_alpha(P1C5$O2_MEAN,P1C5$MO2b)$alpha,
                  calc_alpha(P1C6$O2_MEAN,P1C6$MO2b)$alpha,
                  calc_alpha(P1D1$O2_MEAN,P1D1$MO2b)$alpha,
                  calc_alpha(P1D2$O2_MEAN,P1D2$MO2b)$alpha,
                  calc_alpha(P1D3$O2_MEAN,P1D3$MO2b)$alpha,
                  calc_alpha(P1D4$O2_MEAN,P1D4$MO2b)$alpha,
                  calc_alpha(P2A2$O2_MEAN,P2A2$MO2b)$alpha,
                  calc_alpha(P2A3$O2_MEAN,P2A3$MO2b)$alpha,
                  calc_alpha(P2A4$O2_MEAN,P2A4$MO2b)$alpha,
                  calc_alpha(P2A5$O2_MEAN,P2A5$MO2b)$alpha,
                  calc_alpha(P2A6$O2_MEAN,P2A6$MO2b)$alpha,
                  calc_alpha(P2B1$O2_MEAN,P2B1$MO2b)$alpha,
                  calc_alpha(P2B2$O2_MEAN,P2B2$MO2b)$alpha,
                  calc_alpha(P2B4$O2_MEAN,P2B4$MO2b)$alpha,
                  calc_alpha(P2B5$O2_MEAN,P2B5$MO2b)$alpha,
                  calc_alpha(P2B6$O2_MEAN,P2B6$MO2b)$alpha,
                  calc_alpha(P2C1$O2_MEAN,P2C1$MO2b)$alpha,
                  calc_alpha(P2C2$O2_MEAN,P2C2$MO2b)$alpha,
                  calc_alpha(P2C3$O2_MEAN,P2C3$MO2b)$alpha,
                  calc_alpha(P2C4$O2_MEAN,P2C4$MO2b)$alpha,
                  calc_alpha(P2C5$O2_MEAN,P2C5$MO2b)$alpha,
                  calc_alpha(P2D2$O2_MEAN,P2D2$MO2b)$alpha,
                  calc_alpha(P2D3$O2_MEAN,P2D3$MO2b)$alpha,
                  calc_alpha(P2D4$O2_MEAN,P2D4$MO2b)$alpha,
                  calc_alpha(P2D6$O2_MEAN,P2D6$MO2b)$alpha)


###############################################################################
#Using selgmented function instead of calc_pcrit (segmented function) to get Pcrit
library(segmented)
library(respirometry)


P1A1seg<-selgmented(lm(MO2b~O2_MEAN,data=P1A1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1A1)
plot(P1A1seg,add=T)
plot_pcrit(P1A1$O2_MEAN,P1A1$MO2b)
print(P1A1seg)

P1A2seg<-selgmented(lm(MO2b~O2_MEAN,data=P1A2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1A2)
plot(P1A2seg,add=T)
plot_pcrit(P1A2$O2_MEAN,P1A2$MO2b)
print(P1A2seg)

P1A3seg<-selgmented(lm(MO2b~O2_MEAN,data=P1A3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1A3)
plot(P1A3seg,add=T)
plot_pcrit(P1A3$O2_MEAN,P1A3$MO2b)
print(P1A3seg)

P1A4seg<-selgmented(lm(MO2b~O2_MEAN,data=P1A4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1A4)
plot(P1A4seg,add=T)
plot_pcrit(P1A4$O2_MEAN,P1A4$MO2b)
print(P1A4seg)

P1A5seg<-selgmented(lm(MO2b~O2_MEAN,data=P1A5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1A5)
plot(P1A5seg,add=T)
plot_pcrit(P1A5$O2_MEAN,P1A5$MO2b)
print(P1A5seg)

P1A6seg<-selgmented(lm(MO2b~O2_MEAN,data=P1A6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1A6)
plot(P1A6seg,add=T)
plot_pcrit(P1A6$O2_MEAN,P1A6$MO2b)
print(P1A6seg)

P1B1seg<-selgmented(lm(MO2b~O2_MEAN,data=P1B1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1B1)
plot(P1B1seg,add=T)
plot_pcrit(P1B1$O2_MEAN,P1B1$MO2b)
print(P1B1seg)

P1B2seg<-selgmented(lm(MO2b~O2_MEAN,data=P1B2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1B2)
plot(P1B2seg,add=T)
plot_pcrit(P1B2$O2_MEAN,P1B2$MO2b)
print(P1B2seg)

P1B3seg<-selgmented(lm(MO2b~O2_MEAN,data=P1B3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1B3)
plot(P1B3seg,add=T)
plot_pcrit(P1B3$O2_MEAN,P1B3$MO2b)
print(P1B3seg)

P1B4seg<-selgmented(lm(MO2b~O2_MEAN,data=P1B4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1B4)
plot(P1B4seg,add=T)
plot_pcrit(P1B4$O2_MEAN,P1B4$MO2b)
print(P1B4seg)

P1B5seg<-selgmented(lm(MO2b~O2_MEAN,data=P1B5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1B5)
plot(P1B5seg,add=T)
plot_pcrit(P1B5$O2_MEAN,P1B5$MO2b)
print(P1B5seg)

P1B6seg<-selgmented(lm(MO2b~O2_MEAN,data=P1B6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(MO2b~O2_MEAN,data=P1B6)
plot(P1B6seg,add=T)
plot_pcrit(P1B6$O2_MEAN,P1B6$MO2b)
print(P1B6seg)

P1C1seg<-selgmented(lm(MO2b~O2_MEAN,data=P1C1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1C1)
plot(P1C1seg,add=T)
plot_pcrit(P1C1$O2_MEAN,P1C1$MO2b)
print(P1C1seg)

P1C2seg<-selgmented(lm(MO2b~O2_MEAN,data=P1C2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1C2)
plot(P1C2seg,add=T)
plot_pcrit(P1C2$O2_MEAN,P1C2$MO2b)
print(P1C2seg)

P1C3seg<-selgmented(lm(MO2b~O2_MEAN,data=P1C3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1C3)
plot(P1C3seg,add=T)
plot_pcrit(P1C3$O2_MEAN,P1C3$MO2b)
print(P1C3seg)

P1C4seg<-selgmented(lm(MO2b~O2_MEAN,data=P1C4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1C4)
plot(P1C4seg,add=T)
plot_pcrit(P1C4$O2_MEAN,P1C4$MO2b)
print(P1C4seg)

P1C5seg<-selgmented(lm(MO2b~O2_MEAN,data=P1C5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1C5)
plot(P1C5seg,add=T)
plot_pcrit(P1C5$O2_MEAN,P1C5$MO2b)
print(P1C5seg)

P1C6seg<-selgmented(lm(MO2b~O2_MEAN,data=P1C6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(MO2b~O2_MEAN,data=P1C6)
plot(P1C6seg,add=T)
plot_pcrit(P1C6$O2_MEAN,P1C6$MO2b)
print(P1C6seg)

P1D1seg<-selgmented(lm(MO2b~O2_MEAN,data=P1D1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1D1)
plot(P1D1seg,add=T)
plot_pcrit(P1D1$O2_MEAN,P1D1$MO2b)
print(P1D1seg)

P1D2seg<-selgmented(lm(MO2b~O2_MEAN,data=P1D2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1D2)
plot(P1D2seg,add=T)
plot_pcrit(P1D2$O2_MEAN,P1D2$MO2b)
print(P1D2seg)

P1D3seg<-selgmented(lm(MO2b~O2_MEAN,data=P1D3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1D3)
plot(P1D3seg,add=T)
plot_pcrit(P1D3$O2_MEAN,P1D3$MO2b)
print(P1D3seg)

P1D4seg<-selgmented(lm(MO2b~O2_MEAN,data=P1D4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1D4)
plot(P1D4seg,add=T)
plot_pcrit(P1D4$O2_MEAN,P1D4$MO2b)
print(P1D4seg)

P1D5seg<-selgmented(lm(MO2b~O2_MEAN,data=P1D5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(MO2b~O2_MEAN,data=P1D5)
plot(P1D5seg,add=T)
plot_pcrit(P1D5$O2_MEAN,P1D5$MO2b)
print(P1D5seg)

P1D6seg<-selgmented(lm(MO2b~O2_MEAN,data=P1D6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P1D6)
plot(P1D6seg,add=T)
plot_pcrit(P1D6$O2_MEAN,P1D6$MO2b)
print(P1D6seg)

#Plate 2

P2A1seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2A1)
plot(P2A1seg,add=T)
plot_pcrit(P2A1$O2_MEAN,P2A1$MO2b)
print(P2A1seg)

P2A2seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2A2)
plot(P2A2seg,add=T)
plot_pcrit(P2A2$O2_MEAN,P2A2$MO2b)
print(P2A2seg)

P2A3seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2A3)
plot(P2A3seg,add=T)
plot_pcrit(P2A3$O2_MEAN,P2A3$MO2b)
print(P2A3seg)

P2A4seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2A4)
plot(P2A4seg,add=T)
plot_pcrit(P2A4$O2_MEAN,P2A4$MO2b)
print(P2A4seg)

P2A5seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2A5)
plot(P2A5seg,add=T)
plot_pcrit(P2A5$O2_MEAN,P2A5$MO2b)
print(P2A5seg)

P2A6seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2A6)
plot(P2A6seg,add=T)
plot_pcrit(P2A6$O2_MEAN,P2A6$MO2b)
print(P2A6seg)

P2B1seg<-selgmented(lm(MO2b~O2_MEAN,data=P2B1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2B1)
plot(P2B1seg,add=T)
plot_pcrit(P2B1$O2_MEAN,P2B1$MO2b)
print(P2B1seg)

P2B2seg<-selgmented(lm(MO2b~O2_MEAN,data=P2B2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2B2)
plot(P2B2seg,add=T)
plot_pcrit(P2B2$O2_MEAN,P2B2$MO2b)
print(P2B2seg)

P2B3seg<-selgmented(lm(MO2b~O2_MEAN,data=P2B3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(MO2b~O2_MEAN,data=P2B3)
plot(P2B3seg,add=T)
plot_pcrit(P2B3$O2_MEAN,P2B3$MO2b)
print(P2B3seg)

P2B4seg<-selgmented(lm(MO2b~O2_MEAN,data=P2B4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2B4)
plot(P2B4seg,add=T)
plot_pcrit(P2B4$O2_MEAN,P2B4$MO2b)
print(P2B4seg)

P2B5seg<-selgmented(lm(MO2b~O2_MEAN,data=P2B5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2B5)
plot(P2B5seg,add=T)
plot_pcrit(P2B5$O2_MEAN,P2B5$MO2b)
print(P2B5seg)

P2B6seg<-selgmented(lm(MO2b~O2_MEAN,data=P2B6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2B6)
plot(P2B6seg,add=T)
plot_pcrit(P2B6$O2_MEAN,P2B6$MO2b)
print(P2B6seg)

P2C1seg<-selgmented(lm(MO2b~O2_MEAN,data=P2C1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2C1)
plot(P2C1seg,add=T)
plot_pcrit(P2C1$O2_MEAN,P2C1$MO2b)
print(P2C1seg)

P2C2seg<-selgmented(lm(MO2b~O2_MEAN,data=P2C2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2C2)
plot(P2C2seg,add=T)
plot_pcrit(P2C2$O2_MEAN,P2C2$MO2b)
print(P2C2seg)

P2C3seg<-selgmented(lm(MO2b~O2_MEAN,data=P2C3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2C3)
plot(P2C3seg,add=T)
plot_pcrit(P2C3$O2_MEAN,P2C3$MO2b)
print(P2C3seg)

P2C4seg<-selgmented(lm(MO2b~O2_MEAN,data=P2C4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2C4)
plot(P2C4seg,add=T)
plot_pcrit(P2C4$O2_MEAN,P2C4$MO2b)
print(P2C4seg)

P2C5seg<-selgmented(lm(MO2b~O2_MEAN,data=P2C5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(MO2b~O2_MEAN,data=P2C5)
plot(P2C5seg,add=T)
plot_pcrit(P2C5$O2_MEAN,P2C5$MO2b)
print(P2C5seg)

P2C6seg<-selgmented(lm(MO2b~O2_MEAN,data=P2C6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2C6)
plot(P2C6seg,add=T)
plot_pcrit(P2C6$O2_MEAN,P2C6$MO2b)
print(P2C6seg)

P2D1seg<-selgmented(lm(MO2b~O2_MEAN,data=P2D1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2D1)
plot(P2D1seg,add=T)
plot_pcrit(P2D1$O2_MEAN,P2D1$MO2b)
print(P2D1seg)

P2D2seg<-selgmented(lm(MO2b~O2_MEAN,data=P2D2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2D2)
plot(P2D2seg,add=T)
plot_pcrit(P2D2$O2_MEAN,P2D2$MO2b)
print(P2D2seg)

P2D3seg<-selgmented(lm(MO2b~O2_MEAN,data=P2D3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2D3)
plot(P2D3seg,add=T)
plot_pcrit(P2D3$O2_MEAN,P2D3$MO2b)
print(P2D3seg)

P2D4seg<-selgmented(lm(MO2b~O2_MEAN,data=P2D4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2D4)
plot(P2D4seg,add=T)
plot_pcrit(P2D4$O2_MEAN,P2D4$MO2b)
print(P2D4seg)

P2D5seg<-selgmented(lm(MO2b~O2_MEAN,data=P2D5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(MO2b~O2_MEAN,data=P2D5)
plot(P2D5seg,add=T)
plot_pcrit(P2D5$O2_MEAN,P2D5$MO2b)
print(P2D5seg)

P2D6seg<-selgmented(lm(MO2b~O2_MEAN,data=P2D6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(MO2b~O2_MEAN,data=P2D6)
plot(P2D6seg,add=T)
plot_pcrit(P2D6$O2_MEAN,P2D6$MO2b)
print(P2D6seg)

#Finally, update the dataframe with the new Pcrit and spike data
emb_flax$Pcrit_break<-c(2.360,2.131,2.416,1.994,  #P1A
                        1.801,3.041,NA,3.114,0.6157,  #P1B
                        0.9070,0.6364,2.9823,2.6625,1.942,  #P1C
                        2.816,2.055,NA,2.652,  #P1D
                        2.871,2.646,1.2162,1.302,1.389,  #P2A
                        NA,NA,2.094,NA,NA,  #P2B
                        NA,NA,NA,NA,NA,  #P2C
                        0.8513,1.066,2.552,1.132)  #P2D
emb_flax$spike<-c(1,0,0,0,  #P1A
                  0,1,0,0,0,  #P1B
                  0,0,1,0,0,  #P1C
                  0,1,1,1,  #P1D
                  1,0,0,0,0,  #P2A
                  0,0,0,1,0,  #P2B
                  0,1,0,0,0,  #P2C
                  0,0,0,0)  #P2D

################################################################################################
#Analyze, check model assumptions, and plot
#set levels of CO2_level and Tank
str(emb_flax)
emb_flax$CO2_level<-factor(emb_flax$CO2_level,levels=c("amb","med","high"))
emb_flax$Tank<-factor(emb_flax$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))

plot(emb_flax$Pcrit_break~emb_flax$CO2_level)

#calculate mean and SE of Pcrit values by treatment
library(plyr)
break_sum<-ddply(emb_flax,"CO2_level",summarise,N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

break_mod<-lmer(Pcrit_break~CO2_level+(1|Tank),data=emb_flax) #
ranova(break_mod) #tank doesn't affect fit
anova(break_mod) #p=0.6174

break_mod1<-lm(Pcrit_break~CO2_level,data=emb_flax)
anova(break_mod1) #p=0.5403

#try it the other way
break_mod2<-aov(1/(emb_flax$Pcrit_break)~emb_flax$CO2_level/factor(emb_flax$Tank))
summary(break_mod2) #neither CO2 nor tank is significant


#diagnostics
par(mfrow=c(2,2))
plot(break_mod2) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(1/(emb_flax$Pcrit_break)) #p=0.07972
par(mfrow=c(1,1))
hist(emb_flax$Pcrit_break)

#homogeneity of variances
library(car)
leveneTest(1/(emb_flax$Pcrit_break), emb_flax$CO2_level) #p=0.0291 (the variance is lower in the high CO2 trmt)

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxembpcritplot<-ggplot(break_sum, aes(x=CO2_level,y=MeanPcrit))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanPcrit-SE,ymax=MeanPcrit+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("Embryos, Exp. 2",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,3))+
  theme_classic()
print(flaxembpcritplot)

#might try a transformation that would help the variances be more equal

#Calculate the percent from each treatment that are NA (oxygen-dependent the whole time)
pct_NA_amb<-100*sum(is.na(emb_flax$Pcrit_break[emb_flax$CO2_level=="amb"]))/length(emb_flax$Pcrit_break[emb_flax$CO2_level=="amb"])
pct_NA_med<-100*sum(is.na(emb_flax$Pcrit_break[emb_flax$CO2_level=="med"]))/length(emb_flax$Pcrit_break[emb_flax$CO2_level=="med"])
pct_NA_high<-100*sum(is.na(emb_flax$Pcrit_break[emb_flax$CO2_level=="high"]))/length(emb_flax$Pcrit_break[emb_flax$CO2_level=="high"])


###############################################################################################
#Calculate RMR as the average MO2 for which O2>Pcrit

emb_flax$RMR<-c(mean(c(P1A1$MO2b[P1A1$O2_MEAN>emb_flax[1,8]])),
                mean(c(P1A4$MO2b[P1A4$O2_MEAN>emb_flax[2,8]])),
                mean(c(P1A5$MO2b[P1A5$O2_MEAN>emb_flax[3,8]])),
                mean(c(P1A6$MO2b[P1A6$O2_MEAN>emb_flax[4,8]])),
                mean(c(P1B1$MO2b[P1B1$O2_MEAN>emb_flax[5,8]])),
                mean(c(P1B3$MO2b[P1B3$O2_MEAN>emb_flax[6,8]])),
                mean(c(P1B4$MO2b[P1B4$O2_MEAN>3.5])),
                mean(c(P1B5$MO2b[P1B5$O2_MEAN>emb_flax[8,8]])),
                mean(c(P1B6$MO2b[P1B6$O2_MEAN>emb_flax[9,8]])),
                mean(c(P1C1$MO2b[P1C1$O2_MEAN>emb_flax[10,8]])),
                mean(c(P1C2$MO2b[P1C2$O2_MEAN>emb_flax[11,8]])),
                mean(c(P1C4$MO2b[P1C4$O2_MEAN>emb_flax[12,8]])),
                mean(c(P1C5$MO2b[P1C5$O2_MEAN>emb_flax[13,8]])),
                mean(c(P1C6$MO2b[P1C6$O2_MEAN>emb_flax[14,8]])),
                mean(c(P1D1$MO2b[P1D1$O2_MEAN>emb_flax[15,8]])),
                mean(c(P1D2$MO2b[P1D2$O2_MEAN>emb_flax[16,8]])),
                mean(c(P1D3$MO2b[P1D3$O2_MEAN>3.5])),
                mean(c(P1D4$MO2b[P1D4$O2_MEAN>emb_flax[18,8]])),
                mean(c(P2A2$MO2b[P2A2$O2_MEAN>emb_flax[19,8]])),
                mean(c(P2A3$MO2b[P2A3$O2_MEAN>emb_flax[20,8]])),
                mean(c(P2A4$MO2b[P2A4$O2_MEAN>emb_flax[21,8]])),
                mean(c(P2A5$MO2b[P2A5$O2_MEAN>emb_flax[22,8]])),
                mean(c(P2A6$MO2b[P2A6$O2_MEAN>emb_flax[23,8]])),
                mean(c(P2B1$MO2b[P2B1$O2_MEAN>3.5])),
                mean(c(P2B2$MO2b[P2B2$O2_MEAN>3.5])),
                mean(c(P2B4$MO2b[P2B4$O2_MEAN>emb_flax[26,8]])),
                mean(c(P2B5$MO2b[P2B5$O2_MEAN>3.5])),
                mean(c(P2B6$MO2b[P2B6$O2_MEAN>3.5])),
                mean(c(P2C1$MO2b[P2C1$O2_MEAN>3.5])),
                mean(c(P2C2$MO2b[P2C2$O2_MEAN>3.5])),
                mean(c(P2C3$MO2b[P2C3$O2_MEAN>3.5])),
                mean(c(P2C4$MO2b[P2C4$O2_MEAN>3.5])),
                mean(c(P2C5$MO2b[P2C5$O2_MEAN>3.5])),
                mean(c(P2D2$MO2b[P2D2$O2_MEAN>emb_flax[34,8]])),
                mean(c(P2D3$MO2b[P2D3$O2_MEAN>emb_flax[35,8]])),
                mean(c(P2D4$MO2b[P2D4$O2_MEAN>emb_flax[36,8]])),
                mean(c(P2D6$MO2b[P2D6$O2_MEAN>emb_flax[37,8]])))

#analyze RMR
flax_emb_model<-lm(emb_flax$RMR~emb_flax$CO2_level)
anova(flax_emb_model) #CO2 level is not significant

flax_emb_mod<-lmer(RMR~CO2_level+(1|Tank),data=emb_flax) #
anova(flax_emb_mod) #CO2 is significant p=0.017
ranova(flax_emb_mod) #random effect of tank doesn't affect results. 

#try it the other way
flax_emb_mdl<-aov(sqrt(emb_flax$RMR)~emb_flax$CO2_level/factor(emb_flax$Tank))
summary(flax_emb_mdl) #CO2 is significant p=0.000364

TukeyHSD(flax_emb_mdl)

#diagnostics
par(mfrow=c(2,2))
plot(flax_emb_mdl) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(sqrt(emb_flax$RMR)) #sig not normal p=0.00066
par(mfrow=c(1,1))
hist(emb_flax$RMR) #it is right skewed by a 5 individuals that are >0.005

#homogeneity of variances
library(car)
leveneTest(sqrt(emb_flax$RMR), emb_flax$CO2_level) #p=0.1588 with sqrt transformation, reciprocal doesn't work

#calculate the group means 

library(plyr)
flax_emb_sum<-ddply(emb_flax,"CO2_level",summarise,N=length(na.omit(RMR)),MeanMO2=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
flax_emb_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxembplot<-ggplot(flax_emb_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("Embryos, Exp. 2",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.006))+
  theme_classic()
print(flaxembplot)

#Calculate the percentage in each treatment that have spike
pct_spike_amb<-100*sum(emb_flax$spike[emb_flax$CO2_level=="amb"])/length(emb_flax$spike[emb_flax$CO2_level=="amb"])
pct_spike_med<-100*sum(emb_flax$spike[emb_flax$CO2_level=="med"])/length(emb_flax$spike[emb_flax$CO2_level=="med"])
pct_spike_high<-100*sum(emb_flax$spike[emb_flax$CO2_level=="high"])/length(emb_flax$spike[emb_flax$CO2_level=="high"])
