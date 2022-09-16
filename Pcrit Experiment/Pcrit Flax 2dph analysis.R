#Respirometry analysis from 2021 experiment

#Run 4: Larvae 2dph, Flax Pond, 6-18-21

#load data sheets and treatments
lar_p1orig<-read.csv(file.choose(),header=TRUE)
lar_p2orig<-read.csv(file.choose(),header=TRUE)
lar_trmt_p1<-read.csv(file.choose(),header=TRUE)
lar_trmt_p2<-read.csv(file.choose(),header=TRUE)

#If necessary, remove beginning of dataset (e.g. wasn't sealed when measurements started) then renumber rows.
lar_p1<-lar_p1orig
lar_p2<-lar_p2orig
row.names(lar_p1)<-NULL
row.names(lar_p2)<-NULL

#check structure
str(lar_p1)
str(lar_p2)
str(lar_trmt_p1)
str(lar_trmt_p2)

#make the treatment variables factors
lar_trmt_p1$Well<-factor(lar_trmt_p1$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
lar_trmt_p2$Well<-factor(lar_trmt_p2$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
lar_trmt_p1$CO2_level<-factor(lar_trmt_p1$CO2_level,levels=c("amb","med","high","blankamb","blankmed","blankhigh"))
lar_trmt_p2$CO2_level<-factor(lar_trmt_p2$CO2_level,levels=c("amb","med","high","blankamb","blankmed","blankhigh"))
lar_trmt_p1$Tank<-factor(lar_trmt_p1$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))
lar_trmt_p2$Tank<-factor(lar_trmt_p2$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))


#calculate slope for each well using all untrimmed data. Units will be umol consumed per hour
slopes_r3p1<-data.frame(names(lar_p1)[4:27], sapply(lar_p1[4:27],function(x) ((-coef(summary(lm(x~lar_p1$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(lar_p1[4:27],function(x) summary(lm(x~lar_p1$Time.Sec.))$r.squared))
names(slopes_r3p1)<-c("Well","MO2","Rsquared")
slopes_r3p2<-data.frame(names(lar_p2)[4:27], sapply(lar_p2[4:27],function(x) ((-coef(summary(lm(x~lar_p2$Time.Sec.)))[2])/31.9988)*1800),
                        sapply(lar_p2[4:27],function(x) summary(lm(x~lar_p2$Time.Sec.))$r.squared))
names(slopes_r3p2)<-c("Well","MO2","Rsquared")

#plot data for all live wells
par(mfrow=c(2,5))
plot(lar_p1$A3~lar_p1$Time.Min.)
plot(lar_p1$A4~lar_p1$Time.Min.)
plot(lar_p1$A5~lar_p1$Time.Min.)
plot(lar_p1$A6~lar_p1$Time.Min.)
plot(lar_p1$B1~lar_p1$Time.Min.)
plot(lar_p1$B2~lar_p1$Time.Min.)
plot(lar_p1$B3~lar_p1$Time.Min.)
plot(lar_p1$B4~lar_p1$Time.Min.)
plot(lar_p1$B6~lar_p1$Time.Min.)
plot(lar_p1$C1~lar_p1$Time.Min.)
plot(lar_p1$C3~lar_p1$Time.Min.)
plot(lar_p1$C4~lar_p1$Time.Min.)
plot(lar_p1$C5~lar_p1$Time.Min.)
plot(lar_p1$C6~lar_p1$Time.Min.)
plot(lar_p1$D1~lar_p1$Time.Min.)
plot(lar_p1$D2~lar_p1$Time.Min.)
plot(lar_p1$D3~lar_p1$Time.Min.)
plot(lar_p1$D4~lar_p1$Time.Min.)
plot(lar_p1$D5~lar_p1$Time.Min.)
abline(v=c(10,100,200,300,400,310))
#blanks
par(mfrow=c(2,2))
plot(lar_p1$A2~lar_p1$Time.Min.)
plot(lar_p1$B5~lar_p1$Time.Min.)
plot(lar_p1$C2~lar_p1$Time.Min.)
plot(lar_p1$D6~lar_p1$Time.Min.)

#plate 2
par(mfrow=c(1,1))
plot(lar_p2$A2~lar_p2$Time.Min.)
plot(lar_p2$A3~lar_p2$Time.Min.)
plot(lar_p2$A4~lar_p2$Time.Min.)
plot(lar_p2$A5[1:797]~lar_p2$Time.Min.[1:797])
plot(lar_p2$A6~lar_p2$Time.Min.)
plot(lar_p2$B2~lar_p2$Time.Min.)
plot(lar_p2$B3~lar_p2$Time.Min.)
plot(lar_p2$B4~lar_p2$Time.Min.)
plot(lar_p2$B6~lar_p2$Time.Min.)
plot(lar_p2$C1~lar_p2$Time.Min.)
plot(lar_p2$C2~lar_p2$Time.Min.)
plot(lar_p2$C3~lar_p2$Time.Min.)
plot(lar_p2$C5~lar_p2$Time.Min.)
plot(lar_p2$C6~lar_p2$Time.Min.)
plot(lar_p2$D1~lar_p2$Time.Min.)
plot(lar_p2$D2~lar_p2$Time.Min.)
plot(lar_p2$D4~lar_p2$Time.Min.)
plot(lar_p2$D5~lar_p2$Time.Min.)
abline(v=c(10,100,200,300,400,310))
#Blanks
par(mfrow=c(2,3))
plot(lar_p2$A1~lar_p2$Time.Min.)
plot(lar_p2$B1~lar_p2$Time.Min.)
plot(lar_p2$B5~lar_p2$Time.Min.)
plot(lar_p2$C4~lar_p2$Time.Min.)
plot(lar_p2$D6~lar_p2$Time.Min.)

#segmented regression
library(segmented)
summary(selgmented(lm(D6~Time.Min.,data=lar_p2)))


#Trim the data for the purpose of calculating the overall slope (cut off when first one hits zero)
lar_p1_rmr<-data.frame(lar_p1$Time.Min.[21:51],lar_p1$Time.Sec.[21:51],lar_p1$A3[21:51],lar_p1$A4[21:51],lar_p1$A5[21:51],lar_p1$A6[21:51],
                       lar_p1$B1[21:51],lar_p1$B2[21:51],lar_p1$B3[21:51],lar_p1$B4[21:51],lar_p1$B6[21:51],
                       lar_p1$C1[21:51],lar_p1$C3[21:51],lar_p1$C4[21:51],lar_p1$C5[21:51],lar_p1$C6[21:51],
                       lar_p1$D1[21:51],lar_p1$D2[21:51],lar_p1$D3[21:51],lar_p1$D4[21:51],lar_p1$D5[21:51],
                       lar_p1$A2[21:51],lar_p1$B5[51:81],lar_p1$C2[51:81],lar_p1$D6[21:51])
names(lar_p1_rmr)<-c("Time.Min.","Time.Sec.","A3","A4","A5","A6","B1","B2","B3","B4","B6","C1","C3","C4","C5","C6","D1","D2","D3","D4","D5","A2","B5","C2","D6")
row.names(lar_p1_rmr)<-NULL

lar_p2_rmr<-data.frame(lar_p2$Time.Min.[21:51],lar_p2$Time.Sec.[21:51],lar_p2$A2[21:51],lar_p2$A3[21:51],lar_p2$A4[21:51],lar_p2$A5[21:51],lar_p2$A6[21:51],
                       lar_p2$B2[21:51],lar_p2$B3[21:51],lar_p2$B4[21:51],lar_p2$B6[21:51],
                       lar_p2$C1[21:51],lar_p2$C2[21:51],lar_p2$C3[21:51],lar_p2$C5[21:51],lar_p2$C6[21:51],
                       lar_p2$D1[21:51],lar_p2$D2[21:51],lar_p2$D4[21:51],lar_p2$D5[21:51],
                       lar_p2$A1[21:51],lar_p2$B1[21:51],lar_p2$B5[51:81],lar_p2$C4[21:51],lar_p2$D6[21:51])
names(lar_p2_rmr)<-c("Time.Min.","Time.Sec.","A2","A3","A4","A5","A6","B2","B3","B4","B6","C1","C2","C3","C5","C6","D1","D2","D4","D5","A1","B1","B5","C4","D6")
row.names(lar_p2_rmr)<-NULL

slopes_r4p1_rmr<-data.frame(names(lar_p1_rmr)[3:25], sapply(lar_p1_rmr[3:25],function(x) ((-coef(summary(lm(x~lar_p1_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lar_p1_rmr[3:25],function(x) summary(lm(x~lar_p1_rmr$Time.Sec.))$r.squared))
names(slopes_r4p1_rmr)<-c("Well","MO2","Rsquared")

slopes_r4p2_rmr<-data.frame(names(lar_p2_rmr)[3:25], sapply(lar_p2_rmr[3:25],function(x) ((-coef(summary(lm(x~lar_p2_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lar_p2_rmr[3:25],function(x) summary(lm(x~lar_p2_rmr$Time.Sec.))$r.squared))
names(slopes_r4p2_rmr)<-c("Well","MO2","Rsquared")



#calculate the blanks
blank1<-mean(slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="A2"],slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="B1"],slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="D6"])
blank2<-mean(slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="A1"],slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="D6"],slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="B5"])
blank3<-mean(slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="B5"],slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="C4"],slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="C2"])


#Subtract the blanks
lar_p1b<-data.frame(c("A3","A4","A5","A6","B1","B2","B3","B4","B6","C1","C3","C4","C5","C6","D1","D2","D3","D4","D5"),
                    c(slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="A3"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="A4"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="A5"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="A6"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="B1"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="B2"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="B3"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="B4"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="B6"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="C1"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="C3"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="C4"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="C5"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="C6"]-blank3,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="D1"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="D2"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="D3"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="D4"]-blank1,
                      slopes_r4p1_rmr$MO2[slopes_r4p1_rmr$Well=="D5"]-blank1))
names(lar_p1b)<-c("Well","MO2")
lar_p2b<-data.frame(c("A2","A3","A4","A5","A6","B2","B3","B4","B6","C1","C2","C3","C5","C6","D1","D2","D4","D5"),
                    c(slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="A2"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="A3"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="A4"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="A5"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="A6"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="B2"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="B3"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="B4"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="B6"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="C1"]-blank3,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="C2"]-blank3,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="C3"]-blank3,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="C5"]-blank3,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="C6"]-blank3,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="D1"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="D2"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="D4"]-blank2,
                      slopes_r4p2_rmr$MO2[slopes_r4p2_rmr$Well=="D5"]-blank2))
names(lar_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataset and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
lar_p1b<-dplyr::full_join(lar_p1b,lar_trmt_p1,by="Well")
lar_p1b<-lar_p1b[1:19,] #remove the blanks
lar_p2b<-dplyr::full_join(lar_p2b,lar_trmt_p2,by="Well")
lar_p2b<-lar_p2b[1:18,] #remove the blanks
lar_flax<-rbind(lar_p1b,lar_p2b) #combine the two plates into one dataset. 
row.names(lar_flax)<-NULL #renumber the rows so it's easy to index if need be.

#Add a column for mass-specific metabolic rate, MO2/dw in umol O2 / mg dw / h. 
lar_flax<-data.frame(lar_flax, "msmr"<-c(lar_flax[1,2]/lar_flax[1,5],
                                         lar_flax[2,2]/lar_flax[2,5],
                                         lar_flax[3,2]/lar_flax[3,5],
                                         lar_flax[4,2]/lar_flax[4,5],
                                         lar_flax[5,2]/lar_flax[5,5],
                                         lar_flax[6,2]/lar_flax[6,5],
                                         lar_flax[7,2]/lar_flax[7,5],
                                         lar_flax[8,2]/lar_flax[8,5],
                                         lar_flax[9,2]/lar_flax[9,5],
                                         lar_flax[10,2]/lar_flax[10,5],
                                         lar_flax[11,2]/lar_flax[11,5],
                                         lar_flax[12,2]/lar_flax[12,5],
                                         lar_flax[13,2]/lar_flax[13,5],
                                         lar_flax[14,2]/lar_flax[14,5],
                                         lar_flax[15,2]/lar_flax[15,5],
                                         lar_flax[16,2]/lar_flax[16,5],
                                         lar_flax[17,2]/lar_flax[17,5],
                                         lar_flax[18,2]/lar_flax[18,5],
                                         lar_flax[19,2]/lar_flax[19,5],
                                         lar_flax[20,2]/lar_flax[20,5],
                                         lar_flax[21,2]/lar_flax[21,5],
                                         lar_flax[22,2]/lar_flax[22,5],
                                         lar_flax[23,2]/lar_flax[23,5],
                                         lar_flax[24,2]/lar_flax[24,5],
                                         lar_flax[25,2]/lar_flax[25,5],
                                         lar_flax[26,2]/lar_flax[26,5],
                                         lar_flax[27,2]/lar_flax[27,5],
                                         lar_flax[28,2]/lar_flax[28,5],
                                         lar_flax[29,2]/lar_flax[29,5],
                                         lar_flax[30,2]/lar_flax[30,5],
                                         lar_flax[31,2]/lar_flax[31,5],
                                         lar_flax[32,2]/lar_flax[32,5],
                                         lar_flax[33,2]/lar_flax[33,5],
                                         lar_flax[34,2]/lar_flax[34,5],
                                         lar_flax[35,2]/lar_flax[35,5],
                                         lar_flax[36,2]/lar_flax[36,5],
                                         lar_flax[37,2]/lar_flax[37,5]))
names(lar_flax)[8]<-"msmr"

#analyze the MO2 with respect to CO2
flax_lar_model<-lm(lar_flax$msmr~lar_flax$CO2_level)
anova(flax_lar_model) #CO2 level is not significant

flax_lar_mod<-lmer(msmr~CO2_level+(1|Tank),data=lar_flax)
anova(flax_lar_mod)
ranova(flax_lar_mod) #random effect of tank doesn't affect results. 

#try it the other way
flax_lar_mdl<-aov(lar_flax$MO2~lar_flax$CO2_level/factor(lar_flax$Tank))
summary(flax_lar_mdl) #

#calculate the group means 

library(plyr)
flax_lar_sum<-ddply(lar_flax,"CO2_level",summarise,N=length(msmr),MeanMO2=mean(msmr),SE=sd(msmr)/sqrt(N))
flax_lar_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxlarplot<-ggplot(flax_lar_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("2dph Larvae, Exp. 2",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.1,0.25))+
  theme_classic()
print(flaxlarplot)



#Trim the unusable parts from the original datasets for calc_mo2() analysis
#Plate1
lar_p1orig$A3[311:391]<-NA
lar_p1orig$A4[216:351]<-NA
lar_p1orig$A5[301:331]<-NA
lar_p1orig$A6[201:331]<-NA
lar_p1orig$B1[311:491]<-NA
lar_p1orig$B2[311:351]<-NA
lar_p1orig$B2[501:551]<-NA
lar_p1orig$B3[311:371]<-NA
lar_p1orig$B4[311:401]<-NA
lar_p1orig$B6[311:371]<-NA
lar_p1orig$C1[311:351]<-NA
lar_p1orig$C3[201:251]<-NA
lar_p1orig$C4[311:331]<-NA
lar_p1orig$C6[121:161]<-NA
lar_p1orig$C6[311:361]<-NA
lar_p1orig$C6[521:571]<-NA
lar_p1orig$D1[311:451]<-NA
lar_p1orig$D2[311:351]<-NA
lar_p1orig$D3[181:352]<-NA
lar_p1orig$D3[531:561]<-NA
lar_p1orig$D4[311:351]<-NA
lar_p1orig$D5[211:351]<-NA
lar_p1orig$D5[481:521]<-NA
lar_p1orig$A2[301:351]<-NA
lar_p1orig$B5[301:351]<-NA
lar_p1orig$C2[401:797]<-NA

#Plate 2
lar_p2orig$A2[331:401]<-NA
lar_p2orig$A6[246:266]<-NA
lar_p2orig$A6[331:471]<-NA
lar_p2orig$B2[251:401]<-NA
lar_p2orig$B3[311:331]<-NA
lar_p2orig$B6[201:261]<-NA
lar_p2orig$C2[301:551]<-NA
lar_p2orig$C5[651:747]<-NA
lar_p2orig$D1[311:351]<-NA
lar_p2orig$D1[431:551]<-NA
lar_p2orig$D2[311:351]<-NA
lar_p2orig$D4[251:351]<-NA
lar_p2orig$D5[211:276]<-NA
lar_p2orig$B1[141:176]<-NA
lar_p2orig$B1[311:501]<-NA


#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
#P1A1<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$A1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("A1",times=31),rep("1A",times=31),rep("amb",times=31))
#P1A2<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$A2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("A2",times=31),rep("1A",times=31),rep("blank1",times=31))
P1A3<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$A3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("A3",times=31),rep("1A",times=31),rep("amb",times=31))
P1A4<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$A4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("A4",times=31),rep("1A",times=31),rep("amb",times=31))
P1A5<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$A5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("A5",times=31),rep("1A",times=31),rep("amb",times=31))
P1A6<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$A6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("A6",times=31),rep("1A",times=31),rep("amb",times=31))
P1B1<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$B1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("B1",times=31),rep("3A",times=31),rep("high",times=31))
P1B2<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$B2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("B2",times=31),rep("3A",times=31),rep("high",times=31))
P1B3<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$B3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("B3",times=31),rep("3A",times=31),rep("high",times=31))
P1B4<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$B4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("B4",times=31),rep("3A",times=31),rep("high",times=31))
#P1B5<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$B5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("B5",times=31),rep("3A",times=31),rep("blank3",times=31))
P1B6<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$B6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("B6",times=31),rep("3A",times=31),rep("high",times=31))
P1C1<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$C1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("C1",times=31),rep("3C",times=31),rep("high",times=31))
#P1C2<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$C2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("C2",times=31),rep("3C",times=31),rep("high",times=31))
P1C3<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$C3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("C3",times=31),rep("3C",times=31),rep("blank3",times=31))
P1C4<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$C4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("C4",times=31),rep("3C",times=31),rep("high",times=31))
P1C5<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$C5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("C5",times=31),rep("3C",times=31),rep("high",times=31))
P1C6<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$C6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("C6",times=31),rep("3C",times=31),rep("high",times=31))
P1D1<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$D1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("D1",times=31),rep("1C",times=31),rep("amb",times=31))
P1D2<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$D2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("D2",times=31),rep("1C",times=31),rep("amb",times=31))
P1D3<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$D3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("D3",times=31),rep("1C",times=31),rep("amb",times=31))
P1D4<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$D4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("D4",times=31),rep("1C",times=31),rep("amb",times=31))
P1D5<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$D5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("D5",times=31),rep("1C",times=31),rep("amb",times=31))
#P1D6<-data.frame(calc_MO2(duration=lar_p1orig[1:1166,]$Time.Min.,o2=lar_p1orig[1:1166,]$D6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p1orig[1:1166,]$T_internal,sal=27.5),rep("D6",times=31),rep("1C",times=31),rep("blank1",times=31))

#P2A1<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$A1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("A1",times=31),rep("2A",times=31),rep("blank2",times=31))
P2A2<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$A2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("A2",times=31),rep("2A",times=31),rep("med",times=31))
P2A3<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$A3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("A3",times=31),rep("2A",times=31),rep("med",times=31))
P2A4<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$A4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("A4",times=31),rep("2A",times=31),rep("med",times=31))
P2A5<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$A5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("A5",times=31),rep("2A",times=31),rep("med",times=31))
P2A6<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$A6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("A6",times=31),rep("2A",times=31),rep("med",times=31))
#P2B1<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$B1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("B1",times=31),rep("1B",times=31),rep("blank1",times=31))
#P2B2<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$B2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("B2",times=31),rep("2C",times=31),rep("med",times=31))
P2B3<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$B3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("B3",times=31),rep("2C",times=31),rep("med",times=31))
P2B4<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$B4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("B4",times=31),rep("2C",times=31),rep("med",times=31))
#P2B5<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$B5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("B5",times=31),rep("2C",times=31),rep("blank2",times=31))
P2B6<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$B6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("B6",times=31),rep("2C",times=31),rep("med",times=31))
#P2C1<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$C1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("C1",times=31),rep("3B",times=31),rep("high",times=31))
P2C2<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$C2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("C2",times=31),rep("3B",times=31),rep("high",times=31))
#P2C3<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$C3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("C3",times=31),rep("3B",times=31),rep("high",times=31))
#P2C4<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$C4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("C4",times=31),rep("3B",times=31),rep("blank3",times=31))
P2C5<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$C5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("C5",times=31),rep("3B",times=31),rep("high",times=31))
P2C6<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$C6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("C6",times=31),rep("3B",times=31),rep("high",times=31))
P2D1<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$D1,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("D1",times=31),rep("2B",times=31),rep("med",times=31))
P2D2<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$D2,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("D2",times=31),rep("2B",times=31),rep("med",times=31))
#P2D3<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$D3,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("D3",times=31),rep("2B",times=31),rep("med",times=31))
P2D4<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$D4,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("D4",times=31),rep("2B",times=31),rep("med",times=31))
P2D5<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$D5,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("D5",times=31),rep("2B",times=31),rep("med",times=31))
#P2D6<-data.frame(calc_MO2(duration=lar_p2orig[1:1166,]$Time.Min.,o2=lar_p2orig[1:1166,]$D6,o2_unit="mg_per_l",bin_width=1166/30,vol=0.0005,temp=lar_p2orig[1:1166,]$T_internal,sal=27.5),rep("D6",times=31),rep("2B",times=31),rep("blank2",times=31))

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


#Add a column in each well's calc_mo2 output for mass and msmr. 
P1A3$dw<-rep(0.11375326,times=31)
P1A4$dw<-rep(0.11681336,times=31)
P1A5$dw<-rep(0.09922280,times=31)
P1A6$dw<-rep(0.10432513,times=31)
P1B1$dw<-rep(0.08709446,times=31)
P1B2$dw<-rep(0.09902456,times=31)
P1B3$dw<-rep(0.10209817,times=31)
P1B4$dw<-rep(0.11107118,times=31)
P1B6$dw<-rep(0.11923361,times=31)
P1C1$dw<-rep(0.14085895,times=31)
P1C3$dw<-rep(0.09655368,times=31)
P1C4$dw<-rep(0.12954792,times=31)
P1C5$dw<-rep(0.10568236,times=31)
P1C6$dw<-rep(0.13715155,times=31)
P1D1$dw<-rep(0.14086458,times=31)
P1D2$dw<-rep(0.09798792,times=31)
P1D3$dw<-rep(0.13085227,times=31)
P1D4$dw<-rep(0.13390428,times=31)
P1D5$dw<-rep(0.12593972,times=31)
P2A2$dw<-rep(0.10935514,times=31)
P2A3$dw<-rep(0.12358477,times=31)
P2A4$dw<-rep(0.08569174,times=31)
P2A5$dw<-rep(0.07806737,times=31)
P2A6$dw<-rep(0.11007652,times=31)
P2B3$dw<-rep(0.13847522,times=31)
P2B4$dw<-rep(0.10507304,times=31)
P2B6$dw<-rep(0.11904125,times=31)
P2C2$dw<-rep(0.09615336,times=31)
P2C5$dw<-rep(0.15999001,times=31)
P2C6$dw<-rep(0.11356300,times=31)
P2D1$dw<-rep(0.12751676,times=31)
P2D2$dw<-rep(0.14018903,times=31)
P2D4$dw<-rep(0.12256590,times=31)
P2D5$dw<-rep(0.12119752,times=31)

P1A3$msmrs<-P1A3$MO2/P1A3$dw
P1A4$msmrs<-P1A4$MO2/P1A4$dw
P1A5$msmrs<-P1A5$MO2/P1A5$dw
P1A6$msmrs<-P1A6$MO2/P1A6$dw
P1B1$msmrs<-P1B1$MO2/P1B1$dw
P1B2$msmrs<-P1B2$MO2/P1B2$dw
P1B3$msmrs<-P1B3$MO2/P1B3$dw
P1B4$msmrs<-P1B4$MO2/P1B4$dw
P1B6$msmrs<-P1B6$MO2/P1B6$dw
P1C1$msmrs<-P1C1$MO2/P1C1$dw
P1C3$msmrs<-P1C3$MO2/P1C3$dw
P1C4$msmrs<-P1C4$MO2/P1C4$dw
P1C5$msmrs<-P1C5$MO2/P1C5$dw
P1C6$msmrs<-P1C6$MO2/P1C6$dw
P1D1$msmrs<-P1D1$MO2/P1D1$dw
P1D2$msmrs<-P1D2$MO2/P1D2$dw
P1D3$msmrs<-P1D3$MO2/P1D3$dw
P1D4$msmrs<-P1D4$MO2/P1D4$dw
P1D5$msmrs<-P1D5$MO2/P1D5$dw
P2A2$msmrs<-P2A2$MO2/P2A2$dw
P2A3$msmrs<-P2A3$MO2/P2A3$dw
P2A4$msmrs<-P2A4$MO2/P2A4$dw
P2A5$msmrs<-P2A5$MO2/P2A5$dw
P2A6$msmrs<-P2A6$MO2/P2A6$dw
P2B3$msmrs<-P2B3$MO2/P2B3$dw
P2B4$msmrs<-P2B4$MO2/P2B4$dw
P2B6$msmrs<-P2B6$MO2/P2B6$dw
P2C2$msmrs<-P2C2$MO2/P2C2$dw
P2C5$msmrs<-P2C5$MO2/P2C5$dw
P2C6$msmrs<-P2C6$MO2/P2C6$dw
P2D1$msmrs<-P2D1$MO2/P2D1$dw
P2D2$msmrs<-P2D2$MO2/P2D2$dw
P2D4$msmrs<-P2D4$MO2/P2D4$dw
P2D5$msmrs<-P2D5$MO2/P2D5$dw


#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES?? Fit a line to all of the points from the same treatment/tank. 
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
alllar<-rbind(P1A3,P1A4,P1A5,P1A6,P1B1,P1B2,P1B3,P1B4,P1B6,P1C1,P1C3,P1C4,P1C5,P1C6,P1D1,P1D2,P1D3,P1D4,P1D5,
              P2A2,P2A3,P1A4,P2A5,P2A6,P2B3,P2B4,P2B6,P2C2,P2C5,P2C6,P2D1,P2D2,P2D4,P2D5)


#plot the curves
library(ggplot2)
allplotlar<-ggplot(alllar, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlar)

#Try plotting one at a time, grouped by treatment
library(gridExtra)
#Ambient: P1A3, P1A4, P1A5, P1A6, P1D1, P1D2, P1D3, P1D4, P1D5
P1A3plot<-ggplot(P1A3,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A4plot<-ggplot(P1A4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A5plot<-ggplot(P1A5,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A6plot<-ggplot(P1A6,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D1plot<-ggplot(P1D1,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D2plot<-ggplot(P1D2,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D3plot<-ggplot(P1D3,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D4plot<-ggplot(P1D4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D5plot<-ggplot(P1D5,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1A3plot,P1A4plot,P1A5plot,P1A6plot,P1D1plot,P1D2plot,P1D3plot,P1D4plot,P1D5plot,ncol=3)


#Medium: P2A2, P2A3, P2A4, P2A5, P2A6, P2B2, P2B3, P2B4, P2B6, P2D1, P2D2, P2D4, P2D5
P2A2plot<-ggplot(P2A2,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A3plot<-ggplot(P2A3,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A4plot<-ggplot(P2A4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A5plot<-ggplot(P2A5,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A6plot<-ggplot(P2A6,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B2plot<-ggplot(P2B2,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B3plot<-ggplot(P2B3,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B4plot<-ggplot(P2B4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B6plot<-ggplot(P2B6,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D1plot<-ggplot(P2D1,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D2plot<-ggplot(P2D2,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D4plot<-ggplot(P2D4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D5plot<-ggplot(P2D5,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P2A2plot,P2A3plot,P2A4plot,P2A5plot,P2A6plot,P2B2plot,P2B3plot,P2B4plot,P2B6plot,P2D1plot,P2D2plot,P2D4plot,P2D5plot,ncol=5)

#High: P1B1, P1B2, P1B3, P1B4, P1B6, P1C1, P1C3, P1C4, P1C5, P1C6, P2C2, P2C5, P2C6
P1B1plot<-ggplot(P1B1,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B2plot<-ggplot(P1B2,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B3plot<-ggplot(P1B3,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B4plot<-ggplot(P1B4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B6plot<-ggplot(P1B6,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C1plot<-ggplot(P1C1,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C3plot<-ggplot(P1C3,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C4plot<-ggplot(P1C4,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C5plot<-ggplot(P1C5,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C6plot<-ggplot(P1C6,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C2plot<-ggplot(P2C2,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C5plot<-ggplot(P2C5,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C6plot<-ggplot(P2C6,aes(x=O2_MEAN,y=msmrs))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1B1plot,P1B2plot,P1B3plot,P1B4plot,P1B6plot,P1C1plot,P1C3plot,P1C4plot,P1C5plot,P1C6plot,P2C2plot,P2C5plot,P2C6plot,ncol=5)


#calculate pcrit and add it as a column in the lar_flax dataframe. 


lar_flax$Pcrit_alpha<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Alpha'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Alpha'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['Alpha'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$msmrs)['Alpha'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Alpha'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Alpha'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Alpha'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Alpha'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['Alpha'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Alpha'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Alpha'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['Alpha'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Alpha'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['Alpha'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Alpha'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Alpha'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$msmrs)['Alpha'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Alpha'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Alpha'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['Alpha'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['Alpha'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Alpha'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Alpha'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Alpha'],
                        NA,
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Alpha'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Alpha'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Alpha'],
                        NA,
                        calc_pcrit(P2C2$O2_MEAN,P2C2$msmrs)['Alpha'],
                        NA,
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Alpha'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Alpha'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Alpha'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['Alpha'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Alpha'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Alpha'])
lar_flax$Pcrit_break<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Breakpoint'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Breakpoint'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['Breakpoint'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$msmrs)['Breakpoint'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Breakpoint'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Breakpoint'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Breakpoint'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Breakpoint'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['Breakpoint'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Breakpoint'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Breakpoint'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['Breakpoint'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Breakpoint'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['Breakpoint'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Breakpoint'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Breakpoint'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$msmrs)['Breakpoint'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Breakpoint'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Breakpoint'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['Breakpoint'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['Breakpoint'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Breakpoint'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Breakpoint'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Breakpoint'],
                        NA,
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Breakpoint'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Breakpoint'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Breakpoint'],
                        NA,
                        calc_pcrit(P2C2$O2_MEAN,P2C2$msmrs)['Breakpoint'],
                        NA,
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Breakpoint'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Breakpoint'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Breakpoint'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['Breakpoint'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Breakpoint'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Breakpoint'])
lar_flax$Pcrit_subPI<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Sub_PI'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Sub_PI'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['Sub_PI'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$msmrs)['Sub_PI'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Sub_PI'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Sub_PI'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Sub_PI'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Sub_PI'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['Sub_PI'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Sub_PI'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Sub_PI'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['Sub_PI'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Sub_PI'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['Sub_PI'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Sub_PI'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Sub_PI'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$msmrs)['Sub_PI'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Sub_PI'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Sub_PI'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['Sub_PI'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['Sub_PI'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Sub_PI'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Sub_PI'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Sub_PI'],
                        NA,
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Sub_PI'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Sub_PI'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Sub_PI'],
                        NA,
                        calc_pcrit(P2C2$O2_MEAN,P2C2$msmrs)['Sub_PI'],
                        NA,
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Sub_PI'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Sub_PI'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Sub_PI'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['Sub_PI'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Sub_PI'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Sub_PI'])
lar_flax$Pcrit_NLR<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['NLR'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['NLR'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['NLR'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$msmrs)['NLR'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['NLR'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['NLR'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['NLR'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['NLR'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['NLR'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['NLR'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['NLR'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['NLR'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['NLR'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['NLR'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['NLR'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['NLR'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$msmrs)['NLR'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['NLR'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['NLR'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['NLR'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['NLR'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['NLR'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['NLR'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['NLR'],
                        NA,
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['NLR'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['NLR'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['NLR'],
                        NA,
                        calc_pcrit(P2C2$O2_MEAN,P2C2$msmrs)['NLR'],
                        NA,
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['NLR'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['NLR'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['NLR'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['NLR'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['NLR'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['NLR'])
lar_flax$alpha<-c(calc_alpha(P1A3$O2_MEAN,P1A3$msmrs)$alpha,
                  calc_alpha(P1A4$O2_MEAN,P1A4$msmrs)$alpha,
                  calc_alpha(P1A5$O2_MEAN,P1A5$msmrs)$alpha,
                  calc_alpha(P1A6$O2_MEAN,P1A6$msmrs)$alpha,
                  calc_alpha(P1B1$O2_MEAN,P1B1$msmrs)$alpha,
                  calc_alpha(P1B2$O2_MEAN,P1B2$msmrs)$alpha,
                  calc_alpha(P1B3$O2_MEAN,P1B3$msmrs)$alpha,
                  calc_alpha(P1B4$O2_MEAN,P1B4$msmrs)$alpha,
                  calc_alpha(P1B6$O2_MEAN,P1B6$msmrs)$alpha,
                  calc_alpha(P1C1$O2_MEAN,P1C1$msmrs)$alpha,
                  calc_alpha(P1C3$O2_MEAN,P1C3$msmrs)$alpha,
                  calc_alpha(P1C4$O2_MEAN,P1C4$msmrs)$alpha,
                  calc_alpha(P1C5$O2_MEAN,P1C5$msmrs)$alpha,
                  calc_alpha(P1C6$O2_MEAN,P1C6$msmrs)$alpha,
                  calc_alpha(P1D1$O2_MEAN,P1D1$msmrs)$alpha,
                  calc_alpha(P1D2$O2_MEAN,P1D2$msmrs)$alpha,
                  calc_alpha(P1D3$O2_MEAN,P1D3$msmrs)$alpha,
                  calc_alpha(P1D4$O2_MEAN,P1D4$msmrs)$alpha,
                  calc_alpha(P1D5$O2_MEAN,P1D5$msmrs)$alpha,
                  calc_alpha(P2A2$O2_MEAN,P2A2$msmrs)$alpha,
                  calc_alpha(P2A3$O2_MEAN,P2A3$msmrs)$alpha,
                  calc_alpha(P2A4$O2_MEAN,P2A4$msmrs)$alpha,
                  calc_alpha(P2A5$O2_MEAN,P2A5$msmrs)$alpha,
                  calc_alpha(P2A6$O2_MEAN,P2A6$msmrs)$alpha,
                  NA,
                  calc_alpha(P2B3$O2_MEAN,P2B3$msmrs)$alpha,
                  calc_alpha(P2B4$O2_MEAN,P2B4$msmrs)$alpha,
                  calc_alpha(P2B6$O2_MEAN,P2B6$msmrs)$alpha,
                  NA,
                  calc_alpha(P2C2$O2_MEAN,P2C2$msmrs)$alpha,
                  NA,
                  calc_alpha(P2C5$O2_MEAN,P2C5$msmrs)$alpha,
                  calc_alpha(P2C6$O2_MEAN,P2C6$msmrs)$alpha,
                  calc_alpha(P2D1$O2_MEAN,P2D1$msmrs)$alpha,
                  calc_alpha(P2D2$O2_MEAN,P2D2$msmrs)$alpha,
                  calc_alpha(P2D4$O2_MEAN,P2D4$msmrs)$alpha,
                  calc_alpha(P2D5$O2_MEAN,P2D5$msmrs)$alpha)


#plot pcrit to look for probs
plot_pcrit(P1A3$O2_MEAN,P1A3$msmrs)
plot_pcrit(P1A4$O2_MEAN,P1A4$msmrs)
plot_pcrit(P1A5$O2_MEAN,P1A5$msmrs)
plot_pcrit(P1A6$O2_MEAN,P1A6$msmrs)
plot_pcrit(P1B1$O2_MEAN,P1B1$msmrs)
plot_pcrit(P1B2$O2_MEAN,P1B2$msmrs)
plot_pcrit(P1B3$O2_MEAN,P1B3$msmrs)
plot_pcrit(P1B4$O2_MEAN,P1B4$msmrs)
plot_pcrit(P1B6$O2_MEAN,P1B6$msmrs)
plot_pcrit(P1C1$O2_MEAN,P1C1$msmrs)
plot_pcrit(P1C3$O2_MEAN,P1C3$msmrs)
plot_pcrit(P1C4$O2_MEAN,P1C4$msmrs)

plot_pcrit(P1C5$O2_MEAN,P1C5$msmrs)
plot_pcrit(P1C6$O2_MEAN,P1C6$msmrs)
plot_pcrit(P1D1$O2_MEAN,P1D1$msmrs)
plot_pcrit(P1D2$O2_MEAN,P1D2$msmrs)
plot_pcrit(P1D3$O2_MEAN,P1D3$msmrs)
plot_pcrit(P1D4$O2_MEAN,P1D4$msmrs)
plot_pcrit(P1D5$O2_MEAN,P1D5$msmrs)
plot_pcrit(P2A2$O2_MEAN,P2A2$msmrs)
plot_pcrit(P2A3$O2_MEAN,P2A3$msmrs)
plot_pcrit(P2A4$O2_MEAN,P2A4$msmrs)
plot_pcrit(P2A5$O2_MEAN,P2A5$msmrs)
plot_pcrit(P2A6$O2_MEAN,P2A6$msmrs)

plot_pcrit(P2B3$O2_MEAN,P2B3$msmrs)
plot_pcrit(P2B4$O2_MEAN,P2B4$msmrs)
plot_pcrit(P2B6$O2_MEAN,P2B6$msmrs)
plot_pcrit(P2C2$O2_MEAN,P2C2$msmrs)
plot_pcrit(P2C5$O2_MEAN,P2C5$msmrs)
plot_pcrit(P2C6$O2_MEAN,P2C6$msmrs)
plot_pcrit(P2D1$O2_MEAN,P2D1$msmrs)
plot_pcrit(P2D2$O2_MEAN,P2D2$msmrs)
plot_pcrit(P2D4$O2_MEAN,P2D4$msmrs)
plot_pcrit(P2D5$O2_MEAN,P2D5$msmrs)

#Analyze and visualize with respect to CO2 treatments

#set levels of CO2_level and Tank
lar_flax$CO2_level<-factor(lar_flax$CO2_level,levels=c("amb","med","high"))
lar_flax$Tank<-factor(lar_flax$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))

plot(lar_flax$Pcrit_alpha~lar_flax$CO2_level)
plot(lar_flax$Pcrit_break~lar_flax$CO2_level)

#check if different Pcrit metrics are correlated
plot(lar_flax$Pcrit_alpha~lar_flax$Pcrit_break)

#calculate mean and SE of Pcrit values by treatment
library(plyr)
alpha_sum<-ddply(lar_flax,"CO2_level",summarise,N=length(Pcrit_alpha),MeanPcrit=mean(Pcrit_alpha,na.rm=TRUE),SE=sd(Pcrit_alpha,na.rm=TRUE)/sqrt(N))
alpha_sum

break_sum<-ddply(lar_flax,"CO2_level",summarise,N=length(Pcrit_break),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers

a_sum<-ddply(lar_flax,"CO2_level",summarise,N=length(alpha),MeanAlpha=mean(alpha,na.rm=TRUE),SE=sd(alpha,na.rm=TRUE)/sqrt(N))
a_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

alpha_mod<-lmer(Pcrit_alpha~CO2_level+1|Tank,data=lar_flax) #singular fit
ranova(alpha_mod)

alpha_mod1<-lm(Pcrit_alpha~CO2_level,data=lar_flax)
anova(alpha_mod1) #not quite significant, p=0.0891

break_mod<-lmer(Pcrit_break~CO2_level+1|Tank,data=lar_flax) #singular fit
ranova(break_mod)

break_mod1<-lm(Pcrit_break~CO2_level,data=lar_flax)
anova(break_mod1) #ditto, p=0.07111

a_mod1<-lm(alpha~CO2_level,data=lar_flax)
anova(a_mod1)

#are residuals normally distributed
res_alpha<-residuals(alpha_mod1)
hist(res_alpha)
res_break<-residuals(break_mod1)
hist(res_break)
#right skewed

par(mfrow=c(2,2))
plot(alpha_mod1)
plot(break_mod1)

#the data don't meet the assumptions of the model