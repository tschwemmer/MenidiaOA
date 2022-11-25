#Respirometry analysis from 2021 experiment

#Run 3: Larvae, Dana Hall, 6-17-21

#load data sheets and treatments
lar_p1orig<-read.csv(file.choose(),header=TRUE)
lar_p2orig<-read.csv(file.choose(),header=TRUE)
lar_trmt_p1<-read.csv(file.choose(),header=TRUE)
lar_trmt_p2<-read.csv(file.choose(),header=TRUE)

#remove first 85 minutes from Plate 1, first 30 minutes from Plate 2. Then renumber the rows.
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
lar_trmt_p1$Tank<-factor(lar_trmt_p1$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))
lar_trmt_p2$Tank<-factor(lar_trmt_p2$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))


#calculate slope for each well using all untrimmed data. Units will be umol consumed per hour
slopes_r3p1<-data.frame(names(lar_p1)[4:27], sapply(lar_p1[4:27],function(x) ((-coef(summary(lm(x~lar_p1$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(lar_p1[4:27],function(x) summary(lm(x~lar_p1$Time.Sec.))$r.squared))
names(slopes_r3p1)<-c("Well","MO2","Rsquared")
slopes_r3p2<-data.frame(names(lar_p2)[4:27], sapply(lar_p2[4:27],function(x) ((-coef(summary(lm(x~lar_p2$Time.Sec.)))[2])/31.9988)*1800),
                        sapply(lar_p2[4:27],function(x) summary(lm(x~lar_p2$Time.Sec.))$r.squared))
names(slopes_r3p2)<-c("Well","MO2","Rsquared")

#plot data for all live wells
par(mfrow=c(2,4))
plot(lar_p1$A2~lar_p1$Time.Min.)
plot(lar_p1$A3~lar_p1$Time.Min.)
plot(lar_p1$A4~lar_p1$Time.Min.)
plot(lar_p1$A5~lar_p1$Time.Min.)
plot(lar_p1$B1~lar_p1$Time.Min.)
plot(lar_p1$B2~lar_p1$Time.Min.)
plot(lar_p1$B4~lar_p1$Time.Min.)
plot(lar_p1$B5~lar_p1$Time.Min.)
plot(lar_p1$C1~lar_p1$Time.Min.)
plot(lar_p1$C3~lar_p1$Time.Min.)
plot(lar_p1$C5~lar_p1$Time.Min.)
plot(lar_p1$C6~lar_p1$Time.Min.)
plot(lar_p1$D2~lar_p1$Time.Min.)
plot(lar_p1$D4~lar_p1$Time.Min.)
plot(lar_p1$D5~lar_p1$Time.Min.)
#blanks
plot(lar_p1$A1~lar_p1$Time.Min.)
plot(lar_p1$A6~lar_p1$Time.Min.)
plot(lar_p1$B3~lar_p1$Time.Min.)
plot(lar_p1$B6~lar_p1$Time.Min.)
plot(lar_p1$C2~lar_p1$Time.Min.)
plot(lar_p1$C4~lar_p1$Time.Min.)
plot(lar_p1$D1~lar_p1$Time.Min.)
plot(lar_p1$D3~lar_p1$Time.Min.)
abline(v=c(10,80,100))

plot(lar_p2$A1~lar_p2$Time.Min.)
plot(lar_p2$A3~lar_p2$Time.Min.)
plot(lar_p2$A5~lar_p2$Time.Min.)
plot(lar_p2$B1~lar_p2$Time.Min.)
plot(lar_p2$B2~lar_p2$Time.Min.)
plot(lar_p2$B4~lar_p2$Time.Min.)
plot(lar_p2$B6~lar_p2$Time.Min.)
plot(lar_p2$C3~lar_p2$Time.Min.)
plot(lar_p2$C4~lar_p2$Time.Min.)
plot(lar_p2$C5~lar_p2$Time.Min.)
plot(lar_p2$C6~lar_p2$Time.Min.)
plot(lar_p2$D1~lar_p2$Time.Min.)
plot(lar_p2$D4~lar_p2$Time.Min.)
plot(lar_p2$D5~lar_p2$Time.Min.)
#Blanks
par(mfrow=c(3,4))
plot(lar_p2$A2~lar_p2$Time.Min.)
plot(lar_p2$A4~lar_p2$Time.Min.)
plot(lar_p2$A6~lar_p2$Time.Min.)
plot(lar_p2$B3~lar_p2$Time.Min.)
plot(lar_p2$B5~lar_p2$Time.Min.)
plot(lar_p2$C1~lar_p2$Time.Min.)
plot(lar_p2$C2~lar_p2$Time.Min.)
plot(lar_p2$D2~lar_p2$Time.Min.)
plot(lar_p2$D3~lar_p2$Time.Min.)
plot(lar_p2$D6~lar_p2$Time.Min.)

#Trim the data for the purpose of calculating the overall slope (cut off when first one hits zero)
lar_p1_rmr<-data.frame(lar_p1$Time.Min.[46:136], lar_p1$Time.Sec.[46:136],lar_p1$A2[46:136],lar_p1$A3[46:136],lar_p1$A4[46:136],lar_p1$A5[46:136],
                       lar_p1$B1[76:166],lar_p1$B2[46:136],lar_p1$B4[46:136],lar_p1$B5[46:136],
                       lar_p1$C1[46:136],lar_p1$C3[46:136],lar_p1$C5[46:136],lar_p1$C6[46:136],
                       lar_p1$D2[46:136],lar_p1$D4[46:136],lar_p1$D5[46:136],
                       lar_p1$A1[46:136],lar_p1$A6[46:136],lar_p1$B3[46:136],
                       lar_p1$B6[46:136],lar_p1$C2[46:136],lar_p1$C4[46:136],lar_p1$D3[46:136])
names(lar_p1_rmr)<-c("Time.Min.","Time.Sec.","A2","A3","A4","A5","B1","B2","B4","B5","C1","C3","C5","C6","D2","D4","D5","A1","A6","B3","B6","C2","C4","D3")

lar_p2_rmr<-data.frame(lar_p2$Time.Min.[46:136], lar_p2$Time.Sec.[46:136],lar_p2$A1[46:136],lar_p2$A3[46:136],lar_p2$A5[46:136],
                       lar_p2$B1[46:136],lar_p2$B2[46:136],lar_p2$B4[46:136],lar_p2$B6[46:136],
                       lar_p2$C3[46:136],lar_p2$C4[46:136],lar_p2$C5[46:136],lar_p2$C6[46:136],
                       lar_p2$D1[46:136],lar_p2$D4[46:136],lar_p2$D5[46:136],
                       lar_p2$A2[46:136],lar_p2$A4[46:136],lar_p2$A6[46:136],lar_p2$B3[46:136],lar_p2$B5[46:136],
                       lar_p2$C1[46:136],lar_p2$C2[46:136],lar_p2$D2[46:136],lar_p2$D3[46:136],lar_p2$D6[46:136])
names(lar_p2_rmr)<-c("Time.Min.","Time.Sec.","A1","A3","A5","B1","B2","B4","B6","C3","C4","C5","C6","D1","D4","D5","A2","A4","A6","B3","B5","C1","C2","D2","D3","D6")


slopes_r3p1_rmr<-data.frame(names(lar_p1_rmr)[3:24], sapply(lar_p1_rmr[3:24],function(x) ((-coef(summary(lm(x~lar_p1_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lar_p1_rmr[3:24],function(x) summary(lm(x~lar_p1_rmr$Time.Sec.))$r.squared))
names(slopes_r3p1_rmr)<-c("Well","MO2","Rsquared")

slopes_r3p2_rmr<-data.frame(names(lar_p2_rmr)[3:26], sapply(lar_p2_rmr[3:26],function(x) ((-coef(summary(lm(x~lar_p2_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lar_p2_rmr[3:26],function(x) summary(lm(x~lar_p2_rmr$Time.Sec.))$r.squared))
names(slopes_r3p2_rmr)<-c("Well","MO2","Rsquared")



#calculate the blanks
t1blank<-slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="D3"]
t2blank<-mean(slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="A1"],slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="A6"])
t3blank<-mean(slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="A6"],slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="D6"])
t4blank<-mean(slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="A2"],slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="A4"])
t5blank<-mean(slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="D2"],slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="D3"])
t6blank<-mean(slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="C1"],slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="C2"])
t7blank<-mean(slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="C2"],slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="C4"])
t8blank<-mean(slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="B3"],slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="B6"])
t9blank<-mean(slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="B3"],slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="B5"])


#Subtract the blanks
lar_p1b<-data.frame(c("A2","A3","A4","A5","B1","B2","B4","B5","C1","C3","C5","C6","D2","D4","D5"),
                    c(slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="A2"]-t2blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="A3"]-t2blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="A4"]-t2blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="A5"]-t2blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="B1"]-t8blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="B2"]-t8blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="B4"]-t8blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="B5"]-t8blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="C1"]-t7blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="C3"]-t7blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="C5"]-t7blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="C6"]-t7blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="D2"]-t1blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="D4"]-t1blank,
                      slopes_r3p1_rmr$MO2[slopes_r3p1_rmr$Well=="D5"]-t1blank))
names(lar_p1b)<-c("Well","MO2")
lar_p2b<-data.frame(c("A1","A3","A5","B1","B2","B4","B6","C3","C4","C5","C6","D1","D4","D5"),
                    c(slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="A1"]-t4blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="A3"]-t4blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="A5"]-t4blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="B1"]-t9blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="B2"]-t9blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="B4"]-t9blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="B6"]-t3blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="C3"]-t6blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="C4"]-t6blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="C5"]-t6blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="C6"]-t3blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="D1"]-t5blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="D4"]-t5blank,
                      slopes_r3p2_rmr$MO2[slopes_r3p2_rmr$Well=="D5"]-t5blank))
names(lar_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataset and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
lar_p1b<-dplyr::full_join(lar_p1b,lar_trmt_p1,by="Well")
lar_p1b<-lar_p1b[1:15,] #remove the blanks
lar_p2b<-dplyr::full_join(lar_p2b,lar_trmt_p2,by="Well")
lar_p2b<-lar_p2b[1:14,] #remove the blanks
lar_dana<-rbind(lar_p1b,lar_p2b) #combine the two plates into one dataset. 
row.names(lar_dana)<-NULL #renumber the rows so it's easy to index if need be.

#Add a column for mass-specific metabolic rate, MO2/dw in umol O2 / mg dw / h. 
lar_dana<-data.frame(lar_dana, "msmr"<-c(lar_dana[1,2]/lar_dana[1,5],
                                         lar_dana[2,2]/lar_dana[2,5],
                                         lar_dana[3,2]/lar_dana[3,5],
                                         lar_dana[4,2]/lar_dana[4,5],
                                         lar_dana[5,2]/lar_dana[5,5],
                                         lar_dana[6,2]/lar_dana[6,5],
                                         lar_dana[7,2]/lar_dana[7,5],
                                         lar_dana[8,2]/lar_dana[8,5],
                                         lar_dana[9,2]/lar_dana[9,5],
                                         lar_dana[10,2]/lar_dana[10,5],
                                         lar_dana[11,2]/lar_dana[11,5],
                                         lar_dana[12,2]/lar_dana[12,5],
                                         lar_dana[13,2]/lar_dana[13,5],
                                         lar_dana[14,2]/lar_dana[14,5],
                                         lar_dana[15,2]/lar_dana[15,5],
                                         lar_dana[16,2]/lar_dana[16,5],
                                         lar_dana[17,2]/lar_dana[17,5],
                                         lar_dana[18,2]/lar_dana[18,5],
                                         lar_dana[19,2]/lar_dana[19,5],
                                         lar_dana[20,2]/lar_dana[20,5],
                                         lar_dana[21,2]/lar_dana[21,5],
                                         lar_dana[22,2]/lar_dana[22,5],
                                         lar_dana[23,2]/lar_dana[23,5],
                                         lar_dana[24,2]/lar_dana[24,5],
                                         lar_dana[25,2]/lar_dana[25,5],
                                         lar_dana[26,2]/lar_dana[26,5],
                                         lar_dana[27,2]/lar_dana[27,5],
                                         lar_dana[28,2]/lar_dana[28,5],
                                         lar_dana[29,2]/lar_dana[29,5]))
names(lar_dana)[6]<-"msmr"

#analyze the MO2 with respect to CO2
dana_lar_model<-lm(lar_dana$msmr~lar_dana$CO2_level)
anova(dana_lar_model) #CO2 level is not significant

dana_lar_mod<-lmer(msmr~CO2_level+(1|Tank),data=lar_dana)
anova(dana_lar_mod)
ranova(dana_lar_mod) #random effect of tank does affect results. 

#try it the other way
dana_lar_mdl<-aov(lar_dana$msmr~lar_dana$CO2_level/factor(lar_dana$Tank))
summary(dana_lar_mdl) #CO2 not significant, tank is. 

#calculate the group means 

library(plyr)
dana_lar_sum<-ddply(lar_dana,"CO2_level",summarise,N=length(msmr),MeanMO2=mean(msmr),SE=sd(msmr)/sqrt(N))
dana_lar_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
danalarplot<-ggplot(dana_lar_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("2dph Larvae, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  theme_classic()
print(danalarplot)

##############################################################################################################
#Try using piecewise regression to isolate non fluctuating parts
library(segmented)
segmented.P1A3<-selgmented(lm(A3~Time.Min.,data=lar_p1),seg.Z=~Time.Min.)
summary(segmented.P1A3)

#remove first 10 minutes and parts with extremely high temperature change
#first check temperature in 10 min bins
library(Thermimage)
lar_p1slope<-slopeEveryN(lar_p1$T_internal,n=10) #makes the slope with respect to the index, NOT time. In this case time=index because data was recorded once per minute, but in other ones I will have to multiply the slopes by 3 to get degrees per minute instead of per 20 seconds. 
lar_p1slope<-data.frame(lar_p1slope)
lar_p1slope$DegreesHour<-unlist(lapply(X=lar_p1slope$Slope, function(X) FUN=60*X)) #calculates slope as degrees/h from degrees/min

lar_p2slope<-slopeEveryN(lar_p2$T_internal,n=10) #makes the slope with respect to the index, NOT time. In this case time=index because data was recorded once per minute, but in other ones I will have to multiply the slopes by 3 to get degrees per minute instead of per 20 seconds. 
lar_p2slope<-data.frame(lar_p2slope)
lar_p2slope$DegreesHour<-unlist(lapply(X=lar_p2slope$Slope, function(X) FUN=60*X)) #calculates slope as degrees/h from degrees/min

#now identify the sections that have greater than 0.5C/h slope
lar_p1slope$Sample[abs(lar_p1slope$DegreesHour)>0.5] #1170 to 1210 is over 0.5C/h
lar_p2slope$Sample[abs(lar_p2slope$DegreesHour)>0.5] #1170 to 1230 and 1320 to 1330 are over 0.5C/h




#Trim the unusable parts from the original datasets for calc_mo2() analysis
#Plate 1
lar_p1orig$A2[570:613]<-NA
lar_p1orig$A4[1370:1609]<-NA
lar_p1orig$A5[301:450]<-NA
lar_p1orig$B1[1070:1310]<-NA
lar_p1orig$B5[510:577]<-NA
lar_p1orig$C1[652:786]<-NA
lar_p1orig$C3[712:951]<-NA
lar_p1orig$C5[241:301]<-NA
lar_p1orig$C6[540:613]<-NA
lar_p1orig$D2[540:613]<-NA
lar_p1orig$D2[742:801]<-NA
lar_p1orig$D5[652:786]<-NA
lar_p1orig$A1[712:891]<-NA
lar_p1orig$B3[2027:2176]<-NA
lar_p1orig$B6[1698:1818]<-NA

#Plate 2
lar_p2orig$A3[570:712]<-NA
lar_p2orig$A5[450:592]<-NA
lar_p2orig$B1[831:1100]<-NA
lar_p2orig$B2[1220:1489]<-NA
lar_p2orig$B6[1370:1609]<-NA
lar_p2orig$D4[613:771]<-NA
lar_p2orig$A2[682:801]<-NA
lar_p2orig$A4[1848:1967]<-NA
lar_p2orig$C1[331:391]<-NA
lar_p2orig$D2[211:301]<-NA
lar_p2orig$D3[241:301]<-NA
lar_p2orig$D6[241:301]<-NA





#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
#P1A1<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A1",times=30),rep("t2",times=30),rep("blankt2",times=30))
P1A2<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A2",times=30),rep("t2",times=30),rep("med",times=30))
P1A3<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A3",times=30),rep("t2",times=30),rep("med",times=30))
P1A4<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A4",times=30),rep("t2",times=30),rep("med",times=30))
P1A5<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A5",times=30),rep("t2",times=30),rep("med",times=30))
#P1A6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A6",times=30),rep("t2",times=30),rep("blankt2",times=30))
P1B1<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B1",times=30),rep("t8",times=30),rep("amb",times=30))
P1B2<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B2",times=30),rep("t8",times=30),rep("amb",times=30))
#P1B3<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B3",times=30),rep("t8",times=30),rep("blankt8",times=30))
P1B4<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B4",times=30),rep("t8",times=30),rep("amb",times=30))
P1B5<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B5",times=30),rep("t8",times=30),rep("amb",times=30))
#P1B6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B6",times=30),rep("t8",times=30),rep("blankt8",times=30))
P1C1<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C1",times=30),rep("t7",times=30),rep("high",times=30))
#P1C2<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C2",times=30),rep("t7",times=30),rep("blankt7",times=30))
P1C3<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C3",times=30),rep("t7",times=30),rep("high",times=30))
#P1C4<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C4",times=30),rep("t7",times=30),rep("blankt7",times=30))
P1C5<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C5",times=30),rep("t7",times=30),rep("high",times=30))
P1C6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C6",times=30),rep("t7",times=30),rep("high",times=30))
#P1D1<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D1",times=30),rep("t1",times=30),rep("blankt1",times=30))
P1D2<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D2",times=30),rep("t1",times=30),rep("amb",times=30))
#P1D3<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D3",times=30),rep("t1",times=30),rep("blankt1",times=30))
P1D4<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D4",times=30),rep("t1",times=30),rep("amb",times=30))
P1D5<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D5",times=30),rep("t1",times=30),rep("amb",times=30))
#P1D6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D6",times=30),rep("t3",times=30),rep("high",times=30))

P2A1<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A1",times=30),rep("t4",times=30),rep("med",times=30))
#P2A2<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A2",times=30),rep("t4",times=30),rep("blankt4",times=30))
P2A3<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A3",times=30),rep("t4",times=30),rep("med",times=30))
#P2A4<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A4",times=30),rep("t4",times=30),rep("blankt4",times=30))
P2A5<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A5",times=30),rep("t4",times=30),rep("med",times=30))
#P2A6<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A6",times=30),rep("t3",times=30),rep("blankt3",times=30))
P2B1<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B1",times=30),rep("t9",times=30),rep("high",times=30))
P2B2<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B2",times=30),rep("t9",times=30),rep("high",times=30))
#P2B3<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B3",times=30),rep("t9",times=30),rep("blankt9",times=30))
P2B4<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B4",times=30),rep("t9",times=30),rep("high",times=30))
#P2B5<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B5",times=30),rep("t9",times=30),rep("blankt9",times=30))
P2B6<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B6",times=30),rep("t3",times=30),rep("high",times=30))
#P2C1<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C1",times=30),rep("t6",times=30),rep("blankt6",times=30))
#P2C2<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C2",times=30),rep("t6",times=30),rep("blankt6",times=30))
P2C3<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C3",times=30),rep("t6",times=30),rep("amb",times=30))
P2C4<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C4",times=30),rep("t6",times=30),rep("amb",times=30))
P2C5<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C5",times=30),rep("t6",times=30),rep("amb",times=30))
P2C6<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C6",times=30),rep("t3",times=30),rep("high",times=30))
P2D1<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D1,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D1",times=30),rep("t5",times=30),rep("med",times=30))
#P2D2<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D2,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D2",times=30),rep("t5",times=30),rep("blankt5",times=30))
#P2D3<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D3,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D3",times=30),rep("t5",times=30),rep("blankt5",times=30))
P2D4<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D4,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D4",times=30),rep("t5",times=30),rep("med",times=30))
P2D5<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D5,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D5",times=30),rep("t5",times=30),rep("med5",times=30))
#P2D6<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D6,o2_unit="mg_per_l",bin_width=760/30,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D6",times=30),rep("t3",times=30),rep("blankt3",times=30))

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
P1A2$dw<-rep(0.07123573,times=30)
P1A3$dw<-rep(0.12975554,times=30)
P1A4$dw<-rep(0.05992811,times=30)
P1A5$dw<-rep(0.08986283,times=30)
P1B1$dw<-rep(0.08148923,times=30)
P1B2$dw<-rep(0.10368688,times=30)
P1B4$dw<-rep(0.06112130,times=30)
P1B5$dw<-rep(0.06804286,times=30)
P1C1$dw<-rep(0.05410562,times=30)
P1C3$dw<-rep(0.04147655,times=30)
P1C5$dw<-rep(0.08680508,times=30)
P1C6$dw<-rep(0.08896861,times=30)
P1D2$dw<-rep(0.07113832,times=30)
P1D4$dw<-rep(0.09459529,times=30)
P1D5$dw<-rep(0.08894588,times=30)
P2A1$dw<-rep(0.06974729,times=30)
P2A3$dw<-rep(0.06576484,times=30)
P2A5$dw<-rep(0.05682256,times=30)
P2B1$dw<-rep(0.07687877,times=30)
P2B2$dw<-rep(0.08655282,times=30)
P2B4$dw<-rep(0.10198861,times=30)
P2B6$dw<-rep(0.06243424,times=30)
P2C3$dw<-rep(0.05377564,times=30)
P2C4$dw<-rep(0.06965129,times=30)
P2C5$dw<-rep(0.05939634,times=30)
P2C6$dw<-rep(0.05756485,times=30)
P2D1$dw<-rep(0.07261044,times=30)
P2D4$dw<-rep(0.08295252,times=30)
P2D5$dw<-rep(0.09544944,times=30)

P1A2$msmrs<-P1A2$MO2/P1A2$dw
P1A3$msmrs<-P1A3$MO2/P1A3$dw
P1A4$msmrs<-P1A4$MO2/P1A4$dw
P1A5$msmrs<-P1A5$MO2/P1A5$dw
P1B1$msmrs<-P1B1$MO2/P1B1$dw
P1B2$msmrs<-P1B2$MO2/P1B2$dw
P1B4$msmrs<-P1B4$MO2/P1B4$dw
P1B5$msmrs<-P1B5$MO2/P1B5$dw
P1C1$msmrs<-P1C1$MO2/P1C1$dw
P1C3$msmrs<-P1C3$MO2/P1C3$dw
P1C5$msmrs<-P1C5$MO2/P1C5$dw
P1C6$msmrs<-P1C6$MO2/P1C6$dw
P1D2$msmrs<-P1D2$MO2/P1D2$dw
P1D4$msmrs<-P1D4$MO2/P1D4$dw
P1D5$msmrs<-P1D5$MO2/P1D5$dw
P2A1$msmrs<-P2A1$MO2/P2A1$dw
P2A3$msmrs<-P2A3$MO2/P2A3$dw
P2A5$msmrs<-P2A5$MO2/P2A5$dw
P2B1$msmrs<-P2B1$MO2/P2B1$dw
P2B2$msmrs<-P2B2$MO2/P2B2$dw
P2B4$msmrs<-P2B4$MO2/P2B4$dw
P2B6$msmrs<-P2B6$MO2/P2B6$dw
P2C3$msmrs<-P2C3$MO2/P2C3$dw
P2C4$msmrs<-P2C4$MO2/P2C4$dw
P2C5$msmrs<-P2C5$MO2/P2C5$dw
P2C6$msmrs<-P2C6$MO2/P2C6$dw
P2D1$msmrs<-P2D1$MO2/P2D1$dw
P2D4$msmrs<-P2D4$MO2/P2D4$dw
P2D5$msmrs<-P2D5$MO2/P2D5$dw


#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES?? Fit a line to all of the points from the same treatment/tank. 
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
alllar<-rbind(P1A2,P1A3,P1A4,P1A5,P1B1,P1B2,P1B4,P1B5,P1C1,P1C3,P1C5,P1C6,P1D2,P1D4,P1D5,
              P2A1,P2A3,P2A5,P2B1,P2B2,P2B4,P2B6,P2C3,P2C4,P2C5,P2C6,P2D1,P2D4,P2D5)

#plot the curves
library(ggplot2)
allplotlar<-ggplot(alllar, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlar)

#Try plotting one at a time, grouped by treatment
library(gridExtra)
#Ambient: P1B1, P1B2, P1B4, P1B5, P1D2, P1D4, P1D5, P2C3, P2C4, P2C5
P1B1plot<-ggplot(P1B1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B2plot<-ggplot(P1B2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B4plot<-ggplot(P1B4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B5plot<-ggplot(P1B5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D2plot<-ggplot(P1D2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D4plot<-ggplot(P1D4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D5plot<-ggplot(P1D5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C3plot<-ggplot(P2C3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C4plot<-ggplot(P2C4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C5plot<-ggplot(P2C5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1B1plot,P1B2plot,P1B4plot,P1B5plot,P1D2plot,P1D4plot,P1D5plot,P2C3plot,P2C4plot,P2C5plot,ncol=5)

#Medium: P1A2, P1A3, P1A4, P1A5, P2A1, P2A3, P2A5, P2D1, P2D4, P2D5
P1A2plot<-ggplot(P1A2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A3plot<-ggplot(P1A3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A4plot<-ggplot(P1A4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A5plot<-ggplot(P1A5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A1plot<-ggplot(P2A1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A3plot<-ggplot(P2A3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A5plot<-ggplot(P2A5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D1plot<-ggplot(P2D1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D4plot<-ggplot(P2D4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D5plot<-ggplot(P2D5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1A2plot,P1A3plot,P1A4plot,P1A5plot,P2A1plot,P2A3plot,P2A5plot,P2D1plot,P2D4plot,P2D5plot,ncol=5)

#High: P1C1, P1C3, P1C5, P1C6, P2B1, P2B2, P2B4, P2B6, P2C6
P1C1plot<-ggplot(P1C1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C3plot<-ggplot(P1C3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C5plot<-ggplot(P1C5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C6plot<-ggplot(P1C6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B1plot<-ggplot(P2B1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B2plot<-ggplot(P2B2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B4plot<-ggplot(P2B4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B6plot<-ggplot(P2B6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C6plot<-ggplot(P2C6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1C1plot,P1C3plot,P1C5plot,P1C6plot,P2B1plot,P2B2plot,P2B4plot,P2B6plot,P2C6plot,ncol=3)



#calculate pcrit and add it as a column in the lar_flax dataframe. 


lar_dana$Pcrit_alpha<-c(calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['Alpha'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Alpha'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Alpha'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['Alpha'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Alpha'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Alpha'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Alpha'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$msmrs)['Alpha'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Alpha'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Alpha'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Alpha'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['Alpha'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Alpha'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Alpha'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Alpha'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$msmrs)['Alpha'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['Alpha'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Alpha'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['Alpha'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Alpha'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Alpha'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Alpha'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Alpha'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Alpha'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Alpha'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Alpha'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Alpha'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Alpha'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Alpha'])
lar_dana$Pcrit_break<-c(calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['Breakpoint'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Breakpoint'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Breakpoint'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['Breakpoint'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Breakpoint'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Breakpoint'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Breakpoint'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$msmrs)['Breakpoint'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Breakpoint'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Breakpoint'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Breakpoint'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['Breakpoint'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Breakpoint'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Breakpoint'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Breakpoint'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$msmrs)['Breakpoint'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['Breakpoint'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Breakpoint'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['Breakpoint'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Breakpoint'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Breakpoint'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Breakpoint'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Breakpoint'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Breakpoint'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Breakpoint'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Breakpoint'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Breakpoint'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Breakpoint'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Breakpoint'])
lar_dana$Pcrit_subPI<-c(calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['Sub_PI'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Sub_PI'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Sub_PI'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['Sub_PI'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Sub_PI'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Sub_PI'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Sub_PI'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$msmrs)['Sub_PI'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Sub_PI'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Sub_PI'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Sub_PI'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['Sub_PI'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Sub_PI'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Sub_PI'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Sub_PI'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$msmrs)['Sub_PI'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['Sub_PI'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Sub_PI'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['Sub_PI'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Sub_PI'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Sub_PI'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Sub_PI'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Sub_PI'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Sub_PI'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Sub_PI'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Sub_PI'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Sub_PI'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Sub_PI'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Sub_PI'])
lar_dana$Pcrit_NLR<-c(calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['NLR'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['NLR'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['NLR'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$msmrs)['NLR'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['NLR'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['NLR'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['NLR'],
                        calc_pcrit(P1B5$O2_MEAN,P1B5$msmrs)['NLR'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['NLR'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['NLR'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['NLR'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$msmrs)['NLR'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['NLR'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['NLR'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['NLR'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$msmrs)['NLR'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$msmrs)['NLR'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['NLR'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['NLR'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['NLR'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['NLR'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['NLR'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['NLR'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['NLR'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['NLR'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['NLR'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['NLR'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['NLR'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['NLR'])
lar_dana$alpha<-c(calc_alpha(P1A2$O2_MEAN,P1A2$msmrs)$alpha,
                        calc_alpha(P1A3$O2_MEAN,P1A3$msmrs)$alpha,
                        calc_alpha(P1A4$O2_MEAN,P1A4$msmrs)$alpha,
                        calc_alpha(P1A5$O2_MEAN,P1A5$msmrs)$alpha,
                        calc_alpha(P1B1$O2_MEAN,P1B1$msmrs)$alpha,
                        calc_alpha(P1B2$O2_MEAN,P1B2$msmrs)$alpha,
                        calc_alpha(P1B4$O2_MEAN,P1B4$msmrs)$alpha,
                        calc_alpha(P1B5$O2_MEAN,P1B5$msmrs)$alpha,
                        calc_alpha(P1C1$O2_MEAN,P1C1$msmrs)$alpha,
                        calc_alpha(P1C3$O2_MEAN,P1C3$msmrs)$alpha,
                        calc_alpha(P1C5$O2_MEAN,P1C5$msmrs)$alpha,
                        calc_alpha(P1C6$O2_MEAN,P1C6$msmrs)$alpha,
                        calc_alpha(P1D2$O2_MEAN,P1D2$msmrs)$alpha,
                        calc_alpha(P1D4$O2_MEAN,P1D4$msmrs)$alpha,
                        calc_alpha(P1D5$O2_MEAN,P1D5$msmrs)$alpha,
                        calc_alpha(P2A1$O2_MEAN,P2A1$msmrs)$alpha,
                        calc_alpha(P2A3$O2_MEAN,P2A3$msmrs)$alpha,
                        calc_alpha(P2A5$O2_MEAN,P2A5$msmrs)$alpha,
                        calc_alpha(P2B1$O2_MEAN,P2B1$msmrs)$alpha,
                        calc_alpha(P2B2$O2_MEAN,P2B2$msmrs)$alpha,
                        calc_alpha(P2B4$O2_MEAN,P2B4$msmrs)$alpha,
                        calc_alpha(P2B6$O2_MEAN,P2B6$msmrs)$alpha,
                        calc_alpha(P2C3$O2_MEAN,P2C3$msmrs)$alpha,
                        calc_alpha(P2C4$O2_MEAN,P2C4$msmrs)$alpha,
                        calc_alpha(P2C5$O2_MEAN,P2C5$msmrs)$alpha,
                        calc_alpha(P2C6$O2_MEAN,P2C6$msmrs)$alpha,
                        calc_alpha(P2D1$O2_MEAN,P2D1$msmrs)$alpha,
                        calc_alpha(P2D4$O2_MEAN,P2D4$msmrs)$alpha,
                        calc_alpha(P2D5$O2_MEAN,P2D5$msmrs)$alpha)



#plot pcrit to look for probs
plot_pcrit(P1A2$O2_MEAN,P1A2$msmrs)
plot_pcrit(P1A3$O2_MEAN,P1A3$msmrs)
plot_pcrit(P1A4$O2_MEAN,P1A4$msmrs)
plot_pcrit(P1A5$O2_MEAN,P1A5$msmrs)
plot_pcrit(P1B1$O2_MEAN,P1B1$msmrs)
plot_pcrit(P1B2$O2_MEAN,P1B2$msmrs)
plot_pcrit(P1B4$O2_MEAN,P1B4$msmrs)
plot_pcrit(P1B5$O2_MEAN,P1B5$msmrs)
plot_pcrit(P1C1$O2_MEAN,P1C1$msmrs)

plot_pcrit(P1C3$O2_MEAN,P1C3$msmrs)
plot_pcrit(P1C5$O2_MEAN,P1C5$msmrs)
plot_pcrit(P1C6$O2_MEAN,P1C6$msmrs)
plot_pcrit(P1D2$O2_MEAN,P1D2$msmrs)
plot_pcrit(P1D4$O2_MEAN,P1D4$msmrs)
plot_pcrit(P1D5$O2_MEAN,P1D5$msmrs)
plot_pcrit(P2A1$O2_MEAN,P2A1$msmrs)
plot_pcrit(P2A3$O2_MEAN,P2A3$msmrs)
plot_pcrit(P2A5$O2_MEAN,P2A5$msmrs)

plot_pcrit(P2B1$O2_MEAN,P2B1$msmrs)
plot_pcrit(P2B2$O2_MEAN,P2B2$msmrs)
plot_pcrit(P2B4$O2_MEAN,P2B4$msmrs)
plot_pcrit(P2B6$O2_MEAN,P2B6$msmrs)
plot_pcrit(P2C3$O2_MEAN,P2C3$msmrs)
plot_pcrit(P2C4$O2_MEAN,P2C4$msmrs)
plot_pcrit(P2C5$O2_MEAN,P2C5$msmrs)
plot_pcrit(P2C6$O2_MEAN,P2C6$msmrs)

plot_pcrit(P2D1$O2_MEAN,P2D1$msmrs)
plot_pcrit(P2D4$O2_MEAN,P2D4$msmrs)
plot_pcrit(P2D5$O2_MEAN,P2D5$msmrs)

#Analyze and visualize with respect to CO2 treatments

#set levels of CO2_level and Tank
lar_dana$CO2_level<-factor(lar_dana$CO2_level,levels=c("amb","med","high"))
lar_dana$Tank<-factor(lar_dana$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))

plot(lar_dana$Pcrit_alpha~lar_dana$CO2_level)
plot(lar_dana$Pcrit_break~lar_dana$CO2_level)

#check if different Pcrit metrics are correlated
plot(lar_dana$Pcrit_alpha~lar_dana$Pcrit_break) #kind of a positive correlation but not strong line shape at all

#calculate mean and SE of Pcrit values by treatment
library(plyr)
alpha_sum<-ddply(lar_dana,"CO2_level",summarise,N=length(Pcrit_alpha),MeanPcrit=mean(Pcrit_alpha,na.rm=TRUE),SE=sd(Pcrit_alpha,na.rm=TRUE)/sqrt(N))
alpha_sum

break_sum<-ddply(lar_dana,"CO2_level",summarise,N=length(Pcrit_break),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers

a_sum<-ddply(lar_dana,"CO2_level",summarise,N=length(alpha),MeanAlpha=mean(alpha,na.rm=TRUE),SE=sd(alpha,na.rm=TRUE)/sqrt(N))
a_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

alpha_mod<-lmer(Pcrit_alpha~CO2_level+1|Tank,data=lar_dana) #some warning messages
ranova(alpha_mod) #P-value is very high so don't need tank effect

alpha_mod1<-lm(Pcrit_alpha~CO2_level,data=lar_dana)
anova(alpha_mod1) #not significant, p=0.2338

break_mod<-lmer(Pcrit_break~CO2_level+1|Tank,data=lar_dana) #singular fit
ranova(break_mod)

break_mod1<-lm(Pcrit_break~CO2_level,data=lar_dana)
anova(break_mod1) #ditto, p=0.6312

a_mod1<-lm(alpha~CO2_level,data=lar_dana)
anova(a_mod1)

#are residuals normally distributed
res_alpha<-residuals(alpha_mod1)
hist(res_alpha)
res_break<-residuals(break_mod1)
hist(res_break)
#pretty normal but not great

par(mfrow=c(2,2))
plot(alpha_mod1)
plot(break_mod1)

#use statistical tests
library(olsrr)

ols_test_normality(alpha_mod1) #S-W, K-S, and A-D tests passed p>0.05
ols_test_normality(break_mod1) #same tests passed p>0.05
ols_test_bartlett(data=lar_dana,'Pcrit_alpha',group_var='CO2_level') #passed p>0.05
ols_test_bartlett(data=lar_dana,'Pcrit_break',group_var='CO2_level') #passed p>0.05

#the data meet the assumptions of the model :)



