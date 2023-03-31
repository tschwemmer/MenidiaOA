#Respirometry analysis from 2021 experiment

#Run 5: Larvae, Dana Hall, 6-20-21

#load data sheets and treatments
lrv_p1orig<-read.csv(file.choose(),header=TRUE)
lrv_p2orig<-read.csv(file.choose(),header=TRUE)
lrv_trmt_p1<-read.csv(file.choose(),header=TRUE)
lrv_trmt_p2<-read.csv(file.choose(),header=TRUE)

#remove first 85 minutes from Plate 1, first 30 minutes from Plate 2. Then renumber the rows.
lrv_p1<-lrv_p1orig
lrv_p2<-lrv_p2orig
row.names(lrv_p1)<-NULL
row.names(lrv_p2)<-NULL

#check structure
str(lrv_p1)
str(lrv_p2)
str(lrv_trmt_p1)
str(lrv_trmt_p2)

#make the treatment variables factors
lrv_trmt_p1$Well<-factor(lrv_trmt_p1$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
lrv_trmt_p2$Well<-factor(lrv_trmt_p2$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
lrv_trmt_p1$CO2_level<-factor(lrv_trmt_p1$CO2_level,levels=c("amb","med","high","blankamb","blankmed","blankhigh"))
lrv_trmt_p2$CO2_level<-factor(lrv_trmt_p2$CO2_level,levels=c("amb","med","high","blankamb","blankmed","blankhigh"))
lrv_trmt_p1$Tank<-factor(lrv_trmt_p1$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))
lrv_trmt_p2$Tank<-factor(lrv_trmt_p2$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))


#calculate slope for each well using all untrimmed data. Units will be umol consumed per hour
slopes_r5p1<-data.frame(names(lrv_p1)[4:27], sapply(lrv_p1[4:27],function(x) ((-coef(summary(lm(x~lrv_p1$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(lrv_p1[4:27],function(x) summary(lm(x~lrv_p1$Time.Sec.))$r.squared))
names(slopes_r5p1)<-c("Well","MO2","Rsquared")
slopes_r5p2<-data.frame(names(lrv_p2)[4:27], sapply(lrv_p2[4:27],function(x) ((-coef(summary(lm(x~lrv_p2$Time.Sec.)))[2])/31.9988)*1800),
                        sapply(lrv_p2[4:27],function(x) summary(lm(x~lrv_p2$Time.Sec.))$r.squared))
names(slopes_r5p2)<-c("Well","MO2","Rsquared")

#plot data for all live wells
par(mfrow=c(2,4))
#Plate1
plot(lrv_p1$A1~lrv_p1$Time.Min.)
plot(lrv_p1$A3~lrv_p1$Time.Min.)
plot(lrv_p1$A4~lrv_p1$Time.Min.)
plot(lrv_p1$B2~lrv_p1$Time.Min.)
plot(lrv_p1$B3~lrv_p1$Time.Min.)
plot(lrv_p1$B4~lrv_p1$Time.Min.)
plot(lrv_p1$B6~lrv_p1$Time.Min.)
plot(lrv_p1$C1~lrv_p1$Time.Min.)
plot(lrv_p1$C3~lrv_p1$Time.Min.)
plot(lrv_p1$C5~lrv_p1$Time.Min.)
plot(lrv_p1$D1~lrv_p1$Time.Min.)
plot(lrv_p1$D2~lrv_p1$Time.Min.)
plot(lrv_p1$D4~lrv_p1$Time.Min.)
plot(lrv_p1$D5~lrv_p1$Time.Min.)
#blanks
plot(lrv_p1$A2~lrv_p1$Time.Min.)
plot(lrv_p1$A5~lrv_p1$Time.Min.)
plot(lrv_p1$A6~lrv_p1$Time.Min.)
plot(lrv_p1$B1~lrv_p1$Time.Min.)
plot(lrv_p1$B5~lrv_p1$Time.Min.)
plot(lrv_p1$C2~lrv_p1$Time.Min.)
plot(lrv_p1$C4~lrv_p1$Time.Min.)
plot(lrv_p1$C6~lrv_p1$Time.Min.)
plot(lrv_p1$D3~lrv_p1$Time.Min.)
plot(lrv_p1$D6~lrv_p1$Time.Min.)
#Plate2
plot(lrv_p2$A2~lrv_p2$Time.Min.)
plot(lrv_p2$A4~lrv_p2$Time.Min.)
plot(lrv_p2$A5~lrv_p2$Time.Min.)
plot(lrv_p2$A6~lrv_p2$Time.Min.)
plot(lrv_p2$B1~lrv_p2$Time.Min.)
plot(lrv_p2$B2~lrv_p2$Time.Min.)
plot(lrv_p2$B3~lrv_p2$Time.Min.)
plot(lrv_p2$B4~lrv_p2$Time.Min.)
plot(lrv_p2$C1~lrv_p2$Time.Min.)
plot(lrv_p2$C3~lrv_p2$Time.Min.)
plot(lrv_p2$C4~lrv_p2$Time.Min.)
plot(lrv_p2$C6~lrv_p2$Time.Min.)
plot(lrv_p2$D1~lrv_p2$Time.Min.)
plot(lrv_p2$D3~lrv_p2$Time.Min.)
plot(lrv_p2$D4~lrv_p2$Time.Min.)
#Blanks
par(mfrow=c(3,3))
plot(lrv_p2$A1~lrv_p2$Time.Min.)
plot(lrv_p2$A3~lrv_p2$Time.Min.)
plot(lrv_p2$B5~lrv_p2$Time.Min.)
plot(lrv_p2$B6~lrv_p2$Time.Min.)
plot(lrv_p2$C2~lrv_p2$Time.Min.)
plot(lrv_p2$C5~lrv_p2$Time.Min.)
plot(lrv_p2$D2~lrv_p2$Time.Min.)
plot(lrv_p2$D5~lrv_p2$Time.Min.)
plot(lrv_p2$D6~lrv_p2$Time.Min.)

#Trim the data for the purpose of calculating the initial slope (RMR in fully oxygenated water)
lrv_p1_rmr<-data.frame(lrv_p1$Time.Min.[46:136], lrv_p1$Time.Sec.[46:136],lrv_p1$A1[151:241],lrv_p1$A3[46:136],lrv_p1$A4[46:136],
                       lrv_p1$B2[226:316],lrv_p1$B3[46:136],lrv_p1$B6[46:136],
                       lrv_p1$C1[46:136],lrv_p1$C3[46:136],lrv_p1$C5[46:136],
                       lrv_p1$D1[390:480],lrv_p1$D2[46:136],lrv_p1$D4[46:136],lrv_p1$D5[46:136],
                       lrv_p1$A2[46:136],lrv_p1$A5[46:136],lrv_p1$A6[46:136],lrv_p1$B1[46:136],lrv_p1$B5[46:136],lrv_p1$C2[46:136],
                       lrv_p1$C4[46:136],lrv_p1$C6[46:136],lrv_p1$D6[46:136])
names(lrv_p1_rmr)<-c("Time.Min.","Time.Sec.","A1","A3","A4","B2","B3","B6","C1","C3","C5","D1","D2","D4","D5","A2","A5","A6","B1","B5","C2","C4","C6","D6")

lrv_p2_rmr<-data.frame(lrv_p2$Time.Min.[46:136], lrv_p2$Time.Sec.[46:136], lrv_p2$A2[181:271],lrv_p2$A4[46:136],lrv_p2$A5[46:136],lrv_p2$A6[46:136],
                       lrv_p2$B1[46:136],lrv_p2$B2[46:136],lrv_p2$B3[46:136],lrv_p2$B4[46:136],
                       lrv_p2$C1[46:136],lrv_p2$C3[46:136],lrv_p2$C4[91:181],
                       lrv_p2$D1[46:136],lrv_p2$D3[46:136],lrv_p2$D4[46:136],
                       lrv_p2$A1[46:136],lrv_p2$A3[46:136],lrv_p2$B5[46:136],lrv_p2$B6[241:331],lrv_p2$C2[46:136],
                       lrv_p2$C5[46:136],lrv_p2$D2[46:136],lrv_p2$D5[46:136],lrv_p2$D6[556:646])
names(lrv_p2_rmr)<-c("Time.Min.","Time.Sec.","A2","A4","A5","A6","B1","B2","B3","B4","C1","C3","C4","D1","D3","D4","A1","A3","B5","B6","C2","C5","D2","D5","D6")


slopes_r5p1_rmr<-data.frame(names(lrv_p1_rmr)[3:24], sapply(lrv_p1_rmr[3:24],function(x) ((-coef(summary(lm(x~lrv_p1_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lrv_p1_rmr[3:24],function(x) summary(lm(x~lrv_p1_rmr$Time.Sec.))$r.squared))
names(slopes_r5p1_rmr)<-c("Well","MO2","Rsquared")

slopes_r5p2_rmr<-data.frame(names(lrv_p2_rmr)[3:24], sapply(lrv_p2_rmr[3:24],function(x) ((-coef(summary(lm(x~lrv_p2_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lrv_p2_rmr[3:24],function(x) summary(lm(x~lrv_p2_rmr$Time.Sec.))$r.squared))
names(slopes_r5p2_rmr)<-c("Well","MO2","Rsquared")



#calculate the blanks
t1blank<-mean(slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="C2"],slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="C4"])
t2blank<-mean(slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="A1"],slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="A3"])
t3blank<-mean(slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="A2"],slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="A5"])
t4blank<-mean(slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="A6"],slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="C6"])
t5blank<-mean(slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="D2"],slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="D5"])
t6blank<-mean(slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="C2"],slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="C5"])
t7blank<-slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="D6"]
t8blank<-mean(slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="B5"],slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="B6"])
t9blank<-mean(slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="B1"],slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="B5"])


#Subtract the blanks
lrv_p1b<-data.frame(c("A1","A3","A4","B2","B3","B6","C1","C3","C5","D1","D2","D4","D5"),
                    c(slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="A1"]-t3blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="A3"]-t3blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="A4"]-t3blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="B2"]-t9blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="B3"]-t9blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="B6"]-t4blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="C1"]-t1blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="C3"]-t1blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="C5"]-t1blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="D1"]-t7blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="D2"]-t7blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="D4"]-t7blank,
                      slopes_r5p1_rmr$MO2[slopes_r5p1_rmr$Well=="D5"]-t7blank))
names(lrv_p1b)<-c("Well","MO2")
lrv_p2b<-data.frame(c("A2","A4","A5","A6","B1","B2","B3","B4","C1","C3","C4","D1","D3","D4"),
                    c(slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="A2"]-t2blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="A4"]-t2blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="A5"]-t5blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="A6"]-t5blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="B1"]-t8blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="B2"]-t8blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="B3"]-t8blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="B4"]-t8blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="C1"]-t6blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="C3"]-t6blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="C4"]-t6blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="D1"]-t5blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="D3"]-t5blank,
                      slopes_r5p2_rmr$MO2[slopes_r5p2_rmr$Well=="D4"]-t5blank))
names(lrv_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataset and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
lrv_p1b<-dplyr::full_join(lrv_p1b,lrv_trmt_p1,by="Well")
lrv_p1b<-lrv_p1b[1:13,] #remove the blanks
lrv_p2b<-dplyr::full_join(lrv_p2b,lrv_trmt_p2,by="Well")
lrv_p2b<-lrv_p2b[1:14,] #remove the blanks
lrv_dana<-rbind(lrv_p1b,lrv_p2b) #combine the two plates into one dataset. 
row.names(lrv_dana)<-NULL #renumber the rows so it's easy to index if need be.

#Add a column for mass-specific metabolic rate, MO2/dw in umol O2 / mg dw / h. 
lrv_dana<-data.frame(lrv_dana, "msmr"<-c(lrv_dana[1,2]/lrv_dana[1,5],
                                         lrv_dana[2,2]/lrv_dana[2,5],
                                         lrv_dana[3,2]/lrv_dana[3,5],
                                         lrv_dana[4,2]/lrv_dana[4,5],
                                         lrv_dana[5,2]/lrv_dana[5,5],
                                         lrv_dana[6,2]/lrv_dana[6,5],
                                         lrv_dana[7,2]/lrv_dana[7,5],
                                         lrv_dana[8,2]/lrv_dana[8,5],
                                         lrv_dana[9,2]/lrv_dana[9,5],
                                         lrv_dana[10,2]/lrv_dana[10,5],
                                         lrv_dana[11,2]/lrv_dana[11,5],
                                         lrv_dana[12,2]/lrv_dana[12,5],
                                         lrv_dana[13,2]/lrv_dana[13,5],
                                         lrv_dana[14,2]/lrv_dana[14,5],
                                         lrv_dana[15,2]/lrv_dana[15,5],
                                         lrv_dana[16,2]/lrv_dana[16,5],
                                         lrv_dana[17,2]/lrv_dana[17,5],
                                         lrv_dana[18,2]/lrv_dana[18,5],
                                         lrv_dana[19,2]/lrv_dana[19,5],
                                         lrv_dana[20,2]/lrv_dana[20,5],
                                         lrv_dana[21,2]/lrv_dana[21,5],
                                         lrv_dana[22,2]/lrv_dana[22,5],
                                         lrv_dana[23,2]/lrv_dana[23,5],
                                         lrv_dana[24,2]/lrv_dana[24,5],
                                         lrv_dana[25,2]/lrv_dana[25,5],
                                         lrv_dana[26,2]/lrv_dana[26,5],
                                         lrv_dana[27,2]/lrv_dana[27,5]))
names(lrv_dana)[7]<-"msmr"

#analyze the MO2 with respect to CO2
dana_lrv_model<-lm(lrv_dana$msmr~lrv_dana$CO2_level)
anova(dana_lrv_model) #CO2 level is not significant, p=0.5843

#do it with a LME/ANOVA with tank as random effect
library(lme4)
library(lmerTest)
dana_lrv_mod<-lmer(msmr~Tank+(1|CO2_level),data=lrv_dana) #This gave a weird error message I don't understand. 
anova(dana_lrv_mod)
ranova(dana_lrv_mod) #p=1 this confirms that the random effect can be excluded. 

#try it the other way
dana_lrv_mdl<-aov(lrv_dana$msmr~lrv_dana$CO2_level/factor(lrv_dana$Tank))
summary(dana_lrv_mdl) #still not significant

#diagnostics


#calculate the group means 
library(plyr)
dana_lrv_sum<-ddply(lrv_dana,"CO2_level",summarise,N=length(msmr),MeanMO2=mean(msmr),SE=sd(msmr)/sqrt(N))
dana_lrv_sum #elevated CO2 slightly decreases MO2...opposite of previous results. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
danalrvplot<-ggplot(dana_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  theme_classic()
print(danalrvplot)
  
##########################################################################################################
#remove first 10 minutes and parts with extremely high temperature change
#first check temperature in 10 min bins

#remove first 10 minutes and parts with extremely high temperature change
#first check temperature in 10 min bins
library(tibble)
library(zoo)

#first remove 10 minutes from start
lrv_p1orig<-lrv_p1[31:2640,]
lrv_p2orig<-lrv_p2[31:2631,]
row.names(lrv_p1orig)<-NULL
row.names(lrv_p2orig)<-NULL

lrv_p1temp<-data.frame(lrv_p1orig$T_internal,lrv_p1orig$Time.Min.)
names(lrv_p1temp)<-c("T_internal","Time.Min.")
Coef<-function(Z) coef(lm(T_internal~Time.Min., as.data.frame(Z)))[2]
lrv_p1slope<-rollapplyr(zoo(lrv_p1temp), width=30, Coef,by=30,by.column=FALSE, align="center")
lrv_p1slope1<-as.data.frame(lrv_p1slope)
lrv_p1slope1<-rownames_to_column(lrv_p1slope1, "Index")

lrv_p2temp<-data.frame(lrv_p2orig$T_internal,lrv_p2orig$Time.Min.)
names(lrv_p2temp)<-c("T_internal","Time.Min.")
lrv_p2slope<-rollapplyr(zoo(lrv_p2temp), width=30, Coef,by=30,by.column=FALSE, align="center")
lrv_p2slope1<-as.data.frame(lrv_p2slope)
lrv_p2slope1<-rownames_to_column(lrv_p2slope1, "Index")


#now identify the sections that have greater than 0.5C/h slope (0.0083C/min)
lrv_p1slope1$Index[abs(lrv_p1slope1$lrv_p1slope)>0.0083] #15, 45, 75 are too high, remove first 90 rows
lrv_p2slope1$Index[abs(lrv_p2slope1$lrv_p2slope)>0.0083] #15, 75 are too high, remove first 90 rows

lrv_p1orig<-lrv_p1orig[91:2610,]
lrv_p2orig<-lrv_p2orig[91:2601,]
row.names(lrv_p1orig)<-NULL
row.names(lrv_p2orig)<-NULL


#Trim the unusable parts from the original datasets for calc_mo2() analysis
#Need to subtract 120 from these indices since I deleted 120 rows total

#trim fluctuating parts out of only the ones that need it - parts that are due to temperature change mainly. 
#Plate 1
lrv_p1orig$A1[1:31]<-NA
lrv_p1orig$A1[330:445]<-NA
lrv_p1orig$A3[121:240]<-NA
lrv_p1orig$A4[31:61]<-NA
lrv_p1orig$B2[1:106]<-NA
lrv_p1orig$B2[240:300]<-NA
lrv_p1orig$B2[624:744]<-NA
lrv_p1orig$B6[121:624]<-NA
lrv_p1orig$C5[151:180]<-NA
lrv_p1orig$D1[1:270]<-NA
lrv_p1orig$D2[180:409]<-NA
lrv_p1orig$A2[1639:1699]<-NA
lrv_p1orig$C4[415]<-NA
lrv_p1orig$D6[654:744]<-NA
lrv_p1orig$D6[1192:1281]<-NA

#Plate 2
lrv_p2orig$A2[418:615]<-NA
lrv_p2orig$A4[974:1183]<-NA
lrv_p2orig$B4[407]<-NA
lrv_p2orig$C4[411:436]<-NA
lrv_p2orig$C4[675:884]<-NA
lrv_p2orig$C4[1779:2137]<-NA
lrv_p2orig$A1[395:436]<-NA
lrv_p2orig$A3[406:481]<-NA
lrv_p2orig$B6[31:106]<-NA
lrv_p2orig$D2[511:615]<-NA
lrv_p2orig$D6[225:300]<-NA


#Use make_bins() to set bin width but use different rows based on when it bottoms out. Use increments of 50. 
P1A1bin<-make_bins(o2=lrv_p1orig$A1[1:1908],duration=lrv_p1orig$Time.Min.[1:1908],max_o2_width=1/10,min_o2_width=1/30)
#P1A2bin<-make_bins(o2=lrv_p1orig$A2[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1A3bin<-make_bins(o2=lrv_p1orig$A3[1:1042],duration=lrv_p1orig$Time.Min.[1:1042],max_o2_width=1/10,min_o2_width=1/30)
P1A4bin<-make_bins(o2=lrv_p1orig$A4[1:1192],duration=lrv_p1orig$Time.Min.[1:1192],max_o2_width=1/10,min_o2_width=1/30)
#P1A5bin<-make_bins(o2=lrv_p1orig$A5[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1A6bin<-make_bins(o2=lrv_p1orig$A6[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1B1bin<-make_bins(o2=lrv_p1orig$B1[1:1231],duration=lrv_p1orig$Time.Min.[1:1231],max_o2_width=1/10,min_o2_width=1/30)
P1B2bin<-make_bins(o2=lrv_p1orig$B2[1:1341],duration=lrv_p1orig$Time.Min.[1:1341],max_o2_width=1/10,min_o2_width=1/30)
P1B3bin<-make_bins(o2=lrv_p1orig$B3[1:968],duration=lrv_p1orig$Time.Min.[1:968],max_o2_width=1/10,min_o2_width=1/30)
#P1B4bin<-make_bins(o2=lrv_p1orig$B4[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1B5bin<-make_bins(o2=lrv_p1orig$B5[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1B6bin<-make_bins(o2=lrv_p1orig$B6[1:1341],duration=lrv_p1orig$Time.Min.[1:1341],max_o2_width=1/10,min_o2_width=1/30)
P1C1bin<-make_bins(o2=lrv_p1orig$C1[1:1192],duration=lrv_p1orig$Time.Min.[1:1192],max_o2_width=1/10,min_o2_width=1/30)
#P1C2bin<-make_bins(o2=lrv_p1orig$C2[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1C3bin<-make_bins(o2=lrv_p1orig$C3[1:1565],duration=lrv_p1orig$Time.Min.[1:1565],max_o2_width=1/10,min_o2_width=1/30)
#P1C4bin<-make_bins(o2=lrv_p1orig$C4[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1C5bin<-make_bins(o2=lrv_p1orig$C5[1:1192],duration=lrv_p1orig$Time.Min.[1:1192],max_o2_width=1/10,min_o2_width=1/30)
#P1C6bin<-make_bins(o2=lrv_p1orig$C6[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D1bin<-make_bins(o2=lrv_p1orig$D1[1:1192],duration=lrv_p1orig$Time.Min.[1:1192],max_o2_width=1/10,min_o2_width=1/30)
P1D2bin<-make_bins(o2=lrv_p1orig$D2[1:1490],duration=lrv_p1orig$Time.Min.[1:1490],max_o2_width=1/10,min_o2_width=1/30)
#P1D3bin<-make_bins(o2=lrv_p1orig$D3[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D4bin<-make_bins(o2=lrv_p1orig$D4[1:1192],duration=lrv_p1orig$Time.Min.[1:1192],max_o2_width=1/10,min_o2_width=1/30)
P1D5bin<-make_bins(o2=lrv_p1orig$D5[1:1415],duration=lrv_p1orig$Time.Min.[1:1415],max_o2_width=1/10,min_o2_width=1/30)
#P1D6bin<-make_bins(o2=lrv_p1orig$D6[1:1081],duration=lrv_p1orig$Time.Min.[1:1081],max_o2_width=1/10,min_o2_width=1/30)

#P2A1bin<-make_bins(o2=lrv_p2orig$A1[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2A2bin<-make_bins(o2=lrv_p2orig$A2[1:1481],duration=lrv_p2orig$Time.Min.[1:1481],max_o2_width=1/10,min_o2_width=1/30)
#P2A3bin<-make_bins(o2=lrv_p2orig$A3[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2A4bin<-make_bins(o2=lrv_p2orig$A4[1:1928],duration=lrv_p2orig$Time.Min.[1:1928],max_o2_width=1/10,min_o2_width=1/30)
P2A5bin<-make_bins(o2=lrv_p2orig$A5[1:1257],duration=lrv_p2orig$Time.Min.[1:1257],max_o2_width=1/10,min_o2_width=1/30)
P2A6bin<-make_bins(o2=lrv_p2orig$A6[1:959],duration=lrv_p2orig$Time.Min.[1:959],max_o2_width=1/10,min_o2_width=1/30)
P2B1bin<-make_bins(o2=lrv_p2orig$B1[1:1108],duration=lrv_p2orig$Time.Min.[1:1108],max_o2_width=1/10,min_o2_width=1/30)
P2B2bin<-make_bins(o2=lrv_p2orig$B2[1:1257],duration=lrv_p2orig$Time.Min.[1:1257],max_o2_width=1/10,min_o2_width=1/30)
P2B3bin<-make_bins(o2=lrv_p2orig$B3[1:1033],duration=lrv_p2orig$Time.Min.[1:1033],max_o2_width=1/10,min_o2_width=1/30)
P2B4bin<-make_bins(o2=lrv_p2orig$B4[1:735],duration=lrv_p2orig$Time.Min.[1:735],max_o2_width=1/10,min_o2_width=1/30)
#P2B5bin<-make_bins(o2=lrv_p2orig$B5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P2B6bin<-make_bins(o2=lrv_p2orig$B6[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2C1bin<-make_bins(o2=lrv_p2orig$C1[1:1257],duration=lrv_p2orig$Time.Min.[1:1257],max_o2_width=1/10,min_o2_width=1/30)
#P2C2bin<-make_bins(o2=lrv_p2orig$C2[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2C3bin<-make_bins(o2=lrv_p2orig$C3[1:1332],duration=lrv_p2orig$Time.Min.[1:1332],max_o2_width=1/10,min_o2_width=1/30)
P2C4bin<-make_bins(o2=lrv_p2orig$C4[1:2511],duration=lrv_p2orig$Time.Min.[1:2511],max_o2_width=1/10,min_o2_width=1/30)
#P2C5bin<-make_bins(o2=lrv_p2orig$C5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P2C6bin<-make_bins(o2=lrv_p2orig$C6[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2D1bin<-make_bins(o2=lrv_p2orig$D1[1:585],duration=lrv_p2orig$Time.Min.[1:585],max_o2_width=1/10,min_o2_width=1/30)
#P2D2bin<-make_bins(o2=lrv_p2orig$D2[1:841],duration=lrv_p2orig$Time.Min.[1:841],max_o2_width=1/10,min_o2_width=1/30)
P2D3bin<-make_bins(o2=lrv_p2orig$D3[1:884],duration=lrv_p2orig$Time.Min.[1:884],max_o2_width=1/10,min_o2_width=1/30)
P2D4bin<-make_bins(o2=lrv_p2orig$D4[1:1332],duration=lrv_p2orig$Time.Min.[1:1332],max_o2_width=1/10,min_o2_width=1/30)
#P2D5bin<-make_bins(o2=lrv_p2orig$D5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P2D6bin<-make_bins(o2=lrv_p2orig$D6[1:721],duration=lrv_p2orig$Time.Min.[1:721],max_o2_width=1/10,min_o2_width=1/30)


#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
P1A1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1908,]$Time.Min.,o2=lrv_p1orig[1:1908,]$A1,o2_unit="mg_per_l",bin_width=P1A1bin,vol=0.0006,temp=lrv_p1orig[1:1908,]$T_internal,sal=30.0),rep("A1"),rep("t3"),rep("high"))
#P1A2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$A2,o2_unit="mg_per_l",bin_width=P1A2bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("A2"),rep("t3"),rep("blankt3"))
P1A3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1042,]$Time.Min.,o2=lrv_p1orig[1:1042,]$A3,o2_unit="mg_per_l",bin_width=P1A3bin,vol=0.0006,temp=lrv_p1orig[1:1042,]$T_internal,sal=30.0),rep("A3"),rep("t3"),rep("high"))
P1A4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1192,]$Time.Min.,o2=lrv_p1orig[1:1192,]$A4,o2_unit="mg_per_l",bin_width=P1A4bin,vol=0.0006,temp=lrv_p1orig[1:1192,]$T_internal,sal=30.0),rep("A4"),rep("t3"),rep("high"))
#P1A5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$A5,o2_unit="mg_per_l",bin_width=P1A5bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("A5"),rep("t3"),rep("blankt3"))
#P1A6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$A6,o2_unit="mg_per_l",bin_width=P1A6bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("A6"),rep("t4"),rep("blankt4"))
#P1B1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$B1,o2_unit="mg_per_l",bin_width=P1B1bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("B1"),rep("t9"),rep("blankt9"))
P1B2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1341,]$Time.Min.,o2=lrv_p1orig[1:1341,]$B2,o2_unit="mg_per_l",bin_width=P1B2bin,vol=0.0006,temp=lrv_p1orig[1:1341,]$T_internal,sal=30.0),rep("B2"),rep("t9"),rep("high"))
P1B3<-data.frame(calc_MO2(duration=lrv_p1orig[1:968,]$Time.Min.,o2=lrv_p1orig[1:968,]$B3,o2_unit="mg_per_l",bin_width=P1B3bin,vol=0.0006,temp=lrv_p1orig[1:968,]$T_internal,sal=30.0),rep("B3"),rep("t9"),rep("high"))
#P1B4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$B4,o2_unit="mg_per_l",bin_width=P1B4bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("B4"),rep("t9"),rep("high"))
#P1B5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$B5,o2_unit="mg_per_l",bin_width=P1B5bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("B5"),rep("t9"),rep("blankt9"))
P1B6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1341,]$Time.Min.,o2=lrv_p1orig[1:1341,]$B6,o2_unit="mg_per_l",bin_width=P1B6bin,vol=0.0006,temp=lrv_p1orig[1:1341,]$T_internal,sal=30.0),rep("B6"),rep("t4"),rep("med"))
P1C1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1192,]$Time.Min.,o2=lrv_p1orig[1:1192,]$C1,o2_unit="mg_per_l",bin_width=P1C1bin,vol=0.0006,temp=lrv_p1orig[1:1192,]$T_internal,sal=30.0),rep("C1"),rep("t1"),rep("amb"))
#P1C2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$C2,o2_unit="mg_per_l",bin_width=P1C2bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("C2"),rep("t1"),rep("blankt1"))
P1C3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1565,]$Time.Min.,o2=lrv_p1orig[1:1565,]$C3,o2_unit="mg_per_l",bin_width=P1C3bin,vol=0.0006,temp=lrv_p1orig[1:1565,]$T_internal,sal=30.0),rep("C3"),rep("t1"),rep("amb"))
#P1C4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$C4,o2_unit="mg_per_l",bin_width=P1C4bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("C4"),rep("t1"),rep("blankt1"))
P1C5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1192,]$Time.Min.,o2=lrv_p1orig[1:1192,]$C5,o2_unit="mg_per_l",bin_width=P1C5bin,vol=0.0006,temp=lrv_p1orig[1:1192,]$T_internal,sal=30.0),rep("C5"),rep("t1"),rep("amb"))
#P1C6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$C6,o2_unit="mg_per_l",bin_width=P1C6bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("C6"),rep("t4"),rep("blankt4"))
P1D1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1192,]$Time.Min.,o2=lrv_p1orig[1:1192,]$D1,o2_unit="mg_per_l",bin_width=P1D1bin,vol=0.0006,temp=lrv_p1orig[1:1192,]$T_internal,sal=30.0),rep("D1"),rep("t7"),rep("high"))
P1D2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1490,]$Time.Min.,o2=lrv_p1orig[1:1490,]$D2,o2_unit="mg_per_l",bin_width=P1D2bin,vol=0.0006,temp=lrv_p1orig[1:1490,]$T_internal,sal=30.0),rep("D2"),rep("t7"),rep("high"))
#P1D3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$D3,o2_unit="mg_per_l",bin_width=P1D3bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("D3"),rep("t7"),rep("blankt7"))
P1D4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1192,]$Time.Min.,o2=lrv_p1orig[1:1192,]$D4,o2_unit="mg_per_l",bin_width=P1D4bin,vol=0.0006,temp=lrv_p1orig[1:1192,]$T_internal,sal=30.0),rep("D4"),rep("t7"),rep("high"))
P1D5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1415,]$Time.Min.,o2=lrv_p1orig[1:1415,]$D5,o2_unit="mg_per_l",bin_width=P1D5bin,vol=0.0006,temp=lrv_p1orig[1:1415,]$T_internal,sal=30.0),rep("D5"),rep("t7"),rep("high"))
#P1D6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1000,]$Time.Min.,o2=lrv_p1orig[1:1000,]$D6,o2_unit="mg_per_l",bin_width=P1D6bin,vol=0.0006,temp=lrv_p1orig[1:1000,]$T_internal,sal=30.0),rep("D6"),rep("t7"),rep("blankt7"))

#P2A1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$A1,o2_unit="mg_per_l",bin_width=P2A1bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("A1"),rep("t2"),rep("blankt2"))
P2A2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1481,]$Time.Min.,o2=lrv_p2orig[1:1481,]$A2,o2_unit="mg_per_l",bin_width=P2A2bin,vol=0.0005,temp=lrv_p2orig[1:1481,]$T_internal,sal=30.0),rep("A2"),rep("t2"),rep("med"))
#P2A3<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$A3,o2_unit="mg_per_l",bin_width=P2A3bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("A3"),rep("t2"),rep("blankt2"))
P2A4<-data.frame(calc_MO2(duration=lrv_p2orig[1:1928,]$Time.Min.,o2=lrv_p2orig[1:1928,]$A4,o2_unit="mg_per_l",bin_width=P2A4bin,vol=0.0005,temp=lrv_p2orig[1:1928,]$T_internal,sal=30.0),rep("A4"),rep("t2"),rep("med"))
P2A5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1257,]$Time.Min.,o2=lrv_p2orig[1:1257,]$A5,o2_unit="mg_per_l",bin_width=P2A5bin,vol=0.0005,temp=lrv_p2orig[1:1257,]$T_internal,sal=30.0),rep("A5"),rep("t5"),rep("med"))
P2A6<-data.frame(calc_MO2(duration=lrv_p2orig[1:959,]$Time.Min.,o2=lrv_p2orig[1:959,]$A6,o2_unit="mg_per_l",bin_width=P2A6bin,vol=0.0005,temp=lrv_p2orig[1:959,]$T_internal,sal=30.0),rep("A6"),rep("t5"),rep("med"))
P2B1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1108,]$Time.Min.,o2=lrv_p2orig[1:1108,]$B1,o2_unit="mg_per_l",bin_width=P2B1bin,vol=0.0005,temp=lrv_p2orig[1:1108,]$T_internal,sal=30.0),rep("B1"),rep("t8"),rep("amb"))
P2B2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1257,]$Time.Min.,o2=lrv_p2orig[1:1257,]$B2,o2_unit="mg_per_l",bin_width=P2B2bin,vol=0.0005,temp=lrv_p2orig[1:1257,]$T_internal,sal=30.0),rep("B2"),rep("t8"),rep("amb"))
P2B3<-data.frame(calc_MO2(duration=lrv_p2orig[1:1033,]$Time.Min.,o2=lrv_p2orig[1:1033,]$B3,o2_unit="mg_per_l",bin_width=P2B3bin,vol=0.0005,temp=lrv_p2orig[1:1033,]$T_internal,sal=30.0),rep("B3"),rep("t8"),rep("amb"))
P2B4<-data.frame(calc_MO2(duration=lrv_p2orig[1:735,]$Time.Min.,o2=lrv_p2orig[1:735,]$B4,o2_unit="mg_per_l",bin_width=P2B4bin,vol=0.0005,temp=lrv_p2orig[1:735,]$T_internal,sal=30.0),rep("B4"),rep("t8"),rep("amb"))
#P2B5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$B5,o2_unit="mg_per_l",bin_width=P2B5bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("B5"),rep("t8"),rep("blankt8"))
#P2B6<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$B6,o2_unit="mg_per_l",bin_width=P2B6bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("B6"),rep("t8"),rep("blankt8"))
P2C1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1257,]$Time.Min.,o2=lrv_p2orig[1:1257,]$C1,o2_unit="mg_per_l",bin_width=P2C1bin,vol=0.0005,temp=lrv_p2orig[1:1257,]$T_internal,sal=30.0),rep("C1"),rep("t6"),rep("amb"))
#P2C2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$C2,o2_unit="mg_per_l",bin_width=P2C2bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("C2"),rep("t6"),rep("blankt6"))
P2C3<-data.frame(calc_MO2(duration=lrv_p2orig[1:1332,]$Time.Min.,o2=lrv_p2orig[1:1332,]$C3,o2_unit="mg_per_l",bin_width=P2C3bin,vol=0.0005,temp=lrv_p2orig[1:1332,]$T_internal,sal=30.0),rep("C3"),rep("t6"),rep("amb"))
P2C4<-data.frame(calc_MO2(duration=lrv_p2orig[1:2511,]$Time.Min.,o2=lrv_p2orig[1:2511,]$C4,o2_unit="mg_per_l",bin_width=P2C4bin,vol=0.0005,temp=lrv_p2orig[1:2511,]$T_internal,sal=30.0),rep("C4"),rep("t6"),rep("amb"))
#P2C5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$C5,o2_unit="mg_per_l",bin_width=P2C5bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("C5"),rep("t6"),rep("blankt6"))
#P2C6<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$C6,o2_unit="mg_per_l",bin_width=P2C6bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("C6"),rep("t4"),rep("med"))
P2D1<-data.frame(calc_MO2(duration=lrv_p2orig[1:585,]$Time.Min.,o2=lrv_p2orig[1:585,]$D1,o2_unit="mg_per_l",bin_width=P2D1bin,vol=0.0005,temp=lrv_p2orig[1:585,]$T_internal,sal=30.0),rep("D1"),rep("t5"),rep("med"))
#P2D2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$D2,o2_unit="mg_per_l",bin_width=P2D2bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("D2"),rep("t5"),rep("blankt5"))
P2D3<-data.frame(calc_MO2(duration=lrv_p2orig[1:884,]$Time.Min.,o2=lrv_p2orig[1:884,]$D3,o2_unit="mg_per_l",bin_width=P2D3bin,vol=0.0005,temp=lrv_p2orig[1:884,]$T_internal,sal=30.0),rep("D3"),rep("t5"),rep("med"))
P2D4<-data.frame(calc_MO2(duration=lrv_p2orig[1:1332,]$Time.Min.,o2=lrv_p2orig[1:1332,]$D4,o2_unit="mg_per_l",bin_width=P2D4bin,vol=0.0005,temp=lrv_p2orig[1:1332,]$T_internal,sal=30.0),rep("D4"),rep("t5"),rep("med"))
#P2D5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$D5,o2_unit="mg_per_l",bin_width=P2D5bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("D5"),rep("t5"),rep("blankt5"))
#P2D6<-data.frame(calc_MO2(duration=lrv_p2orig[1:1000,]$Time.Min.,o2=lrv_p2orig[1:1000,]$D6,o2_unit="mg_per_l",bin_width=P2D6bin,vol=0.0005,temp=lrv_p2orig[1:1000,]$T_internal,sal=30.0),rep("D6"),rep("t4"),rep("blankt4"))

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
P1A1$MO2b<-P1A1$MO2-t3blank
P1A2$MO2b<-P1A2$MO2-t3blank
P1A3$MO2b<-P1A3$MO2-t3blank
P1A4$MO2b<-P1A4$MO2-t3blank
P1A5$MO2b<-P1A5$MO2-t3blank
P1A6$MO2b<-P1A6$MO2-t4blank
P1B1$MO2b<-P1B1$MO2-t9blank
P1B2$MO2b<-P1B2$MO2-t9blank
P1B3$MO2b<-P1B3$MO2-t9blank
P1B4$MO2b<-P1B4$MO2-t9blank
P1B5$MO2b<-P1B5$MO2-t9blank
P1B6$MO2b<-P1B6$MO2-t4blank
P1C1$MO2b<-P1C1$MO2-t1blank
P1C2$MO2b<-P1C2$MO2-t1blank
P1C3$MO2b<-P1C3$MO2-t1blank
P1C4$MO2b<-P1C4$MO2-t1blank
P1C5$MO2b<-P1C5$MO2-t1blank
P1C6$MO2b<-P1C6$MO2-t4blank
P1D1$MO2b<-P1D1$MO2-t7blank
P1D2$MO2b<-P1D2$MO2-t7blank
P1D3$MO2b<-P1D3$MO2-t7blank
P1D4$MO2b<-P1D4$MO2-t7blank
P1D5$MO2b<-P1D5$MO2-t7blank
P1D6$MO2b<-P1D6$MO2-t7blank
P2A1$MO2b<-P2A1$MO2-t2blank
P2A2$MO2b<-P2A2$MO2-t2blank
P2A3$MO2b<-P2A3$MO2-t2blank
P2A4$MO2b<-P2A4$MO2-t2blank
P2A5$MO2b<-P2A5$MO2-t5blank
P2A6$MO2b<-P2A6$MO2-t5blank
P2B1$MO2b<-P2B1$MO2-t8blank
P2B2$MO2b<-P2B2$MO2-t8blank
P2B3$MO2b<-P2B3$MO2-t8blank
P2B4$MO2b<-P2B4$MO2-t8blank
P2B5$MO2b<-P2B5$MO2-t8blank
P2B6$MO2b<-P2B6$MO2-t8blank
P2C1$MO2b<-P2C1$MO2-t6blank
P2C2$MO2b<-P2C2$MO2-t6blank
P2C3$MO2b<-P2C3$MO2-t6blank
P2C4$MO2b<-P2C4$MO2-t6blank
P2C5$MO2b<-P2C5$MO2-t6blank
P2C6$MO2b<-P2C6$MO2-t4blank
P2D1$MO2b<-P2D1$MO2-t5blank
P2D2$MO2b<-P2D2$MO2-t5blank
P2D3$MO2b<-P2D3$MO2-t5blank
P2D4$MO2b<-P2D4$MO2-t5blank
P2D5$MO2b<-P2D5$MO2-t5blank
P2D6$MO2b<-P2D6$MO2-t4blank

#Add a column in each well's calc_mo2 output for mass and msmr. 
P1A1$dw<-rep(0.06247091)
P1A3$dw<-rep(0.18165058)
P1A4$dw<-rep(0.11295278)
P1B2$dw<-rep(0.09439715)
P1B3$dw<-rep(0.27994570)
P1B6$dw<-rep(0.14755564)
P1C1$dw<-rep(0.08686671)
P1C3$dw<-rep(0.05235767)
P1C5$dw<-rep(0.08677274)
P1D1$dw<-rep(0.10472544)
P1D2$dw<-rep(0.04906919)
P1D4$dw<-rep(0.13807107)
P1D5$dw<-rep(0.06449025)
P2A2$dw<-rep(0.06672954)
P2A4$dw<-rep(0.07941095)
P2A5$dw<-rep(0.11768010)
P2A6$dw<-rep(0.22495392)
P2B1$dw<-rep(0.20566076)
P2B2$dw<-rep(0.06196151)
P2B3$dw<-rep(0.10707247)
P2B4$dw<-rep(0.29503372)
P2C1$dw<-rep(0.17521464)
P2C3$dw<-rep(0.07373983)
P2C4$dw<-rep(0.05529619)
P2D1$dw<-rep(0.22639415)
P2D3$dw<-rep(0.10075909)
P2D4$dw<-rep(0.12439017)

P1A1$msmrs<-P1A1$MO2/P1A1$dw
P1A3$msmrs<-P1A3$MO2/P1A3$dw
P1A4$msmrs<-P1A4$MO2/P1A4$dw
P1B2$msmrs<-P1B2$MO2/P1B2$dw
P1B3$msmrs<-P1B3$MO2/P1B3$dw
P1B6$msmrs<-P1B6$MO2/P1B6$dw
P1C1$msmrs<-P1C1$MO2/P1C1$dw
P1C3$msmrs<-P1C3$MO2/P1C3$dw
P1C5$msmrs<-P1C5$MO2/P1C5$dw
P1D1$msmrs<-P1D1$MO2/P1D1$dw
P1D2$msmrs<-P1D2$MO2/P1D2$dw
P1D4$msmrs<-P1D4$MO2/P1D4$dw
P1D5$msmrs<-P1D5$MO2/P1D5$dw
P2A2$msmrs<-P2A2$MO2/P2A2$dw
P2A4$msmrs<-P2A4$MO2/P2A4$dw
P2A5$msmrs<-P2A5$MO2/P2A5$dw
P2A6$msmrs<-P2A6$MO2/P2A6$dw
P2B1$msmrs<-P2B1$MO2/P2B1$dw
P2B2$msmrs<-P2B2$MO2/P2B2$dw
P2B3$msmrs<-P2B3$MO2/P2B3$dw
P2B4$msmrs<-P2B4$MO2/P2B4$dw
P2C1$msmrs<-P2C1$MO2/P2C1$dw
P2C3$msmrs<-P2C3$MO2/P2C3$dw
P2C4$msmrs<-P2C4$MO2/P2C4$dw
P2D1$msmrs<-P2D1$MO2/P2D1$dw
P2D3$msmrs<-P2D3$MO2/P2D3$dw
P2D4$msmrs<-P2D4$MO2/P2D4$dw

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

#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES?? Fit a line to all of the points from the same treatment/tank. 
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
alllrv<-rbind(P1A1,P1A3,P1A4,P1B2,P1B3,P1B6,P1C1,P1C3,P1C5,P1D1,P1D2,P1D4,P1D5,
              P2A2,P2A4,P2A5,P2A6,P2B1,P2B2,P2B3,P2B4,P2C1,P2C3,P2C4,P2D1,P2D3,P2D4)

#plot the curves
library(ggplot2)
par(mfrow=c(1,1))
allplotlrv<-ggplot(alllrv, aes(x=O2_MEAN,y=msmrs,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlrv)


#look for ones that are above 0.4-0.5 or below -0.05
hist(P1A1$msmrs,breaks=10)
hist(P1A2$msmrs,breaks=10)
hist(P1A3$msmrs,breaks=10) #two below -0.05
hist(P1A4$msmrs,breaks=10) 
hist(P1A5$msmrs,breaks=10)
hist(P1A6$msmrs,breaks=10) 
hist(P1B1$msmrs,breaks=10)
hist(P1B2$msmrs,breaks=10) #one above 0.6
hist(P1B3$msmrs,breaks=10) 
hist(P1B4$msmrs,breaks=10) 
hist(P1B5$msmrs,breaks=10)
hist(P1B6$msmrs,breaks=10)
hist(P1C1$msmrs,breaks=10) #two near 0.5
hist(P1C2$msmrs,breaks=10)
hist(P1C3$msmrs,breaks=10) #one below -0.2, one above 0.6 and many above 0.5
hist(P1C4$msmrs,breaks=10)
hist(P1C5$msmrs,breaks=10)
hist(P1C6$msmrs,breaks=10) 
hist(P1D1$msmrs,breaks=10)
hist(P1D2$msmrs,breaks=10) #one might be below -0.05, three above 0.6
hist(P1D3$msmrs,breaks=10)
hist(P1D4$msmrs,breaks=10)
hist(P1D5$msmrs,breaks=10)
hist(P1D6$msmrs,breaks=10) 
hist(P2A1$msmrs,breaks=10)
hist(P2A2$msmrs,breaks=10)
hist(P2A3$msmrs,breaks=10)
hist(P2A4$msmrs,breaks=10)
hist(P2A5$msmrs,breaks=10)
hist(P2A6$msmrs,breaks=10)
hist(P2B1$msmrs,breaks=10) #one above 0.5
hist(P2B2$msmrs,breaks=10)
hist(P2B3$msmrs,breaks=10)
hist(P2B4$msmrs,breaks=10)
hist(P2B5$msmrs,breaks=10)
hist(P2B6$msmrs,breaks=10)
hist(P2C1$msmrs,breaks=10)
hist(P2C2$msmrs,breaks=10)
hist(P2C3$msmrs,breaks=10)
hist(P2C4$msmrs,breaks=10)
hist(P2C5$msmrs,breaks=10) 
hist(P2C6$msmrs,breaks=10)
hist(P2D1$msmrs,breaks=10)
hist(P2D2$msmrs,breaks=10)
hist(P2D3$msmrs,breaks=10) #several that may be below -0.05, one above 0.8
hist(P2D4$msmrs,breaks=10)
hist(P2D5$msmrs,breaks=10)
hist(P2D6$msmrs,breaks=10)

#Remove problematic data
row.names(P1A3)<-NULL
row.names(P1C3)<-NULL
row.names(P1D2)<-NULL


P1A3<-P1A3[-c(93,94),] 
P1C3<-P1C3[-c(55),] 
P1D2<-P1D2[-c(14),]

row.names(P1A3)<-NULL
row.names(P1C3)<-NULL
row.names(P1D2)<-NULL


lrv_dana$Pcrit_alpha<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['Alpha'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Alpha'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Alpha'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Alpha'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Alpha'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['Alpha'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Alpha'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Alpha'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Alpha'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Alpha'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Alpha'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Alpha'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Alpha'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['Alpha'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Alpha'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Alpha'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Alpha'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['Alpha'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Alpha'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Alpha'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Alpha'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['Alpha'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Alpha'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Alpha'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Alpha'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['Alpha'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Alpha'])
lrv_dana$Pcrit_break<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['Breakpoint'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Breakpoint'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Breakpoint'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Breakpoint'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Breakpoint'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['Breakpoint'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Breakpoint'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Breakpoint'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Breakpoint'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Breakpoint'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Breakpoint'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Breakpoint'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Breakpoint'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['Breakpoint'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Breakpoint'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Breakpoint'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Breakpoint'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['Breakpoint'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Breakpoint'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Breakpoint'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Breakpoint'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['Breakpoint'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Breakpoint'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Breakpoint'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Breakpoint'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['Breakpoint'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Breakpoint'])
lrv_dana$Pcrit_subPI<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['Sub_PI'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['Sub_PI'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Sub_PI'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['Sub_PI'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Sub_PI'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['Sub_PI'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Sub_PI'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Sub_PI'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Sub_PI'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Sub_PI'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Sub_PI'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Sub_PI'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Sub_PI'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['Sub_PI'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Sub_PI'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Sub_PI'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Sub_PI'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['Sub_PI'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Sub_PI'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Sub_PI'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Sub_PI'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['Sub_PI'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Sub_PI'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Sub_PI'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Sub_PI'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['Sub_PI'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['Sub_PI'])
lrv_dana$Pcrit_NLR<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['NLR'],
                        calc_pcrit(P1A3$O2_MEAN,P1A3$msmrs)['NLR'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['NLR'],
                        calc_pcrit(P1B2$O2_MEAN,P1B2$msmrs)['NLR'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['NLR'],
                        calc_pcrit(P1B6$O2_MEAN,P1B6$msmrs)['NLR'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['NLR'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['NLR'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['NLR'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['NLR'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['NLR'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['NLR'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['NLR'],
                        calc_pcrit(P2A2$O2_MEAN,P2A2$msmrs)['NLR'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['NLR'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['NLR'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['NLR'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$msmrs)['NLR'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['NLR'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['NLR'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['NLR'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['NLR'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['NLR'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['NLR'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['NLR'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['NLR'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$msmrs)['NLR'])
lrv_dana$alpha<-c(calc_alpha(P1A1$O2_MEAN,P1A1$msmrs)$alpha,
                        calc_alpha(P1A3$O2_MEAN,P1A3$msmrs)$alpha,
                        calc_alpha(P1A4$O2_MEAN,P1A4$msmrs)$alpha,
                        calc_alpha(P1B2$O2_MEAN,P1B2$msmrs)$alpha,
                        calc_alpha(P1B3$O2_MEAN,P1B3$msmrs)$alpha,
                        calc_alpha(P1B6$O2_MEAN,P1B6$msmrs)$alpha,
                        calc_alpha(P1C1$O2_MEAN,P1C1$msmrs)$alpha,
                        calc_alpha(P1C3$O2_MEAN,P1C3$msmrs)$alpha,
                        calc_alpha(P1C5$O2_MEAN,P1C5$msmrs)$alpha,
                        calc_alpha(P1D1$O2_MEAN,P1D1$msmrs)$alpha,
                        calc_alpha(P1D2$O2_MEAN,P1D2$msmrs)$alpha,
                        calc_alpha(P1D4$O2_MEAN,P1D4$msmrs)$alpha,
                        calc_alpha(P1D5$O2_MEAN,P1D5$msmrs)$alpha,
                        calc_alpha(P2A2$O2_MEAN,P2A2$msmrs)$alpha,
                        calc_alpha(P2A4$O2_MEAN,P2A4$msmrs)$alpha,
                        calc_alpha(P2A5$O2_MEAN,P2A5$msmrs)$alpha,
                        calc_alpha(P2A6$O2_MEAN,P2A6$msmrs)$alpha,
                        calc_alpha(P2B1$O2_MEAN,P2B1$msmrs)$alpha,
                        calc_alpha(P2B2$O2_MEAN,P2B2$msmrs)$alpha,
                        calc_alpha(P2B3$O2_MEAN,P2B3$msmrs)$alpha,
                        calc_alpha(P2B4$O2_MEAN,P2B4$msmrs)$alpha,
                        calc_alpha(P2C1$O2_MEAN,P2C1$msmrs)$alpha,
                        calc_alpha(P2C3$O2_MEAN,P2C3$msmrs)$alpha,
                        calc_alpha(P2C4$O2_MEAN,P2C4$msmrs)$alpha,
                        calc_alpha(P2D1$O2_MEAN,P2D1$msmrs)$alpha,
                        calc_alpha(P2D3$O2_MEAN,P2D3$msmrs)$alpha,
                        calc_alpha(P2D4$O2_MEAN,P2D4$msmrs)$alpha)


###############################################################################
#Using selgmented function instead of calc_pcrit (segmented function) to get Pcrit
library(segmented)


P1A1seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1A1)
plot(P1A1seg,add=T)
plot_pcrit(P1A1$O2_MEAN,P1A1$msmrs)
print(P1A1seg)

P1A2seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A2[1:88,]),seg.Z=~O2_MEAN,type='bic',Kmax=6,msg=F)
plot(msmrs~O2_MEAN,data=P1A2[1:88,])
plot(P1A2seg,add=T)
plot_pcrit(P1A2$O2_MEAN,P1A2$msmrs)
print(P1A2seg)

P1A3seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1A3)
plot(P1A3seg,add=T)
plot_pcrit(P1A3$O2_MEAN,P1A3$msmrs)
print(P1A3seg)

P1A4seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1A4)
plot(P1A4seg,add=T)
plot_pcrit(P1A4$O2_MEAN,P1A4$msmrs)
print(P1A4seg)

P1A5seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1A5)
plot(P1A5seg,add=T)
plot_pcrit(P1A5$O2_MEAN,P1A5$msmrs)
print(P1A5seg)

P1A6seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1A6)
plot(P1A6seg,add=T)
plot_pcrit(P1A6$O2_MEAN,P1A6$msmrs)
print(P1A6seg)

P1B1seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1B1)
plot(P1B1seg,add=T)
plot_pcrit(P1B1$O2_MEAN,P1B1$msmrs)
print(P1B1seg)

P1B2seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B2),seg.Z=~O2_MEAN,type='bic',Kmax=5,msg=F)
plot(msmrs~O2_MEAN,data=P1B2)
plot(P1B2seg,add=T)
plot_pcrit(P1B2$O2_MEAN,P1B2$msmrs)
print(P1B2seg)

P1B3seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1B3)
plot(P1B3seg,add=T)
plot_pcrit(P1B3$O2_MEAN,P1B3$msmrs)
print(P1B3seg)

P1B4seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1B4)
plot(P1B4seg,add=T)
plot_pcrit(P1B4$O2_MEAN,P1B4$msmrs)
print(P1B4seg)

P1B5seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1B5)
plot(P1B5seg,add=T)
plot_pcrit(P1B5$O2_MEAN,P1B5$msmrs)
print(P1B5seg)

P1B6seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B6),seg.Z=~O2_MEAN,type='bic',Kmax=8,msg=T)
plot(msmrs~O2_MEAN,data=P1B6)
plot(P1B6seg,add=T)
plot_pcrit(P1B6$O2_MEAN,P1B6$msmrs)
print(P1B6seg)

P1C1seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1C1)
plot(P1C1seg,add=T)
plot_pcrit(P1C1$O2_MEAN,P1C1$msmrs)
print(P1C1seg)

P1C2seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1C2)
plot(P1C2seg,add=T)
plot_pcrit(P1C2$O2_MEAN,P1C2$msmrs)
print(P1C2seg)

P1C3seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1C3)
plot(P1C3seg,add=T)
plot_pcrit(P1C3$O2_MEAN,P1C3$msmrs)
print(P1C3seg)

P1C4seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1C4)
plot(P1C4seg,add=T)
plot_pcrit(P1C4$O2_MEAN,P1C4$msmrs)
print(P1C4seg)

P1C5seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1C5)
plot(P1C5seg,add=T)
plot_pcrit(P1C5$O2_MEAN,P1C5$msmrs)
print(P1C5seg)

P1C6seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(msmrs~O2_MEAN,data=P1C6)
plot(P1C6seg,add=T)
plot_pcrit(P1C6$O2_MEAN,P1C6$msmrs)
print(P1C6seg)

P1D1seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1D1)
plot(P1D1seg,add=T)
plot_pcrit(P1D1$O2_MEAN,P1D1$msmrs)
print(P1D1seg)

P1D2seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1D2)
plot(P1D2seg,add=T)
plot_pcrit(P1D2$O2_MEAN,P1D2$msmrs)
print(P1D2seg)

P1D3seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1D3)
plot(P1D3seg,add=T)
plot_pcrit(P1D3$O2_MEAN,P1D3$msmrs)
print(P1D3seg)

P1D4seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1D4)
plot(P1D4seg,add=T)
plot_pcrit(P1D4$O2_MEAN,P1D4$msmrs)
print(P1D4seg)

P1D5seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(msmrs~O2_MEAN,data=P1D5)
plot(P1D5seg,add=T)
plot_pcrit(P1D5$O2_MEAN,P1D5$msmrs)
print(P1D5seg)

P1D6seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1D6)
plot(P1D6seg,add=T)
plot_pcrit(P1D6$O2_MEAN,P1D6$msmrs)
print(P1D6seg)

#Plate 2

P2A1seg<-selgmented(lm(msmrs~O2_MEAN,data=P2A1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2A1)
plot(P2A1seg,add=T)
plot_pcrit(P2A1$O2_MEAN,P2A1$msmrs)
print(P2A1seg)

P2A2seg<-selgmented(lm(msmrs~O2_MEAN,data=P2A2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2A2)
plot(P2A2seg,add=T)
plot_pcrit(P2A2$O2_MEAN,P2A2$msmrs)
print(P2A2seg)

P2A3seg<-selgmented(lm(msmrs~O2_MEAN,data=P2A3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2A3)
plot(P2A3seg,add=T)
plot_pcrit(P2A3$O2_MEAN,P2A3$msmrs)
print(P2A3seg)

P2A4seg<-selgmented(lm(msmrs~O2_MEAN,data=P2A4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2A4)
plot(P2A4seg,add=T)
plot_pcrit(P2A4$O2_MEAN,P2A4$msmrs)
print(P2A4seg)

P2A5seg<-selgmented(lm(msmrs~O2_MEAN,data=P2A5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2A5)
plot(P2A5seg,add=T)
plot_pcrit(P2A5$O2_MEAN,P2A5$msmrs)
print(P2A5seg)

P2A6seg<-selgmented(lm(msmrs~O2_MEAN,data=P2A6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2A6)
plot(P2A6seg,add=T)
plot_pcrit(P2A6$O2_MEAN,P2A6$msmrs)
print(P2A6seg)

P2B1seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2B1)
plot(P2B1seg,add=T)
plot_pcrit(P2B1$O2_MEAN,P2B1$msmrs)
print(P2B1seg)

P2B2seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2B2)
plot(P2B2seg,add=T)
plot_pcrit(P2B2$O2_MEAN,P2B2$msmrs)
print(P2B2seg)

P2B3seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(msmrs~O2_MEAN,data=P2B3)
plot(P2B3seg,add=T)
plot_pcrit(P2B3$O2_MEAN,P2B3$msmrs)
print(P2B3seg)

P2B4seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2B4)
plot(P2B4seg,add=T)
plot_pcrit(P2B4$O2_MEAN,P2B4$msmrs)
print(P2B4seg)

P2B5seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2B5)
plot(P2B5seg,add=T)
plot_pcrit(P2B5$O2_MEAN,P2B5$msmrs)
print(P2B5seg)

P2B6seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2B6)
plot(P2B6seg,add=T)
plot_pcrit(P2B6$O2_MEAN,P2B6$msmrs)
print(P2B6seg)

P2C1seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2C1)
plot(P2C1seg,add=T)
plot_pcrit(P2C1$O2_MEAN,P2C1$msmrs)
print(P2C1seg)

P2C2seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2C2)
plot(P2C2seg,add=T)
plot_pcrit(P2C2$O2_MEAN,P2C2$msmrs)
print(P2C2seg)

P2C3seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2C3)
plot(P2C3seg,add=T)
plot_pcrit(P2C3$O2_MEAN,P2C3$msmrs)
print(P2C3seg)

P2C4seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2C4)
plot(P2C4seg,add=T)
plot_pcrit(P2C4$O2_MEAN,P2C4$msmrs)
print(P2C4seg)

P2C5seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(msmrs~O2_MEAN,data=P2C5)
plot(P2C5seg,add=T)
plot_pcrit(P2C5$O2_MEAN,P2C5$msmrs)
print(P2C5seg)

P2C6seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2C6)
plot(P2C6seg,add=T)
plot_pcrit(P2C6$O2_MEAN,P2C6$msmrs)
print(P2C6seg)

P2D1seg<-selgmented(lm(msmrs~O2_MEAN,data=P2D1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2D1)
plot(P2D1seg,add=T)
plot_pcrit(P2D1$O2_MEAN,P2D1$msmrs)
print(P2D1seg)

P2D2seg<-selgmented(lm(msmrs~O2_MEAN,data=P2D2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2D2)
plot(P2D2seg,add=T)
plot_pcrit(P2D2$O2_MEAN,P2D2$msmrs)
print(P2D2seg)

P2D3seg<-selgmented(lm(msmrs~O2_MEAN,data=P2D3),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2D3)
plot(P2D3seg,add=T)
plot_pcrit(P2D3$O2_MEAN,P2D3$msmrs)
print(P2D3seg)

P2D4seg<-selgmented(lm(msmrs~O2_MEAN,data=P2D4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2D4)
plot(P2D4seg,add=T)
plot_pcrit(P2D4$O2_MEAN,P2D4$msmrs)
print(P2D4seg)

P2D5seg<-selgmented(lm(msmrs~O2_MEAN,data=P2D5),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(msmrs~O2_MEAN,data=P2D5)
plot(P2D5seg,add=T)
plot_pcrit(P2D5$O2_MEAN,P2D5$msmrs)
print(P2D5seg)

P2D6seg<-selgmented(lm(msmrs~O2_MEAN,data=P2D6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2D6)
plot(P2D6seg,add=T)
plot_pcrit(P2D6$O2_MEAN,P2D6$msmrs)
print(P2D6seg)

#Finally, update the dataframe with the new Pcrit and spike data
lrv_dana$Pcrit_break<-c(1.201,0.1375,2.301,  #P1A
                        1.8589,0.39678,0.69698,  #P1B
                        0.599,0.9652,0.432494,  #P1C
                        0.9237,0.1601,0.7474,0.5787,  #P1D
                        0.5668,1.353,1.291,0.7647,  #P2A
                        1.648,1.6123,1.087,1.0193,  #P2B
                        1.68102,1.998,NA,  #P2C
                        0.81827,0.7639,1.243)  #P2D
lrv_dana$spike<-c(0,0,1,  #P1A
                  1,0,0,  #P1B
                  0,0,0,  #P1C
                  0,0,1,0,  #P1D
                  0,1,1,0,  #P2A
                  1,1,0,1,  #P2B
                  1,1,NA,  #P2C
                  0,0,1)  #P2D

################################################################################################
#Analyze, check model assumptions, and plot
#set levels of CO2_level and Tank
str(lrv_dana)

plot(lrv_dana$Pcrit_break~lrv_dana$CO2_level)

#calculate mean and SE of Pcrit values by treatment
library(plyr)
break_sum<-ddply(lrv_dana,"CO2_level",summarise,N=sum(!is.na(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

break_mod<-lmer(Pcrit_break~CO2_level+(1|Tank),data=lrv_dana) 
ranova(break_mod) #tank doesn't affect fit
anova(break_mod) #p=0.5869

#break_mod1<-lm(Pcrit_break~CO2_level,data=lrv_dana)
#anova(break_mod1) 

#try it the other way
break_mod2<-aov(lrv_dana$Pcrit_break~lrv_dana$CO2_level/factor(lrv_dana$Tank))
summary(break_mod2) #neither CO2 nor tank is significant, p=0.421 for CO2


#diagnostics
par(mfrow=c(2,2))
plot(break_mod2) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(lrv_dana$Pcrit_break[-c(2,3,4,5)]) #p=0.561
hist(lrv_dana$Pcrit_break)

#homogeneity of variances
library(car)
leveneTest(lrv_dana$Pcrit_break[-c(2,3,4,5)], lrv_dana$CO2_level[-c(2,3,4,5)]) #p=0.2213 good

#Check for outliers
cook<-cooks.distance(break_mod2)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(lrv_dana$Pcrit_break)-2-1), col="red")
text(x=1:length(cook)+1,y=cook,labels=ifelse(cook>4/(length(lrv_dana$Pcrit_break)-2-1),names(cook),""),col="red")
#Five outliers identified: 9 and 14 worst, 11, 12 and 26 on borderline. 
boxplot(lrv_dana$Pcrit_break~lrv_dana$CO2_level)
sort(residuals(break_mod2))

#plot the data - means and SEs
library(ggplot2)
library(grid)
danalrvpcritplot<-ggplot(break_sum, aes(x=CO2_level,y=MeanPcrit))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanPcrit-SE,ymax=MeanPcrit+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 1",x=0.5,y=0.96,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,2))+
  theme_classic()
print(danalrvpcritplot)


###############################################################################################
#Calculate RMR as the average MO2 for which O2>Pcrit

lrv_dana$RMR<-c(mean(c(P1A1$msmrs[P1A1$O2_MEAN>lrv_dana[1,9]])),
                mean(c(P1A3$msmrs[P1A3$O2_MEAN>lrv_dana[2,9]])),
                mean(c(P1A4$msmrs[P1A4$O2_MEAN>lrv_dana[3,9]])),
                mean(c(P1B2$msmrs[P1B2$O2_MEAN>lrv_dana[4,9]])),
                mean(c(P1B3$msmrs[P1B3$O2_MEAN>lrv_dana[5,9]])),
                mean(c(P1B6$msmrs[P1B6$O2_MEAN>lrv_dana[6,9]])),
                mean(c(P1C1$msmrs[P1C1$O2_MEAN>lrv_dana[7,9]])),
                mean(c(P1C3$msmrs[P1C3$O2_MEAN>lrv_dana[8,9]])),
                mean(c(P1C5$msmrs[P1C5$O2_MEAN>lrv_dana[9,9]])),
                mean(c(P1D1$msmrs[P1D1$O2_MEAN>lrv_dana[10,9]])),
                mean(c(P1D2$msmrs[P1D2$O2_MEAN>lrv_dana[11,9]])),
                mean(c(P1D4$msmrs[P1D4$O2_MEAN>lrv_dana[12,9]])),
                mean(c(P1D5$msmrs[P1D5$O2_MEAN>lrv_dana[13,9]])),
                mean(c(P2A2$msmrs[P2A2$O2_MEAN>lrv_dana[14,9]])),
                mean(c(P2A4$msmrs[P2A4$O2_MEAN>lrv_dana[15,9]])),
                mean(c(P2A5$msmrs[P2A5$O2_MEAN>lrv_dana[16,9]])),
                mean(c(P2A6$msmrs[P2A6$O2_MEAN>lrv_dana[17,9]])),
                mean(c(P2B1$msmrs[P2B1$O2_MEAN>lrv_dana[18,9]])),
                mean(c(P2B2$msmrs[P2B2$O2_MEAN>lrv_dana[19,9]])),
                mean(c(P2B3$msmrs[P2B3$O2_MEAN>lrv_dana[20,9]])),
                mean(c(P2B4$msmrs[P2B4$O2_MEAN>lrv_dana[21,9]])),
                mean(c(P2C1$msmrs[P2C1$O2_MEAN>lrv_dana[22,9]])),
                mean(c(P2C3$msmrs[P2C3$O2_MEAN>lrv_dana[23,9]])),
                mean(c(P2C4$msmrs[P2C4$O2_MEAN>3])),
                mean(c(P2D1$msmrs[P2D1$O2_MEAN>lrv_dana[25,9]])),
                mean(c(P2D3$msmrs[P2D3$O2_MEAN>lrv_dana[26,9]])),
                mean(c(P2D4$msmrs[P2D4$O2_MEAN>lrv_dana[27,9]])))

#analyze RMR
dana_lrv_model<-lm(lrv_dana$RMR~lrv_dana$CO2_level)
anova(dana_lrv_model) 

dana_lrv_mod<-lmer(RMR~CO2_level+(1|Tank),data=lrv_dana) 
anova(dana_lrv_mod)
ranova(dana_lrv_mod) #random effect of tank doesn't affect results. 

#try it the other way
dana_lrv_mdl<-aov(lrv_dana$RMR~lrv_dana$CO2_level/factor(lrv_dana$Tank))
summary(dana_lrv_mdl) #no significant effect of tank or CO2 (p=0.288 for CO2)

#diagnostics
par(mfrow=c(2,2))
plot(dana_lrv_mdl) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(lrv_dana$RMR[-c(2,4,5,8,9)]) #looks good p=0.6603
hist(lrv_dana$RMR)

#homogeneity of variances
library(car)
leveneTest(lrv_dana$RMR[-c(2,4,5,8,9)], lrv_dana$CO2_level[-c(2,4,5,8,9)]) #p=0.2131 good

#Check for outliers
cook<-cooks.distance(dana_lrv_mdl)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=9/(length(lrv_dana$RMR)-2-1), col="red")
text(x=1:length(cook)+1,y=cook,labels=ifelse(cook>4/(length(lrv_dana$RMR)-2-1),names(cook),""),col="red")
#Four outliers identified: 20, 24, 25, 26. First three are high, last one low. 
boxplot(lrv_dana$RMR~lrv_dana$CO2_level)
sort(residuals(dana_lrv_mdl))

#calculate the group means 

library(plyr)
dana_lrv_sum<-ddply(lrv_dana,"CO2_level",summarise,N=length(na.omit(RMR)),MeanMO2=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
dana_lrv_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
danalrvplot<-ggplot(dana_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 1",x=0.5,y=0.96,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,0.3))+
  theme_classic()
print(danalrvplot)

pct_spike_amb<-100*sum(na.omit(lrv_dana$spike[lrv_dana$CO2_level=="amb"]))/length(na.omit(lrv_dana$spike[lrv_dana$CO2_level=="amb"]))
pct_spike_med<-100*sum(na.omit(lrv_dana$spike[lrv_dana$CO2_level=="med"]))/length(na.omit(lrv_dana$spike[lrv_dana$CO2_level=="med"]))
pct_spike_high<-100*sum(na.omit(lrv_dana$spike[lrv_dana$CO2_level=="high"]))/length(na.omit(lrv_dana$spike[lrv_dana$CO2_level=="high"]))

#test for significant differences in spike percentage between groups
spike<-c(5,3,3)
fish<-c(9,8,9)
prop.test(spike,fish,correct=F) #no, p=0.6005

#no oxyconformity in larvae


#remove outliers and redo tests and plots and summary
lrv_danao<-lrv_dana
lrv_danao$Pcrit_break[c(2,3,4,5)]<-NA
lrv_danao$RMR[c(2,4,5,8,9)]<-NA

break_mod3<-aov((lrv_danao$Pcrit_break)~lrv_danao$CO2_level/factor(lrv_danao$Tank))
summary(break_mod3)
TukeyHSD(break_mod3)

dana_lrv_mdl3<-aov(sqrt(lrv_danao$RMR)~lrv_danao$CO2_level/factor(lrv_danao$Tank))
summary(dana_lrv_mdl3) #CO2 is significant p=4.46e-6
TukeyHSD(dana_lrv_mdl3)

shapiro.test(lrv_danao$Pcrit_break)
shapiro.test(lrv_danao$RMR)
leveneTest(lrv_danao$Pcrit_break~lrv_danao$CO2_level)
leveneTest((lrv_danao$RMR)~lrv_danao$CO2_level)

#means etc
library(plyr)
break_sum<-ddply(lrv_danao,"CO2_level",summarise,N=length(na.omit(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum 
flax_emb_sum<-ddply(lrv_danao,"CO2_level",summarise,N=length(na.omit(RMR)),MeanMO2=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
flax_emb_sum  

#plots
#plot the data - means and SEs
library(ggplot2)
library(grid)
danalrvpcritplot<-ggplot(break_sum, aes(x=CO2_level,y=MeanPcrit))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanPcrit-SE,ymax=MeanPcrit+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 1",x=0.5,y=0.96,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,2))+
  theme_classic()
print(danalrvpcritplot)

danalrvplot<-ggplot(dana_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 1",x=0.5,y=0.96,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,0.3))+
  theme_classic()
print(danalrvplot)
