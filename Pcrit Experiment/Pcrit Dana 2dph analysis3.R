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
#remove first 10 minutes and parts with extremely high temperature change
#first check temperature in 10 min bins
library(tibble)
library(zoo)
lar_p1temp<-data.frame(lar_p1$T_internal,lar_p1$Time.Min.)
names(lar_p1temp)<-c("T_internal","Time.Min.")
Coef<-function(Z) coef(lm(T_internal~Time.Min., as.data.frame(Z)))[2]
lar_p1slope<-rollapplyr(zoo(lar_p1temp), width=30, Coef,by=30,by.column=FALSE, align="center")
lar_p1slope1<-as.data.frame(lar_p1slope)
lar_p1slope1<-rownames_to_column(lar_p1slope1, "Index")

lar_p2temp<-data.frame(lar_p2$T_internal,lar_p2$Time.Min.)
names(lar_p2temp)<-c("T_internal","Time.Min.")
lar_p2slope<-rollapplyr(zoo(lar_p2temp), width=30, Coef,by=30,by.column=FALSE, align="center")
lar_p2slope1<-as.data.frame(lar_p2slope)
lar_p2slope1<-rownames_to_column(lar_p2slope1, "Index")


#now identify the sections that have greater than 0.5C/h slope (0.0083C/min)
lar_p1slope1$Index[abs(lar_p1slope1$lar_p1slope)>0.0083] #15, 45, and 75 are too high so remove first 90 rows
lar_p2slope1$Index[abs(lar_p2slope1$lar_p2slope)>0.0083] #15, 45, 75, and 135 are too high so remove first 150 rows

lar_p1orig<-lar_p1
lar_p2orig<-lar_p2

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


lar_p1orig<-lar_p1orig[91:3657,]
lar_p2orig<-lar_p2orig[151:3647,]
row.names(lar_p1orig)<-NULL
row.names(lar_p2orig)<-NULL


#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)

#Use make_bins() to set bin width but use different rows based on when it bottoms out. Use increments of 50. 
#P1A1bin<-make_bins(o2=lar_p1orig$A1[1:500],duration=lar_p1orig$Time.Min.[1:500],max_o2_width=1/10,min_o2_width=1/30)
P1A2bin<-make_bins(o2=lar_p1orig$A2[1:1220],duration=lar_p1orig$Time.Min.[1:1220],max_o2_width=1/10,min_o2_width=1/30)
P1A3bin<-make_bins(o2=lar_p1orig$A3[1:921],duration=lar_p1orig$Time.Min.[1:921],max_o2_width=1/10,min_o2_width=1/30)
P1A4bin<-make_bins(o2=lar_p1orig$A4[1:2116],duration=lar_p1orig$Time.Min.[1:2116],max_o2_width=1/10,min_o2_width=1/30)
P1A5bin<-make_bins(o2=lar_p1orig$A5[1:1220],duration=lar_p1orig$Time.Min.[1:1220],max_o2_width=1/10,min_o2_width=1/30)
#P1A6bin<-make_bins(o2=lar_p1orig$A6[1:550],duration=lar_p1orig$Time.Min.[1:550],max_o2_width=1/10,min_o2_width=1/30)
P1B1bin<-make_bins(o2=lar_p1orig$B1[1:1817],duration=lar_p1orig$Time.Min.[1:1817],max_o2_width=1/10,min_o2_width=1/30)
P1B2bin<-make_bins(o2=lar_p1orig$B2[1:1220],duration=lar_p1orig$Time.Min.[1:1220],max_o2_width=1/10,min_o2_width=1/30)
#P1B3bin<-make_bins(o2=lar_p1orig$B3[1:1100],duration=lar_p1orig$Time.Min.[1:1100],max_o2_width=1/10,min_o2_width=1/30)
P1B4bin<-make_bins(o2=lar_p1orig$B4[1:1220],duration=lar_p1orig$Time.Min.[1:1220],max_o2_width=1/10,min_o2_width=1/30)
P1B5bin<-make_bins(o2=lar_p1orig$B5[1:1220],duration=lar_p1orig$Time.Min.[1:1220],max_o2_width=1/10,min_o2_width=1/30)
#P1B6bin<-make_bins(o2=lar_p1orig$B6[1:650],duration=lar_p1orig$Time.Min.[1:650],max_o2_width=1/10,min_o2_width=1/30)
P1C1bin<-make_bins(o2=lar_p1orig$C1[1:1369],duration=lar_p1orig$Time.Min.[1:1369],max_o2_width=1/10,min_o2_width=1/30)
#P1C2bin<-make_bins(o2=lar_p1orig$C2[1:600],duration=lar_p1orig$Time.Min.[1:600],max_o2_width=1/10,min_o2_width=1/30)
P1C3bin<-make_bins(o2=lar_p1orig$C3[1:1369],duration=lar_p1orig$Time.Min.[1:1369],max_o2_width=1/10,min_o2_width=1/30)
#P1C4bin<-make_bins(o2=lar_p1orig$C4[1:1000],duration=lar_p1orig$Time.Min.[1:1000],max_o2_width=1/10,min_o2_width=1/30)
P1C5bin<-make_bins(o2=lar_p1orig$C5[1:771],duration=lar_p1orig$Time.Min.[1:771],max_o2_width=1/10,min_o2_width=1/30)
P1C6bin<-make_bins(o2=lar_p1orig$C6[1:1070],duration=lar_p1orig$Time.Min.[1:1070],max_o2_width=1/10,min_o2_width=1/30)
#P1D1bin<-make_bins(o2=lar_p1orig$D1[1:750],duration=lar_p1orig$Time.Min.[1:750],max_o2_width=1/10,min_o2_width=1/30)
P1D2bin<-make_bins(o2=lar_p1orig$D2[1:1369],duration=lar_p1orig$Time.Min.[1:1369],max_o2_width=1/10,min_o2_width=1/30)
#P1D3bin<-make_bins(o2=lar_p1orig$D3[1:750],duration=lar_p1orig$Time.Min.[1:750],max_o2_width=1/10,min_o2_width=1/30)
P1D4bin<-make_bins(o2=lar_p1orig$D4[1:1369],duration=lar_p1orig$Time.Min.[1:1369],max_o2_width=1/10,min_o2_width=1/30)
P1D5bin<-make_bins(o2=lar_p1orig$D5[1:2265],duration=lar_p1orig$Time.Min.[1:2265],max_o2_width=1/10,min_o2_width=1/30)
P1D6bin<-make_bins(o2=lar_p1orig$D6[1:1668],duration=lar_p1orig$Time.Min.[1:1668],max_o2_width=1/10,min_o2_width=1/30)

P2A1bin<-make_bins(o2=lar_p2orig$A1[1:1299],duration=lar_p2orig$Time.Min.[1:1299],max_o2_width=1/10,min_o2_width=1/30)
#P2A2bin<-make_bins(o2=lar_p2orig$A2[1:800],duration=lar_p2orig$Time.Min.[1:800],max_o2_width=1/10,min_o2_width=1/30)
P2A3bin<-make_bins(o2=lar_p2orig$A3[1:1449],duration=lar_p2orig$Time.Min.[1:1449],max_o2_width=1/10,min_o2_width=1/30)
#P2A4bin<-make_bins(o2=lar_p2orig$A4[1:850],duration=lar_p2orig$Time.Min.[1:850],max_o2_width=1/10,min_o2_width=1/30)
P2A5bin<-make_bins(o2=lar_p2orig$A5[1:1299],duration=lar_p2orig$Time.Min.[1:1299],max_o2_width=1/10,min_o2_width=1/30)
#P2A6bin<-make_bins(o2=lar_p2orig$A6[1:800],duration=lar_p2orig$Time.Min.[1:800],max_o2_width=1/10,min_o2_width=1/30)
P2B1bin<-make_bins(o2=lar_p2orig$B1[1:1897],duration=lar_p2orig$Time.Min.[1:1897],max_o2_width=1/10,min_o2_width=1/30)
P2B2bin<-make_bins(o2=lar_p2orig$B2[1:1747],duration=lar_p2orig$Time.Min.[1:1747],max_o2_width=1/10,min_o2_width=1/30)
#P2B3bin<-make_bins(o2=lar_p2orig$B3[1:650],duration=lar_p2orig$Time.Min.[1:650],max_o2_width=1/10,min_o2_width=1/30)
P2B4bin<-make_bins(o2=lar_p2orig$B4[1:1150],duration=lar_p2orig$Time.Min.[1:1150],max_o2_width=1/10,min_o2_width=1/30)
#P2B5bin<-make_bins(o2=lar_p2orig$B5[1:500],duration=lar_p2orig$Time.Min.[1:500],max_o2_width=1/10,min_o2_width=1/30)
P2B6bin<-make_bins(o2=lar_p2orig$B6[1:2046],duration=lar_p2orig$Time.Min.[1:2046],max_o2_width=1/10,min_o2_width=1/30)
#P2C1bin<-make_bins(o2=lar_p2orig$C1[1:300],duration=lar_p2orig$Time.Min.[1:300],max_o2_width=1/10,min_o2_width=1/30)
#P2C2bin<-make_bins(o2=lar_p2orig$C2[1:750],duration=lar_p2orig$Time.Min.[1:750],max_o2_width=1/10,min_o2_width=1/30)
P2C3bin<-make_bins(o2=lar_p2orig$C3[1:1449],duration=lar_p2orig$Time.Min.[1:1449],max_o2_width=1/10,min_o2_width=1/30)
P2C4bin<-make_bins(o2=lar_p2orig$C4[1:1150],duration=lar_p2orig$Time.Min.[1:1150],max_o2_width=1/10,min_o2_width=1/30)
P2C5bin<-make_bins(o2=lar_p2orig$C5[1:1449],duration=lar_p2orig$Time.Min.[1:1449],max_o2_width=1/10,min_o2_width=1/30)
P2C6bin<-make_bins(o2=lar_p2orig$C6[1:1150],duration=lar_p2orig$Time.Min.[1:1150],max_o2_width=1/10,min_o2_width=1/30)
P2D1bin<-make_bins(o2=lar_p2orig$D1[1:1598],duration=lar_p2orig$Time.Min.[1:1598],max_o2_width=1/10,min_o2_width=1/30)
#P2D2bin<-make_bins(o2=lar_p2orig$D2[1:700],duration=lar_p2orig$Time.Min.[1:700],max_o2_width=1/10,min_o2_width=1/30)
#P2D3bin<-make_bins(o2=lar_p2orig$D3[1:750],duration=lar_p2orig$Time.Min.[1:750],max_o2_width=1/10,min_o2_width=1/30)
P2D4bin<-make_bins(o2=lar_p2orig$D4[1:1747],duration=lar_p2orig$Time.Min.[1:1747],max_o2_width=1/10,min_o2_width=1/30)
P2D5bin<-make_bins(o2=lar_p2orig$D5[1:701],duration=lar_p2orig$Time.Min.[1:701],max_o2_width=1/10,min_o2_width=1/30)
#P2D6bin<-make_bins(o2=lar_p2orig$D6[1:600],duration=lar_p2orig$Time.Min.[1:600],max_o2_width=1/10,min_o2_width=1/30)


#P1A1<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A1,o2_unit="mg_per_l",bin_width=P1A1bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A1"),rep("t2"),rep("blankt2"))
P1A2<-data.frame(calc_MO2(duration=lar_p1orig[1:1220,]$Time.Min.,o2=lar_p1orig[1:1220,]$A2,o2_unit="mg_per_l",bin_width=P1A2bin,vol=0.0006,temp=lar_p1orig[1:1220,]$T_internal,sal=28.5),rep("A2"),rep("t2"),rep("med"))
P1A3<-data.frame(calc_MO2(duration=lar_p1orig[1:921,]$Time.Min.,o2=lar_p1orig[1:921,]$A3,o2_unit="mg_per_l",bin_width=P1A3bin,vol=0.0006,temp=lar_p1orig[1:921,]$T_internal,sal=28.5),rep("A3"),rep("t2"),rep("med"))
P1A4<-data.frame(calc_MO2(duration=lar_p1orig[1:2116,]$Time.Min.,o2=lar_p1orig[1:2116,]$A4,o2_unit="mg_per_l",bin_width=P1A4bin,vol=0.0006,temp=lar_p1orig[1:2116,]$T_internal,sal=28.5),rep("A4"),rep("t2"),rep("med"))
P1A5<-data.frame(calc_MO2(duration=lar_p1orig[1:1220,]$Time.Min.,o2=lar_p1orig[1:1220,]$A5,o2_unit="mg_per_l",bin_width=P1A5bin,vol=0.0006,temp=lar_p1orig[1:1220,]$T_internal,sal=28.5),rep("A5"),rep("t2"),rep("med"))
#P1A6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$A6,o2_unit="mg_per_l",bin_width=P1A6bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("A6"),rep("t2"),rep("blankt2"))
P1B1<-data.frame(calc_MO2(duration=lar_p1orig[1:1817,]$Time.Min.,o2=lar_p1orig[1:1817,]$B1,o2_unit="mg_per_l",bin_width=P1B1bin,vol=0.0006,temp=lar_p1orig[1:1817,]$T_internal,sal=28.5),rep("B1"),rep("t8"),rep("amb"))
P1B2<-data.frame(calc_MO2(duration=lar_p1orig[1:1220,]$Time.Min.,o2=lar_p1orig[1:1220,]$B2,o2_unit="mg_per_l",bin_width=P1B2bin,vol=0.0006,temp=lar_p1orig[1:1220,]$T_internal,sal=28.5),rep("B2"),rep("t8"),rep("amb"))
#P1B3<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B3,o2_unit="mg_per_l",bin_width=P1B3bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B3"),rep("t8"),rep("blankt8"))
P1B4<-data.frame(calc_MO2(duration=lar_p1orig[1:1220,]$Time.Min.,o2=lar_p1orig[1:1220,]$B4,o2_unit="mg_per_l",bin_width=P1B4bin,vol=0.0006,temp=lar_p1orig[1:1220,]$T_internal,sal=28.5),rep("B4"),rep("t8"),rep("amb"))
P1B5<-data.frame(calc_MO2(duration=lar_p1orig[1:1220,]$Time.Min.,o2=lar_p1orig[1:1220,]$B5,o2_unit="mg_per_l",bin_width=P1B5bin,vol=0.0006,temp=lar_p1orig[1:1220,]$T_internal,sal=28.5),rep("B5"),rep("t8"),rep("amb"))
#P1B6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$B6,o2_unit="mg_per_l",bin_width=P1B6bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("B6"),rep("t8"),rep("blankt8"))
P1C1<-data.frame(calc_MO2(duration=lar_p1orig[1:1369,]$Time.Min.,o2=lar_p1orig[1:1369,]$C1,o2_unit="mg_per_l",bin_width=P1C1bin,vol=0.0006,temp=lar_p1orig[1:1369,]$T_internal,sal=28.5),rep("C1"),rep("t7"),rep("high"))
#P1C2<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C2,o2_unit="mg_per_l",bin_width=P1C2bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C2"),rep("t7"),rep("blankt7"))
P1C3<-data.frame(calc_MO2(duration=lar_p1orig[1:1369,]$Time.Min.,o2=lar_p1orig[1:1369,]$C3,o2_unit="mg_per_l",bin_width=P1C3bin,vol=0.0006,temp=lar_p1orig[1:1369,]$T_internal,sal=28.5),rep("C3"),rep("t7"),rep("high"))
#P1C4<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$C4,o2_unit="mg_per_l",bin_width=P1C4bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("C4"),rep("t7"),rep("blankt7"))
P1C5<-data.frame(calc_MO2(duration=lar_p1orig[1:771,]$Time.Min.,o2=lar_p1orig[1:771,]$C5,o2_unit="mg_per_l",bin_width=P1C5bin,vol=0.0006,temp=lar_p1orig[1:771,]$T_internal,sal=28.5),rep("C5"),rep("t7"),rep("high"))
P1C6<-data.frame(calc_MO2(duration=lar_p1orig[1:1070,]$Time.Min.,o2=lar_p1orig[1:1070,]$C6,o2_unit="mg_per_l",bin_width=P1C6bin,vol=0.0006,temp=lar_p1orig[1:1070,]$T_internal,sal=28.5),rep("C6"),rep("t7"),rep("high"))
#P1D1<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D1,o2_unit="mg_per_l",bin_width=P1D1bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D1"),rep("t1"),rep("blankt1"))
P1D2<-data.frame(calc_MO2(duration=lar_p1orig[1:1369,]$Time.Min.,o2=lar_p1orig[1:1369,]$D2,o2_unit="mg_per_l",bin_width=P1D2bin,vol=0.0006,temp=lar_p1orig[1:1369,]$T_internal,sal=28.5),rep("D2"),rep("t1"),rep("amb"))
#P1D3<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D3,o2_unit="mg_per_l",bin_width=P1D3bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D3"),rep("t1"),rep("blankt1"))
P1D4<-data.frame(calc_MO2(duration=lar_p1orig[1:1369,]$Time.Min.,o2=lar_p1orig[1:1369,]$D4,o2_unit="mg_per_l",bin_width=P1D4bin,vol=0.0006,temp=lar_p1orig[1:1369,]$T_internal,sal=28.5),rep("D4"),rep("t1"),rep("amb"))
P1D5<-data.frame(calc_MO2(duration=lar_p1orig[1:2265,]$Time.Min.,o2=lar_p1orig[1:2265,]$D5,o2_unit="mg_per_l",bin_width=P1D5bin,vol=0.0006,temp=lar_p1orig[1:2265,]$T_internal,sal=28.5),rep("D5"),rep("t1"),rep("amb"))
#P1D6<-data.frame(calc_MO2(duration=lar_p1orig[1:2235,]$Time.Min.,o2=lar_p1orig[1:2235,]$D6,o2_unit="mg_per_l",bin_width=P1D6bin,vol=0.0006,temp=lar_p1orig[1:2235,]$T_internal,sal=28.5),rep("D6"),rep("t3"),rep("high"))

P2A1<-data.frame(calc_MO2(duration=lar_p2orig[1:1299,]$Time.Min.,o2=lar_p2orig[1:1299,]$A1,o2_unit="mg_per_l",bin_width=P2A1bin,vol=0.0005,temp=lar_p2orig[1:1299,]$T_internal,sal=28.5),rep("A1"),rep("t4"),rep("med"))
#P2A2<-data.frame(calc_MO2(duration=lar_p2orig[1:800,]$Time.Min.,o2=lar_p2orig[1:800,]$A2,o2_unit="mg_per_l",bin_width=P2A2bin,vol=0.0005,temp=lar_p2orig[1:800,]$T_internal,sal=28.5),rep("A2"),rep("t4"),rep("blankt4"))
P2A3<-data.frame(calc_MO2(duration=lar_p2orig[1:1449,]$Time.Min.,o2=lar_p2orig[1:1449,]$A3,o2_unit="mg_per_l",bin_width=P2A3bin,vol=0.0005,temp=lar_p2orig[1:1449,]$T_internal,sal=28.5),rep("A3"),rep("t4"),rep("med"))
#P2A4<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A4,o2_unit="mg_per_l",bin_width=P2A4bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A4"),rep("t4"),rep("blankt4"))
P2A5<-data.frame(calc_MO2(duration=lar_p2orig[1:1299,]$Time.Min.,o2=lar_p2orig[1:1299,]$A5,o2_unit="mg_per_l",bin_width=P2A5bin,vol=0.0005,temp=lar_p2orig[1:1299,]$T_internal,sal=28.5),rep("A5"),rep("t4"),rep("med"))
#P2A6<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$A6,o2_unit="mg_per_l",bin_width=P2A6bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("A6"),rep("t3"),rep("blankt3"))
P2B1<-data.frame(calc_MO2(duration=lar_p2orig[1:1897,]$Time.Min.,o2=lar_p2orig[1:1897,]$B1,o2_unit="mg_per_l",bin_width=P2B1bin,vol=0.0005,temp=lar_p2orig[1:1897,]$T_internal,sal=28.5),rep("B1"),rep("t9"),rep("high"))
P2B2<-data.frame(calc_MO2(duration=lar_p2orig[1:1747,]$Time.Min.,o2=lar_p2orig[1:1747,]$B2,o2_unit="mg_per_l",bin_width=P2B2bin,vol=0.0005,temp=lar_p2orig[1:1747,]$T_internal,sal=28.5),rep("B2"),rep("t9"),rep("high"))
#P2B3<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B3,o2_unit="mg_per_l",bin_width=P2B3bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B3"),rep("t9"),rep("blankt9"))
P2B4<-data.frame(calc_MO2(duration=lar_p2orig[1:1150,]$Time.Min.,o2=lar_p2orig[1:1150,]$B4,o2_unit="mg_per_l",bin_width=P2B4bin,vol=0.0005,temp=lar_p2orig[1:1150,]$T_internal,sal=28.5),rep("B4"),rep("t9"),rep("high"))
#P2B5<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$B5,o2_unit="mg_per_l",bin_width=P2B5bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("B5"),rep("t9"),rep("blankt9"))
P2B6<-data.frame(calc_MO2(duration=lar_p2orig[1:2046,]$Time.Min.,o2=lar_p2orig[1:2046,]$B6,o2_unit="mg_per_l",bin_width=P2B6bin,vol=0.0005,temp=lar_p2orig[1:2046,]$T_internal,sal=28.5),rep("B6"),rep("t3"),rep("high"))
#P2C1<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C1,o2_unit="mg_per_l",bin_width=P2C1bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C1"),rep("t6"),rep("blankt6"))
#P2C2<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$C2,o2_unit="mg_per_l",bin_width=P2C2bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("C2"),rep("t6"),rep("blankt6"))
P2C3<-data.frame(calc_MO2(duration=lar_p2orig[1:1449,]$Time.Min.,o2=lar_p2orig[1:1449,]$C3,o2_unit="mg_per_l",bin_width=P2C3bin,vol=0.0005,temp=lar_p2orig[1:1449,]$T_internal,sal=28.5),rep("C3"),rep("t6"),rep("amb"))
P2C4<-data.frame(calc_MO2(duration=lar_p2orig[1:1150,]$Time.Min.,o2=lar_p2orig[1:1150,]$C4,o2_unit="mg_per_l",bin_width=P2C4bin,vol=0.0005,temp=lar_p2orig[1:1150,]$T_internal,sal=28.5),rep("C4"),rep("t6"),rep("amb"))
P2C5<-data.frame(calc_MO2(duration=lar_p2orig[1:1449,]$Time.Min.,o2=lar_p2orig[1:1449,]$C5,o2_unit="mg_per_l",bin_width=P2C5bin,vol=0.0005,temp=lar_p2orig[1:1449,]$T_internal,sal=28.5),rep("C5"),rep("t6"),rep("amb"))
P2C6<-data.frame(calc_MO2(duration=lar_p2orig[1:1150,]$Time.Min.,o2=lar_p2orig[1:1150,]$C6,o2_unit="mg_per_l",bin_width=P2C6bin,vol=0.0005,temp=lar_p2orig[1:1150,]$T_internal,sal=28.5),rep("C6"),rep("t3"),rep("high"))
P2D1<-data.frame(calc_MO2(duration=lar_p2orig[1:1598,]$Time.Min.,o2=lar_p2orig[1:1598,]$D1,o2_unit="mg_per_l",bin_width=P2D1bin,vol=0.0005,temp=lar_p2orig[1:1598,]$T_internal,sal=28.5),rep("D1"),rep("t5"),rep("med"))
#P2D2<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D2,o2_unit="mg_per_l",bin_width=P2D2bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D2"),rep("t5"),rep("blankt5"))
#P2D3<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D3,o2_unit="mg_per_l",bin_width=P2D3bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D3"),rep("t5"),rep("blankt5"))
P2D4<-data.frame(calc_MO2(duration=lar_p2orig[1:1747,]$Time.Min.,o2=lar_p2orig[1:1747,]$D4,o2_unit="mg_per_l",bin_width=P2D4bin,vol=0.0005,temp=lar_p2orig[1:1747,]$T_internal,sal=28.5),rep("D4"),rep("t5"),rep("med"))
P2D5<-data.frame(calc_MO2(duration=lar_p2orig[1:701,]$Time.Min.,o2=lar_p2orig[1:701,]$D5,o2_unit="mg_per_l",bin_width=P2D5bin,vol=0.0005,temp=lar_p2orig[1:701,]$T_internal,sal=28.5),rep("D5"),rep("t5"),rep("med5"))
#P2D6<-data.frame(calc_MO2(duration=lar_p2orig[1:2225,]$Time.Min.,o2=lar_p2orig[1:2225,]$D6,o2_unit="mg_per_l",bin_width=P2D6bin,vol=0.0005,temp=lar_p2orig[1:2225,]$T_internal,sal=28.5),rep("D6"),rep("t3"),rep("blankt3"))

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
P1A1$MO2b<-P1A1$MO2-t2blank
P1A2$MO2b<-P1A2$MO2-t2blank
P1A3$MO2b<-P1A3$MO2-t2blank
P1A4$MO2b<-P1A4$MO2-t2blank
P1A5$MO2b<-P1A5$MO2-t2blank
P1A6$MO2b<-P1A6$MO2-t2blank
P1B1$MO2b<-P1B1$MO2-t8blank
P1B2$MO2b<-P1B2$MO2-t8blank
P1B3$MO2b<-P1B3$MO2-t8blank
P1B4$MO2b<-P1B4$MO2-t8blank
P1B5$MO2b<-P1B5$MO2-t8blank
P1B6$MO2b<-P1B6$MO2-t8blank
P1C1$MO2b<-P1C1$MO2-t7blank
P1C2$MO2b<-P1C2$MO2-t7blank
P1C3$MO2b<-P1C3$MO2-t7blank
P1C4$MO2b<-P1C4$MO2-t7blank
P1C5$MO2b<-P1C5$MO2-t7blank
P1C6$MO2b<-P1C6$MO2-t7blank
P1D1$MO2b<-P1D1$MO2-t1blank
P1D2$MO2b<-P1D2$MO2-t1blank
P1D3$MO2b<-P1D3$MO2-t1blank
P1D4$MO2b<-P1D4$MO2-t1blank
P1D5$MO2b<-P1D5$MO2-t1blank
P1D6$MO2b<-P1D6$MO2-t1blank
P2A1$MO2b<-P2A1$MO2-t4blank
P2A2$MO2b<-P2A2$MO2-t4blank
P2A3$MO2b<-P2A3$MO2-t4blank
P2A4$MO2b<-P2A4$MO2-t4blank
P2A5$MO2b<-P2A5$MO2-t4blank
P2A6$MO2b<-P2A6$MO2-t4blank
P2B1$MO2b<-P2B1$MO2-t9blank
P2B2$MO2b<-P2B2$MO2-t9blank
P2B3$MO2b<-P2B3$MO2-t9blank
P2B4$MO2b<-P2B4$MO2-t9blank
P2B5$MO2b<-P2B5$MO2-t9blank
P2B6$MO2b<-P2B6$MO2-t3blank
P2C1$MO2b<-P2C1$MO2-t6blank
P2C2$MO2b<-P2C2$MO2-t6blank
P2C3$MO2b<-P2C3$MO2-t6blank
P2C4$MO2b<-P2C4$MO2-t6blank
P2C5$MO2b<-P2C5$MO2-t6blank
P2C6$MO2b<-P2C6$MO2-t3blank
P2D1$MO2b<-P2D1$MO2-t5blank
P2D2$MO2b<-P2D2$MO2-t5blank
P2D3$MO2b<-P2D3$MO2-t5blank
P2D4$MO2b<-P2D4$MO2-t5blank
P2D5$MO2b<-P2D5$MO2-t5blank
P2D6$MO2b<-P2D6$MO2-t5blank


#Add a column in each well's calc_mo2 output for mass and msmr. 
P1A2$dw<-rep(0.07123573)
P1A3$dw<-rep(0.12975554)
P1A4$dw<-rep(0.05992811)
P1A5$dw<-rep(0.08986283)
P1B1$dw<-rep(0.08148923)
P1B2$dw<-rep(0.10368688)
P1B4$dw<-rep(0.06112130)
P1B5$dw<-rep(0.06804286)
P1C1$dw<-rep(0.05410562)
P1C3$dw<-rep(0.04147655)
P1C5$dw<-rep(0.08680508)
P1C6$dw<-rep(0.08896861)
P1D2$dw<-rep(0.07113832)
P1D4$dw<-rep(0.09459529)
P1D5$dw<-rep(0.08894588)
P2A1$dw<-rep(0.06974729)
P2A3$dw<-rep(0.06576484)
P2A5$dw<-rep(0.05682256)
P2B1$dw<-rep(0.07687877)
P2B2$dw<-rep(0.08655282)
P2B4$dw<-rep(0.10198861)
P2B6$dw<-rep(0.06243424)
P2C3$dw<-rep(0.05377564)
P2C4$dw<-rep(0.06965129)
P2C5$dw<-rep(0.05939634)
P2C6$dw<-rep(0.05756485)
P2D1$dw<-rep(0.07261044)
P2D4$dw<-rep(0.08295252)
P2D5$dw<-rep(0.09544944)

P1A2$msmrs<-P1A2$MO2b/P1A2$dw
P1A3$msmrs<-P1A3$MO2b/P1A3$dw
P1A4$msmrs<-P1A4$MO2b/P1A4$dw
P1A5$msmrs<-P1A5$MO2b/P1A5$dw
P1B1$msmrs<-P1B1$MO2b/P1B1$dw
P1B2$msmrs<-P1B2$MO2b/P1B2$dw
P1B4$msmrs<-P1B4$MO2b/P1B4$dw
P1B5$msmrs<-P1B5$MO2b/P1B5$dw
P1C1$msmrs<-P1C1$MO2b/P1C1$dw
P1C3$msmrs<-P1C3$MO2b/P1C3$dw
P1C5$msmrs<-P1C5$MO2b/P1C5$dw
P1C6$msmrs<-P1C6$MO2b/P1C6$dw
P1D2$msmrs<-P1D2$MO2b/P1D2$dw
P1D4$msmrs<-P1D4$MO2b/P1D4$dw
P1D5$msmrs<-P1D5$MO2b/P1D5$dw
P2A1$msmrs<-P2A1$MO2b/P2A1$dw
P2A3$msmrs<-P2A3$MO2b/P2A3$dw
P2A5$msmrs<-P2A5$MO2b/P2A5$dw
P2B1$msmrs<-P2B1$MO2b/P2B1$dw
P2B2$msmrs<-P2B2$MO2b/P2B2$dw
P2B4$msmrs<-P2B4$MO2b/P2B4$dw
P2B6$msmrs<-P2B6$MO2b/P2B6$dw
P2C3$msmrs<-P2C3$MO2b/P2C3$dw
P2C4$msmrs<-P2C4$MO2b/P2C4$dw
P2C5$msmrs<-P2C5$MO2b/P2C5$dw
P2C6$msmrs<-P2C6$MO2b/P2C6$dw
P2D1$msmrs<-P2D1$MO2b/P2D1$dw
P2D4$msmrs<-P2D4$MO2b/P2D4$dw
P2D5$msmrs<-P2D5$MO2b/P2D5$dw

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
alllar<-rbind(P1A2,P1A3,P1A4,P1A5,P1B1,P1B2,P1B4,P1B5,P1C1,P1C3,P1C5,P1C6,P1D2,P1D4,P1D5,
              P2A1,P2A3,P2A5,P2B1,P2B2,P2B4,P2B6,P2C3,P2C4,P2C5,P2C6,P2D1,P2D4,P2D5)

#plot the curves
library(ggplot2)
allplotlar<-ggplot(alllar, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlar)

#check for extreme values
hist(P1A1$msmrs,breaks=10)
hist(P1A2$msmrs,breaks=10) #maybe two too low (-0.1)
hist(P1A3$msmrs,breaks=10)
hist(P1A4$msmrs,breaks=10) #maybe one too low (<-0.1)
hist(P1A5$msmrs,breaks=10) #maybe two too low (-0.1)
hist(P1A6$msmrs,breaks=10) 
hist(P1B1$msmrs,breaks=10)
hist(P1B2$msmrs,breaks=10)
hist(P1B3$msmrs,breaks=10)
hist(P1B4$msmrs,breaks=10) #a few too low (<-0.1)
hist(P1B5$msmrs,breaks=10) #one too high (>0.7) and a few too low (-0.1)
hist(P1B6$msmrs,breaks=10)
hist(P1C1$msmrs,breaks=10) #maybe one too high (>0.6)
hist(P1C2$msmrs,breaks=10)
hist(P1C3$msmrs,breaks=10) #several may be too low (-0.1)
hist(P1C4$msmrs,breaks=10)
hist(P1C5$msmrs,breaks=10) #two too high (>0.7) and one maybe too low (-0.1)
hist(P1C6$msmrs,breaks=10) #several too low (-0.1)
hist(P1D1$msmrs,breaks=10)
hist(P1D2$msmrs,breaks=10) #one is too low (<-0.1)
hist(P1D3$msmrs,breaks=10)
hist(P1D4$msmrs,breaks=10)
hist(P1D5$msmrs,breaks=10) #some may be too low (-0.1)
hist(P1D6$msmrs,breaks=10)
hist(P2A1$msmrs,breaks=10) #many may be too low (-0.1)
hist(P2A2$msmrs,breaks=10)
hist(P2A3$msmrs,breaks=10) #a couple maybe too low (-0.1)
hist(P2A4$msmrs,breaks=10)
hist(P2A5$msmrs,breaks=10) #one too high (>0.7) and many maybe too low (-0.1)
hist(P2A6$msmrs,breaks=10)
hist(P2B1$msmrs,breaks=10)
hist(P2B2$msmrs,breaks=10)
hist(P2B3$msmrs,breaks=10)
hist(P2B4$msmrs,breaks=10)
hist(P2B5$msmrs,breaks=10)
hist(P2B6$msmrs,breaks=10) #one may be too high (>0.7) and some too low (-0.1)
hist(P2C1$msmrs,breaks=10)
hist(P2C2$msmrs,breaks=10)
hist(P2C3$msmrs,breaks=10) #some maybe too low (-0.1)
hist(P2C4$msmrs,breaks=10) #some maybe too low (-0.1)
hist(P2C5$msmrs,breaks=10) #some maybe too low (-0.1)
hist(P2C6$msmrs,breaks=10) #some maybe too low (-0.1)
hist(P2D1$msmrs,breaks=10)
hist(P2D2$msmrs,breaks=10)
hist(P2D3$msmrs,breaks=10)
hist(P2D4$msmrs,breaks=10)
hist(P2D5$msmrs,breaks=10)
hist(P2D6$msmrs,breaks=10)

#Remove problematic data
row.names(P1A4)<-NULL
row.names(P1D2)<-NULL
row.names(P1D5)<-NULL
row.names(P2A5)<-NULL
row.names(P2B6)<-NULL
P1A4<-P1A4[-c(41),] #removed the extremely negative point because it was a fluctuation that should have been removed. 
P1D2<-P1D2[-c(20),] #removed the extremely negative point because it was a fluctuation that should have been removed.
P1D5<-P1D5[-c(15,60:71),] #removed the negative points because they were fluctuations that should have been removed.
P2A5<-P2A5[-c(10),] #removed the extreme high point because it was right before a fluctuation and may have been part of it. 
P2B6<-P2B6[-c(47),] #removed the extreme high point because it was right before a fluctuation and may have been part of it. 






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

###############################################################################
#Using selgmented function instead of calc_pcrit (segmented function) to get Pcrit
library(segmented)


P1A1seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A1),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P1A1)
plot(P1A1seg,add=T)
plot_pcrit(P1A1$O2_MEAN,P1A1$msmrs)
print(P1A1seg)

P1A2seg<-selgmented(lm(msmrs~O2_MEAN,data=P1A2),seg.Z=~O2_MEAN,type='bic',Kmax=5,msg=F)
plot(msmrs~O2_MEAN,data=P1A2)
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

P1B1seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B1),seg.Z=~O2_MEAN,type='bic',Kmax=6,msg=F)
plot(msmrs~O2_MEAN,data=P1B1)
plot(P1B1seg,add=T)
plot_pcrit(P1B1$O2_MEAN,P1B1$msmrs)
print(P1B1seg)

P1B2seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B2),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
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

P1B6seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B6),seg.Z=~O2_MEAN,type='bic',Kmax=7,msg=T)
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

P1C5seg<-selgmented(lm(msmrs~O2_MEAN,data=P1C5[9:84,]),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
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

P1D5seg<-selgmented(lm(msmrs~O2_MEAN,data=P1D5[-c(7,12,14,52:84),]),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
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

P2B3seg<-selgmented(lm(msmrs~O2_MEAN,data=P2B3),seg.Z=~O2_MEAN,type='bic',Kmax=2,msg=T)
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

P2C3seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C3),seg.Z=~O2_MEAN,type='bic',Kmax=6,msg=F)
plot(msmrs~O2_MEAN,data=P2C3)
plot(P2C3seg,add=T)
plot_pcrit(P2C3$O2_MEAN,P2C3$msmrs)
print(P2C3seg)

P2C4seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C4),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=F)
plot(msmrs~O2_MEAN,data=P2C4)
plot(P2C4seg,add=T)
plot_pcrit(P2C4$O2_MEAN,P2C4$msmrs)
print(P2C4seg)

P2C5seg<-selgmented(lm(msmrs~O2_MEAN,data=P2C5[1:28,]),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
plot(msmrs~O2_MEAN,data=P2C5[1:28,])
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
lar_dana$Pcrit_break<-c(1.52775,1.19425,2.3790,2.88619,  #P1A
                        1.83072,1.1312,2.349399,1.4106,  #P1B
                        1.1833,1.03390,0.2145,0.24651,  #P1C
                        2.69175,1.2264,3.247,  #P1D
                        1.15438,1.9210,1.54958,  #P2A
                        1.5563,2.2997,0.9771,2.3855,  #P2B
                        2.0959,1.3305,3.054,1.003,  #P2C
                        0.8220,1.0301,1.1585)  #P2D
lar_dana$spike<-c(1,1,1,1,  #P1A
                  1,1,1,1,  #P1B
                  1,1,1,0,  #P1C
                  1,1,1,  #P1D
                  1,1,1,  #P2A
                  1,1,1,0,  #P2B
                  1,1,1,1,  #P2C
                  1,1,0)  #P2D

################################################################################################
#Analyze, check model assumptions, and plot
#set levels of CO2_level and Tank
str(lar_dana)
lar_dana$CO2_level<-factor(lar_dana$CO2_level,levels=c("amb","med","high"))

plot(lar_dana$Pcrit_break~lar_dana$CO2_level)

#calculate mean and SE of Pcrit values by treatment
library(plyr)
break_sum<-ddply(lar_dana,"CO2_level",summarise,N=length(Pcrit_break),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

break_mod<-lmer(Pcrit_break~CO2_level+(1|Tank),data=lar_dana) 
ranova(break_mod) #tank doesn't affect fit, p=0.9962
anova(break_mod) #p=0.2222
summary(break_mod)

break_mod1<-lm(Pcrit_break~CO2_level,data=lar_dana)
anova(break_mod1) #p=0.06533
anova(break_mod,break_mod1) #this isn't significantly different than the lmer one so the random effect doesn't change results

#try it the other way
break_mod2<-aov(lar_dana$Pcrit_break~lar_dana$CO2_level/factor(lar_dana$Tank))
summary(break_mod2) #CO2 is significant (p=0.0488), tank is not (p=0.1727)

#break_mod3<-aov(lar_dana$Pcrit_break~lar_dana$CO2_level)
#summary(break_mod3)

#break_mod4<-aov(Pcrit_break~CO2_level+Error(Tank),data=lar_dana)
#summary(break_mod4)



TukeyHSD(break_mod2)

#diagnostics
par(mfrow=c(2,2))
plot(break_mod2) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(lar_dana$Pcrit_break) #looks good p=0.18

#homogeneity of variances
library(car)
leveneTest(lar_dana$Pcrit_break, lar_dana$CO2_level) #p=0.7317 good

#Check for outliers
cook<-cooks.distance(break_mod2)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(lar_dana$Pcrit_break)-2-1), col="red")
text(x=1:length(cook)+1,y=cook,labels=ifelse(cook>4/(length(lar_dana$Pcrit_break)-2-1),names(cook),""),col="red")
#Three outliers identified: 14, 22, 26
boxplot(lar_dana$Pcrit_break~lar_dana$CO2_level) #no outliers
sort(residuals(break_mod2))

#plot the data - means and SEs
library(ggplot2)
library(grid)
danalarpcritplot<-ggplot(break_sum, aes(x=CO2_level,y=MeanPcrit))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanPcrit-SE,ymax=MeanPcrit+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("2dph Larvae, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,3))+
  theme_classic()
print(danalarpcritplot)

###############################################################################################
#Calculate RMR as the average MO2 for which O2>Pcrit

lar_dana$RMR<-c(mean(c(P1A2$msmrs[P1A2$O2_MEAN>lar_dana[1,10]])),
                mean(c(P1A3$msmrs[P1A3$O2_MEAN>lar_dana[2,10]])),
                mean(c(P1A4$msmrs[P1A4$O2_MEAN>lar_dana[3,10]])),
                mean(c(P1A5$msmrs[P1A5$O2_MEAN>lar_dana[4,10]])),
                mean(c(P1B1$msmrs[P1B1$O2_MEAN>lar_dana[5,10]])),
                mean(c(P1B2$msmrs[P1B2$O2_MEAN>lar_dana[6,10]])),
                mean(c(P1B4$msmrs[P1B4$O2_MEAN>lar_dana[7,10]])),
                mean(c(P1B5$msmrs[P1B5$O2_MEAN>lar_dana[8,10]])),
                mean(c(P1C1$msmrs[P1C1$O2_MEAN>lar_dana[9,10]])),
                mean(c(P1C3$msmrs[P1C3$O2_MEAN>lar_dana[10,10]])),
                mean(c(P1C5$msmrs[P1C5$O2_MEAN>lar_dana[11,10]])),
                mean(c(P1C6$msmrs[P1C6$O2_MEAN>lar_dana[12,10]])),
                mean(c(P1D2$msmrs[P1D2$O2_MEAN>lar_dana[13,10]])),
                mean(c(P1D4$msmrs[P1D4$O2_MEAN>lar_dana[14,10]])),
                mean(c(P1D5$msmrs[P1D5$O2_MEAN>lar_dana[15,10]])),
                mean(c(P2A1$msmrs[P2A1$O2_MEAN>lar_dana[16,10]])),
                mean(c(P2A3$msmrs[P2A3$O2_MEAN>lar_dana[17,10]])),
                mean(c(P2A5$msmrs[P2A5$O2_MEAN>lar_dana[18,10]])),
                mean(c(P2B1$msmrs[P2B1$O2_MEAN>lar_dana[19,10]])),
                mean(c(P2B2$msmrs[P2B2$O2_MEAN>lar_dana[20,10]])),
                mean(c(P2B4$msmrs[P2B4$O2_MEAN>lar_dana[21,10]])),
                mean(c(P2B6$msmrs[P2B6$O2_MEAN>lar_dana[22,10]])),
                mean(c(P2C3$msmrs[P2C3$O2_MEAN>lar_dana[23,10]])),
                mean(c(P2C4$msmrs[P2C4$O2_MEAN>lar_dana[24,10]])),
                mean(c(P2C5$msmrs[P2C5$O2_MEAN>lar_dana[25,10]])),
                mean(c(P2C6$msmrs[P2C6$O2_MEAN>lar_dana[26,10]])),
                mean(c(P2D1$msmrs[P2D1$O2_MEAN>lar_dana[27,10]])),
                mean(c(P2D4$msmrs[P2D4$O2_MEAN>lar_dana[28,10]])),
                mean(c(P2D5$msmrs[P2D5$O2_MEAN>lar_dana[29,10]])))

#analyze RMR
dana_lar_model<-lm(lar_dana$RMR~lar_dana$CO2_level)
anova(dana_lar_model) #CO2 level is not significant, p=0.82

dana_lar_mod<-lmer(RMR~CO2_level+(1|Tank),data=lar_dana) #singular fit
anova(dana_lar_mod)
ranova(dana_lar_mod) #random effect of tank doesn't affect results. 

#try it the other way
dana_lar_mdl<-aov(1/(lar_dana$RMR)~lar_dana$CO2_level/factor(lar_dana$Tank))
summary(dana_lar_mdl) #no significance

#assumptions
shapiro.test(1/(lar_dana$RMR))
leveneTest(1/(lar_dana$RMR),lar_dana$CO2_level)

#Check for outliers
cook<-cooks.distance(dana_lar_mdl)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(lar_dana$RMR)-2-1), col="red")
text(x=1:length(cook)+1,y=cook,labels=ifelse(cook>4/(length(lar_dana$RMR)-2-1),names(cook),""),col="red")
#Two outliers: 5 and 15
boxplot(lar_dana$RMR~lar_dana$CO2_level)
sort(residuals(dana_lar_mdl))

#calculate the group means 

library(plyr)
dana_lar_rmrsum<-ddply(lar_dana,"CO2_level",summarise,N=length(na.omit(RMR)),MeanMO2=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
dana_lar_rmrsum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
danalarplot<-ggplot(dana_lar_rmrsum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("2dph Larvae, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,0.35))+
  theme_classic()
print(danalarplot)

pct_spike_amb<-100*sum(na.omit(lar_dana$spike[lar_dana$CO2_level=="amb"]))/length(na.omit(lar_dana$spike[lar_dana$CO2_level=="amb"]))
pct_spike_med<-100*sum(na.omit(lar_dana$spike[lar_dana$CO2_level=="med"]))/length(na.omit(lar_dana$spike[lar_dana$CO2_level=="med"]))
pct_spike_high<-100*sum(na.omit(lar_dana$spike[lar_dana$CO2_level=="high"]))/length(na.omit(lar_dana$spike[lar_dana$CO2_level=="high"]))

#test for significant differences in spike percentage between groups
spike<-c(10,9,7)
fish<-c(10,10,9)
prop.test(spike,fish,correct=F) #no, p=0.2831

#no oxyconformity in larvae

