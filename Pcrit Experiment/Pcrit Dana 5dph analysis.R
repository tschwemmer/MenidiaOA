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
  



#Trim the unusable parts from the original datasets for calc_mo2() analysis
#plot data for all live wells

#Plate1
plot(lrv_p1orig$A1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A3~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B3~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B6~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C3~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D5~lrv_p1orig$Time.Min.)
abline(v=c(270,300,450,480))
#blanks
plot(lrv_p1orig$A2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A6~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C6~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D3~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D6~lrv_p1orig$Time.Min.)
#Plate2
plot(lrv_p2orig$A2~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$A4~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$A5~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$A6~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B1~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B2~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B3~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B4~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C1~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C3~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C4~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C6~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D1~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D3~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D4~lrv_p2orig$Time.Min.)
abline(v=c(115,140))
#Blanks
par(mfrow=c(2,2))
plot(lrv_p2orig$A1~lrv_p2$Time.Min.)
plot(lrv_p2orig$A3~lrv_p2$Time.Min.)
plot(lrv_p2orig$B5~lrv_p2$Time.Min.)
plot(lrv_p2orig$B6~lrv_p2$Time.Min.)
plot(lrv_p2orig$C2~lrv_p2$Time.Min.)
plot(lrv_p2orig$C5~lrv_p2$Time.Min.)
plot(lrv_p2orig$D2~lrv_p2$Time.Min.)
plot(lrv_p2orig$D5~lrv_p2$Time.Min.)
plot(lrv_p2orig$D6~lrv_p2$Time.Min.)

#trim fluctuating parts out of only the ones that need it - parts that are due to temperature change mainly. 
#Plate 1
lrv_p1orig$A1[106:151]<-NA
lrv_p1orig$A1[450:565]<-NA
lrv_p1orig$A3[241:360]<-NA
lrv_p1orig$A4[151:181]<-NA
lrv_p1orig$B2[1:226]<-NA
lrv_p1orig$B2[360:420]<-NA
lrv_p1orig$B2[744:864]<-NA
lrv_p1orig$B6[241:744]<-NA
lrv_p1orig$C5[271:300]<-NA
lrv_p1orig$D1[1:390]<-NA
lrv_p1orig$D2[300:529]<-NA
lrv_p1orig$A2[1759:1819]<-NA
lrv_p1orig$C4[535]<-NA
lrv_p1orig$D6[774:864]<-NA
lrv_p1orig$D6[1312:1401]<-NA

#Plate 2
lrv_p2orig$A2[538:735]<-NA
lrv_p2orig$A4[1:31]<-NA
lrv_p2orig$A4[1094:1303]<-NA
lrv_p2orig$B4[527]<-NA
lrv_p2orig$C4[1:91]<-NA
lrv_p2orig$C4[531:556]<-NA
lrv_p2orig$C4[825:855]<-NA
lrv_p2orig$C4[1989:2257]<-NA
lrv_p2orig$A1[515:556]<-NA
lrv_p2orig$A3[526:601]<-NA
lrv_p2orig$B6[151:226]<-NA
lrv_p2orig$D2[631:735]<-NA
lrv_p2orig$D6[345:420]<-NA




#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
P1A1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("A1",times=23),rep("t3",times=23),rep("high",times=23))
#P1A2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("A2",times=23),rep("t3",times=23),rep("blankt3",times=23))
P1A3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("A3",times=23),rep("t3",times=23),rep("high",times=23))
P1A4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("A4",times=23),rep("t3",times=23),rep("high",times=23))
#P1A5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("A5",times=23),rep("t3",times=23),rep("blankt3",times=23))
#P1A6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("A6",times=23),rep("t4",times=23),rep("blankt4",times=23))
#P1B1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("B1",times=23),rep("t9",times=23),rep("blankt9",times=23))
P1B2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("B2",times=23),rep("t9",times=23),rep("high",times=23))
P1B3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("B3",times=23),rep("t9",times=23),rep("high",times=23))
#P1B4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("B4",times=23),rep("t9",times=23),rep("high",times=23))
#P1B5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("B5",times=23),rep("t9",times=23),rep("blankt9",times=23))
P1B6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("B6",times=23),rep("t4",times=23),rep("med",times=23))
P1C1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("C1",times=23),rep("t1",times=23),rep("amb",times=23))
#P1C2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("C2",times=23),rep("t1",times=23),rep("blankt1",times=23))
P1C3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("C3",times=23),rep("t1",times=23),rep("amb",times=23))
#P1C4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("C4",times=23),rep("t1",times=23),rep("blankt1",times=23))
P1C5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("C5",times=23),rep("t1",times=23),rep("amb",times=23))
#P1C6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("C6",times=23),rep("t4",times=23),rep("blankt4",times=23))
P1D1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("D1",times=23),rep("t7",times=23),rep("high",times=23))
P1D2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("D2",times=23),rep("t7",times=23),rep("high",times=23))
#P1D3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("D3",times=23),rep("t7",times=23),rep("blankt7",times=23))
P1D4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("D4",times=23),rep("t7",times=23),rep("high",times=23))
P1D5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("D5",times=23),rep("t7",times=23),rep("high",times=23))
#P1D6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=27.3),rep("D6",times=23),rep("t7",times=23),rep("blankt7",times=23))

#P2A1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("A1",times=23),rep("t2",time=23),rep("blankt2",time=23))
P2A2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("A2",time=23),rep("t2",time=23),rep("med",time=23))
#P2A3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("A3",time=23),rep("t2",time=23),rep("blankt2",time=23))
P2A4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("A4",time=23),rep("t2",time=23),rep("med",time=23))
P2A5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("A5",time=23),rep("t5",time=23),rep("med",time=23))
P2A6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("A6",time=23),rep("t5",time=23),rep("med",time=23))
P2B1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("B1",time=23),rep("t8",time=23),rep("amb",time=23))
P2B2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("B2",time=23),rep("t8",time=23),rep("amb",time=23))
P2B3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("B3",time=23),rep("t8",time=23),rep("amb",time=23))
P2B4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("B4",time=23),rep("t8",time=23),rep("amb",time=23))
#P2B5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("B5",time=23),rep("t8",time=23),rep("blankt8",time=23))
#P2B6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("B6",time=23),rep("t8",time=23),rep("blankt8",time=23))
P2C1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("C1",time=23),rep("t6",time=23),rep("amb",time=23))
#P2C2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("C2",time=23),rep("t6",time=23),rep("blankt6",time=23))
P2C3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("C3",time=23),rep("t6",time=23),rep("amb",time=23))
P2C4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("C4",time=23),rep("t6",time=23),rep("amb",time=23))
#P2C5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("C5",time=23),rep("t6",time=23),rep("blankt6",time=23))
#P2C6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("C6",time=23),rep("t4",time=23),rep("med",time=23))
P2D1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("D1",time=23),rep("t5",time=23),rep("med",time=23))
#P2D2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("D2",time=23),rep("t5",time=23),rep("blankt5",time=23))
P2D3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("D3",time=23),rep("t5",time=23),rep("med",time=23))
P2D4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("D4",time=23),rep("t5",time=23),rep("med",time=23))
#P2D5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("D5",time=23),rep("t5",time=23),rep("blankt5",time=23))
#P2D6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=27.3),rep("D6",time=23),rep("t4",time=23),rep("blankt4",time=23))

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
P1A1$dw<-rep(0.06247091,times=23)
P1A3$dw<-rep(0.18165058,times=23)
P1A4$dw<-rep(0.11295278,times=23)
P1B2$dw<-rep(0.09439715,times=23)
P1B3$dw<-rep(0.27994570,times=23)
P1B6$dw<-rep(0.14755564,times=23)
P1C1$dw<-rep(0.08686671,times=23)
P1C3$dw<-rep(0.05235767,times=23)
P1C5$dw<-rep(0.08677274,times=23)
P1D1$dw<-rep(0.10472544,times=23)
P1D2$dw<-rep(0.04906919,times=23)
P1D4$dw<-rep(0.13807107,times=23)
P1D5$dw<-rep(0.06449025,times=23)
P2A2$dw<-rep(0.06672954,times=23)
P2A4$dw<-rep(0.07941095,times=23)
P2A5$dw<-rep(0.11768010,times=23)
P2A6$dw<-rep(0.22495392,times=23)
P2B1$dw<-rep(0.20566076,times=23)
P2B2$dw<-rep(0.06196151,times=23)
P2B3$dw<-rep(0.10707247,times=23)
P2B4$dw<-rep(0.29503372,times=23)
P2C1$dw<-rep(0.17521464,times=23)
P2C3$dw<-rep(0.07373983,times=23)
P2C4$dw<-rep(0.05529619,times=23)
P2D1$dw<-rep(0.22639415,times=23)
P2D3$dw<-rep(0.10075909,times=23)
P2D4$dw<-rep(0.12439017,times=23)

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


#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES?? Fit a line to all of the points from the same treatment/tank. 
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
alllrv<-rbind(P1A1,P1A3,P1A4,P1B2,P1B3,P1B6,P1C1,P1C3,P1C5,P1D1,P1D2,P1D4,P1D5,
              P2A2,P2A4,P2A5,P2A6,P2B1,P2B2,P2B3,P2B4,P2C1,P2C3,P2C4,P2D1,P2D3,P2D4)

#plot the curves
library(ggplot2)
par(mfrow=c(1,1))
allplotlrv<-ggplot(alllrv, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlrv)


#Try plotting one at a time, grouped by treatment
library(gridExtra)
#Ambient: P1C1, P1C3, P1C5, P2B1, P2B2, P2B3, P2B4, P2C1, P2C3, P2C4
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
P2B1plot<-ggplot(P2B1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B2plot<-ggplot(P2B2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B3plot<-ggplot(P2B3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B4plot<-ggplot(P2B4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C1plot<-ggplot(P2C1,aes(x=O2_MEAN,y=MO2))+
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
grid.arrange(P1C1plot,P1C3plot,P1C5plot,P2B1plot,P2B2plot,P2B3plot,P2B4plot,P2C1plot,P2C3plot,P2C4plot,ncol=5)

#Medium: P1B6, P2A2, P2A4, P2A5, P2A6, P2D1, P2D3, P2D4
P1B6plot<-ggplot(P1B6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A2plot<-ggplot(P2A2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A4plot<-ggplot(P2A4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A5plot<-ggplot(P2A5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2A6plot<-ggplot(P2A6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D1plot<-ggplot(P2D1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D3plot<-ggplot(P2D3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D4plot<-ggplot(P2D4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1B6plot,P2A2plot,P2A4plot,P2A5plot,P2A6plot,P2D1plot,P2D3plot,P2D4plot,ncol=4)

#High: P1A1, P1A3, P1A4, P1B2, P1B3, P1D1, P1D2, P1D4, P1D5
P1A1plot<-ggplot(P1A1,aes(x=O2_MEAN,y=MO2))+
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
P1B2plot<-ggplot(P1B2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B3plot<-ggplot(P1B3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1D1plot<-ggplot(P1D1,aes(x=O2_MEAN,y=MO2))+
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
grid.arrange(P1A1plot,P1A3plot,P1A4plot,P1B2plot,P1B3plot,P1D1plot,P1D2plot,P1D4plot,P1D5plot,ncol=3)


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


#set levels of CO2_level and Tank
lrv_dana$CO2_level<-factor(lrv_dana$CO2_level,levels=c("amb","med","high"))
lrv_dana$Tank<-factor(lrv_dana$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))

plot(lrv_dana$Pcrit_alpha~lrv_dana$CO2_level)
plot(lrv_dana$Pcrit_break~lrv_dana$CO2_level)
plot(lrv_dana$alpha~lrv_dana$CO2_level)

#check if different Pcrit metrics are correlated
plot(lrv_dana$Pcrit_alpha~lrv_dana$Pcrit_break) #Cone shaped. 

#calculate mean and SE of Pcrit values by treatment
library(plyr)
alpha_sum<-ddply(lrv_dana,"CO2_level",summarise,N=length(Pcrit_alpha),MeanPcrit=mean(Pcrit_alpha,na.rm=TRUE),SE=sd(Pcrit_alpha,na.rm=TRUE)/sqrt(N))
alpha_sum

break_sum<-ddply(lrv_dana,"CO2_level",summarise,N=length(Pcrit_break),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers

a_sum<-ddply(lrv_dana,"CO2_level",summarise,N=length(alpha),MeanPcrit=mean(alpha,na.rm=TRUE),SE=sd(alpha,na.rm=TRUE)/sqrt(N))
a_sum

#use lm and anova to test for significance
library(lmerTest)
library(lme4)

alpha_mod<-lmer(Pcrit_alpha~CO2_level+1|Tank,data=lrv_dana) #singular fit
ranova(alpha_mod)

alpha_mod1<-lm(Pcrit_alpha~CO2_level,data=lrv_dana)
anova(alpha_mod1) #not significant, p=0.5533

break_mod<-lmer(Pcrit_break~CO2_level+1|Tank,data=lrv_dana) #singular fit
ranova(break_mod)

break_mod1<-lm(Pcrit_break~CO2_level,data=lrv_dana)
anova(break_mod1) #ditto, p=0.1159

a_mod<-lm(alpha~CO2_level,data=lrv_dana)
anova(a_mod)


