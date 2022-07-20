#Respirometry analysis from 2021 experiment

#Run 6: 5dph Larvae, Flax Pond, 6-21-21

#load data sheets and treatments
lrv_p1orig<-read.csv(file.choose(),header=TRUE)
lrv_p2orig<-read.csv(file.choose(),header=TRUE)
lrv_trmt_p1<-read.csv(file.choose(),header=TRUE)
lrv_trmt_p2<-read.csv(file.choose(),header=TRUE)


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
lrv_trmt_p1$Tank<-factor(lrv_trmt_p1$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))
lrv_trmt_p2$Tank<-factor(lrv_trmt_p2$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))


#plot data for all live wells
par(mfrow=c(4,6))
#Plate1
plot(lrv_p1$A1~lrv_p1$Time.Min.)
plot(lrv_p1$A2~lrv_p1$Time.Min.)
plot(lrv_p1$A3~lrv_p1$Time.Min.)
plot(lrv_p1$A4~lrv_p1$Time.Min.)
plot(lrv_p1$B1~lrv_p1$Time.Min.)
plot(lrv_p1$B3~lrv_p1$Time.Min.)
plot(lrv_p1$B4~lrv_p1$Time.Min.)
plot(lrv_p1$B5~lrv_p1$Time.Min.)
plot(lrv_p1$B6~lrv_p1$Time.Min.)
plot(lrv_p1$C1~lrv_p1$Time.Min.)
plot(lrv_p1$C2~lrv_p1$Time.Min.)
plot(lrv_p1$C3~lrv_p1$Time.Min.)
plot(lrv_p1$C4~lrv_p1$Time.Min.)
plot(lrv_p1$C5~lrv_p1$Time.Min.)
plot(lrv_p1$D1~lrv_p1$Time.Min.)
plot(lrv_p1$D2~lrv_p1$Time.Min.)
plot(lrv_p1$D4~lrv_p1$Time.Min.)
plot(lrv_p1$D5~lrv_p1$Time.Min.)
plot(lrv_p1$D6~lrv_p1$Time.Min.)
abline(v=c(5,15,45,55,70,80,110))
#blanks
plot(lrv_p1$A6~lrv_p1$Time.Min.)
plot(lrv_p1$C6~lrv_p1$Time.Min.)
plot(lrv_p1$D3~lrv_p1$Time.Min.)
#Plate2
plot(lrv_p2$A4~lrv_p2$Time.Min.)
plot(lrv_p2$A5~lrv_p2$Time.Min.)
plot(lrv_p2$A6~lrv_p2$Time.Min.)
plot(lrv_p2$B2~lrv_p2$Time.Min.)
plot(lrv_p2$B3~lrv_p2$Time.Min.)
plot(lrv_p2$B4~lrv_p2$Time.Min.)
plot(lrv_p2$B5~lrv_p2$Time.Min.)
plot(lrv_p2$B6~lrv_p2$Time.Min.)
plot(lrv_p2$C1~lrv_p2$Time.Min.)
plot(lrv_p2$C3~lrv_p2$Time.Min.)
plot(lrv_p2$C4~lrv_p2$Time.Min.)
plot(lrv_p2$C5~lrv_p2$Time.Min.)
plot(lrv_p2$C6~lrv_p2$Time.Min.)
plot(lrv_p2$D1~lrv_p2$Time.Min.)
plot(lrv_p2$D2~lrv_p2$Time.Min.)
plot(lrv_p2$D3~lrv_p2$Time.Min.)
plot(lrv_p2$D5~lrv_p2$Time.Min.)
plot(lrv_p2$D6~lrv_p2$Time.Min.)
abline(v=c(5,35,50,70,100))
#Blanks
par(mfrow=c(2,2))
plot(lrv_p2$A3~lrv_p2$Time.Min.)
abline(v=c(5,35,50))
plot(lrv_p2$B1~lrv_p2$Time.Min.)
abline(v=c(5,35,50))
plot(lrv_p2$C2~lrv_p2$Time.Min.)
abline(v=c(5,35,50))
plot(lrv_p2$D4~lrv_p2$Time.Min.)
abline(v=c(5,35,50))

#Trim the data for the purpose of calculating the initial slope (RMR in fully oxygenated water)
lrv_p1_rmr<-data.frame(lrv_p1$Time.Min.[16:106],lrv_p1$Time.Sec.[16:106],lrv_p1$A1[16:106],lrv_p1$A2[16:106],lrv_p1$A4[16:106],
                       lrv_p1$B1[16:106],lrv_p1$B3[181:271],lrv_p1$B4[16:106],lrv_p1$B6[46:136],
                       lrv_p1$C1[16:106],lrv_p1$C3[16:106],lrv_p1$C4[16:106],lrv_p1$C5[16:106],
                       lrv_p1$D1[46:136],lrv_p1$D2[46:136],lrv_p1$D4[16:106],lrv_p1$D5[76:166],lrv_p1$D6[46:136],
                       lrv_p1$A5[16:106],lrv_p1$A6[16:106],lrv_p1$C6[211:301],lrv_p1$D3[211:301])
names(lrv_p1_rmr)<-c("Time.Min.","Time.Sec.","A1","A2","A4","B1","B3","B4","B6","C1","C3","C4","C5","D1","D2","D4","D5","D6","A5","A6","C6","D3")

lrv_p2_rmr<-data.frame(lrv_p2$Time.Min.[31:121],lrv_p2$Time.Sec.[31:121],lrv_p2$A4[31:121],lrv_p2$A5[16:106],lrv_p2$A6[31:121],
                       lrv_p2$B2[31:121],lrv_p2$B3[31:121],lrv_p2$B4[31:121],lrv_p2$B5[31:121],lrv_p2$B6[31:121],
                       lrv_p2$C1[31:121],lrv_p2$C3[61:151],lrv_p2$C4[61:151],lrv_p2$C5[31:121],lrv_p2$C6[31:121],
                       lrv_p2$D1[31:121],lrv_p2$D2[31:121],lrv_p2$D3[31:121],lrv_p2$D5[31:121],lrv_p2$D6[31:121],
                       lrv_p2$A3[61:151],lrv_p2$B1[61:151],lrv_p2$C2[61:151],lrv_p2$D4[61:151])
names(lrv_p2_rmr)<-c("Time.Min.","Time.Sec.","A4","A5","A6","B2","B3","B4","B5","B6","C1","C3","C4","C5","C6","D1","D2","D3","D5","D6","A3","B1","C2","D4")


slopes_r6p1_rmr<-data.frame(names(lrv_p1_rmr)[3:22], sapply(lrv_p1_rmr[3:22],function(x) ((-coef(summary(lm(x~lrv_p1_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lrv_p1_rmr[3:22],function(x) summary(lm(x~lrv_p1_rmr$Time.Sec.))$r.squared))
names(slopes_r6p1_rmr)<-c("Well","MO2","Rsquared")

slopes_r6p2_rmr<-data.frame(names(lrv_p2_rmr)[3:24], sapply(lrv_p2_rmr[3:24],function(x) ((-coef(summary(lm(x~lrv_p2_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lrv_p2_rmr[3:24],function(x) summary(lm(x~lrv_p2_rmr$Time.Sec.))$r.squared))
names(slopes_r6p2_rmr)<-c("Well","MO2","Rsquared")



#calculate the blanks
blank1<-mean(slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="A6"],slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="D3"],slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="A3"])
blank2<-mean(slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="A5"],slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="C6"],slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="B1"])
blank3<-mean(slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="C2"],slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="D4"])



#Subtract the blanks
lrv_p1b<-data.frame(c("A1","A2","A4","B1","B3","B4","B6","C1","C3","C4","C5","D1","D2","D4","D5","D6"),
                    c(slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="A1"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="A2"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="A4"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="B1"]-blank3,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="B3"]-blank3,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="B4"]-blank3,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="B6"]-blank3,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="C1"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="C3"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="C4"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="C5"]-blank2,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="D1"]-blank1,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="D2"]-blank1,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="D4"]-blank1,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="D5"]-blank1,
                      slopes_r6p1_rmr$MO2[slopes_r6p1_rmr$Well=="D6"]-blank1))
names(lrv_p1b)<-c("Well","MO2")
lrv_p2b<-data.frame(c("A4","A5","A6","B2","B3","B4","B5","B6","C1","C3","C4","C5","C6","D1","D2","D3","D5","D6"),
                    c(slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="A4"]-blank1,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="A5"]-blank1,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="A6"]-blank1,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="B2"]-blank2,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="B3"]-blank2,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="B4"]-blank2,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="B5"]-blank2,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="B6"]-blank2,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="C1"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="C3"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="C4"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="C5"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="C6"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="D1"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="D2"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="D3"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="D5"]-blank3,
                      slopes_r6p2_rmr$MO2[slopes_r6p2_rmr$Well=="D6"]-blank3))
names(lrv_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataset and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
lrv_p1b<-dplyr::full_join(lrv_p1b,lrv_trmt_p1,by="Well")
lrv_p1b<-lrv_p1b[1:16,] #remove the blanks
lrv_p2b<-dplyr::full_join(lrv_p2b,lrv_trmt_p2,by="Well")
lrv_p2b<-lrv_p2b[1:18,] #remove the blanks
lrv_flax<-rbind(lrv_p1b,lrv_p2b) #combine the two plates into one dataset. 
row.names(lrv_flax)<-NULL #renumber the rows so it's easy to index if need be.

#Add a column for mass-specific metabolic rate, MO2/dw in umol O2 / mg dw / h. 
lrv_flax<-data.frame(lrv_flax, "msmr"<-c(lrv_flax[1,2]/lrv_flax[1,5],
                                         lrv_flax[2,2]/lrv_flax[2,5],
                                         lrv_flax[3,2]/lrv_flax[3,5],
                                         lrv_flax[4,2]/lrv_flax[4,5],
                                         lrv_flax[5,2]/lrv_flax[5,5],
                                         lrv_flax[6,2]/lrv_flax[6,5],
                                         lrv_flax[7,2]/lrv_flax[7,5],
                                         lrv_flax[8,2]/lrv_flax[8,5],
                                         lrv_flax[9,2]/lrv_flax[9,5],
                                         lrv_flax[10,2]/lrv_flax[10,5],
                                         lrv_flax[11,2]/lrv_flax[11,5],
                                         lrv_flax[12,2]/lrv_flax[12,5],
                                         lrv_flax[13,2]/lrv_flax[13,5],
                                         lrv_flax[14,2]/lrv_flax[14,5],
                                         lrv_flax[15,2]/lrv_flax[15,5],
                                         lrv_flax[16,2]/lrv_flax[16,5],
                                         lrv_flax[17,2]/lrv_flax[17,5],
                                         lrv_flax[18,2]/lrv_flax[18,5],
                                         lrv_flax[19,2]/lrv_flax[19,5],
                                         lrv_flax[20,2]/lrv_flax[20,5],
                                         lrv_flax[21,2]/lrv_flax[21,5],
                                         lrv_flax[22,2]/lrv_flax[22,5],
                                         lrv_flax[23,2]/lrv_flax[23,5],
                                         lrv_flax[24,2]/lrv_flax[24,5],
                                         lrv_flax[25,2]/lrv_flax[25,5],
                                         lrv_flax[26,2]/lrv_flax[26,5],
                                         lrv_flax[27,2]/lrv_flax[27,5],
                                         lrv_flax[28,2]/lrv_flax[28,5],
                                         lrv_flax[29,2]/lrv_flax[29,5],
                                         lrv_flax[30,2]/lrv_flax[30,5],
                                         lrv_flax[31,2]/lrv_flax[31,5],
                                         lrv_flax[32,2]/lrv_flax[32,5],
                                         lrv_flax[33,2]/lrv_flax[33,5],
                                         lrv_flax[34,2]/lrv_flax[34,5]))
names(lrv_flax)[8]<-"msmr"

#analyze the MO2 with respect to CO2
flax_lrv_model<-lm(lrv_flax$msmr~lrv_flax$CO2_level)
anova(flax_lrv_model) #CO2 level is significant, p=0.0046

#do it with a LME/ANOVA with tank as random effect
library(lme4)
library(lmerTest)
flax_lrv_mod<-lmer(msmr~Tank+(1|CO2_level),data=lrv_flax) #This gave a warning message that I don't understand.  
ranova(flax_lrv_mod) #p=1 this confirms that the random effect can be excluded. 

#try it the other way
flax_lrv_mdl<-aov(lrv_flax$msmr~lrv_flax$CO2_level/factor(lrv_flax$Tank))
summary(flax_lrv_mdl) #CO2 is significant (p=0.00387), tank is not. 

#diagnostics


#calculate the group means 
library(plyr)
flax_lrv_sum<-ddply(lrv_flax,"CO2_level",summarise,N=length(msmr),MeanMO2=mean(msmr),SE=sd(msmr)/sqrt(N))
flax_lrv_sum #elevated CO2 slightly decreases MO2...opposite of previous results. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxlrvplot<-ggplot(flax_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  theme_classic()
print(flaxlrvplot)




#Trim the unusable parts from the original datasets for calc_mo2() analysis

#plot data for all live wells
par(mfrow=c(4,6))
#Plate1
plot(lrv_p1orig$A1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B3~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$B6~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C3~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D1~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D2~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D4~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D6~lrv_p1orig$Time.Min.)
abline(v=c(260,280))
#blanks
plot(lrv_p1orig$A5~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$A6~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$C6~lrv_p1orig$Time.Min.)
plot(lrv_p1orig$D3~lrv_p1orig$Time.Min.)
#Plate2
plot(lrv_p2orig$A4~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$A5~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$A6~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B2~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B3~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B4~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B5~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$B6~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C1~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C3~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C4~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C5~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$C6~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D1~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D2~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D3~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D5~lrv_p2orig$Time.Min.)
plot(lrv_p2orig$D6~lrv_p2orig$Time.Min.)
abline(v=c(10))
#Blanks
par(mfrow=c(2,2))
plot(lrv_p2orig$A3~lrv_p2$Time.Min.)
plot(lrv_p2orig$B1~lrv_p2$Time.Min.)
plot(lrv_p2orig$C2~lrv_p2$Time.Min.)
plot(lrv_p2orig$D4~lrv_p2$Time.Min.)

#trim fluctuating parts out of only the ones that need it - parts that are due to temperature change mainly. 
lrv_p1orig$A4[226:301]<-NA
lrv_p1orig$A4[751:871]<-NA
lrv_p1orig$B3[1:181]<-NA
lrv_p1orig$B3[451:511]<-NA
lrv_p1orig$B4[121:241]<-NA
lrv_p1orig$C3[601:721]<-NA
lrv_p1orig$D2[691:811]<-NA
lrv_p1orig$D5[1:75]<-NA
lrv_p1orig$D6[1111:1310]<-NA
lrv_p2orig$A6[1:30]<-NA
lrv_p2orig$C1[1141:1186]<-NA
lrv_p2orig$C1[1261:1310]<-NA
lrv_p2orig$D2[871:1310]<-NA
lrv_p2orig$D3[1:30]<-NA
lrv_p2orig$D3[1171:1310]<-NA
lrv_p2orig$D6[391:511]<-NA
lrv_p2orig$A3[1021:1126]<-NA
lrv_p2orig$B1[481:616]<-NA
lrv_p2orig$B1[1:16]<-NA
lrv_p2orig$C2[1:31]<-NA
lrv_p2orig$D4[1:31]<-NA
lrv_p2orig$D4[796:871]<-NA



#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
P1A1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("A1",times=11),rep("2A",times=11),rep("med",times=11))
P1A2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("A2",times=11),rep("2A",times=11),rep("med",times=11))
#P1A3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("A3",times=11),rep("2A",times=11),rep("med",times=11))
P1A4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("A4",times=11),rep("2A",times=11),rep("med",times=11))
P1A5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("A5",times=11),rep("2A",times=11),rep("blank2A",times=11))
P1A6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$A6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("A6",times=11),rep("1B",times=11),rep("blank1B",times=11))
P1B1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("B1",times=11),rep("3C",times=11),rep("high",times=11))
#P1B2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("B2",times=11),rep("3C",times=11),rep("blank3C",times=11))
P1B3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("B3",times=11),rep("3C",times=11),rep("high",times=11))
P1B4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("B4",times=11),rep("3C",times=11),rep("high",times=11))
#P1B5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("B5",times=11),rep("3C",times=11),rep("high",times=11))
#P1B6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$B6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("B6",times=11),rep("3C",times=11),rep("high",times=11))
P1C1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("C1",times=11),rep("2B",times=11),rep("med",times=11))
#P1C2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("C2",times=11),rep("2B",times=11),rep("med",times=11))
P1C3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("C3",times=11),rep("2B",times=11),rep("med",times=11))
P1C4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("C4",times=11),rep("2B",times=11),rep("med",times=11))
P1C5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("C5",times=11),rep("2B",times=11),rep("med",times=11))
P1C6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$C6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("C6",times=11),rep("2B",times=11),rep("blank2B",times=11))
P1D1<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("D1",times=11),rep("1C",times=11),rep("amb",times=11))
P1D2<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("D2",times=11),rep("1C",times=11),rep("amb",times=11))
P1D3<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("D3",times=11),rep("1C",times=11),rep("blank1C",times=11))
P1D4<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("D4",times=11),rep("1C",times=11),rep("amb",times=11))
P1D5<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("D5",times=11),rep("1C",times=11),rep("amb",times=11))
P1D6<-data.frame(calc_MO2(duration=lrv_p1orig$Time.Min.,o2=lrv_p1orig$D6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p1orig$T_internal,sal=28),rep("D6",times=11),rep("1C",times=11),rep("amb",times=11))

#P2A1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("A1",times=11),rep("1A",times=11),rep("amb",times=11))
#P2A2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("A2",times=11),rep("1A",times=11),rep("amb",times=11))
P2A3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("A3",times=11),rep("1A",times=11),rep("blank1A",times=11))
P2A4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("A4",times=11),rep("1A",times=11),rep("amb",times=11))
P2A5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("A5",times=11),rep("1A",times=11),rep("amb",times=11))
P2A6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$A6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("A6",times=11),rep("1A",times=11),rep("amb",times=11))
P2B1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("B1",times=11),rep("2C",times=11),rep("blank2C",times=11))
P2B2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("B2",times=11),rep("2C",times=11),rep("med",times=11))
P2B3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("B3",times=11),rep("2C",times=11),rep("med",times=11))
P2B4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("B4",times=11),rep("2C",times=11),rep("med",times=11))
P2B5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("B5",times=11),rep("2C",times=11),rep("med",times=11))
P2B6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$B6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("B6",times=11),rep("2C",times=11),rep("med",times=11))
P2C1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("C1",times=11),rep("3B",times=11),rep("high",times=11))
P2C2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("C2",times=11),rep("3B",times=11),rep("blank3B",times=11))
P2C3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("C3",times=11),rep("3B",times=11),rep("high",times=11))
P2C4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("C4",times=11),rep("3B",times=11),rep("high",times=11))
P2C5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("C5",times=11),rep("3B",times=11),rep("high",times=11))
P2C6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$C6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("C6",times=11),rep("3B",times=11),rep("high",times=11))
P2D1<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D1,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("D1",times=11),rep("3A",times=11),rep("high",times=11))
P2D2<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D2,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("D2",times=11),rep("3A",times=11),rep("high",times=11))
P2D3<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D3,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("D3",times=11),rep("3A",times=11),rep("high",times=11))
P2D4<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D4,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("D4",times=11),rep("3A",times=11),rep("blank3A",times=11))
P2D5<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D5,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("D5",times=11),rep("3A",times=11),rep("high",times=11))
P2D6<-data.frame(calc_MO2(duration=lrv_p2orig$Time.Min.,o2=lrv_p2orig$D6,o2_unit="mg_per_l",bin_width=40,vol=0.0005,temp=lrv_p2orig$T_internal,sal=28),rep("D6",times=11),rep("3A",times=11),rep("high",times=11))

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

#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES?? Fit a line to all of the points from the same treatment/tank. 
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
alllar<-rbind(P1A1,P1A2,P1A4,P1B1,P1B3,P1B4,P1C1,P1C3,P1C4,P1C5,P1D1,P1D2,P1D4,P1D5,P1D6,
              P2A4,P2A5,P2A6,P2B2,P2B3,P2B4,P2B5,P2B6,P2C1,P2C3,P2C4,P2C5,P2C6,P2D1,P2D2,P2D3,P2D5,P2D6)

#plot the curves
library(ggplot2)
library(gridExtra)
allplotlar<-ggplot(alllar, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_point(size=2)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlar)

#Try plotting one at a time, grouped by treatment
#Ambient: P1D1, P1D2, P1D4, P1D5, P1D6, P2A4, P2A5, P2A6
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
P1D6plot<-ggplot(P1D6,aes(x=O2_MEAN,y=MO2))+
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
grid.arrange(P1D1plot,P1D2plot,P1D4plot,P1D5plot,P1D6plot,P2A4plot,P2A5plot,P2A6plot,ncol=4)

#Medium: P1A1, P1A2, P1A4, P1C1, P1C3, P1C4, P1C5, P2B2, P2B3, P2B4, P2B5, P2B6
P1A1plot<-ggplot(P1A1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A2plot<-ggplot(P1A2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1A4plot<-ggplot(P1A4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C1plot<-ggplot(P1C1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C3plot<-ggplot(P1C3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C4plot<-ggplot(P1C4,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1C5plot<-ggplot(P1C5,aes(x=O2_MEAN,y=MO2))+
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
P2B5plot<-ggplot(P2B5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2B6plot<-ggplot(P2B6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1A1plot,P1A2plot,P1A4plot,P1C1plot,P1C3plot,P1C4plot,P1C5plot,P2B2plot,P2B3plot,P2B4plot,P2B5plot,P2B6plot,ncol=4)

#High: P1B1, P1B3, P1B4, P1B5, P1B6, P2C1, P2C3, P2C4, P2C5, P2C6, P2D1, P2D2, P2D3, P2D5, P2D6
P1B1plot<-ggplot(P1B1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P1B3plot<-ggplot(P1B3,aes(x=O2_MEAN,y=MO2))+
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
P1B6plot<-ggplot(P1B6,aes(x=O2_MEAN,y=MO2))+
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
P2C5plot<-ggplot(P2C5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2C6plot<-ggplot(P2C6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D1plot<-ggplot(P2D1,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D2plot<-ggplot(P2D2,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="loess",se=F)+
  theme_classic()
P2D3plot<-ggplot(P2D3,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D5plot<-ggplot(P2D5,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
P2D6plot<-ggplot(P2D6,aes(x=O2_MEAN,y=MO2))+
  geom_point(size=2)+
  geom_smooth(method="gam",se=F)+
  theme_classic()
grid.arrange(P1B1plot,P1B3plot,P1B4plot,P1B5plot,P1B6plot,
             P2C1plot,P2C3plot,P2C4plot,P2C5plot,P2C6plot,
             P2D1plot,P2D2plot,P2D3plot,P2D5plot,P2D6plot,ncol=5)



