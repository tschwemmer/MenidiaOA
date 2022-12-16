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
par(mfrow=c(3,4))
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

#calculate the group means and do a post hoc Tukey test
TukeyHSD(aov(lrv_flax$msmr~lrv_flax$CO2_level))

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
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 2",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.1,0.25))+
  theme_classic()
print(flaxlrvplot)


##########################################################################################################
#remove first 10 minutes and parts with extremely high temperature change
#first check temperature in 10 min bins

#remove first 10 minutes and parts with extremely high temperature change
#first check temperature in 10 min bins
library(tibble)
library(zoo)

#first remove 10 minutes from start
lrv_p1orig<-lrv_p1[31:1310,]
lrv_p2orig<-lrv_p2[31:1310,]
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
lrv_p1slope1$Index[abs(lrv_p1slope1$lrv_p1slope)>0.0083] #none
lrv_p2slope1$Index[abs(lrv_p2slope1$lrv_p2slope)>0.0083] #none



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
#subtracted 30 from each original index because I removed the first 30 rows (10min) after this was written.
lrv_p1orig$A4[196:271]<-NA
lrv_p1orig$A4[721:841]<-NA
lrv_p1orig$A4[931:1021]<-NA
lrv_p1orig$B3[1:151]<-NA
lrv_p1orig$B3[421:481]<-NA
lrv_p1orig$B4[91:211]<-NA
lrv_p1orig$B6[376:811]<-NA
lrv_p1orig$C3[571:691]<-NA
lrv_p1orig$D2[661:781]<-NA
lrv_p1orig$D5[1:45]<-NA
lrv_p1orig$D6[1081:1280]<-NA
lrv_p2orig$C1[1111:1156]<-NA
lrv_p2orig$C1[1231:1280]<-NA
lrv_p2orig$D2[841:1280]<-NA
lrv_p2orig$D3[1141:1280]<-NA
lrv_p2orig$D6[361:481]<-NA
lrv_p2orig$A3[991:1096]<-NA
lrv_p2orig$B1[451:586]<-NA
lrv_p2orig$D4[766:841]<-NA


#Use make_bins() to set bin width but use different rows based on when it bottoms out. Use increments of 50. 
P1A1bin<-make_bins(o2=lrv_p1orig$A1[1:1081],duration=lrv_p1orig$Time.Min.[1:1081],max_o2_width=1/10,min_o2_width=1/30)
P1A2bin<-make_bins(o2=lrv_p1orig$A2[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1A3bin<-make_bins(o2=lrv_p1orig$A3[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1A4bin<-make_bins(o2=lrv_p1orig$A4[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1A5bin<-make_bins(o2=lrv_p1orig$A5[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1A6bin<-make_bins(o2=lrv_p1orig$A6[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1B1bin<-make_bins(o2=lrv_p1orig$B1[1:1231],duration=lrv_p1orig$Time.Min.[1:1231],max_o2_width=1/10,min_o2_width=1/30)
#P1B2bin<-make_bins(o2=lrv_p1orig$B2[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1B3bin<-make_bins(o2=lrv_p1orig$B3[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1B4bin<-make_bins(o2=lrv_p1orig$B4[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1B5bin<-make_bins(o2=lrv_p1orig$B5[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1B6bin<-make_bins(o2=lrv_p1orig$B6[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1C1bin<-make_bins(o2=lrv_p1orig$C1[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1C2bin<-make_bins(o2=lrv_p1orig$C2[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1C3bin<-make_bins(o2=lrv_p1orig$C3[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1C4bin<-make_bins(o2=lrv_p1orig$C4[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1C5bin<-make_bins(o2=lrv_p1orig$C5[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1C6bin<-make_bins(o2=lrv_p1orig$C6[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D1bin<-make_bins(o2=lrv_p1orig$D1[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D2bin<-make_bins(o2=lrv_p1orig$D2[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P1D3bin<-make_bins(o2=lrv_p1orig$D3[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D4bin<-make_bins(o2=lrv_p1orig$D4[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D5bin<-make_bins(o2=lrv_p1orig$D5[1:1280],duration=lrv_p1orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P1D6bin<-make_bins(o2=lrv_p1orig$D6[1:1081],duration=lrv_p1orig$Time.Min.[1:1081],max_o2_width=1/10,min_o2_width=1/30)

#P2A1bin<-make_bins(o2=lrv_p2orig$A1[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P2A2bin<-make_bins(o2=lrv_p2orig$A2[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P2A3bin<-make_bins(o2=lrv_p2orig$A3[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2A4bin<-make_bins(o2=lrv_p2orig$A4[1:721],duration=lrv_p2orig$Time.Min.[1:721],max_o2_width=1/10,min_o2_width=1/30)
P2A5bin<-make_bins(o2=lrv_p2orig$A5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2A6bin<-make_bins(o2=lrv_p2orig$A6[1:1171],duration=lrv_p2orig$Time.Min.[1:1171],max_o2_width=1/10,min_o2_width=1/30)
#P2B1bin<-make_bins(o2=lrv_p2orig$B1[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2B2bin<-make_bins(o2=lrv_p2orig$B2[1:1200],duration=lrv_p2orig$Time.Min.[1:1200],max_o2_width=1/10,min_o2_width=1/30)
P2B3bin<-make_bins(o2=lrv_p2orig$B3[1:571],duration=lrv_p2orig$Time.Min.[1:571],max_o2_width=1/10,min_o2_width=1/30)
P2B4bin<-make_bins(o2=lrv_p2orig$B4[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2B5bin<-make_bins(o2=lrv_p2orig$B5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2B6bin<-make_bins(o2=lrv_p2orig$B6[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2C1bin<-make_bins(o2=lrv_p2orig$C1[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
#P2C2bin<-make_bins(o2=lrv_p2orig$C2[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2C3bin<-make_bins(o2=lrv_p2orig$C3[1:841],duration=lrv_p2orig$Time.Min.[1:841],max_o2_width=1/10,min_o2_width=1/30)
P2C4bin<-make_bins(o2=lrv_p2orig$C4[1:781],duration=lrv_p2orig$Time.Min.[1:781],max_o2_width=1/10,min_o2_width=1/30)
P2C5bin<-make_bins(o2=lrv_p2orig$C5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2C6bin<-make_bins(o2=lrv_p2orig$C6[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2D1bin<-make_bins(o2=lrv_p2orig$D1[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2D2bin<-make_bins(o2=lrv_p2orig$D2[1:841],duration=lrv_p2orig$Time.Min.[1:841],max_o2_width=1/10,min_o2_width=1/30)
P2D3bin<-make_bins(o2=lrv_p2orig$D3[1:1141],duration=lrv_p2orig$Time.Min.[1:1141],max_o2_width=1/10,min_o2_width=1/30)
#P2D4bin<-make_bins(o2=lrv_p2orig$D4[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2D5bin<-make_bins(o2=lrv_p2orig$D5[1:1280],duration=lrv_p2orig$Time.Min.[1:1280],max_o2_width=1/10,min_o2_width=1/30)
P2D6bin<-make_bins(o2=lrv_p2orig$D6[1:721],duration=lrv_p2orig$Time.Min.[1:721],max_o2_width=1/10,min_o2_width=1/30)



#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
P1A1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1081,]$Time.Min.,o2=lrv_p1orig[1:1081,]$A1,o2_unit="mg_per_l",bin_width=P1A1bin,vol=0.0006,temp=lrv_p1orig[1:1081,]$T_internal,sal=28),rep("A1"),rep("2A"),rep("med"))
P1A2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$A2,o2_unit="mg_per_l",bin_width=P1A2bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("A2"),rep("2A"),rep("med"))
#P1A3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$A3,o2_unit="mg_per_l",bin_width=P1A3bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("A3"),rep("2A"),rep("med"))
P1A4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$A4,o2_unit="mg_per_l",bin_width=P1A4bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("A4"),rep("2A"),rep("med"))
#P1A5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$A5,o2_unit="mg_per_l",bin_width=P1A5bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("A5"),rep("2A"),rep("blank2A"))
#P1A6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$A6,o2_unit="mg_per_l",bin_width=P1A6bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("A6"),rep("1B"),rep("blank1B"))
P1B1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1231,]$Time.Min.,o2=lrv_p1orig[1:1231,]$B1,o2_unit="mg_per_l",bin_width=P1B1bin,vol=0.0006,temp=lrv_p1orig[1:1231,]$T_internal,sal=28),rep("B1"),rep("3C"),rep("high"))
#P1B2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$B2,o2_unit="mg_per_l",bin_width=P1B2bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("B2"),rep("3C"),rep("blank3C"))
P1B3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$B3,o2_unit="mg_per_l",bin_width=P1B3bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("B3"),rep("3C"),rep("high"))
P1B4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$B4,o2_unit="mg_per_l",bin_width=P1B4bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("B4"),rep("3C"),rep("high"))
#P1B5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$B5,o2_unit="mg_per_l",bin_width=P1B5bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("B5"),rep("3C"),rep("high"))
P1B6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$B6,o2_unit="mg_per_l",bin_width=P1B6bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("B6"),rep("3C"),rep("high"))
P1C1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$C1,o2_unit="mg_per_l",bin_width=P1C1bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("C1"),rep("2B"),rep("med"))
#P1C2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$C2,o2_unit="mg_per_l",bin_width=P1C2bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("C2"),rep("2B"),rep("med"))
P1C3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$C3,o2_unit="mg_per_l",bin_width=P1C3bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("C3"),rep("2B"),rep("med"))
P1C4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$C4,o2_unit="mg_per_l",bin_width=P1C4bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("C4"),rep("2B"),rep("med"))
P1C5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$C5,o2_unit="mg_per_l",bin_width=P1C5bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("C5"),rep("2B"),rep("med"))
#P1C6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$C6,o2_unit="mg_per_l",bin_width=P1C6bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("C6"),rep("2B"),rep("blank2B"))
P1D1<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$D1,o2_unit="mg_per_l",bin_width=P1D1bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("D1"),rep("1C"),rep("amb"))
P1D2<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$D2,o2_unit="mg_per_l",bin_width=P1D2bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("D2"),rep("1C"),rep("amb"))
#P1D3<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$D3,o2_unit="mg_per_l",bin_width=P1D3bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("D3"),rep("1C"),rep("blank1C"))
P1D4<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$D4,o2_unit="mg_per_l",bin_width=P1D4bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("D4"),rep("1C"),rep("amb"))
P1D5<-data.frame(calc_MO2(duration=lrv_p1orig[1:1280,]$Time.Min.,o2=lrv_p1orig[1:1280,]$D5,o2_unit="mg_per_l",bin_width=P1D5bin,vol=0.0006,temp=lrv_p1orig[1:1280,]$T_internal,sal=28),rep("D5"),rep("1C"),rep("amb"))
P1D6<-data.frame(calc_MO2(duration=lrv_p1orig[1:1081,]$Time.Min.,o2=lrv_p1orig[1:1081,]$D6,o2_unit="mg_per_l",bin_width=P1D6bin,vol=0.0006,temp=lrv_p1orig[1:1081,]$T_internal,sal=28),rep("D6"),rep("1C"),rep("amb"))

#P2A1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$A1,o2_unit="mg_per_l",bin_width=P2A1bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("A1"),rep("1A"),rep("amb"))
#P2A2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$A2,o2_unit="mg_per_l",bin_width=P2A2bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("A2"),rep("1A"),rep("amb"))
#P2A3<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$A3,o2_unit="mg_per_l",bin_width=P2A3bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("A3"),rep("1A"),rep("blank1A"))
P2A4<-data.frame(calc_MO2(duration=lrv_p2orig[1:721,]$Time.Min.,o2=lrv_p2orig[1:721,]$A4,o2_unit="mg_per_l",bin_width=P2A4bin,vol=0.0005,temp=lrv_p2orig[1:721,]$T_internal,sal=28),rep("A4"),rep("1A"),rep("amb"))
P2A5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$A5,o2_unit="mg_per_l",bin_width=P2A5bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("A5"),rep("1A"),rep("amb"))
P2A6<-data.frame(calc_MO2(duration=lrv_p2orig[1:1171,]$Time.Min.,o2=lrv_p2orig[1:1171,]$A6,o2_unit="mg_per_l",bin_width=P2A6bin,vol=0.0005,temp=lrv_p2orig[1:1171,]$T_internal,sal=28),rep("A6"),rep("1A"),rep("amb"))
#P2B1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$B1,o2_unit="mg_per_l",bin_width=P2B1bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("B1"),rep("2C"),rep("blank2C"))
P2B2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1200,]$Time.Min.,o2=lrv_p2orig[1:1200,]$B2,o2_unit="mg_per_l",bin_width=P2B2bin,vol=0.0005,temp=lrv_p2orig[1:1200,]$T_internal,sal=28),rep("B2"),rep("2C"),rep("med"))
P2B3<-data.frame(calc_MO2(duration=lrv_p2orig[1:571,]$Time.Min.,o2=lrv_p2orig[1:571,]$B3,o2_unit="mg_per_l",bin_width=P2B3bin,vol=0.0005,temp=lrv_p2orig[1:571,]$T_internal,sal=28),rep("B3"),rep("2C"),rep("med"))
P2B4<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$B4,o2_unit="mg_per_l",bin_width=P2B4bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("B4"),rep("2C"),rep("med"))
P2B5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$B5,o2_unit="mg_per_l",bin_width=P2B5bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("B5"),rep("2C"),rep("med"))
P2B6<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$B6,o2_unit="mg_per_l",bin_width=P2B6bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("B6"),rep("2C"),rep("med"))
P2C1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$C1,o2_unit="mg_per_l",bin_width=P2C1bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("C1"),rep("3B"),rep("high"))
#P2C2<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$C2,o2_unit="mg_per_l",bin_width=P2C2bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("C2"),rep("3B"),rep("blank3B"))
P2C3<-data.frame(calc_MO2(duration=lrv_p2orig[1:841,]$Time.Min.,o2=lrv_p2orig[1:841,]$C3,o2_unit="mg_per_l",bin_width=P2C3bin,vol=0.0005,temp=lrv_p2orig[1:841,]$T_internal,sal=28),rep("C3"),rep("3B"),rep("high"))
P2C4<-data.frame(calc_MO2(duration=lrv_p2orig[1:781,]$Time.Min.,o2=lrv_p2orig[1:781,]$C4,o2_unit="mg_per_l",bin_width=P2C4bin,vol=0.0005,temp=lrv_p2orig[1:781,]$T_internal,sal=28),rep("C4"),rep("3B"),rep("high"))
P2C5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$C5,o2_unit="mg_per_l",bin_width=P2C5bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("C5"),rep("3B"),rep("high"))
P2C6<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$C6,o2_unit="mg_per_l",bin_width=P2C6bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("C6"),rep("3B"),rep("high"))
P2D1<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$D1,o2_unit="mg_per_l",bin_width=P2D1bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("D1"),rep("3A"),rep("high"))
P2D2<-data.frame(calc_MO2(duration=lrv_p2orig[1:841,]$Time.Min.,o2=lrv_p2orig[1:841,]$D2,o2_unit="mg_per_l",bin_width=P2D2bin,vol=0.0005,temp=lrv_p2orig[1:841,]$T_internal,sal=28),rep("D2"),rep("3A"),rep("high"))
P2D3<-data.frame(calc_MO2(duration=lrv_p2orig[1:1141,]$Time.Min.,o2=lrv_p2orig[1:1141,]$D3,o2_unit="mg_per_l",bin_width=P2D3bin,vol=0.0005,temp=lrv_p2orig[1:1141,]$T_internal,sal=28),rep("D3"),rep("3A"),rep("high"))
#P2D4<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$D4,o2_unit="mg_per_l",bin_width=P2D4bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("D4"),rep("3A"),rep("blank3A"))
P2D5<-data.frame(calc_MO2(duration=lrv_p2orig[1:1280,]$Time.Min.,o2=lrv_p2orig[1:1280,]$D5,o2_unit="mg_per_l",bin_width=P2D5bin,vol=0.0005,temp=lrv_p2orig[1:1280,]$T_internal,sal=28),rep("D5"),rep("3A"),rep("high"))
P2D6<-data.frame(calc_MO2(duration=lrv_p2orig[1:721,]$Time.Min.,o2=lrv_p2orig[1:721,]$D6,o2_unit="mg_per_l",bin_width=P2D6bin,vol=0.0005,temp=lrv_p2orig[1:721,]$T_internal,sal=28),rep("D6"),rep("3A"),rep("high"))

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
P1A1$MO2b<-P1A1$MO2-blank2
P1A2$MO2b<-P1A2$MO2-blank2
P1A3$MO2b<-P1A3$MO2-blank2
P1A4$MO2b<-P1A4$MO2-blank2
P1A5$MO2b<-P1A5$MO2-blank2
P1A6$MO2b<-P1A6$MO2-blank1
P1B1$MO2b<-P1B1$MO2-blank3
P1B2$MO2b<-P1B2$MO2-blank3
P1B3$MO2b<-P1B3$MO2-blank3
P1B4$MO2b<-P1B4$MO2-blank3
P1B5$MO2b<-P1B5$MO2-blank3
P1B6$MO2b<-P1B6$MO2-blank3
P1C1$MO2b<-P1C1$MO2-blank2
P1C2$MO2b<-P1C2$MO2-blank2
P1C3$MO2b<-P1C3$MO2-blank2
P1C4$MO2b<-P1C4$MO2-blank2
P1C5$MO2b<-P1C5$MO2-blank2
P1C6$MO2b<-P1C6$MO2-blank2
P1D1$MO2b<-P1D1$MO2-blank1
P1D2$MO2b<-P1D2$MO2-blank1
P1D3$MO2b<-P1D3$MO2-blank1
P1D4$MO2b<-P1D4$MO2-blank1
P1D5$MO2b<-P1D5$MO2-blank1
P1D6$MO2b<-P1D6$MO2-blank1
P2A1$MO2b<-P2A1$MO2-blank1
P2A2$MO2b<-P2A2$MO2-blank1
P2A3$MO2b<-P2A3$MO2-blank1
P2A4$MO2b<-P2A4$MO2-blank1
P2A5$MO2b<-P2A5$MO2-blank1
P2A6$MO2b<-P2A6$MO2-blank1
P2B1$MO2b<-P2B1$MO2-blank2
P2B2$MO2b<-P2B2$MO2-blank2
P2B3$MO2b<-P2B3$MO2-blank2
P2B4$MO2b<-P2B4$MO2-blank2
P2B5$MO2b<-P2B5$MO2-blank2
P2B6$MO2b<-P2B6$MO2-blank2
P2C1$MO2b<-P2C1$MO2-blank3
P2C2$MO2b<-P2C2$MO2-blank3
P2C3$MO2b<-P2C3$MO2-blank3
P2C4$MO2b<-P2C4$MO2-blank3
P2C5$MO2b<-P2C5$MO2-blank3
P2C6$MO2b<-P2C6$MO2-blank3
P2D1$MO2b<-P2D1$MO2-blank3
P2D2$MO2b<-P2D2$MO2-blank3
P2D3$MO2b<-P2D3$MO2-blank3
P2D4$MO2b<-P2D4$MO2-blank3
P2D5$MO2b<-P2D5$MO2-blank3
P2D6$MO2b<-P2D6$MO2-blank3

#Add a column in each well's calc_mo2 output for mass and msmr. 
P1A1$dw<-rep(0.15557210)
P1A2$dw<-rep(0.09621537)
P1A4$dw<-rep(0.08648657)
P1B1$dw<-rep(0.26372411)
P1B3$dw<-rep(0.24435111)
P1B4$dw<-rep(0.17151180)
P1C1$dw<-rep(0.07421028)
P1C3$dw<-rep(0.15325371)
P1C4$dw<-rep(0.12318048)
P1C5$dw<-rep(0.27059221)
P1D1$dw<-rep(0.11670653)
P1D2$dw<-rep(0.11280051)
P1D4$dw<-rep(0.09533867)
P1D5$dw<-rep(0.13466436)
P1D6$dw<-rep(0.07749913)
P2A4$dw<-rep(0.08820891)
P2A5$dw<-rep(0.12758227)
P2A6$dw<-rep(0.26626994)
P2B2$dw<-rep(0.20917328)
P2B3$dw<-rep(0.24829764)
P2B4$dw<-rep(0.26406012)
P2B5$dw<-rep(0.27305967)
P2B6$dw<-rep(0.12665792)
P2C1$dw<-rep(0.12249533)
P2C3$dw<-rep(0.19557470)
P2C4$dw<-rep(0.19857807)
P2C5$dw<-rep(0.13727376)
P2C6$dw<-rep(0.15032161)
P2D1$dw<-rep(0.09221769)
P2D2$dw<-rep(0.16848580)
P2D3$dw<-rep(0.14029115)
P2D5$dw<-rep(0.22736947)
P2D6$dw<-rep(0.30740212)

P1A1$msmrs<-P1A1$MO2/P1A1$dw
P1A2$msmrs<-P1A2$MO2/P1A2$dw
P1A4$msmrs<-P1A4$MO2/P1A4$dw
P1B1$msmrs<-P1B1$MO2/P1B1$dw
P1B3$msmrs<-P1B3$MO2/P1B3$dw
P1B4$msmrs<-P1B4$MO2/P1B4$dw
P1C1$msmrs<-P1C1$MO2/P1C1$dw
P1C3$msmrs<-P1C3$MO2/P1C3$dw
P1C4$msmrs<-P1C4$MO2/P1C4$dw
P1C5$msmrs<-P1C5$MO2/P1C5$dw
P1D1$msmrs<-P1D1$MO2/P1D1$dw
P1D2$msmrs<-P1D2$MO2/P1D2$dw
P1D4$msmrs<-P1D4$MO2/P1D4$dw
P1D5$msmrs<-P1D5$MO2/P1D5$dw
P1D6$msmrs<-P1D6$MO2/P1D6$dw
P2A4$msmrs<-P2A4$MO2/P2A4$dw
P2A5$msmrs<-P2A5$MO2/P2A5$dw
P2A6$msmrs<-P2A6$MO2/P2A6$dw
P2B2$msmrs<-P2B2$MO2/P2B2$dw
P2B3$msmrs<-P2B3$MO2/P2B3$dw
P2B4$msmrs<-P2B4$MO2/P2B4$dw
P2B5$msmrs<-P2B5$MO2/P2B5$dw
P2B6$msmrs<-P2B6$MO2/P2B6$dw
P2C1$msmrs<-P2C1$MO2/P2C1$dw
P2C3$msmrs<-P2C3$MO2/P2C3$dw
P2C4$msmrs<-P2C4$MO2/P2C4$dw
P2C5$msmrs<-P2C5$MO2/P2C5$dw
P2C6$msmrs<-P2C6$MO2/P2C6$dw
P2D1$msmrs<-P2D1$MO2/P2D1$dw
P2D2$msmrs<-P2D2$MO2/P2D2$dw
P2D3$msmrs<-P2D3$MO2/P2D3$dw
P2D5$msmrs<-P2D5$MO2/P2D5$dw
P2D6$msmrs<-P2D6$MO2/P2D6$dw

#remove P1B6 because decided cant use it
lrv_flax<-lrv_flax[-c(7),]

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


#Compile into one dataframe to plot 
alllrv<-rbind(P1A1,P1A2,P1A4,P1B1,P1B3,P1B4,P1C1,P1C3,P1C4,P1C5,P1D1,P1D2,P1D4,P1D5,P1D6,
              P2A4,P2A5,P2A6,P2B2,P2B3,P2B4,P2B5,P2B6,P2C1,P2C3,P2C4,P2C5,P2C6,P2D1,P2D2,P2D3,P2D5,P2D6)

#plot the curves
library(ggplot2)
library(gridExtra)
allplotlrv<-ggplot(alllrv, aes(x=O2_MEAN,y=msmrs,colour=Well))+
  geom_point(size=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlrv)

#look for ones that are above 0.4-0.5 or below -0.05
hist(P1A1$msmrs,breaks=10)
hist(P1A2$msmrs,breaks=10)
hist(P1A3$msmrs,breaks=10) 
hist(P1A4$msmrs,breaks=10) 
hist(P1A5$msmrs,breaks=10)
hist(P1A6$msmrs,breaks=10) 
hist(P1B1$msmrs,breaks=10)
hist(P1B2$msmrs,breaks=10)
hist(P1B3$msmrs,breaks=10) #one below -0.05
hist(P1B4$msmrs,breaks=10) 
hist(P1B5$msmrs,breaks=10)
hist(P1B6$msmrs,breaks=10)
hist(P1C1$msmrs,breaks=10)
hist(P1C2$msmrs,breaks=10)
hist(P1C3$msmrs,breaks=10) 
hist(P1C4$msmrs,breaks=10)
hist(P1C5$msmrs,breaks=10)
hist(P1C6$msmrs,breaks=10) 
hist(P1D1$msmrs,breaks=10)
hist(P1D2$msmrs,breaks=10)
hist(P1D3$msmrs,breaks=10)
hist(P1D4$msmrs,breaks=10)
hist(P1D5$msmrs,breaks=10)
hist(P1D6$msmrs,breaks=10) #seven above 0.4
hist(P2A1$msmrs,breaks=10)
hist(P2A2$msmrs,breaks=10)
hist(P2A3$msmrs,breaks=10)
hist(P2A4$msmrs,breaks=10)
hist(P2A5$msmrs,breaks=10)
hist(P2A6$msmrs,breaks=10)
hist(P2B1$msmrs,breaks=10)
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
hist(P2D3$msmrs,breaks=10)
hist(P2D4$msmrs,breaks=10)
hist(P2D5$msmrs,breaks=10)
hist(P2D6$msmrs,breaks=10)

#Remove problematic data
row.names(P1B3)<-NULL
row.names(P1D6)<-NULL

P1B3<-P1B3[-c(30),] 
P1D6<-P1D6[-c(76),]

row.names(P1B3)<-NULL
row.names(P1D6)<-NULL


lrv_flax$Pcrit_alpha<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['Alpha'],
                        calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['Alpha'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Alpha'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Alpha'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Alpha'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Alpha'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Alpha'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Alpha'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['Alpha'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Alpha'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Alpha'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Alpha'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Alpha'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Alpha'],
                        calc_pcrit(P1D6$O2_MEAN,P1D6$msmrs)['Alpha'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Alpha'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Alpha'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Alpha'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Alpha'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Alpha'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Alpha'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$msmrs)['Alpha'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Alpha'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['Alpha'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Alpha'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Alpha'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Alpha'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Alpha'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Alpha'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['Alpha'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['Alpha'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Alpha'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$msmrs)['Alpha'])
lrv_flax$Pcrit_break<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['Breakpoint'],
                        calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['Breakpoint'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Breakpoint'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Breakpoint'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Breakpoint'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Breakpoint'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Breakpoint'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Breakpoint'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['Breakpoint'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Breakpoint'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Breakpoint'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Breakpoint'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Breakpoint'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Breakpoint'],
                        calc_pcrit(P1D6$O2_MEAN,P1D6$msmrs)['Breakpoint'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Breakpoint'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Breakpoint'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Breakpoint'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Breakpoint'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Breakpoint'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Breakpoint'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$msmrs)['Breakpoint'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Breakpoint'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['Breakpoint'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Breakpoint'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Breakpoint'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Breakpoint'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Breakpoint'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Breakpoint'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['Breakpoint'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['Breakpoint'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Breakpoint'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$msmrs)['Breakpoint'])
lrv_flax$Pcrit_subPI<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['Sub_PI'],
                        calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['Sub_PI'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['Sub_PI'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['Sub_PI'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['Sub_PI'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['Sub_PI'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['Sub_PI'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['Sub_PI'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['Sub_PI'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['Sub_PI'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['Sub_PI'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['Sub_PI'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['Sub_PI'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['Sub_PI'],
                        calc_pcrit(P1D6$O2_MEAN,P1D6$msmrs)['Sub_PI'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['Sub_PI'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['Sub_PI'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['Sub_PI'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['Sub_PI'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['Sub_PI'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['Sub_PI'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$msmrs)['Sub_PI'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['Sub_PI'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['Sub_PI'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['Sub_PI'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['Sub_PI'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['Sub_PI'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['Sub_PI'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['Sub_PI'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['Sub_PI'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['Sub_PI'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['Sub_PI'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$msmrs)['Sub_PI'])
lrv_flax$Pcrit_NLR<-c(calc_pcrit(P1A1$O2_MEAN,P1A1$msmrs)['NLR'],
                        calc_pcrit(P1A2$O2_MEAN,P1A2$msmrs)['NLR'],
                        calc_pcrit(P1A4$O2_MEAN,P1A4$msmrs)['NLR'],
                        calc_pcrit(P1B1$O2_MEAN,P1B1$msmrs)['NLR'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$msmrs)['NLR'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$msmrs)['NLR'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$msmrs)['NLR'],
                        calc_pcrit(P1C3$O2_MEAN,P1C3$msmrs)['NLR'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$msmrs)['NLR'],
                        calc_pcrit(P1C5$O2_MEAN,P1C5$msmrs)['NLR'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$msmrs)['NLR'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$msmrs)['NLR'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$msmrs)['NLR'],
                        calc_pcrit(P1D5$O2_MEAN,P1D5$msmrs)['NLR'],
                        calc_pcrit(P1D6$O2_MEAN,P1D6$msmrs)['NLR'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$msmrs)['NLR'],
                        calc_pcrit(P2A5$O2_MEAN,P2A5$msmrs)['NLR'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$msmrs)['NLR'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$msmrs)['NLR'],
                        calc_pcrit(P2B3$O2_MEAN,P2B3$msmrs)['NLR'],
                        calc_pcrit(P2B4$O2_MEAN,P2B4$msmrs)['NLR'],
                        calc_pcrit(P2B5$O2_MEAN,P2B5$msmrs)['NLR'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$msmrs)['NLR'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$msmrs)['NLR'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$msmrs)['NLR'],
                        calc_pcrit(P2C4$O2_MEAN,P2C4$msmrs)['NLR'],
                        calc_pcrit(P2C5$O2_MEAN,P2C5$msmrs)['NLR'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$msmrs)['NLR'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$msmrs)['NLR'],
                        calc_pcrit(P2D2$O2_MEAN,P2D2$msmrs)['NLR'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$msmrs)['NLR'],
                        calc_pcrit(P2D5$O2_MEAN,P2D5$msmrs)['NLR'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$msmrs)['NLR'])
lrv_flax$alpha<-c(calc_alpha(P1A1$O2_MEAN,P1A1$msmrs)$alpha,
                        calc_alpha(P1A2$O2_MEAN,P1A2$msmrs)$alpha,
                        calc_alpha(P1A4$O2_MEAN,P1A4$msmrs)$alpha,
                        calc_alpha(P1B1$O2_MEAN,P1B1$msmrs)$alpha,
                        calc_alpha(P1B3$O2_MEAN,P1B3$msmrs)$alpha,
                        calc_alpha(P1B4$O2_MEAN,P1B4$msmrs)$alpha,
                        calc_alpha(P1C1$O2_MEAN,P1C1$msmrs)$alpha,
                        calc_alpha(P1C3$O2_MEAN,P1C3$msmrs)$alpha,
                        calc_alpha(P1C4$O2_MEAN,P1C4$msmrs)$alpha,
                        calc_alpha(P1C5$O2_MEAN,P1C5$msmrs)$alpha,
                        calc_alpha(P1D1$O2_MEAN,P1D1$msmrs)$alpha,
                        calc_alpha(P1D2$O2_MEAN,P1D2$msmrs)$alpha,
                        calc_alpha(P1D4$O2_MEAN,P1D4$msmrs)$alpha,
                        calc_alpha(P1D5$O2_MEAN,P1D5$msmrs)$alpha,
                        calc_alpha(P1D6$O2_MEAN,P1D6$msmrs)$alpha,
                        calc_alpha(P2A4$O2_MEAN,P2A4$msmrs)$alpha,
                        calc_alpha(P2A5$O2_MEAN,P2A5$msmrs)$alpha,
                        calc_alpha(P2A6$O2_MEAN,P2A6$msmrs)$alpha,
                        calc_alpha(P2B2$O2_MEAN,P2B2$msmrs)$alpha,
                        calc_alpha(P2B3$O2_MEAN,P2B3$msmrs)$alpha,
                        calc_alpha(P2B4$O2_MEAN,P2B4$msmrs)$alpha,
                        calc_alpha(P2B5$O2_MEAN,P2B5$msmrs)$alpha,
                        calc_alpha(P2B6$O2_MEAN,P2B6$msmrs)$alpha,
                        calc_alpha(P2C1$O2_MEAN,P2C1$msmrs)$alpha,
                        calc_alpha(P2C3$O2_MEAN,P2C3$msmrs)$alpha,
                        calc_alpha(P2C4$O2_MEAN,P2C4$msmrs)$alpha,
                        calc_alpha(P2C5$O2_MEAN,P2C5$msmrs)$alpha,
                        calc_alpha(P2C6$O2_MEAN,P2C6$msmrs)$alpha,
                        calc_alpha(P2D1$O2_MEAN,P2D1$msmrs)$alpha,
                        calc_alpha(P2D2$O2_MEAN,P2D2$msmrs)$alpha,
                        calc_alpha(P2D3$O2_MEAN,P2D3$msmrs)$alpha,
                        calc_alpha(P2D5$O2_MEAN,P2D5$msmrs)$alpha,
                        calc_alpha(P2D6$O2_MEAN,P2D6$msmrs)$alpha)



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

P1B6seg<-selgmented(lm(msmrs~O2_MEAN,data=P1B6),seg.Z=~O2_MEAN,type='bic',Kmax=4,msg=T)
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
lrv_flax$Pcrit_break<-c(1.6298,2.435,1.807,  #P1A
                        1.56153,NA,NA,  #P1B
                        1.242,2.290,1.1668,1.1044,  #P1C
                        2.621,1.959,3.0317,1.894,2.319,  #P1D
                        NA,0.852,1.2649,  #P2A
                        1.57,0.967,1.52556,2.6837,1.36,  #P2B
                        NA,0.503,0.816,1.888,0.863,  #P2C
                        1.641,0.704,2.740,1.333,1.264)  #P2D
lrv_flax$spike<-c(1,1,1,  #P1A
                  1,NA,NA,  #P1B
                  0,1,1,1,  #P1C
                  1,1,1,1,1,  #P1D
                  NA,1,0,  #P2A
                  0,0,1,1,1,  #P2B
                  NA,0,0,1,0,  #P2C
                  1,0,1,0,0)  #P2D

################################################################################################
#Analyze, check model assumptions, and plot
#set levels of CO2_level and Tank
str(lrv_flax)
lrv_flax$CO2_level<-factor(lrv_flax$CO2_level,levels=c("amb","med","high"))
lrv_flax$Tank<-factor(lrv_flax$Tank,levels=c("1A","1B","1C","2A","2B","2C","3A","3B","3C"))

plot(lrv_flax$Pcrit_break~lrv_flax$CO2_level)

#calculate mean and SE of Pcrit values by treatment
library(plyr)
break_sum<-ddply(lrv_flax,"CO2_level",summarise,N=sum(!is.na(Pcrit_break)),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

break_mod<-lmer(Pcrit_break~CO2_level+(1|Tank),data=lrv_flax) 
ranova(break_mod) #tank doesn't affect fit
anova(break_mod) #p=0.5271

break_mod1<-lm(Pcrit_break~CO2_level,data=lrv_flax)
anova(break_mod1) #p=0.5403

#try it the other way
break_mod2<-aov(lrv_flax$Pcrit_break~lrv_flax$CO2_level/factor(lrv_flax$Tank))
summary(break_mod2) #neither CO2 nor tank is significant, p=0.0973 for CO2


#diagnostics
par(mfrow=c(2,2))
plot(break_mod2) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(lrv_flax$Pcrit_break) #p=0.3904
hist(lrv_flax$Pcrit_break)

#homogeneity of variances
library(car)
leveneTest(lrv_flax$Pcrit_break, lrv_flax$CO2_level) #p=0.7352 good

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxlrvpcritplot<-ggplot(break_sum, aes(x=CO2_level,y=MeanPcrit))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanPcrit-SE,ymax=MeanPcrit+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 2",x=0.5,y=0.96,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,3))+
  theme_classic()
print(flaxlrvpcritplot)


###############################################################################################
#Calculate RMR as the average MO2 for which O2>Pcrit

lrv_flax$RMR<-c(mean(c(P1A1$msmrs[P1A1$O2_MEAN>lrv_flax[1,10]])),
                mean(c(P1A2$msmrs[P1A2$O2_MEAN>lrv_flax[2,10]])),
                mean(c(P1A4$msmrs[P1A4$O2_MEAN>lrv_flax[3,10]])),
                mean(c(P1B1$msmrs[P1B1$O2_MEAN>lrv_flax[4,10]])),
                NA, #removed because it seems to be dead, very high MO2 at beginning then suddenly low (matches microbial)
                mean(c(P1B4$msmrs[P1B4$O2_MEAN>3])),
                mean(c(P1C1$msmrs[P1C1$O2_MEAN>lrv_flax[7,10]])),
                mean(c(P1C3$msmrs[P1C3$O2_MEAN>lrv_flax[8,10]])),
                mean(c(P1C4$msmrs[P1C4$O2_MEAN>lrv_flax[9,10]])),
                mean(c(P1C5$msmrs[P1C5$O2_MEAN>lrv_flax[10,10]])),
                mean(c(P1D1$msmrs[P1D1$O2_MEAN>lrv_flax[11,10]])),
                mean(c(P1D2$msmrs[P1D2$O2_MEAN>lrv_flax[12,10]])),
                mean(c(P1D4$msmrs[P1D4$O2_MEAN>lrv_flax[13,10]])),
                mean(c(P1D5$msmrs[P1D5$O2_MEAN>lrv_flax[14,10]])),
                mean(c(P1D6$msmrs[P1D6$O2_MEAN>lrv_flax[15,10]])),
                mean(c(P2A4$msmrs[P2A4$O2_MEAN>3])),
                mean(c(P2A5$msmrs[P2A5$O2_MEAN>lrv_flax[17,10]])),
                mean(c(P2A6$msmrs[P2A6$O2_MEAN>lrv_flax[18,10]])),
                mean(c(P2B2$msmrs[P2B2$O2_MEAN>lrv_flax[19,10]])),
                mean(c(P2B3$msmrs[P2B3$O2_MEAN>lrv_flax[20,10]])),
                mean(c(P2B4$msmrs[P2B4$O2_MEAN>lrv_flax[21,10]])),
                mean(c(P2B5$msmrs[P2B5$O2_MEAN>lrv_flax[22,10]])),
                mean(c(P2B6$msmrs[P2B6$O2_MEAN>lrv_flax[23,10]])),
                mean(c(P2C1$msmrs[P2C1$O2_MEAN>3])),
                mean(c(P2C3$msmrs[P2C3$O2_MEAN>lrv_flax[25,10]])),
                mean(c(P2C4$msmrs[P2C4$O2_MEAN>lrv_flax[26,10]])),
                mean(c(P2C5$msmrs[P2C5$O2_MEAN>lrv_flax[27,10]])),
                mean(c(P2C6$msmrs[P2C6$O2_MEAN>lrv_flax[28,10]])),
                mean(c(P2D1$msmrs[P2D1$O2_MEAN>lrv_flax[29,10]])),
                mean(c(P2D2$msmrs[P2D2$O2_MEAN>lrv_flax[30,10]])),
                mean(c(P2D3$msmrs[P2D3$O2_MEAN>lrv_flax[31,10]])),
                mean(c(P2D5$msmrs[P2D5$O2_MEAN>lrv_flax[32,10]])),
                mean(c(P2D6$msmrs[P2D6$O2_MEAN>lrv_flax[33,10]])))

#analyze RMR
flax_lrv_model<-lm(lrv_flax$RMR~lrv_flax$CO2_level)
anova(flax_lrv_model) 

flax_lrv_mod<-lmer(RMR~CO2_level+(1|Tank),data=lrv_flax) 
anova(flax_lrv_mod)
ranova(flax_lrv_mod) #random effect of tank doesn't affect results. 

#try it the other way
flax_lrv_mdl<-aov(lrv_flax$RMR~lrv_flax$CO2_level/factor(lrv_flax$Tank))
summary(flax_lrv_mdl) #p=0.000964 for CO2, tank not significant

TukeyHSD(flax_lrv_mdl)

#diagnostics
par(mfrow=c(2,2))
plot(flax_lrv_mdl) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(lrv_flax$RMR) #looks good p=0.829

#homogeneity of variances
library(car)
leveneTest(lrv_flax$RMR, lrv_flax$CO2_level) #p=0.1433 good

#calculate the group means 

library(plyr)
flax_lrv_sum<-ddply(lrv_flax,"CO2_level",summarise,N=length(na.omit(RMR)),MeanMO2=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
flax_lrv_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
flaxlrvplot<-ggplot(flax_lrv_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("5dph Larvae, Exp. 2",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.1,0.25))+
  theme_classic()
print(flaxlrvplot)

pct_spike_amb<-100*sum(na.omit(lrv_flax$spike[lrv_flax$CO2_level=="amb"]))/length(na.omit(lrv_flax$spike[lrv_flax$CO2_level=="amb"]))
pct_spike_med<-100*sum(na.omit(lrv_flax$spike[lrv_flax$CO2_level=="med"]))/length(na.omit(lrv_flax$spike[lrv_flax$CO2_level=="med"]))
pct_spike_high<-100*sum(na.omit(lrv_flax$spike[lrv_flax$CO2_level=="high"]))/length(na.omit(lrv_flax$spike[lrv_flax$CO2_level=="high"]))

