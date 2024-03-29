#Respirometry analysis from 2021 experiment

#Run 1: Embryos, Dana Hall, 6-14-21

#load data sheets and treatments
emb_p1<-read.csv(file.choose(),header=TRUE)
emb_p2<-read.csv(file.choose(),header=TRUE)
emb_trmt_p1<-read.csv(file.choose(),header=TRUE)
emb_trmt_p2<-read.csv(file.choose(),header=TRUE)

#remove first 85 minutes from Plate 1, first 30 minutes from Plate 2. Then renumber the rows.
emb_p1<-emb_p1[232:4416,]
emb_p2<-emb_p2[86:4415,]
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
emb_trmt_p1$Tank<-factor(emb_trmt_p1$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))
emb_trmt_p2$Tank<-factor(emb_trmt_p2$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))


#calculate slope for each well using all untrimmed data. Units will be umol consumed per hour
slopes_r1p1<-data.frame(names(emb_p1)[4:27], sapply(emb_p1[4:27],function(x) ((-coef(summary(lm(x~emb_p1$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(emb_p1[4:27],function(x) summary(lm(x~emb_p1$Time.Sec.))$r.squared))
names(slopes_r1p1)<-c("Well","MO2","Rsquared")
slopes_r1p2<-data.frame(names(emb_p2)[4:27], sapply(emb_p2[4:27],function(x) ((-coef(summary(lm(x~emb_p2$Time.Sec.)))[2])/31.9988)*1800))
names(slopes_r1p2)<-c("Well","MO2")

#plot data for all live wells
par(mfrow=c(3,5))
plot(emb_p1$A3~emb_p1$Time.Min.)
plot(emb_p1$A5~emb_p1$Time.Min.)
plot(emb_p1$A6~emb_p1$Time.Min.)
plot(emb_p1$B3~emb_p1$Time.Min.)
plot(emb_p1$B4~emb_p1$Time.Min.)
plot(emb_p1$B6~emb_p1$Time.Min.)
plot(emb_p1$C1~emb_p1$Time.Min.)
plot(emb_p1$C2~emb_p1$Time.Min.)
plot(emb_p1$C4~emb_p1$Time.Min.)
plot(emb_p1$C6~emb_p1$Time.Min.)
plot(emb_p1$D1~emb_p1$Time.Min.)
plot(emb_p1$D2~emb_p1$Time.Min.)
plot(emb_p1$D3~emb_p1$Time.Min.)
plot(emb_p1$D4~emb_p1$Time.Min.)
plot(emb_p1$D6~emb_p1$Time.Min.)

plot(emb_p2$A1~emb_p2$Time.Min.)
plot(emb_p2$A3~emb_p2$Time.Min.)
plot(emb_p2$A4~emb_p2$Time.Min.)
plot(emb_p2$A6~emb_p2$Time.Min.)
plot(emb_p2$B1~emb_p2$Time.Min.)
plot(emb_p2$B2~emb_p2$Time.Min.)
plot(emb_p2$B6~emb_p2$Time.Min.)
plot(emb_p2$C1~emb_p2$Time.Min.)
plot(emb_p2$C2~emb_p2$Time.Min.)
plot(emb_p2$C3~emb_p2$Time.Min.)
plot(emb_p2$C6~emb_p2$Time.Min.)
plot(emb_p2$D1~emb_p2$Time.Min.)
plot(emb_p2$D3~emb_p2$Time.Min.)
plot(emb_p2$D4~emb_p2$Time.Min.)
plot(emb_p2$D6~emb_p2$Time.Min.)

#Trim the data and calculate the overall slopes again
emb_p1_400<-emb_p1[1:966,]
emb_p2_400<-emb_p2[1:1112,]

slopes_r1p1_400<-data.frame(names(emb_p1_400)[4:27], sapply(emb_p1_400[4:27],function(x) ((-coef(summary(lm(x~emb_p1_400$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(emb_p1_400[4:27],function(x) summary(lm(x~emb_p1_400$Time.Sec.))$r.squared))
names(slopes_r1p1_400)<-c("Well","MO2","Rsquared")

slopes_r1p2_400<-data.frame(names(emb_p2_400)[4:27], sapply(emb_p2_400[4:27],function(x) ((-coef(summary(lm(x~emb_p2_400$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(emb_p2_400[4:27],function(x) summary(lm(x~emb_p2_400$Time.Sec.))$r.squared))
names(slopes_r1p2_400)<-c("Well","MO2","Rsquared")

#plot the oxygen concentration over time for each plate
par(mfrow=c(3,5))
plot(emb_p1_400$A3~emb_p1_400$Time.Min.)
plot(emb_p1_400$A5~emb_p1_400$Time.Min.)
plot(emb_p1_400$A6~emb_p1_400$Time.Min.)
plot(emb_p1_400$B3~emb_p1_400$Time.Min.)
plot(emb_p1_400$B4~emb_p1_400$Time.Min.)
plot(emb_p1_400$B6~emb_p1_400$Time.Min.)
plot(emb_p1_400$C1~emb_p1_400$Time.Min.)
plot(emb_p1_400$C2~emb_p1_400$Time.Min.)
plot(emb_p1_400$C4~emb_p1_400$Time.Min.)
plot(emb_p1_400$C6~emb_p1_400$Time.Min.)
plot(emb_p1_400$D1~emb_p1_400$Time.Min.)
plot(emb_p1_400$D2~emb_p1_400$Time.Min.)
plot(emb_p1_400$D3~emb_p1_400$Time.Min.)
plot(emb_p1_400$D4~emb_p1_400$Time.Min.)
plot(emb_p1_400$D6~emb_p1_400$Time.Min.)

plot(emb_p2_400$A1~emb_p2_400$Time.Min.)
plot(emb_p2_400$A3~emb_p2_400$Time.Min.)
plot(emb_p2_400$A4~emb_p2_400$Time.Min.)
plot(emb_p2_400$A6~emb_p2_400$Time.Min.)
plot(emb_p2_400$B1~emb_p2_400$Time.Min.)
plot(emb_p2_400$B2~emb_p2_400$Time.Min.)
plot(emb_p2_400$B6~emb_p2_400$Time.Min.)
plot(emb_p2_400$C1~emb_p2_400$Time.Min.)
plot(emb_p2_400$C2~emb_p2_400$Time.Min.)
plot(emb_p2_400$C3~emb_p2_400$Time.Min.)
plot(emb_p2_400$C6~emb_p2_400$Time.Min.)
plot(emb_p2_400$D1~emb_p2_400$Time.Min.)
plot(emb_p2_400$D3~emb_p2_400$Time.Min.)
plot(emb_p2_400$D4~emb_p2_400$Time.Min.)
plot(emb_p2_400$D6~emb_p2_400$Time.Min.)


#calculate the blanks
t1blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="A1"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="B4"])
t2blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="A4"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="D5"])
t3blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="B1"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="C5"])
t4blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="B5"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="D2"])
t5blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="C3"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="B5"])
t6blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="C5"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="C4"])
t7blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="B2"],slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="D5"])
t8blank<-mean(slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="A2"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="A5"])
t9blank<-mean(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="A2"],slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="B3"])


#Subtract the blanks
emb_p1b<-data.frame(c("A3","A5","A6","B3","B4","B6","C1","C2","C4","C6","D1","D2","D3","D4","D6"),
                    c(slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="A3"]-t1blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="A5"]-t8blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="A6"]-t7blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="B3"]-t5blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="B4"]-t7blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="B6"]-t7blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="C1"]-t9blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="C2"]-t2blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="C4"]-t3blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="C6"]-t4blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="D1"]-t9blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="D2"]-t8blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="D3"]-t6blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="D4"]-t5blank,
                      slopes_r1p1_400$MO2[slopes_r1p1_400$Well=="D6"]-t5blank))
names(emb_p1b)<-c("Well","MO2")
emb_p2b<-data.frame(c("A1","A3","A4","A6","B1","B2","B6","C1","C2","C3","C6","D1","D3","D4","D6"),
                    c(slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="A1"]-t3blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="A3"]-t1blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="A4"]-t4blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="A6"]-t6blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="B1"]-t8blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="B2"]-t7blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="B6"]-t3blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="C1"]-t9blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="C2"]-t1blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="C3"]-t6blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="C6"]-t2blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="D1"]-t4blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="D3"]-t2blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="D4"]-t4blank,
                      slopes_r1p2_400$MO2[slopes_r1p2_400$Well=="D6"]-t6blank))
names(emb_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataseet and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
emb_p1b<-dplyr::full_join(emb_p1b,emb_trmt_p1,by="Well")
emb_p1b<-emb_p1b[1:15,] #remove the blanks
emb_p2b<-dplyr::full_join(emb_p2b,emb_trmt_p2,by="Well")
emb_p2b<-emb_p2b[1:15,] #remove the blanks
emb_dana<-rbind(emb_p1b[-c(6,15),],emb_p2b) #combine the two plates into one dataset, removing 6 and 15 because of problems. 
row.names(emb_dana)<-NULL #renumber the rows so it's easy to index if need be.

#analyze the MO2 with respect to CO2
dana_emb_model<-lm(emb_dana$MO2~emb_dana$CO2_level)
anova(dana_emb_model) #CO2 level is significant, p=0.035

#calculate the group means and do a post hoc Tukey test
TukeyHSD(aov(emb_dana$MO2~emb_dana$CO2_level))

library(plyr)
dana_emb_sum<-ddply(emb_dana,"CO2_level",summarise,N=length(MO2),MeanMO2=mean(MO2),SE=sd(MO2)/sqrt(N))
dana_emb_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 




#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
P1A1<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$A1,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("A1",times=33),rep("blankt1",times=33),rep("amb",times=33))
P1A2<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$A2,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("A2",times=33),rep("blankt9",times=33),rep("high",times=33))
P1A3<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$A3,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("A3",times=33),rep("t1",times=33),rep("amb",times=33))
P1A4<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$A4,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("A4",times=33),rep("blankt2",times=33),rep("med",times=33))
P1A5<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$A5,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("A5",times=33),rep("t8",times=33),rep("amb",times=33))
P1A6<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$A6,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("A6",times=33),rep("t7",times=33),rep("high",times=33))
P1B1<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$B1,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("B1",times=33),rep("blankt3",times=33),rep("high",times=33))
P1B2<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$B2,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("B2",times=33),rep("blankt7",times=33),rep("high",times=33))
P1B3<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$B3,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("B3",times=33),rep("t5",times=33),rep("med",times=33))
P1B4<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$B4,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("B4",times=33),rep("t7",times=33),rep("high",times=33))
P1B5<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$B5,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("B5",times=33),rep("blankt4",times=33),rep("med",times=33))
P1C1<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$C1,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("C1",times=33),rep("t9",times=33),rep("high",times=33))
P1C2<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$C2,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("C2",times=33),rep("t2",times=33),rep("med",times=33))
P1C3<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$C3,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("C3",times=33),rep("blankt5",times=33),rep("med",times=33))
P1C4<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$C4,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("C4",times=33),rep("t3",times=33),rep("high",times=33))
P1C5<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$C5,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("C5",times=33),rep("blankt6",times=33),rep("amb",times=33))
P1C6<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$C6,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("C6",times=33),rep("t4",times=33),rep("med",times=33))
P1D1<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$D1,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("D1",times=33),rep("t9",times=33),rep("high",times=33))
P1D2<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$D2,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("D2",times=33),rep("t8",times=33),rep("amb",times=33))
P1D3<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$D3,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("D3",times=33),rep("t6",times=33),rep("amb",times=33))
P1D4<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$D4,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("D4",times=33),rep("t5",times=33),rep("med",times=33))
P1D5<-data.frame(calc_MO2(duration=emb_p1_400$Time.Min.,o2=emb_p1_400$D5,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1_400$T_internal,sal=27.3),rep("D5",times=33),rep("blankt7",times=33),rep("high",times=33))

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

#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES??
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
all<-rbind(P1A3,P1A5,P1A6,P1B3,P1B4,P1C1,P1C2,P1C4,P1C6,P1D1,P1D2,P1D3,P1D4)

#plot the curves
library(ggplot2)
allplot<-ggplot(all, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=2)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4"))
print(allplot)


emb_MO2s_p1<-data.frame("Well"<-c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"),
                         "Pcrit"<-sapply(emb_p1[]))