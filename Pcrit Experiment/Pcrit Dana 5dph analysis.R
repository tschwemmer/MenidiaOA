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
lrv_p1_rmr<-data.frame(lrv_p1$Time.Min.[136:226], lrv_p1$Time.Sec.[136:226], lrv_p1$A3[136:226], lrv_p1$A4[136:226],lrv_p1$B3[136:226],lrv_p1$B6[136:226],
                       lrv_p1$C1[136:226],lrv_p1$C5[136:226],lrv_p1$D1[136:226],lrv_p1$D2[136:226],lrv_p1$D4[136:226],lrv_p1$D5[136:226],
                       lrv_p1$A1[241:331],lrv_p1$B2[241:331],lrv_p1$C3[744:834],
                       lrv_p1$A2[136:226],lrv_p1$A5[136:226],lrv_p1$A6[136:226],lrv_p1$B1[136:226],lrv_p1$B5[136:226],lrv_p1$C2[136:226],
                       lrv_p1$C4[136:226],lrv_p1$C6[136:226],lrv_p1$D3[744:834],lrv_p1$D6[136:226])
names(lrv_p1_rmr)<-c("Time.Min.","Time.Sec.","A3","A4","B3","B6","C1","C5","D1","D2","D4","D5","A1","B2","C3","A2","A5","A6","B1","B5","C2","C4","C6","D3","D6")

lrv_p2_rmr<-data.frame(lrv_p2$Time.Min.[76:166], lrv_p2$Time.Sec.[76:166], lrv_p2$A2[76:166],lrv_p2$A4[76:166],lrv_p2$A5[76:166],lrv_p2$A6[76:166],
                       lrv_p2$B1[76:166],lrv_p2$B2[76:166],lrv_p2$B3[76:166],lrv_p2$B4[76:166],lrv_p2$C1[76:166],lrv_p2$C3[76:166],lrv_p2$D1[76:166],
                       lrv_p2$D3[76:166],lrv_p2$D4[76:166],
                       lrv_p2$C4[181:271],
                       lrv_p2$A1[76:166],lrv_p2$A3[76:166],lrv_p2$B5[76:166],lrv_p2$B6[76:166],lrv_p2$C2[76:166],
                       lrv_p2$C5[76:166],lrv_p2$D2[76:166],lrv_p2$D5[76:166],lrv_p2$D6[76:166])
names(lrv_p2_rmr)<-c("Time.Min.","Time.Sec.","A2","A4","A5","A6","B1","B2","B3","B4","C1","C3","D1","D3","D4","C4","A1","A3","B5","B6","C2","C5","D2","D5","D6")


slopes_r3p1_200<-data.frame(names(lrv_p1_200)[4:27], sapply(lrv_p1_200[4:27],function(x) ((-coef(summary(lm(x~lrv_p1_200$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lrv_p1_200[4:27],function(x) summary(lm(x~lrv_p1_200$Time.Sec.))$r.squared))
names(slopes_r3p1_200)<-c("Well","MO2","Rsquared")

slopes_r3p2_200<-data.frame(names(lrv_p2_200)[4:27], sapply(lrv_p2_200[4:27],function(x) ((-coef(summary(lm(x~lrv_p2_200$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(lrv_p2_200[4:27],function(x) summary(lm(x~lrv_p2_200$Time.Sec.))$r.squared))
names(slopes_r3p2_200)<-c("Well","MO2","Rsquared")



#calculate the blanks
t1blank<-mean(slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="D1"],slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="D3"])
t2blank<-mean(slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="A1"],slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="A6"])
t3blank<-mean(slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="A6"],slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="D6"])
t4blank<-mean(slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="A2"],slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="A4"])
t5blank<-mean(slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="D2"],slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="D3"])
t6blank<-mean(slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="C1"],slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="C2"])
t7blank<-mean(slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="C2"],slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="C4"])
t8blank<-mean(slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="B3"],slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="B6"])
t9blank<-mean(slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="B3"],slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="B5"])


#Subtract the blanks
lar_p1b<-data.frame(c("A2","A3","A4","A5","B1","B2","B4","B5","C1","C3","C5","C6","D2","D4","D5","D6"),
                    c(slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="A2"]-t2blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="A3"]-t2blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="A4"]-t2blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="A5"]-t2blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="B1"]-t8blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="B2"]-t8blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="B4"]-t8blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="B5"]-t8blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="C1"]-t7blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="C3"]-t7blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="C5"]-t7blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="C6"]-t7blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="D2"]-t1blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="D4"]-t1blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="D5"]-t1blank,
                      slopes_r3p1_200$MO2[slopes_r3p1_200$Well=="D6"]-t3blank))
names(lar_p1b)<-c("Well","MO2")
lar_p2b<-data.frame(c("A1","A3","A5","B1","B2","B4","B6","C3","C4","C5","C6","D1","D4","D5"),
                    c(slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="A1"]-t4blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="A3"]-t4blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="A5"]-t4blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="B1"]-t9blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="B2"]-t9blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="B4"]-t9blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="B6"]-t3blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="C3"]-t6blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="C4"]-t6blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="C5"]-t6blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="C6"]-t3blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="D1"]-t5blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="D4"]-t5blank,
                      slopes_r3p2_200$MO2[slopes_r3p2_200$Well=="D5"]-t5blank))
names(lar_p2b)<-c("Well","MO2")

#Check that they are all around the same order of magnitude and none are negative. Yay!
#Match treatments up to wells, then rbind into one dataset and remove any that had problems in the notes (e.g. egg was missing from well, there was a bubble or leaky seal) 
library(dplyr)
lar_p1b<-dplyr::full_join(lar_p1b,lar_trmt_p1,by="Well")
lar_p1b<-lar_p1b[1:16,] #remove the blanks
lar_p2b<-dplyr::full_join(lar_p2b,lar_trmt_p2,by="Well")
lar_p2b<-lar_p2b[1:14,] #remove the blanks
lar_dana<-rbind(lar_p1b[-c(16),],lar_p2b) #combine the two plates into one dataset, removing 16 because of problems (larva dried up before measuring). 
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

#calculate the group means 

library(plyr)
dana_lar_sum<-ddply(lar_dana,"CO2_level",summarise,N=length(msmr),MeanMO2=mean(msmr),SE=sd(msmr)/sqrt(N))
dana_lar_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#Trim the unusable parts from the original datasets for calc_mo2() analysis
lar_p1formo2<-lar_p1orig[c(7:2218),]
row.names(lar_p1formo2)<-NULL
lar_p2formo2<-lar_p2orig[c(7:2081),]
row.names(lar_p2formo2)<-NULL

#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
#P1A1<-data.frame(calc_MO2(duration=emb_p1orig$Time.Min.[emb_p1orig$A1<6],o2=emb_p1orig$A1[emb_p1orig$A1<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=emb_p1orig$T_internal[emb_p1orig$A1>4],sal=27.3))#,rep("A1",times=33),rep("blankt1",times=33),rep("amb",times=33))
P1A2<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$A2<6],o2=lar_p1formo2$A2[lar_p1formo2$A2<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$A2<6],sal=27.3),rep("A2",times=26),rep("t2",times=26),rep("med",times=26))
P1A3<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$A3<6],o2=lar_p1formo2$A3[lar_p1formo2$A3<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$A3<6],sal=27.3),rep("A3",times=26),rep("t2",times=26),rep("med",times=26))
P1A4<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$A4<6],o2=lar_p1formo2$A4[lar_p1formo2$A4<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$A4<6],sal=27.3),rep("A4",times=26),rep("t2",times=26),rep("med",times=26))
P1A5<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$A5<6],o2=lar_p1formo2$A5[lar_p1formo2$A5<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$A5<6],sal=27.3),rep("A5",times=26),rep("t2",times=26),rep("med",times=26))
#P1A6<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$A6<6],o2=lar_p1formo2$A6[lar_p1formo2$A6<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$A6<6],sal=27.3),rep("A6",times=26),rep("t7",times=26),rep("high",times=26))
P1B1<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$B1<6],o2=lar_p1formo2$B1[lar_p1formo2$B1<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$B1<6],sal=27.3),rep("B1",times=26),rep("t8",times=26),rep("amb",times=26))
P1B2<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$B2<6],o2=lar_p1formo2$B2[lar_p1formo2$B2<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$B2<6],sal=27.3),rep("B2",times=26),rep("t8",times=26),rep("amb",times=26))
#P1B3<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$B3<6],o2=lar_p1formo2$B3[lar_p1formo2$B3<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$B3<6],sal=27.3),rep("B3",times=26),rep("t5",times=26),rep("med",times=26))
P1B4<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$B4<6],o2=lar_p1formo2$B4[lar_p1formo2$B4<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$B4<6],sal=27.3),rep("B4",times=26),rep("t8",times=26),rep("amb",times=26))
P1B5<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$B5<6],o2=lar_p1formo2$B5[lar_p1formo2$B5<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$B5<6],sal=27.3),rep("B5",times=26),rep("t8",times=26),rep("amb",times=26))
P1C1<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$C1<6],o2=lar_p1formo2$C1[lar_p1formo2$C1<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$C1<6],sal=27.3),rep("C1",times=26),rep("t7",times=26),rep("high",times=26))
#P1C2<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$C2<6],o2=lar_p1formo2$C2[lar_p1formo2$C2<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$C2<6],sal=27.3),rep("C2",times=26),rep("t2",times=26),rep("med",times=26))
P1C3<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$C3<6],o2=lar_p1formo2$C3[lar_p1formo2$C3<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$C3<6],sal=27.3),rep("C3",times=26),rep("t7",times=26),rep("high",times=26))
#P1C4<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$C4<6],o2=lar_p1formo2$C4[lar_p1formo2$C4<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$C4<6],sal=27.3),rep("C4",times=26),rep("t3",times=26),rep("high",times=26))
P1C5<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$C5<6],o2=lar_p1formo2$C5[lar_p1formo2$C5<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$C5<6],sal=27.3),rep("C5",times=26),rep("t7",times=26),rep("high",times=26))
P1C6<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$C6<6],o2=lar_p1formo2$C6[lar_p1formo2$C6<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$C6<6],sal=27.3),rep("C6",times=26),rep("t7",times=26),rep("high",times=26))
#P1D1<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$D1<6],o2=lar_p1formo2$D1[lar_p1formo2$D1<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$D1<6],sal=27.3),rep("D1",times=26),rep("t9",times=26),rep("high",times=26))
P1D2<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$D2<6],o2=lar_p1formo2$D2[lar_p1formo2$D2<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$D2<6],sal=27.3),rep("D2",times=26),rep("t1",times=26),rep("amb",times=26))
#P1D3<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$D3<6],o2=lar_p1formo2$D3[lar_p1formo2$D3<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$D3<6],sal=27.3),rep("D3",times=26),rep("t6",times=26),rep("amb",times=26))
P1D4<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$D4<6],o2=lar_p1formo2$D4[lar_p1formo2$D4<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$D4<6],sal=27.3),rep("D4",times=26),rep("t1",times=26),rep("amb",times=26))
P1D6<-data.frame(calc_MO2(duration=lar_p1formo2$Time.Min.[lar_p1formo2$D6<6],o2=lar_p1formo2$D5[lar_p1formo2$D6<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p1formo2$T_internal[lar_p1formo2$D6<6],sal=27.3),rep("D5",times=26),rep("t1",times=26),rep("amb",times=26))

P2A1<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$A1<6],o2=lar_p2formo2$A1[lar_p2formo2$A1<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$A1<6],sal=27.3),rep("P2A1",times=24),rep("t4",times=24),rep("med",times=24))
#P2A2<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.,o2=lar_p2formo2$A2,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal,sal=27.3),rep("A2",times=33),rep("blankt9",times=33),rep("high",times=33))
P2A3<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$A3<6],o2=lar_p2formo2$A3[lar_p2formo2$A3<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$A3<6],sal=27.3),rep("P2A3",times=24),rep("t4",times=24),rep("med",times=24))
#P2A4<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$A4<6],o2=lar_p2formo2$A4[lar_p2formo2$A4<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$A4<6],sal=27.3),rep("P2A4",times=24),rep("t4",times=24),rep("med",times=24))
P2A5<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$A5<6],o2=lar_p2formo2$A5[lar_p2formo2$A5<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$A5<6],sal=27.3),rep("P2A5",times=24),rep("t4",times=24),rep("med",times=24))
#P2A6<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$A6<6],o2=lar_p2formo2$A6[lar_p2formo2$A6<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$A6<6],sal=27.3),rep("P2A6",times=24),rep("t6",times=24),rep("amb",times=24))
P2B1<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$B1<6],o2=lar_p2formo2$B1[lar_p2formo2$B1<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$B1<6],sal=27.3),rep("P2B1",times=24),rep("t9",times=24),rep("high",times=24))
P2B2<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$B2<6],o2=lar_p2formo2$B2[lar_p2formo2$B2<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$B2<6],sal=27.3),rep("P2B2",times=24),rep("t9",times=24),rep("high",times=24))
#P2B3<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$B3<6],o2=lar_p2formo2$B3[lar_p2formo2$B3<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$B3<6],sal=27.3),rep("P2B3",times=24),rep("t5",times=24),rep("med",times=24))
P2B4<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$B4<6],o2=lar_p2formo2$B4[lar_p2formo2$B4<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$B4<6],sal=27.3),rep("P2B4",times=24),rep("t9",times=24),rep("high",times=24))
#P2B5<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.,o2=lar_p2formo2$B5,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal,sal=27.3),rep("B5",times=33),rep("blankt4",times=33),rep("med",times=33))
#P2B6<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$B6<6],o2=lar_p2formo2$B6[lar_p2formo2$B6<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$B6<6],sal=27.3),rep("P2B6",times=24),rep("t3",times=24),rep("high",times=24))
#P2C1<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$C1<6],o2=lar_p2formo2$C1[lar_p2formo2$C1<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$C1<6],sal=27.3),rep("P2C1",times=24),rep("t9",times=24),rep("high",times=24))
#P2C2<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$C2<6],o2=lar_p2formo2$C2[lar_p2formo2$C2<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$C2<6],sal=27.3),rep("P2C2",times=24),rep("t1",times=24),rep("amb",times=24))
P2C3<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$C3<6],o2=lar_p2formo2$C3[lar_p2formo2$C3<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$C3<6],sal=27.3),rep("P2C3",times=24),rep("t6",times=24),rep("amb",times=24))
P2C4<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$C4<6],o2=lar_p2formo2$C4[lar_p2formo2$C4<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$C4<6],sal=27.3),rep("P2C4",times=24),rep("t6",times=24),rep("amb",times=24))
P2C5<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$C5<6],o2=lar_p2formo2$C5[lar_p2formo2$C5<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$C5<6],sal=27.3),rep("P2C5",times=23),rep("t6",times=23),rep("amb",times=23))
P2C6<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$C6<6],o2=lar_p2formo2$C6[lar_p2formo2$C6<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$C6<6],sal=27.3),rep("P2C6",times=24),rep("t3",times=24),rep("high",times=24))
P2D1<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$D1<6],o2=lar_p2formo2$D1[lar_p2formo2$D1<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$D1<6],sal=27.3),rep("P2D1",times=24),rep("t5",times=24),rep("med",times=24))
#P2D2<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$D2<6],o2=lar_p2formo2$D2[lar_p2formo2$D2<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$D2<6],sal=27.3),rep("D2",times=24),rep("t8",times=24),rep("amb",times=24))
#P2D3<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$D3<6],o2=lar_p2formo2$D3[lar_p2formo2$D3<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$D3<6],sal=27.3),rep("P2D3",times=24),rep("t2",times=24),rep("med",times=24))
P2D4<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$D4<6],o2=lar_p2formo2$D4[lar_p2formo2$D4<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$D4<6],sal=27.3),rep("P2D4",times=24),rep("t5",times=24),rep("med",times=24))
P2D5<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$D5<6],o2=lar_p2formo2$D5[lar_p2formo2$D5<6],o2_unit="mg_per_l",bin_width=30,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$D5<6],sal=27.3),rep("P2D5",times=24),rep("t5",times=24),rep("med",times=24))
#P2D6<-data.frame(calc_MO2(duration=lar_p2formo2$Time.Min.[lar_p2formo2$D6<6],o2=lar_p2formo2$D6[lar_p2formo2$D6<6],o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=lar_p2formo2$T_internal[lar_p2formo2$D6<6],sal=27.3),rep("P2D6",times=24),rep("t6",times=24),rep("amb",times=24))

#names(P1A1)[9:11]<-c("Well","Tank","CO2")
names(P1A2)[9:11]<-c("Well","Tank","CO2")
names(P1A3)[9:11]<-c("Well","Tank","CO2")
names(P1A4)[9:11]<-c("Well","Tank","CO2")
names(P1A5)[9:11]<-c("Well","Tank","CO2")
#names(P1A6)[9:11]<-c("Well","Tank","CO2")
names(P1B1)[9:11]<-c("Well","Tank","CO2")
names(P1B2)[9:11]<-c("Well","Tank","CO2")
#names(P1B3)[9:11]<-c("Well","Tank","CO2")
names(P1B4)[9:11]<-c("Well","Tank","CO2")
names(P1B5)[9:11]<-c("Well","Tank","CO2")
#names(P1B6)[9:11]<-c("Well","Tank","CO2")
names(P1C1)[9:11]<-c("Well","Tank","CO2")
#names(P1C2)[9:11]<-c("Well","Tank","CO2")
names(P1C3)[9:11]<-c("Well","Tank","CO2")
#names(P1C4)[9:11]<-c("Well","Tank","CO2")
names(P1C5)[9:11]<-c("Well","Tank","CO2")
names(P1C6)[9:11]<-c("Well","Tank","CO2")
#names(P1D1)[9:11]<-c("Well","Tank","CO2")
names(P1D2)[9:11]<-c("Well","Tank","CO2")
#names(P1D3)[9:11]<-c("Well","Tank","CO2")
names(P1D4)[9:11]<-c("Well","Tank","CO2")
#names(P1D5)[9:11]<-c("Well","Tank","CO2")
names(P1D6)[9:11]<-c("Well","Tank","CO2")

names(P2A1)[9:11]<-c("Well","Tank","CO2")
#names(P2A2)[9:11]<-c("Well","Tank","CO2")
names(P2A3)[9:11]<-c("Well","Tank","CO2")
#names(P2A4)[9:11]<-c("Well","Tank","CO2")
names(P2A5)[9:11]<-c("Well","Tank","CO2")
#names(P2A6)[9:11]<-c("Well","Tank","CO2")
names(P2B1)[9:11]<-c("Well","Tank","CO2")
names(P2B2)[9:11]<-c("Well","Tank","CO2")
#names(P2B3)[9:11]<-c("Well","Tank","CO2")
names(P2B4)[9:11]<-c("Well","Tank","CO2")
#names(P2B5)[9:11]<-c("Well","Tank","CO2")
#names(P2B6)[9:11]<-c("Well","Tank","CO2")
#names(P2C1)[9:11]<-c("Well","Tank","CO2")
#names(P2C2)[9:11]<-c("Well","Tank","CO2")
names(P2C3)[9:11]<-c("Well","Tank","CO2")
names(P2C4)[9:11]<-c("Well","Tank","CO2")
names(P2C5)[9:11]<-c("Well","Tank","CO2")
names(P2C6)[9:11]<-c("Well","Tank","CO2")
names(P2D1)[9:11]<-c("Well","Tank","CO2")
#names(P2D2)[9:11]<-c("Well","Tank","CO2")
#names(P2D3)[9:11]<-c("Well","Tank","CO2")
names(P2D4)[9:11]<-c("Well","Tank","CO2")
names(P2D5)[9:11]<-c("Well","Tank","CO2")
#names(P2D6)[9:11]<-c("Well","Tank","CO2")

#skip blank adjustments for now, calculate Pcrit and plot ...HOW TO DO AVERAGES?? Fit a line to all of the points from the same treatment/tank. 
#Calculate the mean MO2 for each mean O2 value? Do they match up well? 
alllar<-rbind(P1A2,P1A3,P1A4,P1A5,P1B1,P1B2,P1B4,P1B5,P1C1,P1C3,P1C5,P1C6,P1D2,P1D4,P1D6,
              P2A1,P2A3,P2A5,P2B1,P2B2,P2B4,P2C3,P2C4,P2C5,P2C6,P2D1,P2D4,P2D5)

#plot the curves
library(ggplot2)
par(mfrow=c(1,1))
allplotlar<-ggplot(alllar, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","bisque","indianred1",
                               "orange","cornsilk","mediumspringgreen","mediumseagreen","lightblue1","cyan1","cornflowerblue","lightpink","hotpink","lavenderblush3","khaki4","gray16","darksalmon"))
print(allplotlar)
