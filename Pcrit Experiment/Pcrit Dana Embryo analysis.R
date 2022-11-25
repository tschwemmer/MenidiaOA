#Respirometry analysis from 2021 experiment

#Run 1: Embryos, Dana Hall, 6-14-21

#load data sheets and treatments
emb_p1orig<-read.csv(file.choose(),header=TRUE)
emb_p2orig<-read.csv(file.choose(),header=TRUE)
emb_trmt_p1<-read.csv(file.choose(),header=TRUE)
emb_trmt_p2<-read.csv(file.choose(),header=TRUE)

#remove first 85 minutes from Plate 1, first 30 minutes from Plate 2. Then renumber the rows.
emb_p1<-emb_p1orig[232:4416,]
emb_p2<-emb_p2orig[86:4415,]
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
#For RMR try using initial ~30 min, or average 3 20 min chunks
emb_p1_rmr<-emb_p1[3:93,]
emb_p2_rmr<-emb_p2[3:93,]

slopes_r1p1_rmr<-data.frame(names(emb_p1_rmr)[4:27], sapply(emb_p1_rmr[4:27],function(x) ((-coef(summary(lm(x~emb_p1_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                        sapply(emb_p1_rmr[4:27],function(x) summary(lm(x~emb_p1_rmr$Time.Sec.))$r.squared))
names(slopes_r1p1_rmr)<-c("Well","MO2","Rsquared")

slopes_r1p2_rmr<-data.frame(names(emb_p2_rmr)[4:27], sapply(emb_p2_rmr[4:27],function(x) ((-coef(summary(lm(x~emb_p2_rmr$Time.Sec.)))[2])/31.9988)*1800), 
                            sapply(emb_p2_rmr[4:27],function(x) summary(lm(x~emb_p2_rmr$Time.Sec.))$r.squared))
names(slopes_r1p2_rmr)<-c("Well","MO2","Rsquared")

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
t1blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A1"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B4"])
t2blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A4"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D5"])
t3blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B1"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C5"])
t4blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B5"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D2"])
t5blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C3"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B5"])
t6blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C5"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C4"])
t7blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B2"],slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D5"])
t8blank<-mean(slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A2"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A5"])
t9blank<-mean(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A2"],slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B3"])


#Subtract the blanks
emb_p1b<-data.frame(c("A3","A5","A6","B3","B4","B6","C1","C2","C4","C6","D1","D2","D3","D4","D6"),
                    c(slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A3"]-t1blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A5"]-t8blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="A6"]-t7blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B3"]-t5blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B4"]-t7blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="B6"]-t7blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C1"]-t9blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C2"]-t2blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C4"]-t3blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="C6"]-t4blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D1"]-t9blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D2"]-t8blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D3"]-t6blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D4"]-t5blank,
                      slopes_r1p1_rmr$MO2[slopes_r1p1_rmr$Well=="D6"]-t5blank))
names(emb_p1b)<-c("Well","MO2")
emb_p2b<-data.frame(c("A1","A3","A4","A6","B1","B2","B6","C1","C2","C3","C6","D1","D3","D4","D6"),
                    c(slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A1"]-t3blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A3"]-t1blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A4"]-t4blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="A6"]-t6blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B1"]-t8blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B2"]-t7blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="B6"]-t3blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C1"]-t9blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C2"]-t1blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C3"]-t6blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="C6"]-t2blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D1"]-t4blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D3"]-t2blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D4"]-t4blank,
                      slopes_r1p2_rmr$MO2[slopes_r1p2_rmr$Well=="D6"]-t6blank))
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

dana_emb_mod<-lmer(MO2~CO2_level+(1|Tank),data=emb_dana) #singular fit - don't need random effect
anova(dana_emb_mod)
ranova(dana_emb_mod) #no effect of random effect tank, p=1


#try it the other way
dana_emb_mdl<-aov(emb_dana$MO2~emb_dana$CO2_level/factor(emb_dana$Tank))
summary(dana_emb_mdl) #p=0.0579 for CO2, tank not significant

library(plyr)
dana_emb_sum<-ddply(emb_dana,"CO2_level",summarise,N=length(MO2),MeanMO2=mean(MO2),SE=sd(MO2)/sqrt(N))
dana_emb_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
danaembplot<-ggplot(dana_emb_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("Embryos, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  theme_classic()
print(danaembplot)



##############################################################################################################
##############################################################################################################
#In emb_p1 and emb_p2 I already removed first 85 minutes of plate 1 (wasn't lined up) and first 35 minutes of plate 2 (temperature)

#Trim the unusable parts from the original datasets
#emb_p1formo2<-emb_p1orig[c(231:2041),]
#row.names(emb_p1formo2)<-NULL
#emb_p2formo2<-emb_p2orig[c(85:2041),]
#row.names(emb_p2formo2)<-NULL

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
emb_p1slope1$Index[abs(emb_p1slope1$emb_p1slope)>0.0083] #975:1035, 1755, 1815:1875, 2355, 2775, 2805, 3225, 3255, 3735, 3765 are too high 
#so remove 410:430, 655:710, 855:885, 995:1035, 1145:1185, 1315:1355 minutes
#convert that to index: 973:1033,1707:1870,2304:2394,2723:2843,3170:3290,3678:3797
emb_p2slope1$Index[abs(emb_p2slope1$emb_p2slope)>0.0083] #1125, 1155, 1965, 1995, 2115, 2475, 2505, 2895, 2925, 3345, 3375, 3855, 3885, 4005, 4305 are too high 
#so remove 395:435, 675:715, (725:765 dont throw out), 845:885, 985:1025, 1135:1175, 1305:1385, (1455:1485 don't throw out)
#convert that to index: 1074:1194,1912:2031,2420:2540,2839:2959,3287:3406,3794:4032

emb_p1orig<-emb_p1
emb_p2orig<-emb_p2

#Trim the unusable parts from the original datasets for calc_mo2 analysis
emb_p1orig[c(973:1033,1707:1870,2304:2394,2723:2843,3170:3290,3678:3797),c(4:27)]<-NA
emb_p2orig[c(1074:1194,1912:2031,2420:2540,2839:2959,3287:3406,3794:4032),c(4:27)]<-NA


emb_p1orig$A6[958:972]<-NA
emb_p1orig$C1[958:972]<-NA
emb_p1orig$D1[958:972]<-NA
emb_p1orig$D2[958:972]<-NA
emb_p1orig$D3[958:972]<-NA

emb_p2orig$A3[1195:1539]<-NA
emb_p2orig$A4[1195:1911]<-NA
emb_p2orig$B2[c(1763:1911,2226:2419)]<-NA
emb_p2orig$C2[865:1073]<-NA
emb_p2orig$C3[c(2032:2419,3660:3793)]<-NA
emb_p2orig$D4[865:1314]<-NA




#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)

#Use make_bins() to set bin width but use different rows based on when it bottoms out. Use increments of 50. 
#P1A1bin<-make_bins(o2=emb_p1orig$A1[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1A2bin<-make_bins(o2=emb_p1orig$A2[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1A3bin<-make_bins(o2=emb_p1orig$A3[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1A4bin<-make_bins(o2=emb_p1orig$A4[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1A5bin<-make_bins(o2=emb_p1orig$A5[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1A6bin<-make_bins(o2=emb_p1orig$A6[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1B1bin<-make_bins(o2=emb_p1orig$B1[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1B2bin<-make_bins(o2=emb_p1orig$B2[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1B3bin<-make_bins(o2=emb_p1orig$B3[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1B4bin<-make_bins(o2=emb_p1orig$B4[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1B5bin<-make_bins(o2=emb_p1orig$B5[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1B6bin<-make_bins(o2=emb_p1orig$B6[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1C1bin<-make_bins(o2=emb_p1orig$C1[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1C2bin<-make_bins(o2=emb_p1orig$C2[1:1318],duration=emb_p1orig$Time.Min.[1:1318],max_o2_width=1/10,min_o2_width=1/30)
#P1C3bin<-make_bins(o2=emb_p1orig$C3[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1C4bin<-make_bins(o2=emb_p1orig$C4[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1C5bin<-make_bins(o2=emb_p1orig$C5[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1C6bin<-make_bins(o2=emb_p1orig$C6[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1D1bin<-make_bins(o2=emb_p1orig$D1[1:4185],duration=emb_p1orig$Time.Min.[1:4185],max_o2_width=1/10,min_o2_width=1/30)
P1D2bin<-make_bins(o2=emb_p1orig$D2[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1D3bin<-make_bins(o2=emb_p1orig$D3[1:1318],duration=emb_p1orig$Time.Min.[1:1318],max_o2_width=1/10,min_o2_width=1/30)
P1D4bin<-make_bins(o2=emb_p1orig$D4[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
#P1D5bin<-make_bins(o2=emb_p1orig$D5[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)
P1D6bin<-make_bins(o2=emb_p1orig$D6[1:1706],duration=emb_p1orig$Time.Min.[1:1706],max_o2_width=1/10,min_o2_width=1/30)

P2A1bin<-make_bins(o2=emb_p2orig$A1[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2A2bin<-make_bins(o2=emb_p2orig$A2[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2A3bin<-make_bins(o2=emb_p2orig$A3[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2A4bin<-make_bins(o2=emb_p2orig$A4[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2A5bin<-make_bins(o2=emb_p2orig$A5[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2A6bin<-make_bins(o2=emb_p2orig$A6[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2B1bin<-make_bins(o2=emb_p2orig$B1[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2B2bin<-make_bins(o2=emb_p2orig$B2[1:3600],duration=emb_p2orig$Time.Min.[1:3600],max_o2_width=1/10,min_o2_width=1/30)
#P2B3bin<-make_bins(o2=emb_p2orig$B3[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2B4bin<-make_bins(o2=emb_p2orig$B4[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2B5bin<-make_bins(o2=emb_p2orig$B5[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2B6bin<-make_bins(o2=emb_p2orig$B6[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2C1bin<-make_bins(o2=emb_p2orig$C1[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2C2bin<-make_bins(o2=emb_p2orig$C2[1:4166],duration=emb_p2orig$Time.Min.[1:4166],max_o2_width=1/10,min_o2_width=1/30)
P2C3bin<-make_bins(o2=emb_p2orig$C3[1:4166],duration=emb_p2orig$Time.Min.[1:4166],max_o2_width=1/10,min_o2_width=1/30)
#P2C4bin<-make_bins(o2=emb_p2orig$C4[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2C5bin<-make_bins(o2=emb_p2orig$C5[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2C6bin<-make_bins(o2=emb_p2orig$C6[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2D1bin<-make_bins(o2=emb_p2orig$D1[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2D2bin<-make_bins(o2=emb_p2orig$D2[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2D3bin<-make_bins(o2=emb_p2orig$D3[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2D4bin<-make_bins(o2=emb_p2orig$D4[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
#P2D5bin<-make_bins(o2=emb_p2orig$D5[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)
P2D6bin<-make_bins(o2=emb_p2orig$D6[1:4330],duration=emb_p2orig$Time.Min.[1:4330],max_o2_width=1/10,min_o2_width=1/30)



#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)
#P1A1<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$A1,o2_unit="mg_per_l",bin_width=P1A1bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("A1"),rep("t1"),rep("amb"))
#P1A2<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$A2,o2_unit="mg_per_l",bin_width=P1A2bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("A2"),rep("t9"),rep("blank1"))
P1A3<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$A3,o2_unit="mg_per_l",bin_width=P1A3bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("A3"),rep("t1"),rep("amb"))
#P1A4<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$A4,o2_unit="mg_per_l",bin_width=P1A4bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("A4"),rep("t2"),rep("amb"))
P1A5<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$A5,o2_unit="mg_per_l",bin_width=P1A5bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("A5"),rep("t8"),rep("amb"))
P1A6<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$A6,o2_unit="mg_per_l",bin_width=P1A6bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("A6"),rep("t7"),rep("high"))
#P1B1<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$B1,o2_unit="mg_per_l",bin_width=P1B1bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("B1"),rep("t3"),rep("med"))
#P1B2<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$B2,o2_unit="mg_per_l",bin_width=P1B2bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("B2"),rep("t7"),rep("high"))
P1B3<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$B3,o2_unit="mg_per_l",bin_width=P1B3bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("B3"),rep("t5"),rep("med"))
P1B4<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$B4,o2_unit="mg_per_l",bin_width=P1B4bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("B4"),rep("t7"),rep("high"))
#P1B5<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$B5,o2_unit="mg_per_l",bin_width=P1B5bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("B5"),rep("t4"),rep("med"))
P1B6<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$B6,o2_unit="mg_per_l",bin_width=P1B6bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("B6"),rep("t7"),rep("high"))
P1C1<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$C1,o2_unit="mg_per_l",bin_width=P1C1bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("C1"),rep("t9"),rep("high"))
P1C2<-data.frame(calc_MO2(duration=emb_p1orig[1:1318,]$Time.Min.,o2=emb_p1orig[1:1318,]$C2,o2_unit="mg_per_l",bin_width=P1C2bin,vol=0.0006,temp=emb_p1orig[1:1318,]$T_internal,sal=27.3),rep("C2"),rep("t2"),rep("med"))
#P1C3<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$C3,o2_unit="mg_per_l",bin_width=P1C3bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("C3"),rep("t5"),rep("blank3"))
P1C4<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$C4,o2_unit="mg_per_l",bin_width=P1C4bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("C4"),rep("t3"),rep("high"))
#P1C5<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$C5,o2_unit="mg_per_l",bin_width=P1C5bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("C5"),rep("t6"),rep("med"))
P1C6<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$C6,o2_unit="mg_per_l",bin_width=P1C6bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("C6"),rep("t4"),rep("med"))
P1D1<-data.frame(calc_MO2(duration=emb_p1orig[1:4185,]$Time.Min.,o2=emb_p1orig[1:4185,]$D1,o2_unit="mg_per_l",bin_width=P1D1bin,vol=0.0006,temp=emb_p1orig[1:4185,]$T_internal,sal=27.3),rep("D1"),rep("t9"),rep("high"))
P1D2<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$D2,o2_unit="mg_per_l",bin_width=P1D2bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("D2"),rep("t8"),rep("amb"))
P1D3<-data.frame(calc_MO2(duration=emb_p1orig[1:1318,]$Time.Min.,o2=emb_p1orig[1:1318,]$D3,o2_unit="mg_per_l",bin_width=P1D3bin,vol=0.0006,temp=emb_p1orig[1:1318,]$T_internal,sal=27.3),rep("D3"),rep("t6"),rep("amb"))
P1D4<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$D4,o2_unit="mg_per_l",bin_width=P1D4bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("D4"),rep("t5"),rep("med"))
#P1D5<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$D5,o2_unit="mg_per_l",bin_width=P1D5bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("D5"),rep("t7"),rep("amb"))
P1D6<-data.frame(calc_MO2(duration=emb_p1orig[1:1706,]$Time.Min.,o2=emb_p1orig[1:1706,]$D6,o2_unit="mg_per_l",bin_width=P1D6bin,vol=0.0006,temp=emb_p1orig[1:1706,]$T_internal,sal=27.3),rep("D6"),rep("t5"),rep("med"))

P2A1<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$A1,o2_unit="mg_per_l",bin_width=P2A1bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("A1"),rep("t3"),rep("high"))
#P2A2<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$A2,o2_unit="mg_per_l",bin_width=P2A2bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("A2"),rep("t8"),rep("amb"))
P2A3<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$A3,o2_unit="mg_per_l",bin_width=P2A3bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("A3"),rep("t1"),rep("amb"))
P2A4<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$A4,o2_unit="mg_per_l",bin_width=P2A4bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("A4"),rep("t4"),rep("med"))
#P2A5<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$A5,o2_unit="mg_per_l",bin_width=P2A5bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("A5"),rep("t8"),rep("amb"))
P2A6<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$A6,o2_unit="mg_per_l",bin_width=P2A6bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("A6"),rep("t6"),rep("amb"))
P2B1<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$B1,o2_unit="mg_per_l",bin_width=P2B1bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("B1"),rep("t8"),rep("amb"))
P2B2<-data.frame(calc_MO2(duration=emb_p2orig[1:3600,]$Time.Min.,o2=emb_p2orig[1:3600,]$B2,o2_unit="mg_per_l",bin_width=P2B2bin,vol=0.0005,temp=emb_p2orig[1:3600,]$T_internal,sal=27.3),rep("B2"),rep("t7"),rep("high"))
#P2B3<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$B3,o2_unit="mg_per_l",bin_width=P2B3bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("B3"),rep("t9"),rep("med"))
#P2B4<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$B4,o2_unit="mg_per_l",bin_width=P2B4bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("B4"),rep("t1"),rep("high"))
#P2B5<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$B5,o2_unit="mg_per_l",bin_width=P2B5bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("B5"),rep("t5"),rep("high"))
P2B6<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$B6,o2_unit="mg_per_l",bin_width=P2B6bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("B6"),rep("t3"),rep("high"))
P2C1<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$C1,o2_unit="mg_per_l",bin_width=P2C1bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("C1"),rep("t9"),rep("high"))
P2C2<-data.frame(calc_MO2(duration=emb_p2orig[1:4166,]$Time.Min.,o2=emb_p2orig[1:4166,]$C2,o2_unit="mg_per_l",bin_width=P2C2bin,vol=0.0005,temp=emb_p2orig[1:4166,]$T_internal,sal=27.3),rep("C2"),rep("t1"),rep("amb"))
P2C3<-data.frame(calc_MO2(duration=emb_p2orig[1:4166,]$Time.Min.,o2=emb_p2orig[1:4166,]$C3,o2_unit="mg_per_l",bin_width=P2C3bin,vol=0.0005,temp=emb_p2orig[1:4166,]$T_internal,sal=27.3),rep("C3"),rep("t6"),rep("amb"))
#P2C4<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$C4,o2_unit="mg_per_l",bin_width=P2C4bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("C4"),rep("t6"),rep("high"))
#P2C5<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$C5,o2_unit="mg_per_l",bin_width=P2C5bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("C5"),rep("t3"),rep("high"))
P2C6<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$C6,o2_unit="mg_per_l",bin_width=P2C6bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("C6"),rep("t2"),rep("med"))
P2D1<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$D1,o2_unit="mg_per_l",bin_width=P2D1bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("D1"),rep("t4"),rep("med"))
#P2D2<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$D2,o2_unit="mg_per_l",bin_width=P2D2bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("D2"),rep("t4"),rep("med"))
P2D3<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$D3,o2_unit="mg_per_l",bin_width=P2D3bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("D3"),rep("t2"),rep("med"))
P2D4<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$D4,o2_unit="mg_per_l",bin_width=P2D4bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("D4"),rep("t4"),rep("med"))
#P2D5<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$D5,o2_unit="mg_per_l",bin_width=P2D5bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("D5"),rep("t2"),rep("med"))
P2D6<-data.frame(calc_MO2(duration=emb_p2orig[1:4330,]$Time.Min.,o2=emb_p2orig[1:4330,]$D6,o2_unit="mg_per_l",bin_width=P2D6bin,vol=0.0005,temp=emb_p2orig[1:4330,]$T_internal,sal=27.3),rep("D6"),rep("t6"),rep("amb"))

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
P1A1$MO2b<-P1A1$MO2-t1blank
P1A2$MO2b<-P1A2$MO2-t9blank
P1A3$MO2b<-P1A3$MO2-t1blank
P1A4$MO2b<-P1A4$MO2-t2blank
P1A5$MO2b<-P1A5$MO2-t8blank
P1A6$MO2b<-P1A6$MO2-t7blank
P1B1$MO2b<-P1B1$MO2-t3blank
P1B2$MO2b<-P1B2$MO2-t7blank
P1B3$MO2b<-P1B3$MO2-t5blank
P1B4$MO2b<-P1B4$MO2-t7blank
P1B5$MO2b<-P1B5$MO2-t4blank
P1B6$MO2b<-P1B6$MO2-t7blank
P1C1$MO2b<-P1C1$MO2-t9blank
P1C2$MO2b<-P1C2$MO2-t2blank
P1C3$MO2b<-P1C3$MO2-t5blank
P1C4$MO2b<-P1C4$MO2-t3blank
P1C5$MO2b<-P1C5$MO2-t6blank
P1C6$MO2b<-P1C6$MO2-t4blank
P1D1$MO2b<-P1D1$MO2-t9blank
P1D2$MO2b<-P1D2$MO2-t8blank
P1D3$MO2b<-P1D3$MO2-t6blank
P1D4$MO2b<-P1D4$MO2-t5blank
P1D5$MO2b<-P1D5$MO2-t7blank
P1D6$MO2b<-P1D6$MO2-t5blank
P2A1$MO2b<-P2A1$MO2-t3blank
P2A2$MO2b<-P2A2$MO2-t8blank
P2A3$MO2b<-P2A3$MO2-t1blank
P2A4$MO2b<-P2A4$MO2-t4blank
P2A5$MO2b<-P2A5$MO2-t8blank
P2A6$MO2b<-P2A6$MO2-t6blank
P2B1$MO2b<-P2B1$MO2-t8blank
P2B2$MO2b<-P2B2$MO2-t7blank
P2B3$MO2b<-P2B3$MO2-t9blank
P2B4$MO2b<-P2B4$MO2-t1blank
P2B5$MO2b<-P2B5$MO2-t5blank
P2B6$MO2b<-P2B6$MO2-t3blank
P2C1$MO2b<-P2C1$MO2-t9blank
P2C2$MO2b<-P2C2$MO2-t1blank
P2C3$MO2b<-P2C3$MO2-t6blank
P2C4$MO2b<-P2C4$MO2-t6blank
P2C5$MO2b<-P2C5$MO2-t3blank
P2C6$MO2b<-P2C6$MO2-t2blank
P2D1$MO2b<-P2D1$MO2-t4blank
P2D2$MO2b<-P2D2$MO2-t4blank
P2D3$MO2b<-P2D3$MO2-t2blank
P2D4$MO2b<-P2D4$MO2-t4blank
P2D5$MO2b<-P2D5$MO2-t2blank
P2D6$MO2b<-P2D6$MO2-t6blank

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
allemb<-rbind(P1A3,P1A5,P1A6,P1B3,P1B4,P1B6,P1C1,P1C2,P1C4,P1C6,P1D1,P1D2,P1D3,P1D4,P1D6,
           P2A1,P2A3,P2A4,P2A6,P2B1,P2B2,P2B6,P2C1,P2C2,P2C3,P2C6,P2D1,P2D3,P2D4,P2D6)

#plot the curves
library(ggplot2)
allplot<-ggplot(allemb, aes(x=O2_MEAN,y=MO2,colour=Well))+
  geom_line(lwd=1)+
  scale_colour_manual(values=c("brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3","deeppink1","deeppink4","azure4","brown","red1","darkorange1","darkgoldenrod1","chartreuse1","darkolivegreen4","cadetblue1","cadetblue","blue3","darkorchid3"))
print(allplot)



hist(P1A1$MO2b,breaks=10) 
hist(P1A2$MO2b,breaks=10)
hist(P1A3$MO2b,breaks=10) #one below -0.002 and one above 0.01
hist(P1A4$MO2b,breaks=10) 
hist(P1A5$MO2b,breaks=10) #five below -0.002 and one above 0.01
hist(P1A6$MO2b,breaks=10) #nine below -0.002 
hist(P1B1$MO2b,breaks=10)
hist(P1B2$MO2b,breaks=10)
hist(P1B3$MO2b,breaks=10) #ten below -0.002
hist(P1B4$MO2b,breaks=10) #four below -0.002
hist(P1B5$MO2b,breaks=10)
hist(P1B6$MO2b,breaks=10) #one below -0.002
hist(P1C1$MO2b,breaks=10) #eleven below -0.002
hist(P1C2$MO2b,breaks=10) #a LOT below -0.002
hist(P1C3$MO2b,breaks=10) 
hist(P1C4$MO2b,breaks=10) #two below -0.002
hist(P1C5$MO2b,breaks=10)
hist(P1C6$MO2b,breaks=10) #fifteen below -0.002
hist(P1D1$MO2b,breaks=10) #fifteen below -0.002 and a lot of negative ones
hist(P1D2$MO2b,breaks=10) #six below -0.002 and two above 0.01
hist(P1D3$MO2b,breaks=10) #many below -0.002
hist(P1D4$MO2b,breaks=10) #seven below -0.002
hist(P1D5$MO2b,breaks=10)
hist(P1D6$MO2b,breaks=10) #a LOT below -0.002
hist(P2A1$MO2b,breaks=10)
hist(P2A2$MO2b,breaks=10)
hist(P2A3$MO2b,breaks=10) #four below -0.002
hist(P2A4$MO2b,breaks=10) #a lot below -0.002
hist(P2A5$MO2b,breaks=10)
hist(P2A6$MO2b,breaks=10) #eleven below -0.002
hist(P2B1$MO2b,breaks=10) #seven below -0.002
hist(P2B2$MO2b,breaks=10) #six below -0.002
hist(P2B3$MO2b,breaks=10)
hist(P2B4$MO2b,breaks=10) 
hist(P2B5$MO2b,breaks=10)
hist(P2B6$MO2b,breaks=10) #six below -0.002
hist(P2C1$MO2b,breaks=10) #nine below -0.002
hist(P2C2$MO2b,breaks=10) #five below -0.002
hist(P2C3$MO2b,breaks=10) #a lot below -0.002
hist(P2C4$MO2b,breaks=10)
hist(P2C5$MO2b,breaks=10) 
hist(P2C6$MO2b,breaks=10) #a LOT below -0.002
hist(P2D1$MO2b,breaks=10) #one below -0.002
hist(P2D2$MO2b,breaks=10)
hist(P2D3$MO2b,breaks=10) #a lot below -0.002
hist(P2D4$MO2b,breaks=10) #a lot below -0.002
hist(P2D5$MO2b,breaks=10)
hist(P2D6$MO2b,breaks=10) #three below -0.002

#remove extreme points, and rename the row numbers before and after
row.names(P1C2)<-NULL
row.names(P1C4)<-NULL
row.names(P1D1)<-NULL
row.names(P1D2)<-NULL
row.names(P1D3)<-NULL
row.names(P1D4)<-NULL
row.names(P1D6)<-NULL
row.names(P2A3)<-NULL
row.names(P2A4)<-NULL
row.names(P2B1)<-NULL
row.names(P2B2)<-NULL
row.names(P2B6)<-NULL
row.names(P2C1)<-NULL
row.names(P2C2)<-NULL
row.names(P2C3)<-NULL
row.names(P2C6)<-NULL
row.names(P2D3)<-NULL
row.names(P2D4)<-NULL


P1C2<-P1C2[-c(66:111),]
P1C4<-P1C4[-c(2),]
P1D1<-P1D1[-c(124,125),]
P1D2<-P1D2[-c(2),]
P1D3<-P1D3[-c(76),]
P1D4<-P1D4[-c(14,22),]
P1D6<-P1D6[-c(7,9),]
P2A3<-P2A3[-c(43,44,52,54,55,56),]
P2A4<-P2A4[-c(45:47,58:63,93:95,98),]
P2B1<-P2B1[-c(38:42,50:52,62:67),]
P2B2<-P2B2[-c(35:37,48,49,62,64,76:81,95),]
P2B6<-P2B6[-c(14,31:33,44:48,57,58,68:72),]
P2C1<-P2C1[-c(13,27:29,38:43,51:53,62:68,96:103),]
P2C2<-P2C2[-c(13,40,51:53,65),]
P2C3<-P2C3[-c(40,41,42,51:55,67:69,83,97:99),]
P2C6<-P2C6[-c(15,16,29,31:33,42:47,55:57,66:73),]
P2D3<-P2D3[-c(14:16,31:37,45:50,58:60,70:76),]
P2D4<-P2D4[-c(27,29:31,41:46,54:56,66:72),]


row.names(P1C2)<-NULL
row.names(P1C4)<-NULL
row.names(P1D1)<-NULL
row.names(P1D2)<-NULL
row.names(P1D3)<-NULL
row.names(P1D4)<-NULL
row.names(P1D6)<-NULL
row.names(P2A3)<-NULL
row.names(P2A4)<-NULL
row.names(P2B1)<-NULL
row.names(P2B2)<-NULL
row.names(P2B6)<-NULL
row.names(P2C1)<-NULL
row.names(P2C2)<-NULL
row.names(P2C3)<-NULL
row.names(P2C6)<-NULL
row.names(P2D3)<-NULL
row.names(P2D4)<-NULL



emb_dana$Pcrit_alpha<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$MO2b)['Alpha'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['Alpha'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['Alpha'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['Alpha'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['Alpha'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['Alpha'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['Alpha'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['Alpha'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['Alpha'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['Alpha'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['Alpha'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['Alpha'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['Alpha'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$MO2b)['Alpha'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['Alpha'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['Alpha'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['Alpha'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['Alpha'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['Alpha'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['Alpha'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['Alpha'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['Alpha'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['Alpha'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$MO2b)['Alpha'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$MO2b)['Alpha'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['Alpha'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['Alpha'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['Alpha'])
emb_dana$Pcrit_break<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$MO2b)['Breakpoint'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['Breakpoint'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['Breakpoint'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['Breakpoint'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['Breakpoint'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['Breakpoint'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['Breakpoint'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['Breakpoint'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['Breakpoint'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['Breakpoint'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['Breakpoint'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['Breakpoint'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['Breakpoint'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$MO2b)['Breakpoint'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['Breakpoint'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['Breakpoint'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['Breakpoint'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['Breakpoint'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['Breakpoint'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['Breakpoint'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['Breakpoint'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['Breakpoint'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['Breakpoint'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$MO2b)['Breakpoint'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$MO2b)['Breakpoint'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['Breakpoint'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['Breakpoint'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['Breakpoint'])
emb_dana$Pcrit_subPI<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$MO2b)['Sub_PI'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['Sub_PI'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['Sub_PI'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['Sub_PI'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['Sub_PI'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['Sub_PI'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['Sub_PI'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['Sub_PI'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['Sub_PI'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['Sub_PI'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['Sub_PI'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['Sub_PI'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['Sub_PI'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$MO2b)['Sub_PI'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['Sub_PI'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['Sub_PI'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['Sub_PI'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['Sub_PI'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['Sub_PI'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['Sub_PI'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['Sub_PI'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['Sub_PI'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['Sub_PI'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$MO2b)['Sub_PI'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$MO2b)['Sub_PI'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['Sub_PI'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['Sub_PI'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['Sub_PI'])
emb_dana$Pcrit_NLR<-c(calc_pcrit(P1A3$O2_MEAN,P1A3$MO2b)['NLR'],
                        calc_pcrit(P1A5$O2_MEAN,P1A5$MO2b)['NLR'],
                        calc_pcrit(P1A6$O2_MEAN,P1A6$MO2b)['NLR'],
                        calc_pcrit(P1B3$O2_MEAN,P1B3$MO2b)['NLR'],
                        calc_pcrit(P1B4$O2_MEAN,P1B4$MO2b)['NLR'],
                        calc_pcrit(P1C1$O2_MEAN,P1C1$MO2b)['NLR'],
                        calc_pcrit(P1C2$O2_MEAN,P1C2$MO2b)['NLR'],
                        calc_pcrit(P1C4$O2_MEAN,P1C4$MO2b)['NLR'],
                        calc_pcrit(P1C6$O2_MEAN,P1C6$MO2b)['NLR'],
                        calc_pcrit(P1D1$O2_MEAN,P1D1$MO2b)['NLR'],
                        calc_pcrit(P1D2$O2_MEAN,P1D2$MO2b)['NLR'],
                        calc_pcrit(P1D3$O2_MEAN,P1D3$MO2b)['NLR'],
                        calc_pcrit(P1D4$O2_MEAN,P1D4$MO2b)['NLR'],
                        calc_pcrit(P2A1$O2_MEAN,P2A1$MO2b)['NLR'],
                        calc_pcrit(P2A3$O2_MEAN,P2A3$MO2b)['NLR'],
                        calc_pcrit(P2A4$O2_MEAN,P2A4$MO2b)['NLR'],
                        calc_pcrit(P2A6$O2_MEAN,P2A6$MO2b)['NLR'],
                        calc_pcrit(P2B1$O2_MEAN,P2B1$MO2b)['NLR'],
                        calc_pcrit(P2B2$O2_MEAN,P2B2$MO2b)['NLR'],
                        calc_pcrit(P2B6$O2_MEAN,P2B6$MO2b)['NLR'],
                        calc_pcrit(P2C1$O2_MEAN,P2C1$MO2b)['NLR'],
                        calc_pcrit(P2C2$O2_MEAN,P2C2$MO2b)['NLR'],
                        calc_pcrit(P2C3$O2_MEAN,P2C3$MO2b)['NLR'],
                        calc_pcrit(P2C6$O2_MEAN,P2C6$MO2b)['NLR'],
                        calc_pcrit(P2D1$O2_MEAN,P2D1$MO2b)['NLR'],
                        calc_pcrit(P2D3$O2_MEAN,P2D3$MO2b)['NLR'],
                        calc_pcrit(P2D4$O2_MEAN,P2D4$MO2b)['NLR'],
                        calc_pcrit(P2D6$O2_MEAN,P2D6$MO2b)['NLR'])
emb_dana$alpha<-c(calc_alpha(P1A3$O2_MEAN,P1A3$MO2b)$alpha,
                        calc_alpha(P1A5$O2_MEAN,P1A5$MO2b)$alpha,
                        calc_alpha(P1A6$O2_MEAN,P1A6$MO2b)$alpha,
                        calc_alpha(P1B3$O2_MEAN,P1B3$MO2b)$alpha,
                        calc_alpha(P1B4$O2_MEAN,P1B4$MO2b)$alpha,
                        calc_alpha(P1C1$O2_MEAN,P1C1$MO2b)$alpha,
                        calc_alpha(P1C2$O2_MEAN,P1C2$MO2b)$alpha,
                        calc_alpha(P1C4$O2_MEAN,P1C4$MO2b)$alpha,
                        calc_alpha(P1C6$O2_MEAN,P1C6$MO2b)$alpha,
                        calc_alpha(P1D1$O2_MEAN,P1D1$MO2b)$alpha,
                        calc_alpha(P1D2$O2_MEAN,P1D2$MO2b)$alpha,
                        calc_alpha(P1D3$O2_MEAN,P1D3$MO2b)$alpha,
                        calc_alpha(P1D4$O2_MEAN,P1D4$MO2b)$alpha,
                        calc_alpha(P2A1$O2_MEAN,P2A1$MO2b)$alpha,
                        calc_alpha(P2A3$O2_MEAN,P2A3$MO2b)$alpha,
                        calc_alpha(P2A4$O2_MEAN,P2A4$MO2b)$alpha,
                        calc_alpha(P2A6$O2_MEAN,P2A6$MO2b)$alpha,
                        calc_alpha(P2B1$O2_MEAN,P2B1$MO2b)$alpha,
                        calc_alpha(P2B2$O2_MEAN,P2B2$MO2b)$alpha,
                        calc_alpha(P2B6$O2_MEAN,P2B6$MO2b)$alpha,
                        calc_alpha(P2C1$O2_MEAN,P2C1$MO2b)$alpha,
                        calc_alpha(P2C2$O2_MEAN,P2C2$MO2b)$alpha,
                        calc_alpha(P2C3$O2_MEAN,P2C3$MO2b)$alpha,
                        calc_alpha(P2C6$O2_MEAN,P2C6$MO2b)$alpha,
                        calc_alpha(P2D1$O2_MEAN,P2D1$MO2b)$alpha,
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

P2A4seg<-selgmented(lm(MO2b~O2_MEAN,data=P2A4),seg.Z=~O2_MEAN,type='bic',Kmax=6,msg=F)
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
emb_dana$Pcrit_break<-c(NA,NA,NA,  #P1A
                        NA,2.521,  #P1B
                        NA,4.561,3.974,3.944,  #P1C
                        NA,2.98,3.675,4.013,  #P1D
                        2.949,NA,1.452,NA,  #P2A
                        1.847,NA,2.326,  #P2B
                        2.227,NA,1.277,2.396,  #P2C
                        NA,2.002,2.671,NA)  #P2D
emb_dana$spike<-c(0,1,0,  #P1A
                  0,1,  #P1B
                  0,0,0,1,  #P1C
                  0,0,0,0,  #P1D
                  0,0,0,0,  #P2A
                  0,0,0,  #P2B
                  0,0,0,0,  #P2C
                  1,0,0,0)  #P2D

################################################################################################
#Analyze, check model assumptions, and plot
#set levels of CO2_level and Tank
str(emb_dana)
emb_dana$CO2_level<-factor(emb_dana$CO2_level,levels=c("amb","med","high"))

plot(emb_dana$Pcrit_break~emb_dana$CO2_level)

#calculate mean and SE of Pcrit values by treatment
library(plyr)
break_sum<-ddply(emb_dana,"CO2_level",summarise,N=length(Pcrit_break),MeanPcrit=mean(Pcrit_break,na.rm=TRUE),SE=sd(Pcrit_break,na.rm=TRUE)/sqrt(N))
break_sum #in both cases, Pcrit increases in elevated CO2 treatments but so does SE - may need to transform and/or check for outliers


#use lm and anova to test for significance
library(lmerTest)
library(lme4)

break_mod<-lmer(Pcrit_break~CO2_level+(1|Tank),data=emb_dana) #singular fit
ranova(break_mod) #tank doesn't affect fit
anova(break_mod) #p=0.6926

break_mod1<-lm(Pcrit_break~CO2_level,data=emb_dana)
anova(break_mod1) #

#try it the other way
break_mod2<-aov(emb_dana$Pcrit_break~emb_dana$CO2_level/factor(emb_dana$Tank))
summary(break_mod2) #neither CO2 nor Tank is significant


#diagnostics
par(mfrow=c(2,2))
plot(break_mod2) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(emb_dana$Pcrit_break) #p=0.5406
par(mfrow=c(1,1))
hist(emb_dana$Pcrit_break)

#homogeneity of variances
library(car)
leveneTest(emb_dana$Pcrit_break, emb_dana$CO2_level) #p=0.3706

#plot the data - means and SEs
library(ggplot2)
library(grid)
danaembpcritplot<-ggplot(break_sum, aes(x=CO2_level,y=MeanPcrit))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanPcrit-SE,ymax=MeanPcrit+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("Embryos, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0,4))+
  theme_classic()
print(danaembpcritplot)


#Calculate the percent from each treatment that are NA (oxygen-dependent the whole time)
#but since three of them were not oxygen-dependent but still have NAs we subtract those from the corresponding trmt
pct_NA_amb<-100*(sum(is.na(emb_dana$Pcrit_break[emb_dana$CO2_level=="amb"]))-2)/length(emb_dana$Pcrit_break[emb_dana$CO2_level=="amb"])
pct_NA_med<-100*sum(is.na(emb_dana$Pcrit_break[emb_dana$CO2_level=="med"]))/length(emb_dana$Pcrit_break[emb_dana$CO2_level=="med"])
pct_NA_high<-100*(sum(is.na(emb_dana$Pcrit_break[emb_dana$CO2_level=="high"]))-1)/length(emb_dana$Pcrit_break[emb_dana$CO2_level=="high"])


###############################################################################################
#Calculate RMR as the average MO2 for which O2>Pcrit

emb_dana$RMR<-c(mean(c(P1A3$MO2b[P1A3$O2_MEAN>4])),
                mean(c(P1A5$MO2b[P1A5$O2_MEAN>3])),
                mean(c(P1A6$MO2b[P1A6$O2_MEAN>4])),
                mean(c(P1B3$MO2b[P1B3$O2_MEAN>4])),
                mean(c(P1B4$MO2b[P1B4$O2_MEAN>emb_dana[5,6]])),
                mean(c(P1C1$MO2b[P1C1$O2_MEAN>4])),
                mean(c(P1C2$MO2b[P1C2$O2_MEAN>emb_dana[7,6]])),
                mean(c(P1C4$MO2b[P1C4$O2_MEAN>emb_dana[8,6]])),
                mean(c(P1C6$MO2b[P1C6$O2_MEAN>emb_dana[9,6]])),
                mean(c(P1D1$MO2b[P1D1$O2_MEAN>3])),
                mean(c(P1D2$MO2b[P1D2$O2_MEAN>emb_dana[11,6]])),
                mean(c(P1D3$MO2b[P1D3$O2_MEAN>emb_dana[12,6]])),
                mean(c(P1D4$MO2b[P1D4$O2_MEAN>emb_dana[13,6]])),
                mean(c(P2A1$MO2b[P2A1$O2_MEAN>emb_dana[14,6]])),
                mean(c(P2A3$MO2b[P2A3$O2_MEAN>4])),
                mean(c(P2A4$MO2b[P2A4$O2_MEAN>emb_dana[16,6]])),
                mean(c(P2A6$MO2b[P2A6$O2_MEAN>3])),
                mean(c(P2B1$MO2b[P2B1$O2_MEAN>emb_dana[18,6]])),
                mean(c(P2B2$MO2b[P2B2$O2_MEAN>4])),
                mean(c(P2B6$MO2b[P2B6$O2_MEAN>emb_dana[20,6]])),
                mean(c(P2C1$MO2b[P2C1$O2_MEAN>emb_dana[21,6]])),
                mean(c(P2C2$MO2b[P2C2$O2_MEAN>4])),
                mean(c(P2C3$MO2b[P2C3$O2_MEAN>emb_dana[23,6]])),
                mean(c(P2C6$MO2b[P2C6$O2_MEAN>emb_dana[24,6]])),
                mean(c(P2D1$MO2b[P2D1$O2_MEAN>4])),
                mean(c(P2D3$MO2b[P2D3$O2_MEAN>emb_dana[26,6]])),
                mean(c(P2D4$MO2b[P2D4$O2_MEAN>emb_dana[27,6]])),
                mean(c(P2D6$MO2b[P2D6$O2_MEAN>4])))

#analyze RMR
dana_emb_model<-lm(emb_dana$RMR~emb_dana$CO2_level)
anova(dana_emb_model) #CO2 level is not significant

dana_emb_mod<-lmer(RMR~CO2_level+(1|Tank),data=emb_dana) #singular fit
anova(dana_emb_mod) #p=0.2872
ranova(dana_emb_mod) #random effect of tank doesn't affect results. 

#try it the other way
dana_emb_mdl<-aov(sqrt(emb_dana$RMR)~emb_dana$CO2_level/factor(emb_dana$Tank))
summary(dana_emb_mdl) #Not significant



#diagnostics
par(mfrow=c(2,2))
plot(dana_emb_mdl) 

#For ANOVA the assumptions are normality of the DATA and homogeneity of variances
#normality of data
shapiro.test(emb_dana$RMR) #good p=0.7352
par(mfrow=c(1,1))
hist(emb_dana$RMR) #it is right skewed by a 5 individuals that are >0.005

#homogeneity of variances
library(car)
leveneTest(emb_dana$RMR, emb_dana$CO2_level) #p=0.0496 may need to transform

#calculate the group means 

library(plyr)
dana_emb_sum<-ddply(emb_dana,"CO2_level",summarise,N=length(na.omit(RMR)),MeanMO2=mean(RMR,na.rm=TRUE),SE=sd(RMR,na.rm=TRUE)/sqrt(N))
dana_emb_sum #elevated CO2 slightly decreases MO2...opposite of previous results. But may need to redo using only data before ~Pcrit if want to compare to previous experiments. 

#plot the data - means and SEs
library(ggplot2)
library(grid)
danaembplot<-ggplot(dana_emb_sum, aes(x=CO2_level,y=MeanMO2))+
  geom_point(size=3,shape=16)+
  geom_errorbar(aes(ymin=MeanMO2-SE,ymax=MeanMO2+SE),width=0.2)+
  annotation_custom(grobTree(textGrob("Embryos, Exp. 1",x=0.5,y=0.98,gp=gpar(fontsize=16,fontface="bold"))))+
  coord_cartesian(ylim=c(0.0,0.006))+
  theme_classic()
print(danaembplot)

#Calculate the percentage in each treatment that have spike
pct_spike_amb<-100*sum(emb_dana$spike[emb_dana$CO2_level=="amb"])/length(emb_dana$spike[emb_dana$CO2_level=="amb"])
pct_spike_med<-100*sum(emb_dana$spike[emb_dana$CO2_level=="med"])/length(emb_dana$spike[emb_dana$CO2_level=="med"])
pct_spike_high<-100*sum(emb_dana$spike[emb_dana$CO2_level=="high"])/length(emb_dana$spike[emb_dana$CO2_level=="high"])

