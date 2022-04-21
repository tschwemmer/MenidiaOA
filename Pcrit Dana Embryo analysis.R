#Respirometry analysis from 2021 experiment

#Embryos, Dana Hall, 6-14-21

#load data sheets and treatments
emb_p1<-read.csv(file.choose(),header=TRUE)
emb_p2<-read.csv(file.choose(),header=TRUE)
emb_trmt_p1<-read.csv(file.choose(),header=TRUE)
emb_trmt_p2<-read.csv(file.choose(),header=TRUE)


#check structure
str(emb_p1)
str(emb_p2)
str(emb_trmt_p1)
str(emb_trmt_p2)

#make the treatment variables factors
emb_trmt_p1$Well<-factor(emb_trmt_p1$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
emb_trmt_p2$Well<-factor(emb_trmt_p2$Well,levels=c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"))
emb_trmt_p1$CO2_level<-factor(emb_trmt_p1$CO2_level,levels=c("amb","med","high","blank"))
emb_trmt_p2$CO2_level<-factor(emb_trmt_p2$CO2_level,levels=c("amb","med","high","blank"))
emb_trmt_p1$Tank<-factor(emb_trmt_p1$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))
emb_trmt_p2$Tank<-factor(emb_trmt_p2$Tank,levels=c("t1","t2","t3","t4","t5","t6","t7","t8","t9"))

#Use calc_MO2() function from 'respirometry' package to calculate MO2 for binned chunks of time for each column of the data sheets and store it in a new dataframe
library(respirometry)


p1C1<-calc_MO2(duration=emb_p1$Time_min,o2=emb_p1$C1,o2_unit="mg_per_l",bin_width=10,vol=0.0005,temp=24,sal=27.3,atm_pres=962)

p1D1<-calc_MO2(duration=emb_p1$Time_min,o2=emb_p1$D1,o2_unit="mg_per_l",bin_width=5,vol=0.0005,temp=24,sal=27.3,atm_pres=962)
plot(p1D1$MO2~p1D1$O2_MEAN)
plot(emb_p1$D1~emb_p1$Time_min)


emb_MO2s_p1<-data.frame("Well"<-c("A1","B1","C1","D1","A2","B2","C2","D2","A3","B3","C3","D3","A4","B4","C4","D4","A5","B5","C5","D5","A6","B6","C6","D6"),
                         "Pcrit"<-sapply(emb_p1[]))