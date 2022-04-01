#Gene Expression Data Analysis
#March 2022

#Data: Maya Pena Lobel
#Analysis: Teresa Schwemmer

#Load data for each life stage and save as an environment so I can easily load it again
ge_data<-read.csv(file.choose(),header=TRUE)

#check the structure and make variables factors when necessary
str(ge_data)
ge_data$age<-factor(ge_data$age,levels=c("Y","T"))
ge_data$Experiment<-factor(ge_data$Experiment,levels=c("exp2","exp3"))
ge_data$gene<-factor(ge_data$gene,levels=c("nka","vha","nbc","nhe1","nhe3","nkcc1","nkcc2"))
ge_data$CO2.level<-factor(ge_data$CO2.level,levels=c("400uatm","2200uatm","4200uatm"))
ge_data$CO2.level2<-factor(ge_data$CO2.level2,levels=c("C","H","VH"))
ge_data$Temp.level<-factor(ge_data$Temp.level,levels=c("17C","20C","24C"))
ge_data$Temp.level2<-factor(ge_data$Temp.level2,levels=c("L","M","H"))
str(ge_data)

#subset the data by gene to make models easier to code
ge_nka<-ge_data[ge_data$gene=="nka",]
ge_vha<-ge_data[ge_data$gene=="vha",]
ge_nbc<-ge_data[ge_data$gene=="nbc",]
ge_nhe1<-ge_data[ge_data$gene=="nhe1",]
ge_nhe3<-ge_data[ge_data$gene=="nhe3",]
ge_nkcc1<-ge_data[ge_data$gene=="nkcc1",]
ge_nkcc2<-ge_data[ge_data$gene=="nkcc2",]


#Continuous analysis - linear regression
lmnka<-lm(expression~CO2*Temp,data=ge_nka)
summary(lmnka) #temp significant

lmvha<-lm(expression~CO2*Temp,data=ge_vha)
summary(lmvha) #temp significant

lmnbc<-lm(expression~CO2*Temp,data=ge_nbc)
summary(lmnbc)

lmnhe1<-lm(expression~CO2*Temp,data=ge_nhe1)
summary(lmnhe1) #interaction significant

lmnhe3<-lm(expression~CO2*Temp,data=ge_nhe3)
summary(lmnhe3) #interaction significant

lmnkcc1<-lm(expression~CO2*Temp,data=ge_nkcc1)
summary(lmnkcc1) #co2 significant

lmnkcc2<-lm(expression~CO2*Temp,data=ge_nkcc2)
summary(lmnkcc2) #interaction significant


#plot means and all data with linear regressions




