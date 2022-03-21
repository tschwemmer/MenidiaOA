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

#