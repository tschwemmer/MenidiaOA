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
ge_data$Experiment<-factor(ge_data$gene,levels=c("nka","vha","nbc","nhe1","nhe3","nkcc1","nkcc2"))