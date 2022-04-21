#Calculating experiment-specific means for respirometry dataset

#load the data, use the spreadsheet submitted to BCO-DMO
rd<-read.csv(file.choose(),header=TRUE)

#make experiment, target_pCO2, target_temp, and target_DO factors instead of integers
rd$experiment<-factor(rd$experiment,levels=c("1","2","3","4","5","6"))
rd$target_pCO2<-factor(rd$target_pCO2,levels=c("400","2200","4200"))
rd$target_temp<-factor(rd$target_temp,levels=c("17","20","24","28"))
rd$target_DO<-factor(rd$target_DO,levels=c("2.5","3","4","8"))

#Replace 'nd' with NA
rd[rd == "nd"]<-NA

#Make embryo_RMR and larva_RMR numberic instead of character 
rd$embryo_RMR<-as.numeric(rd$embryo_RMR)
rd$larva_RMR<-as.numeric(rd$larva_RMR)

#Embryos - calculate means for each treatment combo within experiment 1
mean(c(rd$embryo_RMR[rd$experiment=="1"&rd$target_pCO2=="400"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="1"&rd$target_pCO2=="2200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="1"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="1"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)

#Larvae - experiment 1
mean(c(rd$larva_RMR[rd$experiment=="1"&rd$target_pCO2=="400"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="1"&rd$target_pCO2=="2200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="1"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="1"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)


#Embryos - none for experiment 2
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="400"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="2200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="4200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="400"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="2200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="4200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="2"&rd$target_pCO2=="4200"&rd$target_temp=="24"]),na.rm=TRUE)

#Larvae - experiment 2
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="400"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="2200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="4200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="400"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="2200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="4200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="2"&rd$target_pCO2=="4200"&rd$target_temp=="24"]),na.rm=TRUE)


#Embryos - experiment 3
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="400"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="2200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="4200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="400"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="2200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="4200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="3"&rd$target_pCO2=="4200"&rd$target_temp=="24"]),na.rm=TRUE)

#Larvae - experiment 3
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="400"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="2200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="4200"&rd$target_temp=="17"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="400"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="2200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="4200"&rd$target_temp=="20"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="3"&rd$target_pCO2=="4200"&rd$target_temp=="24"]),na.rm=TRUE)


#Embryos - experiment 4
mean(c(rd$embryo_RMR[rd$experiment=="4"&rd$target_pCO2=="400"&rd$target_temp=="28"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="4"&rd$target_pCO2=="2200"&rd$target_temp=="28"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="4"&rd$target_pCO2=="4200"&rd$target_temp=="28"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="4"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="4"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="4"&rd$target_pCO2=="4200"&rd$target_temp=="24"]),na.rm=TRUE)

#Larvae - experiment 4
mean(c(rd$larva_RMR[rd$experiment=="4"&rd$target_pCO2=="400"&rd$target_temp=="28"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="4"&rd$target_pCO2=="2200"&rd$target_temp=="28"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="4"&rd$target_pCO2=="4200"&rd$target_temp=="28"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="4"&rd$target_pCO2=="400"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="4"&rd$target_pCO2=="2200"&rd$target_temp=="24"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="4"&rd$target_pCO2=="4200"&rd$target_temp=="24"]),na.rm=TRUE)


#Embryos - experiment 5
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="2.5"]),na.rm=TRUE)

#Larvae - experiment 5
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="2.5"]),na.rm=TRUE)

#Embryos - experiment 6
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="3"]),na.rm=TRUE)

#Larvae - experiment 6
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="3"]),na.rm=TRUE)


#Standard errors
#Embryos - experiment 5
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="2.5"]),na.rm=TRUE)

#Larvae - experiment 5
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="400"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="2200"&rd$target_DO=="2.5"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="5"&rd$target_pCO2=="4200"&rd$target_DO=="2.5"]),na.rm=TRUE)

#Embryos - experiment 6
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$embryo_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="3"]),na.rm=TRUE)

#Larvae - experiment 6
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="8"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="4"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="400"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="2200"&rd$target_DO=="3"]),na.rm=TRUE)
mean(c(rd$larva_RMR[rd$experiment=="6"&rd$target_pCO2=="4200"&rd$target_DO=="3"]),na.rm=TRUE)

