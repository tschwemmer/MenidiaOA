#Concannon fecundity trial data

#load data
fecund<-read.csv(file.choose(),header=TRUE)
str(fecund)

#calculate mean, se, and N for TL for each temperature, control CO2
mean(fecund$TL[fecund$Temp==17&fecund$CO2==335])
mean(fecund$TL[fecund$Temp==24&fecund$CO2==347])
length(fecund$TL[fecund$Temp==17&fecund$CO2==335])
length(fecund$TL[fecund$Temp==24&fecund$CO2==347])


#calculate mean, se, and N for TL for each temperature, high co2
mean(fecund$TL[fecund$Temp==17&fecund$CO2==1959])
mean(fecund$TL[fecund$Temp==24&fecund$CO2==2097])
length(fecund$TL[fecund$Temp==17&fecund$CO2==1959])
length(fecund$TL[fecund$Temp==24&fecund$CO2==2097])


#calculate mean, se, and N for wW for each temperature, control CO2
mean(fecund$wW[fecund$Temp==17&fecund$CO2==335])
mean(fecund$wW[fecund$Temp==24&fecund$CO2==347])
length(fecund$wW[fecund$Temp==17&fecund$CO2==335])
length(fecund$wW[fecund$Temp==24&fecund$CO2==347])


#calculate mean, se, and N for wW for each temperature, high co2
mean(fecund$wW[fecund$Temp==17&fecund$CO2==1959])
mean(fecund$wW[fecund$Temp==24&fecund$CO2==2097])
length(fecund$wW[fecund$Temp==17&fecund$CO2==1959])
length(fecund$wW[fecund$Temp==24&fecund$CO2==2097])
