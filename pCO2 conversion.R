library(seacarb)
library(ggplot2)

#FLAX POND DATA#################################################################################################################
pCO2<-matrix(nrow=9,ncol=1)


#06/10/2021
pCO2[1,1]<-as.numeric(carb(flag=15,var1=0.0020374,var2=0.0018714,S=26.4,T=22.6)[9]) #Tank 1
pCO2[2,1]<-as.numeric(carb(flag=15,var1=0.0020535,var2=0.0020513,S=26.5,T=22.2)[9]) #Tank 2
pCO2[3,1]<-as.numeric(carb(flag=15,var1=0.0020500,var2=0.0021364,S=26.6,T=22.0)[9]) #Tank 3

#06/16/2021
pCO2[4,1]<-as.numeric(carb(flag=15,var1=0.0021067,var2=0.0019252,S=28.35,T=22.5)[9]) #Tank 1
pCO2[5,1]<-as.numeric(carb(flag=15,var1=0.0021449,var2=0.0021723,S=28.46,T=22.4)[9]) #Tank 2
pCO2[6,1]<-as.numeric(carb(flag=15,var1=0.0021623,var2=0.0022899,S=28.51,T=22.8)[9]) #Tank 3

#06/21/2021
pCO2[7,1]<-as.numeric(carb(flag=15,var1=0.0021470,var2=0.0019659,S=28.89,T=22.2)[9]) #Tank 1
pCO2[8,1]<-as.numeric(carb(flag=15,var1=0.0021943,var2=0.0021973,S=29.18,T=22.4)[9]) #Tank 2
pCO2[9,1]<-as.numeric(carb(flag=15,var1=0.0022110,var2=0.0022351,S=29.20,T=21.9)[9]) #Tank 3

row.names(pCO2)<-c("TS-F-1-1-061021","TS-F-2-1-061021","TS-F-3-1-061021",
                   "TS-F-1-1-061621","TS-F-2-1-061621","TS-F-3-1-061621",
                   "TS-F-1-1-062121","TS-F-2-1-062121","TS-F-3-1-062121")
pCO2<-data.frame(pCO2)
names(pCO2)<-"pCO2 (uatm)"

#calculate the pCO2 and DIC from TA and pH. ALK is now var2 and pH is var1; DIC is result [16] and pCO2 is result [9]; convert DIC to umol/kg by multiplying by 1,000,000
from_pH<-matrix(nrow=9,ncol=2)
from_pH<-data.frame(from_pH)

###pCO2
#06/10/2021 - pCO2
from_pH[1,1]<-as.numeric(carb(flag=8,var1=8.06,var2=0.0020374,S=26.4,T=22.6)[9]) #Tank 1
from_pH[2,1]<-as.numeric(carb(flag=8,var1=7.50,var2=0.0020535,S=26.5,T=22.2)[9]) #Tank 2
from_pH[3,1]<-as.numeric(carb(flag=8,var1=7.16,var2=0.0020500,S=26.6,T=22.0)[9]) #Tank 3

#06/16/2021 - pCO2
from_pH[4,1]<-as.numeric(carb(flag=8,var1=8.09,var2=0.0021067,S=28.35,T=22.5)[9]) #Tank 1
from_pH[5,1]<-as.numeric(carb(flag=8,var1=7.37,var2=0.0021449,S=28.46,T=22.4)[9]) #Tank 2
from_pH[6,1]<-as.numeric(carb(flag=8,var1=7.07,var2=0.0021623,S=28.51,T=22.8)[9]) #Tank 3

#06/21/2021 - pCO2
from_pH[7,1]<-as.numeric(carb(flag=8,var1=8.08,var2=0.0021470,S=28.89,T=22.2)[9]) #Tank 1
from_pH[8,1]<-as.numeric(carb(flag=8,var1=7.31,var2=0.0021943,S=29.18,T=22.4)[9]) #Tank 2
from_pH[9,1]<-as.numeric(carb(flag=8,var1=7.04,var2=0.0022110,S=29.20,T=21.9)[9]) #Tank 3

###DIC
#06/10/2021 - DIC
from_pH[1,2]<-as.numeric(carb(flag=8,var1=8.06,var2=0.0020374,S=26.4,T=22.6)[16])*1000000 #Tank 1
from_pH[2,2]<-as.numeric(carb(flag=8,var1=7.50,var2=0.0020535,S=26.5,T=22.2)[16])*1000000 #Tank 2
from_pH[3,2]<-as.numeric(carb(flag=8,var1=7.16,var2=0.0020500,S=26.6,T=22.0)[16])*1000000 #Tank 3

#06/16/2021 - DIC
from_pH[4,2]<-as.numeric(carb(flag=8,var1=8.09,var2=0.0021067,S=28.35,T=22.5)[16])*1000000 #Tank 1
from_pH[5,2]<-as.numeric(carb(flag=8,var1=7.37,var2=0.0021449,S=28.46,T=22.4)[16])*1000000 #Tank 2
from_pH[6,2]<-as.numeric(carb(flag=8,var1=7.07,var2=0.0021623,S=28.51,T=22.8)[16])*1000000 #Tank 3

#06/21/2021 - DIC
from_pH[7,2]<-as.numeric(carb(flag=8,var1=8.08,var2=0.0021470,S=28.89,T=22.2)[16])*1000000 #Tank 1
from_pH[8,2]<-as.numeric(carb(flag=8,var1=7.31,var2=0.0021943,S=29.18,T=22.4)[16])*1000000 #Tank 2
from_pH[9,2]<-as.numeric(carb(flag=8,var1=7.04,var2=0.0022110,S=29.20,T=21.9)[16])*1000000 #Tank 3

row.names(from_pH)<-c("TS-F-1-1-061021","TS-F-2-1-061021","TS-F-3-1-061021",
                      "TS-F-1-1-061621","TS-F-2-1-061621","TS-F-3-1-061621",
                      "TS-F-1-1-062121","TS-F-2-1-062121","TS-F-3-1-062121")

names(from_pH)<-c("pCO2_frompH","DIC_frompH")

#make vectors of the alkalinity, DIC (VINDTA), pH, temperature, and salinity data
flax_TA<-c(2037.4,2053.5,2050.0,2106.7,2144.9,2162.3,2147.0,2194.3,2211.0)
flax_DIC_VINDTA<-c(1871.4,2051.3,2136.4,1925.2,2172.3,2289.9,1965.9,2197.3,2235.1)
flax_pH<-c(8.06,7.5,7.16,8.09,7.37,7.07,8.08,7.31,7.04)
flax_temperature<-c(22.6,22.2,22.0,22.5,22.4,22.8,22.2,22.4,21.9)
flax_salinity<-c(26.4,26.5,26.6,28.35,28.46,28.51,28.89,29.18,29.20)


#make a dataframe that contains all of the carbonate chemistry data, tank numbers (and treatments), and dates.
pCO2_forplot<-data.frame("Tank"<-c("Tank 1 - Ambient CO2","Tank 2 - Medium CO2","Tank 3 - High CO2","Tank 1 - Ambient CO2","Tank 2 - Medium CO2","Tank 3 - High CO2","Tank 1 - Ambient CO2","Tank 2 - Medium CO2","Tank 3 - High CO2"),
                         "Date"<-c("06/10/2021","06/10/2021","06/10/2021","06/16/2021","06/16/2021","06/16/2021","06/21/2021","06/21/2021","06/21/2021"),
                         "pCO2_fromDIC"<-pCO2$`pCO2 (uatm)`,
                         "pCO2_frompH"<-from_pH$pCO2_frompH,
                         "DIC_frompH"<-from_pH$DIC_frompH,
                         flax_DIC_VINDTA,
                         flax_TA,
                         flax_pH,
                         flax_temperature,
                         flax_salinity)
names(pCO2_forplot)<-c("Tank","Date","pCO2_fromVINDTA","pCO2_frompH","DIC_frompH","DIC_fromVINDTA","TA","pH_fromYSI","Temperature","Salinity")
pCO2_forplot$Tank<-factor(pCO2_forplot$Tank,levels=c("Tank 1 - Ambient CO2","Tank 2 - Medium CO2","Tank 3 - High CO2"))
pCO2_forplot$Date<-factor(pCO2_forplot$Date,levels=c("06/10/2021","06/16/2021","06/21/2021"))

#plot the pCO2 that was calculated from the TA and DIC (VINDTA)
flaxplot<-ggplot(pCO2_forplot,aes(x=Date,y=pCO2_fromVINDTA,group=Tank,color=Tank),show.legend=TRUE)+
  scale_colour_manual(values=c("blue","darkgreen","red"),labels=c("Tank 1 - Ambient CO2","Tank 2 - Medium CO2","Tank 3 - High CO2"))+
  geom_point(aes(shape=Tank),size=3,shape=16)+
  geom_line(show.legend=FALSE)+
  scale_x_discrete(labels=c("06/10/2021","06/16/2021","06/21/2021"))+
  coord_cartesian(ylim=c(0,6000))+
  theme_classic()
flaxplot

#plot on the same graph the pCO2 that was calculated from the VINDTA DIC and the pCO2 calculated from TA and pH
#Easiest way might be to make a dataframe where the Tank column has the source of the pCO2 value specified
pCO2_comparison<-data.frame("Tank_Source"<-c("T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, pH","T2 Med, pH","T3 Hi, pH","T1 Amb, pH","T2 Med, pH","T3 Hi, pH","T1 Amb, pH","T2 Med, pH","T3 Hi, pH"),
                            "Date"<-c("06/10/2021","06/10/2021","06/10/2021","06/16/2021","06/16/2021","06/16/2021","06/21/2021","06/21/2021","06/21/2021","06/10/2021","06/10/2021","06/10/2021","06/16/2021","06/16/2021","06/16/2021","06/21/2021","06/21/2021","06/21/2021"),
                            cbind(c(pCO2_forplot$pCO2_fromVINDTA,pCO2_forplot$pCO2_frompH)))
names(pCO2_comparison)<-c("Tank_Source","Date","pCO2")
pCO2_comparison$Tank_Source<-factor(pCO2_comparison$Tank_Source,levels=c("T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, pH","T2 Med, pH","T3 Hi, pH"))
pCO2_comparison$Date<-factor(pCO2_comparison$Date,levels=c("06/10/2021","06/16/2021","06/21/2021"))

pCO2_comparison_plot<-ggplot(pCO2_comparison,aes(x=Date,y=pCO2,group=Tank_Source,color=Tank_Source),show.legend=TRUE)+
  scale_colour_manual(values=c("#0e40ff","#028602","#D00000","#8099fe","#7fce7f","#E1a4a4"),labels=c("T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, pH","T2 Med, pH","T3 Hi, pH"))+
  geom_point(aes(shape=Tank_Source),size=3,shape=16)+
  geom_line(show.legend=FALSE)+
  scale_x_discrete(labels=c("06/10/2021","06/16/2021","06/21/2021"))+
  coord_cartesian(ylim=c(0,6000))+
  theme_classic()
pCO2_comparison_plot



#same plot but for DIC
DIC_comparison<-data.frame("Tank_Source"<-c("T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, pH","T2 Med, pH","T3 Hi, pH","T1 Amb, pH","T2 Med, pH","T3 Hi, pH","T1 Amb, pH","T2 Med, pH","T3 Hi, pH"),
                           "Date"<-c("06/10/2021","06/10/2021","06/10/2021","06/16/2021","06/16/2021","06/16/2021","06/21/2021","06/21/2021","06/21/2021","06/10/2021","06/10/2021","06/10/2021","06/16/2021","06/16/2021","06/16/2021","06/21/2021","06/21/2021","06/21/2021"),
                           cbind(c(pCO2_forplot$DIC_fromVINDTA,pCO2_forplot$DIC_frompH)))
names(DIC_comparison)<-c("Tank_Source","Date","DIC")
DIC_comparison$Tank_Source<-factor(DIC_comparison$Tank_Source,levels=c("T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, pH","T2 Med, pH","T3 Hi, pH"))
DIC_comparison$Date<-factor(DIC_comparison$Date,levels=c("06/10/2021","06/16/2021","06/21/2021"))

DIC_comparison_plot<-ggplot(DIC_comparison,aes(x=Date,y=DIC,group=Tank_Source,color=Tank_Source),show.legend=TRUE)+
  scale_colour_manual(values=c("#0e40ff","#028602","#D00000","#8099fe","#7fce7f","#E1a4a4"),labels=c("T1 Amb, DIC","T2 Med, DIC","T3 Hi, DIC","T1 Amb, pH","T2 Med, pH","T3 Hi, pH"))+
  geom_point(aes(shape=Tank_Source),size=3,shape=16)+
  geom_line(show.legend=FALSE)+
  scale_x_discrete(labels=c("06/10/2021","06/16/2021","06/21/2021"))+
  coord_cartesian(ylim=c(1700,2500))+
  theme_classic()
DIC_comparison_plot


#calculate overall mean for each tank
flaxmeans<-data.frame("Tank"<-c("Tank 1 - Ambient CO2","Tank 2 - Medium CO2","Tank 3 - High CO2"),
                      "Mean pCO2"<-c(mean(pCO2_forplot$pCO2[pCO2_forplot$Tank=="Tank 1 - Ambient CO2"]),
                                     mean(pCO2_forplot$pCO2[pCO2_forplot$Tank=="Tank 2 - Medium CO2"]),
                                     mean(pCO2_forplot$pCO2[pCO2_forplot$Tank=="Tank 3 - High CO2"])))
names(flaxmeans)<-c("Tank","Mean pCO2")

#DANA HALL DATA########################################################################################################################
pCO2_D<-matrix(nrow=18,ncol=2)
pCO2_D<-data.frame(pCO2_D)

pCO2_D[,2]<-c(rep("06/10/2021",times=9),rep("06/20/2021",times=9))

#06/10/2021
pCO2_D[1,1]<-as.numeric(carb(flag=15,var1=0.0020172,var2=0.0019104,S=25.58,T=23.4)[9]) #Tank 1
pCO2_D[2,1]<-as.numeric(carb(flag=15,var1=0.0020271,var2=0.0020358,S=25.54,T=23.5)[9]) #Tank 2
pCO2_D[3,1]<-as.numeric(carb(flag=15,var1=0.0020270,var2=0.0021248,S=25.47,T=23.8)[9]) #Tank 3
pCO2_D[4,1]<-as.numeric(carb(flag=15,var1=0.0020245,var2=0.0019609,S=25.80,T=23.7)[9]) #Tank 4
pCO2_D[5,1]<-as.numeric(carb(flag=15,var1=0.0020290,var2=0.0020458,S=25.71,T=23.6)[9]) #Tank 5
pCO2_D[6,1]<-as.numeric(carb(flag=15,var1=0.0020059,var2=0.0019054,S=25.49,T=23.9)[9]) #Tank 6
pCO2_D[7,1]<-as.numeric(carb(flag=15,var1=0.0020078,var2=0.0020672,S=25.38,T=23.9)[9]) #Tank 7
pCO2_D[8,1]<-as.numeric(carb(flag=15,var1=0.0019886,var2=0.0019085,S=25.20,T=23.9)[9]) #Tank 8
pCO2_D[9,1]<-as.numeric(carb(flag=15,var1=0.0020253,var2=0.0021160,S=25.47,T=23.6)[9]) #Tank 9

#06/20/2021
pCO2_D[10,1]<-as.numeric(carb(flag=15,var1=0.0022581,var2=0.0020480,S=29.70,T=23.8)[9]) #Tank 1
pCO2_D[11,1]<-as.numeric(carb(flag=15,var1=0.0023122,var2=0.0022477,S=29.61,T=24.0)[9]) #Tank 2
pCO2_D[12,1]<-as.numeric(carb(flag=15,var1=0.0023058,var2=0.0023523,S=30.07,T=23.8)[9]) #Tank 3
pCO2_D[13,1]<-as.numeric(carb(flag=15,var1=0.0023154,var2=0.0022967,S=30.33,T=24.0)[9]) #Tank 4
pCO2_D[14,1]<-as.numeric(carb(flag=15,var1=0.0023575,var2=0.0022874,S=30.79,T=23.8)[9]) #Tank 5
pCO2_D[15,1]<-as.numeric(carb(flag=15,var1=0.0022815,var2=0.0020992,S=29.92,T=23.8)[9]) #Tank 6
pCO2_D[16,1]<-as.numeric(carb(flag=15,var1=0.0023114,var2=0.0023990,S=30.14,T=24.0)[9]) #Tank 7
pCO2_D[17,1]<-as.numeric(carb(flag=15,var1=0.0022605,var2=0.0020627,S=29.53,T=23.9)[9]) #Tank 8
pCO2_D[18,1]<-as.numeric(carb(flag=15,var1=0.0022938,var2=0.0023339,S=29.79,T=23.8)[9]) #Tank 9

#Add columns for tank and treatment
pCO2_D<-data.frame(pCO2_D,
                   c("Tank 1","Tank 2","Tank 3","Tank 4","Tank 5","Tank 6","Tank 7","Tank 8","Tank 9","Tank 1","Tank 2","Tank 3","Tank 4","Tank 5","Tank 6","Tank 7","Tank 8","Tank 9"),
                   c("A","M","H","M","M","A","H","A","H","A","M","H","M","M","A","H","A","H"))
names(pCO2_D)<-c("pCO2","Date","Tank","Treatment")
pCO2_D$Date<-factor(pCO2_D$Date,levels=c("06/10/2021","06/20/2021"))
pCO2_D$Tank<-factor(pCO2_D$Tank,levels=c("Tank 1","Tank 2","Tank 3","Tank 4","Tank 5","Tank 6","Tank 7","Tank 8","Tank 9"))
pCO2_D$Treatment<-factor(pCO2_D$Treatment,levels=c("A","M","H"))


danaplot<-ggplot(pCO2_D,aes(x=Date,y=pCO2,group=Tank,color=Treatment,shape=Tank),show.legend=TRUE)+
  scale_colour_manual(values=c("blue","darkgreen","red"),labels=c("Ambient CO2","Medium CO2","High CO2"))+
  geom_point(aes(shape=Tank),size=3)+
  scale_shape_manual(values=c(15,16,17,18,20,8,9,10,12),labels=c("Tank 1","Tank 2","Tank 3","Tank 4","Tank 5","Tank 6","Tank 7","Tank 8","Tank 9"))+
  geom_line(show.legend=FALSE)+
  scale_x_discrete(labels=c("06/10/2021","06/20/2021"))+
  theme_classic()
danaplot

#calculate the mean for each tank


#calculate the mean for each treatment (mean of all tanks and both dates, 6 data point per treatment)


