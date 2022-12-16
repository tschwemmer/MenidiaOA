#saving datasets to back them up in folders under My Documents
#types to save: 
#   -'all' dataframe that has the MO2-O2 data for each well compiled (P1A1 thru P2D6)
#   -dataframe with calculated RMR and Pcrit data and treatments (e.g. lar_flax)
#   -trimmed versions of raw data that were used for MO2-O2 calculations (e.g. lar_p1orig)
#Also enter the blanks in in case that needed to be re-entered (but don't run now)

#Dana embryos

#Dana 2dph
write.csv(alllar,file.choose(),row.names=FALSE)
write.csv(lar_dana,file.choose(),row.names=FALSE)
write.csv(lar_p1orig,file.choose(),row.names=FALSE)
write.csv(lar_p2orig,file.choose(),row.names=FALSE)
#t1blank<-0.001890335578
#t2blank<-0.002397990987
#t3blank<-0.001820060471
#t4blank<-0.003679446810
#t5blank<-0.001727475180
#t6blank<-0.003054593815
#t7blank<-0.001301545276
#t8blank<-0.000989728787
#t9blank<-0.003046755616

#Dana 5dph

#Flax embryos

#Flax 2dph
write.csv(alllar,file.choose(),row.names=FALSE)
write.csv(lar_flax,file.choose(),row.names=FALSE)
write.csv(lar_p1orig,file.choose(),row.names=FALSE)
write.csv(lar_p2orig,file.choose(),row.names=FALSE)
#blank1<-0.000871565467
#blank2<-0.001374710564
#blank3<-0.000898772861

#Flax 5dph