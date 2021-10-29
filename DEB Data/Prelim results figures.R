#Figure of preliminary results for AFS 2021 presentation

est_param<-data.frame("Parameter"<-c("Zoom factor","Energy conductance","Kappa","Vol spec somatic maint","Spec cost for structure","Maturity at birth","Maturity at metamorphosis","Maturity at puberty","Weibull aging accel","Shape coef","Scaled funct response","Zoom factor males","Zoom factor","Energy conductance","Kappa","Vol spec somatic maint","Spec cost for structure","Maturity at birth","Maturity at metamorphosis","Maturity at puberty","Weibull aging accel","Shape coef","Scaled funct response","Zoom factor males"),   #Make it so all parameter values are in same column, and then I can index them by CO2 level
                      "CO2"<-c(rep("Ambient",times=12),rep("High",times=12)),
                      "Value"<-c(0.2548,0.00747,0.9994,33.53,5546,0.00001068,0.008586,3.038,0.000001125,0.153,2.042,0.3959,
                                 0.867,0.01019,0.9959,36.43,5239,0.0003393,0.004979,1.632,0.0000009608,0.113,1.816,0.7858))
