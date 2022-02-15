#Ionocyte Data Analysis
#January 2022


#Load data for each life stage and save as an environment so I can easily load it again
all_emb<-read.csv('Ionocyte/All embryo ionocyte data.csv',header=TRUE)
all_1dph<-read.csv('Ionocyte/All 1dph ionocyte data.csv',header=TRUE)
all_10mm<-read.csv('Ionocyte/All 10mm ionocyte data.csv',header=TRUE)


#Examine the data and create factors when necessary
str(all_emb)
all_emb$Experiment<-factor(all_emb$Experiment,levels=c("exp1","exp3","exp4"))

str(all_1dph)
all_1dph$Experiment<-factor(all_1dph$Experiment,levels=c("exp1","exp2","exp3","exp4"))

str(all_10mm)
all_10mm$Experiment<-factor(all_10mm$Experiment,levels=c("exp2","exp3"))


#Standard deviation analysis of duplicates - embryo
hist(all_emb$SDYolkmm,breaks=50)
abline(v=mean(all_emb$SDYolkmm)+(3*sd(all_emb$SDYolkmm)),col="red",lwd=3) #mean+3SD = 298.5196

hist(all_emb$SDBodymm,breaks=50)
abline(v=mean(all_emb$SDBodymm)+(3*sd(all_emb$SDBodymm)),col="blue",lwd=3) #mean+3SD = 299.1431

#Find the sample numbers of the most extreme ones
all_emb$Sample[all_emb$SDYolkmm>298] #2016-373, 2016-456, 2016-457, 2016-458, 2016-525, 2016-542, 2016-544
all_emb$Sample[all_emb$SDBodymm>299] #2016-380, 2016-517


#Standard deviation analysis of duplicates - 1dph
hist(all_1dph$SDFrontmm,breaks=50) #the problems identified in last year's analysis were corrected: 2016-119, 2016-245a, 2016-270. 400 was the SD threshold. 
hist(all_1dph$SDBackmm,breaks=50) #same, probs seem to be fixed: 2016-127, 2016-319, 2016-323, 2016-558. SD threshold was 190. 

#I think 2017 data was excluded from first analysis - I'll calculate mean+3sd just in case threshold should be adjusted. 
#If any 2017 samples are above the threshold I will examine them
mean(all_1dph$SDFrontmm)+(3*sd(all_1dph$SDFrontmm)) #258.388
mean(all_1dph$SDBackmm)+(3*sd(all_1dph$SDBackmm)) #165.5559
all_1dph$Sample[all_1dph$SDFrontmm>258] #2017-373
all_1dph$Sample[all_1dph$SDBackmm>165] #none from 2017


#Standard deviation analysis of duplicates - 10mm
hist(all_10mm$SDFrontmm,breaks=100)
abline(v=mean(all_10mm$SDFrontmm)+(3*sd(all_10mm$SDFrontmm)),col="magenta",lwd=3) #There are two very extreme ones (2016-105t and 2016-163t) and the rest seem to be below ~200
#The extreme ones were both identified in the 2021 analysis, suggesting the problems haven't been fixed in this version of the dataset. 
#I can just compare the histogram with that one
hist(data1$FrontSD,breaks=100)
abline(v=mean(data1$FrontSD)+(3*sd(data1$FrontSD)),col="blue",lwd=3) #it is different, there are three above the line and one just below it. 

all_10mm$SDFrontmm[all_10mm$Sample=="2016-391t"]
data1$FrontSD[data1$Sample=="391t"]
#105t hasn't been changed
#163t hasn't been changed
#362t changed from 411 to 2 SD. Front Density 1 changed from 919.796 to 341.173, but the count and area are the same. Looking at the counting spreadsheet, density was miscalculated the first time. 
#391t SD changed from 626.8 to 36.3. Front Density 2 changed from 1173.8 to 338.7. The count stayed the same but area increased. It looks like originally an excluded area was accidentally used as the total area but I fixed it in April. 
#So I just need to revisit 105t and 163t for Front. 

hist(all_10mm$SDBackmm,breaks=100)
abline(v=mean(all_10mm$SDBackmm)+(3*sd(all_10mm$SDBackmm)),col="orange",lwd=3) #mean+3sd = 79.29
all_10mm$Sample[all_10mm$SDBackmm>79]
#all the same except the last one is 390t instead of 370t - was that a typo? 
data1$Sample[data1$BackSD>0.000079]
#No, originally 370t and 390t were both greater than that threshold. 
data1$BackSD[data1$Sample=="390t"]
#390t is included now but not before because the threshold I used in 2021 was 0.00009 (or 90 if using mm). 
#370t changed because originally SD was 110.9, now it is 11.65. 
#Back Density 2 is higher in the original data (483.7), now it is 310.4. Looks like the area and count changed. I edited it in April 2021. 
#So the ones that need to be checked are 2016-56t, 2016-57t, 2016-75t, 2016-77t, 2016-233t, 2016-307t, and 2016-390t. 


#As of 2/11/22 after fixing the ones pointed out above, the SDs are all below the original threshold calculated as 3 SDs from mean. Proceed with analysis. 

#__________________________________________________________________________________________________________________

#Load the treatment spreadsheets and merge the dataframes using dplyr::full_join
trmt_emb<-read.csv('Ionocyte/Embryo treatments.csv',header=TRUE)
trmt_1dph<-read.csv('Ionocyte/1dph treatments.csv',header=TRUE)
trmt_10mm<-read.csv('Ionocyte/10mm treatments.csv',header=TRUE)

library(dplyr)
d1_emb<-full_join(all_emb,trmt_emb,by="Sample")
d1_1dph<-full_join(all_1dph,trmt_1dph,by="Sample")
d1_10mm<-full_join(all_10mm,trmt_10mm,by="Sample")
#These are the full datasets with all available information about ionocyte density, treatments, stdev, and metabolic rates. 

#Now create smaller dataframes with only the necessary information for the analysis (everything else is just in case we want it for later analyses).
d2_emb<-d1_emb[,c(1,2,4,43,47,51,54,56,57,58,60,62)]
d2_1dph<-d1_1dph[,c(1,2,4,37,41,45,48,50,51,52,54,56)]
d2_10mm<-d1_10mm[,c(1,2,3,36,40,44,47,49,50,51,53,55)]

#----------------------------------------------------------------------------------------------------------
#Analyze embryos using LMER

#First check structure of dataframe
str(d2_emb)
d2_emb$Experiment.x<-factor(d2_emb$Experiment.x,levels=c("exp1","exp3","exp4"))
d2_emb$CO2.level<-factor(d2_emb$CO2.level,levels=c("400uatm","2200uatm","4200uatm"))
d2_emb$Temp.level<-factor(d2_emb$Temp.level,levels=c("17C","20C","24C","28C"))

#Use plyr to make a summary of the data for yolk, body, and total
library(plyr)
summary_emb<-ddply(d2_emb,c("CO2.level","Temp.level"),summarise,
                   N.yolk=length(AverageYolkDensitymm),Mean.yolk=mean(AverageYolkDensitymm),se.yolk=sd(AverageYolkDensitymm)/sqrt(N.yolk),
                   N.body=length(AverageBodyDensitymm),Mean.body=mean(AverageBodyDensitymm),se.body=sd(AverageBodyDensitymm)/sqrt(N.body),
                   N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
summary_emb

#Use a linear mixed effects model to test for significant effects for yolk, body, and total. 
#Set Experiment.x as a random effect and CO2.level and Temp.level as fixed effects.
library(lme4)
library(lmerTest)
modelyolk<-lmer(AverageYolkDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_emb)
anova(modelyolk)

modelbody<-lmer(AverageBodyDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_emb)
anova(modelbody)

modeltotal<-lmer(AverageTotalDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_emb)
anova(modeltotal)

#Test effect of including Experiment.x in model - create an lm without the random effect and do an anova to compare the two models. 
modelyolk_lm<-lm(AverageYolkDensitymm~CO2.level*Temp.level,data=d2_emb)
modelbody_lm<-lm(AverageBodyDensitymm~CO2.level*Temp.level,data=d2_emb)
modeltotal_lm<-lm(AverageTotalDensitymm~CO2.level*Temp.level,data=d2_emb)

anova(modelyolk,modelyolk_lm)
anova(modelbody,modelbody_lm)
anova(modeltotal,modeltotal_lm)
#Including experiment as a random effect significantly affects all models. ranova() shows this as well. 

#Now use an lm and the continuous (not categorical) independent variables
#Use emmeans to examine significance
library(emmeans)
lmyolk<-lm(AverageYolkDensitymm~CO2*Temp,data=d2_emb)
summary(lmyolk)

lmbody<-lm(AverageBodyDensitymm~CO2*Temp,data=d2_emb)
summary(lmbody)

lmtotal<-lm(AverageTotalDensitymm~CO2*Temp,data=d2_emb)
summary(lmtotal)

#Run diagnostics and look for outliers
library(car)

#Yolk diagnostics
plot(lm(d2_emb$AverageYolkDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),1) #Residuals vs. fitted
plot(lm(d2_emb$AverageYolkDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),2) #Q-Q plot
yolkres<-residuals(modelyolk)
hist(yolkres,breaks=20) #somewhat longer tail on right
shapiro.test(yolkres) #normality test
leveneTest(modelyolk_lm) #Homogeneity of variances test

cook<-cooks.distance(modelyolk)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_emb$AverageYolkDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageYolkDensitymm)-3-1),names(cook),""),col="red")
#I think I need to transform the data, there are a ton of 'outliers'

#Body diagnostics
plot(lm(d2_emb$AverageBodyDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),1) #Residuals vs. fitted
plot(lm(d2_emb$AverageBodyDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),2) #Q-Q plot
bodyres<-residuals(modelbody)
hist(bodyres,breaks=20) #pretty symmetrical except for one observation with residual above 400. 
shapiro.test(bodyres) #normality test
leveneTest(modelbody_lm) #Homogeneity of variances test

cook<-cooks.distance(modelbody)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_emb$AverageBodyDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageBodyDensitymm)-3-1),names(cook),""),col="red")
#similar to yolk; exp 3 and 4 have most 'influential' observations

#Total diagnostics
plot(lm(d2_emb$AverageTotalDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),1) #Residuals vs. fitted
plot(lm(d2_emb$AverageTotalDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),2) #Q-Q plot
totalres<-residuals(modeltotal)
hist(totalres,breaks=20) #pretty symmetrical except for two observations with residuals around 400. 
shapiro.test(totalres) #normality test
leveneTest(modeltotal_lm) #Homogeneity of variances test

cook<-cooks.distance(modeltotal)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_emb$AverageTotalDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageTotalDensitymm)-3-1),names(cook),""),col="red")
#similar to other two

#make plots for yolk and body; then do separate ones for each experiment. 
library(ggplot2)
library(grid)
library(gridExtra)

yolkplot<-ggplot(summary_emb,aes(x=Temp.level,y=Mean.yolk,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.yolk-se.yolk,ymax=Mean.yolk+se.yolk),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24","28"))+
  annotation_custom(grobTree(textGrob("Yolk Sac Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()
print(yolkplot)

bodyplot<-ggplot(summary_emb,aes(x=Temp.level,y=Mean.body,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.body-se.body,ymax=Mean.body+se.body),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24","28"))+
  annotation_custom(grobTree(textGrob("Body (non-yolk) Ionocytes",x=0.1,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()
print(bodyplot)

#to separate by experiment need to make plyr summaries for each experiment
e1embsum<-ddply(d2_emb[d2_emb$Experiment.x=="exp1",],c("CO2.level","Temp.level"),summarise,
                   N.yolk=length(AverageYolkDensitymm),Mean.yolk=mean(AverageYolkDensitymm),se.yolk=sd(AverageYolkDensitymm)/sqrt(N.yolk),
                   N.body=length(AverageBodyDensitymm),Mean.body=mean(AverageBodyDensitymm),se.body=sd(AverageBodyDensitymm)/sqrt(N.body),
                   N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e1embsum

e3embsum<-ddply(d2_emb[d2_emb$Experiment.x=="exp3",],c("CO2.level","Temp.level"),summarise,
                N.yolk=length(AverageYolkDensitymm),Mean.yolk=mean(AverageYolkDensitymm),se.yolk=sd(AverageYolkDensitymm)/sqrt(N.yolk),
                N.body=length(AverageBodyDensitymm),Mean.body=mean(AverageBodyDensitymm),se.body=sd(AverageBodyDensitymm)/sqrt(N.body),
                N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e3embsum

e4embsum<-ddply(d2_emb[d2_emb$Experiment.x=="exp4",],c("CO2.level","Temp.level"),summarise,
                N.yolk=length(AverageYolkDensitymm),Mean.yolk=mean(AverageYolkDensitymm),se.yolk=sd(AverageYolkDensitymm)/sqrt(N.yolk),
                N.body=length(AverageBodyDensitymm),Mean.body=mean(AverageBodyDensitymm),se.body=sd(AverageBodyDensitymm)/sqrt(N.body),
                N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e4embsum

e1yolkplot<-ggplot(e1embsum,aes(x=Temp.level,y=Mean.yolk,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3"))+
  geom_errorbar(aes(ymin=Mean.yolk-se.yolk,ymax=Mean.yolk+se.yolk),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","24"))+
  annotation_custom(grobTree(textGrob("Yolk Sac Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()

e1bodyplot<-ggplot(e1embsum,aes(x=Temp.level,y=Mean.body,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3"))+
  geom_errorbar(aes(ymin=Mean.body-se.body,ymax=Mean.body+se.body),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","24"))+
  annotation_custom(grobTree(textGrob("Body (non-yolk) Ionocytes",x=0.1,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()

grid.arrange(e1yolkplot,e1bodyplot,ncol=2)

e3yolkplot<-ggplot(e3embsum,aes(x=Temp.level,y=Mean.yolk,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.yolk-se.yolk,ymax=Mean.yolk+se.yolk),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Yolk Sac Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()

e3bodyplot<-ggplot(e3embsum,aes(x=Temp.level,y=Mean.body,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.body-se.body,ymax=Mean.body+se.body),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Body (non-yolk) Ionocytes",x=0.1,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()

grid.arrange(e3yolkplot,e3bodyplot,ncol=2)

e4yolkplot<-ggplot(e4embsum,aes(x=Temp.level,y=Mean.yolk,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.yolk-se.yolk,ymax=Mean.yolk+se.yolk),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("24","28"))+
  annotation_custom(grobTree(textGrob("Yolk Sac Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()

e4bodyplot<-ggplot(e4embsum,aes(x=Temp.level,y=Mean.body,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.body-se.body,ymax=Mean.body+se.body),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("24","28"))+
  annotation_custom(grobTree(textGrob("Body (non-yolk) Ionocytes",x=0.1,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,500))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()

grid.arrange(e4yolkplot,e4bodyplot,ncol=2)

#problem might be imbalanced sample sizes? This could be another reason it is better to present the data the way we did in the resp paper. 
#In exp 3 the overall yolk sac densities at 17C are higher but when all of the data are combined, the lower CO2 levels get dragged down more because the
#sample size is greater (~30 as opposed to 10) - so there are more low values from Exp 1 to bring it down compared to 4200uatm, because 4200uatm wasn't even in Exp 1. 

#Make a plot for yolk and body like the ones in the resp paper, where every data point is printed but lines are fitted to show the interaction.
yolkplot2<-ggplot(d2_emb,aes(x=Temp,y=AverageYolkDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,750))+
  annotation_custom(grobTree(textGrob("Yolk Sac Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))
print(yolkplot2)
#lines are still affected by sample sizes/the fact that exp 1 didn't have 4200uatm 
#BUT regardless of the balance issue, there is still a different effect when you look at yolk vs body - just have to examine exp 3 and 1 separately. 

bodyplot2<-ggplot(d2_emb,aes(x=Temp,y=AverageBodyDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,750))+
  annotation_custom(grobTree(textGrob("Body (non-yolk) Ionocytes",x=0.1,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))
print(bodyplot2)
#actually the body one looks similar to the yolk one this way. 

grid.arrange(yolkplot2,bodyplot2,ncol=2)

#this also shows the positive skew of the data - try transforming. https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/


#Transforming embryo data
#square-root for moderate skew: sqrt(x)
#log10 for greater skew: log10(x) - try this first
#inverse for severe skew: 1/x
sqrtmodelyolk<-lm(sqrt(AverageYolkDensitymm)~CO2*Temp,data=d2_emb)
summary(sqrtmodelyolk)

sqrtmodelbody<-lm(sqrt(AverageBodyDensitymm)~CO2*Temp,data=d2_emb)
summary(sqrtmodelbody)

sqrtmodeltotal<-lm(sqrt(AverageTotalDensitymm)~CO2*Temp,data=d2_emb)
summary(sqrtmodeltotal)

#Yolk diagnostics - transformed
plot(sqrtmodelyolk,1) #Residuals vs. fitted
plot(sqrtmodelyolk,2) #Q-Q plot
yolkres<-residuals(sqrtmodelyolk)
hist(yolkres,breaks=20) #somewhat longer tail on right
shapiro.test(yolkres) #normality test - with log transformation now it is slight negative skew
 #Homogeneity of variances test
ols_test_normality(sqrtmodelbody)
ols_test_breusch_pagan(sqrtmodelbody)

cook<-cooks.distance(modelyolk)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_emb$AverageYolkDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageYolkDensitymm)-3-1),names(cook),""),col="red")
#I think I need to transform the data, there are a ton of 'outliers'

#Body diagnostics - transformed
plot(sqrtmodelbody,1) #Residuals vs. fitted
plot(sqrtmodelbody,2) #Q-Q plot
bodyres<-residuals(sqrtmodelbody)
hist(bodyres,breaks=20) #pretty symmetrical except for one observation with residual above 400. 
shapiro.test(bodyres) #normality test
 #Homogeneity of variances test

cook<-cooks.distance(modelbody)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_emb$AverageBodyDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageBodyDensitymm)-3-1),names(cook),""),col="red")
#similar to yolk; exp 3 and 4 have most 'influential' observations

#Total diagnostics
plot(lm(d2_emb$AverageTotalDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),1) #Residuals vs. fitted
plot(lm(d2_emb$AverageTotalDensitymm~d2_emb$CO2.level*d2_emb$Temp.level),2) #Q-Q plot
totalres<-residuals(modeltotal)
hist(totalres,breaks=20) #pretty symmetrical except for two observations with residuals around 400. 
shapiro.test(totalres) #normality test
leveneTest(modeltotal_lm) #Homogeneity of variances test

cook<-cooks.distance(modeltotal)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_emb$AverageTotalDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageTotalDensitymm)-3-1),names(cook),""),col="red")







