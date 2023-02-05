#1dph Larvae Analysis Script
#Teresa Schwemmer
#February 2022

#First check structure of dataframe
str(d2_1dph)
d2_1dph$Experiment.x<-factor(d2_1dph$Experiment.x,levels=c("exp1","exp2","exp3","exp4"))
d2_1dph$CO2.level<-factor(d2_1dph$CO2.level,levels=c("400uatm","2200uatm","4200uatm"))
d2_1dph$Temp.level<-factor(d2_1dph$Temp.level,levels=c("17C","20C","24C","28C"))

#Use plyr to make a summary of the data for yolk, body, and total
library(plyr)
summary_1dph<-ddply(d2_1dph,c("CO2.level","Temp.level"),summarise,
                   N.front=length(AverageFrontDensitymm),Mean.front=mean(AverageFrontDensitymm),se.front=sd(AverageFrontDensitymm)/sqrt(N.front),
                   N.back=length(AverageBackDensitymm),Mean.back=mean(AverageBackDensitymm),se.back=sd(AverageBackDensitymm)/sqrt(N.back),
                   N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
summary_1dph
#Use a linear mixed effects model to test for significant effects for yolk, body, and total. 
#Set Experiment.x as a random effect and CO2.level and Temp.level as fixed effects.
library(lme4)
library(lmerTest)
modelfront<-lmer(AverageFrontDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_1dph)
anova(modelfront)

modelback<-lmer(AverageBackDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_1dph)
anova(modelback)

modeltotal1<-lmer(AverageTotalDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_1dph)
anova(modeltotal1)

#Test effect of including Experiment.x in model - create an lm without the random effect and do an anova to compare the two models. 
modelfront_lm<-lm(AverageFrontDensitymm~CO2.level*Temp.level,data=d2_1dph)
modelback_lm<-lm(AverageBackDensitymm~CO2.level*Temp.level,data=d2_1dph)
modeltotal1_lm<-lm(AverageTotalDensitymm~CO2.level*Temp.level,data=d2_1dph)

anova(modelfront,modelfront_lm)
anova(modelback,modelback_lm)
anova(modeltotal1,modeltotal1_lm)
#Including experiment as a random effect significantly affects all models. ranova() shows this as well. 

#Now use an lm and the continuous (not categorical) independent variables
#Use emmeans to examine significance
library(emmeans)
lmfront<-lm(AverageFrontDensitymm~CO2*Temp,data=d2_1dph)
summary(lmfront)

lmback<-lm(AverageBackDensitymm~CO2*Temp,data=d2_1dph)
summary(lmback)

lmtotal1<-lm(AverageTotalDensitymm~CO2*Temp,data=d2_1dph)
summary(lmtotal1)

#Try plotting CO2 with linear regression
plot(d2_1dph$AverageTotalDensitymm~d2_1dph$CO2)
co2mod1dph<-lm(AverageTotalDensitymm~CO2,data=d2_1dph)
abline(co2mod1dph)
summary(co2mod1dph)

#Run diagnostics and look for outliers
library(car)

#Front diagnostics
plot(lmfront,1) #Residuals vs. fitted
plot(lmfront,2) #Q-Q plot
frontres<-residuals(lmfront)
hist(frontres,breaks=20) #very symmetrical
shapiro.test(frontres) #normality test
ols_test_normality(lmfront)
ols_test_breusch_pagan(lmfront)
#passes all tests

#cook<-cooks.distance(modelyolk)
#plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
#abline(h=4/(length(d2_emb$AverageYolkDensitymm)-3-1), col="red")
#text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageYolkDensitymm)-3-1),names(cook),""),col="red")
#I think I need to transform the data, there are a ton of 'outliers'

#Back diagnostics
plot(lmback,1) #Residuals vs. fitted
plot(lmback,2) #Q-Q plot
backres<-residuals(lmback)
hist(backres,breaks=20) #pretty symmetrical except for a few observations on the right tail. 
shapiro.test(backres) #normality test
ols_test_normality(lmback)
ols_test_breusch_pagan(lmback)
#passes normality but fails homoskedasticity. 

cook<-cooks.distance(lmback)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_1dph$AverageBackDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_1dph$AverageBackDensitymm)-3-1),names(cook),""),col="red")
#Rows 335, 336, and 389 have the highest Cook's distance - removing them may fix the heteroskedasticity. 

#Total diagnostics
plot(lmtotal1,1) #Residuals vs. fitted
plot(lmtotal1,2) #Q-Q plot
total1res<-residuals(lmtotal1)
hist(total1res,breaks=20) #pretty symmetrical 
shapiro.test(total1res) #normality test
ols_test_normality(lmtotal1)
ols_test_breusch_pagan(lmtotal1)
#Passes both tests
#If I just use the total fish ionocyte density instead of breaking it up into front and back the assumptions are all good. 
#I also think there is more biological justification for singling out the yolk sac, compared to little biological
#justification (that I know of) for looking at the head/abdomen vs the trunk/tail. 

cook<-cooks.distance(lmtotal1)
plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
abline(h=4/(length(d2_1dph$AverageTotalDensitymm)-3-1), col="red")
text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_1dph$AverageTotalDensitymm)-3-1),names(cook),""),col="red")

sort(total1res) #the highest residual is line 365, which also has a high Cook's distance

d3_1dph<-d2_1dph[-c(365),]

#Try model and diagnostics again
lmtotal1<-lm(AverageTotalDensitymm~CO2*Temp,data=d3_1dph)
summary(lmtotal1)

total1res<-residuals(lmtotal1)
hist(total1res,breaks=20) #pretty symmetrical 
ols_test_normality(lmtotal1)
ols_test_breusch_pagan(lmtotal1)

#Use emmeans to test for significant differences between groups
library(emmeans)
emmeans(lmer(AverageTotalDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d3_1dph),list(pairwise~CO2.level*Temp.level),adjust="tukey")



#________________________________________________________________________________________________________
#make plots of means and SEs for yolk and body; then do separate ones for each experiment. 
library(ggplot2)
library(grid)
library(gridExtra)

summary_1dph<-ddply(d3_1dph,c("CO2.level","Temp.level"),summarise,
                    N.front=length(AverageFrontDensitymm),Mean.front=mean(AverageFrontDensitymm),se.front=sd(AverageFrontDensitymm)/sqrt(N.front),
                    N.back=length(AverageBackDensitymm),Mean.back=mean(AverageBackDensitymm),se.back=sd(AverageBackDensitymm)/sqrt(N.back),
                    N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
summary_1dph

frontplot<-ggplot(summary_1dph,aes(x=Temp.level,y=Mean.front,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.front-se.front,ymax=Mean.front+se.front),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24","28"))+
  scale_y_continuous(breaks=seq(0,600,100))+
  annotation_custom(grobTree(textGrob("A",x=0.06,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  labs(x=expression(paste("Temperature ("*degree,"C)")),y=expression(paste("Ionocyte Density (ionocytes mm"^"-2",")")))+
  theme_classic()+
  theme(legend.position="none")
print(frontplot)

backplot<-ggplot(summary_1dph,aes(x=Temp.level,y=Mean.back,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.back-se.back,ymax=Mean.back+se.back),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24","28"))+
  annotation_custom(grobTree(textGrob("Back (trunk) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(backplot)

total1plot<-ggplot(summary_1dph,aes(x=Temp.level,y=Mean.total,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.total-se.front,ymax=Mean.total+se.front),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24","28"))+
  scale_y_continuous(breaks=seq(0,600,100))+
  annotation_custom(grobTree(textGrob("A",x=0.06,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  labs(x=expression(paste("Temperature ("*degree,"C)")),y=expression(paste("Ionocyte Density (ionocytes mm"^"-2",")")))+
  theme_classic()+
  theme(legend.position="none")
print(total1plot)
ggsave(total1plot,file="larvae1dphmeans.pdf",width=100,height=100,units="mm",dpi=350)

#Make a plot for front, back, and total like the ones in the resp paper, where every data point is printed but lines are fitted to show the interaction.
frontplot2<-ggplot(d2_1dph,aes(x=Temp,y=AverageFrontDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,800),xlim=c(15,30))+
  annotation_custom(grobTree(textGrob("Front (head) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  theme(legend.position="none")
print(frontplot2)
#slope reverses for different CO2 levels: ionocytes decrease with temperature slightly at ambient CO2, 
#but increase with temperature at high CO2 levels. 

backplot2<-ggplot(d2_1dph,aes(x=Temp,y=AverageBackDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,800),xlim=c(15,30))+
  annotation_custom(grobTree(textGrob("Back (trunk) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  theme(legend.position="none")
print(backplot2)
#for back, the ionocyte density always increases with temperature but slope is highest at 4200 uatm CO2.

total1plot2<-ggplot(d3_1dph,aes(x=Temp,y=AverageTotalDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",linewidth=1.7,se=FALSE)+
  labs(x=expression(paste("Temperature ("*degree,"C)")),y=expression(paste("Ionocyte Density (ionocytes mm"^"-2",")")))+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,700),xlim=c(15,30))+
  annotation_custom(grobTree(textGrob("A",x=0.06,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  theme(legend.position="none")
print(total1plot2)
ggsave(total1plot2,file="lar1dphalldata.pdf",width=100,height=100,units="mm",dpi=350)
#400uatm is pretty flat, then slope increases with CO2. 





library(cowplot)
legend1dph<-get_legend(total1plot2)

grid.arrange(frontplot2,backplot2,total1plot2,legend1dph,ncol=4,widths=c(2,2,2,1))
grid.arrange(frontplot,backplot,total1plot,legend1dph,ncol=4,widths=c(2,2,2,1))

#Make plots separated out by experiment - differences may be due to ages at sampling
#First make summary tables for each experiment
library(plyr)
e11dphsum<-ddply(d2_1dph[d2_1dph$Experiment.x=="exp1",],c("CO2.level","Temp.level"),summarise,
                N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e11dphsum

e21dphsum<-ddply(d2_1dph[d2_1dph$Experiment.x=="exp2",],c("CO2.level","Temp.level"),summarise,
                 N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e21dphsum

e31dphsum<-ddply(d2_1dph[d2_1dph$Experiment.x=="exp3",],c("CO2.level","Temp.level"),summarise,
                 N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e31dphsum

e41dphsum<-ddply(d2_1dph[d2_1dph$Experiment.x=="exp4",],c("CO2.level","Temp.level"),summarise,
                 N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
e41dphsum

#Then make a plot for each experiment and display as one row
e11dphplot<-ggplot(e11dphsum,aes(x=Temp.level,y=Mean.total,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3"))+
  geom_errorbar(aes(ymin=Mean.total-se.total,ymax=Mean.total+se.total),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","24"))+
  annotation_custom(grobTree(textGrob("Exp 1, 1dph",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(e11dphplot)

e21dphplot<-ggplot(e21dphsum,aes(x=Temp.level,y=Mean.total,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.total-se.total,ymax=Mean.total+se.total),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Exp 2, 1dph",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(e21dphplot)

e31dphplot<-ggplot(e31dphsum,aes(x=Temp.level,y=Mean.total,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.total-se.total,ymax=Mean.total+se.total),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Exp 3, 1dph",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(e31dphplot)

e41dphplot<-ggplot(e41dphsum,aes(x=Temp.level,y=Mean.total,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.total-se.total,ymax=Mean.total+se.total),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("24","28"))+
  annotation_custom(grobTree(textGrob("Exp 4, 1dph",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,600))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(e41dphplot)

legend1dph<-get_legend(e31dphplot)
grid.arrange(e11dphplot,e21dphplot,e31dphplot,e41dphplot,legend1dph,ncol=5,widths=c(2,2,2,2,1))


