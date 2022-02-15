#10mm larvae analysis
#Teresa Schwemmer
#February 2022

#First check structure of dataframe
str(d2_10mm)
d2_10mm$Experiment.x<-factor(d2_10mm$Experiment.x,levels=c("exp2","exp3"))
d2_10mm$CO2.level<-factor(d2_10mm$CO2.level,levels=c("400uatm","2200uatm","4200uatm"))
d2_10mm$Temp.level<-factor(d2_10mm$Temp.level,levels=c("17C","20C","24C"))

#Use plyr to make a summary of the data for yolk, body, and total
library(plyr)
summary_10mm<-ddply(d2_10mm,c("CO2.level","Temp.level"),summarise,
                    N.front=length(AverageFrontDensitymm),Mean.front=mean(AverageFrontDensitymm),se.front=sd(AverageFrontDensitymm)/sqrt(N.front),
                    N.back=length(AverageBackDensitymm),Mean.back=mean(AverageBackDensitymm),se.back=sd(AverageBackDensitymm)/sqrt(N.back),
                    N.total=length(AverageTotalDensitymm),Mean.total=mean(AverageTotalDensitymm),se.total=sd(AverageTotalDensitymm)/sqrt(N.total))
summary_10mm
#Use a linear mixed effects model to test for significant effects for yolk, body, and total. 
#Set Experiment.x as a random effect and CO2.level and Temp.level as fixed effects.
library(lme4)
library(lmerTest)
model10front<-lmer(AverageFrontDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_10mm)
anova(model10front)

model10back<-lmer(AverageBackDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_10mm) #SINGULAR FIT
anova(model10back)

modeltotal10<-lmer(AverageTotalDensitymm~CO2.level*Temp.level+(1|Experiment.x),data=d2_10mm)
anova(modeltotal10)

#Test effect of including Experiment.x in model - create an lm without the random effect and do an anova to compare the two models. 
model10front_lm<-lm(AverageFrontDensitymm~CO2.level*Temp.level,data=d2_10mm)
model10back_lm<-lm(AverageBackDensitymm~CO2.level*Temp.level,data=d2_10mm)
modeltotal10_lm<-lm(AverageTotalDensitymm~CO2.level*Temp.level,data=d2_10mm)

anova(model10front,model10front_lm)
anova(model10back,model10back_lm)
anova(modeltotal10,modeltotal10_lm)
#Including experiment as a random effect doesn't significantly affect the models. ranova() shows this as well. 

#Now use an lm and the continuous (not categorical) independent variables
#Use emmeans to examine significance
library(emmeans)
lmfront10<-lm(AverageFrontDensitymm~CO2*Temp,data=d2_10mm)
summary(lmfront10)

lmback10<-lm(AverageBackDensitymm~CO2*Temp,data=d2_10mm)
summary(lmback10)

lmtotal10<-lm(AverageTotalDensitymm~CO2*Temp,data=d2_10mm)
summary(lmtotal10)

#Run diagnostics and look for outliers
library(car)

#Front diagnostics
plot(lmfront10,1) #Residuals vs. fitted
plot(lmfront10,2) #Q-Q plot
front10res<-residuals(lmfront10)
hist(front10res,breaks=20) #very symmetrical
shapiro.test(front10res) #normality test
ols_test_normality(lmfront10)
ols_test_breusch_pagan(lmfront10)
#passes all tests

#cook<-cooks.distance(modelyolk)
#plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
#abline(h=4/(length(d2_emb$AverageYolkDensitymm)-3-1), col="red")
#text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageYolkDensitymm)-3-1),names(cook),""),col="red")
#I think I need to transform the data, there are a ton of 'outliers'

#Back diagnostics
plot(lmback10,1) #Residuals vs. fitted
plot(lmback10,2) #Q-Q plot
back10res<-residuals(lmback10)
hist(back10res,breaks=20) #pretty symmetrical except for a few observations on the right tail. 
shapiro.test(back10res) #normality test
ols_test_normality(lmback10)
ols_test_breusch_pagan(lmback10)
#passes all tests 

#cook<-cooks.distance(lmback)
#plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
#abline(h=4/(length(d2_1dph$AverageBackDensitymm)-3-1), col="red")
#text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_1dph$AverageBackDensitymm)-3-1),names(cook),""),col="red")
#Rows 335, 336, and 389 have the highest Cook's distance - removing them may fix the heteroskedasticity. 

#Total diagnostics
plot(lmtotal10,1) #Residuals vs. fitted
plot(lmtotal10,2) #Q-Q plot
total10res<-residuals(lmtotal10)
hist(total10res,breaks=20) #pretty symmetrical 
shapiro.test(total10res) #normality test
ols_test_normality(lmtotal10)
ols_test_breusch_pagan(lmtotal10)
#Passes both tests
#If I just use the total fish ionocyte density instead of breaking it up into front and back the assumptions are all good. 
#I also think there is more biological justification for singling out the yolk sac, compared to little biological
#justification (that I know of) for looking at the head/abdomen vs the trunk/tail. 

#cook<-cooks.distance(modeltotal)
#plot(cook,pch="*",cex=2,main="Influential Obs by Cooks Distance") 
#abline(h=4/(length(d2_emb$AverageTotalDensitymm)-3-1), col="red")
#text(x=1:length(cook)+10,y=cook,labels=ifelse(cook>4/(length(d2_emb$AverageTotalDensitymm)-3-1),names(cook),""),col="red")


#make plots of means and SEs for yolk and body; then do separate ones for each experiment. 
library(ggplot2)
library(grid)
library(gridExtra)

front10plot<-ggplot(summary_10mm,aes(x=Temp.level,y=Mean.front,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.front-se.front,ymax=Mean.front+se.front),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Front (head) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,400))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(front10plot)

back10plot<-ggplot(summary_10mm,aes(x=Temp.level,y=Mean.back,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.back-se.back,ymax=Mean.back+se.back),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Back (trunk) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,400))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(back10plot)

total10plot<-ggplot(summary_10mm,aes(x=Temp.level,y=Mean.total,group=CO2.level,color=CO2.level))+
  scale_color_manual(values=c("skyblue","steelblue3","steelblue4"))+
  geom_errorbar(aes(ymin=Mean.total-se.total,ymax=Mean.total+se.total),width=0.2,position=position_dodge(0.1))+
  geom_point(size=3,position=position_dodge(0.1),shape=16)+
  geom_line(position=position_dodge(0.1),linetype="dashed",show.legend=FALSE)+
  scale_x_discrete(labels=c("17","20","24"))+
  annotation_custom(grobTree(textGrob("Total Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=17,fontface="bold"))))+
  coord_cartesian(ylim=c(0,400))+
  xlab("Temperature (C)")+
  ylab("Ionocyte Density (ionocytes/mm^2)")+
  theme_classic()+
  theme(legend.position="none")
print(total10plot)

#Make a plot for front, back, and total like the ones in the resp paper, where every data point is printed but lines are fitted to show the interaction.
front10plot2<-ggplot(d2_10mm,aes(x=Temp,y=AverageFrontDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,500),xlim=c(16,26))+
  annotation_custom(grobTree(textGrob("Front (head) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  theme(legend.position="none")
print(front10plot2)
#slope reverses for different CO2 levels: ionocytes decrease with temperature slightly at ambient CO2, 
#but increase with temperature at high CO2 levels. 

back10plot2<-ggplot(d2_10mm,aes(x=Temp,y=AverageBackDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,500),xlim=c(16,26))+
  annotation_custom(grobTree(textGrob("Back (trunk) Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  theme(legend.position="none")
print(back10plot2)
#for back, the ionocyte density always increases with temperature but slope is highest at 4200 uatm CO2.

total10plot2<-ggplot(d2_10mm,aes(x=Temp,y=AverageTotalDensitymm,colour=CO2.level,shape=CO2.level))+
  theme_classic()+
  geom_point(size=1.3,alpha=0.4)+
  geom_smooth(method="lm",lwd=1.7,se=FALSE)+
  labs(x="Temperature (C)",y="Ionocyte Density (ionocytes/mm^2)")+
  scale_shape_manual(values=c(16,16,16))+
  scale_colour_manual(values=c("skyblue","steelblue3","steelblue4"))+
  coord_cartesian(ylim=c(0,500),xlim=c(16,26))+
  annotation_custom(grobTree(textGrob("Total Ionocytes",x=0.2,y=0.95,hjust=0,gp=gpar(col="black",fontsize=15,fontface="bold"))))+
  theme(legend.position="none")
print(total10plot2)
#400uatm is pretty flat, then slope increases with CO2. 

library(cowplot)
legend1dph<-get_legend(total1plot2) #can use same legend for all figures because same 3 CO2 levels for all. 

grid.arrange(front10plot2,back10plot2,total10plot2,legend1dph,ncol=4,widths=c(2,2,2,1))
grid.arrange(front10plot,back10plot,total10plot,legend1dph,ncol=4,widths=c(2,2,2,1))
