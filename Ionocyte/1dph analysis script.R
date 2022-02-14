#1dph Larvae Analysis Script
#Teresa Schwemmer
#February 14, 2022

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
