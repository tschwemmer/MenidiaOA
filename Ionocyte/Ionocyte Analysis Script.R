#Ionocyte Data Analysis
#January 21, 2022


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

#Run diagnostics and look for outliers
