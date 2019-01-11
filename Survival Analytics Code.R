setwd("C:/Users/pc/Documents/R Project")

library(survminer)
library(rpart)
library(dplyr)
library(rattle)

pmain<-read.csv("P.Maintenance Project_dataset.csv")
summary(pmain) #No missing values

summary(pmain$broken)

pmain %>% filter(broken==1) %>% summarise(n()) #no. of machines broken is 39.47% of total (35522 out of 90000)

#Broken percentage grouped by team & provider
broken<-pmain %>% filter(broken==1) %>% group_by(team,provider) %>% 
  summarise(Broken=n(),BPerc=Broken/sum(pmain$broken)*100) %>% data.frame()

class(broken)

t<-pmain %>% group_by(team,provider) %>% 
  summarise(Total=n(),TotalBroken=Broken/sum(pmain$broken==1)) %>% data.frame()

broken<- cbind(broken,t[,c("Total")])

colnames(broken)<-c("team","provider","Broken","BPerc","Total")

broken$PercofTotal<-broken[,"Broken"]/broken[,"Total"]*100

library(survival)
library(ggplot2)

###Survival Analysis with respect to lifetime grouped with team & provider

##Kaplan-Mieier non-parametric analysis

kmsurvival<-survfit(Surv(pmain$lifetime,pmain$broken)~1)
summary(kmsurvival)$table #Summary shows survival probability

plot(kmsurvival, xlab="Lifetime", ylab="Survival Prob",col="blue", lwd=2,confint=TRUE)
abline(a=0.5,b=0)
title("Survival Probability over time")

#summary & plot shows that machines have broken on or after lifetime= 60, that means Pressure at 3 points
#and other variables don't have any effect on machine breakdown till lifetime=60

pmain %>% filter(broken==1) %>% summarise(mindu=min(lifetime))

##Survival Prob analysis grouped with team

kmsurvival1<-survfit(Surv(pmain$lifetime,pmain$broken)~pmain$team)
summary(kmsurvival1)$table #medain lifetime of machines maintained across is pretty much same (around 80 days)
kmsurvival1

plot(kmsurvival1, xlab="Lifetime", ylab="Survival Prob",col="blue", lwd=2,confint=TRUE)
abline(a=0.5,b=0)
title("Survival Probability over lifetime across teams")

#Log-rank test for comparing Survival Curves across Teams
#H0(Null-hypothesis)=survival prob is same across teams

surv_diff<-survdiff(Surv(lifetime,broken)~team,data=pmain)
surv_diff
#Since p=0 therefore we reject Null Hypothesis that survival probability is same across teams.

##Survival Probability analysis grouped with providers
kmsurvival2<-survfit(Surv(pmain$lifetime,pmain$broken)~pmain$provider)
summary(kmsurvival2)$table #medain lifetime of machines maintained across is pretty much same (around 80 days)
kmsurvival2

#median lifetime of machines manufactured by provider 3 has median lifetime =65 months whereas breakdown starts from 60 months only

#median lifetime of machines manufactured by provider 1 has median lifetime =92 months whereas breakdown starts from 60 months only
#and last breakdown happened at 93 months

plot(kmsurvival1, xlab="Lifetime", ylab="Survival Prob",col="blue", lwd=2,confint=TRUE)
abline(a=0.5,b=0)
title("Survival Probability over lifetime across Providers")

#Log-Rank test for comparing Survival-curves
#H0(Null-hypothesis)=survival prob is same across providers
surv_diff1<-survdiff(Surv(lifetime,broken)~provider,data=pmain)
surv_diff1
#Since p=0 therefore we reject Null Hypothesis that survival probability is same across providers.

###Survival Regression Model

###Predicting Expected remaining lifetime of a machine

###Creating Linear Regresison Model

linmodel<-lim(lifetime~.broken,data=pmain)
summary(linmodel)

##Creating Survival Object
s<-Surv(pmain$lifetime,pmain$broken)

#survreg command is used to create Parametric regression model for survival objects
survregmodel<-survreg(s~pressureInd_1+pressure_2+pressure_3+team+provider,dist="gaussian",data=pmain)
summary(survregmodel)

#Predicting Median Expected Lifetime of machines from survival regression model

expect<- prdeict(survreg,data=pmain,type="quantile",p=0.5)
forecast<-data.frame(expect)

#predciting remaining lifetime of machines

forecast$lifetime<-pmain$lifetime
forecast$broken<-pmain$broken
forecast$remainingLT<-forecast$expect-pmain$lifetime

pmain$remainingLT<-forecast$remainingLT

#Machines with remaining lifetime less than 20 months should be arranged at top of dataset so that preventive
#maintenance can be scheduled for them wihtout breakdown.

pmain %>% filter(broken==0 & remainingLT>=0 & remainingL<=20) %>%
  arrange(remainingLT)

pmain %>% filter(broken==0 & remainingLT>=0 & remainingL<=20) %>%
  group_by(team) %>% summarise(Total=n())
#TeamB has large amount of machines with remainingLt<=20 months

pmain %>% filter(broken==0 & remainingLT>=0 & remainingL<=20) %>%
  group_by(provider) %>% summarise(Total=n())
#provider3 has large amount of machines with remainingLt<=20 months

pmain %>% filter(broken==0 & remainingLT>=0 & remainingL<=20) %>%
  group_by(provider,team) %>% summarise(Total=n())

write.csv(pmain,file="Pexitics_Expected_RemainingLifetime.csv")

###SURVIVAL TREE

datapex2<-pmain

##Creating pressure windows for variables Pressure_Ind1,2,3
#creating deciles for Pressure1 levels by dividing in 20 quants

oj<-quantile(datapex2$pressureInd_1,p=(1:5)/5)
oj<-round(oj)
oj2<-as.list(oj)
oj2[20]

datapex2$Pressure1_dumm<-'0'

for(i in 1:length(oj2)-1)
{
  index<-which(oj2[i]<-datapex2$pressureInd_1 & datapex2$pressureInd_1<=oj2[i+1])
  datapex2$Pressure1_dumm[index]<- paste(oj2[i],"to",oj2[i+1])
  
}

index<-which(datapex2$Pressure1_dumm=='0')
datapex2$Pressure1_dumm[index]<-paste("less than",oj2[1])

#creating deciles for Pressure2 levels by dividing in 20 quants

oj<-quantile(datapex2$pressureInd_2,p=(1:5)/5)
oj<-round(oj)
oj2<-as.list(oj)
oj2[20]

datapex2$Pressure2_dumm<-'0'

for(i in 1:length(oj2)-1)
{
  index<-which(oj2[i]<-datapex2$pressureInd_2 & datapex2$pressureInd_2<=oj2[i+1])
  datapex2$Pressure2_dumm[index]<- paste(oj2[i],"to",oj2[i+1])
  
}

index<-which(datapex2$Pressure2_dumm=='0')
datapex2$Pressure2_dumm[index]<-paste("less than",oj2[1])

#creating deciles for Pressure3 levels by dividing in 20 quants

oj<-quantile(datapex2$pressureInd_3,p=(1:5)/5)
oj<-round(oj)
oj2<-as.list(oj)
oj2[20]

datapex2$Pressure3_dumm<-'0'

for(i in 1:length(oj2)-1)
{
  index<-which(oj2[i]<-datapex2$pressureInd_3 & datapex2$pressureInd_3<=oj2[i+1])
  datapex2$Pressure3_dumm[index]<- paste(oj2[i],"to",oj2[i+1])
  
}

index<-which(datapex2$Pressure3_dumm=='0')
datapex2$Pressure3_dumm[index]<-paste("less than",oj2[1])

##Creating Survival Tree using rpart with method="exp" (exponential)

#omitting pressure column and only considering windows of pressure to create Tree

stree<-rpart(Surv(lifetime,broke)~provider+team+Pressure1_dumm+Pressure2_dumm+Pressure3_dumm,
             data=dataepx2[,-c(1,4,5,6)],method='exp',control=rpart.control(cp=0.001,maxdepth=4))

stree

#Plotting Survival Tree

par(mar=rep(0.1,4))
plot(stree,uniform=TRUE,branch=1, compress=TRUE)
text(stree, n=TRUE, cex=0.5)

#Adding terminal to their respective data points in data sets

datapex2$TerminalNode<-stree$where

##Creating Survival Plot using Kaplan-Mieier Non Parametric Test between Survival Object and Terminal Node Variable

km<-survfit(formula=Surv(lifetime,broken)~datapex2$TerminalNode, data=datapex2[,-c(1,4,5,6)])
km
summary(km)

plot(km, xlab="Lifetime", ylab="Survival Rate",main="Survival Tree", lwd=2,confint=TRUE)
abline(a=0.5,b=0)