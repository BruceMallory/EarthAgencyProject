library(knitr)
library(tidyverse)
library(glmnet)
library(car)
library(ggplot2)
library(arm)
library(rstanarm)
library(magrittr)
library(brant)
library(plyr)
library(gplots)
library(gridExtra)

setwd("/Users/amelia/Documents/mssp/MA675/Consulting/Lizette")

adult <- read.csv("/Users/amelia/Documents/mssp/MA675/Consulting/Lizette/EarthAgency_Adults_R.csv", header = TRUE)

children <- read.csv("/Users/amelia/Documents/mssp/MA675/Consulting/Lizette/EarthAgency_Children_R.csv", header = TRUE)

adult_df <- adult %>%
  
  dplyr::select(
    
    Condition,
    
    Agency_Language,
    
    SRFactsTotal,
    
    invitalscore,
    
    inpsychscore,
    
    MeanSever,
    
    BioJtscore,
    
    AntJtscore,
    
    BioJFtotal,
    
    FirstLang,
    
    Age,Sever3_Air,Sever4_Trash,Sever1_Forest,Sever2_Water
    
  ) %>%
  
  mutate(
    
    Condition = as.factor(Condition),
    
    SRFactsTotal = as.integer(SRFactsTotal),
    
    invitalscore = as.integer(invitalscore),
    
    inpsychscore = as.integer(inpsychscore),
    
    Agency_Language = as.factor(Agency_Language),
    
    FirstLang = as.factor(FirstLang),
    
    Age,Sever3_Air,Sever4_Trash,Sever1_Forest,Sever2_Water
    
  )







#compress scores to match invitalscore and inpsychscore for children

adult_df$invitalscore[which(adult_df$invitalscore==0 | adult_df$invitalscore==1)]<-0

adult_df$invitalscore[which(adult_df$invitalscore==2) ]<-1

adult_df$invitalscore[which(adult_df$invitalscore==3) ]<-2

adult_df$invitalscore[which(adult_df$invitalscore==4 | adult_df$invitalscore==5)]<-3



adult_df$inpsychscore[which(adult_df$inpsychscore==2 | adult_df$inpsychscore==3)]<-2

adult_df$inpsychscore[which(adult_df$inpsychscore==4)]<-3

adult_df$inpsychscore[which(adult_df$inpsychscore==5)]<-4



child_df <- children %>%
  
  dplyr::select(
    
    Condition,
    
    Agency_Language,
    
    SRFactsTotal,
    
    invitalscore,
    
    inpsychscore,
    
    MeanSever,
    
    BioJtscore,
    
    AntJtscore,
    
    BioJFtotal,
    
    Age,Sever3_Air,Sever4_Trash,Sever1_Forest,Sever2_Water
    
  ) %>%
  
  mutate(
    
    Condition = as.factor(Condition),
    
    SRFactsTotal = as.integer(SRFactsTotal),
    
    invitalscore = as.integer(invitalscore),
    
    inpsychscore = as.integer(inpsychscore),
    
    Agency_Language = as.factor(Agency_Language),
    
    Age ,Sever3_Air,Sever4_Trash,Sever1_Forest,Sever2_Water
    
  )



#We're not sure we have the coding for FirstLang correct (1 = speaking their first language??)

FirstLang <- rep(1, nrow(child_df))

child_df <- child_df %>% 
  
  mutate(
    
    FirstLang = as.factor(FirstLang),
    
  )



#To take care of the one errant entry, children[87] (mean severity scores are divided by 4 so .67 is not possible…)

child_df$MeanSever[child_df$MeanSever==2.67] <- 2.5



#To remove children[41] which has no BIoJtscore or AntJtscore

#To remove children[45] which has no Agency_Language or SRFactsTotal

child_df <- na.omit(child_df)



## COMBINING children and adult into one data.frame

AC_df <- rbind(adult_df, child_df)



#Giving factor levels cleaner language

AC_df$Condition <- revalue(AC_df$Condition, c("1"="Obj", "2"="Nat", "3"="Per"))

AC_df$Agency_Language <- revalue(AC_df$Agency_Language, c("0"="Obj", "2"="Nat", "3"="Per"))

AC_df$Age <- ifelse(AC_df$Age > 10, "Adult", "Child")

AC_df$Age <- factor(AC_df$Age)

AC_df$air<-(AC_df$Sever3_Air+AC_df$Sever4_Trash)/2
AC_df$animals<-(AC_df$Sever2_Water+AC_df$Sever1_Forest)/2
AC_df$air<-as.factor(AC_df$air)
AC_df$animals<-as.factor(AC_df$animals)
#To check for correlation among independent variables
AC_indep <- AC_df %>%
  dplyr::select(
    Condition,
    Agency_Language,
    SRFactsTotal,
    invitalscore,
    inpsychscore,
    FirstLang,
    Age)


#note the strong correlation between Condition and Agency_Language

## 1. Description
# Our Client, Lizette Pizza Becerra, is a student from the Psychology 
# department at Boston University working under Advisor Deborah Kelemen. 
# Her research involves various tasks, but her question for us aims to 
# explore the impact of the attributions of agency to the Earth on moral 
# judgements about the harm to nature. The attributions of agency refer 
# to perception of the Earth. The perception can be either vitalist, 
# viewing the earth like an animal that needs water, or psychological, 
# viewing the earth like a person that has feelings. In particular, she 
# is interested in finding which predictor affects the outcome, is there 
# significant difference between different groups, and the power analysis.

# ## 2. Experimental Design (and correlation of predictor variables)
# Because of our worry about the independence of the independent variables,
# we checked the correlations between the independent variables. The 
# following chart (which is redundant across the diagonal), shows Pearson 
# product-moment correlations between the numeric variables (SRFactsTotal, 
# invitalscore, inpsychscore), a polychoric correlations between ordinal 
# categorical variables (Condition, Agency_Language, FirstLang and Age), 
# and polyserial correlations between numeric and ordinal variables. 1 or 
# -1 is a strong correlation. 0 is no correlation.

AC_indep <- AC_df %>%
  dplyr::select(
    Condition,
    Agency_Language,
    SRFactsTotal,
    invitalscore,
    inpsychscore,
    postqs,
    FirstLang,
    Age)

cor.check <- polycor::hetcor(AC_indep)

kable(round(cor.check$correlations,2))

# ###  Correlation between `Condition` and `Agency_language`  
# `Condition` and `Agency_language` had the strongest correlation and 
# the p-value of the Pearson's chi-square test was effectively zero 
# ($p=7.3*10^{-36}$).  Because of this, and because of our concerns 
# about the experimental design, we have not included `Agency_language` 
# in the model fits below.  If we have time, we will explore using PCA 
# (principal component analysis) to combine these two independent 
# variables.  

pvalue <- chisq.test(table(AC_df$Condition, AC_df$Agency_Language), simulate.p.value = FALSE)$p.value
balloonplot(table(AC_df$Condition, AC_df$Agency_Language), xlab ="Condition", ylab="Agency_Lang",
            label = TRUE, show.margins = TRUE, label.size=.5)


# ### Correlation between `Condition`, `invitalscore`, and `inpsychscore`  
# A Chi-square test shows that there is not a significant correlation 
# between `Condition` and `invitalscore` (p=0.54), and there is a slightly 
# significant correlation between `Condition` and `inpsychscore` (p=0.04).  
# But the correlation between `invitalscore` and `inpsychscore` is highly 
# significant ($p=5.0*10^{-4}$).  We have included both of these variables 
# in the model fit below, and (if time permits) we are going to explore 
# using PCA (principal component analysis) to combine these two independent 
# variables (`invitalscore` and `inpsychsocre`).   

pvalue.1 <- chisq.test(table(AC_df$Condition, AC_df$invitalscore), simulate.p.value = FALSE)$p.value
balloonplot(table(AC_df$Condition, AC_df$invitalscore), xlab ="Condition", ylab="invitalscore",
            label = TRUE, show.margins = TRUE, label.size=.5)

pvalue.2 <- chisq.test(table(AC_df$Condition, AC_df$inpsychscore), simulate.p.value = FALSE)$p.value
balloonplot(table(AC_df$Condition, AC_df$inpsychscore), xlab ="Condition", ylab="inpsychscore",
            label = TRUE, show.margins = TRUE, label.size=.5)

pvalue.3 <- chisq.test(table(AC_df$inpsychscore, AC_df$invitalscore), simulate.p.value = TRUE)$p.value
balloonplot(table(AC_df$invitalscore, AC_df$inpsychscore), xlab ="invitalscore", ylab="inpsychscore",
            label = TRUE, show.margins = TRUE, label.size=.5)


# ### Correlation between `FirstLang` and `Age`  
# Because we are assuming that the 89 children in the study are all using 
# their first language, there is a strong correlation between `FirstLang` 
# and `Age` ($p=2.4*10^{-6}$).  But, for contextual reasons, we have left 
# both of these variables in the models below.  And when we ran the model 
# without `FirstLang` it did not improve the model fit.

pvalue.4 <- chisq.test(table(AC_df$FirstLang, AC_df$Age), simulate.p.value = FALSE)$p.value
balloonplot(table(AC_df$FirstLang, AC_df$Age), xlab ="First Language", ylab="Age",
            label = TRUE, show.margins = TRUE, label.size=.5)

##4. Modeling
### 4.11 Mean Severity Ordinal model
#Meansever
AC_df$MeanSever <- factor(AC_df$MeanSever, ordered = TRUE)

plor.fit1 <- polr(MeanSever ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit1)

#Get p-value
ctable <- round(coef(summary(plor.fit1)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit1)
n <- table(AC_df$MeanSever, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]), n[4,4]/sum(n[4,]), n[5,5]/sum(n[5,]), n[6,6]/sum(n[6,]), n[7,7]/sum(n[7,]), n[8,8]/sum(n[8,]), n[9,9]/sum(n[9,]), n[10,10]/sum(n[10,]), n[11,11]/sum(n[11,]), n[12,12]/sum(n[12,]), n[13,13]/sum(n[13,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$MeanSever) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

###4.12 Mean Severity Ordinal model with combined levels
#Meansever combined into three levels (bcm)
AC_df$comMeanSever <- AC_df$MeanSever
AC_df$comMeanSever[which((AC_df$MeanSever < 1.5))]<-1
AC_df$comMeanSever[which((AC_df$MeanSever > 1.25 & AC_df$MeanSever < 2.25))]<-2
AC_df$comMeanSever[which((AC_df$MeanSever > 2))]<-3
AC_df$comMeanSever <- factor(AC_df$comMeanSever, ordered = TRUE)

table(AC_df$MeanSever)
table(AC_df$comMeanSever)

plor.fit1c <- polr(comMeanSever ~  Condition  + SRFactsTotal + invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit1c)

#Get p-value
ctable <- round(coef(summary(plor.fit1c)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" =round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit1c)
n <- table(AC_df$comMeanSever, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$comMeanSever) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

###4.21 Biocentric Justification score, Ordinal model
#BioJtscore
AC_df$BioJtscore <- factor(AC_df$BioJtscore, levels = c("0","1","2","3","4"),ordered = TRUE)

plor.fit2 <- polr(BioJtscore ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit2)

#Get p-value
ctable <- round(coef(summary(plor.fit2)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit2)
n <- table(AC_df$BioJtscore, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]), n[4,4]/sum(n[4,]), n[5,5]/sum(n[5,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$BioJtscore) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

###4.22 Biocentric Justification score, Ordinal model with combined levels
#BioJtscore with Combination 
AC_df$comBioJtscore<-AC_df$BioJtscore
AC_df$comBioJtscore[which((AC_df$comBioJtscore==0))]<-1
AC_df$comBioJtscore[which((AC_df$comBioJtscore==2))]<-1
AC_df$comBioJtscore[which((AC_df$comBioJtscore==3))]<-2
AC_df$comBioJtscore[which((AC_df$comBioJtscore==4))]<-3
AC_df$comBioJtscore<- factor(AC_df$comBioJtscore, ordered = TRUE)

table(AC_df$BioJtscore)
table(AC_df$comBioJtscore)

plor.fit2c <- polr(comBioJtscore ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit2c)

#Get p-value
ctable <- round(coef(summary(plor.fit2c)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit2c)
n <- table(AC_df$comBioJtscore, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$comBioJtscore) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

###4.31 Anthropocentric Justification score, Ordinal model
#AntJtscore
AC_df$AntJtscore <- factor(AC_df$AntJtscore, levels = c("0","1","2","3","4"),ordered = TRUE)

plor.fit3<- polr(AntJtscore~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit3)

#Get p-value
ctable <- round(coef(summary(plor.fit3)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit3)
n <- table(AC_df$AntJtscore, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]), n[4,4]/sum(n[4,]), n[5,5]/sum(n[5,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$AntJtscore) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

###4.32 Anthropocentric Justification score, Ordinal model with combined levels
#AntJtscore with Combination 
AC_df$comAntJtscore<-AC_df$AntJtscore
AC_df$comAntJtscore[which((AC_df$comAntJtscore==0))]<-1
AC_df$comAntJtscore[which((AC_df$comAntJtscore==2))]<-1
AC_df$comAntJtscore[which((AC_df$comAntJtscore==3))]<-2
AC_df$comAntJtscore[which((AC_df$comAntJtscore==4))]<-3
AC_df$comAntJtscore<- factor(AC_df$comAntJtscore, ordered = TRUE)

table(AC_df$AntJtscore)
table(AC_df$comAntJtscore)

plor.fit3c<- polr(comAntJtscore~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit3c)

#Get p-value
ctable <- round(coef(summary(plor.fit3c)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit3c)
n <- table(AC_df$comAntJtscore, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$comAntJtscore) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

### 4.41 Biocentric choice score, Ordinal model
#BioJFtotal
AC_df$BioJFtotal <- factor(AC_df$BioJFtotal, levels = c("0","1","2","3","4"),ordered = TRUE)

plor.fit4 <- polr(BioJFtotal ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit4)

#Get p-value
ctable <- round(coef(summary(plor.fit4)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit4)
n <- table(AC_df$BioJFtotal, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]), n[4,4]/sum(n[4,]), n[5,5]/sum(n[5,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$BioJFtotal) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")

### 4.42 Biocentric choice score, Ordinal model with combined levels 
#BioJFtotal with Combination
AC_df$comBioJFtotal<-AC_df$BioJFtotal
AC_df$comBioJFtotal[which((AC_df$comBioJFtotal==0))]<-1
#AC_df$comBioJFtotal[which((AC_df$comBioJFtotal==2))]<-1
#AC_df$comBioJFtotal[which((AC_df$comBioJFtotal==3))]<-2
AC_df$comBioJFtotal[which((AC_df$comBioJFtotal==4))]<-3
AC_df$comBioJFtotal<- factor(AC_df$comBioJFtotal, ordered = TRUE)

table(AC_df$BioJFtotal)
table(AC_df$comBioJFtotal)

plor.fit4c <- polr(comBioJFtotal ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
#summary(plor.fit4c)

#Get p-value
ctable <- round(coef(summary(plor.fit4c)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1

#Compute confusion table and misclassification error
fitted <- predict(plor.fit4c)
n <- table(AC_df$comBioJFtotal, fitted)
prop.correct <- c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]))
prop.correct <- round(100*prop.correct, 0)
rbind(n,prop.correct)
ME <- mean(as.character(AC_df$comBioJFtotal) !=as.character(fitted))
cat("Misclassification error is:",ME,"\n")


## 5.Effects
### 5.1Age
#Mean Severity
plor.fit1c2 <- polr(comMeanSever ~  Condition  + SRFactsTotal + invitalscore  + inpsychscore + FirstLang, data = AC_df)

anova(plor.fit1c, plor.fit1c2)


#Biocentric Justification score
plor.fit2c2 <- polr(comBioJtscore ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang, data = AC_df)

anova(plor.fit2c, plor.fit2c2)


#Anthropocentric Justification score
plor.fit3c2<- polr(comAntJtscore~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang, data = AC_df)

anova(plor.fit3c,plor.fit3c2)


#Biocentric choice score
plor.fit4c2 <- polr(comBioJFtotal ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang, data = AC_df)

anova(plor.fit4c,plor.fit4c2)

#Air
plor.fit5c <- polr(air ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)

plor.fit5c2 <- polr(air ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang, data = AC_df)
anova(plor.fit5c,plor.fit5c2)

#Animals
plor.fit6c <- polr(animals ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)

plor.fit6c2 <- polr(animals ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang, data = AC_df)
anova(plor.fit6c,plor.fit6c2)

### 5.2 OrderS(children)
children1 <- children %>%
  dplyr::select(Condition,Agency_Language,SRFactsTotal,SRTotal,invitalscore,
                inpsychscore,inagencyscore,Location,Gender,Order,
                MeanSever,BioJtscore,AntJtscore,BioJFtotal,Sever3_Air,Sever4_Trash,Sever1_Forest,Sever2_Water) %>%
  mutate(Condition = factor(Condition))
children1$OrderS <- substring(children1$Order, 1, 2)
children1 <- na.omit(children1)
children1 <- children1 %>%
  mutate(SRFactsTotal=as.integer(SRFactsTotal),inagencyscore=as.integer(inagencyscore),invitalscore=as.integer(invitalscore),inpsychscore=as.integer(inpsychscore),Gender=as.factor(Gender),Location=as.factor(Location),Agency_Language=as.factor(Agency_Language),OrderS=as.factor(OrderS),Sever3_Air,Sever4_Trash,Sever1_Forest,Sever2_Water)

#Meansever combined into three levels
children1$comMeanSever <- children1$MeanSever
children1$comMeanSever[which((children1$MeanSever < 1.5))]<-1
children1$comMeanSever[which((children1$MeanSever > 1.25 & children1$MeanSever < 2.25))]<-2
children1$comMeanSever[which((children1$MeanSever > 2))]<-3
children1$comMeanSever <- factor(children1$comMeanSever, ordered = TRUE)

#BioJtscore with Combination
children1$comBioJtscore<-children1$BioJtscore
children1$comBioJtscore[which((children1$comBioJtscore==0))]<-1
children1$comBioJtscore[which((children1$comBioJtscore==2))]<-1
children1$comBioJtscore[which((children1$comBioJtscore==3))]<-2
children1$comBioJtscore[which((children1$comBioJtscore==4))]<-3
children1$comBioJtscore<- factor(children1$comBioJtscore, ordered = TRUE)

#AntJtscore with Combination (bcm)
children1$comAntJtscore<-children1$AntJtscore
children1$comAntJtscore[which((children1$comAntJtscore==0))]<-1
children1$comAntJtscore[which((children1$comAntJtscore==2))]<-1
children1$comAntJtscore[which((children1$comAntJtscore==3))]<-2
children1$comAntJtscore[which((children1$comAntJtscore==4))]<-3
children1$comAntJtscore<- factor(children1$comAntJtscore, ordered = TRUE)

#BioJFtotal with Combination
children1$comBioJFtotal<-children1$BioJFtotal
children1$comBioJFtotal[which((children1$comBioJFtotal==0))]<-1
children1$comBioJFtotal[which((children1$comBioJFtotal==2))]<-1
children1$comBioJFtotal[which((children1$comBioJFtotal==3))]<-2
children1$comBioJFtotal[which((children1$comBioJFtotal==4))]<-3
children1$comBioJFtotal<- factor(children1$comBioJFtotal, ordered = TRUE)
children1$air<-(children1$Sever3_Air+children1$Sever4_Trash)/2
children1$animals<-(children1$Sever2_Water+children1$Sever1_Forest)/2
children1$air<-as.factor(children1$air)
children1$animals<-as.factor(children1$animals)

#Mean Severity
plor.fit1c1 <- polr(comMeanSever ~  Condition + SRFactsTotal + invitalscore  + inpsychscore +  OrderS, data = children1)

plor.fit1c2 <- polr(comMeanSever ~  Condition +  SRFactsTotal + invitalscore  + inpsychscore , data = children1)

anova(plor.fit1c1, plor.fit1c2)


#Biocentric Justification score
plor.fit2c1 <- polr(comBioJtscore ~  Condition + SRFactsTotal +invitalscore  + inpsychscore +OrderS, data = children1)

plor.fit2c2 <- polr(comBioJtscore ~  Condition +  SRFactsTotal +invitalscore  + inpsychscore , data = children1)

anova(plor.fit2c1, plor.fit2c2)


#Anthropocentric Justification score
plor.fit3c1<- polr(comAntJtscore~  Condition + SRFactsTotal +invitalscore  + inpsychscore +OrderS, data = children1)

plor.fit3c2<- polr(comAntJtscore~  Condition +  SRFactsTotal +invitalscore  + inpsychscore , data = children1)

anova(plor.fit3c1,plor.fit3c2)


#Biocentric choice score
plor.fit4c1 <- polr(comBioJFtotal ~  Condition + SRFactsTotal +invitalscore  + inpsychscore +OrderS, data = children1)

plor.fit4c2 <- polr(comBioJFtotal ~  Condition +  SRFactsTotal +invitalscore  + inpsychscore , data = children1)

anova(plor.fit4c1,plor.fit4c2)

#Air
plor.fit5c1 <- polr(air ~  Condition + Agency_Language + SRFactsTotal +invitalscore  + inpsychscore+OrderS , data = children1)

plor.fit5c2 <- polr(air ~   Condition+Agency_Language + SRFactsTotal +invitalscore  + inpsychscore , data = children1)
anova(plor.fit5c1,plor.fit5c2)

#Animals
plor.fit6c1 <- polr(animals ~  Condition + Agency_Language + SRFactsTotal +invitalscore  + inpsychscore+OrderS , data = children1)

plor.fit6c2 <- polr(animals ~   Condition+Agency_Language + SRFactsTotal +invitalscore  + inpsychscore , data = children1)
anova(plor.fit6c1,plor.fit6c2)

## 5.3 Condition
plor.fit1c3 <- polr(comMeanSever ~  SRFactsTotal + invitalscore  + inpsychscore + FirstLang + Age, data = AC_df)

anova(plor.fit1c, plor.fit1c3)


#Biocentric Justification score
plor.fit2c3 <- polr(comBioJtscore ~   SRFactsTotal +invitalscore  + inpsychscore + FirstLang + Age, data = AC_df)

anova(plor.fit2c, plor.fit2c3)


#Anthropocentric Justification score
plor.fit3c3<- polr(comAntJtscore~   SRFactsTotal +invitalscore  + inpsychscore + FirstLang + Age, data = AC_df)

anova(plor.fit3c,plor.fit3c3)


#Biocentric choice score
plor.fit4c3 <- polr(comBioJFtotal ~   SRFactsTotal +invitalscore  + inpsychscore + FirstLang + Age, data = AC_df)

anova(plor.fit4c,plor.fit4c3)

#Air
plor.fit5c <- polr(air ~   SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)

plor.fit5c2 <- polr(air ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
anova(plor.fit5c,plor.fit5c2)

#Animals
plor.fit6c <- polr(animals ~   SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)

plor.fit6c2 <- polr(animals ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = AC_df)
anova(plor.fit6c,plor.fit6c2)

### 5.4 Air&Animals
air_df<-AC_df%>%dplyr::select(  Condition,
                                
                                Agency_Language,
                                
                                SRFactsTotal,
                                
                                invitalscore,
                                
                                inpsychscore,
                                
                                MeanSever,
                                
                                BioJtscore,
                                
                                AntJtscore,
                                
                                BioJFtotal,
                                
                                FirstLang,
                                
                                Age,air)
air_df$type<-c("air")
colnames(air_df)[12]='situation'
animal_df<-AC_df%>%dplyr::select(  Condition,
                                   
                                   Agency_Language,
                                   
                                   SRFactsTotal,
                                   
                                   invitalscore,
                                   
                                   inpsychscore,
                                   
                                   MeanSever,
                                   
                                   BioJtscore,
                                   
                                   AntJtscore,
                                   
                                   BioJFtotal,
                                   
                                   FirstLang,
                                   
                                   Age,animals)
animal_df$type<-c("animals")
colnames(animal_df)[12]='situation'
type_df <- rbind(animal_df, air_df)
plor.fit7 <- polr(situation ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age, data = type_df)
plor.fit8 <- polr(situation ~  Condition  + SRFactsTotal +invitalscore  + inpsychscore + FirstLang+Age+type, data = type_df)
anova(plor.fit7,plor.fit8)
summary(plor.fit8)
#Get p-value
ctable <- round(coef(summary(plor.fit8)), 3)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable, "p value" = round(p, 3))
ctable1














