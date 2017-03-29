#install the lme4, lsmeans and lmertest packages first
install.packages ("lme4")
install.packages ("lmerTest") #this uses Satterthwaite approximations to degrees of freedom for estimation of p-values
install.packages ("lsmeans")
install.packages ("pbkrtest") #needed for Kenward Roger approximations of bias corrections and degrees of freedom
library (lme4)
library (lmerTest)
library (lsmeans) 
library (pbkrtest)

#Read in First Pass Data
FPs <- read.csv("~/FPs.csv")

FPs$Meaning <- as.factor(FPs$Meaning)
FPs$Imposition <- as.factor(FPs$Imposition)

#this sets up the contrasts so that the intercept in the mixed LMM is the grand mean (i.e., the mean of all conditions)
my.coding <- matrix (c(.5, -.5))

contrasts (FPs$Meaning) <- my.coding  
contrasts (FPs$Imposition) <- my.coding 

#construct the models with crossed random effects for subjects and items for the pre-critical, critical and post-crtical regions of text on first pass times
fpmodelprec <- lmer (Probmanip ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning+Imposition |Item), data=FPs, REML=TRUE)
summary (fpmodelprec)
lsmeans (fpmodelprec, pairwise~Meaning*Imposition, adjust="none")

fpmodelc <- lmer (statement ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning*Imposition |Item), data=FPs, REML=TRUE)
summary (fpmodelc)
lsmeans (fpmodelc, pairwise~Meaning*Imposition, adjust="none")

fpmodelpostc <- lmer (response ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning+Imposition |Item), data=FPs, REML=TRUE)
summary (fpmodelpostc)
lsmeans (fpmodelpostc, pairwise~Meaning*Imposition, adjust="none")


#Read in Regression Path data
RPs <- read.csv("~/RPs.csv")

RPs$Meaning <- as.factor(RPs$Meaning)
RPs$Imposition <- as.factor(RPs$Imposition)

contrasts (RPs$Meaning) <- my.coding  
contrasts (RPs$Imposition) <- my.coding 

#construct the models with crossed random effects for subjects and items for the pre-critical, critical and post-crtical regions of text on regression path times
rpmodelprec <- lmer (Probmanip ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning*Imposition |Item), data=RPs, REML=TRUE)
summary (rpmodelprec)
lsmeans (rpmodelprec, pairwise~Meaning*Imposition, adjust="none")

rpmodelc <- lmer (statement ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning*Imposition |Item), data=RPs, REML=TRUE)
summary (rpmodelc)
lsmeans (rpmodelc, pairwise~Meaning*Imposition, adjust="none")

rpmodelpostc <- lmer (response ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning*Imposition |Item), data=RPs, REML=TRUE)
summary (rpmodelpostc)
lsmeans (rpmodelpostc, pairwise~Meaning*Imposition, adjust="none")

#Read in total reading time data
TTs <- read.csv("~/TTs.csv")

TTs$Meaning <- as.factor(TTs$Meaning)
TTs$Imposition <- as.factor(TTs$Imposition)

contrasts (TTs$Meaning) <- my.coding  
contrasts (TTs$Imposition) <- my.coding 

#construct the models with crossed random effects for subjects and items for the pre-critical, critical and post-crtical regions of text on total reading times
ttmodelprec <- lmer (Probmanip ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning+Imposition |Item), data=TTs, REML=TRUE)
summary (ttmodelprec)
lsmeans (ttmodelprec, pairwise~Meaning*Imposition, adjust="none")

ttmodelc <- lmer (statement ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning*Imposition |Item), data=TTs, REML=TRUE)
summary (ttmodelc)
lsmeans (ttmodelc, pairwise~Meaning*Imposition, adjust="none")

ttmodelpostc <- lmer (response ~ Meaning*Imposition + (1+Meaning*Imposition |P.s) + (1+Meaning*Imposition |Item), data=TTs, REML=TRUE)
summary (ttmodelpostc)
lsmeans (ttmodelpostc, pairwise~Meaning*Imposition, adjust="none")

#Read in regressions out data
ROs <- read.csv("~/ROs.csv")

ROs$Meaning <- as.factor(ROs$Meaning)
ROs$Imposition <- as.factor(ROs$Imposition)

contrasts (ROs$Meaning) <- my.coding  
contrasts (ROs$Imposition) <- my.coding 

#run separate F1 and F2 models as model with both random effects included fails to converge 
romodelprecf1 <- glmer(Probmanip ~ Meaning*Imposition + (1+Meaning |P.s) , data = ROs, family=binomial) 
summary (romodelprecf1)

romodelprecf2 <- glmer(Probmanip ~ Meaning*Imposition + (1+Meaning|Item), data = ROs, family=binomial) 
summary (romodelprecf2)

#run separate F1 and F2 models as model with both random effects included fails to converge 
romodelcf1 <- glmer(statement ~ Meaning*Imposition + (1+Meaning|P.s), data = ROs, family=binomial) 
summary (romodelcf1)

romodelcf2 <- glmer(statement ~ Meaning*Imposition + (1+Meaning |Item), data = ROs, family=binomial) 
summary (romodelcf2)

#run separate F1 and F2 models as model with both random effects included fails to converge 
romodelpostcf1 <- glmer(response ~ Meaning*Imposition + (1+Meaning |P.s) , data = ROs, family=binomial) 
summary (romodelpostcf1)
lsmeans (romodelpostcf1, pairwise~Meaning*Imposition, adjust="none")

romodelpostcf2 <- glmer(response ~ Meaning*Imposition + (1+Meaning|Item) , data = ROs, family=binomial) 
summary (romodelpostcf2)

#Read in regressions in data
RIs <- read_csv("~/RIs.csv")

RIs$Meaning <- as.factor(RIs$Meaning)
RIs$Imposition <- as.factor (RIs$Imposition)

contrasts (RIs$Meaning) <- my.coding  
contrasts (RIs$Imposition) <- my.coding 

#run separate F1 and F2 models as model with both random effects included fails to converge 

rimodelimpf1 <- glmer(Probmanip ~ Meaning*Imposition + (1+Meaning|P.s) , data = RIs, family=binomial) 
summary (rimodelimpf1)

rimodelimpf2 <- glmer(Probmanip ~ Meaning*Imposition + (1+Meaning+Imposition |Item) , data = RIs, family=binomial) 
summary (rimodelimpf2)
