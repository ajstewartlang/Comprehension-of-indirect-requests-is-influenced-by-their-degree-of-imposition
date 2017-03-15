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
FPs <- read.csv("~/Desktop/Air Work/R analyses/Indirect Request Expt/Experiment 1 - probability of success - Libby's data/FPs.csv")

#this sets up the contrasts so that the intercept in the mixed LMM is the grand mean (i.e., the mean of all conditions)
my.coding <- matrix (c(.5, -.5))
contrasts (FPs$Meaning)<-my.coding  
contrasts (FPs$Probability)<-my.coding 

#construct the models with crossed random effects for subjects and items for the pre-critical, critical and post-crtical regions of text on first pass times
fpmodelprec <- lmer (Speaker ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning+Probability |Item), data=FPs, REML=TRUE)
summary (fpmodelprec)
lsmeans (fpmodelprec, pairwise~Meaning*Probability, adjust="none")

fpmodelc <- lmer (statement ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning*Probability |Item), data=FPs, REML=TRUE)
summary (fpmodelc)
lsmeans (fpmodelc, pairwise~Meaning*Probability, adjust="none")

fpmodelpostc <- lmer (response ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning+Probability |Item), data=FPs, REML=TRUE)
summary (fpmodelpostc)
lsmeans (fpmodelpostc, pairwise~Meaning*Probability, adjust="none")


#Read in Regression Path data
RPs <- read.csv("~/Desktop/Air Work/R analyses/Indirect Request Expt/Experiment 1 - probability of success - Libby's data/RPs.csv")

contrasts (RPs$Meaning)<-my.coding  
contrasts (RPs$Probability)<-my.coding 

#construct the models with crossed random effects for subjects and items for the pre-critical, critical and post-crtical regions of text on regression path times
rpmodelprec <- lmer (Speaker ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning*Probability |Item), data=RPs, REML=TRUE)
summary (rpmodelprec)
lsmeans (rpmodelprec, pairwise~Meaning*Probability, adjust="none")

rpmodelc <- lmer (statement ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning*Probability |Item), data=RPs, REML=TRUE)
summary (rpmodelc)
lsmeans (rpmodelc, pairwise~Meaning*Probability, adjust="none")

rpmodelpostc <- lmer (response ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning*Probability |Item), data=RPs, REML=TRUE)
summary (rpmodelpostc)
lsmeans (rpmodelpostc, pairwise~Meaning*Probability, adjust="none")

#Read in total reading time data
TTs <- read.csv("~/Desktop/Air Work/R analyses/Indirect Request Expt/Experiment 1 - probability of success - Libby's data/TTsb.csv")

contrasts (TTs$Meaning)<-my.coding  
contrasts (TTs$Probability)<-my.coding 

#construct the models with crossed random effects for subjects and items for the pre-critical, critical and post-crtical regions of text on total reading times
ttmodelprec <- lmer (Speaker ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning+Probability |Item), data=TTs, REML=TRUE)
summary (ttmodelprec)
lsmeans (ttmodelprec, pairwise~Meaning*Probability, adjust="none")

ttmodelc <- lmer (statement ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning*Probability |Item), data=TTs, REML=TRUE)
summary (ttmodelc)
lsmeans (ttmodelc, pairwise~Meaning*Probability, adjust="none")

ttmodelpostc <- lmer (response ~ Meaning*Probability + (1+Meaning*Probability |P.s) + (1+Meaning*Probability |Item), data=TTs, REML=TRUE)
summary (ttmodelpostc)
lsmeans (ttmodelpostc, pairwise~Meaning*Probability, adjust="none")

#Read in regressions out data
ROs <- read.csv("~/Desktop/Air Work/R analyses/Indirect Request Expt/Experiment 1 - probability of success - Libby's data/ROs.csv")

contrasts (ROs$Meaning)<-my.coding  
contrasts (ROs$Probability)<-my.coding 

#run separate F1 and F2 models as model with both random effects included fails to converge 
romodelprecf1 <- glmer(Speaker ~ Meaning*Probability + (1+Meaning |P.s) , data = ROs, family=binomial) 
summary (romodelprecf1)

romodelprecf2 <- glmer(Speaker ~ Meaning*Probability + (1+Meaning|Item), data = ROs, family=binomial) 
summary (romodelprecf2)

#run separate F1 and F2 models as model with both random effects included fails to converge 
romodelcf1 <- glmer(statement ~ Meaning*Probability + (1+Meaning|P.s), data = ROs, family=binomial) 
summary (romodelcf1)

romodelcf2 <- glmer(statement ~ Meaning*Probability + (1+Meaning |Item), data = ROs, family=binomial) 
summary (romodelcf2)

#run separate F1 and F2 models as model with both random effects included fails to converge 
romodelpostcf1 <- glmer(response ~ Meaning*Probability + (1+Meaning |P.s) , data = ROs, family=binomial) 
summary (romodelpostcf1)
lsmeans (romodelpostcf1, pairwise~Meaning*Probability, adjust="none")

romodelpostcf2 <- glmer(response ~ Meaning*Probability + (1+Meaning|Item) , data = ROs, family=binomial) 
summary (romodelpostcf2)

#Read in regressions in data
RIs <- read_csv("~/Desktop/Air Work/R analyses/Indirect Request Expt/Experiment 1 - probability of success - Libby's data/RIs.csv")

RIs$Meaning <- as.factor(RIs$Meaning)
RIs$Probability <- as.factor (RIs$Probability)

contrasts (RIs$Meaning)<-my.coding  
contrasts (RIs$Probability)<-my.coding 

#run separate F1 and F2 models as model with both random effects included fails to converge 

rimodelimpf1 <- glmer(Probmanip ~ Meaning*Probability + (1+Meaning+Probability |P.s) , data = RIs, family=binomial) 
summary (rimodelimpf1)

rimodelimpf2 <- glmer(Probmanip ~ Meaning*Probability + (1+Meaning+Probability |Item) , data = RIs, family=binomial) 
summary (rimodelimpf2)






