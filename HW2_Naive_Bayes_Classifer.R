# CRIM 4012
# Homework #2: Naive Bayes Classifer
# Saara Ghani, 15486636

# Set up ----
setwd("/Users/saaraghani/Desktop/Spring24/CRIM4012/HW")
library(dplyr)
library(tidyr)
library(kableExtra)
library(naivebayes)
library(pROC)
library(splines)

datRecid <- read.csv("NIJ_s_Recidivism_Challenge_Full_Dataset_20240131.csv")

#
# 1.) Explore the remaining variables in the Georgia parolee data. Assess ----
# whether any of them substantially increase the predictive performance of 
# the naïve Bayes classifier on the validation dataset 
# (That's where Training_Sample==0) 
#

# ANSWER:
# Variables I have chosen after exploration: Race, Gang_Affiliated, 
# Supervision_Level_First, Prior_Arrest_Episodes_Felony, 
# Prior_Arrest_Episodes_Misd, Dependents, Program_Attendances

# coverting empty values to "NA" values to appease the predict fucntion
datRecid <- datRecid |>
  mutate(Gang_Affiliated=case_match(Gang_Affiliated,
                                   "" ~ NA,
                                   .default=Gang_Affiliated))
datRecid <- datRecid |>
  mutate(Supervision_Level_First=case_match(Supervision_Level_First,
                                    "" ~ NA,
                                    .default=Supervision_Level_First))

# Creating the naive bayes model
nb1 <- naive_bayes((Recidivism_Within_3years=="true")~Race+
                     Gang_Affiliated+
                     Supervision_Level_First+
                     Dependents+
                     Prior_Arrest_Episodes_Felony+
                     Prior_Arrest_Episodes_Misd+
                     Program_Attendances,
                   data=subset(datRecid,Training_Sample==0),
                   laplace=1)

a <- datRecid |> 
  select(names(get_cond_dist(nb1)))
datRecid$pNBpack <- predict(nb1, newdata=a, type="prob")[,2]

# check that the predicted probabilities are nearly the same
plot(datRecid$pNBpack, datRecid$p)


#
# a.) Calculate the misclassification rate, the false positive rate, and the ----
# false negative rate for your best model
#

  # misclassification
datRecid |> 
  group_by(Training_Sample) |>
  summarize(misclass=mean((Recidivism_Within_3years=="false" & pNBpack>0.5) |
                            (Recidivism_Within_3years=="true" & pNBpack<0.5)))

  # false positive rate
datRecid |> 
  group_by(Training_Sample) |>
  filter(Recidivism_Within_3years=="false") |>
  summarize(mean(pNBpack > 0.5))

  # false negative rate
datRecid |> 
  group_by(Training_Sample) |>
  filter(Recidivism_Within_3years=="true") |>
  summarize(mean(pNBpack < 0.5))

#
# b.) Create an ROC curve and report the AUC ----
#
nbROC <- roc((Recidivism_Within_3years=="true")~pNBpack, data=datRecid)
plot(nbROC)
nbROC$auc # Area under the curve: 0.6831


#
# c.) Assess whether your predicted probabilities are well-calibrated ----
#
# calibration (quality measure of a model)
datRecid |>
  filter(Training_Sample==0) |>
  mutate(pCat=cut(pNBpack,breaks=seq(1:9)/10)) |>
  filter(!is.na(pCat)) |> # for the few with p<0.1
  group_by(pCat) |>
  summarize(phat=mean(Recidivism_Within_3years=="true")) |>
  mutate(p=0.05+(1:8)/10) |>
  plot(phat~p, data=_, pch=16, col="purple3",
       xlim=0:1, ylim=0:1,
       xlab="Predicted probability",
       ylab="Actual probability")

# a smoothed version
calib <- lm((Recidivism_Within_3years=="true")~ns(pNBpack,10), 
            data=subset(datRecid, Training_Sample==0))
pNBpack <- seq(min(datRecid$pNBpack), max(datRecid$pNBpack), length=100)
lines(pNBpack,
      predict(calib, newdata=data.frame(pNBpack=pNBpack)),
      type="l", lwd=3, col="blue")

abline(0,1)
rug(quantile(datRecid$p, prob=(0:10)/10))

# ANSWER: 
# The predicted probabilities are calibrated acceptably, because they are 
# relatively close to the x=y line. 


#
# d.) Select one parolee and create an evidence balance sheet for that parolee ----
#

# function to calculate weights of evidence
WoE <- function(x,y)
{
  w <- table(y, x)
  w <- w/rowSums(w)
  return( log(w[2,]/w[1,]) )
}

# apply WoE() to key parolee features
modNB <- lapply(datRecid[c("Race", "Gang_Affiliated", "Supervision_Level_First", 
                           "Prior_Arrest_Episodes_Felony", 
                           "Prior_Arrest_Episodes_Misd", 
                           "Dependents","Program_Attendances")], 
                function(x) WoE(x, datRecid$Recidivism_Within_3years))

modNB <- data.frame(var=rep(names(modNB), lengths(modNB)),
                    value=unlist(sapply(modNB, names)),
                    woe=unlist(modNB),
                    row.names = NULL)
 

# predict for everyone
predNBwoe <- datRecid |>
  select(ID, Race, Gang_Affiliated, Supervision_Level_First, 
         Prior_Arrest_Episodes_Felony, Prior_Arrest_Episodes_Misd, Dependents, 
         Program_Attendances) |>
  pivot_longer(-ID,names_to="var") |>
  left_join(modNB, join_by(var,value))

# evidence balance sheet
ebs <- 
  predNBwoe |> 
  filter(ID==73) |>
  mutate(feature=paste0(var,"=",value)) |>
  select(feature,woe) |> 
  rbind(data.frame(feature="Prior",woe=modNB$woe[1])) |>
  mutate(woe=round(100*woe)) |>
  arrange(feature!="Prior", desc(abs(woe)))  # put Prior at top

posEvidence <- ebs |> filter(woe>0)
negEvidence <- ebs |> filter(woe<0)

maxRows <- max(nrow(posEvidence), nrow(negEvidence))
tab <- data.frame(posVar=rep(NA,maxRows+3),
                  woeP=NA,
                  negVar=NA,
                  woeN=NA)
tab[1:nrow(posEvidence),1:2] <- posEvidence
tab[1:nrow(negEvidence),3:4] <- negEvidence
tab[maxRows+1,c(1,3)] <- c("Total positive weight","Total negative weight")
tab[maxRows+1,c(2,4)] <- colSums(tab[,c(2,4)], na.rm=TRUE)
tab[maxRows+2,3] <- "Total weight of evidence"
tab[maxRows+2,4] <- sum(tab[maxRows+1,c(2,4)])
tab[maxRows+3,3] <- "Probability ="
tab[maxRows+3,4] <- round(1/(1+exp(-tab[maxRows+2,4]/100)), 2)
tab$woeN <- gsub(".00", "", as.character(tab$woeN))
tab[is.na(tab)] <- ""

# Making a nice table
library(kableExtra)
kbl(tab,
    col.names=c("Feature", "Weight of evidence", "Feature", "Weight of evidence"),
    row.names = FALSE,
    align="lrlr",
    digits=0) |>
  row_spec(maxRows+1:3,background="mediumpurple") |>
  kable_classic()


####

#
# 2.) Compare the performance of your best naïve Bayes classifier separately ----
# for white and black parolees
#

# White parolees:
datRecid_white <- subset(datRecid,Race=="WHITE")
nb1_white <- naive_bayes((Recidivism_Within_3years=="true")~Race+
                     Gang_Affiliated+
                     Supervision_Level_First+
                     Dependents+
                     Prior_Arrest_Episodes_Felony+
                     Prior_Arrest_Episodes_Misd+
                     Program_Attendances,
                   data=subset(datRecid_white,Training_Sample==0),
                   laplace=1)

# Assess perforce for white parollees using the area under the ROC curve
nbROC <- roc((Recidivism_Within_3years=="true")~pNBpack, data=datRecid_white)
plot(nbROC)
nbROC$auc # Area under the curve: 0.7009

# White parolee: misclassification rate
datRecid_white |> 
  group_by(Training_Sample) |>
  summarize(misclass=mean((Recidivism_Within_3years=="false" & pNBpack>0.5) |
                            (Recidivism_Within_3years=="true" & pNBpack<0.5)))

# White parolee: false positive rate
datRecid_white  |> 
  group_by(Training_Sample) |>
  filter(Recidivism_Within_3years=="false") |>
  summarize(mean(pNBpack > 0.5))

# White parolee: false negative rate
datRecid_white  |> 
  group_by(Training_Sample) |>
  filter(Recidivism_Within_3years=="true") |>
  summarize(mean(pNBpack < 0.5))

# Black Parolees
datRecid_black <- subset(datRecid,Race=="BLACK")
nb1_black <- naive_bayes((Recidivism_Within_3years=="true")~Race+
                           Gang_Affiliated+
                           Supervision_Level_First+
                           Dependents+
                           Prior_Arrest_Episodes_Felony+
                           Prior_Arrest_Episodes_Misd+
                           Program_Attendances,
                         data=subset(datRecid_black,Training_Sample==0),
                         laplace=1)
# Assess perforce for Black parollees using the area under the ROC curve
nbROC <- roc((Recidivism_Within_3years=="true")~pNBpack, data=datRecid_black)
plot(nbROC)
nbROC$auc # Area under the curve: 0.6692

# Black parolee: misclassification rate
datRecid_black |> 
  group_by(Training_Sample) |>
  summarize(misclass=mean((Recidivism_Within_3years=="false" & pNBpack>0.5) |
                            (Recidivism_Within_3years=="true" & pNBpack<0.5)))

# Black parolee: false positive rate
datRecid_black  |> 
  group_by(Training_Sample) |>
  filter(Recidivism_Within_3years=="false") |>
  summarize(mean(pNBpack > 0.5))

# Black parolee: false negative rate
datRecid_black  |> 
  group_by(Training_Sample) |>
  filter(Recidivism_Within_3years=="true") |>
  summarize(mean(pNBpack < 0.5))

#
# a.) Discuss what you find and potential implications ----
#

# ANSWER: 
# My best naive bayes classier model is better at predicting whether white
# parolees will recidivate within 3 years as opposed to whether black parolees 
# will recidivate. We know this because the Area under the ROC curve for white 
# parolees is higher (0.7009) than for Clack parolees (0.6692). The 
# classifications rate, fals positive rate, and false negative rate for Black 
# parolees are also slightly higher than for white parolees.
# The potential implications of this are that there is a racial bias to the 
# data. Models predict and give higher risk scores to Black individuals, but 
# they are less accurate than they are for white individuals. More accurate 
# data needs to be collected for Black individuals, and law enforcement 
# agencies need to recognize the racial discrepancies of predictive models 
# before applying them in the field. 

#
