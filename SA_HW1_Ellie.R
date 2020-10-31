##################################################
##############Survival Analysis HW 1
##################################################

#Oct. 30, 2020
#Ellie Code

############################
############## Loading in Data & Libraries
############################
library(dplyr)
library(sas7bdat)
library(haven)
library(broom)

#target variable = hour, survive

setwd("~/Documents/NC State/Fall 3/Survival Analysis")

hurricane <- read.sas7bdat("hurricane.sas7bdat")
table(hurricane$survive)

############################
############## Graphing (A) - Basic
############################
# Survival probability across time for all pumps together – not broken down by failure type.
# Survival Function - Hurricane Data #
Surv(time = hurricane$hour, event = hurricane$survive == 0)
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0) #plots correctly for some reason if =0, not 1

recid_km <- survfit(recid_surv ~ 1, data = hurricane)
summary(recid_km)
plot(recid_km, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

ggsurvplot(recid_km, data = hurricane, conf.int = TRUE, palette = "purple",
           xlab = "Hour", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1)

############## 

simple_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = hurricane)
summary(simple_km)
plot(simple_km, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

#recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 1)

############################
############## Graphing (B) - Stratified
############################
# Stratified Analysis #
# Survival probability across time for pumps broken down by failure type overlaid into one graph.
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)
survdiff(recid_surv ~ survive, rho = 0, data = hurricane)

recid_strat <- survfit(recid_surv ~ reason, data = hurricane)
summary(recid_strat)
ggsurvplot(recid_strat, data = hurricane, conf.int = TRUE, palette = c("purple", "orange", "yellow", "blue", "red"),
           xlab = "Hour", ylab = "Survival Probability", break.y.by = 0.1,
           legend.title = "Failure Reason", legend.labs = c(0,1,2,3,4))

pairwise_survdiff(recid_surv ~ survive, rho = 0, data = hurricane)

############################
############## Graphing (C) - Conditional Hazard
############################
# Conditional failure probabilities across time for all pumps together – not broken down by failure type.
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0) #plots correctly for some reason if =0, not 1

recid_km <- survfit(recid_surv ~ 1, data = hurricane)
#Hazard Function
recid_km$hp <- recid_km$n.event/recid_km$n.risk
recid_haz <- merge(data.frame(time = seq(1,52,1)), data.frame(time = recid_km$time, hp = recid_km$hp), by = "time", all = TRUE)
recid_haz[is.na(recid_haz) == TRUE] <- 0

plot(y = recid_haz$hp, x = recid_haz$time, main = "Hurricane Hazard Probability Function", xlab = "Time", ylab = "Hazard Probability",
     type = 'l') #There's a big spikes at time=25ish & 45ish, wonder what's going on here

############################
############## Graphing (D) - Conditional Hazard Stratified
############################

#STILL NEED TO STRATIFY 

# Conditional failure probabilities across time for pumps broken down by failure type overlaid into one graph.
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)
survdiff(recid_surv ~ survive, rho = 0, data = hurricane)
recid_strat <- survfit(recid_surv ~ reason, data = hurricane)


#Hazard Function
str(recid_strat)
recid_strat<- tidy(recid_strat)

recid_strat$hp <- recid_strat$n.event/recid_strat$n.risk
#recid_haz <- merge(data.frame(time = seq(1,52,1)), data.frame(time = recid_strat$time, hp = recid_strat$hp, reason=recid_strat$n.censor), by = "time", all = TRUE)
recid_strat[is.na(recid_strat) == TRUE] <- 0

plot(y = recid_haz$hp, x = recid_haz$time, by = recid_strat$strata, main = "Hurricane Hazard Probability Function", xlab = "Time", ylab = "Hazard Probability",
     type = 'l')

ggplot(data=recid_strat, aes(x=time, y=hp, group=strata, color=strata)) + geom_line() + 
  labs(x="Hour", y= " Hazard Probability", main="Hurricane Hazard Probability Function") + 
  theme_light()

w####################################################################################################
#Instructions:
#Provide the following graphs as well as any meaningful insights from the graphs that you see:
  # Survival probability across time for all pumps together – not broken down by failure type.
  # Survival probability across time for pumps broken down by failure type overlaid into one graph.
  # Conditional failure probabilities across time for all pumps together – not broken down by failure type.
  # Conditional failure probabilities across time for pumps broken down by failure type overlaid into one graph.
  # (HINT: Some of these can be put into the appendix. Only include the ones in the main report you feel provide the meaningful insights.)


