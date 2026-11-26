#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
library(stringr)
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###QUESTION 1: ECONOMICS###
#In this question, use the prestige dataset in the car library.
#First, run the following commands:

#install.packages(car)
library(car)
data(Prestige)
help(Prestige) #prestige of Canadian occupations: 102 rows, 6 columns

#We would like to study whether individuals with higher levels of income have
#more prestigious jobs.
#Moreover, we would like to study whether professionals have more prestigious jobs than
#blue and white collar workers.


#1. Create a new variable professional by re-coding the variable type so that professionals
#are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).
?ifelse
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
#We can check that the re-coding was correctly done by opening the data. Done!


#2. Run a linear model with prestige as an outcome and income, professional, and the
#interaction of the two as predictors (Note: this is a continuous * dummy interaction.)
#Y FIRST

reg1 <- lm(prestige ~ income + professional + income:professional, data=Prestige)
reg1

#3. Write the prediction equation based on the result.

#Prestige = 21.142259 + income*0.003171 + professional*37.781280 - 0.002326*income*professional

#Prestige for professionals = 58.923539  + income(0.003171 - 0.002326)
#Prestige for white and blue collar = 21.142259 + income*0.003171



#4.Interpret the coefficient for income.

#Holding the job category constant, each additional Canadian dollar (1971) of income
# a person receives is associated with an increase of 0.003171 units of the prestige score.



#5. Interpret the coefficient for professional.

#Holding the income constant, a person holding a professional type of job is
#expected to score 37.781280 units higher than a person having a white or blue
#collar job, on avg. The interactive effect of income and profession type decreases this
#prestige score of 0.002326 on average.


#6. What is the effect of a $1,000 increase in income on prestige score for professional
#occupations?
#In other words, we are interested in the marginal effect of income when
#the variable professional takes the value of 1.
#Calculate the change in ^y associated with a $1,000 increase in income
#based on your answer for (c).

qF_oneUnit <- 0.003171 - 0.002326*1
qF_oneUnit

qF_thou <- qF_oneUnit *1000
qF_thou
#qF_thou=0.845
#An $1,000 increase in income is associated with a 59.8 prestige score.



#7. What is the effect of changing one's occupations from non-professional to professional
#when her income is $6,000?
#We are interested in the marginal effect of professional jobs when
#the variable income takes the value of 6.000.
#Calculate the change in ^y based on your answer for (c).

qG_nonpro <- 21.142259 + 6000*0.003171
qG_nonpro
#qG_nonpro = 40.16826

qG_pro <- 58.923539  + 6000*(0.003171 - 0.002326)
qG_pro
#qG_pro = 63.99354

qG <- qG_pro - qG_nonpro
qG

#A change in one's occupation from non-professional to professional when
#her income is $6,000 is associated with a 23.82528 units increase in prestige score.








###QUESTION 2: POLITICAL SCIENCE###
#Researchers are interested in learning the effect of all of those yard signs on voting
#preferences. Working with a campaign in Fairfax County, Virginia, 131 precincts were
#randomly divided into a treatment and control group. In 30 precincts, signs were posted
#around the precinct that read:
#"For Sale: Terry McAuliffe. Don't Sellout Virgina on November 5."

#Below is the result of a regression with two variables and a constant. The dependent
#variable is the proportion of the vote that went to McAuliffe's opponent Ken Cuccinelli.
#The first variable indicates whether a precinct was randomly assigned to have the
#sign against McAuliffe posted.
#The second variable indicates a precinct that was adjacent to a precinct in the
#treatment group (since people in those precincts might be exposed to the signs).



#1. Use the results from a linear regression to determine whether having these yard signs
#in a precinct affects vote share (e.g., conduct a hypothesis test with alpha = :05).

#Hypotheses:
      #H0 : Beta = 0 vs. H1 : Beta ≠ 0

#t test = beta estimate / sigma estimate
t_stat1 <- 0.042 / 0.016
t_stat1

df <- 131 - 1
df

2*pt(t_stat1, df, lower.tail=FALSE)
#0.009703405
#0.009703405 < 0.05 so we can reject the null hypothesis that our coefficient is 0,
#meaning there is an association between having the yard signs in a precinct
#and the vote share for Ken Cuccinelli.



#2. Use the results to determine whether being next to precincts with these yard signs
#affects vote share (e.g., conduct a hypothesis test with alpha = :05).

#Hypotheses:
      #H0 : Beta = 0 vs. H1 : Beta ≠ 0

#t test = beta estimate / sigma estimate
t_stat2 <- 0.042 / 0.013
t_stat2

df2 <- 131 - 1
df2

2*pt(t_stat2, df2, lower.tail=FALSE)
#0.001563955 < 0.05 so we can reject the null hypothesis that our coefficient is 0,
#meaning there is an association between being next to precincts with the yard signs
#and the vote share for Ken Cuccinelli.


#3. Interpret the coefficient for the constant term substantively.
#In a control precinct, we estimate the average proportion of the vote
#that went to Ken Cuccinelli is 30,2%.




#4. Evaluate the model fit for this regression. What does this tell us about the
#importance of yard signs versus other factors that are not modeled?

#R² is 0.094, which is very low. We need a better theory lol.
