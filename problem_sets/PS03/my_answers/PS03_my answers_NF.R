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
# lapply(c("stringr"),  pkgTest)
library(ggplot2)
install.packages("caTools")
library(caTools)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")



###



###QUESTION 1###
#1. Run a regression where Y = voteshare and X = difflog.
reg1 <- lm(voteshare ~ difflog, data=data)
summary(reg1)

#2. Make a scatterplot of the two variables and add the regression line.
plot(x=data$difflog, y=data$voteshare,
     main = "Effect of the difference in campaign spending on the incumbent's voteshare",
     cex.main=1,
     ylab = "Incumbent's voteshare",
     xlab = "Difference in campaign spending between incumbent and challenger") +
  abline(reg1, col = "red")


#3. Save the residuals of the model in a separate object.
residuals_reg1 <- residuals(reg1)
residuals_reg1


#4. Write the prediction equation.
summary(reg1)
#Y = intercept + X*slope
#Y = 0.579031 + X*0.041666
#incumbent's vote share = 0.579031 + 0.579031*difference in campaign spending
      #between incumbent and challenger



###



###QUESTION 2###

#1. Run a regression where Y = presvote and X = difflog.
reg2 <- lm(presvote ~ difflog, data=data)
summary(reg2)


#2. Make a scatterplot of the two variables and add the regression line.
plot(x=data$difflog, y=data$presvote,
     main = "Effect of the difference in campaign spending on the voteshare of the presidential candidate",
     cex.main=.75,
     ylab = "Vote share of the presidential candidate of the incumbent's party",
     cex.lab=.70,
     xlab = "Difference in campaign spending between incumbent and challenger") +
  abline(reg2, col = "red")


#3. Save the residuals of the model in a separate object.
residuals_reg2 <- residuals(reg2)


#4. Write the prediction equation.
summary(reg2)
#Y = intercept + X*slope
#Y = 0.507583 + X*0.023837
#vote share of the presidential candidate of the incumbent's party =
        #0.507583 + 0.023837*difference between incumbent and challenger's spending




###



###QUESTION 3###

#1. Run a regression where Y = voteshare and X = presvote.
reg3 <- lm(voteshare ~ presvote, data=data)
summary(reg3)


#2. Make a scatterplot of the two variables and add the regression line.
plot(x=data$presvote, y=data$voteshare,
     main = "Vote share of the presidential candidate of the incumbent's party and incumbent's electoral success",
     cex.main=.75,
     ylab = "Incumbent's voteshare",
     xlab = "Vote share of the presidential candidate of the incumbent's party") +
  abline(reg3, col = "red")



#3. Write the prediction equation.
summary(reg3)
#Y = intercept + X*slope
#Y = 0.441330 + X*0.388018
#vote share of the presidential candidate of the incumbent's party =
    #0.441330 + 0.388018*incumbent's electoral success



###



###QUESTION 4###
#The residuals from part (a) tell us how much of the variation in voteshare
#is not explained by the difference in spending between incumbent and challenger.
#The residuals in part (b) tell us how much of the variation in presvote
#is not explained by the difference in spending between incumbent and challenger
#in the district.

#1. Run a regression where the outcome variable is the residuals from Question 1 and the
#explanatory variable is the residuals from Question 2.
reg4 <- lm(residuals_reg1 ~ residuals_reg2, data=data)
summary(reg4)


#2. Make a scatterplot of the two variables and add the regression line.
plot(x=residuals_reg2, y=residuals_reg1,
     main = "Unexaplained variation in the incumbent's vote share and \nunexplained variation in the vote share of the presidential candidate of the incumbent's party",
     cex.main=.8,
     ylab = "Unexaplained variation in the incumbent's vote share",
     cex.lab=.8,
     xlab = "Unexplained variation in the vote share of the presidential candidate of the incumbent's party") +
  abline(reg4, col = "red")


#3. Write the prediction equation.
summary(reg4)
#Y = intercept + X*slope
#Y = -5.934e-18 + X*2.569e-01



###



###QUESTION 5###
#What if the incumbent's vote share is affected by both the president's popularity and the
#difference in spending between incumbent and challenger?.

#1. Run a regression where Y = incumbent's voteshare and X = difflog and presvote.
reg5 <- lm(voteshare ~ difflog + presvote, data = data)
summary(reg5)


#2. Write the prediction equation.
summary(reg5)
#Y = 0.4486442 + X1*0.0355431 + X2*0.2568770
#Y = 0.4486442 + difflog*0.0355431 + presvote*0.2568770


#3. What is it in this output that is identical to the output in Question 4?
      #residuals_reg2 = 2.569e-01
      #presvote = 0.2568770

#Why do you think this is the case?
#residuals1 tell us  how much of the variation in voteshare is NOT explained by difflog
#residuals2 tell us how much of the variation in presvote is NOT explained by difflog.

  #-> 
# The coefficients for the incumbent presidential candidate's vote share are
#identical in the models of Question 4 and Question 5.
# This is because the regression model in Q4 controls for the effect of
#difference in campaign spending by regressing the residuals of the models of Q1 and Q2.
# As the effect of difference in campaign spending has been removed from the model in Q4,
#we can compute the effect of the incumbent presidential candidate's vote share on
#the vote share of the party.
# Hence, the estimated beta2 coefficient for the incumbent presidential candidate's
#vote share that is displayed in the output of the multivariate regression of Q5 is
#identical to the estimated beta1 of the model of Q4 that presents the effect of
#presidential candidate vote share by controlling for difference in campaign spending.







