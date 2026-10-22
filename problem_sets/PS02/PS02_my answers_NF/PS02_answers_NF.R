rm(list=ls())

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()


pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}


lapply(c("readr", "ggplot2"),  pkgTest)
library(dplyr)



###QUESTION 1: political science###
###IGNORE THAT IGNORE THAT IGNORE THAT IGNORE THAT###
#we start by creating the dataset IGNORE THAT IGNORE THAT IGNORE THAT IGNORE THAT
#not_stopped <- c(14, 7)
#bribe_requested <- c(6, 7)
#stopped_given_warning <- c(7, 1)
#df <- data.frame(not_stopped = not_stopped, bribe_requested = bribe_requested, stopped_given_warning = stopped_given_warning)
#print(df)



#df = data.frame( 
 # "Class" = c("Upper class", "Lower class"), 
  #"Police reaction" = c("Not Stopped", "Bribe requested", "Stopped/given warning")
#) 

#conTable = table(df)
#print(conTable)


###!!!###
###RIGHT ANSWERS###
###!!!###

  #a)Calculate the χ2 test statistic by hand/manually.

#H0: the variables are statistically independent from each other.
#H1: the variables are statistically dependent from each other.

#rappel formules:
      #FE = row total/grand total * column total
      #chi_quadrat = sum (Fo - Fe)²/fE


#Row total upper =  27
#Row total lower = 15
#column total not stopped = 21
#column total bribe = 13
#column total stopped = 8
#grand total = 42

FE_Upper_class_x_Not_Stopped <- 27/42 * 21
Chi_square_Upper_class_x_Not_Stopped <- (14 - FE_Upper_class_x_Not_Stopped)^2 / FE_Upper_class_x_Not_Stopped

FE_Upper_class_x_Bribe_requested <- 27/42 * 13
Chi_square_Upper_class_x_Bribe_requested <- (6 - FE_Upper_class_x_Bribe_requested)^2 / FE_Upper_class_x_Bribe_requested

FE_Upper_class_x_Stopped_given_warning <- 27/42 * 8
Chi_square_Upper_class_x_Stopped_given_warning <- (7 - FE_Upper_class_x_Stopped_given_warning)^2 / FE_Upper_class_x_Stopped_given_warning




Chi_square_upper <- sum(Chi_square_Upper_class_x_Not_Stopped + Chi_square_Upper_class_x_Bribe_requested + Chi_square_Upper_class_x_Stopped_given_warning)
print(Chi_square_upper)

#

FE_Lower_class_x_Not_Stopped <- 15/42 * 21
Chi_square_Lower_class_x_Not_Stopped <- (7 - FE_Lower_class_x_Not_Stopped)^2 / FE_Lower_class_x_Not_Stopped

FE_Lower_class_x_Bribe_requested <- 15/42 * 13
Chi_square_Lower_class_x_Bribe_requested <- (7 - FE_Lower_class_x_Bribe_requested)^2 /FE_Lower_class_x_Bribe_requested

FE_Lower_class_x_Stopped_given_warning <- 15/42 * 8
Chi_square_Lower_class_x_Stopped_given_warning <- (1 - FE_Lower_class_x_Stopped_given_warning)^2 / FE_Lower_class_x_Stopped_given_warning

Chi_square_lower <- sum(Chi_square_Lower_class_x_Not_Stopped + Chi_square_Lower_class_x_Bribe_requested + Chi_square_Lower_class_x_Stopped_given_warning)
print(Chi_square_lower)

#Result
Chi_square <- sum(Chi_square_upper + Chi_square_lower)
print(Chi_square)


#b)Now calculate the p-value from the test statistic you just created.
#What do you conclude if α = 0.1?

df <- (2-1)*(3-1)
df
pchisq(Chi_square, df=2, lower.tail=F)

#Conclusion: Because the p-value is above the threshold of 0,1, we fail to reject
#the null hypothesis that the variables are statistically independent from each other.

#c) Calculate the standardized residuals for each cell and put them in the table below.

#rappel formule: z = fO - fE / racine (fE * (1-(row total/total))*(1-column total/total)))

denominator_Upper_class_x_Not_Stopped <- sqrt(FE_Upper_class_x_Not_Stopped * (1-(27/42)) * (1-(21/42)))
z_Upper_class_x_Not_Stopped <- (14 - FE_Upper_class_x_Not_Stopped) / denominator_Upper_class_x_Not_Stopped
round(z_Upper_class_x_Not_Stopped, digits = 3)

denominator_Upper_class_x_Bribe_requested <- sqrt(FE_Upper_class_x_Bribe_requested * (1-(27/42)) * (1-(13/42)))
z_Upper_class_x_Bribe_requested <- (6 - FE_Upper_class_x_Bribe_requested) / denominator_Upper_class_x_Bribe_requested
round(z_Upper_class_x_Bribe_requested, digits = 3)

denominator_Upper_class_x_Stopped_given_warning <- sqrt(FE_Upper_class_x_Stopped_given_warning* (1-(27/42)) * (1-(8/42)))
z_Upper_class_x_Stopped_given_warning <- (7 - FE_Upper_class_x_Stopped_given_warning) / denominator_Upper_class_x_Stopped_given_warning
round(z_Upper_class_x_Stopped_given_warning, digits = 3)

#

denominator_Lower_class_x_Not_Stopped <- sqrt(FE_Lower_class_x_Not_Stopped * (1-(15/42)) * (1-(21/42)))
z_Lower_class_x_Not_Stopped <- (7 - FE_Lower_class_x_Not_Stopped) / denominator_Lower_class_x_Not_Stopped
round(z_Lower_class_x_Not_Stopped, digits = 3)

denominator_Lower_class_x_Bribe_requested <- sqrt(FE_Lower_class_x_Bribe_requested * (1-(15/42)) * (1-(13/42)))
z_Lower_class_x_Bribe_requested <- (7 - FE_Lower_class_x_Bribe_requested) / denominator_Lower_class_x_Bribe_requested
round(z_Lower_class_x_Bribe_requested, digits = 3)

denominator_Lower_class_x_Stopped_given_warning <- sqrt(FE_Lower_class_x_Stopped_given_warning * (1-(15/42)) * (1-(8/42)))
z_Lower_class_x_Stopped_given_warning <- (1 - FE_Lower_class_x_Stopped_given_warning) / denominator_Lower_class_x_Stopped_given_warning
round(z_Lower_class_x_Stopped_given_warning, digits = 3)

#d) How might the standardized residuals help you interpret the results?
#the closer to 0, the more it supports the null: it is how far away each value is from
#expectation. 0,3 = expected higher than observed.
#the higher nbs are contributing: the bigger the value, the stronger the contribution
#those categories matter more


#QUESTION 2: ECONOMICS
data <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))

#a) State a null and alternative (two-tailed) hypothesis.
      #X = "reserved" (reservation policy)
      #Y = "water" (number of new or repaired drinking water facilities in the villages)
#H0: there is no correlation/linear association between X and Y
#H1: there is a correlation/linear association between X and Y


#b) Run a bivariate regression to test this hypothesis in R.
#We start by trying to observe the correlation.

plot(x=data$reserved, y=data$water,
     main = "Reserved spaces for women and nb of new/repaired drinking water facilities",
     ylab = "Nb of new/repaired drinking water facilities",
     xlab = "Reserved spaces for women on the councils (binary variable)",)
#Interpre: it would seem that there are newer/more repaired drinking water facilities
#when women have reserved spaces on councils.

#Then we calculate the correlation coefficient:
cor.test(data$reserved, data$water)
lm(water ~ reserved, data=data)

#Interpretation: very low correlation coefficient.




#c) Interpret the coefficient estimate for reservation policy.
#almost no linear association between both. But! may be bc binary variable.
#when there is an increase in 1 unit of X, there is a
#at X=0, Y=. every one unit increase of X is associated a ___ in Y. on average.





