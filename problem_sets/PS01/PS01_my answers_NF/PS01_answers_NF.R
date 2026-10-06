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

lapply(c(),  pkgTest)

#####################
# Problem 1: A school counselor was curious about the average of IQ of the students in her school and
#took a random sample of 25 students' IQ scores. The following is the data set:
#####################

y <- c(105 , 69 , 86 , 100 , 82 , 111 , 104 , 110 , 87 , 108 , 87 , 90 , 94 , 113 , 112 , 98 , 80 , 97 , 95 , 111 , 114 , 89 , 95 , 126 , 98)
#this is an ordinal type of data: the numbers are not continuous, but ordered.


##############
#1. Find a 90% confidence interval for the average student IQ in the school
#(As a hint, you first need the mean and SD to create a CI):

####METHOD 1: MANUAL VERSION OF THE CALCULATIONS###
#having n show how many cases there are in the sample
n <- length(y)
n           #n = 25

#calculating the mean of y, aka ȳ
ȳ <- mean(y)
ȳ           #ȳ = 98.44

#calculating the standard deviation of y
sd_y <-sd(y)
sd_y        #finding the standard deviation: S = 13.09287


#calculating the standard error of y
σ <- sd_y/sqrt(n)
σ           # σ = 2.618575

#t-score and result
alpha <- 0.1
degrees_of_freedom <- n - 1
t_score <- qt(p = alpha / 2, df = degrees_of_freedom, lower.tail = FALSE)

#pt and qt commands pour les t-distributions
#la z-stat me donne la distance (std dev) entre ma test-stat et la mean
#la p-value est la zone sous la courbe, entre la test-stat et l'extérieur
    #de la courbe: zone de rejet. p>alpha = reject H0

#O.E +- z * standard error
#t = sample mean - pop mean / standard error. need the degree of freedom: qt function.
#pour calculer l'intervalle: utilise la formule pareil que pour z-critical value
#diff between 2 groups: t = (sample gr 1 - sample gr 2) - O / standard error

margin_error <- t_score * sd_y

lower_bound <- ȳ - margin_error
upper_bound <- ȳ + margin_error

print(c(lower_bound, upper_bound))
#confidence interval= [76.03964 ; 120.84036]

###METHOD 2: R-COMMAND VERSION with 95% confidence interval###
result <- t.test(y)

confidence_interval <- result$conf.int

print(confidence_interval)



#2. Next, the school counselor was curious whether the average student IQ in
#her school is higher than the average IQ score (100) among all the schools
#in the country.
#+Using the same sample, conduct the appropriate hypothesis test with α = 0:05.

#We are conducting a one-sided test:
    #n = 25
    #ȳ : average IQ at the school (sample mean) = 99.44
    #ȳ_country : average IQ at all the schools in the country (population mean)
ȳ_country <- 100
    #-> statistically diff from ȳ = 98.44 ?
    
    #Our hypotheses for a one-sided test:
      #H0 = y ≤ ȳ_country
      #H1 = y > ȳ_country

#check if the assumptions are met:
n

#calculate the test statistic
t

#calculate the p-value


#draw the conclusion

SE <- sqrt((σ1²/n1) + (σ2²/n2)) #double check!!
t_statistic <- (ȳ_country - ȳ) - 0 / SE 


#Example: Create our Test Statistic and p-value
# 1) create function for "by hand"
# difference in means test
t_test_q2 <= function(inputVec1,inputVec2 , m0=0) {
  ȳ ; ȳ_country
  sd_y ; sd_country <- sd(y_country)
  n ; n_country #??????????
  











  
#####################
# Problem 2
#####################
#Researchers are curious about what affects the amount of money communities
#spend on addressing homelessness. The following variables constitute our
#data set about social welfare expenditures in the USA.

library(ggplot2)
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
expenditure

#State  50 states in US
#Y      per capita expenditure on shelters/housing assistance in state
#X1     per capita personal income in state
#X2     Number of residents per 100,000 that are "financially insecure" in state
#X3     Number of people per thousand residing in urban areas in state
#Region 1=Northeast, 2= North Central, 3= South, 4=West

ggplot(expenditure, aes(x = factor(region), y = Y)) +
  geom_boxplot()

ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point()

