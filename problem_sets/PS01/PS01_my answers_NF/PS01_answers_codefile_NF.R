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
n

#calculating the mean of y, aka ȳ
y_mean <- mean(y)
y_mean

#calculating the standard deviation of y
sd_y <-sd(y)
sd_y


#calculating the standard error of y
std_error <- sd_y/sqrt(n)
std_error

#t-score and result
alpha <- 0.1
degrees_of_freedom <- n - 1
degrees_of_freedom
t_score <- qt(p = alpha / 2, df = degrees_of_freedom, lower.tail = FALSE)
t_score
#pt and qt commands pour les t-distributions
#la z-stat me donne la distance (std dev) entre ma test-stat et la mean
#la p-value est la zone sous la courbe, entre la test-stat et l'extérieur
    #de la courbe: zone de rejet. p>alpha = reject H0

#O.E +- z * standard error
#t = sample mean - pop mean / standard error. need the degree of freedom: qt function.
#pour calculer l'intervalle: utilise la formule pareil que pour z-critical value
#diff between 2 groups: t = (sample gr 1 - sample gr 2) - O / standard error

margin_error <- t_score * std_error
margin_error

lower_bound <- y_mean - margin_error
upper_bound <- y_mean + margin_error

print(c(lower_bound, upper_bound))
#confidence interval= [93.95993 ; 102.92007] #PAS MEME RESULTAT QUE LE T-TEST DESSOUS

###METHOD 2: R-COMMAND VERSION with 90% confidence interval###

result_CI <- t.test(y, conf.level = 0.90)
result_CI

confidence_interval <- result_CI$conf.int
print(confidence_interval)



#2. Next, the school counselor was curious whether the average student IQ in
#her school is higher than the average IQ score (100) among all the schools
#in the country.
#+Using the same sample, conduct the appropriate hypothesis test with α = 0:05.

#We are conducting a one-sided test:
    #n = 25
    #y_mean : average IQ at the school (sample mean) = 98.44
    #y_mean_country : average IQ at all the schools in the country (population mean)
y_mean_country <- 100
    #-> statistically diff from y_mean = 98.44 ?
    
    #Our hypotheses for a one-sided test:
      #H0 = y_mean ≤ y_mean_country
      #H1 = y_mean > y_mean_country 

#check if the assumptions are met:
n 

#calculate the test statistic
t_statistic <- (y_mean - y_mean_country) / std_error
t_statistic

df <- n-1
df

#calculate the p-value
pt(t_statistic, df, lower.tail = FALSE)
pt


#draw the conclusion
#"Because the p-value is above the threshold of 0,05, we fail to reject the null
#hypothesis that the school's average IQ is equal or inferior to the average IQ in the country"



  
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

#QUESTION 1
#Please plot the relationships among Y, X1, X2, and X3? What are the correlations
#among them (you just need to describe the graph and the relationships among them)?

plot(expenditure$X1, expenditure$Y,
     main = "State expenditure depending on personal income",
     xlab = "X1: Per capita personal income in state",
     ylab = "Y: Per capita expenditure on shelters/housing assistance in state")
#positive correlation: the higher the per capital personal income in the state,
                      #the more per capita expenditure on shelters/housing assistance in state.


plot(expenditure$X2, expenditure$Y,
     main = "State expenditure depending on financial insecurity",
     xlab = "X2: Number of residents per 100,000 that are 'financially insecure' in state",
     ylab = "Y: Per capita expenditure on shelters/housing assistance in state")
#does not look linear


plot(expenditure$X3, expenditure$Y,
     main = "State expenditure depending on urban population",
     xlab = "X3: Number of people per thousand residing in urban areas in state",
     ylab = "Y: Per capita expenditure on shelters/housing assistance in state")
#slight positive correlation: the more people per thousand residing in urban areas in state
                      #the more per capita expenditure on shelters/housing assistance in state.


plot(expenditure$X1, expenditure$X2,
     main = "Income and financial insecurity",
     xlab = "X1: Per capita personal income in state",
     ylab = "X2: Number of residents per 100,000 that are 'financially insecure' in state")
#

plot(expenditure$X1, expenditure$X3,
     main = "Income and urban population",
     xlab = "X1: Per capita personal income in state",
     ylab = "X3: Number of people per thousand residing in urban areas in state")

plot(expenditure$X2, expenditure$X3,
     main = "Financial insecurity and urban population",
     xlab = "X2: Number of residents per 100,000 that are 'financially insecure' in state",
     ylab = "X3: Number of people per thousand residing in urban areas in state")


#QUESTION 2
#Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?
plot(expenditure$Region, expenditure$Y,
     main = "State expenditure depending on the region",
     xlab = "Region",
     ylab = "Y: Per capita expenditure on shelters/housing assistance in state")

library(tidyverse)
region_mean <- expenditure %>%
  group_by(Region) %>%
  mutate(region_mean = mean(Y)) %>%
  ungroup()
table(region_mean$Region, region_mean$region_mean)

#->calculer les moyennes de chaque région puis faire un hist pour comparer facilement


#QUESTION 3
#Please plot the relationship between Y and X1? Describe this graph and the relationship. Reproduce the above graph including one more variable Region and display
#Reproduce the above graph including one more variable Region and display
#different regions with different types of symbols and colors.

plot(expenditure$X1, expenditure$Y,
     main = "State expenditure depending on personal income",
     xlab = "X1: Per capita personal income in state",
     ylab = "Y: Per capita expenditure on shelters/housing assistance in state")

library(ggplot2)
ggplot(data = expenditure,
       aes(x = X1,
           y = Y,
           colour = as.factor(Region),
           shape = as.factor(Region)))+
  geom_point()
