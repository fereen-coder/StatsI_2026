# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()

# Agenda
# (a.) Correlation
# (b.) Bivariate regression 

# -------------------------------#
# a. Correlation
# -------------------------------#

df <- read.csv("C:/Users/nalia/Documents/Doctorat TCD/Quants I/Tutorial/StatsI_2026/datasets/fictional_data.csv")
View(df)

# Create a scatter plot 
plot(df$edu,df$income)

# Calculate correlation
cor(df$edu,df$income)


# Add correlation to scatter plot
text(1.5, 2600, sprintf("Correlation=%s", round(cor(df$edu,df$income), 4)))

# Improve visualization with detailed axis and main labels, save as PDF


# Run a t-test for the correlation coefficient
cor.test(df$edu,df$income)

# Check p-value
sprintf("%.20f", 7.52e-07)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion -> we reject the null! there is an association

# -------------------------------#
# b. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education
model <- lm(df$income ~ df$edu)

# Save model as object

# Output interpretation:
#  - Intercept (b0)
#  - Slope (b1)
summary(model)

# Conduct t-test for the slope of a regression line
    #7.582
250.64/33.06

# Check p-value
    #7.52e-07
sprintf("%.20f", 2.17e-06)

#-> the slope is not 0, aka not a zero association at the population level

# Create a residual plot
model.res <- resid(model)

plot(df$edu, model.res,
     ylab = "Residuals",
     xlab = "University eduction level (in years)")
abline(0,0) #that's drawing a baseline and looking at the pattern (as X increases, no patter to identify) = homoskedasticity

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Calculate 95% and 99% confidence intervals around your estimated coefficients
confint(model, level=0.95)
    #the intervall does not include 0 -> we can reject the null! (there is association)
confint(model, level=0.99)
    #the intervall does not include 0 -> we can reject the null! (there is association)

# Create a scatter plot w/ educate and income, add regression line
plot(x=df$edu, y=df$income,
     ylab = "Income",
     xlab = "University eduction level (in years)",)
abline(model, col = "red")

# What is the prediction equation?
summary(model)
  #hat_income = 976.16 + 250.64*educ

# Make predictions for first observation in df (for the first X, aka educ)
  976.16 + 250.64*1 #= 1226.8


# Make predictions for a range of x values (specifically min to max of education)
predict(model) #predicted outcomes
model$fitted.values #predicted outcomes
unique(df$edu) #unique values of x
seq(min(df$edu), max(df$edu), by=1) #specify a sequences for which predictions are returned
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$income), by=1)))
  
  
# Add standard errors for your predicted values

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
# Rembmer: more variability in individual responses --> wider intervals

# Make predictions for x values not in data
# (1) Mean education
# (2) Only unique values of x

# Now, plot those predictions in base R

# Plot, regression line with confidence intervals
# First, save confidence intervals
# Second, create scatter plot
# Then, add regression line, lower bound, and upper bound

# And now try with ggplot2