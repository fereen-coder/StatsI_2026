# remove objects = any previous objects in the envt
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function
sum(y)

# (2) find mean of y using your "own" function
sum(y)/length(y)

# now do the same thing, but faster using the built-in R function
mean(y)

# (3) find sum of demeaned values
demeaned_sum <- NULL
for(i in 1:length(y)){
  demeaned_sum[i] <- y[i] - mean(y)
}

#easier version of that same process:
demeaned_sum_made_easy <- y - mean(y)

# (4) calculate sum of squared error
sum_of_sqr_error <- sum(demeaned_sum_made_easy^2)
sum_of_sqr_error

###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)
summary(quantilesVec)

# (1) calculate median 
median(quantilesVec)

# (2) calculate quantiles
quantile(quantilesVec)

# (3) make a histogram of state median income
state.x77[,2]
pdf("median_income_hist.pdf")
hist(state.x77[,2], main="Distribution of the state median income")
dev.off()
# remember to save your plot as a pdf

getwd()
