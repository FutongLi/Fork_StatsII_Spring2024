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

lapply(),  pkgTest)

# set wd for current folder
setwd("/Users/poisson/Documents/GitHub/Fork_StatsII_Spring2024/problemSets/PS01/my_answer")
getwd()


#####################
# Problem 1
#####################

ks_test_normal <- function(data){
  
  #create empiral distribution of input data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  #create reference distribution CDF
  normalCDF <- pnorm(data)
  
  #generate statistic: largerst absolute difference value
  D <- max(abs(empiricalCDF - normalCDF))
  
  #generate p value 
  p_value <- (sqrt(2*pi)/D) * sum(exp((-(2*seq(1, length(data))-1)^2*pi^2)/(8*D^2)))
  
  
  #return the test statistic and p-value
  return(list(D = D, p_value = p_value))
}


#generate Cauchy random variables
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

#execute the test
ks_test_normal(data)

#check the test
ks.test(data, "pnorm", mean = 0, sd = 1)


#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

#calculate the RSS for an OLS regression
RSS_ols <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2] * x
  sum((y - y_hat)^2)
}

#estimate the parameters using BFGS method
bfgs_result <- optim(fn = RSS_ols, par = 0:1, x = data$x, y = data$y, method = "BFGS")

#extract estimated coefficients after optimization
print(bfgs_result$par)

#get the equivalent result using lm()
lm_result <- lm(y ~ x, data)
print(coef(lm_result))


