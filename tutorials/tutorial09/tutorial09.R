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

lapply(c('tidyverse', 'ggplot2'),  pkgTest)

# set wd for current folder
setwd()

## Poisson

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)

# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-year period (ment)

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# What conclusions would you draw from this analysis?

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)


# data preprocessing
long_data <- within(long_data, {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

# EDA
str(long_data)
summary(long_data)

with(long_data,
     list(mean(art), var(art))) # not =

# a)
hist(long_data$art)

ggplot(long_data, aes(ment, art, color = fem)) +
  geom_jitter(alpha = 0.5)

# OLS?
mod.lm <- lm(art ~ ., data = long_data)
summary(mod.lm)

mod2.lm <- lm(art ~ fem*., data = long_data)

# assumption meet test
plot(predict(mod2.lm), abs(resid(mod2.lm)))

sresid <- rstandard(mod2.lm)
hist(sresid, main = "")

par(mfrow = c(2,2))
plot(mod2.lm) # for lr model assesment for normal distribution

# b) Poisson regression
mod.ps <- glm(art ~., data = long_data, family = poisson)
summary(mod.ps)

cfs <- coef(mod.ps)
cfs
# predicted no. of articles for a married male PhD researcher with 1 child at 2-rated 
# institute whose PhD supervisor published 5 articles.
exp(cfs[1] + cfs[2]*0 + cfs[3]*5 + cfs[4]*2 + cfs[5]*1 + cfs[6]*1) # equal to predict()

# c) over-dispersion
install.packages("AER")
library(AER)

dispersiontest(mod.ps) # over-dispersion test 
# dispersion value shoule less than 1

# family = quasipoisson指定了使用广义线性模型的伪泊松回归
# 与普通的泊松回归相比，伪泊松回归允许泊松分布的方差大于其均值，适用于泊松回归的一些假设不成立的情况，
# 例如过度离散性或过度离差。因此，伪泊松回归可以更灵活地适用于一些实际数据。
mod2.ps <- glm(art ~ fem *., data = long_data, family = quasipoisson)
summary(mod2.ps)

# quasipoisson is used to address over-disperson issue
# 0 inflation is used to address too many zero values


install.packages("pscl")
library(pscl)

mod.zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(mod.zip)

AIC(mod.ps, mod2.ps)
AIC(mod.zip, mod2.ps)
# smaller is better
