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

lapply(c('stringr', 'texreg', 'stargazer'), pkgTest)

# set wd for current folder
#setwd()
getwd()

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# data wrangling
str(climateSupport)
summary(climateSupport)
head(climateSupport)

# check missing values
sum(is.na(climateSupport))

# check factor distribution
table(climateSupport$choice)
table(climateSupport$countries)
table(climateSupport$sanctions)

# convert variables into unordered factor variables
climateSupport$sanctions_fac <- as.factor(as.integer(climateSupport$sanctions))
climateSupport$countries_fac <- as.factor(as.integer(climateSupport$countries))

# convert response variable into logical variable
climateSupport$choice <- as.logical(ifelse(climateSupport$choice == 'Supported', 1, 0))

# 1. run an additive model
mod <- glm(choice ~ countries_fac + relevel(sanctions_fac, ref = '1'),
           data = climateSupport[, !names(climateSupport) %in% c('countries', 'sanctions')],
           family = binomial(link = 'logit'))

# print the summary 
summary(mod)
texreg(list(mod), digits=3)
stargazer(mod$coefficients)

# check if coefficients' p valuesa re less than 0,05 alpha level
p_values <- summary(mod)$coefficients[, "Pr(>|z|)"]
print(p_values)
p_values <= 0.05

texreg(p_values, digits=5)
stargazer(p_values)



# 2.(a) increasing sanctions from 5% to 15% 
log_odd <- -0.08 + 0.336 * 0 + 0.648 * 1 - 0.192 * 0 - 0.325 * 1 - 0.495 * 0
round(exp(log_odd),3)


# 2.(b) estimated prob given that 80 of 192 countries with no sanctions
# compute logit value
logit <- -0.081 + 0.336 * 1 + 0.648 * 0 - 0.192 * 1 - 0.325 * 0 - 0.495 * 0

# Use the logistic function for converting
esti_prob <- plogis(logit)

# print the result
round(esti_prob, 3)

# 2.c) additive model vs interation term model
mod_int <- glm(choice ~ countries_fac * relevel(sanctions_fac, ref = '2'),
           data = climateSupport[, !names(climateSupport) %in% c('countries', 'sanctions')],
           family = binomial(link = 'logit'))

# print the model output
summary(mod_int)
stargazer(mod_int)

# perform an likelihood ratio test
anova(mod, mod_int, test = "LRT")


