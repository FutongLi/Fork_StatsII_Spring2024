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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
Question1.1
# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)
head(gdp_data)

ftable(xtabs(~OIL+REG+GDPWdiff,data = gdp_data))

#do some wrangling
gdp_data$GDPWdiff <- ifelse(gdp_data$GDPWdiff > 0, 'positive',
                                   ifelse(gdp_data$GDPWdiff < 0, 'negative', 'no change'))
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff,
                                   levels = c('positive', 'negative','no change'),
                                   labels = c('positive', 'negative','no change'))
gdp_data$REG <- factor(gdp_data$REG,
                       levels = c(0,1),
                       labels = c('Non-Democracy','Democracy'))
gdp_data$OIL <- factor(gdp_data$OIL,
                       levels = c(0,1),
                       labels = c('Otherwise','exceeded'))

ftable(xtabs(~OIL+REG+GDPWdiff,data = gdp_data))

#set a reference level for the outcome
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")

#Constract a unordered multinomial logit 
unorder_model <- multinom(GDPWdiff~OIL+REG, data = gdp_data,)
summary(unorder_model)

#Question1.2
#Perform an ordered (proportional odds) logistic regression
order.model <- polr(GDPWdiff~OIL+REG, data = gdp_data, Hess = TRUE)
summary(order.model)

#####################  
# Problem 2
#####################

#run a poisson regression 
# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

#Run a poisson regression model
model.pois <- glm(PAN.visits.06 ~competitive.district+marginality.06+PAN.governor.06, data = mexico_elections, family = poisson)
summary(model.pois)

# predicted the mean number of visits from the winning PAN presidential candidate for a hypothetical district by hand.
exp(cfs[1] + cfs[2] * 1 + cfs[3] * 0 + cfs[4] * 1 )

# predicted the mean number by R
pred <- data.frame(
  competitive.district=1,
  marginality.06 = 0,
  PAN.governor.06=1 
)

# check with predict() function
predict(model.pois, newdata = pred, type = "response")




