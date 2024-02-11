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

lapply(c('stringr', 'tidyverse', 'ggplot2'),  pkgTest)

# set wd for current folder
# setwd(dirname()
getwd()

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)


# Data Wrangling
grad <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt", stringsAsFactors = TRUE)


str(grad)
summary(grad) # notice the min of nsibs is less than 0, which is weird.

# drop problematic cases
grad <- grad[which(grad$nsibs >= 0),]

# 
grad$hsgrad <- ifelse(grad$hsgrad == 'Yes', 1, 0)
grad$hsgrad <- as.logical(grad$hsgrad)

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.


ml <- glm(hsgrad ~ ., data =grad, family = 'binomial')
summary(ml)

# likelihood ratio test
nullml <- glm(hsgrad ~ 1, data = grad, family = 'binomial') # set 1 as input, we can get the intercept for this model
summary(nullml)
# run anova test to compare two models' difference above
anova(nullml, ml, test = "Chisq")
anova(nullml, ml, test = "LRT") # eauivalent
# CI
?confint
confint(ml, level = 0.95)
?exp
expcof <- exp(confint(ml, level = 0.95))
class(expcof)
# creare a df to store values
?cbind
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m

# Logistic 回归模型通常输出的是对数几率
# 通过指数化系数，可以得到原始几率的估计值
cofml <- data.frame(lower = expcof[,1], # every values will be exponentially computed 
                    coefs = exp(coef(ml)),
                    upper = expcof[,2])

grad$predicted <- predict(ml, type = "response")

# coef ci plot 
ggplot(data = cofml, mapping = aes(x = row.names(cofml), y = coefs))+
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")




# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?
 

# treat nsibs as a factor 
# model.matrix()函数被用于lm()和glm()等函数的内部，用于创建模型的设计矩阵
# 该函数将因子变量转换为二进制编码以便于回归分析
model.matrix( ~ unique(nsibs), data = grad) # not helpful and need fix
model.matrix( ~ as.factor(unique(nsibs)), data = grad) # converted it into dummy variables

# perform a model
ml2 <- glm(hsgrad ~ nonwhite + mhs + fhs,
    data = grad,
    family = 'binomial')

with(grad, expand.grid(nonwhite = unique(nonwhite),
                             mhs = unique(mhs),
                             fhs = unique(fhs)))

predicted_data <- with(grad, expand.grid(nonwhite = unique(nonwhite),
                                               mhs = unique(mhs),
                                               fhs = unique(fhs)))

predicted_data <- cbind(predicted_data, predict(ml2, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))
# codes from jeff's lecture
predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })


grad$nsibs_cut <- cut(grad$nsibs,
                      breaks = c(0, 0.9, 1, 3, Inf),
                      include.lowest = TRUE,
                      labels = c('None', 'One', "Two_Three", "FourPlus"))


ml3 <- glm(hsgrad ~., 
            data = grad[,!names(grad) %in% c("nsibs", "nsibs_f")],  # why nsibs_f
            family = "binomial")

summary(ml3)
summary(ml)
summary(ml2)



