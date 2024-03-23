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

lapply(c("nnet", "MASS", "dplyr", "stargazer", "texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# view data
head(gdp_data)

# make a copy
df <- gdp_data
# add a new column as outcome variable
df$GDPWdiff <- ifelse(df$GDPWdiff > 0, 'positive', ifelse(df$GDPWdiff < 0, 'negative', 'no change'))
# convert integers into factors
df$GDPWdiff <- as.factor(df$GDPWdiff)
df$REG <- as.factor(df$REG)
df$OIL <- as.factor(df$OIL)
# set a reference variable for the outcome
df$GDPWdiff <- relevel(df$GDPWdiff, ref = "no change")
# run a unordered multinomial logistic regression model
mult_log <- multinom(GDPWdiff ~ REG + OIL, data = df)
# exponentiate coefficients 
exp_coef <- exp(coef (mult_log) [, c (1:3) ])
exp_coef <- round(exp_coef,2)
summary(mult_log)
exp(coef(mult_log))

texreg(list(mult_log), digits=3)

stargazer(exp_coef) # didn't work


# run an ordered multinomial logistic regression model
df$GDPWdiff <- factor(df$GDPWdiff,
                      levels = c("negative","no change","positive"),
                      ordered = TRUE)

ord_log <- polr(GDPWdiff ~ REG + OIL, data = df, Hess = TRUE)
summary(ord_log)
texreg(list(ord_log), digits=3)

coef <- coef(summary(ord_log))
p <- pnorm(abs(coef[, "t value"]), lower.tail = FALSE) * 2
(coef <- cbind(coef, "p value" = p))

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# view data
str(mexico_elections)

# wrangle data
dat <- within(mexico_elections, {
  competitive.district <- as.logical(competitive.district)
  PAN.governor.06 <- as.logical(PAN.governor.06)
})

dat <- select(dat, marginality.06:competitive.district)

# EDA
summary(dat)

with(dat,
     list(mean(PAN.visits.06), var(PAN.visits.06))) # not equal
hist(dat$PAN.visits.06)  # not normaly distributed

# perform poisson regression model
reg.ps <- glm(PAN.visits.06 ~., data = dat, family = poisson)
summary(reg.ps)
texreg(list(reg.ps), digits=3)

exp(coef(reg.ps))

# c) Estimated mean of visits
coef <- coef(reg.ps)
exp(coef[1] + coef[2]*0 + coef[3]*1 + coef[4]*1)
# Result: 0.0149

pred <- data.frame(marginality.06  = 0,
                   PAN.governor.06 = TRUE,
                   competitive.district = TRUE)

predict(reg.ps, newdata = pred, type = "response")
