###################################
# Tutorial 10: Survival Analysis #
###################################

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

#### Survival Analysis

# The `child` dataset from the `eha` package is a dataset of 26,855 children born in 
# Skellefte?, Sweden, 1850-1884. Children are followed for fifteen years or until death or 
# outmigration.
# The response variable is `exit`
# Explanatory variables include:
# - id: An identification number.
# - m.id: Mother's id.
# - sex: Sex.
# - socBranch: Working branch of family (father).
# - birthdate: Birthdate.
# - enter: Start age of follow-up, always zero.
# - exit: Age of departure, either by death or emigration.
# - event: Type of departure, death = 1, right censoring = 0.
# - illeg: Born out of marriage ("illegitimate")?
# - m.age: Mother's age.

data(child)
head(child)
# tail(child)
summary(child)

## a)data()## a) Using the Surv() function, build a survival object out of the `child` data.frame. 
##    Using survfit() and R's plotting functions, produce a Kaplan-Meier plot of the data,
##    firstly for overall survival, and secondly comparing categories of socBranch. How do
##    you interpret the second plot?

# with() 被用来指定在 child 数据框中执行 Surv(enter, exit, event) 表达式
child_surv <- with(child, Surv(enter, exit, event))
summary(child_surv)

km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))

plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km) # equal to last one

km_socBranch <- survfit(child_surv ~ socBranch, data = child)
summary (km_socBranch)
autoplot(km_socBranch)


## b) Run a Cox Proportional Hazard regression on the data, using an additive model with 
##    `socBranch` and `sex` as explanatory variables. Run a test to assess the quality of the
##    model. How can we interpret the coefficients? Plot the model.

cox <- coxph(child_surv ~ sex + socBranch, data = child)
summary(cox)

drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
               )
)

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))


