###################################
#    ProblemSet 04 Futong Li.     #
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

# Load Libraies
lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# Review data
data(child)
head(child)
summary(child)

# Create a survival object
child_surv <- with(child, Surv(enter, exit, event))

# Fit the Cox Proportional Hazards model
cox_mod <- coxph(child_surv ~ m.age + sex, data = child)
summary(cox_mod)
stargazer(cox_mod, type = 'latex')

# Single term deletions
cox_drop <- drop1(cox_mod, test = "Chisq")
stargazer(cox_drop, type = 'latex')

cox_fit <- survfit(cox_mod)
autoplot(cox_fit)



