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

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# view data
head(gdp_data)

# make a copy
df <- gdp_data

# add a new column as outcome variable
df$GDPWdiff <- ifelse(df$GDPWdiff > 0, 'positive', ifelse(df$GDPWdiff < 0, 'negative', 'no change'))

df$GDPWdiff <- as.factor(df$GDPWdiff)

head(df)

# set a reference variable for the outcome
df$GDPWdiff <- relevel(df$GDPWdiff, ref = "no change")

# run a unordered multinomial logistic regression model
mult_log <- multinom(GDPWdiff ~ REG + OIL, data = df)
summary(mult_log)
exp(coef(mult_log))

coefficients <- coef(mult_log)

cutoff<- -coefficients[,1] / coefficients[,2]

# print the cutoff points
print(cutoff)

# run an ordered multinomial logistic regression model
df$GDPWdiff <- factor(df$GDPWdiff,
                      levels = c(0,1,2),
                      labels = c("negative", "no change", "positive"))

ord_log <- polr(GDPWdiff ~ REG + OIL, data = df, Hess = TRUE)
summary(ord_log)



#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
