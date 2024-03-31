# Replication Code for "Public Attitudes toward Young Men"
# Author: Dalston G. Ward
# Date: September, 2018

library(data.table)
library(lmtest)
library(multiwayvcov)
library(texreg)
library(car)




# Table 1 (Variable labels added in Tex)
texreg(list(
  log_main,
  log_main_NW),
  override.se = list(
    log_main_coefs[,2],
    log_main_NW_coefs[,2]),
  override.pvalues = list(
    log_main_coefs[,4],
    log_main_NW_coefs[,4]),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men0' = "0%",
    'men25' = "25%",
    'men75' = "75%",
    'men100' = "100%",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

```
```{r job model}
# Figure 2 and Table 2, Model 1
job_log_mod <- glm(jobs_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,family = binomial(link = "logit"),data = datw)
job_log_vcov <- cluster.vcov(job_log_mod, datw[, id])
(job_log_coef <- coeftest(job_log_mod, job_log_vcov))
linearHypothesis(job_log_mod, "men25 - men75", vcov = job_log_vcov)
linearHypothesis(job_log_mod, "men25 - men75", vcov = job_log_vcov)
linearHypothesis(job_log_mod, "men75 - men100", vcov = job_log_vcov)

# Figure 2 and Table 2, Model 2
job_log_modNW <- glm(jobs_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,family = binomial(link = "logit"),data = data)
job_log_vcovNW <- cluster.vcov(job_log_modNW, data[, id])
(job_log_coefNW <- coeftest(job_log_modNW, job_log_vcovNW))

```
```{r sec model, echo = FALSE}
# Figure 2 and Table 2, Model 3
sec_log_mod <- glm(security_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,family = binomial(link = "logit"),data = datw)
sec_log_vcov <- cluster.vcov(sec_log_mod, datw[ , id])
(sec_log_coef <- coeftest(sec_log_mod, sec_log_vcov))

# test differences between effects as mentioned in text
linearHypothesis(sec_log_mod, "men0 - men25 = 0", vcov = sec_log_vcov)
linearHypothesis(sec_log_mod, "men100 - men75 = 0", vcov = sec_log_vcov)

# Figure 2 and Table 2, Model 4
sec_log_modNW <- glm(security_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,family = binomial(link = "logit"),data = data)
sec_log_vcovNW <- cluster.vcov(sec_log_modNW, data[ , id])
(sec_log_coefNW <- coeftest(sec_log_modNW, sec_log_vcovNW))

```
```{r culture model, echo = FALSE}
# Figure 2 and Table 2, Model 5
culture_log_mod <- glm(culture_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,family = binomial(link = "logit"),data = datw)
culture_log_vcov <- cluster.vcov(culture_log_mod, datw[ , id])
(culture_log_coef <- coeftest(culture_log_mod, culture_log_vcov))

# test differences between effects as mentioned in text
linearHypothesis(culture_log_mod, "men0 - men25", vcov = culture_log_vcov)
linearHypothesis(culture_log_mod, "men25 - men75", vcov = culture_log_vcov)
linearHypothesis(culture_log_mod, "men75 - men100", vcov = culture_log_vcov)

# Figure 2 and Table 2, Model 6
culture_log_modNW <- glm(culture_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,family = binomial(link = "logit"),data = data)
culture_log_vcovNW <- cluster.vcov(culture_log_modNW, data[ , id])
(culture_log_coefNW <- coeftest(culture_log_modNW, culture_log_vcovNW))
```

```{r figure 2, included=FALSE}
# Figure 2 
opar <- par(mfrow = c(1,3), mar = c(0, 0, 3, 1.5), oma = c(4.5,5.5,0,.5))

#### Jobs Plot ####

plot_stuff <- clust_confint(job_log_mod, job_log_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ]
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

plot( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20, ylim = c(.8, 5.2), xlim = c(0.6, 1.4), type = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n", main = "DV: Economic Potential")
abline(v = 0, lwd = .75, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(1, at = seq(0, 2, by = 0.05))
axis(2, at = plot_stuff$y_vals, labels = plot_stuff$varName)

mtext( "Effect on Probability of High Scale Rating", side = 1, line = 3, cex = 1, outer = T)
mtext("Percentage Young Men in Group", side = 2, line = 3)

#### Security plot ####

plot_stuff <- clust_confint(sec_log_mod, sec_log_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ] 
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

plot( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20, ylim = c(.8, 5.2), xlim = c(0.6, 1.4), type = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n", main = "DV: Security Threat")
abline(v = 0, lwd = .75, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(1, at = seq(0, 2, by = 0.05))

#### Culture Plot ####

plot_stuff <- clust_confint(culture_log_mod, culture_log_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ] 
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

plot( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20, ylim = c(.8, 5.2), xlim = c(0.6, 1.4), type = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n", main = "DV: Cultural Threat")
abline(v = 0, lwd = .75, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(1, at = seq(0, 2, by = 0.05))

par(opar)

```





# used in plotting below
clust_confint <- function(Mod, Cov){
  tt <- qt(c(0.025, 0.975), Mod$df.residual)
  se <- sqrt(diag(Cov))
  ci <- data.frame( varName = names(coef(Mod)), beta = coef(Mod), upper = coef(Mod) + se * tt[ 1 ], lower = coef(Mod) + se * tt[ 2 ], stringsAsFactors = F)
  
  ci$varName <- gsub("group_edu|group_work|group_eng|group_men", "", ci$varName)
  return(ci)
}

setwd("/Users/poisson/Documents/Trinity/Msc in ADSD/Hilary Term/Applied Stats/Replication")

data <- fread("YoungImmMenConjoint.csv", na.strings = "")
head(data)

# 36 respondents who were outside the target age range (18-75)
# or had missing values on state, gender, or education 
# were dropped from the data before calculating weights.

# This data is used for all weighted estimation.
datw <- data[!is.na(ps_weights)] 

# Table A.2
resp_level <- unique(data[ , .(id, age, as.numeric(gender == "Male"), duration_seconds, edu2)]) #respondent-level data for summary stats (all respondents)
resp_level[ , summary(age)]; resp_level[,sd(age)]
resp_level[ , summary(V3)]; resp_level[ , sd(V3, na.rm = T)]
resp_level[ , summary(duration_seconds)]; resp_level[ , sd(duration_seconds, na.rm = T)]
resp_level[  , round(table(edu2)/.N*100, 2)]
resp_level[  , round(sum(is.na(edu2))/.N*100, 2)]

# Table A.3 (Age)
resp_levelNW <- unique(datw[ , .(id, age, gender, edu2, state, ps_weights, duration_seconds)]) 
#respondent-level data (only respondents in estimation sample)
# 取了id（受访者的唯一标识符）、age（年龄）、gender（性别）、
# edu2（教育程度）、state（所在州）、ps_weights（预先分配的样本权重）、
# duration_seconds（调查持续时间）这些列

# make categorical age variables
resp_levelNW[ , age1824 := age %in% 18:24]
resp_levelNW[ , age2534 := age %in% 25:34]
resp_levelNW[ , age3544 := age %in% 35:44]
resp_levelNW[ , age4554 := age %in% 45:54]
resp_levelNW[ , age5564 := age %in% 55:64]
resp_levelNW[ , age65plus := age >= 65]

# Unweighted
round(resp_levelNW[ , mean(age1824)],4)*100
round(resp_levelNW[ , mean(age2534)],4)*100
round(resp_levelNW[ , mean(age3544)],4)*100
round(resp_levelNW[ , mean(age4554)],4)*100
round(resp_levelNW[ , mean(age5564)],4)*100
round(resp_levelNW[ , mean(age65plus)],4)*100

# Weighted
round(resp_levelNW[ , weighted.mean(age1824, ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(age2534, ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(age3544, ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(age4554, ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(age5564, ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(age65plus, ps_weights)],4)*100

# Table A.3 (gender)
resp_levelNW[ , female := gender == "Female"]
round(resp_levelNW[ , mean(female)],4)*100 # Unweighted
round(resp_levelNW[ , weighted.mean(female, ps_weights)],4)*100 # Weighted

# Table A.3 (state)
# Unweighted
round(resp_levelNW[ , mean(state == "Baden-Wuerttemberg")],4)*100
round(resp_levelNW[ , mean(state == "Bayern")],4)*100
round(resp_levelNW[ , mean(state == "Berlin")],4)*100
round(resp_levelNW[ , mean(state == "Brandenburg")],4)*100
round(resp_levelNW[ , mean(state == "Bremen")],4)*100
round(resp_levelNW[ , mean(state == "Hamburg")],4)*100
round(resp_levelNW[ , mean(state == "Hessen")],4)*100
round(resp_levelNW[ , mean(state == "Mecklenburg-Vorpommern")],4)*100
round(resp_levelNW[ , mean(state == "Niedersachsen")],4)*100
round(resp_levelNW[ , mean(state == "Nordrhein-Westfalen")],4)*100
round(resp_levelNW[ , mean(state == "Rheinland-Pfalz")],4)*100
round(resp_levelNW[ , mean(state == "Saarland")],4)*100
round(resp_levelNW[ , mean(state == "Sachsen")],4)*100
round(resp_levelNW[ , mean(state == "Sachsen-Anhalt")],4)*100
round(resp_levelNW[ , mean(state == "Schleswig-Holstein")],4)*100
round(resp_levelNW[ , mean(state == "Thueringen")],4)*100

# Weighted
round(resp_levelNW[ , weighted.mean(state == "Baden-Wuerttemberg", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Bayern", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Berlin", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Brandenburg", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Bremen", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Hamburg", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Hessen", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Mecklenburg-Vorpommern", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Niedersachsen", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Nordrhein-Westfalen", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Rheinland-Pfalz", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Saarland", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Sachsen", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Sachsen-Anhalt", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Schleswig-Holstein", ps_weights)],4)*100
round(resp_levelNW[ , weighted.mean(state == "Thueringen", ps_weights)],4)*100

# Table A.3 (education)
#Unweighted
round(resp_levelNW[ , mean(edu2 == "Low")], 4)*100
round(resp_levelNW[ , mean(edu2 == "Medium")], 4)*100
round(resp_levelNW[ , mean(edu2 == "High")], 4)*100
round(resp_levelNW[ , mean(edu2 == "University")], 4)*100

#Weighted
round(resp_levelNW[ , weighted.mean(edu2 == "Low", ps_weights)], 4)*100
round(resp_levelNW[ , weighted.mean(edu2 == "Medium", ps_weights)], 4)*100
round(resp_levelNW[ , weighted.mean(edu2 == "High", ps_weights)], 4)*100
round(resp_levelNW[ , weighted.mean(edu2 == "University", ps_weights)], 4)*100


##########################
## Statistical analysis ##
##########################

# Model for Figure 1 and Table B.1, Model 1.
main <- lm(settle_binary ~ factor(group_edu) + men0 + men25 + men75 + men100,
           weights = ps_weights, data = datw)
main_vcov <- cluster.vcov(main, datw[ , "id"])
(main_coefs <- coeftest(main, main_vcov))

# Footnote 14, main text
linearHypothesis(main, "men25 - men0 = 0", vcov = main_vcov)

# Table B.1, Model 2
main_NW <- lm(settle_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, data = data)
main_NW_vcov <- cluster.vcov(main_NW, data[, "id"])
(main_NW_coefs <- coeftest(main_NW, main_NW_vcov))

# Figure 1 (Save as 5in x 7in to recreate paper)
plot_stuff <- clust_confint(main, main_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ] # Remove intercept
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

opar <- par(mar = c(5, 4, 1, 2))
plot( x = as.numeric(plot_stuff$beta), y = as.numeric(plot_stuff$y_vals), pch = 20, ylim = c(0.8, 5.2), xlim = c( -0.23, .13 ), type = "n", yaxt = "n", xlab = "", ylab = "Percentage Young Men in Group", xaxt = "n", main = "")
abline(v = 0, lwd = .5, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(2, at = plot_stuff$y_vals, labels = plot_stuff$varName)
axis(1, at = c(-.3, -.2, -.1, 0, .1, .2, .3))
mtext("Effect on Probability of Settlement Preference", side = 1, line = 2.25, cex = 1, outer = F)
par(opar)

# Table B.1 (Variable labels added in Tex)
texreg(list(
  main,
  main_NW),
  override.se = list(
    main_coefs[,2],
    main_NW_coefs[,2]),
  override.pvalues = list(
    main_coefs[,4],
    main_NW_coefs[,4]),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men0' = "0%",
    'men25' = "25%",
    'men75' = "75%",
    'men100' = "100%",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

# Figure 2, Panel 1, and Table B.2, Model 1
job_mod <- lm( jobs_binary ~ factor(group_edu) + men0 + men25 + men75 + men100 , weights = ps_weights, data = datw)
job_vcov <- cluster.vcov(job_mod, datw[, id])
(job_coef <- coeftest(job_mod, job_vcov))
linearHypothesis(job_mod, "men0 - men25", vcov = job_vcov)
linearHypothesis(job_mod, "men25 - men75", vcov = job_vcov)
linearHypothesis(job_mod, "men75 - men100", vcov = job_vcov)

# Table B.2, Model 2
job_modNW <- lm( jobs_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, data = data)
job_vcovNW <- cluster.vcov(job_modNW, data[, id])
(job_coefNW <- coeftest(job_modNW, job_vcovNW))

# Figure 2, Panel 2, and Table B.2, Model 3
sec_mod <- lm(security_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw)
sec_vcov <- cluster.vcov(sec_mod, datw[ , id])
(sec_coef <- coeftest(sec_mod, sec_vcov))

# test differences between effects as mentioned in text
linearHypothesis(sec_mod, "men0 - men25 = 0", vcov = sec_vcov)
linearHypothesis(sec_mod, "men100 - men75 = 0", vcov = sec_vcov)

# Table B.2, Model 4
sec_modNW <- lm(security_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, data = data)
sec_vcovNW <- cluster.vcov(sec_modNW, data[ , id])
(sec_coefNW <- coeftest(sec_modNW, sec_vcovNW))

# Figure 2, Panel 3, and Table B.2, Model 5
culture_mod <- lm(culture_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw)
culture_vcov <- cluster.vcov(culture_mod, datw[ , id])
(culture_coef <- coeftest(culture_mod, culture_vcov))

# test differences between effects as mentioned in text
linearHypothesis(culture_mod, "men0 - men25", vcov = culture_vcov)
linearHypothesis(culture_mod, "men25 - men75", vcov = culture_vcov)
linearHypothesis(culture_mod, "men75 - men100", vcov = culture_vcov)

# Table B.2, Model 6
culture_modNW <- lm(culture_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, data = data)
culture_vcovNW <- cluster.vcov(culture_modNW, data[ , id])
(culture_coefNW <- coeftest(culture_modNW, culture_vcovNW))




# Figure 2 ( make this 5in by 7in to replicate the paper)

opar <- par(mfrow = c(1,3), mar = c(0, 0, 3, 1.5), oma = c(4.5,5.5,0,.5))

#### Jobs Plot ####

plot_stuff <- clust_confint(job_mod, job_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ]
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

plot( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20, ylim = c(.8, 5.2), xlim = c( -.1, .1), type = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n", main = "DV: Economic Potential")
abline(v = 0, lwd = .75, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(1, at = c(  -.1, -0.05, 0, .05, .1, .15))
axis(2, at = plot_stuff$y_vals, labels = plot_stuff$varName)

mtext( "Effect on Probability of High Scale Rating", side = 1, line = 3, cex = 1, outer = T)
mtext("Percentage Young Men in Group", side = 2, line = 3)

#### Security plot ####

plot_stuff <- clust_confint(sec_mod, sec_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ] 
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

plot( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20, ylim = c(.8, 5.2), xlim = c( -.1, .1), type = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n", main = "DV: Security Threat")
abline(v = 0, lwd = .75, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(1, at = c(  -.1, -0.05, 0, .05, .1, .15))

#### Culture Plot ####

plot_stuff <- clust_confint(culture_mod, culture_vcov)
plot_stuff <- plot_stuff[ -c(1,2,3,4), ] 
plot_stuff[5, ] <- c("50%", 0, 0, 0)
plot_stuff$y_vals <- c(1, 2, 4, 5, 3)
plot_stuff$varName[1:4] <- c("0%", "25%", "75%", "100%")

plot( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20, ylim = c(.8, 5.2), xlim = c( -.1, .1), type = "n", yaxt = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n", main = "DV: Cultural Threat")
abline(v = 0, lwd = .75, lty = 2)
points( x = plot_stuff$beta, y = plot_stuff$y_vals, pch = 20)
segments( x0 = as.numeric(plot_stuff$lower), x1 = as.numeric(plot_stuff$upper), y0 = plot_stuff$y_vals, y1 = plot_stuff$y_vals)
axis(1, at = c(  -.1, -0.05, 0, .05, .1, .15))

par(opar)

# Table B.2 (variable labels added in Tex)
texreg(list(
  job_mod,
  job_modNW,
  sec_mod,
  sec_modNW,
  culture_mod,
  culture_modNW),
  override.se = list(
    job_coef[,2],
    job_coefNW[,2],
    sec_coef[,2],
    sec_coefNW[,2],
    culture_coef[,2],
    culture_coefNW[,2]),
  override.pvalues = list(
    job_coef[,4],
    job_coefNW[,4],
    sec_coef[,4],
    sec_coefNW[,4],
    culture_coef[,4],
    culture_coefNW[,4]),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men0' = "0%",
    'men25' = "25%",
    'men75' = "75%",
    'men100' = "100%",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

 

# Table C.1, Model 1
male_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[ gender == "Male"])
male_vcov <- cluster.vcov(male_mod, datw[gender == "Male", id])
(male_coef <- coeftest(male_mod, male_vcov))

# Table C.1, Model 2
female_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[ gender == "Female" ])
female_vcov <- cluster.vcov(female_mod, datw[gender == "Female", id])
(female_coef <- coeftest(female_mod, female_vcov))

# Old and Young
datw[ , old_age := age > 49 ]

# Table C.1, Model 3
young_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[ old_age == F ])
young_vcov <- cluster.vcov(young_mod, datw[ old_age == F, id])
(young_coef <- coeftest(young_mod, young_vcov))

# Table C.1, Model 4
old_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[old_age == T ])
old_vcov <- cluster.vcov(old_mod, datw[old_age == T, id])
(old_coef <- coeftest(old_mod, old_vcov))


# Education models
datw[ , high_edu := edu2 %in% c("High", "University")]

# Table C.1, Model 5
low_ed <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[high_edu == F])
low_ed_vcov <- cluster.vcov(low_ed, datw[high_edu == F, id])
(low_ed_coef <- coeftest(low_ed, low_ed_vcov))

# Table C.1, Model 6
high_ed <- lm(settle_binary ~   factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[high_edu == T])
high_ed_vcov <- cluster.vcov(high_ed, datw[high_edu == T, id])
(high_ed_coef <- coeftest(high_ed, high_ed_vcov))

# East V west
datw[ , ost := state %in% c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thueringen")]

# Table C.1, Model 7
ost_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[ ost == T])
ost_vcov <- cluster.vcov(ost_mod, datw[ost == T, id])
(ost_coef <- coeftest(ost_mod, ost_vcov))

# Table C.1, Model 8
west_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[ ost == F ])
west_vcov <- cluster.vcov(west_mod, datw[ost == F, id])
(west_coef <- coeftest(west_mod, west_vcov))

### Table C.1 (Variable labels added in Tex)
texreg(list(
  male_mod,
  female_mod,
  young_mod,
  old_mod,
  high_ed,
  low_ed,
  ost_mod,
  west_mod),
  override.se = list(
    male_coef[,2],
    female_coef[,2],
    old_coef[,2],
    young_coef[,2],
    high_ed_coef[,2],
    low_ed_coef[,2],
    ost_coef[,2],
    west_coef[,2]),
  override.pvalues = list(
    male_coef[,4],
    female_coef[,4],
    old_coef[,4],
    young_coef[,4],
    high_ed_coef[,4],
    low_ed_coef[,4],
    ost_coef[,4],
    west_coef[,4]),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men0' = "0%",
    'men25' = "25%",
    'men75' = "75%",
    'men100' = "100%",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

######################
# Table D.1, Model 1 #
######################

datw[ , lmc_threat := age <= 30 & gender == "Male" & edu2 %in% c("Low")]
lmc_threatA_interact <- lm(settle_binary ~  factor(group_edu) + men0*lmc_threat 
                           + men25*lmc_threat + men75*lmc_threat + men100*lmc_threat,
                           weights = ps_weights, data = datw[])
lmc_threatA_interact_vcov <- cluster.vcov(lmc_threatA_interact, datw[, id])
(lmc_threatA_coef <- coeftest(lmc_threatA_interact, lmc_threatA_interact_vcov))
comp_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25+ men75 + men100 + lmc_threat, weights = ps_weights, data = datw[])
lmtest::waldtest(lmc_threatA_interact, comp_mod, vcov = lmc_threatA_interact_vcov, test = "F")

# Table D.1, Model 2
datw[ , lmc_threat := age <= 35 & gender == "Male" & edu2 %in% c("Low")]
lmc_threatB_interact <- lm(settle_binary ~  factor(group_edu) + men0*lmc_threat + men25*lmc_threat + men75*lmc_threat + men100*lmc_threat, weights = ps_weights, data = datw[])
lmc_threatB_interact_vcov <- cluster.vcov(lmc_threatB_interact, datw[, id])
(lmc_threatB_coef <- coeftest(lmc_threatB_interact, lmc_threatB_interact_vcov))
comp_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25+ men75 + men100 + lmc_threat, weights = ps_weights, data = datw[])
lmtest::waldtest(lmc_threatB_interact, comp_mod, vcov = lmc_threatB_interact_vcov, test = "F")

# Table D.1, Model 3
datw[ , lmc_threat := age <= 40 & gender == "Male" & edu2 %in% c("Low")]
lmc_threatC_interact <- lm(settle_binary ~  factor(group_edu) + men0*lmc_threat + men25*lmc_threat + men75*lmc_threat + men100*lmc_threat, weights = ps_weights, data = datw[])
lmc_threatC_interact_vcov <- cluster.vcov(lmc_threatC_interact, datw[, id])
(lmc_threatC_coef <- coeftest(lmc_threatC_interact, lmc_threatC_interact_vcov))
comp_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25+ men75 + men100 + lmc_threat, weights = ps_weights, data = datw[])
lmtest::waldtest(lmc_threatC_interact, comp_mod, vcov = lmc_threatC_interact_vcov, test = "F")

# Table D.1, Model 4
datw[ , lmc_threat := age <= 30 & gender == "Male" & edu2 %in% c("Low", "Medium")]
lmc_threatD_interact <- lm(settle_binary ~  factor(group_edu) + men0*lmc_threat + men25*lmc_threat + men75*lmc_threat + men100*lmc_threat, weights = ps_weights, data = datw[])
lmc_threatD_interact_vcov <- cluster.vcov(lmc_threatD_interact, datw[, id])
(lmc_threatD_coef <- coeftest(lmc_threatD_interact, lmc_threatD_interact_vcov))
comp_mod <- lm(settle_binary ~   factor(group_edu) + men0 + men25+ men75 + men100 + lmc_threat, weights = ps_weights, data = datw[])
lmtest::waldtest(lmc_threatD_interact, comp_mod, vcov = lmc_threatD_interact_vcov, test = "F")

# Table D.1, Model 5
datw[ , lmc_threat := age <= 35 & gender == "Male" & edu2 %in% c("Low", "Medium")]
lmc_threatE_interact <- lm(settle_binary ~  factor(group_edu) + men0*lmc_threat + men25*lmc_threat + men75*lmc_threat + men100*lmc_threat, weights = ps_weights, data = datw[])
lmc_threatE_interact_vcov <- cluster.vcov(lmc_threatE_interact, datw[, id])
(lmc_threatE_coef <- coeftest(lmc_threatE_interact, lmc_threatE_interact_vcov))
comp_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25+ men75 + men100 + lmc_threat, weights = ps_weights, data = datw[])
lmtest::waldtest(lmc_threatE_interact, comp_mod, vcov = lmc_threatE_interact_vcov, test = "F")

# Table D.1, Model 6
datw[ , lmc_threat := age <= 40 & gender == "Male" & edu2 %in% c("Low", "Medium")]
lmc_threatF_interact <- lm(settle_binary ~ factor(group_edu) + men0*lmc_threat + men25*lmc_threat + men75*lmc_threat + men100*lmc_threat, weights = ps_weights, data = datw[])
lmc_threatF_interact_vcov <- cluster.vcov(lmc_threatF_interact, datw[, id])
(lmc_threatF_coef <- coeftest(lmc_threatF_interact, lmc_threatF_interact_vcov))
comp_mod <- lm(settle_binary ~  factor(group_edu) + men0 + men25+ men75 + men100 + lmc_threat, weights = ps_weights, data = datw[])
lmtest::waldtest(lmc_threatF_interact, comp_mod, vcov = lmc_threatF_interact_vcov, test = "F")

# Table D.1 (F stats and P-values estiamted above, added in manually in Tex.  Variable labels also added in Tex)
### table SI 3.3
texreg(list(
  lmc_threatA_interact,
  lmc_threatB_interact,
  lmc_threatC_interact,
  lmc_threatD_interact,
  lmc_threatE_interact,
  lmc_threatF_interact),
  override.se = list(
    lmc_threatA_coef[,2],
    lmc_threatB_coef[,2],
    lmc_threatC_coef[,2],
    lmc_threatD_coef[,2],
    lmc_threatE_coef[,2],
    lmc_threatF_coef[,2]),
  override.pvalues = list(
    lmc_threatA_coef[,4],
    lmc_threatB_coef[,4],
    lmc_threatC_coef[,4],
    lmc_threatD_coef[,4],
    lmc_threatE_coef[,4],
    lmc_threatF_coef[,4]),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'lmc_threatTRUE' = 'High LMC Threat',
    'men0' = "0%",
    'men0:lmc_threatTRUE'= "0% * High LMC Threat",
    'men25' = "25%",   
    'lmc_threatTRUE:men25' = "25% * High LMC Threat",
    'men75' = "75%",  
    'lmc_threatTRUE:men75' = "75% * High LMC Threat",
    'men100' = "100%",  
    'lmc_threatTRUE:men100' = "100% * High LMC Threat",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

# Table E.1, Model 1
mod2550 <- lm(settle_binary ~ factor(group_edu) + men25, data = datw[men25 == T | men50 == T])
mod2550_vcov <- cluster.vcov(mod2550, datw[men25 == T | men50 == T, id])
(mod2550_coef <- coeftest(mod2550, vcov = mod2550_vcov))

#Table E.1, Model 2
datw[ , taskid := paste(id, task_number)]
datw[ , same := sum(men25) == 1 & sum(men50) == 1, by = taskid]

mod2550_same <- lm(settle_binary ~ factor(group_edu) + men25, data = datw[same == T])
mod2550_same_vcov <- cluster.vcov(mod2550_same, datw[same == T, "id"])
(mod2550_same_coef <- coeftest(mod2550_same, vcov = mod2550_same_vcov))

# Table E.1, Model 3
mod2550_same_first <- lm(settle_binary ~ factor(group_edu) + men25, data = datw[same == T & task_number == 1])
mod2550_same_first_vcov <- cluster.vcov(mod2550_same_first, datw[same == T & task_number == 1, "id"])
(mod2550_same_first_coef <- coeftest(mod2550_same_first, vcov = mod2550_same_first_vcov))

# Table E.1 (Variable labes added in Tex)
texreg(
  list(
    mod2550,
    mod2550_same,
    mod2550_same_first
  ),
  override.se = list(
    mod2550_coef[,2],
    mod2550_same_coef[,2],
    mod2550_same_first_coef[,2]
  ),
  override.pvalues = list(
    mod2550_coef[,4],
    mod2550_same_coef[,4],
    mod2550_same_first_coef[,4]
  ),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men25' = "25%",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

# Table F.1, Model 1
mturk <- fread("MTurkPilot.csv") #read in pilot data

mturk_mod <-  lm(settle_binary ~  men0 + men25 + men75 + men100 + factor(group_edu) + work1 + work3 + work5 + factor(group_eng), data = mturk)
mturk_vcov <- cluster.vcov(mturk_mod, mturk[,id])
(mturk_coef <- coeftest(mturk_mod, mturk_vcov))

# Table F.1 (Add variable labels in Tex)
texreg(
  list(mturk_mod),
  override.se = mturk_coef[,2],
  override.pvalues = mturk_coef[,4],
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men0' = "0%",
    'men25' = "25%",
    'men75' = "75%",
    'men100' = "100%",
    'factor(group_edu)10%' = "10%",
    'factor(group_edu)20%' = "20%",
    'factor(group_edu)30%' = "30%",
    'work1' = "1 year",
    'work3' = "3 years",
    'work5' = "5 years",
    'factor(group_eng)10%' = "10%a",
    'factor(group_eng)20%' = "20%a",
    'factor(group_eng)30%' = "30%a"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)


# Footnote 1, Appendix A (robustness to alternative handling of 3 double-interviewed respondents)

# create indicator for three double interviewed respondents
# change "datw" to "data" to do these robustness tests on unweighted data.
datw[ , double_interview := .N == 16, by = id] 
# .N is the number of choice tasks completed by each respondent.
# One completeion of the survey is 8 tasks, two completions is 16 taks.


# ROBUSTNESS 1: refit main model excluding these three respondents

# ND for "No Double-Interviewed"
mainND <- lm(settle_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[double_interview == F])
mainND_vcov <- cluster.vcov(mainND, datw[double_interview == F , "id"])
(mainND_coefs <- coeftest(mainND, mainND_vcov))

# ROBUSTNESS 2: refit main model with separate clusters for each of these three individuals' interviews. 
datw[ , id2 := paste0(id,duration_seconds)] # use interview length to differntiate the double interviews.

# SC for "Separate Clusters"
mainSC <- lm(settle_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[])
mainSC_vcov <- cluster.vcov(mainSC, datw[, "id2"])
(mainSC_coefs <- coeftest(mainSC, mainSC_vcov))

# ROBUSTNESS 3: refit main model but only using first interview
datw[double_interview == T, .(unique(startDate), unique(duration_seconds)), keyby = id] # just to show different start dates and durations for the double-interview

# (Respondent-id 767 started both interviews at the same time. 
# I treat his interivew with the shorter duration as his first inteview,
# but this is robust to treating the other interview as the first interview)

# create an indicator for first interview, 
# and then make this false for second interviews 
# (based on info from code displayed above)
datw[ , first_interview := T]
datw[id == 591 & duration_seconds == 107, first_interview := F]
datw[id == 767 & duration_seconds  == 559, first_interview := F] #change "duration_seconds ==" to "duration_seconds !=" to treat this respondent's other interview as his first.
datw[id == 1275 & duration_seconds == 716, first_interview := F]

# FI for "first interview"
mainFI <- lm(settle_binary ~ factor(group_edu) + men0 + men25 + men75 + men100, weights = ps_weights, data = datw[first_interview == T])
mainFI_vcov <- cluster.vcov(mainFI, datw[first_interview == T , "id"])
(mainFI_coefs <- coeftest(mainFI, mainFI_vcov))

# code to make a Tex table of these robustness checks and the main result. 
# Note the changes in num. obs. resulting from excluding double-interivew respondents/second interviews.
texreg(list(
  main,
  mainND,
  mainSC,
  mainFI),
  override.se = list(
    main_coefs[,2],
    mainND_coefs[,2],
    mainSC_coefs[,2],
    mainFI_coefs[,2]),
  override.pvalues = list(
    main_coefs[,4],
    mainND_coefs[,4],
    mainSC_coefs[,4],
    mainFI_coefs[,4]),
  custom.coef.map = list(
    '(Intercept)' = "Intercept",
    'men0' = "0%",
    'men25' = "25%",
    'men75' = "75%",
    'men100' = "100%",
    'factor(group_edu)10' = "10%",
    'factor(group_edu)20' = "20%",
    'factor(group_edu)30' = "30%"
  ),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3
)

