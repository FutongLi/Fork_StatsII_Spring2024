##############
# Tutorial 5 Ordered and Multinomial Logistic Regression #
##############

library(MASS)
#  **MASS**：这是一个R语言的包，提供了一些统计分析和机器学习方面的函数和数据集。
# 它包含了许多统计学家使用的函数和经典的数据集。
# 例如，MASS包含了用于线性模型拟合的lm()函数，以及一些数据集如Boston和USArrests等。

library(nnet)
# 2. **nnet**：这也是一个R语言的包，用于实现神经网络相关的功能。
# 其中的函数可用于训练多层神经网络，执行分类和回归等任务。

library(ggplot2)

# EDA
summary(workingMoms)
ftable(xtabs(~ gender + year + attitude, data = workingMoms))

# do some wrangling
workingMoms$attitude <- factor(workingMoms$attitude, 
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))

ftable(xtabs(~gender + year + attitude, data = workingMoms))

workingMoms$gender <- as.factor(workingMoms$gender)

workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))

workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

ftable(xtabs(~ gender + year + attitude, data = workingMoms))

ggplot(workingMoms, aes(attitude, prestige)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(gender ~ year)



# a) Perform an ordered (proportional odds) logistic regression

ord.log <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(ord.log)

# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# lower.tail = FALSE 表示计算右尾概率，最后乘以 2 得到双侧检验的 p 值
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(ord.log))

# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

# How do we interpret these coefficients?






# b) fit a multinomial logit model
# set a reference level for the outcome
# rule: theory/extreme catogorical variable(avoid the one with least frequency)
workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")

# run model
mult.log <- multinom(attitude ~ ., data = workingMoms)
summary(mult.log)
exp(coef(mult.log))

# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
# coef(summary(mult.log)) /
(p <- (1 - pnorm(abs(z), 0, 1)) * 2) 
# pnorm(abs(z)这个函数的作用是计算给定 z 值对应的标准正态分布的累积概率。

# how do we interpret these coefficients?

# we can use predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))# 回归模型 mult.log 对于每个观测值的预测结果
head(data.frame(attitude = workingMoms$attitude,
                SD = pp$Strongly.Disagree,
                D = pp$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))


# c) Consider gender as an interaction
mult.log.int <- multinom(attitude ~ gender * ., data = workingMoms)
summary(mult.log.int)

z.int <- summary(mult.log.int)$coefficients/summary(mult.log.int)$standard.errors
(p.int <- (1 - pnorm(abs(z.int), 0, 1)) * 2)

pp.int <- data.frame(fitted(mult.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp.int$Strongly.Disagree,
                D = pp.int$Disagree,
                A = pp.int$Agree,
                SA = pp.int$Strongly.Agree))

