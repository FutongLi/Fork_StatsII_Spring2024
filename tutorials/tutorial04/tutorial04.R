#######################
# Stats 2: tutorial 4 #
#######################

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
# ‘car’ = Companion to Applied Regression

lapply(c('tidyverse', 'car'),  pkgTest)

## More on logits: visualising and goodness of fit

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# 1. This time, let's analyse the data in more detail. Run some checks to see if 
#    the data are well distributed. Try some simple plots to get an idea of the 
#    relationship between variables. Drop those errors too.
str(graduation)

class(graduation)

xtabs(~hsgrad + nonwhite, data = graduation) # with(graduation, table(hsgrad, nonwhite))
xtabs(~hsgrad + intact, data = graduation)

summary(graduation)


# 2. Last week we created a kitchen sink model, with nsibs as a binned factor. 
# a kitchen sink model includes everything in it
#    Here was the code:
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf),  #分段的断点设置为 0、0.9、1、3 和正无穷。
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))
graduation <- graduation[-which(graduation$nsibs < 0),]

# remove 'nisbs' column
data = graduation[,!names(graduation) %in% c("nsibs")]




mod_1 <- glm(hsgrad ~., 
             data = data, 
             family = "binomial")

summary(mod_1)
# Create a more parsimonious model of your own choice. Select three predictor 
# variables, run the regression, and check with summary.

mod_2 <- glm(hsgrad ~ nonwhite * intact,
             data = graduation,
             family = 'binomial') #your model

summary(mod_2)

# 3. a) Create a new data frame comprising the outcome variable and two columns 
#       of fitted values, one from mod_1 and another from mod_2. 

df <- data.frame(
  hsgrad = data$hsgrad,
  est_mod_1 = mod_1$fitted.values, # how come 1641 obs
  est_mod_2 = mod_2$fitted.values # 1643 obs
)


# 3. b) Create a pipe (without reassigning) whereby you reorder your new 
#       dataframe according to the fitted values of mod_1, create a new rank 
#       variable, then create a scatterplot of rank by fitted value, 
#       colored by the outcome variable.

df_rank <- df %>% 
  order(df$est_mod_1) %>%
  rank(df$est_mod_1)

df_new <- df %>%
  arrange(est_mod_1) %>%
  mutate(rank = row_number())
  

ggplot(df_new, aes(rank, est_mod_1)) +
  geom_point(aes(colour = hsgrad), alpha = 0.5) +
  scale_y_continuous(limits = c(0,1))
# 3. c) Do the same for mod_2. Compare the results.

df %>%
  arrange(est_mod_2) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, est_mod_2)) +
    geom_point(aes(colour = hsgrad), alpha = 0.5) +
    scale_y_continuous(limits = c(0,1))

# 4. Calculate McFadden's Pseudo R squared for both models
# 用于衡量二项逻辑回归模型对观察数据的拟合程度. 
#    Which model explains more variance?
#    What are the p values?

#Null deviance（零偏差）是用来评估拟合模型的一种指标。
#它衡量了拟合模型相对于一个只包含截距项的模型的拟合优度。
#Null deviance的值越小，表示拟合模型相对于只包含截距项的模型的拟合程度越好。
mod_1$null.deviance == mod_2$null.deviance


mod_null <- glm(hsgrad~1,
                data = graduation,
                family = 'binomial')
summary(mod_null)


1-logLik(mod_1)/logLik(mod_null)
1-logLik(mod_2)/logLik(mod_null)

ll.null <- mod_1$null.deviance/-2
ll.fit_1 <- mod_1$deviance/-2
ll.fit_2 <- mod_2$deviance/-2

#我们通常感兴趣的是模型改进是否显著，而不是模型改进的程度
1 - pchisq(2*(ll.fit_1 - ll.null), df = (length(mod_1$coefficients)-1))
1 - pchisq(2*(ll.fit_2 - ll.null), df = (length(mod_2$coefficients)-1))
