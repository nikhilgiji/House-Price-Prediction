library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(leaps)
library(rpart)
library(mgcv)
library(glmnet)
library(boot)
library(rpart.plot)
#loading data
data(Boston)
dim(Boston)
#a look at first few rows
head(Boston)
#a look at structure of the data set
glimpse(Boston)
#summary statistics
summary(Boston)
#Check for missing values
sum(is.na(Boston))
#Check for duplicated values
sum(duplicated(Boston))
#checking correlation between variables
corrplot(cor(Boston), method = "number", type = "upper", diag = FALSE)
Boston %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of dependent variables vs Median Value (medv)") 
table(Boston$chas)
set.seed(12383010)
index <- sample(nrow(Boston), nrow(Boston) * 0.80)
Boston.train <- Boston[index, ]
Boston.test <- Boston[-index, ]
model1 <- lm(medv ~ ., data = Boston.train)
model1.sum <- summary(model1)
model1.sum
#Looking at model summary, we see that variables indus and age are insignificant
#Building model without variables indus and age
model2 <- lm(medv ~ . -indus -age, data = Boston.train)
model2.sum <- summary(model2)
model2.sum
model.subset <- regsubsets(medv ~ ., data = Boston.train, nbest = 1, nvmax = 13)
summary(model.subset)
plot(model.subset, scale = "bic")
#Variable selection using stepwise regression
nullmodel <- lm(medv ~ 1, data = Boston.train)
fullmodel <- lm(medv ~ ., data = Boston.train)

#forward selection
model.step.f <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward")
#Backward selection
model.step.b <- step(fullmodel, direction = "backward")
AIC(model.step)
BIC(model.step)
summary(model.step)
#Model Diagnostics for model 2
par(mfrow = c(2,2))
plot(model.step)
par(mfrow = c(1,1))
#In-sample performance
#MSE
model.sum <- summary(model1)
(model.sum$sigma) ^ 2
