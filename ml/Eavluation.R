# RMSE, MSE (회귀 모형 평가)
library(ISLR)
hitters <- na.omit(Hitters)
fit_model <- lm(Salary ~ AtBat + Hits + CWalks + Division + PutOuts, data = hitters)
second_model <- lm(Salary ~ Hits + CWalks + Division + PutOuts, data = hitters)
library(ModelMetrics)
rmse(fit_model)
mse(fit_model)
rmse(second_model)
mse(second_model)
summary(fit_model)$r.squared
summary(fit_model)$adj.r.squared
summary(second_model)$r.squared
summary(second_model)$adj.r.squared


# 혼동 행렬 (분류 모형 평가)
library(mlbench)
data("PimaIndiansDiabetes2")
df_pima <- na.omit(PimaIndiansDiabetes2)
set.seed(42)
train.idx <- sample(1:nrow(df_pima), size = nrow(df_pima) * 0.8)
train <- df_pima[train.idx, ]
test <- df_pima[-train.idx, ]
library(randomForest)
set.seed(42)
md.rf <- randomForest(diabetes ~., data = train, ntree = 300)
pred <- predict(md.rf, newdata = test)
library(caret)
confusionMatrix(as.factor(pred), test$diabetes, positive = "pos")


# AUC
library(mlbench)
data("PimaIndiansDiabetes2")
df_pima <- na.omit(PimaIndiansDiabetes2)
set.seed(42)
train.idx <- sample(1:nrow(df_pima), size = nrow(df_pima) * 0.8)
train <- df_pima[train.idx, ]
test <- df_pima[-train.idx, ]
library(randomForest)
set.seed(42)
md.rf <- randomForest(diabetes ~., data = train, ntree = 300)
pred <- predict(md.rf, newdata = test)
library(ModelMetrics)
auc(actual = test$diabetes, predicted = as.factor(pred))
