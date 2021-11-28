# 배깅
library(mlbench)
data("PimaIndiansDiabetes2")
str(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
Pima2 <- na.omit(PimaIndiansDiabetes2)
summary(Pima2)
train_idx <- sample(1:nrow(Pima2), size = 2/3 * nrow(Pima2))
train <- Pima2[train_idx, ]
test <- Pima2[-train_idx, ]
library(ipred)
md.bagging <- bagging(diabetes ~ ., data = train, nbagg = 25)
md.bagging
pred <- predict(md.bagging, test)
library(caret)
pred
confusionMatrix(as.factor(pred), reference = test$diabetes, positive = "pos")


# 부스팅
library(xgboost)
train.label <- as.integer(train$diabetes) - 1
mat_train.data <- as.matrix(train[, -9])
mat_test.data <- as.matrix(test[, -9])
xgb.train <- xgb.DMatrix(data = mat_train.data, label = train.label)
xgb.test <- xgb.DMatrix(data = mat_test.data)
param_list <- list(booster = "gbtree", eta = 0.0001, max_depth = 10,
                   gamma = 5, subsample = 0.8, objective = "binary:logistic",
                   eval_metric = "auc")
md.xgb <- xgb.train(params = param_list, data = xgb.train,
                    nrounds = 200, early_stopping_rounds = 10,
                    watchlist = list(val1 = xgb.train), verbose = 1)
xgb.pred <- predict(md.xgb, newdata = xgb.test)
xgb.pred2 <- ifelse(xgb.pred >= 0.5, xgb.pred <- "pos", xgb.pred <- "neg")
xgb.pred2 <- as.factor(xgb.pred2)
confusionMatrix(xgb.pred2, reference = test$diabetes, positive = "pos")


# 랜덤 포레스트
library(randomForest)
md.rf <- randomForest(diabetes ~ ., data = train, ntree = 100, proxmity = TRUE)
print(md.rf)
importance(md.rf)
pred <- predict(md.rf, newdata = test)
confusionMatrix(as.factor(pred), test$diabetes, positive = "pos")
