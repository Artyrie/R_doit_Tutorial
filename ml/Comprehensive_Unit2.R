# 2 유형


# pr1 다시 풀기
ds <- read.csv('cu_6_telco.csv')
str(ds)
head(ds)
summary(ds)
library(dplyr)
ds$TotalCharges <- ifelse(is.na(ds$TotalCharges), 
                          mean(ds$TotalCharges, na.rm = TRUE), ds$TotalCharges)
ds$Churn <- as.factor(ds$Churn)
summary(ds)
library(caret)
data_idx <- createDataPartition(ds$Churn, times = 1, p = 0.8)
data <- ds[data_idx$Resample1, ]
test <- ds[-data_idx$Resample1, ]
train_idx <- createDataPartition(data$Churn, times = 1, p = 0.8)
train <- data[train_idx$Resample1, ]
valid <- data[-train_idx$Resample1, ]
scale_model <- preProcess(train, method = "range")
scaled_train <- predict(scale_model, train)
scaled_valid <- predict(scale_model, valid)
scaled_test <- predict(scale_model, test)

library(randomForest)
md.rf <- randomForest(scaled_train$Churn ~. - customerID,
                      scaled_train, ntree = 300, do.trace = TRUE)
# do.trace 옵션을 통해 OOB에 대한 감소량을 확인하여 ntree값을 조정할 수 있음 
predict_rf <- predict(md.rf, newdata = scaled_valid)
confusionMatrix(valid$Churn, predict_rf)

library(e1071)
md.svm <- svm(scaled_train$Churn ~. - customerID, scaled_train)
# 훈련에 customerID 들어가면 predict에서 long에러
predict_svm <- predict(md.svm, newdata = scaled_valid)
# type response는 확률 출력
confusionMatrix(valid$Churn, predict_svm)


# pr2
ds <- mtcars
train_idx <- sample(1:nrow(ds), size = 0.7 * nrow(ds))
train <- ds[train_idx, ]
test <- ds[-train_idx, ]
library(randomForest)
md.rf <- randomForest(train$mpg ~. , train, ntree = 300, do.trace = TRUE)
summary(md.rf)
pred_rf <- predict(md.rf, newdata = test)
library(ModelMetrics)
rmse(actual = test$mpg, predicted = pred_rf)
md.lm <- lm(train$mpg ~., train)
summary(md.lm)
st.lm <- step(md.lm, direction = "backward")
st_fm <- formula(st.lm)
md.lm2 <- lm(st_fm, train)
summary(md.lm2)
pred_lm <- predict(md.lm2, newdata = test)
rmse(actual = test$mpg, predicted = pred_lm)