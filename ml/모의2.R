# 1 연관분석 -> 연관 규칙 분석 / 장바구니 분석
# 2 파라미터 -> (매개변수)
# 3 비지도 학습 -> 준지도 학습
# 4 데이터 웨어하우스 -> 데이터 레이크
# 5 ? -> LOD(Linked Open Data)
# 6 ETL
# 7 K-means -> (K-means 군집화)
# 8 기술 통계
# 9 ?? -> 데이터 프로파일링
# 10 앙상블 -> (앙상블 기법)


# 11 답지 오류
library(ISLR)
data(Carseats)
ds <- Carseats
summary(ds)
sales_mean <- mean(ds$Sales)
sales_sd <- sd(ds$Sales)
out_down <- sales_mean - 1.5 * sales_sd
out_up <- sales_mean + 1.5 * sales_sd
library(dplyr)
ds %>%
  filter(Sales >= out_down, Sales <= out_up) %>%
  summarise(sd(Age))


# 12
library(MASS)
data(Cars93)
ds <- Cars93
before_mean <- mean(ds$Luggage.room, na.rm = TRUE)
ds$Luggage.room <- ifelse(is.na(ds$Luggage.room),
                          median(ds$Luggage.room, na.rm = TRUE), ds$Luggage.room)
after_mean <- mean(ds$Luggage.room)
print(abs(after_mean - before_mean))


# 13
ds <- read.csv('mt_13_timeage.csv')
library(dplyr)
mean20 <- ds %>%
  filter(age == '20s') %>%
  summarise(mean(confirmed))
mean50 <- ds %>%
  filter(age == '50s') %>%
  summarise(mean(confirmed))
print(abs(mean50 - mean20))


# 14
ds <- read.csv('mt_14_loan.csv')
head(ds)
summary(ds)
str(ds)
ds$loan_status <- as.factor(ds$loan_status)
ds$past_due_days <- NULL
library(caret)
train_idx <- createDataPartition(ds$loan_status, times = 1, p = 0.7)
train <- ds[train_idx$Resample1, ]
test <- ds[-train_idx$Resample1, ]
library(randomForest)
md.rf <- randomForest(loan_status ~ . - Loan_ID, train)
pred_rf <- predict(md.rf, test[-2], type = "class")
confusionMatrix(pred_rf, test$loan_status) # 76%
write.csv(pred_rf, 'test.csv')

# 14 답안 반영
ds <- read.csv('mt_14_loan.csv')
head(ds)
summary(ds)
str(ds)
ds$past_due_days <- NULL
ds$loan_status <- ifelse(ds$loan_status == "PAIDOFF", "Success", "Failure")
ds$loan_status <- as.factor(ds$loan_status)
library(caret)
train_idx <- createDataPartition(ds$loan_status, times = 1, p = 0.7)
train <- ds[train_idx$Resample1, ]
test <- ds[-train_idx$Resample1, ]
library(randomForest)
md.rf <- randomForest(loan_status ~ . - Loan_ID, train)
pred_rf <- predict(md.rf, test[-2], type = "class")
confusionMatrix(pred_rf, test$loan_status, positive = "Success") # 70.7%
write.csv(pred_rf, 'test.csv')
