# 1.  ? -> CDC (Change Data Capture)
# 2. 강화 학습
# 3. 모멘텀
# 4. 관계 시각화 -> (다차원 척도법)
# 5. 랜덤 포레스트
# 6. ? -> 데이터 비식별화
# 7. 임베디드 기법 
# 8. ? -> k-익명성
# 9. DBSCAN
# 10. ROC -> 이익 도표


# 11
ds <- esoph
library(dplyr)
ds2 <- ds %>%
  mutate(nsums = ncases + ncontrols)
nsums_table <- xtabs(nsums ~ alcgp + tobgp, ds2)
chi <- chisq.test(nsums_table)
result <- chi$statistic
result
# ???
  
  
# 12
library(MASS)
data("ChickWeight")
ds <- ChickWeight
head(ds)
scaled_ds <- ds
scaled_ds$weight <- scale(scaled_ds$weight,
                          center = min(scaled_ds$weight),
                          scale = max(scaled_ds$weight) - min(scaled_ds$weight))
library(dplyr)
scaled_ds %>%
  filter(weight >= 0.5) %>%
  nrow()


# 13
library(mlbench)
data("PimaIndiansDiabetes2")
ds <- PimaIndiansDiabetes2
library(dplyr)
ds2 <- ds %>%
  filter(!is.na(glucose) & !is.na(pressure) & !is.na(mass))
colSums(is.na(ds2))
ds2 <- ds %>%
  mutate(group = ifelse(age >= 60, 3,
                        ifelse(age >= 41, 2, 1)))
ds2_1 <- ds2[ds2$group == 1, ]
ds2_2 <- ds2[ds2$group == 2, ]
ds2_3 <- ds2[ds2$group == 3, ]
nrow(ds2_1[ds2_1$diabetes == "pos",  ]) / nrow(ds2_1)
nrow(ds2_2[ds2_2$diabetes == "pos",  ]) / nrow(ds2_2)
nrow(ds2_3[ds2_3$diabetes == "pos",  ]) / nrow(ds2_3)
print(nrow(ds2_2[ds2_2$diabetes == "pos",  ]) / nrow(ds2_2))


# 14
ds <- read.csv('21-2_3_insurance.csv')
head(ds)
library(randomForest)
train_idx <- sample(1:nrow(ds), size = 0.8 * nrow(ds))
train <- ds[train_idx, ]
test <- ds[-train_idx, ]
md.rf <- randomForest(charges ~ ., train, ntree = 300, do.trace = TRUE)
pred_rf <- predict(md.rf, test[-7])
library(ModelMetrics)
rmse(test$charges, pred_rf)
library(nnet)
md.nn_full <- nnet(charges ~., train, size = 3, maxit = 200)
pred_nn <- predict(md.nn_full, test[-7])
rmse(test$charges, pred_nn)
md.lm_full <- lm(charges ~ ., train)
md.lm_step <- step(md.lm_full, direction = "both")
step_formula <- formula(md.lm_step)
md.lm <- lm(step_formula, train)
pred_lm <- predict(md.lm, test[-7])
rmse(test$charges, pred_lm) # 4900
library(xgboost)
xgb_ds <- ds
xgb_ds$sex <- ifelse(xgb_ds$sex == "male", 1, 0)
xgb_ds$smoker <- ifelse(xgb_ds$smoker == "yes", 1, 0)
xgb_ds$region <- ifelse(xgb_ds$region == "southeast", 0, 
                        ifelse(xgb_ds$region == "southwest", 1,
                               ifelse(xgb_ds$region == "northeast", 2, 3)))
head(xgb_ds)
train2 <- xgb_ds[train_idx, ]
test2 <- xgb_ds[-train_idx, ]
mat_train <- as.matrix(train2[-7])
mat_test <- as.matrix(test2[-7])
xgb_train <- xgb.DMatrix(mat_train, label = train2$charges)
xgb_test <- xgb.DMatrix(mat_test)
param_list <- list(booster = "gbtree", eta = 0.0001, depth = 10,
                   sumsample = 0.8, verbose = 1)
md.xgb <- xgb.train(params = param_list, data = xgb_train, nrounds = 200,
                    early_stopping_rounds = 10, watchlist = list(val1 = xgb_train))
xgb.pred <- predict(md.xgb, xgb_test)
rmse(test$charges, xgb.pred)

ds <- read.csv('21-2_3_insurance.csv')
head(ds)
library(caret)
scale_model <- preProcess(ds, method = "range")
scale_ds <- predict(scale_model, ds)
scale_ds$sex <- as.factor(scale_ds$sex)
levels(scale_ds$sex) <- c("F", "M")
scale_ds$smoker <- as.factor(scale_ds$smoker)
levels(scale_ds$smoker) <- c("N", "Y")
scale_ds$region <- as.factor(scale_ds$region)
levels(scale_ds$region) <- c("NE", "NW", "SE", "SW")
train_idx <- sample(1:nrow(ds), size = 0.8 * nrow(ds))
train <- scale_ds[train_idx, ]
test <- scale_ds[-train_idx, ]
md.rf <- randomForest(charges ~ ., train, ntree = 300, do.trace = TRUE)
pred_rf <- predict(md.rf, test[-7])
library(ModelMetrics)
rmse(test$charges, pred_rf) # 5300
trControl <- trainControl(method = "cv", number = 5)
metric <- "RMSE"
set.seed(42)
head(train)
md.rf <- train(x = train)