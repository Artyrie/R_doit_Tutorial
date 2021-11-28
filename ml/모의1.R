# 1. 크롤링
# 2. 메타데이터
# 3. 차원의 저주
# 4. 요인 분석?
# 5. 랜덤포레스트? -> CART
# 6. 단순대치법 -> 단순 확률 대치법
# 7. 관계 그래프 -> 분포 시각화
# 8. 등분산성
# 9. 단계적 방법 -> 단계적 선택법
# 10. 곡선 -> 엘보우 기법

# 11
ds <- BostonHousing
top50 <- head(sort(ds$medv, decreasing = TRUE), 50)
fifth <- min(top50)
ds$medv <- ifelse(ds$medv >= fifth, fifth, ds$medv)
library(dplyr)
ds %>%
  filter(crim >= 1) %>%
  summarise(mean(crim)) # 문제에 mean 구하라는 말이 없었음음


# 12
ds <- iris
train_idx <- sample(1:nrow(ds), size = 0.7 * nrow(ds))
train <- ds[train_idx, ]
test <- ds[-train_idx, ]
sd(train$Sepal.Length)


# 13
ds <- mtcars
ds$wt <- scale(ds$wt, center = min(ds$wt), scale = max(ds$wt) - min(ds$wt))
library(dplyr)
ds %>%
  filter(wt > 0.5) %>%
  summarise(nrow(wt))


# 14
ds <- iris
library(rpart)
md.df <- rpart(Species ~., ds)
pred_df <- predict(md.df, ds[-5], type = "class")
library(caret)
confusionMatrix(pred_df, ds$Species) # 96%
library(e1071)
md.svm <- svm(Species ~., ds)
pred_svm <- predict(md.svm, ds[-5], type = "class")
confusionMatrix(pred_svm, ds$Species) # 97.3%
write.csv(pred_svm, "test.csv")
