# 인공 신경망
library(nnet)
iris_data <- iris
iris_data.scaled <- cbind(scale(iris[-5]), iris[5])
set.seed(42)
idx <- createDataPartition(iris_data.scaled$Species, times = 1, 0.7)
train <- iris_data.scaled[idx$Resample1, ]
test <- iris_data.scaled[-idx$Resample1, ]
model.nnet <- nnet(Species ~ ., data = train, size = 2, maxit = 200)
summary(model.nnet)
pred <- predict(model, newdata = iris[-5])
confusionMatrix(data = pred, reference = iris$Species)


# 나이브 베이즈 기법
library(e1071)
train_data <- sample(1:150, size = 100)
naive_model <- naiveBayes(Species ~ ., data = iris, subset = train_data)
naive_model # [,1]은 평균, [,2]는 표준편차
pred <- predict(naive_model, newdata = iris)
confusionMatrix(pred, reference = iris$Species)