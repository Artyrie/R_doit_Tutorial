#의사결정나무
library(rpart)
str(iris)
head(iris)
summary(iris)

md <- rpart(Species ~., data = iris)
md
plot(md, compress = TRUE, margin = 0.5)
text(md, cex = 1)

library(rpart.plot)
prp(md, type = 2, extra = 2)
ls(md)
md$cptable
plotcp(md)

tree_pred <- predict(md, newdata = iris, type = "class")
confusionMatrix(tree_pred, reference = iris$Species)

# SVM
library(e1071)
model <- svm(Species ~ ., data = iris)
model
pred <- predict(model, iris)
confusionMatrix(data = pred, reference = iris$Species)

# KNN
library(class)
data <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]
set.seed(42)
idx <- sample(x = c("train", "valid", "test"),
              size = nrow(data), replace = TRUE, prob = c(3, 1, 1))
train <- data[idx == "train", ]
valid <- data[idx == "valid", ]
test <- data[idx == "test", ]
train_x <- train[, -3]
valid_x <- valid[, -3]
test_x <- test[, -3]
train_y <- train[, 3]
valid_y <- valid[, 3]
test_y <- test[, 3]

knn_1 <- knn(train = train_x, test = valid_x, cl = train_y, k = 1)

accuracy_k <- NULL
for(i in c(1:nrow(train_x))) {
  set.seed(42)
  knn_k <- knn(train = train_x, test = valid_x, cl = train_y, k = i)
  accuracy_k <- c(accuracy_k, sum(knn_k == valid_y) / length(valid_y))
}
valid_k <- data.frame(k = c(1:nrow(train_x)), accuracy = accuracy_k)
plot(formula = accuracy ~ k, data = valid_k, type = 'o', pch = 20)
min(valid_k[valid_k$accuracy %in% max(accuracy_k), "k"])
max(accuracy_k)
knn_2 <- knn(train = train_x, test = test_x, cl = train_y, k = 2)
confusionMatrix(knn_2, reference = test_y)
