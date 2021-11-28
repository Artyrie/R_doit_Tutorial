# 주성분 분석
iris_pca <- princomp(iris[, -5], cor = FALSE, scores = TRUE)
summary(iris_pca)
plot(iris_pca, type = 'l', main = 'iris scree plot')

iris_pca$loadings
iris_pca$scores
biplot(iris_pca, scale = 0, main = 'iris biplot')

# 단순 선형 회귀
lm_hitters <- lm(Salary ~ PutOuts, data = Hitters)
summary(lm_hitters)

# 다중 선형 회귀
hitters2 <- na.omit(Hitters)
full_model <- lm(Salary ~ ., data = hitters2)
summary(full_model)
first_model <- lm(Salary ~ AtBat + Hits + Walks + CWalks + Division + PutOuts,
                  data = hitters2)
fit_model <- step(first_model, direction = "backward")
vif(fit_model) # 다중 공산성 확인 / library car
second_model <- lm(Salary ~ Hits + CWalks + Division + PutOuts,
                   data = hitters2)
vif(second_model) # 10이 넘으면 다중 공선성 문제가 있다고 볼 수 있고, 높은 값부터 제거
summary(second_model) # R^2 = 0.4326


# 로지스틱 회귀
library(ISLR)
str(Default)
head(Default)
summary(Default)
bankruptcy <- Default
set.seed(42)
train_idx <- sample(1:nrow(bankruptcy), size = 0.8 * nrow(bankruptcy),
                                                          replace = FALSE)
test_idx <- (-train_idx)
bankruptcy_train <- bankruptcy[train_idx,]
bankruptcy_test <- bankruptcy[test_idx,]
full_model <- glm(default ~ ., family = binomial, data = bankruptcy_train)
step_model <- step(full_model, direction = "both")
summary(step_model)
null_deviance <- 2354.0
residual_deviance <- 1287.4
model_deviance <- null_deviance - residual_deviance
pchisq(model_deviance, df = 2, lower.tail = FALSE)
vif(step_model)
pred <- predict(step_model, newdata = bankruptcy_test[, -1],
                type = "response")
df_pred <- as.data.frame(pred)
df_pred$default <- ifelse(df_pred$pred >= 0.5,
                          df_pred$default <- "Yes", df_pred$default <- "No")
df_pred$default <- as.factor(df_pred$default)
head(df_pred)
confusionMatrix(data = df_pred$default,
                reference = bankruptcy_test[,1])
library(ModelMetrics)
auc(actual = bankruptcy_test[,1], predicted = df_pred$default)


# practice 1
hitter_non_norm <- Hitters %>%
  select(-League, -Division, -NewLeague)
hitter_non_na <- hitter_non_norm %>%
  filter(complete.cases(hitter_non_norm))
# PCA
pca_hitters <- princomp(hitteR_non_na)
summary(pca_hitters)
biplot(pca_hitters, scale = 0)
lm_pca <- lm(Salary ~ CAtBat, Hitters) # 제 1성분 선택
summary(lm_pca) # R^2 = 0.2768
# Feature select
m1 <- lm(Salary ~., data = hitter_non_na)
m2 <- step(m1, direction = "both")
hitter_form <- formula(m2)
lm_featuring <- lm(hitter_form, hitter_non_na)
summary(lm_featuring) # R^2 = 0.5255
vif(lm_featuring)
lm_featuring2 <- lm(Salary ~ Hits + Walks + CRBI +
                      CWalks + PutOuts + Assists, data = hitter_non_na)
summary(lm_featuring2) # R^2 = 0.463

