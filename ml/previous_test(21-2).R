# 1유형 문제 1
library(mlbench)
library(dplyr)
data("BostonHousing")
data <- BostonHousing
head(data)
top10 <- data %>%
  arrange(desc(crim))
top10 <- head(top10$crim, 10)
tenth <- top10[10]
data$crim <- ifelse(data$crim >= tenth, tenth, data$crim)
print(mean(data[data$age >= 80, "crim"]))


# 1유형 문제 2 Boston 데이터가 기출과 다름
library(MASS)
data <- Boston
train_idx <- c(1:(nrow(data) * 0.8))
train <- data[train_idx, ]
test <- data[-train_idx, ]
na_idx <- sample(c(1:nrow(train)), size = 30)
train[na_idx, "rm"] <- NA
train
before_sd <- sd(train$rm, na.rm = TRUE)
before_sd
summary(train)
train[is.na(train$rm), "rm"] <- median(train$rm, na.rm = TRUE)
summary(train)
after_sd <- sd(train$rm)
after_sd
print(abs(after_sd - before_sd))


# 1유형 문제 3
library(dplyr)
data <- read.csv('21-2_3_insurance.csv')
mean_data <- mean(data$charges)
sd_data <- sd(data$charges)
print(mean_data)
print(sd_data)
outlier <- data %>%
  filter(charges >= mean_data + 1.5 * sd_data | charges <= mean_data - 1.5 * sd_data)
nrow(data)
nrow(outlier)
sum(outlier$charges)


# 2유형 문제 1
data <- read.csv('21-2_4_customer_analytics.csv')
head(data)
colnames(data)[1] <- "ID"
head(data)
summary(data)
str(data)
train_idx <- sample(1:nrow(data), size = 0.8 * nrow(data))
train <- data[train_idx, colnames(data)[-1]]
test <- data[-train_idx, colnames(data)[-1]]
full_model <- glm(Reached.on.Time_Y.N ~ ., family = gaussian, data = train)
summary(full_model)
second_model <- step(full_model, direction = "both")
step_formula <- formula(second_model)
step_model <- glm(step_formula, family = gaussian, data = train)
summary(step_model)
pred_step <- predict(step_model, test[-11])
pred_step <- ifelse(pred_step >= 0.5, 1, 0)
pred_step
library(caret)
confusionMatrix(as.factor(pred_step), as.factor(test$Reached.on.Time_Y.N),
                positive = '1') # 64.59%
summary(full_model)
select_model <- glm(Reached.on.Time_Y.N ~ Customer_care_calls + Prior_purchases + 
                      Product_importance + Discount_offered + Weight_in_gms,
                    family = gaussian, data = train)
summary(select_model)
pred_select <- predict(select_model, test[-11])
pred_select <- ifelse(pred_select >= 0.5, 1, 0)
confusionMatrix(as.factor(pred_select), as.factor(test$Reached.on.Time_Y.N),
                positive = '1') # 64.27%
df_pred <- as.data.frame(pred_step)
write.csv(df_pred, 'test.csv')
test_df <- read.csv('test.csv')
head(test_df)

# 2유형 문제 1 정답
md <- lm(Reached.on.Time_Y.N ~ ., train)
step_formula <- step(md, direction = "both")
step_formula <- formula(step_formula)
md_step <- lm(step_formula, data = train)
md_pred <- predict(md_step, newdata = test[-11])
md_pred <- ifelse(pred_step >= 0.5, 1, 0)
confusionMatrix(as.factor(md_pred), as.factor(test$Reached.on.Time_Y.N),
                positive = '1') # 64.59%
