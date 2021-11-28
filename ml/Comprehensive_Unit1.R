# 1유형


# pr1
ds <- airquality
summary(ds)
ds <- ds[!is.na(ds$Solar.R), ]
ds_sd <- sd(ds$Ozone, na.rm = TRUE)
ds_median <- median(ds$Ozone, na.rm = TRUE)
ds$Ozone <- ifelse(is.na(ds$Ozone), ds_median, ds$Ozone)
ds_sd2 <- sd(ds$Ozone)
print(abs(ds_sd2 - ds_sd))


# pr2
library(ISLR)
data(Hitters)
ds <- Hitters
summary(Hitters)
ds <- ds[complete.cases(Hitters$Salary), ]
summary(ds)
out_down <- median(ds$Salary) - 2 * IQR(ds$Salary)
out_up <- median(ds$Salary) + 2 * IQR(ds$Salary)
library(dplyr)
ds %>%
  filter(Salary < out_down | Salary > out_up) %>%
  summarise(sum(Salary))


# pr3
ds <- diamonds
train <- ds[1:(nrow(ds) * 0.8), ]
sorted <- head(sort(train$price, decreasing = TRUE), 100)
print(mean(sorted))


# pr4
ds <- airquality
train <- ds[c(1:(nrow(ds) * 0.9)), ]
before_median <- median(train$Ozone, na.rm = TRUE)
train$Ozone <- ifelse(is.na(train$Ozone), mean(train$Ozone, na.rm = TRUE), train$Ozone)
after_median <- median(train$Ozone)
print(abs(after_median - before_median))


# pr5
# R은 fefault로 소수점 포함 7자리까지밖에 안나타내는 듯 함.
# 따라서 quantile로 값을 확인하여 수기로 적어 변경해야 값이 정확하게 나옴옴
ds <- read.csv('cu_4_music.csv')
head(ds)
summary(ds)
#up25 <- head(sort(ds$tempo, decreasing = TRUE), nrow(ds) * 0.25)
#up25 <- min(up25)
#down25 <- head(sort(ds$tempo), nrow(ds) * 0.25)
#down25 <- max(down25)
quantile(ds$tempo)
ds$tempo <- ifelse(ds$tempo >= 135.99918 | ds$tempo <= 99.38401, 0, ds$tempo)
print(mean(ds$tempo) + sd(ds$tempo))


# pr6
ds <- read.csv('cu_6_telco.csv')
summary(ds)
head(ds)
ds_mean <- mean(ds$TotalCharges, na.rm = TRUE)
ds_sd <- sd(ds$TotalCharges, na.rm = TRUE)
out_up <- ds_mean + 1.5 * ds_sd
out_down <- ds_mean - 1.5 * ds_sd
library(dplyr)
ds %>%
  filter(TotalCharges > out_down & TotalCharges < out_up) %>%
  summarise(mean = mean(TotalCharges, na.rm = TRUE))
  # ds$TotalCharges를 여기서 쓰면 filtering이 적용이 안된 결과가 나옴


# pr7
library(MASS)
data(cats)
ds <- cats
head(ds)
summary(ds)
ds_mean <- mean(ds$Hwt)
ds_sd <- sd(ds$Hwt)
out_down <- ds_mean - 1.5 * ds_sd
out_up <- ds_mean + 1.5 * ds_sd
ds %>%
  filter(Hwt > out_up | Hwt < out_down) %>%
  summarise(mean(Hwt))


# pr8
library(faraway)
data(orings)
ds <- orings
library(dplyr)
ds_dmg1 <- ds %>%
  filter(damage >= 1)
summary(ds_dmg1)
print(cor(ds_dmg1$temp, ds_dmg1$damage, method = "pearson"))


# pr9
ds <- mtcars
library(dplyr)
mean_manual <- ds %>%
  filter(am == 1) %>%
  arrange(wt) %>%
  summarise(mean = mean(head(mpg, 10)))
mean_auto <- ds %>%
  filter(am == 0) %>%
  arrange(wt) %>%
  summarise(mean = mean(head(mpg, 10)))
print(abs(mean_manual - mean_auto))


# pr10
ds <- diamonds
head(ds)
summary(ds)
ds_cut <- ds[c((nrow(ds) * 0.8):nrow(ds)), ]
ds_cut %>%
  filter(cut == "Fair" & carat > 1) %>%
  summarise(max = max(price))


# pr11
ds <- airquality
head(ds)
library(dplyr)
ds_821 <- ds[ds$Month == 8 & ds$Day == 21, ]
ds_821 %>%
  dplyr::select(Ozone)


# pr12
ds <- iris
print(mean(iris$Sepal.Length) + mean(iris$Sepal.Width))


# pr13
ds <- mtcars
head(ds)
prop.table(table(ds$cyl))["4"]


# pr14
ds <- mtcars
ds_cut <- ds[ds$gear == 4 & ds$am == 1, ]
print(mean(ds_cut$mpg) + sd(ds_cut$hp))


# pr15
library(mlbench)
data("BostonHousing")
ds <- BostonHousing
library(dplyr)
ds %>%
  filter(crim <= 1) %>%
  summarise(mean = mean(medv))


# pr16
ds <- iris
library(dplyr)
ds %>%
  filter(Species == 'virginica') %>%
  mutate(Len = ifelse(Sepal.Length > 6, 1, 0)) %>%
  summarise(sum = sum(Len))


# pr17
ds <- airquality
summary(ds)
ds$Ozone <- ifelse(is.na(ds$Ozone), mean(ds$Ozone, na.rm = TRUE), ds$Ozone)
out_down <- median(ds$Ozone) - 2 * IQR(ds$Ozone)
out_up <- median(ds$Ozone) + 2 * IQR(ds$Ozone)
library(dplyr)
ds %>%
  filter(Ozone > out_down & Ozone < out_up) %>%
  summarise(sum = sum(Ozone))


# pr18
ds <- read.csv('cu_18_marvel.csv')
train <- ds[ds$HAIR == 'Brown Hair' & ds$EYE == 'Brown Eyes', ]
ds_mean <- mean(train$APPEARANCES, na.rm = TRUE)
ds_sd <- sd(train$APPEARANCES, na.rm = TRUE)
out_down <- ds_mean - 1.5 * ds_sd
out_up <- ds_mean + 1.5 * ds_sd
library(dplyr)
train %>%
  filter(APPEARANCES < out_up & APPEARANCES > out_down) %>%
  summarise(mean = mean(APPEARANCES))


# pr19
library(MASS)
data("ChickWeight")
ds <- ChickWeight
head(ds)
summary(ds)
library(dplyr)
train <- ds %>%
  filter(Time == 10)
top30 <- head(sort(train$weight, decreasing = TRUE), 30)
th30 <- min(top30)
before_mean <- mean(train$weight)
train$weight <- ifelse(train$weight >= th30, before_mean, train$weight)
after_mean <- mean(train$weight)
print(abs(after_mean - before_mean))


# pr20
ds <- read.csv('cu_20_fifa_ranking.csv')
head(ds)
summary(ds)
library(dplyr)
top3 <- ds %>%
  arrange(desc(total_points)) %>%
  dplyr::select(country_abrv) %>%
  head(3)
top3 <- top3$country_abrv
ds %>%
  filter(country_abrv %in% c(top3)) %>%
  summarise(mean = mean(total_points))


# pr21
ds <- read.csv('cu_21_sales_train.csv')
head(ds)
top3 <- head(sort(table(ds$item_id), decreasing = TRUE), 3)
top3_name <- names(top3)
library(dplyr)
sd3 <- ds %>%
  filter(item_id %in% c(top3_name)) %>%
  summarise(sd = sd(item_price))
sdt <- ds %>%
  summarise(sd = sd(item_price))
print(abs(sd3 - sdt))
