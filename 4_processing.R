exam <- read.csv("data/excel_exam.csv")

# 데이터 파악
head(exam)
head(exam, 10) # 개수 지정
tail(exam)
tail(exam, 10)

View(exam) # view 창에서 데이터 확인

dim(exam) # 행, 열 출력
str(exam) # 변수 속성 출력
summary(exam) # 요약 통계량


mpg <- as.data.frame(ggplot2::mpg) # 데이터 프레임으로 ggplot에서 mpg를 부름
head(mpg)
View(mpg)
dim(mpg)
str(mpg)

?mpg # 패키지 데이터는 설명 확인 가능

summary(mpg)


# 변수명 변경
df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw

install.packages("dplyr")
library(dplyr) # 데이터 가공 패키지

df_new <- df_raw
df_new <- rename(df_new, v2 = var2)
df_new
df_raw


# 연습
mpg <- rename(mpg, city = cty, highway = hwy)
head(mpg)


# 파생변수 생성
df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 + df$var2)/2
df

mpg$total <- (mpg$city + mpg$highway)/2
head(mpg)
mean(mpg$total)

# 조건이 걸린 파생변수
summary(mpg$total)
hist(mpg$total) # 히스토그램

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20)

table(mpg$test) # 빈도표

library(ggplot2)
qplot(mpg$test) # 막대 그래프

mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C")) # 중첩 조건문



# 연습 문제
midwest <- as.data.frame(ggplot2::midwest)
summary(midwest)

midwest <- rename(midwest, total = poptotal, asian = popasian)
summary(midwest)

midwest$asian_rate <- midwest$asian / midwest$total * 100
head(midwest)
summary(midwest)
hist(midwest$asian_rate)

midwest$asian_rate_val <- ifelse(midwest$asian_rate > mean(midwest$asian_rate), "large", "small")
head(midwest)

table(midwest$asian_rate_val)
qplot(midwest$asian_rate_val)
