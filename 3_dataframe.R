# 데이터 프레임
english <- c(90, 80, 60, 70)
english

math <- c(50, 60, 100, 20)
math

df_midterm <- data.frame(english, math)
df_midterm

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                        math = c(50, 60, 100, 20),
                        class = c(1, 1, 2, 2))
df_midterm


# 데이터 프레임 사용
mean(df_midterm$english)
mean(df_midterm$math)


# practice problem
df_fruit <- data.frame(제품 = c("사과", "딸기", "수박"),
                         가격 = c(1800, 1500, 3000),
                         판매량 = c(24, 38, 13))
df_fruit


## 엑셀 파일 이용하기
#install.packages("readxl")
library(readxl)

df_exam <- read_excel("data/excel_exam.xlsx")
#df_exam <- read_excel("data/excel_exam.xlsx", col_names = F)
# 엑셀의 첫 번째 행이 변수명이 아닌 경우 처리
#df_exam <- read_excel("data/excel_exam.xlsx", sheet = 3)
# 엑셀의 세 번째 시트 데이터를 불러옴
df_exam

mean(df_exam$english)
mean(df_exam$science)

write.csv(df_exam, file = "data/excel_exam.csv")


## csv 파일 이용하기
df_csv_exam <- read.csv("data/excel_exam.csv")
df_csv_exam


## rds 파일
# R전용 데이터 파일
saveRDS(df_exam, file = "data/excel_exam.rds")
rm(df_exam) # 데이터 삭제
df_exam

df_exam <- readRDS("data/excel_exam.rds")
df_exam