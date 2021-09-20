library(dplyr)
exam <- read.csv('data/excel_exam.csv')
exam

# dplyr 패키지는 %>% 기호를 이용해 함수를 나열하는 방식을 사용
# %in% 기호는 변수의 값이 지정한 조건 목록에 해당하는지 확인하는 기능
# R의 특이 기호 - %/% : 나눗셈의 몫, %% 나눗셈의 나머지, %in% 매칭 확인
# write.csv(exam2, "data/excel_exam.csv", row.names = F) 인덱스 제거 저장장

# 필터링 (filter)
exam %>% filter(class == 1)
exam %>% filter(class != 1)
exam %>% filter(math >= 50)
exam %>% filter(class == 1 & math >= 50)
exam %>% filter(english < 90 | science < 50)
exam %>% filter(class %in% c(1, 3, 5))

class1 <- exam %>% filter(class == 1)
mean(class1$math)


# 연습
mpg <- as.data.frame(ggplot2::mpg)
mpg_displ4 <- mpg %>% filter(displ <= 4)
mpg_displ5 <- mpg %>% filter(displ >= 5)
mean(mpg_displ4$hwy)
mean(mpg_displ5$hwy)

mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)

mpg_3com <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_3com$hwy)


# 변수 추출 (select)
exam %>% select(math)
exam %>% select(class, math, english)
exam %>% select(-math)
exam %>% filter(class == 1) %>% select(english)
exam %>%
  filter(class == 1) %>%
  select(english)
exam %>%
  select(id, math) %>%
  head


# 연습
mpg <- as.data.frame(ggplot2::mpg)
mpg_cc <- mpg %>% select(class, cty)
head(mpg_cc)
mpg_suv <- mpg_cc %>% filter(class == "suv")
mean(mpg_suv$cty)
mpg_compact <- mpg_cc %>% filter(class == "compact")
mean(mpg_compact$cty)


# 정렬 (arrange)
exam %>% arrange(math) # default 낮은 순
exam %>% arrange(desc(math)) # 높은 순
exam %>% arrange(class, math) # 앞에 넣은 것부터 정렬함
exam %>% arrange(math, class)


# 연습
mpg <- as.data.frame(ggplot2::mpg)
mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_audi %>% arrange(desc(hwy)) %>% head


# 파생변수 추가 (mutate)
exam %>%
  mutate(total = math + english + science) %>%
  head
exam %>% # 한 번에 적용
  mutate(total = math + english + science, 
         mean = (math + english + science) / 3) %>%
  head
exam %>% # 조건 적용
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head
exam %>% # 응용
  mutate(total = math + english + science) %>%
  arrange(total) %>%
  head
# dplyr 패키지는 데이터 프레임명을 반복하여 입력하지 않음


# 연습
mpg <- as.data.frame(ggplot2::mpg)
mpg2 <- mpg %>%
  mutate(ch_total = cty + hwy) %>%
  mutate(ch_mean = ch_total / 2)
head(mpg2)
mpg2 %>%
  arrange(desc(ch_mean)) %>%
  head(3)
mpg %>%
  mutate(ch_total = cty + hwy,
         ch_mean = ch_total / 2) %>%
  arrange(desc(ch_mean)) %>%
  head


# 집단별 요약 (summarise)
exam %>% summarise(mean_math = mean(math))
exam %>% # group_by와 함께 그룹별 계산을 할 때 사용
  group_by(class) %>%
  summarise(mean_math = mean(math))
exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())
# n은 빈도를 나타냄
# mean, sd(표준편차), sum, median, min, max, n 등을 사용
mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty)) %>%
  head(10)
# 복합
mpg %>%
  group_by(manufacturer) %>%
  filter(class == "suv") %>%
  mutate(total = (cty + hwy) / 2) %>%
  summarise(mean_total = mean(total)) %>%
  arrange(desc(mean_total)) %>%
  head(5)
# 실험 결과 group_by ~ mutate의 순서는 바뀌어도 되나,
# group_by => summarise의 순서는 지켜져야 함


# 연습
mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty))
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)
mpg %>%
  group_by(manufacturer) %>%
  filter(class == "compact") %>%
  summarise(n = n()) %>%
  arrange(desc(n))


# 데이터 합치기 (left_join, bind_rows)
# 가로 (left_join)
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 70, 80, 90, 85))
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))
test1
test2
total <- left_join(test1, test2, by = "id") # id를 기준으로 total에 할당당
total
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name
exam_new <- left_join(exam, name, by = "class")
exam_new
# 세로 (bind_rows)
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 70, 80, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))
group_a
group_b
group_all <- bind_rows(group_a, group_b) # 두 데이터의 변수명이 같아야 함함
group_all


# 연습
head(mpg)
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22))
fuel
mpg2 <- left_join(mpg, fuel, by = "fl")
head(mpg2)
mpg2 %>%
  select(model, fl, price_fl) %>%
  head(5)


# 도전
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>%
  mutate(popkidsRate = 1 - popadults/poptotal)
head(midwest)
midwest %>%
  select(county, popkidsRate) %>%
  arrange(desc(popkidsRate)) %>%
  head(5)
midwest %>% # 오답 참조
  mutate(kidsGrade = ifelse(popkidsRate >= 0.4, "large", ifelse(popkidsRate >= 0.3, "middle", "small"))) %>%
  group_by(kidsGrade) %>%
  summarise(n = n())
midwest %>%
  mutate(popasianRate = popasian/poptotal) %>%
  select(state, county, popasianRate) %>%
  arrange(popasianRate) %>%
  head(10)

# 오답 (최적화)
midwest <- midwest %>%
  mutate(kidsGrade = ifelse(popkidsRate >= 0.4, "large", ifelse(popkidsRate >= 0.3, "middle", "small")))
table(midwest$kidsGrade)