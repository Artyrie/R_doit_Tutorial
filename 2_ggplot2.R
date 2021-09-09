library(ggplot2)

qplot(data = mpg, x = hwy) # Mile Per Gallon 데이터 로드 및 x축을 hwy로 지정
qplot(data = mpg, x = cty) # 막대 그래프
qplot(data = mpg, x = drv, y = hwy, geom = "line") # 선 그래프
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot") # 박스 플롯
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv) # 컬러 지정

?qplot


# practice problem
score <- c(80, 60, 70, 50, 90)
score
mean_score <- mean(score)
mean_score