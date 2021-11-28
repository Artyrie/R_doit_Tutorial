# 군집 분석
str(USArrests)
head(USArrests)
summary(USArrests)
US.dist_eucliean <- dist(USArrests, "euclidean")
US.dist_eucliean
US.single <- hclust(US.dist_eucliean^2, method = "single") # 제곱 안해도 같은 결과인듯
plot(US.single) # 덴드로그램으로 표현
group <- cutree(US.single, k = 6)
group # 6개의 그룹으로 분할
rect.hclust(US.single, k = 6, border = "blue") # 덴드로그램에 그룹 표시


# K-means
library(rattle)
df = scale(wine[-1])
set.seed(42)
fit.km <- kmeans(df, 3, nstart = 25)
fit.km$size # k개의 점과 가까운 데이터 개수
fit.km$centers # 속성별 k개의 점에 대한 위치
plot(df, col = fit.km$cluster)
points(fit.km$cluster, col = 1:3, pch = 8, cex = 1.5)