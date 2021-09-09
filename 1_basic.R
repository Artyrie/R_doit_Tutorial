# 변수 선언
a <- 1
a


# combine
var1 <- c(1, 2, 3)
var1
var1 <- c(1:5)
var1
var1 <- seq(1, 3)
var1
var1 <- seq(1, 5, by = 2)
var1

var1 <- c(1:5)
var2 <- c(1:5)
var1 + var2
var1 + 3


# String
str1 <- "Hello"
str1
str1 <- c("Hello!", "R!")
str1 #연산 불가


# 기본 함수
x <- c(1:3)
mean(x)
max(x)
min(x)

paste(str1, collapse = ", ")


# package
install.packages("ggplot2")
library(ggplot2)
x <- c("a", "a", "b", "c")
qplot(x)