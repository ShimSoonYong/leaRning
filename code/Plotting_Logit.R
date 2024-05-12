# mtcars 데이터 불러오기
library(ggplot2)
data(mtcars)

# 로지스틱 회귀분석
model <- glm(vs ~ mpg, data = mtcars, family = binomial)
mtcars$vs

# 회귀선 그리기
ggplot(mtcars, aes(x = mpg, y = vs)) +
  geom_point() +  # 산점도 추가
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "연비", y = "vs (0: V-shaped, 1: straight)")


X<-c(0:50)
pred<-1/(1+exp(-(model$coefficients[1]+model$coefficients[2]*X)))

# 회귀선 그리기
plot(mtcars$mpg, mtcars$vs, xlab = "연비", 
     ylab = "vs (0: V-shaped, 1: straight)", pch = 16)
lines(X,pred, col = "blue")
