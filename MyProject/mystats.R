
windows(width = 7, height = 5)
#
v <- rbinom(n = 1000, size = 100, prob = 0.5)
v
hist(dbinom(v, size = 100, prob = 0.5), prob = T)
w <- rnorm(v, mean = 5, sd = 2)
hist(v)
w
hist(w, col = 'orange', breaks = 20)
hist(v, col = 'orange', breaks = 20)
# 균일함수 (0~100이라는 범위를 줌)
set.seed(2022)
v <- runif(n = 100000, min = 0, max = 100)
hist(v, col = 'steelblue')

mean(v)
sd(v)



# 
v <- rnorm(n = 100, mean = 50, sd = 20)
v
hist(v,breaks = 20)
# 원주율
n_sim <- 1000
x <- vector(length = n_sim)
y <- vector(length = n_sim)
res = 0
for (i in 1:n_sim) {
  x[i] <- runif(1)
  y[i] <- runif(1)
  if (x[i]^2 + y[i]^2 < 1) {
    res <- res + 1
  }
}
4 * res / n_sim
circle <- function (x) sqrt(1 - x^2)
plot(x, y, xlab='X', ylab='Y', col='blue')
curve(circle, from = 0, to = 1, add=T, col='red', lwd=2)

# 정규 분포
x <- seq(0,100, length = 100)
x
y <- dnorm(x, mean = 50, sd = 20)
y
plot(x, y, col = 'steelblue', lwd = 3, type = 'l')


x <- seq(0,1000, length = 100)
x
y <- dunif(x, min = 0, max = 1000
y
plot(x, y, col = 'steelblue', lwd = 3, type = 'b')

seq(0,100, length = 100)
dunif(x, min = 0, max = 100)

x <- seq(140,210, length = 100)
x
y <- dnorm(x, mean = 170, sd = 10)
y
plot(x, y, col = 'steelblue', lwd = 3, type = 'b')
x
y
# 연습문제
# 국민소득 mean = $30000, sd = $10000 X를 개인의 소득을 나타내는 확률변수
pnorm(35000, mean = 30000, sd = 10000)
pnorm(25000, 30000, 10000)

pnorm(35000, mean = 30000, sd = 10000) - pnorm(25000, 30000, 10000)

pnorm(1, 0, 1) - pnorm(-1, 0, 1)

pnorm(2) - pnorm(-2)

pnorm(70, 60, 10, lower.tail = F)
pnorm(80, 70, 20, lower.tail = F)

x <- rbinom(10000, size = 100, prob = 0.5)
hist(x, col = 'skyblue', breaks = 20, prob = T)
x
curve(dnorm(x, 50, 5), 30, 70, col = 'tomato', add = T, lwd = 3, lty = 2)

library(MASS)
height <- na.omit(survey$Height)
length(height)
hist(height, col = 'skyblue', breaks = 20)
# 209행중 30개 샘플링
samp <- height[sample(1:209, size = 30)]
X.bar <- mean(samp)
X.sd <- sd(samp)

X.bar <- c()
for (i in 1:100000) {
  samp <- height[sample(1:209, size = 30)]
  X.bar[i] <- mean(samp)
  X.sd[i] <- sd(samp)
}
hist(X.bar, col = 'skyblue', breaks = 20)
x <- seq(160, 180, length = 200)
curve(dnorm(x, mean(height), sd(X.bar)), 160, 180, col = 'tomato', add = T, lwd = 3, lty = 2)

x.1 <- rnorm(n = 5000, mean = 70, sd = 5)
x.2 <- rnorm(n = 5000, mean = 50, sd = 5)
x <- c(x.1, x.2)
hist(x, col = 'skyblue', breaks = 20)

X.bar <- c()
for (i in 1:100000) {
  samp <- x[sample(x, size = 30)]
  X.bar[i] <- mean(samp)
  X.sd[i] <- sd(samp)
}

hist(X.bar, col = 'skyblue', breaks = 20, prob = T )
x.samp <- seq(30, 90, length = 200)
curve(dnorm(x.samp, mean(x), sd(X.bar)), 30, 90, col = 'tomato', add = T, lwd = 3, lty = 2)

cor(iris[,-5])
cor.test(iris$Petal.Width, iris$Petal.Length)
# 60번 성공 100번 시도 
binom.test(x = 60, n = 100, p = 0.5)

binom.test(x = 6, n = 10, p = 0.5)
pnorm(q = 60, mean = 50, sd = 10)
qnorm(p = 0.84 , mean = 50, sd = 10)
50+1*10
qnorm(p = 0.975, mean = 50, sd = 10)
qnorm(p = 0.025, mean = 50, sd = 10)
qnorm(p = 0.005, mean = 50, sd = 10)
# n은 시행 횟수이고 p는 각 시행에서 사건 A가 일어날 확률
binom.test(x = 65, n = 100, p = 0.5)

shapiro.test(survey$Height)
shapiro.test(survey$Age)
hist(survey$Age) # p-value 2.2e -16 정규 분포를 따른다고 보기 어려움

shapiro.test(iris$Petal.Length)
hist(iris$Petal.Length)

# x축 이론적 y축 샘플
qqnorm(survey$Height, col = 'steelblue')
qqline(survey$Height, col = 'tomato', lwd = 3)

qqnorm(survey$Age, col = 'steelblue')
qqline(survey$Age, col = 'tomato', lwd = 3)

v <- rt(n = 10000, df = 29)
hist(v, prob = T)
x <- seq(-4, 4, length = 200)
curve(dt(x, df = 29), -4, 4, add = T)

curve(dnorm(x), -4, 4, add = T, col = 'violet', lwd = 5, lty = 4)

pt(q = 0.975, df = 19)
pt(q = 0.975, df = 29)

str(cats)
data(cats)
# 평균의 차이
t.test(Bwt ~ Sex, data = cats, conf.level = 0.99)

tapply(cats$Bwt, INDEX = list(Sex =cats$Sex), mean)

str(sleep)

t.test(extra ~ group, data = sleep, paired = T)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
library(tidyr)
wide.df <- spread(sleep, key = group, value = extra)
wide.df
summary(wide.df)

tapply(sleep$extra, INDEX = list(sleep$group), mean)

t.test(extra ~ group, data = sleep)
t.test(wide.df$`1`, wide.df$`2`, paired = T)

v <- rchisq(10000, df = 1)
hist(v, col = 'orange')

x <- seq(0, 15, enlgth = 200)
curve(dchisq(x, df = 1),0 , 15, col = 'red', lwd = 2, lty = 1)
curve(dchisq(x, df = 5),0 , 15, col = 'steelblue', lwd = 2, lty = 1, add = T )
curve(dchisq(x, df = 10),0 , 15, col = 'green', lwd = 2, lty = 1, add = T )

qchisq(p = 0.95, df = 1)
pchisq(q = 2.5, df = 1)

mt <- matrix(c(1443, 151, 47, 1781, 312, 135), nrow = 3)
mt

df <- data.frame(mt)
str(df)
df
# dataframe 행, 컬럼명 지정
colnames(df) <- c('with', 'without')
rownames(df) <- c('정상', '중상', '사망')
df

eij <- c(1367, 1855.9, 196.9, 267.4, 77.1, 104.7)
oij <- c(1443, 1781, 151, 312, 47, 135)
cs.value <- sum((oij - eij)^2 / eij)

Titanic
class(Titanic)

tb <- margin.table(Titanic, margin = c(4,1))
class(tb)
chisq.test(tb)

v <- rf(n = 10000, df1 = 1, df2 = 30)
hist(v, col = 'red')
x <- seq(0, 15, length = 200)
curve(df(x, df1 = 1, df2 = 30), 0, 15, col = 'blue', add = T, lwd = 2, lty = 1)
curve(df(x, df1 = 5, df2 = 50), 0, 15, col = 'green', add = T, lwd = 2, lty = 1)
curve(df(x, df1 = 10, df2 = 80), 0, 15, col = 'blue', add = T, lwd = 2, lty = 1)

qf(p = 0.95, df1 = 1, df2 =30)
pf(q = 4.170877, df1 = 1, df2 = 30)

df <- InsectSprays
str(df)
table(df$spray)

round(tapply(df$count, INDEX = list(df$spray), FUN =  mean), 3)
boxplot(count~spray, data = df, col = 2:7)
# aov 집단간 분산까지 구해 결과를 구한 것
aov.result <- aov(count ~ spray, data = df)
summary(aov.result)
# 사후검정 집단 내 각각 
TukeyHSD(aov.result)

install.packages('gplots')
plotmeans(count ~ spray, data = df,
          col = 'tomato', lwd = 3,
          barcol = 'orange', barwidth = 3)
model.tables(aov.result, type = 'mean')
model.tables(aov.result, type = 'effect')

plot(TukeyHSD(aov.result), col = 'blue', las = 1,
     )

library(car)
qqPlot(df$count, pch = 19)
shapiro.test(df$count)

# 집단간 분산의 동일성 여부 검정 >> 레벤 테스트, 바를렛 테스트
leveneTest(count ~ spray, data = df)
# ** 별두개 << 등분산이 아니다 // 등분산이다 - 귀무가설 // thus 기각 
bartlett.test(count ~ spray, data = df)
# 일원 배치 분산
oneway.test(count ~ spray, data = df)
oneway.test(count ~ spray, data = df, var.equal = TRUE)

df <- ToothGrowth
str(df)
unique(df$dose)
df$dose <- factor(df$dose, levels = c(0.5 , 1.0, 2.0), labels = c('L', 'M', 'H'))
tapply(df$len,
       INDEX = list(SUPP = df$supp, DOSE = df$dose),
       FUN = mean)

with(df, tapply(len, INDEX = list(supp, dose), mean))

boxplot(len~supp * dose, data = df, col = c(2,3))

Y(종속변수) ~ X(독립변수)

Y ~ X1 +- X2 (주효과)

len ~ supp + dose + supp:dose
#supp:dose 상호작용

aov.result <- aov(len ~ supp * dose, data = df)
summary(aov.result)
# 사후분석
TukeyHSD(aov.result)
plot(TukeyHSD(aov.result), las = 1)

cor(cats$Bwt, cats$Hwt)
plot(cats$Bwt, cats$Hwt, pch = 19, col = 'tomato')

cor(cats$Bwt, cats$Hwt, method = 'pearson')
cor(cats$Bwt, cats$Hwt, method = 'spearman')
cor(cats$Bwt, cats$Hwt, method = 'kendall')
cats$Bwt
cats$Hwt
install.packages('HistData')
library(HistData)
df <- GaltonFamilies
str(df)

cor(df$midparentHeight, df$childHeight)
plot(jitter(childHeight)~jitter(midparentHeight), 
     data = df,
     col = adjustcolor('steelblue', alpha = 0.5))

model <- lm(childHeight ~ midparentHeight, data = df)
abline(model, col = 'tomato', lwd = 3)

x <- runif(n = 100, min = 0, max = 100)
y <- 3 * x + 5 + rnorm(100, 0, 20)
plot(x, y , pch = 19, col = 'blue')

cor(x, y)
model <- lm(y ~ x)
abline(model, col = 'tomato', lwd = 2)

summary(model)

abline(a = 1, b = 5, col = 'red', lwd = 1, lty = 2)

library(car)
data(Prestige)
df <- Prestige
str(df)

table(df$type)
barplot(table(df$type), col = 'orange')

hist(df$income, col = 'tomato', breaks = 20)

shapiro.test(df$income)

hist(df$education, col = 'tomato', breaks = 20)
hist(df$women, col = 'tomato', breaks = 20)
hist(df$prestige, col = 'tomato', breaks = 20)

shapiro.test(df$prestige)

plot(df[, -(5:6)], pch = 19, col = 'steelblue')

model <- lm(income ~ education, data = df)
summary(model)
cor(df[, -(5:6)])
model
plot(income ~ education, data = df,
     col = 'skyblue', pch = 19)
abline(model, col = 'tomato', lwd = 2)
# 회귀 계수 coef

# 잔차 값
summary(resid(model))
# 신뢰구간
confint(model)
# 분산 분석 
anova(model)

# 다중 선형회귀 : 독립변수가 여러개
# 다중 회귀식 : y = B0 + B1x1 + B2x2 ...... 절편 1개에 기울기 여러개
model <- lm(income ~ education + women + prestige, data = df )
summary(model)
lm(income ~ education + women + prestige, data = df)

model <- lm(income ~ education + prestige, data = df )
summary(model)

model <- lm(income ~ education + women, data = df )
summary(model)

model <- lm(income ~ women + prestige, data = df )
summary(model)
install.packages('stargazer')
library(stargazer)
stargazer(model, type = 'text')

par(mfrow = c(2,2))
plot(model)
par(mfrow =c(1,1))

model <- lm(income ~ education, data = df)
plot(income ~ education, data = df)

library(dplyr)
model <- lm(income ~ education + I(education^2), data = df)
summary(model)
abline(model)
install.packages('tidyverse')
library(tidyverse)

with(df, lines(arrange(data.frame(education, fitted(model)))))

df <- mtcars
str(df)
df <- mtcars[,1:6]
df
plot(df, col = 'green', pch = 19)

cor(df)
install.packages("corrgram")
library(corrgram)
corrgram(df)

lm(mpg ~ ., data = df)
model <- lm(mpg ~ ., data = df)
summary(model)

model <- lm(mpg ~ hp + wt, data = df)
summary(model)
model <- lm(mpg ~　hp + wt, data = df)
