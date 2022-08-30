
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
y <- dnorm(x, mean = 50, sd = 1)
y
plot(x, y, col = 'steelblue', lwd = 3, type = 'l')


x <- seq(0,1000, length = 100)
x
y <- dunif(x, min = 0, max = 1000)
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
survey$Height
library(MASS)
height <- na.omit(survey$Height)
height
length(height)
hist(height, col = 'skyblue', breaks = 20)
# 209행중 30개 샘플링
samp <- height[sample(1:209, size = 30)]
samp
X.bar <- mean(samp)
X.sd <- sd(samp)

X.bar <- c()
for (i in 1:100000) {
  samp <- height[sample(1:209, size = 30)]
  X.bar[i] <- mean(samp)
}
X.bar
hist(X.bar, col = 'skyblue', breaks = 20, freq = F)
x <- seq(160, 180, length = 200)
curve(dnorm(x, mean(X.bar), sd(X.bar)), 160, 180, col = 'tomato', add = T, lwd = 3, lty = 2)

x.1 <- rnorm(n = 5000, mean = 70, sd = 5)
x.2 <- rnorm(n = 5000, mean = 50, sd = 5)
x <- c(x.1, x.2)
hist(x, col = 'skyblue', breaks = 20)

X.bar <- c()
for (i in 1:100000) {
  samp <- x[sample(x, size = 30)]
  X.bar[i] <- mean(samp)
}
samp

iris$Species
hist(X.bar, col = 'skyblue', breaks = 20, prob = T)
x.samp <- seq(30, 90, length = 200)
curve(dnorm(x.samp, mean(X.bar), sd(X.bar)), 30, 90, col = 'tomato', add = T, lwd = 3, lty = 2)

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
# 데이터가 정규 분포를 따르는지 샤피로 윌크 검정을 수행한다. 귀무가설은 정규 분포를 따른다는 것
shapiro.test(survey$Height)
hist(survey$Height)
shapiro.test(survey$Age)
hist(survey$Age) # p-value 2.2e -16 정규 분포를 따른다고 보기 어려움

shapiro.test(iris$Petal.Length)
hist(iris$Petal.Length)

# x축 이론적 y축 샘플

# qqplot
#데이터가 정규 분포하는 지에 대한 확인
#여러 변수에 대한 분포의 비교(같은 분포인 지 여부)
#잔차의 정규성을 확인하여 회귀 분석의 적합도 확인

qqnorm(survey$Height, col = 'steelblue')
qqline(survey$Height, col = 'tomato', lwd = 3)
boxplot(survey$Height)
qqnorm(survey$Age, col = 'steelblue')
qqline(survey$Age, col = 'tomato', lwd = 3)

v <- rt(n = 10000, df = 29)
hist(v, prob = T)
x <- seq(-4, 4, length = 200)
# df 자유도가 높을수록 정균분포에 가까워짐
curve(dt(x, df = 50), -4, 4, add = T)
# dnorm 정규분포 따라 곡선
curve(dnorm(x), -4, 4, add = T, col = 'violet', lwd = 5, lty = 4)

pt(q = 0.975, df = 19)
pt(q = 0.975, df = 29)
install.packages('cats')
library(cats)
str(cats)
data(cats)
# 평균의 차이
t.test(Bwt ~ Sex, data = cats, conf.level = 0.95)

tapply(cats$Bwt, INDEX = list(Sex =cats$Sex), mean)

str(sleep)
sleep
t.test(extra ~ group, data = sleep, paired = T)
sleep$extra                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
sleep$group
library(tidyr)
str(sleep)
summary(sleep)
sleep
wide.df <- spread(sleep, key = group, value = extra)
wide.df
summary(wide.df)

tapply(sleep$extra, INDEX = list(sleep$group), mean)

f(extra ~ group, data = sleep)
t.test(wide.df$`1`, wide.df$`2`, paired = T)
# r 카이제곱 100 > 발생할 난수의 개수
v <- rchisq(1000, df = 1)
v
hist(v, col = 'orange')
shapiro.test(v)

x <- seq(0, 15, length = 200)
curve(rchisq(x, df = 1),0 , 15, col = 'red', lwd = 2, lty = 1)
curve(rchisq(x, df = 10),0 , 15, col = 'steelblue', lwd = 2, lty = 1, add = T )
curve(rchisq(x, df = 20),0 , 15, col = 'green', lwd = 2, lty = 1, add = T )
curve(rchisq(x, df = 30),0 , 15, col = 'green', lwd = 2, lty = 1, add = T )

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

tb <- margin.table(Titanic, margin = c(3,2))
tb
class(tb)
chisq.test(tb)

v <- rf(n = 10000, df1 = 1, df2 = 30)
v
hist(v, col = 'red', freq = F)
x <- seq(0, 15, length = 200)
shapiro.test(x)
curve(df(x, df1 = 1, df2 = 30), 0, 15, col = 'blue', add = T, lwd = 2, lty = 1)
curve(df(x, df1 = 5, df2 = 50), 0, 15, col = 'green', add = T, lwd = 2, lty = 1)
curve(df(x, df1 = 10, df2 = 80), 0, 15, col = 'orange', add = T, lwd = 2, lty = 1)

qf(p = 0.95, df1 = 1, df2 =30)

pf(q = 4.170877, df1 = 1, df2 = 30)

df <- InsectSprays
str(df)
table(df$spray)
df
df$count
aov(count ~ spray, data = df)
summary(aov(count ~ spray, data = df))
round(tapply(df$count, INDEX = list(df$spray), FUN =  mean), 3)
boxplot(count~spray, data = df, col = 2:7)
# aov 집단간 분산까지 구해 결과를 구한 것(여러 집단)
aov.result
# oneway <> aov 차이 : aov 출력되는 통계량 더 많지만, summary로 출력해야 p-value 나옴
# oneway 집단변수로 입력된 값 자동으로 범주형으로 인식 but aov 는 연속형 변수로 인식하기 때문에
# factor를 사용해 범주형으로 바꿔야함
aov.result <- aov(count ~ spray, data = df)
summary(aov.result)
# 사후검정 집단 내 각각 
TukeyHSD(aov.result)

install.packages('gplots')
library(gplots)
plotmeans(count ~ spray, data = df,
          col = 'tomato', lwd = 3,
          barcol = 'orange', barwidth = 3)
model.tables(aov.result, type = 'mean')
model.tables(aov.result, type = 'effect')

plot(TukeyHSD(aov.result), col = 'blue', las = 1,)

library(car)
qqPlot(df$count, pch = 19)
shapiro.test(df$count)
df$count
# 집단간 분산의 동일성 여부 검정 >> 레벤 테스트, 바를렛 테스트
leveneTest(count ~ spray, data = df)
# ** 별두개 << 등분산이 아니다 // 등분산이다 - 귀무가설 // thus 기각 
bartlett.test(count ~ spray, data = df)
# 일원 배치 분산
oneway.test(count ~ spray, data = df)
# var.equal = TRUE 등분산 가정
oneway.test(count ~ spray, data = df, var.equal = TRUE)

df <- ToothGrowth
str(df)
unique(df$dose)
df$dose <- factor(df$dose, levels = c(0.5 , 1.0, 2.0), labels = c('L', 'M', 'H'))
tapply(df$len,
       INDEX = list(SUPP = df$supp, DOSE = df$dose),
       FUN = mean)
df$len
with(df, tapply(len, INDEX = list(supp, dose), mean))
df$dose
boxplot(len~supp * dose, data = df, col = c(2,3))

Y(종속변수) ~ X(독립변수)

Y ~ X1 +- X2 (주효과)

len ~ supp + dose + supp:dose
#supp:dose 상호작용

aov.result <- aov(len ~ supp, data = df)
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
df
cor(df$midparentHeight, df$childHeight)
plot(jitter(childHeight)~jitter(midparentHeight), 
     data = df,
     col = adjustcolor('steelblue', alpha = 0.5))

model <- lm(childHeight ~ midparentHeight, data = df)
model
abline(model, col = 'tomato', lwd = 3)
summary(model)
x <- runif(n = 100, min = 0, max = 100)
y <- 3 * x + 5 + rnorm(100, 0, 20)
plot(x, y , pch = 19, col = 'blue')
leveneTest(df$childHeight,df$gender)
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
df
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

model <- lm(mpg ~　disp + hp + wt + drat, data = df)
mod.selected <- step(model, direction = 'backward')
summary(mod.selected)

# house

df = read.csv('train.csv', header = T)
df
str(df)
model <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea, data = df)
summary(model)
model <- lm(SalePrice ~ MSSubClass, data = df)
summary(model)

v <- c()
for (i in 1:length(colnames(df))) {
  if (mode(df[,i]) == 'numeric') {
    v <- c(v,i)
  }
}
v
str(df[,v])
df <- df[,v]
df <- df[complete.cases(df),]
model <- lm(SalePrice ~ . , data = df)
summary(model)
mod.selected <- step(model, direction = 'backward')
summary(mod.selected)

model <- lm(SalePrice ~ 1, data =df)

df <- InsectSprays
# spray 범주형이면 A기준 변수로 더미 변환
model <- lm(count ~ spray, data = df)
summary(model)
contrasts(df$spray)

df <- mtcars[, 1:6]
str(df)

df$cyl <- factor(df$cyl)
head(df)
table(df$cyl)

plot(iris[iris$Species != 'virgicolor', c(1,5)])
install.packages('robust')
library(robust)
data("breslow.dat")

df <- breslow.dat
str(df)

df <- df[, c('Base', 'Age', 'Trt', 'sumY')]
str(df)
dim(df)
model <- glm(sumY ~ ., data = df, family = poisson)
summary(model)
# exp ln(x) 한값이기 때문에 exp로 e의 x 승
exp(coef(model))

df <- split(iris, f = iris$Species)
df <- rbind(df$setosa, df$versicolor)
plot(df[, c(3,5)])

df$Species <- as.numeric(df$Species)
glm(Species ~ Petal.Length, data = df, family = binomial(link = 'logit'))

library(palmerpenguins)
df <- penguins
df <- iris
df$Species <- factor(ifelse(
  df$Species == "virginica", "Yes", "No"))

model <- glm(Species ~ ., data = df,
             family = binomial(link = "logit"))
summary(model)
par(mfrow = c(2,2))
plot(model)
df$pred <- factor(ifelse(
  model$fitted.values > 0.5, "Yes", "No"))

tab <- table(df$pred, df$Species)

TP <- tab[2,2]
TN <- tab[1,1]
PF <- tab[2,1]
FN <- tab[1,2]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
install.packages('pROC')
library(pROC)
roc(Species ~ model$fitted.values, data = df, plot = TRUE, main = 'ROC CURVE')
