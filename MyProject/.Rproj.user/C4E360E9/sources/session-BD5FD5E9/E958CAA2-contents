df <- iris
str(df)
class(df)
dim(df)
nrow(df)
ncol(df)

rownames(df)
head(df)
colnames(df)

v <- c(85, 77, 96)
v

names(v)
names(v) <- c('Kor','Eng','Math')
v
v['Kor']
# 이름에는 -c 불가능
v[c('Eng','Math')]

df$Sepal.Length
df$Sepal.Width
df$Sepal.sum <- df$Sepal.Length + df$Sepal.Width
str(df)

head(df[,5:6])

df$Sepal.Sep <- ifelse(mean(df$Sepal.sum) < df$Sepal.sum, 'Big', 'Small')
df$Sepal.Sep
df$Sepal.Sep <- factor(df$Sepal.Sep)
str(df)
levels(df$Sepal.Sep)
table(df$Sepal.Sep)
barplot(table(df$Sepal.Sep))

?state.x77
class(state.x77)
is.data.frame(state.x77)
st <- as.data.frame(state.x77)
class(st)
str(st)
dim(st)
max(st$Population)
st$Population
rownames
st[which.max(st$Population), ]


df <- iris
df$Sepal.sum <- df$Sepal.Length + df$Sepal.Width
write.csv(df, 'my.iris.csv', row.names = F)
getwd()

df <- read.csv('my.iris.csv', header = T)
str(df)

# install.packages.('readxl')
library(readxl)
df <- read_excel('성적표.xlsx', sheet = 1)
str(df)
class(df)
df
df <- data.frame(df)
str(df)
df$평균 <- round(apply(df[,2:4], MARGIN = 1, mean), 2)
head(df)
rm(round)
# NA 값 처리하기

?airquality
str(airquality)

aq <- airquality
str(aq)
mean(aq$Ozone, na.rm = T)
aq$Ozone[is.na(aq$Ozone)]
sum(is.na(aq$Ozone))

ozone <- aq$Ozone
ozone[is.na(ozone)] <- mean(ozone, na.rm = T)
ozone

ozone[is.na(ozone)] <- sample(na.omit(aq$Ozone), 37)
ozone

# 샘플링 기존 표준편차에서 값이 달라지지 않음
# DF에서 is or omit 친화적
# complete.cases(aq) NA를 포함하지 않은 것만 추출(행을 기준으로 함)
complete.cases(aq)
aq <- aq[complete.cases(aq),]
aq
#install.packages.('VIM')
library(VIM)
?aggr
aggr(airquality)

df <- data.frame(state.x77)
boxplot(df$Income, pch = 19, col = 'orange', border = 'brown')

class(boxplot.stats(st$Income))
# boxplot.stats() 함수를 이용한 이상치에 대한 상세 확인
boxplot.stats(st$Income)$out

st[st$Income==boxplot.stats(st$Income)$out, ]

df <- iris

boxplot(df$Petal.Length, col = 'skyblue')
df$Petal.Length
boxplot(Petal.Length ~ Species, data = iris,
        col = 'steelblue')

outlier <- boxplot.stats(iris[iris$Species == 'setosa', 4])$out
iris[iris$Petal.Width %in% outlier, ]

st <- data.frame(state.x77)
st[st$Population==max(st$Population), c(3, 6)]
# 선택 : subset()
subset(st, 
       subset = st$Population == max(st$Population),
       select = c(3, 6))
iris[,-5]
subset(iris, select = 1:4)

set <- iris[iris$Species == 'setosa', ]
vrs <- iris[iris$Species == 'versicolor', ]
vrg <- iris[iris$Species == 'virginica', ]
# 분리 split()

sp <- split(iris, f = iris$Species)
length(sp)
names(sp)
class(sp)

sp$setosa
sp$versicolor
sp$virginica

#결합 rbind (행)
df.2 <- rbind(sp$setosa, sp$versicolor)
dim(df.2)

iris[,1:2]
iris[,3:4]

# 결합 cbind (열)
df.3 <- cbind(iris[, 1:2], iris[, 3:4])
dim(df.3)

# 결합 merge() : 특정 변수의 값이 같은 행을 기준으로 여러 개의 데이터 프레임을 병합
library(readxl)
df.1 <- read_excel('성적표.xlsx', sheet = 1)
df.2 <- read_excel('성적표.xlsx', sheet = 2)
# outer join
merge(df.1,df.2, all = T)
# inner join
merge(df.1,df.2)
# 컬럼명 기준 inner join
df <- merge(df.1,df.2, all = T, 
      by.x = c('번호','이름'), 
      by.y = c('No','Name'))

colnames(df) <- c('no',
                  'name',
                  'python',
                  'r',
                  'ml',
                  'dl',
                  'cloud')
df
# aggregate() : 범주별로 통계량을 확인하고 싶을 때 주로 활용

df <- iris
aggregate(df[,-5],
          by = list(품종=df$Species),
          FUN = mean)
aggregate(df[,-5],
          by = list(품종=df$Species),
          FUN = sd)

library(MASS)
data('survey')
df <- survey
str(df)

df <- na.omit(df)
df <- df[complete.cases(df),]
dim(df)

hist(df$Height, breaks = 20)

hist(df[df$Sex=='Female', ]$Height,
     breaks = 20)

mean(df[df$Sex=='Female', ]$Height)
mean(df[df$Sex=='Male', ]$Height)

aggregate(df[,c(10, 12)],
          by = list(Gender = df$Sex),
          FUN = mean)
table(df$Sex)
t.test(Height ~ Sex, data = df)
boxplot(Height ~ Sex, data = df,
        col = c('orange', 'tomato'))

v <- c(30, 50, 20, 40, 10)
v

sort(v)
sort(v, decreasing = T)

df <- mtcars
str(df)

df <- data.frame(state.x77)
str(df)

rm(order)

sort(df$Illiteracy, decreasing = T)
sort(df, decreasing = T)

ord <- order(df$Illiteracy, df$Income, decreasing = T)
df[ord[1:10], c(3,2)]

x <- sample(1:10, size = 5)
y <- sample(1:10, size = 5, replace = T)
sum(x==7)
x
s <- 0
for (i in 1:1000000) {
  x <- sample(1:10, size = 5)
  s <- s + sum(x == 7)
}
s

idx <- sample(1:nrow(iris), size = 50)
iris[idx, ]

set.seed(2022)
sample(1:10, size = 5, replace = T)

sample(1:10, size = 5)
library(palmerpenguins)
data(package = 'palmerpenguins')
data('penguins')
pg <- data.frame(penguins)
summary(penguins[, c(1, 2, 7)])
library(VIM)
aggr(pg, numbers = T, prop = F)
pg <- na.omit(pg)
dim(pg)

str(pg)
table(pg$species)
barplot(table(pg$species))

table(pg$island)
barplot(table(pg$island))

table(pg$sex)
barplot(table(pg$sex))

str(pg[, 3:6])
summary(pg[, 3:6])

par(mfrow = c(2, 2))
hist(pg$bill_length_mm, col = c('orange', 'violet', 'pink'))
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow = c(1, 1))

my.color <- ifelse(pg$species == 'Gentoo', 1,
                   ifelse(pg$species == 'Adelie', 'steelblue', 'orange'))
plot(pg$bill_length_mm, pg$bill_depth_mm,
     pch = 19, col = my.color)

cor(pg$bill_length_mm, pg$bill_depth_mm)

cor(pg[pg$species == 'Adelie', ]$bill_length_mm,
    pg[pg$species == 'Adelie', ]$bill_depth_mm)

df <- na.omit(penguins)
prop.table(table(df$species))
tapply(df$species,
       INDEX = list(df$species),
       FUN = length

install.packages('gmodels')
CrossTable(df$island, df$species,
           prop.t = F, prop.chisq = T)

