table(iris$Species)
# 2-1번
barplot(table(iris$Species), 
        col = 'tomato',
        main = '품종의 막대그래프',
        xlab = '품종',
        ylab = '개수')

# 2-2번
mean(iris$Petal.Width)
var(iris$Petal.Width)
sd(iris$Petal.Width)

hist(iris$Petal.Width, 
     col = 'tomato',
     main = 'Histogram of iris$Petal,Width',
     xlab = 'iris$Petal,Width',
     ylab = 'Frequency')

# 2-3번
hist(mtcars$hp, col = 'orange',
     xlim = c(0,400),
     ylim = c(0,12))

# 2-4번
plot(mtcars$hp, mtcars$mpg,
     col = 'tomato',
     pch = '+')

plot(cars$speed, cars$dist,
     col = 'tomato',
     pch = 'x',
     cex = 0.5)

# 3-1번

v <- c(5,10,15)
area <- function (x) x^2
area(v)

roun <- function (r) {
  v <- c(r*2*3.14,r^2*3.14)
  return(v)
}
sapply(v,roun)
roun(5)

# 3-2번
n <- 8
if ((n %% 3 == 0) & (n %% 5 == 0)) {
  order <- '피자나라치킨공주'
}  else if (n %% 3 == 0) {
    order <- '피자'
}  else if (n %% 5 == 0) {
    order <- '치킨' 
}  else {
    order <- '다이어트'
}
order


ord <- function (i) ifelse(i %% 15 == 0, 'combo',
                             ifelse(i %% 3 == 0, 'Pizza',
                                    ifelse(i %% 5 == 0, 'Chicken', 'Diet')))

ord(1:15)
rm(order)
# 3-3번
tr = function(a) {
  v <- 1:a
  sum(v^3)
}
n <- c(10,15,20)
S <- sapply(n,tr)
S

cumsum.1 <- function (x) ((x*(x+1)/2)^2)
sapply(n, cumsum.1)

fac <- function(n) {
  mul <- 1
  v <- 1:n
  for (i in v) {
    mul <- mul * i
  }
  return(mul)
}

fac(10)
fac(15)
fac(20)

# 3.4번

food <- function (n) {
  v <- 1:n
  pizza <- 0
  chicken <- 0
  combo <- 0
  diet <- 0
  
  for (i in v) {
    if ((i %% 3 == 0) & (i %% 5 == 0)) {
      order <- '피자나라치킨공주' 
      combo <- combo + 1
}   else if (i %% 3 == 0) {
      order <- '피자' 
      pizza <- pizza + 1
}   else if (i %% 5 == 0) {
      order <- '치킨' 
      chicken <- chicken + 1
}   else {
      order <- '다이어트' 
      diet <- diet + 1
}
    }
  cat("pizza =", pizza, "\n")
  cat("chicken =", chicken, "\n")
  cat("combo =", combo, "\n")
  cat("diet =", diet, "\n")
  
  }
food(15)

# 3.5번
n=5
for (i in 1:n) {
  for (j in 1:n) {
    cat("*")
}
  cat("\n")
}

for (i in 1:n) {
  for (j in 1:i) {
    cat("*") 
  }
  cat("\n")
}

for (i in 1:n) {
  for (j in 1:n) { 
      cat("*")

  if ((i==2) | (i==4)) {
      break
  }
}
cat("\n")
}

# 3.6번

s1 <-  function(n) {
  v <-  1:n
  length(v[n %% v == 0]) == 2
}
(1:10)[sapply(1:10,s1)]

length((1:10)[sapply(1:10,s1)])

div <- function (n) {
  for (i in 1:sqrt(n)) {
    if (n %% i == 0)
      cat(i, n / i, "\n")
  }
}
div(36)

# 3.7번

leng <- function(n) {
  v <- 1:n
    length(v[n %% v == 0]) 
} 


div.cnt = function(n) {
  for (i in 1:n) {
    cat(i,':',leng(i),'\n')
  }
}  

leng(10)
div.cnt(10)
num.max <- function(v) {
  max = 0
  max_value = 0
  for (i in 1:v) {
    if (leng(i) >= max) {
      
      max <- leng(i)
      max_value <- i
    }
    cat(i,':',leng(i),'\n')
    }
  max_value
}
maxval = num.max(10)
maxval
num.max(100)
num.max(1000)

# 4.1번
# 약수
div3.cnt <- function(n) {
  v <- c()
  for (i in 1:n) {
    v1 <- 1:i
    v <- c(v,length(v1[i %% v1 == 0]))
  }    
  v
}

# 약수의 개수 sum()함수
div2.cnt <- function(n) {
  a <- c()
  for (i in 1:n) {
    if (length(v1[i %% v1 == 0]) == 2){
      a <- c(a,1)
    }
  }
  sum(a)
}

div3.cnt(15)
div2.cnt(15)

# 약수의 개수 2인 원소 인덱스 which사용

div4.cnt <- function(n) {
  v <- c()
  for (i in 1:n) {
    v1 <- 1:i
    v <- c(v,length(v1[i %% v1 == 0]))
  }    
  which(v==2)
}
div4.cnt(15)
# 1~15 소수의 개수
length(div4.cnt(15))

# 4.2번
v1 <- c('A', 'B', 'C', 'D', 'E')
v2 <- c(163, 175, 182, 178, 161)
v3 <- c(65, 87, 74, 63, 51)
v4 <- factor(c('A', 'B', 'AB', 'O', 'A'))

lst <- list('변량' = v1, 'height' = v2, 'weight' = v3, 'blood' = v4)
str(lst)
mean(lst$height)
mean(lst$weight)
table(lst$blood)
div.cnt2 <- function(n) length((1:n)[sapply(1:n, function(n) length((1:n)[n %% (1:n) == 0])==2)])
#length((1:n)[n %% (1:n) == 0])==2

# 5.1번
div.count <- function(n) sapply(1:n, function(n) length((1:n)[n %% (1:n) == 0]))
div.count(15)

# 5.2번
#소수의 개수
prime.cnt <- function(n) (1:n)[sapply(1:n, function(n) length((1:n)[n %% (1:n) == 0])==2)]
prime.cnt(10)
length(prime.cnt(100))

is.prime <- function(n) {
  (length((1:n)[n %% (1:n) == 0]) == 2) 
}
is.prime(10)

cnt <- function(n) sum(sapply(1:n, is.prime))
cnt(10)

div <- function (n) {
  for (i in 1:sqrt(n)) {
    if (n %% i == 0)
      cat(i, n / i, "\n")
  }
}
div(30)

div1 <- function(n) {
  length((1:sqrt(n))[n %% (1:sqrt(n)) == 0]) == 1
}
sum(sapply(1:15,div1))-1

# 6.1번
df <- data.frame(state.x77)
df
str(df)
dim(df)
table(df)
nrow(df)
ncol(df)
class(df)
# 각 주별 소득 평균
mean(df$Income)
# 인구 10000이상 주 의 소득
df[df$Population>=10000,1:2]
# 플로리다 주의 인구와 소득
df[rownames(st) == 'Florida', 1:2]
# 인구가 1,000보다 작고, 소득이 4,436보다 작은 주의 모든 정보를 출력
df[(df$Population<1000)&(df$Income<4436),]
# 문맹률(Illiteracy)의 평균
# 소득이 5,000보다 작은 주
mean(df[df$Income<5000,3])
# 소득이 5,000보다 큰 주
mean(df[df$Income>5000,3])

# 6.2번
# 인구가 1,000보다 작고, 소득이 5,000보다 작은 주의 모든 정보
df[(df$Population<1000)&(df$Income)<5000,]
# 소득 문맹률 관계(음의 상관관계)
plot(df$Income,df$Illiteracy, pch=19, col = 'orange')
# 6.3번
df <- read_excel('scores.xlsx')
df
df$Sum <- df$Kor + df$Eng + df$Math
df$Mean <- df$Sum/3
df
getwd()
write.csv(df, file = 'result.csv', row.names = F)
# 7.1번
install.packages('mice')
data()
df <- data.frame(nhanes)
df
# 관측값
nrow(df)
# 변수
ncol(df)
# NA 포함되지 않은 관측값
df[complete.cases(df),]
# NA 포함된 관측값
df[!complete.cases(df),]
# NA 포함된 관측값 개수
nrow(df[!complete.cases(df),])
# 변수별로 NA 개수
df
nrow(df[is.na(df$age),])
nrow(df[is.na(df$bmi),])
nrow(df[is.na(df$hyp),])
nrow(df[is.na(df$chl),])
aggr(is.na(df))
aggr(df, prop = F, numbers = T, sortVar = T)
na.omit(df)
# 7.2번
df <- data.frame(iris)
df
boxplot(Petal.Length ~ Species, data = iris, 
        col = 'orange',
        border = 'brown',
        pch = 19)
# 7.3번
# with( )는 데이터 프레임 또는 리스트 내 필드를 필드 이름만으로 접근할 수 있게 해주는 함수
df1 <- with(iris, iris[Species == 'setosa',])
boxplot.stats(df1$Petal.Length)
out.set <- boxplot.stats(df1$Petal.Length)$out
df2 <- with(iris, iris[Species == 'versicolor',])
boxplot.stats(df2$Petal.Length)
out.ver <- boxplot.stats(df2$Petal.Length)$out
# 이상치 NA로 변경
df1[df1$Petal.Length %in% out.set,] <- NA
df2[df2$Petal.Length %in% out.ver,] <- NA
# NA 제거
df1.no.out.set <- na.omit(df1)
nrow(df1.no.out.set)
df2.no.out.set <- na.omit(df2)
nrow(df2.no.out.set)
# 8.1번
df <- data.frame(state.x77)
sort(df$Population)
sort(df$Income, decreasing = T)
ord <- order(df$Population, df$Illiteracy,decreasing = T)
df[ord,c(1,3)]
# 8.2번
df <- data.frame(mtcars)
df
# split 함수 : 데이터 프레임을 범주형 변수를 기준으로 여러 개로 분할
df.split <- split(df, f = df$gear)
df.split
df.34 <- rbind(df[df$gear==3,],df[df$gear==4,])
df.34

# 8.3번
# subset함수 subset = 조건, select = 컬럼 선책
df.air <- data.frame(airquality)
df <- subset(df, select = c(1:4))
df
# aggregate() 함수로 mean() 함수를 범주를 Month
# NA 값에 대해서는 na.rm = T로 매개변수값을 지정
aggregate(df,
          by = list(df.air$Month),
          FUN = mean,
          na.rm = T)

df.day <- aggregate(df,
          by = list(df.air$Day),
          FUN = sd,
          na.rm = T)

# 9장
pg <- data.frame(penguins)
pg
str(pg)
dim(pg)
summary(penguins[, c(1, 2, 7)])
aggr(pg, numbers = T, prop = F)
pg <- na.omit(pg)
aggr(pg, numbers = T, prop = F)
dim(pg)

# table table 값들의 빈도수를 보여줌
table(pg$island)
barplot(table(pg$island))

table(pg$species)
barplot(table(pg$species))

table(pg$sex)
barplot(table(pg$sex))
# 3가지 범주형 변수에 대한 막대그래프
par(mfrow = c(1,3))
barplot(table(pg$island), col = c('pink','green','blue'))
barplot(table(pg$species), col = c('skyblue','violet','orange'))
barplot(table(pg$sex), col = c('yellow','red'))
par(mfrow = c(1,1))

# 3가지 범주형 변수에 대해서 각 범주의 비율
proportions(table(pg$island))
proportions(table(pg$species))
proportions(table(pg$sex))

str(pg[,3:6])
summary(pg[,3:6])
par(mfrow = c(2,2))
hist(pg$bill_length_mm, col = 'skyblue')
hist(pg$bill_depth_mm, col = 'blue')
hist(pg$flipper_length_mm, col = 'orange')
hist(pg$body_mass_g, col = 'pink')
par(mfrow = c(1,1)

# hist >> border = T 디폴트 값
par(mfrow=c(1,1))
hist(pg$bill_depth_mm)
# psych 패키지 describe함수 (기술통계량 산출)
install.packages('psych')
describe(pg)[, c(1:3,8:9)]
# ggplot
install.packages('ggplot')
aggregate(pg[,3:6],
          by = list(species = pg$species),
          FUN = mean)
# aggregate() 함수를 이용 aggregate(기준열 ~ 그룹화할 열, x, 적용할 통계)
# 펭귄의 종별로 부리의 길이와 깊이, 날개의 길이, 체질량의 평균)

aggregate(bill_length_mm ~ species, pg, mean)
aggregate(bill_depth_mm ~ species, pg, mean)          
aggregate(flipper_length_mm ~ species, pg, mean)
aggregate(body_mass_g ~ species, pg, mean)
# pg 결측치 개수
nrow(pg[is.na(pg$species)==T,])
nrow(pg[is.na(pg$island)==T,])
nrow(pg[is.na(pg$bill_length_mm)==T,])
nrow(pg[is.na(pg$bill_depth_mm)==T,])
nrow(pg[is.na(pg$flipper_length_mm)==T,])
nrow(pg[is.na(pg$body_mass_g)==T,])
nrow(pg[is.na(pg$sex)==T,])

# VIM 패키지의 aggr() 함수를 이용하여 결측값의 패턴을 확인 prop = 비율
pg <- data.frame(penguins)
pg
aggr(pg, prop = F, numbers = T, sortVar = T)
# complete.cases NA 개수 확인
sum(!complete.cases(pg))
# 결측치 제외 pg 저장
pg <- pg[complete.cases(pg),]
pg
# 4가지 수치형 변수에 대해 박스플롯
par(mfrow = c(1,4))
boxplot(pg$bill_length_mm,
        col = 'orange',
        border = 'black',
        pch = 19)
boxplot(pg$bill_depth_mm,
        col = 'orange',
        border = 'black',
        pch = 19)
boxplot(pg$flipper_length_mm,
        col = 'orange',
        border = 'black',
        pch = 19)
boxplot(pg$body_mass_g,
        col = 'orange',
        border = 'black',
        pch = 19)
par(mfrow = c(1,1))

boxplot(flipper_length_mm ~ species,
        data = pg, col = 2:4)
adelie <- split(df, f = df$species)$Adelie
adelie
outlier <- boxplot.stats(adelie$flipper_length_mm)$out
df[df$flipper_length_mm %in% outlier,]
dim(df[df$flipper_length_mm %in% outlier,])
# 펭귄의 종별로 박스플롯
par(mfrow = c(2,2))
boxplot(bill_length_mm ~ species, data = pg, 
        col = c('pink','skyblue','green'),
        border = 'brown',
        pch = 19)
boxplot(bill_depth_mm ~ species, data = pg, 
        col = c('pink','skyblue','green'),
        border = 'brown',
        pch = 19)
boxplot(flipper_length_mm ~ species, data = pg, 
        col = c('pink','skyblue','green'),
        border = 'brown',
        pch = 19)
boxplot(body_mass_g ~ species, data = pg, 
        col = c('pink','skyblue','green'),
        border = 'brown',
        pch = 19)
par(mfrow = c(1,1))
str(pg)
# boxplot.stats() 함수를 이용하여 Adelie 펭귄의 날개 길이에서 나타난 이상치의 값과 Chinstrap 
# 펭귄의 체질량에서 나타난 이상치의 값
boxplot.stats(pg[pg$species=='Adelie',5])$out
boxplot.stats(pg[pg$species=='Chinstrap',6])$out
# order() 함수를 이용하여 penguins 데이터셋을 날개의 길이를 기준으로 오름차순으로 정렬
# rownames 로 인덱스 번호 초기화
ord <- order(pg$flipper_length_mm, decreasing = F)
pg.ord <- pg[ord,]
rownames(pg.ord) <- NULL
pg.ord
# penguins 데이터셋을 날개의 길이를 기준으로 오름차순으로 정렬하되, 
# 날개의 길이가 같으면 체질량을 기준으로 내림차순으로 정렬해보자.

ord2 <- order(pg$flipper_length_mm, -pg$body_mass_g) # 뒤에 decreaing = F시 둘다 오름차순 정렬됨
pg.ord2 <- pg[ord2,]
rownames(pg.ord2) <- NULL
View(pg.ord2)
# penguins 데이터셋에서 부리의 길이가 가장 긴 10개의 데이터를 출력

# 10장
