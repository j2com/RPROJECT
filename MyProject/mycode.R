3 + 4
print("Hello, R!")

x = 3
3 = x
x <- 3
3 -> y

x <- 3
y <- 5
z <- x+y
print(z)
z

getwd()
plot(iris)
?iris
View(iris)

library(cowsay)

say("안녕, 난 주니온이야!")
say("안녕", by = 'chicken')

table(iris$Species)
barplot(table(iris$Species), 
        col = 'tomato',
        main = '품종의 막대그래프',
        xlab = '품종',
        ylab = '개수')

mean(iris$Petal.Width)
var(iris$Petal.Width)
sd(iris$Petal.Width)

hist(iris$Petal.Width, 
     col = 'tomato',
     main = 'Histogram of iris$Petal,Width',
     xlab = 'iris$Petal,Width',
     ylab = 'Frequency')

hist(mtcars$hp, col = 'orange',
     xlim = c(0,400),
     ylim = c(0,12))

plot(mtcars$hp, mtcars$mpg,
     col = 'tomato',
     pch = '+')

str(cars)
?cars

plot(cars$speed, cars$dist,
    col = 'tomato',
    pch = 'x',
    cex = 0.5)

str(cars)
summary(cars)

score <- 91

if (score >= 90){
    grade <- 'A'
} else if (score >= 80){
    grade <- 'B'
} else {
    grade <- 'F'
}
grade

n <-  15
if (n %% 15 == 0) {

    order <-  '피자나라치킨공주' 
} else if (n %% 5 == 0) {
    order <-  '치킨' 
} else if (n %% 3 == 0) {
    order <-  '피자' 
} else {
    order <-  '다이어트'
}    
order

v <- c(10, 20, 30, 40, 50, 60, 70)
v

v[c(1,3,4,6)]
v[1:7]
v[-1]
v[-2:-5]
v[8]
v[6:8]
v[-1:3]
v[-(1:3)]

v[1:3] <- 100
v
v[1:3] <- c(100,200,300)
v

v > 100  
v + 1

c(10,20) + c(30,40)
c(10,20) + 30

v[v > 30]

1:9 + 1:2

rep(1:3, times = 3)
v[1:3] <- c(10,20,30)
v

v[c(T,F)]
!v < 30 
# 1에서 100까지의 수 중에서 7의 배수의 합은
a = 1:100
sum(seq(7,100,7))
sum(a[a%%7==0])

v <- c()
v
v <- c(v,1)
v <- c(v,2)
v
for (i in 1:10) {
  v[i] <-  i
}
v <- c(10,20,30)
v

v[7] <- 70
v

c('male','female','male','female')

# factor로 변환

f <- factor(c('male','female','male','female'))
levels(f)

f[1]
f[f=='male']
f[6] <- 'male'
f
f[7] <- 'TG' # factor 의 범주에 속하지 않아 NA값 생성
f
f <- factor(c(1, 2, 1, 2),
            levels = 1:3,
            labels = c('male','female','TG'))
f
v.1 <- c(1,2,3)
v.2 <- c('F','F','M')
# 벡터의 type은 같아야 하고 list의 각각의 벡터는 type이 같아도 됨
lst <- list(id = v.1, gender = v.2)
lst

lst$id
lst$gender

v <- 1:10
which(v%%3==0)

v <- c(10,20,30,40,50)
which(v>30)
v[which(v>30)]

n <- 32
# n의 약수를 모두 출력하시오.
# 반복문은 사용하지 마시오.

v <-  1:32
length(v[n%%v == 0])

str(iris)(
iris[ 1:5 , 1:2]
iris[ 1:5 , -5]

iris[iris$Sepal.Length <= 5, -5]
nrow(iris[iris$Sepal.Length < 5, -5])

# Petal.Length가 평균보다 큰 데이터의 Petal.width 평균값

mean(iris[iris$Petal.Length>mean(iris$Petal.Length), 4]) 

my.fun <- function (x,y,z) {
  cat(x, y, z, '\n')
  return(x + y * 2 + z * 3)
}
my.fun(z = 1, 2, x = 3)

my.fun <- function (x,y,z=0) {
  cat(x, y, z, '\n')
  return(x + y * 2 + z * 3)
}
my.fun(1, 2)

divisor <-  function (n) {
  v <- 1:n
  v[n %% v ==0]
}

divisor(n=32)

#asdadadads
