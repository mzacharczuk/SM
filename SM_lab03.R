## Zadanie 14.
dane <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")
head(dane)

przyspA <- dane$przysp[dane$producent == 1]
przyspJ <- dane$przysp[dane$producent == 3]

boxplot(przyspA, przyspJ, main = "Przyspieszenie", names = c("Ameryka", "Japonia"))

## Lista nr 3
x20 <- rnorm(n=20)
x100 <- rnorm(n=100)

#F_n(t) = #{X_i; X_i <=t, i=1, ...,n}/n

plot(ecdf(x20))

curve(pnorm(x), -1.5, 1.5, add = T, col = "red", lwd = 2)


plot(ecdf(x100))

curve(pnorm(x), -4, 3, add = T, col = "red", lwd = 2)


## Zadanie 2.
x <- rcauchy(500, 0, 1)

#a)
res_mean <- numeric()
res_median <- numeric()

for(i in 1:500){
  res_mean[i] <- mean(x[1:i])
  res_median[i] <- median(x[1:i])
}
plot(1:500, res_mean, type = "l", xlab = "licznosc proby", ylab = "", lwd = 2)
lines(1:500, res_median, col = "red", lwd = 2)
abline(h = 0, lty = 3)

#b) 
x <- rcauchy(500, 0, 1)
res_std <- numeric()
res_sqr <- numeric()
for(i in 2:500){
  res_std[i-1] <- sd(x[1:i])
  res_sqr[i-1] <- IQR(x[1:i])/2
}
plot(2:500, res_std, type = "l", xlab = "licznosc proby", ylab = "", ylim = c(0,35))
lines(2:500, res_sqr, col = "red")
abline(h=1, lty=3)

## Zadanie 3.
x <- rnorm(500, 0, 1)

#a)
res_mean <- numeric()
res_median <- numeric()

for(i in 1:500){
  res_mean[i] <- mean(x[1:i])
  res_median[i] <- median(x[1:i])
}
plot(1:500, res_mean, type = "l", xlab = "licznosc proby", ylab = "", lwd = 2)
lines(1:500, res_median, col = "red", lwd = 2)
abline(h = 0, lty = 3)

#b)

x <- rnorm(500, 0, 1)
res_std <- numeric()
res_sqr <- numeric()
for(i in 2:500){
  res_std[i-1] <- sd(x[1:i])
  res_sqr[i-1] <- IQR(x[1:i])/1.35
}
plot(2:500, res_std, type = "l", xlab = "licznosc proby", ylab = "")
lines(2:500, res_sqr, col = "red")
abline(h=1, lty=3)


## Zadanie 4.
#replicate(ile razy, {co zrobic})

replicate(3, rnorm(5))

replicate(3, {
  x = rnorm(5)
  mean(x)
})

m <- 10000
n <- 20

estymatory <- replicate(m,
                        {
                          x = runif(n)
                          c(2*mean(x), max(x))
                        })
estymatory[, 1:5]
EMM <- estymatory[1,]
ENW <- estymatory[2,]

#obci¹¿enia
theta <- 1
mean(EMM - theta) #-0.000720448
mean(ENW - theta) #-0.04745622

-theta/(n+1)

#X_{n:n}*(n+1)/n

## b³¹d œredniokwadratowy
#EMM
mean((EMM - theta)^2) #0.01655241

theta^2/(3*n) #0.01666667

#ENW
mean((ENW - theta)^2) #0.004348844

2*(theta^2)/((n+1)*(n+2)) #0.004329004

## Zadanie 5.
m <- 10000
n <- 10
alpha <- 0.05
kw <- qt(1 - alpha/2, n-1)

czy.wpada <- replicate(m,
                       {
                         x = rnorm(n)
                         sr = mean(x)
                         odch = sd(x)
                         (sr - kw*odch/sqrt(n) < 0) & (sr + kw*odch/sqrt(n)>0)
                       }
)

czy.wpada[1:10]
sum(czy.wpada)/m

## Zadanie 6.
n <- 50
sr <- 28.4
odch <- 4.75
alpha <- 0.05
kw <- qnorm(1 - alpha/2)

c(sr - kw*odch/sqrt(n), sr+kw*odch/sqrt(n))

sr + c(-1,1)*(kw*odch/sqrt(n))


## Zadanie 7.
x <- scan(nline=3)
x

n <- 18
alpha <- 0.05
kw <- qt(1 - alpha/2, n-1)
sr <- mean(x)
odch <- sd(x)

c(sr - kw*odch/sqrt(n), sr+kw*odch/sqrt(n)) #329.6482 336.8851

mean(x)
t.test(x, conf.level = 0.95)$conf.int #329.6482 336.8851

#przedzia³ ufnosci dla odchylenia std
sd(x)
sqrt((n-1)*var(x)/qchisq(1-alpha/2, n-1)) #5.460114
sqrt((n-1)*var(x)/qchisq(alpha/2, n-1)) #10.90836

var(x)
c((n-1)*var(x)/qchisq(1-alpha/2, n-1), (n-1)*var(x)/qchisq(alpha/2, n-1)) #29.81285 118.99231

install.packages("TeachingDemos")
library(TeachingDemos)
sigma.test(x)$conf.int #29.81285 118.99231 <- dla wariancji
sqrt(sigma.test(x)$conf.int) #5.460114 10.908360 <- dla odch. std


##Zadanie 10.
data(iris)
head(iris)

x <- iris$Petal.Length[iris$Species == "virginica"]

#a)
mean(x) #5.552
t.test(x, conf.level = 0.99)$conf.int #5.342831 5.761169

#recznie
n <- length(x)
alpha <- 0.01
kw <- qt(1 - alpha/2, n-1)
sr <- mean(x)
odch <- sd(x)
c(sr - kw*odch/sqrt(n), sr+kw*odch/sqrt(n)) #5.342831 5.761169

#b) 95% 
alpha <- 0.05
sd(x)
var(x)
sqrt(sigma.test(x, conf.level = 0.95)$conf.int) #0.4610164 0.6877344
sigma.test(x)$conf.int #0.2125361 0.4729786

c((n-1)*var(x)/qchisq(1-alpha/2, n-1), (n-1)*var(x)/qchisq(alpha/2, n-1)) #0.2125361 0.4729786


## Zadanie 8.
k <- 578
n <- 1014

p <- k/n
p

#dla duzych prob!
prop.test(k, n, conf.level = 0.95)$conf.int #0.5388446 0.6006578

prop.test(k, n, conf.level = 0.95, correct = F)$conf.int

#dla malych prob
binom.test(k, n, conf.level = 0.95) #0.5388883 0.6007435

binom::binom.confint(k, n, conf.level= 0.95)


## Zadanie 9.
k <- 3
n <- 12

binom.test(k, n, conf.level = 0.95)$conf.int #0.05486064 0.57185846

## Zadanie 11.
k <- 19
n <- 150

p <- k/n
p

prop.test(k, n, conf.level = 0.96)$conf.int #0.07828824 0.19665016
binom.test(k, n, conf.level = 0.96)$conf.int #0.07613137 0.19387666


data(iris)
