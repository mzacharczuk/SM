## Zadanie 8.

k <- 578
n <- 1014

p <- k/n
p

prop.test(k, n, conf.level = 0.95)$conf.int #duze proby
#0.5388446 0.6006578

binom.test(k, n, conf.level = 0.95)$conf.int #malych prob
#0.5388883 0.6007435

binom::binom.confint(k, n, conf.level=0.95)


## Zadanie 9.
k <- 3
n <- 12
k/n

binom.test(k, n)$conf.int #0.05486064 0.57185846


## Zadanie 11.
k <- 19
n <- 150

prop.test(k, n, conf.level = 0.96)$conf.int #0.07828824 0.19665016
binom.test(k, n, conf.level = 0.96)$conf.int #0.07613137 0.19387666


## Zadanie 12.
data(chickwts)
head(chickwts)
names(chickwts)

x <- chickwts$weight[chickwts$feed == "soybean"]
var(x)
library(TeachingDemos)
sigma.test(x, conf.level = 0.93)$conf.int #1613.858 7039.401

#rêcznie
alpha <- 0.07
n <- length(x)

c((n-1)*var(x)/qchisq(1-alpha/2, n-1), (n-1)*var(x)/qchisq(alpha/2, n-1)) #1613.858 7039.401

## Zadanie 13.
data(faithful)
head(faithful)

n <- length(faithful$waiting)
alpha <- 0.01
kw <- qnorm(1-alpha/2)
sr <- mean(faithful$waiting)
odch <- sd(faithful$waiting)

c(sr - kw*odch/sqrt(n), sr + kw*odch/sqrt(n)) #68.77376 73.02036

## Zadanie 14.
data(Orange)
head(Orange)

x <- Orange$circumference

sd(x)

sqrt(sigma.test(x, conf.level = 0.99)$conf.int) #43.65408 82.52001


#rêcznie
alpha <- 0.01
n <- length(x)

c(sqrt((n-1)*var(x)/qchisq(1-alpha/2, n-1)), sqrt((n-1)*var(x)/qchisq(alpha/2, n-1))) #43.65408 82.52001

## Zadanie 15. 
library(MASS)
data(Pima.te)
head(Pima.te)

n <- length(Pima.te$type)
n 
k <- sum(Pima.te$type == "Yes")


p <- k/n
p #0.3283133

prop.test(k, n, conf.level = 0.95)$conf.int #0.2785847 0.3820858

# >=35 lat
n <- length(Pima.te$type[Pima.te$age >= 35])
n
k <- sum(Pima.te$type[Pima.te$age >= 35] == "Yes")
k

p <- k/n
p #0.5196078
prop.test(k, n, conf.level = 0.95)$conf.int #0.4189563 0.6187654


## Lista nr 4
mu0 <- 1.2
sigma <- 0.07

x <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36, 
       1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)

#statystyka testowa
T <- (mean(x) - mu0)/sigma*sqrt(length(x))
T #4.823518

#obszar krytyczny: [z(1-alpha), +inf)

#T \in obszar krytyczny
alpha <- 0.04
T > qnorm(1-alpha) #4.823518

# p - wartoœæ
# najmniejszy poziom istotnoœci, przy którym zaobserwowana wartoœæ statystyki tesowej prowadzi do odrzucenia H0

pvalue <- 1-pnorm(T)
pvalue < 0.04 #TRUE
#odrzucamy H0

## Zadanie 2.
x <- c(142, 151, 148, 151, 145, 150, 141)

#H0: mu = mu0
#H1: mu != mu0

mu0 <- 150
alpha <- 0.05

T <- (mean(x) - mu0)*sqrt(length(x))/sd(x)
T #-1.970369

#obszar krytyczny
kw <- qt(1-alpha/2, length(x)-1)

T < -kw | T > kw #FALSE
#nie odrzucamy H0 na poziomie istotnoœci 0.05

t.test(x, mu = 150) #142.9542 150.7601
#p-value = 0.0963
p < alpha #FALSE => nie odrzucamy H0


p <- t.test(x, mu=150)$p.value
p

#H0: sigma =4 
#H1: sigma!=4

sig0 <- 4
alpha <- 0.05
T <- (length(x)-1)*var(x)/(sig0^2) #6.678571

kw1 <- qchisq(alpha/2, length(x)-1)
kw1 #1.237344

kw2 <- qchisq(1 - alpha/2, length(x)-1)
kw2 #14.44938

T < kw1 | T > kw2 #FALSE -> nie odrzucamy H0 na poziomie istotnoœci 0.05



