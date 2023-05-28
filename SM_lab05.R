dane <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")
head(dane)

boxplot(dane$waga)$out


## Zadanie 3.
x <- c(2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914)

#
n <- length(x)
alpha <- 0.03
kw <- qt(1-alpha/2, n-1)
sr <- mean(x)
odch <- sd(x)

#97% przedzia³ ufnoœci dla œredniej
c(sr-kw*odch/sqrt(n), sr+kw*odch/sqrt(n)) #2711.605 2904.062
t.test(x, conf.level = 0.97)$conf.int #2711.605 2904.062

#97% przedzia³ unfoœci dla odchylenia sd

sqrt(c((n-1)*var(x)/qchisq(1-alpha/2, n-1),(n-1)*var(x)/qchisq(alpha/2, n-1)))
#91.56228 242.04084

library(TeachingDemos)
sqrt(sigma.test(x, conf.level = 0.97)$conf.int) #91.56228 242.04084

# b)
#H0: mu = 2900
#H1: mu < 2900

T <- (mean(x) - 2900)*sqrt(length(x))/sd(x)
T #-2.385525

kw <- qt(1-alpha, length(x)-1)
T < -kw #TRUE

#na poziomie istotnosci 0.03 stwierdzamy, ze czas swiecenia zarowek jest istotnie krotszy od 2900h
#czyli odrzucamy H0

t.test(x, mu = 2900, alt = "less")$p.value < 0.03
#na poziomie ist. 0.03 odrzucamy H0

## Zadanie 12.

data(Loblolly)
head(Loblolly)

#H0: mu = 40
#H1: mu != 40

x <- Loblolly$height[Loblolly$age == 15]

t.test(x, mu = 40)$p.value < 0.05
#FALSE
#na poziomie istotnosci 0.05 nie odrzucamy hipotezy zerowej

mu0 <- 40
T <- (mean(x) - mu0)/sd(x)*sqrt(length(x))
T

#obszar krytyczny
alpha <- 0.05
kw <- qt(1-alpha/2, length(x)-1)
T < -kw 
T > kw
#nie odrzucamy H0 na poziomie istotnosci 0.05

## Zadanie 15.
data(Orange)

head(Orange)
#H0: mu = 130
#H1: mu < 130

x <- Orange$circumference
p <- t.test(x, mu = 130, alt = "less", conf.level = 0.9)$p.value
#0.07736046 
p < 0.1 #TRUE => odrzucamy H0 na poz. ist. 0.1
# na poziomie ist. 0.1 

# rêcznie
mu0 <- 130
T <- (mean(x) - mu0)*sqrt(length(x))/sd(x)
T

alpha <- 0.1
kw <- qt(1 - alpha, length(x)-1)
T < -kw #TRUE
#na poz. ist. 0.1 odrzucamy H0

## Zadanie 11.
# H0: mu = 2
# H1: mu >2

n <- 10
mu0 <-2
alpha <- 0.05

#T = (mean(x) - mu0)*sqrt(n)/sd(x)
# obszar krytyczny: [kw, +inf), kw = qt(1-alpha, n-1)

#P(T > kw)

mu <- seq(0, 4, 0.1)
moce <- numeric()
kw <- qt(1 - alpha, n-1)

m <- 1000

for(i in 1:length(mu)){
  wynik = replicate(m,
                    {
                      x = rnorm(n, mu[i], 1)
                      T = (mean(x) - mu0)*sqrt(n)/sd(x)
                      T > kw
                    })
  moce[i] <- sum(wynik)/m
}

moce

plot(mu, moce, type = "l", ylim = c(0,1))
abline(v = 2, lty = 2, col = "red")



#H0: mu = 2
#H1: mu != 2

n <- 10
mu0 <- 2
alpha <- 0.05

#(-Inf,-kw] u [kw, +inf), kw = qt(1-alpha/2, n-1)

mu <- seq(0, 4, 0.1)
moce <- numeric()

kw <- qt(1 - alpha/2, n-1)

m <- 1000

for(i in 1:length(mu)){
  wynik = replicate(m,
                    {
                      x = rnorm(n, mu[i], 1)
                      T = (mean(x) - mu0)*sqrt(n)/sd(x)
                      T < (-1)*kw | T > kw
                    })
  moce[i] <- sum(wynik)/m
}

moce
plot(mu, moce, type = "l", ylim = c(0,1))


# to samo, ale z p-value
alpha <- 0.05
for(i in 1:length(mu)){
  wynik = replicate(m,
                    {
                      x = rnorm(n, mu[i], 1)
                      p = t.test(x, mu = 2, alt = "greater")$p.value
                      p < alpha
                    })
  moce[i] <- sum(wynik)/m
}
moce


for(i in 1:length(mu)){
  wynik = replicate(m,
                    {
                      x = rnorm(n, mu[i], 1)
                      p = t.test(x, mu=2)$p.value
                      p < alpha
                    })
  moce[i] <- sum(wynik)/m
}
moce


## Zadanie 8.
#H0: p = 0.04
#H1: p > 0.04

k <- 14 
n <- 200


p0 <- 0.04

p <- prop.test(k, n, p0, alternative = "greater")$p.value
alpha <- 0.05
p < 0.05
#TRUE
#na poziomie istotnosci 0.05 odrzucamy hipotezê zerow¹

## Zadanie 13.
#H0: p=0.35
#H1: p>0.35

k <- 15+32
n <- 120

p0 <- 0.35

p <- prop.test(k, n, p0, alternative = "greater")$p.value
alpha <- 0.03
p < alpha #FALSE
#na poziomie istotnosci 0.03 nie odrzucamy H0

#wejsciowka -> przedzia³y ufnoœci, estymator punktowy

