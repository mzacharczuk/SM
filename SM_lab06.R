## Zadanie 10.

n <- 20
p0 <- 0.2
alpha <- 0.02

F <- pbinom(0:n, n, p0)
Fk <- data.frame(k, F = 1- round(F, 10))
Fk
Fk[8:9,]

#k = 8

# rozmiar
k <- 8
1 - pbinom(k, n, p0) #0.009981786

#metoda symulacyjna
m <- 10000
wyniki <- replicate(m,
                    {
                      x = rbinom(n, 1, p0)
                      T = sum(x)
                      T > k
                    })

rozmiar <- sum(wyniki)/m
rozmiar

#d) test zrandomizowany
#fi(X) = 1 gdy T>8, 0, T<8, gamma T=8

#0.02

gamma <- (alpha - (1-pbinom(k, n, p0)))/dbinom(k, n, p0)
gamma #0.4520676

m <- 10000
wyniki <- replicate(m,
                    {
                      x <- rbinom(n, 1, p0)
                      T = sum(x)
                      (T>k) | (T==k  & runif(1) < gamma)
                    })

rozmiar <- sum(wyniki)/m
rozmiar

## Zadanie 4. 
#model 2, bo jest rozklady normalny i sigma1 = sigma2
#H0: mu1 = mu2
#H1: mu1 < mu2

x=c(145,150,153,148,141,152,146,154,139,148)
y=c(152,150,147,155,140,146,158,152,151,143,153)

#rêcznie
mean1 <- mean(x)
mean2 <- mean(y)
n1 <- length(x)
n2 <- length(y)
var1 <- var(x)
var2 <- var(y)
alpha <- 0.05

r <- ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2)
T <- (mean1 - mean2)/sqrt(r * (n1+n2)/(n1*n2))
T #-0.9466366

T < -qt(1-alpha, n1+n2-2) #FALSE => na poz. ist. 0.05 nie mamy podstaw do odrzucenia H0

t.test(x, y, alternative = "less", var.equal = TRUE)
#t = -0.94664 
p <- 0.1779
p < alpha #FALSE

##  Zadanie 14.
data(chickwts)
head(chickwts)
x <- chickwts$weight[chickwts$feed == "meatmeal"]
y <- chickwts$weight[chickwts$feed == "casein"]

t.test(x, y, var.equal = TRUE, alt = "greater")
p <- t.test(x, y, var.equal = TRUE, alt = "greater")$p.value
alpha <- 0.05
p < alpha #FALSE
# na poz. ist. 0.05 nie odrzucamy H0

#rêcznie
mean1 <- mean(x)
mean2 <- mean(y)
n1 <- length(x)
n2 <- length(y)
var1 <- var(x)
var2 <- var(y)
alpha <- 0.05

r <- ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2)
T <- (mean1 - mean2)/sqrt(r * (n1+n2)/(n1*n2))
T #-1.72937
#t = -1.7294

T > qt(1 - alpha, n1+n2-2) #FALSE

## Zadanie 17.
#H0: mu1 = mu2
#H1: mu1 < mu2

library(MASS)
data(crabs)
x <- crabs$CW[crabs$sp=="B"]
y <- crabs$CW[crabs$sp=="O"]
t.test(x, y, var.equal = TRUE, alt = "less") 
#t = -3.1156


#0.001054
0.001054 < 0.04
# na poz. ist. 0.04 odrzucamy H0 na rzecz H1








