# Zadanie 14.
#a) wykladniczy
#b) 
lambda <- 4
1/lambda
1/lambda^2

#c)
pexp(0.5, lambda)

#d) 
dpois(0, lambda)

1 - pexp(1, lambda)

## Zadanie 15.
#(X1, Y1), ... U[0,1]

#Z_i = I(f(X_i)>Y_i)
#MPWL
#1/n sum Z_i -> calka_0^1 f(x) dx

n <- 1000
u <- runif(n)
v <- runif(n)

plot(u, v, pch=16, col = "lightgray")
curve(x * x, 0, 1, col = "darkorange", lwd=2, add = T)
points(u[u*u>v], v[u*u > v], col = "darkorange")

sum(v<u*u)/n


# -----------------------------------------
# lista 2

#Zadanie 2.1
dane <- c(17364, 56128, 11239, 8170)
dane

klasa <- c("panny","mezatki", "wdowy", "rozwodki")

pie(dane, labels = klasa, main = "stan cywilny kobiet w USA")

pi <- dane/sum(dane) * 100
pi

etykiety <- paste(klasa, "-", round(pi, 2), "%")
pie(dane, labels = etykiety)

pie(dane, labels = etykiety, col = rainbow(10))


#wykres s³upkowy
barplot(dane, names = klasa)
opis <- paste(round(pi,2), "%")
barplot(dane, names = klasa, col = rainbow(4), legend = opis)



## Zadanie 2.
dane <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")
head(dane)
t <- table(dane)
pie(t)
barplot(t)


## Zadanie 3.
x <- c(23.3, 24.5, 25.3, 25.3, 24.3, 24.8, 25.2, 24.5, 24.6, 24.1, 24.3, 26.1, 23.1, 25.5, 22.6, 24.6, 24.3, 25.4, 25.2, 26.8)
length(x)

plot(x)
plot(x, type = "b", xlab = "dzien", ylab = "cena")
plot(x, type = "b", xlab = "dzien", ylab = "cena", pch = 1:20)


## Zadanie 4. 
butelki <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=butelki.csv")
head(butelki)

butelki$cisnienie <- butelki$strength * 0.0068947
head(butelki)

#b) 
x <- butelki$cisnienie
hist(x)
hist(x, prob = T)
hist(x, labels = T, col = "blue", main = "")

h <- hist(x)
h

h$mids

#c) 
hist(x)
lines(h$mids, h$counts, type = "l", col="red")


#d)
stem(x)
head(x)

#e)
boxplot(x)
b <- boxplot(x)
b

b$out


#f)
#min, max, mean, median, var, sd
min(x)
summary(x)
var(x)
sd(x)


#wspolczynnik skosnosci
#(1/n*sum((x-mean(x))^3))/sd^3

n <- length(x)
sum((x-mean(x))^3)/(n*sd(x)^3)

e1071::skewness(x)
install.packages("moments")
moments::skewness(x)

#g)
?quantile
quantile(x, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))

#h) 
mean(x, trim = 0.1)
mean(x)

## Zadanie 6.
dane <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")
head(dane)

dane$zp <- 378.5 / (1.609 * dane$mpg)
head(dane)

summary(dane$zp)
mean(dane$zp)
mean(dane$zp, na.rm = T)

x <- dane$zp[!is.na(dane$zp)]
summary(x)

#b) 
stem(x)

#c) 
hist(x)

#d) 
#f_h(x) = sum(K((x-xi)/h))/(n*h) 

hist(x, prob = T)
lines(density(x), col = "blue", lwd = 2)
lines(density(x, bw = "nrd"), col = "green", lty=2, lwd = 2)
lines(density(x, kernel = "triangular"), col = "orange", lty = 3, lwd = 2)
#triangular: K(x) = 1-|x|

#e) 
boxplot(x)

#f)
summary(x)
min(x)

#wspolczynnik zmiennosci
cv <- sd(x) / abs(mean(x))
cv

#g)
quantile(x, c(0.05, 0.1, 0.9, 0.95))

#h)
mean(x, trim = 0.05)


## Zadanie 7.
malo <- dane$zp[dane$zp <= 7]
srednio <- dane$zp[dane$zp > 7 & dane$zp <=10]
duzo <- dane$zp[dane$zp > 10]


licznosci <- c(length(malo), length(srednio), length(duzo))
nazwy <- c("malo", "srednio", "duzo")

barplot(licznosci, names = nazwy)

## Zadanie 8.
head(dane)

zpA <- dane$zp[dane$producent == 1]
zpE <- dane$zp[dane$producent == 2]
zpJ <- dane$zp[dane$producent == 3]

mean(zpA)
mean(zpE, na.rm = T)
mean(na.omit(zpE))
mean(zpJ)

sd(zpA)
sd(na.omit(zpE))
sd(zpJ)

boxplot(zpA, zpE, zpJ, horizaontal = T, names = c("A", "E", "J"))
boxplot(dane$zp ~ dane$producent, horizaontal = T, names = c("A", "E", "J"))

## Zadanie 9.
unique(dane$cylindry)

table(dane$cylindry)

boxplot(dane$zp ~ dane$cylindry, horizaontal = T, names = c(3,4,5,6,8))

tapply(dane$zp, dane$cylindry, sd)
tapply(dane$zp, dane$cylindry, summary)

## Zadanie 10.
zpwag <- dane$zp[dane$waga<2500]

mean(zpwag)
median(zpwag)
var(zpwag)
sd(zpwag)

sum((x-mean(x))^3)/(length(x)*sd(x)^3)

zpwag2 <- dane$zp[dane$waga >= 2500]
boxplot(zpwag, zpwag2)

par(mfrow=c(1,2))
hist(zpwag, prob = T, xlim = c(4,19), ylim = c(0, 0.65))
hist(zpwag2, prob = T, xlim = c(4,19), ylim = c(0, 0.65))


par(mfrow=c(1,1))


#Zadanie 11.
mocrok <- dane$moc[dane$rok>=79 & dane$rok <= 81]
summary(mocrok)

boxplot(mocrok)
b <- boxplot(mocrok)
b$out

#b)
quantile(mocrok, 0.95)
quantile(mocrok, 0.95, na.rm=T)


#https://github.com/mzacharczuk/SM



