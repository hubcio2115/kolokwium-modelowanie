#Rozkład normalny wielowymiarowy
library(ggplot2)
library(ggExtra)
library(mnormt)
library(MASS)
library(QRM)
library(evir)

#Przyklad 1 (rozklad normalny - wykres gestosci)
#==========
library(mnormt)

#a) rozklad normalny standardowy N(0,0,1,1,0) -- wykres gestosci
#----------------------------------------------------------------
#wartosc oczekiwana/macierz kowariancji
mu    <- c(0, 0)                       
Sigma <- matrix(c(1, 0, 
              0, 1), nrow = 2)

#obliczamy wartosci gestosci na siatce punktow [-x0,x0] x [-y0,y0]
#rozmiar siatki dobieramy z reguly trzech sigm, dla rozkladu normalnego
s1 <- s2 <-  1  #odchylenia standardowe 
x     <- seq(-3*s1, 3*s1, 0.25) 
y     <- seq(-3*s2, 3*s2, 0.25)

#gestosc rozkladu normalnego o sredniej mu i macierzy kowariancji S
?dmnorm
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)  #sprawdx co oblicza funkcja outer

#wykres gestosci
persp(x,y,z,theta = 30,phi = 30,col="lightblue")

#lub dokladniejszy wykres
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")


#b) rozklad N(0,0,1,1,-0.7) - wykres gestosci, generowanie z rozkladu
#--------------------------------------------------------------------
#wartosc oczekiwana/macierz kowariancji
mu    <- c(0, 0)  
Sigma <- matrix(c(1, -0.7, 
             -0.7,  1), nrow = 2)   

#obliczamy wartosci gestosci na siatce punktow 
s1 <- s2 <-  1
x     <- seq(-3*s1, 3*s1, 0.1) 
y     <- seq(-3*s2, 3*s2, 0.1)

#wykres gestosci
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)

persp(x,y,z,theta = 30,phi = 30,col="lightblue")

#lub dokladniejszy wykres
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")

#generowanie z rozkladu w bibliotece 'mnormt'
set.seed(100)

Z <- rmnorm(1000,mu,Sigma)

head(Z)

#wykres rozrzutu
plot(Z,pch=19)

#mozemy do generowania wykorzystac tez biblioteke MASS
set.seed(100)
Z <- MASS::mvrnorm(1000,mu=mu,Sigma= Sigma)

#Zadanie. Narysuj gestosci i wygeneruj proby licznosci N=1000
#z rozkladow N(0,0,1,1,0.9) i  N(0,0,1,2,0.5). Zrob wykresy rozrzutu.


#Przyklad 2 (rozklady brzegowe)
#===========
#a) wykresy gestosci rozkladu N(0,0,2,3,0.5) i rozkladow brzegowych N(0,2^2), N(0,3^2)
mu <- rep(0,2)
Sigma <- matrix(c(4, 3, 
                  3, 9), nrow = 2) 
Sigma
#obliczamy wartosci gestosci na siatce punktow 
s1 <- 2;  s2 <- 3
x     <- seq(-3*s1, 3*s1, 0.2) 
y     <- seq(-3*s2, 3*s2, 0.2)

#wykres gestosci wektora i gestosci brzegowych
par(mfrow=c(3,1))
f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")

curve(dnorm(x,0,2),xlim=c(-4*s1,4*s1))
grid()

curve(dnorm(x,0,3),xlim=c(-4*s2,4*s2))
grid()

#b) generujemy probe licznosci N=1000 i tworzymy wykres rozrzutu
set.seed(100)

Z <- rmnorm(1000,mu,Sigma)

Z <- as.data.frame(Z) 
colnames(Z) <- c('x','y')
head(Z)

library(ggplot2)
library(ggExtra)

#wykres rozrzutu 
p <-  ggplot(Z, aes(x=x, y=y))+
  geom_point()
p

#wykres rozrzutu z histogramami rozkladow brzegowych
p2 <- ggMarginal(p, type="histogram")
p2


#Przyklad 3 
#===========
#=== a) 
#estymacja: wektora wartości oczekiwanych, 
#macierzy kowariancji (estymator nieobciążony)

Kursy <- read.csv("~/0InstInformatyki/2022MM/Wyklady/Dane/Kursy.csv")
kursy <- Kursy[,c(2,3)]

#wykres rozrzuty z histogramami dla rozkladow brzegowych
p <-  ggplot(kursy, aes(x=A, y=B)) + geom_point()
ggMarginal(p, type="histogram")

#estymacja wektora srednich, macierzy kowariancji, macierzy korelacji 
mu <- colMeans(kursy); mu

Sigma <- cov(kursy)             #estymator nieobciazony

n <- dim(kursy)[1]; n
Sigma_ob <- (n-1)*cov(kursy)/n  #estymator obciążony

Sigma; Sigma_ob

P <- cor(kursy)  #macierz korelacji
P

#=== b) odleglosc Mahalanobisa pierwszej pary cen od sredniej
x <- kursy[1,]
x; mu

?mahalanobis
sqrt(mahalanobis(x,mu,Sigma))

#=== c) kwadraty odleglosci Mahalanobisa
#obliczamy kwadrat odleglosci mahalanobisa kazdej pary cen od wektora srednich
dM <- mahalanobis(kursy,mu,Sigma)

#wyniki na histogramie
par(mfrow=c(1,1))
hist(dM,prob=TRUE)

#=== d) testowanie normalnosci rozkladu dwuwymiarowego (wykres diagnostyczny)
#Jesli rozklad kursow jest dwuwymiarowym rozkladem normalnym: F= N(mu,Sigma),
#to kwadrat odleglosci Mahalanobisa ma rozklad chi(2).

#--- QQ-plot (porownanie kwantyli empirycznych z proby dM oraz kwantyli rozkladu chi(2))
n <- dim(kursy[1]); n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

#Punkty ukladaja sie blisko prostej y=x, co wskazuje na to, ze rozklad
#kwadratu odleglosci Mahalanobisa, moze byc rozkladem chi(2).

#--- test zgodnosci oparty na statystyce Kolmogorowa-Smirnowa 
#wykorzystujemy funkcje ks.test i zaimplementowany w niej  algorytm 
#do obliczania wartsci p-value) 
ks.test(dM,'pchisq',2,exact=TRUE)

#p-valu=0.945 > 5%, zatem na poziomie istotnosci 5%, nie ma podstaw do odrzucenia hipotezy, ze rozklad
#kwadratu odleglosci jest rozkladem chi(2). Nie ma zatem podstaw (w tym tescie), do odrzucenia
#hipotezy o normalnosci dwuwymiarowego rozkladu kursow.


#Przykład 4 (analiza log-zwrotow spolek Siemens i BMW)
#==========
#=== log-zwtoty
#Jesli x1,x2,...,xn sa kursami w kolejnych dniach, to dzienne log-zwroty definiujemy jako
#ln(x2/x1),ln(x3/x2),...,ln(xn/x_(n-1))

library(evir)
#dzienne log-zwroty (2.01.1973-23.07.1996)
data(siemens)  
data(bmw)
?siemens 
?bmw

head(siemens)
head(bmw)

df <- data.frame(siemens=siemens,bmw=bmw)

#wykres rozrzutu z histogramami rozkladow brzegowych
p <-  ggplot(df, aes(x=siemens, y=bmw)) + geom_point()
ggMarginal(p, type="histogram")

#=== a) estymujemy parametry rozkladu normalnego 
#wektor srednich, macierz kowariancji/korelacji
mu <- colMeans(df)
Sigma <- cov(df)      #estymator nieobciażony macierzy korelacji
P <- cor(df)
mu; Sigma; P

#=== b) porownujemy dwa wykresy rozrzutu:
#wykres rozrzutu danych (kursow), 
#wykresem rozrzutu proby licznosci danych, wygenerowanej z rozkladu N(mu,Sigma)

#generujemy probe z rozkladu N(mu,Sigma)
n <- nrow(df); n

set.seed(100)
Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma)

#wykresy rozrzutu
par(mfrow=c(1,2))
plot(df, xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(Z,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))

#=== c) QQ-ploty Mahalanobisa/test chi(2)
dM <- mahalanobis(df,mu,Sigma)

n <- dim(df)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

#testujemyhipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2)
#ks.test(dM,'pchisq',2,exact=TRUE)
ks.test(dM,'pchisq',2)

#p-value < 5%, zatem odrzucamy hipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2),
#co skutkuje tez odrzuceniem hipotezy o normalnosci rozkladu log-zwrotow

#=== d) test Mardii w bibliotece QRM
library(QRM)
MardiaTest(df)

#lub w bibliotece MVN, 
#jednoczesnie z testowaniem normalnosci rozkladow brzegowych
#z wykorzystaniem statystyki Andersona-Darlinga
library(MVN)

par(mfrow=c(1,2))
result = mvn(data = df, mvnTest = "mardia",
             univariateTest = "AD", univariatePlot = "qq",
             multivariatePlot = "qq")

result

#=== c) material dodatkowy, porownanie rozkladu normalnego z rozkladem t

#Uwaga. Rozklad dwuwymiarowy t, definiujemy analogicznie do rozkladu normalnego,
#wychodzac od wektora (Z1,Z2) zmiennych niezaleznych o rozkladach jednowymiarowych t-Studenta.

# Estymujemy  parametry rozkladow normalnego i t w bibliotece QRM
#----------------------------------------------------------------
library(QRM)
library(evir)
X <- cbind(bmw,siemens)

#rozklad normalny dwuwymiarowy
mu.n <- colMeans(X)
Sigma.n <- cov(X)
mu.n; Sigma.n

#rozklad dwuwymiarowy t
fit.t <- fit.mst(X, method = "BFGS") 
mu.t <- fit.t$mu 
Sigma.t <- as.matrix(fit.t$Sigma) 
nu <- fit.t$df #stopnie swobody
mu.t; Sigma.t; nu

#Porownujemy wykresy rozrzutu
#----------------------------
#Generujemy dwie proby licznosci danych z wyestymowanych rozkladow 
#i porownujemy wykresy rozrzutu (library(mnormt)).
n <- dim(X)[1]; n

X.n <- rmnorm(n,mu.n,Sigma.n)
X.t <- rmt(n,mu.t,Sigma.t,df=nu)

#analiza porownawcza na wykresach
par(mfrow=c(1,3))
plot(X, xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(X.n,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(X.t,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))

