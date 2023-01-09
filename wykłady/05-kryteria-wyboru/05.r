#Kryteria wyboru rozkladu
#==========================

#============ 
#Przyklad 1 (statystyka Kolmogorowa-Smirnowa)
#============
#Generujemy proby licznosci n=10 (100) z rozkladow: N(0,1), Exp(1), LN(0,1).
#Obliczamy warosc statystyki Kolmogorowa-Smirnowa (Dn).
set.seed((1))
n <- 10
X <- rnorm(n)
Y <- rexp(n)
Z <- rlnorm(n)

#wykresy dystrybuant teoretycznych i empirycznych
par(mfrow=c(1,1))
plot(ecdf(X))
curve(pnorm(x),col=2,add=T)

plot(ecdf(Y))
curve(pexp(x),col=2,add=T)

plot(ecdf(Z))
curve(plnorm(x),col=2,add=T)

#wartosci statystyki Dn
ks.test(X,pnorm,0,1,exact=TRUE)$statistic
ks.test(Y,pexp,1,exact=TRUE)$statistic
ks.test(Z,plnorm,0,1,exact=TRUE)$statis

#==========
#Przyklad 2 (Rozklad statystyki Dn)
#==========
#Powtorzymy N=10000 razy, generowanie z Przykladu 1.
#Wygenerujemy proby licznosci n=10 z rozkladu N(0,1), Exp(1), LN(0,1) 
#i obliczymy odleglosc miedzy  dystrybuanta empiryczna i teoretyczna. 
N <- 10000
n <- 100

Dnorm <- c()
Dexp <- c()
Dln <- c()

for (i in 1:N) { 
  
  Xn <- rnorm(n) 
  Xe <-rexp(n)
  Xln <- rlnorm(n)
  
  Dnorm[i] <- ks.test(Xn,pnorm,0,1,exact=TRUE)$statistic
  Dexp[i] <- ks.test(Xe,pexp,1,exact=TRUE)$statistic
  Dln[i] <- ks.test(Xln,plnorm,0,1,exact=TRUE)$statistic
}

#Wyniki na histogramach
par(mfrow=c(3,1))
hist(Dnorm,prob=T)
hist(Dexp,prob=T)
hist(Dln,prob=T)

#wartosc oczekiwana i odchylenie standardowe wynikow
D <- list(Dnorm,Dexp,Dln)

for(d in D){
  
  print(round(c(mean(d),sd(d)),2))
}

#============ 
#Przyklad 3.  (dobor rozkladu do proby wygenerowanej ze znanego nam rozkladu)
#============
#generujemy probe licznosci n=10 z rozkladu Exp(1)
#sa to nasze dane z nieznanego rozkladu
set.seed(10)
n <- 100
X <- rexp(n)

#1. Tworzymy histogram
hist(X,prob=TRUE)

#2. Bazujac na ksztalcie histogramu i naszej wiedzy o rozkladach, 
#wybieramy dwie rodziny rozkładów: wykładniczą i log-normalną.
#Estymujemy parametry tych rozkladow, estymatorem MLE.
library(fitdistrplus)

fexp <- fitdist(X,'exp')
fln <- fitdist(X,'lnorm')

#wyniki estymacji/parametry
fexp; fln

lambda <- fexp$estimate; lambda
mu <- fln$estimate[[1]]; sigma <- fln$estimate[[2]]; mu; sigma

#3. Wykresy diagnostyczne 
par(mfrow=c(3,1))
plot.legend <- c('exp','log-norm')

denscomp(list(fexp,fln),legendtext =plot.legend)
cdfcomp(list(fexp,fln),legendtext =plot.legend)
qqcomp(list(fexp,fln),legendtext =plot.legend)

#4. Wartosci statystyki Dn obliczone za pomoca funkcji ks.test
par(mfrow=c(1,1))
plot.legend <- c('exp','log-norm')
cdfcomp(list(fexp,fln),legendtext =plot.legend)

ks.test(X,pexp,lambda,exact=TRUE)$statistic
ks.test(X,plnorm,mu,sigma,exact=TRUE)$statistic

#5. Wartosci statystyk w bibliotece 'fitdistrplus'
gofstat(list(fexp,fln), fitnames = plot.legend)

#Jaki rozklad wybieramy bazując na wartosciach statystyk Dn, CM i AC?

#============ 
#Przyklad 4.  (testowanie rozkladu z Przykladu 3)
#============
#Testujemy hipoteze
#H0: F=Exp(0.98)(F0) przeciwko hipotezie H1: F nie jest rowny Exp(0.98)(F0)

#1. Rozklad statystyki Dn (metoda MC).
#Generujemy N=10000 probek licznosci n=100 z rozkladu F0=Exp(0.98) wybranego w Przykladzie 3 
#i obliczamy odleglosc dystrybuant empirycznych od rozkladu F0 (wartosc statystyki Dn).
N <- 10000
n <- 100

D <- c()

for (i in 1:N) { 
  
  Y <- rexp(n,lambda) 
  D[i] <- ks.test(Y,pexp,lambda,exact=TRUE)$statistic
}

#2. Obliczamy dn wartosc statystyki dla proby X i F0.
dn <- ks.test(X,pexp,lambda,exact=TRUE)$statistic

#wyniki na histogramie
hist(D,prob=T)
points(dn,0,pch=19,col=2)

#3. Obliczamy p-value
p_value <- length(D[D>dn])/N; p_value

#Przyjmujemy poziom istotnosci 5%
alpha <- 0.05
p_value <- alpha
#Na poziomie istotności 5%, nie ma podstaw do odrzucenia hipotezy zerowej (F0=Exp(0.98)),


#============ 
#Przyklad 5.  (dane:'danishuni' - rozklad wielkosci wyplat z portfela polis)
#============
library("actuar")
#library(fitdistrplus)
data("danishuni", package = "fitdistrplus")
head(danishuni)
tail(danishuni)

#straty na wykresach
par(mfrow=c(3,1))
plot(danishuni$Loss)
hist(danishuni$Loss,prob=T)
hist(danishuni$Loss[danishuni$Loss<30],prob=T)

#do analiz wezmiemy straty z grudnia
library(tidyr)
data <- separate(danishuni,Date,c("year","mth","day"), convert=TRUE)
head(data)

Loss <- data[data$mth == 12, 'Loss']

#straty grudniowe na wykresach
par(mfrow=c(2,1))
plot(Loss)
hist(Loss,prob=T)

#A.Wybor jednego z trzech rozkladow.
#----------------------------------
#estymujemy parametry trzech rozkladow: log-normalnego, pareto i gamma
fln <- fitdist(Loss, "lnorm")
fpar <- fitdist(Loss, "pareto")
fg <- fitdist(Loss, "gamma")

#wykresy diagnostyczne
par(mfrow=c(3,1))
denscomp(list(fln,fpar,fg),legend = c("lognormal","Pareto","gamma"))
cdfcomp(list(fln,fpar,fg),legend = c("lognormal","Pareto","gamma"))
qqcomp(list(fln,fpar,fg),legend = c("lognormal","Pareto","gamma"))

#kryteria wyboru rozkladu
gofstat(list(fln,fpar,fg),fitnames = c("lognormal","Pareto","gamma"))

#Wybieramy rozklad log-normalny z wyestymowanymi parametrami jako, że 
#ma najnizsze wartosci kryteriow.

#B. Test zgodnosci dla rozkladu wybranego w punkcie A.
#-----------------------------------------------------
#1. Metoda MC wyznaczamy rozklad statystyki Dn. Generujemy N=10000 probek licznosci danych
#z rozkladu wybranego w punkcie A: F0=LN(z wyestymowanymi parametrami) oraz obliczamy
#odleglosc dystrybuant empirycznych od wybranego rozkladu (czyli wartosc statystyki Dn).
N <- 10000
n <- length(Loss); n


Dln <- c()

for (i in 1:N) { 
  
  Yln <- rlnorm(n,fln$estimate[1],fln$estimate[2])
  
  Dln[i] <-  ks.test(Yln,plnorm, fln$estimate[1],fln$estimate[2],exact=TRUE)$statistic
}

#2. Obliczamy dn, czyli wartosc statystyki Dn, dla danych Loss i rozkładu F0 wybranego w punkcie A.
dn_ln <-  ks.test(Loss,plnorm,fln$estimate[[1]],fln$estimate[[2]],exact=TRUE)$statistic
dn_ln

#wyniki z punktow 1.2 na histogramie
par(mfrow=c(1,1))
hist(Dln,prob=T)
points(dn_ln,0,pch=19,col=2)

#Odleglosc dystrybuanty empirycznej (wartosc statystyki Dn) dla Loss, oraz dystrybuanty F0
# jest istotnie większa od odleglosci obserwowanych dla probek tej samej licznosci z rozkladu F0.

#3. Obliczamy p-value.
p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln

#4. Przyjmujemy poziom istotnosci alpha=0.05
alpha <- 0.05
p_value_ln <= alpha

#Wartosc p-value jest mniejsza od przyjetego poziomu istotnosci, 
#zatem hipoteze o rownosci dystrybuant (F=F0, gdzie F poszukiwany rozklad) odrzucamy.

#C. Dodatkowo test zgodnosci, dla pozostalych dwoch odrzuconych w punkcie A rozkladow.
#-----------------------------------------------------------------------------------
#rozklad DN
N <- 10000
n <- length(Loss); n

Dpar <- c()
Dg <- c()

for (i in 1:N) { 
  
  Ypar <- rpareto(n,fpar$estimate[[1]],fpar$estimate[[2]]) 
  Yg <- rgamma(n,fg$estimate[[1]],fg$estimate[[2]])
  
  Dpar[i] <- ks.test(Ypar,ppareto,fpar$estimate[[1]] ,fpar$estimate[[2]],exact=TRUE)$statistic
  Dg[i] <-   ks.test(Yg,pgamma, fg$estimate[[1]], fg$estimate[[2]],exact=TRUE)$statistic
  
}

#2. wartosc dn statystyki dla Loss i F0
dn_par <- ks.test(Loss,ppareto,fpar$estimate[[1]],fpar$estimate[[2]],exact=TRUE)$statistic
dn_g <-   ks.test(Loss,pgamma,fg$estimate[[1]],fg$estimate[[2]],exact=TRUE)$statistic

round(c(dn_par,dn_g),4)

#wyniki na histogramie (ostatni jest log-norm z B)
par(mfrow=c(3,1))
hist(Dpar,prob=T)
points(dn_par,0,pch=19,col=2)
hist(Dg,prob=T)
points(dn_g,0,pch=19,col=2)
hist(Dln,prob=T)
points(dn_ln,0,pch=19,col=2)


#odleglosc proby  od dystrybuant jest wieksza od maksymalnie obserwowanej z probek

#3. p-value
p_value_par <- length(Dpar[Dpar>dn_par])/N
p_value_g <- length(Dg[Dg>dn_g])/N
p_value_par; p_value_g

#wartosci p-value rowne zero,oznaczają, że przy dowolnie przyjetym poziomie istotnosci
#hipoteze o rownosci rozkladow odrzucamy

