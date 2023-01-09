#A - estymacjaie parametrow rozkladu (estymatory MME i MLE)
#B - wykresy diagnostyczne (dane a dopasowany rozklad)

#A. Estymacja parametrow rozkladu
#================================
#===========
#Przyklad 1. (estymator MME, rozklad Poissona)
#==========
#a) generujemy probe 100-elementowa z rozkladu Pois(10) 
set.seed(1)

lambda <- 10
X <- rpois(100,lambda)

N <- table(X); N
plot(N)

#Jaki wyestymujemy parametr 'lambda' krzystając z estymatora MME?
lambda <- mean(X); lambda

#b) powtarzamy punkt a)  N=10000 razy
N <- 10000
n <- 100

lambda <- 10

Lambda <- c()

for(i in 1:N){
  
  X <- rpois(n,lambda)
  Lambda[i] <- mean(X)
}

#obejrzyjmy wyniki na histogramie
hist(Lambda, prob=TRUE)

#srednia i odchylenie standardowe wynikow
mean(Lambda)
sd(Lambda)  #blad standardowy estymacji

#==========
#Przyklad 2. (estymator MME, rozklad wykladniczy)
#==========
#a) generujemy probe 100-elementowa z rozkladu Exp(1) 
set.seed(70)
X <- rexp(100,1)

hist(X,prob=TRUE)

#Jaki wyestymujemy parametr krzystając z estymatora MME?
lambda <- 1/mean(X); lambda

#b) powtarzamy punkt a)  N=10000 razy
N <- 10000
n <- 100

lambda <- 1

Lambda <- c()

for(i in 1:N){
  
  X <- rexp(n,lambda)
  Lambda[i] <- 1/mean(X)
}

#obejrzyjmy wyniki na histogramie
hist(Lambda, prob=TRUE)

#srednia i odchylenie standardowe wynikow
sd(Lambda) #blad standardowy estymacji
mean(Lambda)

#Zadanie. 
#========
#Wygeneruj n=10000 prob licznosci n=100 z rozkladu N(0,1). 
#Oblicz parametry mu i sigma^2 rozkladu, korzystajac z estymatorow 
#zaimplementowanych w funkcjach 'mean' i 'var'.
#Zrob histogramy dla otrzymanych prob. 

#====================================================================================
#Estymacja parametrow z wykorzystaniem funkcji 'fitdist' w bibliotece 'fitdistrplus'.
#Domyslnie zaimplementowana jest metoda 'mle'.
#====================================================================================

#===========
#Przykład 3. (estymator MLE, rozklad wykladniczy)
#===========
#Generujemy 100-elementowa probke z rozkladu Exp(1).
set.seed(70)
X <- rexp(100,1)

hist(X,prob=TRUE)

#Estymujemy parametry korzystajac z funkcji 'fitdist' w bibliotece 'fitdistrplus'.
#Domyslnie zaimplementowana jest metoda 'mle'.
library(fitdistrplus)
?fitdist
fit <- fitdist(X,"exp")
fit

#===========
#Przykład 4. (estymator MLE, rozklad normalny)
#===========
#Generujemy 100-elementowa probke z rozkladu N(0,1)
set.seed(9)
X <- rnorm(100,0,1)

hist(X,prob=TRUE)

#estymujemy parametry
fit <- fitdist(X,"norm")
fit

#wyestymowane parametry (napisz fit$  i zaczekaj na podpowiedz)
#tych parametrow uzyjemy dalej w wykresach diagnostycznych
m <- fit$estimate[[1]]  
s <- fit$estimate[[2]]
m; s

#B1. Wykresy diagnostyczne (Przyklad 4 cd.)
#-------------------------------------------
#1. histogram-gestosc
par(mfrow=c(2,2))
hist(X,prob=T)
curve(dnorm(x,m,s), add=T, col=2)

#2. dystrybuanta empiryczna-dystrybuanta teoretyczna
plot(ecdf(X))
curve(pnorm(x,m,s),col=2,lwd=2,add=T)

#3. wykres kwantyl-kwantyl (QQ-ploty)
alpha=ppoints(30)           #sprawdz co robi ppoints
q.teo <- qnorm(alpha)       #kwantyle z rozkladu teoretycznego
q.emp <- quantile(X,alpha)  #kwantyle empiryczne 
  
plot(q.teo,q.emp)
abline(a=0,b=1,col=2,lwd=2)


#B2. Te same wykresy diagnostyczne w bibliotece 'fitdistrplus'
#-----------------------------------------------------------------
#'Dopasujemy' dwa rozklady normaly i t-Studenta do proby X 
#i analizujac wykresy diagnostyczne, zobaczymy ktory z nich jest 'lepszy'.
library(fitdistrplus)
set.seed(9)
X <- rnorm(100,0,1)

hist(X,prob=TRUE)

fn <- fitdistrplus::fitdist(X, "norm")
ft <- fitdistrplus::fitdist(X, "t", start=list(df=12)) #rozklad "t" potrzebuje punktu startowego

#wyniki estymacji
fn  #N(mu,sigma)
ft  #t(df) - jeden parametr df (stopnie swobody - degrees of freedom)

#wyestymowane  gestosci na wykresie
par(mfrow=c(1,1))
curve(dnorm(x,fn$estimate[1],fn$estimate[2]), xlim=c(-4,4),lwd=2)
curve(dt(x,ft$estimate), add=T, col=2,lwd=2)

#--- Diagnostyka na wykresach (ktory rozklad lepiej opisuje dane?)
par(mfrow=c(2,2))

plot.legend <- c("normal", "t")
denscomp(list(fn, ft), legendtext = plot.legend)
qqcomp(list(fn, ft), legendtext = plot.legend)
cdfcomp(list(fn, ft), legendtext = plot.legend)
ppcomp(list(fn, ft), legendtext = plot.legend)

#=============== kryteria wyboru (o tym na nastepnym wykladzie)
#Podjecie decyzji o wyborze rozkladu wspomagaja dodatkowe kryteria.

#statystyki Kolmogorow-Smirnow, Cramer-von-Mises, Anderson-Darling (im mniejsza wartosc tym "lepszy" rozklad)
#kryteria AIC, BIC (im mniejsza wartosc tym "lepszy" rozklad)

gofstat(list(fn, ft),  fitnames = c("normal", "t"))


