
my.var <- function(x) {
  
  N <- length(x)
  m <- mean(x)
  sum((x-m)^2)/N
}


#Przyklad 1.
#==========
#realizacje z rozkladu zero-jedynkowego o  p=0.5 (EX=p=0.5, Var(X)=pq=0.25)
n <- 10

#p=0.5
X <- sample(c(0,1),size=n,replace=TRUE); X

#srednia z proby
sum(X)/n  #sum - sumuje elementy wektora
mean(X)   #mean - oblicza srednia

#wariancja z próby zdefiniowana wyzej
my.var(X)

#Przyklad 2.
#==========
#Generujemy N=10000 probek n=10-elementowych z rozkladu zero-jedynkowego o p=0.5
#obliczamy srednia z proby
N <- 10000
n <- 10

m <- c()
v <- c()

for(i in 1:N){
  
  X <- sample(c(0,1),size=n,replace=TRUE)
  m[i] <- mean(X)
  v[i] <- my.var(X)
}

head(m)
head(v)

#Wyniki, srednia z wynikow i rzeczywista wartosc oczekiwana (wariancja) - na histogramie.
m.emp <- mean(m); m.emp      #srednia z wszystkich otrzymanych srednich z prob
v.emp <- mean(v); v.emp      #srednia z wszystkich otrzymanych wariancji z prob

par(mfrow=c(2,1))
hist(m,prob=T)
points(m.emp,0, col=2)
points(0.5,0,col="blue", pch=19)  

hist(v,prob=T)
points(v.emp,0, col=2, pch=19)
points(0.25,0,col="blue", pch=19)


#Przyklad 3.
#==========
#Generujemy 10 000 probek 10-elementowych z rozkladu zero-jedynkowego o p=0.5
#obliczamy wariancje z proby estymatorem nieobciazonym

my.var.nob <- function(x) {
  
  N <- length(x)
  m <- mean(x)
  sum((x-m)^2)/(N-1)
}

N <- 10000
n <- 10
v.nob <- c()

for(i in 1:N){
  
  X <- sample(c(0,1),size=n,replace=TRUE)
  v.nob[i] <- my.var.nob(X)
}

head(v.nob)

#Wyniki, srednia z wynikow i rzeczywista wariancja -  na histogramie.
v.emp <- mean(v.nob); v.emp    

hist(v.nob,prob=T)
points(v.emp,0, col=2, pch=19)
points(0.25,0,col="blue", pch=19)


#Przyklad 4. -- dystrybuanta empiryczna (przeczytaj o/zobacz przyklady uzycia funkcji 'ecdf')
#==========
#Wygenerujemy  proby licznosci n=10, 100 z rozkladu
#a) N(0,1)
#b) Exp(1)
#Narysujemy dystrybuanty empiryczna i teoretyczna.

par(mfrow=c(2,1))
#a)
X <- rnorm(100)
plot(ecdf(X))  
curve(pnorm(x),add=T,col="blue")

#b)
Y <- rexp(100)
plot(ecdf(Y))
curve(pexp(x),add=T,col="blue")


#Przyklad 5. -- kwantyle
#==========
#kwantyle rozkladow wyliczamy korzystajac z funkcji rozpoczynajacej sie od
#litery 'q' i dalej skrotu nazwy rozkladu np.
#qnorm, qexp, qlnorm

p <- c(0.05,0.5,0.95) 
Qn1 <- qnorm(p); Qn1
Qn2 <- qnorm(p,175,5); Qn2
Qe <- qexp(p,1/80); Qe

par(mfrow=c(3,1))

curve(dnorm(x),xlim=c(-4,4))
points(Qn1, rep(0,3), col=2, pch=19)
grid()

curve(dnorm(x,175,5),xlim=c(160,190))
points(Qn2, rep(0,3), col=2, pch=19)
grid()

curve(dexp(x,1/80),xlim=c(0,250),ylim=c(0,0.013))
points(Qe, rep(0,3), col=2, pch=19)
grid()

#Przyklad 6. -- QQ-plot
#==========
#Generujemy dwie proby licznosci n=100 z rozkladow N(0,1) i t(4)

curve(dnorm(x), xlim = c(-4,4))
curve(dt(x,4),col=2,add=T)

X <- rnorm(100)
Y <- rt(100,4)

#wykres kwantyl-kwantyl porownujacy kwanyle z prob X i Y z kwantylami rozkladu normalnego
alpha <- ppoints(50)
q.teo <- qnorm(alpha)
q.emp <- quantile(X,alpha)

plot(q.teo,q.emp)
abline(a=0, b=1,col=2)

#gotowe rozwiazania
par(mfrow=c(2,1))
qqnorm(X)
qqline(X,col=2)

qqnorm(Y)
qqline(Y,col=2)

