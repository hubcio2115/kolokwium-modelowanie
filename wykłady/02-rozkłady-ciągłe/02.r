##============================
#Rozklady ciagle w R
#============================

#-------------------  gestosc i dystrybuanta w R  -----------------------------------
#Wartosc gestosci f(x) i dystrybuanty F(x) obliczamy funkcja rozpoczynajaca sie odpowiednio od 
# "d" (gestosc)  
# "p" (dystrybuanta)
# i dalej skrotu nazwy rozkladu, dla przykladu:

#dnorm(x,m,sd) --  oblicza gestosc f(x), dla rozkladu normalnego N(m,sd^2)
#dt(x,df) -- oblicza gestosc f(x), dla rozkladu t-Studenta  t(df)

#pnorm(x,m,sd) --  oblicza dystrybuante F(x), dla rozkladu normalnego N(m,sd^2)
#pt(x,df) -- oblicza dystrybuante F(x), dla rozkladu t-Studenta  t(df)

#Aby dowiedziec się wiecej o kazdej funkcji napisz w konsoli '?' i po tym, nazwe funkcji np. 
#?dnorm, ?pt

#Przyklad 1. Zmienna X ma rozklad normalny standardowy N(0,1)
#==========
#Wykresy gestosci f i dystrybuanty F
par(mfrow=c(2,1))
curve(dnorm(x),xlim=c(-4,4))
grid()
curve(pnorm(x),xlim=c(-4,4))
grid()

#P(-1<X<1)=F(1)-F(-1)
pnorm(1)-pnorm(-1)

#P(-2<X<2)=F(2)-F(-2)
pnorm(2)-pnorm(-2) 

#P(-3<X<3)=F(3)-F(-3)
pnorm(3)-pnorm(-3)


#Przyklad 2. Zmienna X ma rozklad log-normalny LN(0,1)
#==========
#Wykresy gestosci f i dystrybuanty F
par(mfrow=c(2,1))
curve(dlnorm(x),xlim=c(0,10))
grid()
curve(plnorm(x),xlim=c(0,10))
grid()

#P(X<2)=P(X<=2)=F(2)
plnorm(2)

#P(X>6)=1-P(X<=6)=1-F(6)
1-plnorm(6)

#Przyklda 3 (rozklad chi-kwadrat, Zadania B, zad.6)
#==========
N <- 1000
n <- 100

Y <- c()

for(i in 1:N){
  
  X <- rnorm(n)
  Y[i] <- sum(X^2)
  
}

hist(Y,prob=TRUE)
curve(dchisq(x,100), col=2,add=TRUE)

#Przyklad 4 (rozklad t-Studenta, Zadania B, zad.7)
#==========
#gestosc rozkladu N(0,1)
curve(dnorm(x,0,1),xlim=c(-6,6),col=2,lwd=2)

#gestosc rozkladu t(4)
df=c(4,10,30)
col <- c('grey','blue','green')

count <- 0
for(i in df){
  
  count <- count+1
  curve(dt(x,i), col=col[count],add=TRUE)
}

#Przyklad 5 (rozklad wykladniczy Exp(lambda))
#==========
par(mfrow=c(2,1))
#lambda=1
curve(dexp(x,1),xlim=c(0,10))
grid()
curve(pexp(x,1),xlim=c(0,10))
grid()

#Dla rozkładu wykładniczego EX=1/lambda. Obliczymy P(X>EX)
#lambda=1, P(X>1)
1-pexp(1)

#lambda=1/80, P(X>80)
1-pexp(80,1/80)

#Przyklad 6 (rozklad Erlanga, Zadania C,zad.9)
#=========
lambda <- 1

N <- 1000
n <- 10

Y <- c()

for(i in 1:N){
  
  X <- rexp(n,lambda)
  Y[i] <- sum(X)
  
}

#rozklad Erlanga, to szczegolny przypadek rozkladu gamma G(n,lambda)
hist(Y,prob=TRUE)
curve(dgamma(x,n,lambda), col=2,add=TRUE)

