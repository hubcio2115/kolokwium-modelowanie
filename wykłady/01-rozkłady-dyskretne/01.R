##============================
#Rozklady dyskretne w R
#============================

#------------------- Obliczanie wartosci prawd. dystrybuanty
#Wartosci prawdopodobienstwa P(X=x), i dystrybuanty F(x) obliczamy funkcja rozpoczynajaca sie odpowiednio od 
# "d" (density)  
# "p" (probability)
# i dalej skrotu nazwy rozkladu, dla przykladu

#dbinom(x,n,p) --  oblicza P(N=x), N zmienna o rozkladzie Beroulliego/dwumianowym B(n,p)
#dpois(x,lambda) -- oblicza P(N=x), N zmienna o rozkladzie Poissona P(lambda)

#pbinom(x,n,p) --  oblicza F(x)=P(N<=x), N zmienna o rozkladzie Beroulliego/dwumianowym B(n,p)
#ppois(x,lambda) -- oblicza F(x)=P(N<=x), N zmienna o rozkladzie Poissona P(lambda)

#---------------------- Generowanie z rozkladu
#Probe o licznosci k z danego rozkladu generujemy funkcja rozpoczynajaca się literka 'r'
#i dalej   nazwa rozkladu, dla przykladu

#rbinom(k,n,p) --  generuje probe licznosci k z rozkladu B(n,p)
#rpois(k,lambda) --  generuje probe licznosci k z rozkladu  Poissona P(lambda)


#Aby dowiedzieć się więcej o kazdej funkcji napisz w konsoli '?' i po tym bez spacji nazwe funkcji np. 
#?dbinom, ?pbinom, ?rbinom


#=======================================================
#Rozklad dwumianowy i Poisona. Zbieżność Binom. do Pois.
#=======================================================
#Przyklad0
#==========
#a) Prawdopodobienstwo, ze w 60 rzutach moneta symetryczna wypadnnie dokładnie 30(35) orlow.
#Oznaczmy X - liczba orlow w n rzutach moneta symetryczna, 
#X ma rozklad B(n,p)=B(50,0.5) 
#P(X=10)=?

dbinom(30,60,0.5); dbinom(35,60,0.5) 

#b) Zalozmy, ze zmienna losowa X o rozkladzie Poissona z parametrem  lambda=30
#jest dobrym modelem dla liczby wypadkow w roku - w miejscowosci A.
#Wtedy prawdopodobienstwo, ze bedzie dokladnie 10(30) wypadkow wynosi

dpois(10,lambda=30); dpois(30,lambda=35)

#Przyklad 1. (rozklad dwumianowy/Bernoulliego)
#===========
#Wykres P(N=k) dla k=1,2,..., 100 dla rozkladu B(n=100,p)
#dla roznych p: p=0.1, 0.5, 0.9

#Mozesz myslec jako o prawdopodobienstwie wypadniecia k orlow 
#w  n=100 rzutach moneta, gdzie pr. wypadniecia orla w jednym rzucie wynosi p
n=100
par(mfrow = c(3, 1))  #ustawienia dla okna wykresow

for(p in c(0.1,0.5,0.9)) {
  
  plot(1:n,dbinom(1:n,n,p),
       type='h', xlab=NA)
}

#Przyklad 2.  (rozklad Poissona)
#==========
#Wykres prawdopodob P(N=k) dla k=1,2,..., 30 dla rozkladu Poissona  P(lambda)
#dla roznych lambda: lambda=1,5,10

par(mfrow = c(3, 1))

Lambda <- c(1,5,10)
k=30

for(l in Lambda) {
  
  plot(0:k,dpois(0:k,l),
       type='h',xlab=NA)
}


#Przyklad 3. (zbieznosc Binom. do Pois.) 
#===========
#W portfelu polis mamy 10 000 samochodow. 
#Srednio 1 na 1000 ulega w ciągu roku wypadkowi (p=1/1000)
#Liczbe wypadkow z portfela modelujemy rozkladem B(10000,1/1000),
# który aproksymujemy rozkladem Poissona
# z parametrem lambda=np=10000*(1/1000)=10 

n=10000; p=0.001
lambda <- n*p

par(mfrow = c(2, 1))
plot(1:100,dbinom(1:100,n,p),type='h')
plot(1:100,dpois(1:100,lambda),type='h')

#============================================
#Standaryzacja rozkladu, skosnosc,  kurtoza
#============================================
 
#Przyklad 4. (zmienna standaryzowana)
#===========
#generujemy n liczb z rozkladu Poissona, EX=VaRX=lambda
n <- 10
lambda <- 8

set.seed(5)
X=rpois(n,lambda)    
Y=(X-lambda)/lambda #standaryzacja sredni

par(mfrow = c(1, 1))
plot(X,rep(0,n), xlim=c(-3,max(X)), ylim=c(-1,2),col=2,pch=19)
points(Y,rep(1,n),col='blue',pch=19)
abline(v=0,col='gray')


