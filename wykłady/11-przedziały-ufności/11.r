#Przedzialy ufnosci

#Przyklad 1a)b) (Fakt 1 - uzasadnienie na przykladzie)   
#========
#--- 1a)
#Dla roznych mu i sigma, wygeneruj N=1000 prob licznosci n=100: x1,x2,...,xn  z rozkładu N(mu,sigma^2)
#i korzystajac z nich, wyznacz N: t1,t2,...,tN  mozliwych wartości zmiennej U. Korzystajac z testow diagnostycznych sprawdz teze Faktu 2. 
N=1000; n=10
m=3; sigma=2

licz.t <- function(dane) {(mean(dane)-m)/ (sd(dane)/sqrt(n))}

t <- replicate(N, licz.t(rnorm(n,m,sigma)))

hist(t,prob=T)
curve(dt(x,n-1), add=T,col=2)

p <- ppoints(100); p
t.teor <- qt(p,n-1)  #kwantyle z rozkladu teoretycznego

qqplot(t,t.teor)
abline(a=0,b=1,col=2)

#--- 1b)
#Mamy probe z rozkladu normalnego o nieznanej wartości oczekiwanej i wariancji.
X=c(179,180,171,177,183,167,180,182,182,178)

#Obliczymy konce przedzialu ufnosci, na poziomie ufnosci 95%
CI.mean=function(data,alpha = 0.05) {
  
  n=length(data)
  t = qt(1-alpha/2,n-1)
  
  l <- mean(data) - t * sd(data)/sqrt(n)
  p <- mean(data) + t * sd(data)/sqrt(n)
  
  return(c(left=l,right=p,length=p-l))
}

CI.mean(X,alpha=0.05)

#Uwaga. Proba X zostala wygenerowana z rozkladu N(175,5^2).
#mu=175 nalezy do otrzymanego przedzialu


#Przykład 2a)b) (CI dla wariancji)   
#========
#--- 2a)
#Dla roznych mu i sigma, wygeneruj N=1000 prob licznosci n=100: x1,x2,...,xn  z rozkładu N(mu,sigma^2)
#i korzystajac z nich, wyznacz N: c1,c2,...,cn  mozliwych wartosci zmiennej chi^2.
#Korzystajac z testow diagnostycznych sprawdz teze Faktu 3. 

licz.chi <- function(dane,sigma=2) {n*var(dane)/sigma^2}

N=1000; n=100
m=3; sigma=2


chi <- replicate(N, licz.chi(rnorm(n,m,sigma)))

hist(chi,prob=T)
curve(dchisq(x,n-1), add=T,col=2)


#--- 2b
#proba wygenerowana z rozkladu N(175,5^2)

X=c(179,180,171,177,183,167,180,182,182,178)


#CI dla wariancji
CI.var = function(data,alpha=0.05){
  
  n=length(data)
  df = n - 1
  
  qchi1 = qchisq(1 - alpha/2,df)
  qchi2 = qchisq(alpha/2,df)
  
  l <- (n-1)*var(data)/qchi1
  p <- (n-1)*var(data)/qchi2
  
  return(c(lower=l,upper=p,length=p-l))
}

#rzeczywista wariancja rowna 25 nalezy do wyliczonego przedzialu
CI.var(X,alpha=0.05)

