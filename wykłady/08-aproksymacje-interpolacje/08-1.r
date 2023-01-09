library(ggplot2)

#Przyklad 1 (n=4)
#===========
#wyznaczenie najmniejszego n, dla ktorego
#3/(n+1)! <= 0.01
for(i in 1:10){
  
  Rx <- 3/factorial(i+1)
  
  if(Rx > 10^(-2)){
    
    print(i)
  }
  else 
  {
    break
  }
}

#n=5, e=1+1/1!+1/2!+1/3!+1/4!+1/5!
e.approx <- sum(1/factorial(0:5))
e.approx
exp(1)

#roznica (<0.01)
exp(1)-e.approx

#wielomian Maclaurin'a rzedu n=5, dla funkcji f(x)=e^x
P5 <- function(x){1+x+x^2/2+x^3/6+x^4/24+x^5/120}

curve(exp(x),xlim=c(0,1),col='blue')
curve(P5(x),add=TRUE,col=2)

#na szerszym przedziale wielomian P5 slabo aproksymuje funkcje e^x
curve(exp(x),xlim=c(-5,5),col='blue')
curve(P5(x),add=TRUE,col=2)
grid()

#Przyklad 2 (Example 3, BF p.11)
#============ 
library(pracma)

f <- function(x) cos(x)

#wspolczynniki wielomianu Taylora
T2 <- taylor(f, x0=0, n=2) 
T3 <- taylor(f, x0=0, n=3)
T2; T3  #T2=T3

#wartosc w punkcie 0.01 wielomianu T2=T3
polyval(T2,0.01)
cos(0.01)

#funkcje f i T2=T3 na wykresie 
x <- seq(-pi, pi, length.out=100)
yT2 <- polyval(T2,x)

curve(cos(x), xlim=c(-pi,pi),col = "blue")
lines(x,yT2, col = "red")
grid()


#Przyklad 3 (Example 3, BF p.11)
#============ 
library(pracma)
f <- function(x) 1/x

#wielomian Taylora rzedu  n
Pn <- function(x,n){
  
  res <- 0
  
  for(i in 0:n){
    
    res <- res+(-1)^i*(x-1)^i
  }
  return(res)
}

#wartosci wielomianow P0,P1,...,P7 w punkcie x=3 i wartosc funkcji
sapply(0:7,function(n) Pn(3,n))
f(3)

#funkcja f i wielomiany P0,...,P7 na wykresie
tc <- rainbow(8,rev = TRUE) 

curve(1/x, lwd=2,xlim=c(1/2,4),ylim=c(-5,2))
grid()
for(n in 0:7){curve(Pn(x,n), col=tc[n+1], lwd=2,add=TRUE)}
abline(h=0, col='grey')
abline(v=3, col='grey')
points(3,1/3,col=2,pch=19)
legend('bottomleft', paste0(rep('P',8),0:7),lty=(rep(1,8)),col=tc[1:8])

