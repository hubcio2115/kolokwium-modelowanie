library(matrixcalc)

#Przyklad 1 (BF, Example 2 - 404)
#===========
A <- matrix(c(1,2,3,-1,1,1,-1,2,0,-1,-1,3,3,1,2,-1),nrow=4 )
b <- c(8,7,14,-7)

#LU rozklad (faktoryzacja)
luA <- lu.decomposition( A )
luA

#sprawdzenie
L <- luA$L
U <- luA$U
print( L )
print( U )

print( L %*% U )

#rozwiazanie rownania Ly=b
z <- solve(L,b); z

#rozwiazanie rownania Ux=z
x <- solve(U,z); x


#Przyklad 2 (Czy macierz jest dodatnio okreslona?)
#===========
A <- matrix(c(2,-1,0,-1,2,-1,0,-1,2),nrow=3)

#symetrycznosc
is.symmetric.matrix(A)

#kryterium Sylvestera
A2 <- matrix(c(2,-1,-1,2),nrow=2)
A3 <- A

det(A2)
det(A3)

#gotowa funkcja
is.positive.definite(A)

#Przyklad 3 (Rozklad Choleskiego)
#===========
A <- matrix(c(1,2,3,2,20,26,3,26,70),nrow=3, byrow = TRUE)
A

is.positive.definite(A)

L <- chol(A); L
t(L)

#sprawdzenie
t(L) %*% L  
crossprod(L) 


#Przyklad 4  (Algorytm generujacy z  wielowymiarowego rozkladu normalnego)
#==========
m <- 3           
n <- 2000        #liczba wygenerowanych probek

#wektor srednich i macierz kowariancji 
mu <- rep(0,m)
Sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow=3)

is.singular.matrix(A)
is.symmetric.matrix(A)
is.positive.definite(A)

#a. rozklad choleskiego
L <- chol(Sigma)
L

#b. generujemy m prob licznosci n z rozkladu N(0,1)

Z <- replicate(m,rnorm(n))
head(Z)

#c. Proba z N(mu,Sigma)
X <- mu+L %*% t(Z)
X <- t(X)

#analiza proby pod katem normalnosci
psych::pairs.panels(X)

library(MVN)
result = mvn(data = X, mvnTest = "mardia",
             univariateTest = "AD", univariatePlot = "qq",
             multivariatePlot = "qq")
result

#Przyklad 5 (Algorytm Newtona)
#==========
#a)---- animacja

library(animation)

?ani.options(interval = 1, nmax = 50)
par(pch = 20)

#domyslna funkcja jest f(x)=x^2-4 na przedziale [-1,10])
#mozemy wybrac punkt startu ustawiajac interact=TRUE
#i wybrac punkt klikajac na pierwszy wykres 
#na pierwszy
rt1 = newton.method()
rt1

#take a long long journey
rt2 <- newton.method(function(x) 5 * x^3 - 7 * x^2 - 40 * x + 100, 7.15, 
              c(-6.2, 7.1))

rt2


#---- b)
rt <- newton.method(function(x) 5 * x^3 + sqrt(x) -1, 1,c(0,2))

library(cmna)

#funkcja i jej pochodna
f=function(x) x^3+sqrt(x)-1
df=function(x) 3*x^2+(1/2)*x^(-1/2)

#wybor  punktu startowego (analiza wykresu)
curve(f(x),xlim=c(0,2),lwd=2,xlab=NA,ylab=NA)
abline(h=0,col='grey')
grid()

#punkt startowy
x0 <- 1

#kolejne przyblizenia pierwiastka
x1 <- x0-f(x0)/df(x0); x1
x2 <- x1-f(x1)/df(x1); x2
x3 <- x2-f(x2)/df(x2); x3
x4 <- x3-f(x3)/df(x3); x4

x1;x2;x3;x4
l
ibrary(MASS)
fractions(c(x1,x2,x3,x4))

#--------------
cmna::newton  #metoda Newtona

#modyfikacja kodu z biblioteki 'cmna'
my.newton <- function (f, fp, x, tol = 0.001, m = 100) {
  
  iter <- 0
  oldx <- x
  x <- oldx + 10 * tol
  
  while (abs(x - oldx) > tol) {
    
    iter <- iter + 1
    
    print(round(c(oldx,abs(x - oldx),iter),7))
    
    if (iter > m) 
      stop("No solution found")
    oldx <- x
    x <- x - f(x)/fp(x)
  }
  return(round(c(oldx,abs(x - oldx),iter),7))
  
}

#metoda Newtona

my.newton(f,df,x0,tol=10**-10)

#to samo w bibliotece 'cmna'
cmna::newton(f,df,x0,tol=10**-10,m=100)


#Przyklad 6
#==========
#--- a)
library(rootSolve)
library(plotrix)

F <- function(x){
  
  x1 <- x[1]
  x2 <- x[2]
  
  c(x1^2+x2^2-1,(x1-1)^2+x2^2-1)
}

plot(-2:2,-2:2,type="n",xlab="",ylab="")
draw.circle(0,0,1,lwd=2)
draw.circle(1,0,1,lwd=2)
abline(h=0,col='grey')
abline(v=0,col='grey')
grid()

#punkt startowy
x0 <- c(1/4,1/2)

library(MASS)
F(x0)
fractions(F(x0))

#macierz Jacobiego w punkcie x0
J <- rootSolve::gradient(F,x0); J

#macierz odwrotna do J
solve(J)

#kolejne przyblizenia rozwiazania
x1 <- x0-solve(rootSolve::gradient(F,x0))%*%F(x0); x1
fractions(x1)
x2 <- x1-solve(rootSolve::gradient(F,x1))%*%F(x1); x2
fractions((x2))


#gotowe rozwiazanie
multiroot(F,x0)

library(pracma)
newtonsys(F,x0)

#---b) najmniejsza wartość funkcji
g <- function(x) {
  
  x1 <- x[1]
  x2 <- x[2]
  
  (x1^2+x2^2-1)^2+((x1-1)^2+x2^2-1)^2
}

optim(x0,g)

