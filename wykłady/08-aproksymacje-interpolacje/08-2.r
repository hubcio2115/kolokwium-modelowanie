library(ggplot2)
library(polynom)

#Przyklad 1a) (Ex. 2, BF p.110) wielomian Lagrange'a dla funkcji f(x)=1/x
#==========
f <- function(x) {1/x}


x0 <- 2; x1 <- 2.75; x2 <- 4

x <- c(x0,x1,x2)
y <- f(x)

#wezly
df <- data.frame(cbind(x, y))
head(df)

#wielomian Lagrangea
Lg <- poly.calc(x,y)
Lg

#funkcje f i Lg na wykresie
Lg_wiel <- as.function(Lg)

ggplot(df, aes(x=x, y=y)) + 
  geom_point(size=5, col='blue') + 
  stat_function(fun = f, size=1.25, alpha=0.4,col="blue")+
  stat_function(fun = Lg_wiel, size=1.25, alpha=0.4,col='green')

#lub 
curve(1/x,xlim=c(1,6))
lines(Lg,col='green')
grid()

#lub

curve(1/x,xlim=c(1,6),xlab=NA,col=2)
curve(Lg_wiel,col="green",add=TRUE)
grid()

#---- b) blad bezwzgledny przyblizenia

#wartosc wielomianu Lg dla x=1/3
predict(Lg,3)
#lub
Lg_wiel(3)

#blad przyblizenia
abs(predict(Lg,3)-f(1/3))

#c) ---- blad aproksymacji z Twierdzenia (z wykorzystaniem reszty)
#najwieksza wartosc funkcji w na przedziale [x0,x2]
w <- function(x) abs((x-x0)*(x-x1)*(x-x2))

curve(w(x),xlim=c(x0,x2),col='green')

opt.w <- optimize(w,c(x0,x2),maximum=TRUE)
opt.w

#oszacowanie bledu 
opt.w$objective/2^4

#d) --- porownanie z wielomianem Taylora tego samego rzedu, 
#wokol punktu x0=1

library(pracma)
f <- function(x) 1/x

#wielomian rzedu n=2
T2 <- taylor(f, x0=3, n=2) 
T2

T2_wiel <- function(x) {x^2/27-9*x/27+1}

ggplot(df, aes(x=x, y=y)) + 
  geom_point(size=5, col='blue') + 
  stat_function(fun = f, size=1.25, alpha=0.4,col="blue")+
  stat_function(fun = Lg_wiel, size=1.25, alpha=0.4,col='green')+
  stat_function(fun = T2_wiel, size=1.25, alpha=0.4,col=2)

#oszacowanie bledu dla wielomianu Taylora
wT <- function(x) abs(x-3)^3

curve(wT(x),xlim=c(x0,x2),col='green',lwd=2)
curve(w(x),xlim=c(x0,x2),add=T,col=2,lwd=2)

opt.wT <- optimize(wT,c(x0,x2),maximum=TRUE)
opt.wT

opt.wT$objective/2^4 #Taylor
opt.w$objective/2^4 #Lagrang'e

