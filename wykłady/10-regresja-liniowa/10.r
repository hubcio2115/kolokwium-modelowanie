---
title: "Regresja liniowa"
author: "JC"
output: pdf_document
---

```{r setup, include=FALSE}

library(ggplot2)
library(boot)  #dane: catsM, cats

```

```{r}
#Przyklad 1.a
#=========
catsM

waga.c <- catsM$Bwt
waga.s <- catsM$Hwt

df <- data.frame(waga.c=waga.c,waga.s=waga.s)

#obejrzyjmy dane na wykresie
qplot(waga.c, waga.s, data = df,
      main = "Waga serca, a waga ciała u kotów") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_point(colour = "blue", size = 1.5) 
      

#estymatory wspolczynnikow
beta1 <- cov(waga.c,waga.s)/var(waga.c)
beta0 <- mean(waga.s)-mean(waga.c)*beta1
beta1; beta0

#linia regresji na  wykresie (waga.s=-1.18+4.31 waga.c)
qplot(waga.c, waga.s, data = df,
      main = "Waga serca, a waga ciała u kotów") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_point(colour = "blue", size = 1.5) +
      geom_abline(intercept = beta0, slope = beta1, color="red",size=1)
```



```{r}
#Przyklad 1.b (gotowe rozwiazanie - wspolczynniki i analiza reszt)
#===========
#funkcja 'lm'

waga.c <- catsM$Bwt
waga.s <- catsM$Hwt

df <- data.frame(waga.c=waga.c,waga.s=waga.s)

waga.lm <- lm(waga.s~waga.c,data=df)
waga.lm

sum <- summary(waga.lm)
sum

```


```{r}
#Przyklad 1.c (test istotnosci wspolczynnikow bo, b1)
#===========
#Obliczamy wartosci statystyki T oraz p-value

#wspolczynniki modelu 
coef <- waga.lm$coefficients  #lub coef(waga.lm)
beta0 <- coef[[1]]
beta1 <- coef[[2]]
beta0; beta1  #-1.184, 4.313

#odchylenia standardowe estymatorow beta0, beta1
coef_all <- sum$coefficients
coef_all 

se.beta0 <- coef_all[1,2]
se.beta1 <- coef_all[2,2]
se.beta0; se.beta1

#wartosci statystyki T (t value) 
t0 <- beta0/se.beta0
t1 <- beta1/se.beta1

t0; t1

#p-value (p = P(|T|>t0), p = P(|T|>t1))
2*(1-pt(abs(t0),95))
2*(1-pt(abs(t1),95))

#warosci p = P(|T|>t0), p = P(|T|>t1) sa mniejsze od 5% zatem hipoteze
#ze wspolczynniki sa rowne zero, odrzucamy na tym poziomie istotnosci

```





```{r}
#Przyklad 1.d  (analiza reszt)
#===========

#reszty (residuals) 
reszty <- waga.lm$residuals

#histogram i qq-ploty
hist(reszty)

qqnorm(reszty)
qqline(reszty,col=2)

m <- mean(reszty)
s <- sd(reszty)

ks.test(reszty,'pnorm',m,s)

#test Shapiro-Wilka
shapiro.test(reszty)

#p-value=0.1381, na poziomie 5% nie ma podstaw
#do odrzucenia hipotezy o normalnosci rozkladu reszt

#RSE - blad standardowy reszt
RSE <- sqrt(sum(reszty^2)/(length(waga.c)-2))
RSE

```


```{r}
#Przyklad 1.e Predykcja (pozostawiamy wyestymowane b0)
#=========
#Ile waży serce kotka, którego waga jest rowna sredniej z badanej probki kotkow?
m <- mean(waga.c)

beta0+beta1*m

```

```{r}
#Przyklad 1.f Ponowna regresja i predykcja, przy b0=0
#=========

waga.c <- catsM$Bwt
waga.s <- catsM$Hwt

df <- data.frame(waga.c=waga.c,waga.s=waga.s)

waga.lm2 <- lm(waga.s~waga.c-1,data=df)
waga.lm2 #model 2
waga.lm  #model 1

sum1 <- summary(waga.lm)
sum2 <- summary(waga.lm2)
sum1; sum2 

#Ile waży serce kotka, którego waga jest rowna sredniej z badanej probki kotkow?
m <- mean(waga.c)

beta1_model2 <- waga.lm2$coefficients

beta1_model2*m  #predykcja model 2
beta0+beta1*m   #predykcja model 1

```



```{r}
#Przyklad 1.g (Predykcja i przedzialy ufnosci dla predykcji)
#=========
nowa.waga <- data.frame(waga.c=m)

predict(waga.lm, nowa.waga, interval="confidence")  #model 1
predict(waga.lm2, nowa.waga, interval="confidence") #model 2
 

```



```{r}
#Przyklad 2 (samochody - predkosc a droga hamowania)
#=========
#A Zaleznosc drogi hamowania od predkosci
cars

#1 mph = 0,4470311111 m/s = ok. 1,61 km/h (mila)
#100 km/h = ok. 62 mph.
#1 ft = 0,3038m (stopa) 

#wykres zaleznosci
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  

boxplot(cars$speed, main="Prędkość", range=2,sub=paste("Prędkości odstające: ", boxplot.stats(cars$speed)$out)) 
boxplot(cars$dist, main="Długość hamowania",
        sub=paste("Długości odstające: ",boxplot.stats(cars$dist)$out)) 

#model liniowy, zaleznosc drogi hamowania od predkosci
#-------------
dist.lm <- lm(dist ~ speed, data=cars)  

#wyniki modelu
summary(dist.lm)

#linia regresji na  wykresie 
b0 <- dist.lm$coefficients[[1]]  #-17.58
b1 <- dist.lm$coefficients[[2]]  #3.93

qplot(speed,dist, data = cars)+
      geom_point(colour = "blue", size = 1) +
      geom_abline(intercept = b0, slope = b1, color="red",size=1)

b0; b1  #-17.58, 3.93

#cars$speed,cars$dist
```

#Przykład 2 cd. (Predykcja z modelu - zbior treningowy, zbior testowy)

```{r}
#1. Utworzenie zboru treningowego i  zbioru testowego (80\% do 20\%)
#losowo generujemy indeksy do zbioru treningowego
set.seed(100)
tren.indeksy <- sample(1:nrow(cars), 0.8*nrow(cars)) 

tren.dane <- cars[tren.indeksy, ]   #zbior treningowy
test.dane <- cars[-tren.indeksy, ]  #zbior testowy

#2. 'Uczymy' model na danych treningowych i sprawdzamy miary dopasowania.
dist.lm <- lm(dist ~ speed, data=tren.dane)  
dist.lm$coefficients  #-20.18, 4.25

#3 Prognozujemy z modelu dla  danych testowych i porownujemy z rzeczywistymi danymi.
dist.pred <- predict(dist.lm, test.dane) 

#droga hamowania rzeczywista i prognozowana, dla danych testowych (ramka danych)
dist.h <- data.frame(cbind(real=test.dane$dist, pred=dist.pred))  

#Porownajmy wyniki predykcji z rzeczywistymi wartosciami modelu
qplot(real, pred, data=dist.h)+
      geom_point(colour = "blue", size=1.5) +
      geom_abline(intercept = 0, slope = 1, color="red",size=1)

```


```{r}

#Przyklad 2 cd. (Metody Monte Carlo)
#=========
#Powtorzmy eksperyment z losowym wyborem 80% danych uczacych 1000 razy,
#przeanalizujmy wyestymowane wspolczynniki  oraz  prognozy drogi hamowania
#przy predkosci speed=21 z otrzymanych modeli

uczenie <- function(indeksy){
  tren.dane <- cars[indeksy, ]   #zbior treningowy

  mod.lm <- lm(dist ~ speed, data=tren.dane)  

  nowa.speed <- data.frame(speed=21)
  mod.pred <- predict(mod.lm,nowa.speed) 

  par <- as.numeric(mod.lm$coefficients)

  return(c(par,mod.pred))
}


#wybor indeksow - czyli 80% samochoow sposrod badanych
#indeksy <- sample(1:nrow(cars), 0.8*nrow(cars))


N=1000
MC <- t(replicate(N,uczenie(sample(1:nrow(cars), 0.8*nrow(cars)) )))
MCb0 <- MC[,1]
MCb1 <- MC[,2]
pred21 <- MC[,3]

#rozklad spolczynnikow
hist(MCb0,prob=T)
points(mean(MCb0),0,pch=19,col=2)
points(b0,0,pch=5,col='blue')
hist(MCb1,prob=T)
points(mean(MCb1),0,pch=19,col=2)
points(b1,0,pch=5,col='blue')
hist(pred21,prob=T)
points(mean(pred21),0,pch=19,col=2)
points(b0+21*b1,0,pch=5,col='blue')

mean(MCb0); mean(MCb1); mean(pred21)
b0; b1; b0+21*b1

```
