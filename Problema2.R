#Codigo para problema 2

mis_dades<-iris
mis_dades
mean(mis_dades$Sepal.Length)
sd(mis_dades$Sepal.Length)
dim(mis_dades)
names(mis_dades)
hist(mis_dades$Sepal.Length)

x<-mis_dades$Petal.Length
x
y<-mis_dades$Sepal.Length
y

plot(x,y)
#regresion lineal, recta que mejor describe los datos"la media"
#minimiza la suma de cuadrados (y1-y0)^2, y0=m*x0+b),derivada igual a cero

m<-sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)  #calcular m
m
b<-mean(y)-m*mean(x)
b

m*1.5+b

lm(y~x) #te dice m y b, ~ altgr+4+espacio
mod<-lm(y~x) #lo guarda como un modelo
mod
summary(mod) #da mucha informacion de mod

xpred<-data.frame(x=1.5) #longitud de los datos que quieres
predict(mod,xpred) #predice el valor de y para x=1.5

xpred<-data.frame(1:7)
xpred
predict(mod,xpred)

xpred<-data.frame(x=x)
xpred
predict(mod,xpred)

ypred<-predict(mod,xpred)
ypred
#source, ejecutar todo desde el principio

plot(x,ypred)

plot(x,y)
lines(x,ypred) #represenat las predicciones respecto a la recta

#como de precisa  es tu predicción R^2=1 si es 0=imprecisa
sum((ypred-mean(y))^2)/sum((y-mean(y))^2) #coef. de Pearson= R
Rsq<-sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
