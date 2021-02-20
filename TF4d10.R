df=read.csv("prueba3.csv",sep=";")
df
df$Marca=factor(df$Marca)
df$Trat=factor(df$Trat)
df$Y=as.double(df$Y)
str(df)
modelo=aov(Y~Marca+Trat,data=df)
summary(modelo)
boxplot(Y~Marca,data=df,col='pink')
boxplot(Y~Trat,data=df,col='orange')
qqnorm(modelo$residuals,col='blue')
qqline(modelo$residuals,col='red')
shapiro.test(modelo$residuals)
library(car)
leveneTest(Y~Trat,data=df)
plot(modelo$residuals)
abline(h=0,col='green')


