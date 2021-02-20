
setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseno Experimental")
df=read.csv("PROBLEMA16.csv",sep=";")
df

str(df)
df$Lote=factor(df$Lote)
df$Dias=factor(df$Dias)
df$Tratamiento=factor(df$Tratamiento)

modelo=aov(Y~Lote+Dias+Tratamiento,data=df)
summary(modelo)

boxplot(Y~Tratamiento,data=df)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~Tratamiento,data=df)


plot(modelo$residuals)
abline(h=0)

plot(df$Tratamiento,modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)



DATOS TABULADOS EN COLUMNA

setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseno Experimental")
df=read.csv("Segproblema16.csv",sep=";")
df

str(df)
df$Lote=factor(df$Lote)
df$Dia=factor(df$Dia)
df$Tratamiento=factor(df$Tratamiento)

modelo=aov(Y~Lote+Dia+Tratamiento,data=df)
summary(modelo)

boxplot(Y~Tratamiento,data=df)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~Tratamiento,data=df)


plot(modelo$residuals)
abline(h=0)

plot(df$Tratamiento,modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)

