## Diferencia en costos con relación a tiempo en las diferentes rutas.

df=read.csv("https://raw.githubusercontent.com/yirleymorales/DisenoExperimental/main/22proa.csv")
df 

str(df)
df$Dia=factor(df$Dia)
df$Chofer=factor(df$Chofer)
df$mv=factor(df$mv)
df$Rutas=factor(df$Rutas)

modelo=aov(Y~Dia+Chofer+mv+Rutas,data=df)
summary(modelo)

boxplot(Y~Rutas,data=df)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~Rutas,data=df)

plot(modelo$residuals)
plot(modelo$fitted.values,modelo$residuals)

## Diferencia en costos con relación a gasolina en las diferentes rutas.

df=read.csv("https://raw.githubusercontent.com/yirleymorales/DisenoExperimental/main/22problb.csv")
df 

str(df)
df$Dia=factor(df$Dia)
df$Chofer=factor(df$Chofer)
df$mv=factor(df$mv)
df$Rutas=factor(df$Rutas)

modelo=aov(Y~Dia+Chofer+mv+Rutas,data=df)
summary(modelo)

boxplot(Y~Rutas,data=df)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~Rutas,data=df)

plot(modelo$residuals)
plot(modelo$fitted.values,modelo$residuals)


## Conclusiones
El costo en tiempo entre las diferentes rutas, muestra diferencias significativas solamente entre las rutas A, B y C, mientras en las rytas C y D no se observan diferencias significativas.
Se encuentran diferencias significativas en el costo de la gasolina entre las cuatro rutas de la compañia.
