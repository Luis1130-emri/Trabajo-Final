Y=c(1.328,1.113,0.985,1.057,1.316,1.144,1.553,1.485,1.310,1.386,1.273,0.789,0.985,0.671,1.134,0.554,1.412,1.386,0.917,1.289,1.269,1.093,1.268,0.984,1.091,1.087,1.195,1.482,1.380,1.442,1.036,0.201,0.783,0.900,1.108,0.916,1.129,1.434,1.132,1.223,1.440,1.150,1.079,1.190,1.389,1.247,1.611,1.617,1.445,1.574,1.454,1.018,1.063,1.050,1.219,0.997,1.602,1.538,1.583,1.478)

df=expand.grid(1:10,1:2,1:3)

names(df)=c("Replica","Equipo","Operador")
df$Y=Y
df$Operador=factor(df$Operador)
df$Equipo=factor(df$Equipo)

df

modelo=aov(Y~Equipo+Operador,data=df)
summary(modelo)

boxplot(Y~Equipo,data=df)
boxplot(Y~Operador,data=df)

boxplot(Y~Equipo*Operador,data=df)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

require(car)
leveneTest(Y~Equipo,data=df)
leveneTest(Y~Operador,data=df)

plot(modelo$residuals)
abline(h=0)



















