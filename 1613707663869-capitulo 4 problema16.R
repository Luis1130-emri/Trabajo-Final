df=expand.grid(1:5,1:5)
df$Trat=c("A","C","B","D","E","B","E","A","C","D","D","A","C","E","B","C","D","E","B","A","E","B","D","A","C")
df$Y=c(8,11,4,6,4,7,2,9,8,2,1,7,10,6,3,7,3,1,6,8,3,8,5,10,8)
df


names(df)=c("Lote","Dia","Tratamiento","Y")
df

str(df)

df$Lote=factor(df$Lote)
df$Dia=factor(df$Dia)
df$Tratamiento=factor(df$Tratamiento)

str(df)


modelo=aov(Y~Lote+Dia+Tratamiento,data=df)
summary(modelo)

boxplot(Y~Tratamiento,data=df)

tk=TukeyHSD(modelo)
tk$Tratamiento

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



