
setwd("C:/Users/LISSETTE/Desktop/Maestria met.Invest/Diseno Experimental")
df=read.csv("PROBLEMA21.csv",sep=";")
df

str(df)

df$TIEMPO=factor(df$TIEMPO)
df$ACELERANTE=factor(df$ACELERANTE)

str(df)

modelo=aov(Y~TIEMPO+ACELERANTE,data=df)
summary(modelo)

boxplot(Y~TIEMPO,data=df)
boxplot(Y~ACELERANTE,data = df)
boxplot(Y~TIEMPO+ACELERANTE,data=df)
interaction.plot(df$TIEMPO,df$ACELERANTE,df$Y)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~TIEMPO,data=df)
leveneTest(Y~ACELERANTE,data=df)

plot(modelo$residuals)
abline(h=0)

plot(df$TIEMPO,modelo$residuals)
abline(h=0)
