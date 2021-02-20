
df=expand.grid(1:3,1:3)
df$Trat=c("A","B","C","B","C","A","C","A","B")
df$Y=c(16,15,13,10,9,11,11,14,13)
df

names(df)=c("Inspector","Escala","Trat","Y")
df

str(df)
df$Inspector=factor(df$Inspector)
df$Escala=factor(df$Escala)
df$Trat=factor(df$Trat)

modelo=aov(Y~Inspector+Escala+Trat,data=df)
summary(modelo)

boxplot(Y~Escala,data = df)
boxplot(Y~Trat,data = df)
boxplot(Y~Inspector,data = df)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

requiere(car)
leveneTest(Y~Trat,data=df)
leveneTest(Y~Escala,data=df)

plot(modelo$residuals)
abline(h=0)

