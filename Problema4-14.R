setwd("C:/Users/rosita.serrano/Desktop/disenoexperimental")

df=read.csv("Equipos.csv")
df

str(df)
df$Operador=factor(df$Operador)
df$Equipo=factor(df$Equipo)

modelo=aov(Y~Operador+Equipo,data=df)
summary(modelo)

boxplot(Y~Operador+Equipo,data=df)
boxplot(Y~Equipo+Equipo,data=df)

qqnorm(modelo$residual)
qqline(modelo$residual)

shapiro.test(modelo$residuals)

library(car)
leveneTest(Y~Equipo+Equipo,data=df)
leveneTest(Y~Operador+Operador,data=df)

plot(modelo$residuals)
abline(h=0)


boxplot(Y~Operador,data=df)
boxplot(Y~Operador*Equipo,data=df)
