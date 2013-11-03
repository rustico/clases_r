# Ejercicio 160 (con más cosas)

x <- c(70, 63, 72, 68, 65, 67, 62, 65, 74, 70, 85, 80, 71, 54, 62, 50, 60, 66)
y <- c(155, 150, 180, 152, 139, 145, 132, 160, 178, 168, 172, 170, 169, 150, 155, 133, 135, 156)

#dibujar diagrama de disperción
plot(x, y)

#calcular el modelo lineal
ml <- lm(y ~ x)

# Informacion sobre el modelo
summary(ml)
# correlación entre x e y
cor(x,y)

# dibujar la línea entre los puntos
lines(x, ml$fitted.values, col="red")

#calcular anova del modelo
anova(ml)

# calcular la normalidad de los residuos
shapiro.test(ml$residuals)

#calcular para x = 90
ml$coefficients[1] + 90*ml$coefficients[2]

# cambio de escala en x
xn <- x/10
#calcular el modelo lineal nuevamente
mln <- lm(y ~ xn)
summary(mln)

# cambio de escala en x
yn <- y/10
#calcular el modelo lineal nuevamente
mln2 <- lm(yn ~ x)
summary(mln2)