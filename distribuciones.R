# Referencia http://www.stat.umn.edu/geyer/old/5101/rlook.html

# Normal

#Visualización
x <- seq(-5,5,0.2)
# N(0,1)
plot(x,dnorm(x,0,1),"l",col="red")
# N(1,1)
lines(x,dnorm(x,1,1),col="blue")
# N(1,2)
lines(x,dnorm(x,1,2),col="green")


# Calcular la probabilidad para P(X < 27.4) con Esperanza = 50 y Desvío = 20
pnorm(27.4, 50, 20)
# Inversa Normal
qnorm(0.95, 50, 20)

#Poisson

#Visualización
x <- 0:20
# Lambda = 2
plot(x,dpois(x,2),col="red",pch=18)
# Lambda = 4
points(x,dpois(x,4),col="blue",pch=17)
# Lambda = 10
points(x,dpois(x,10),col="green",pch=16)

# Calcular la probabilidad para P(X < 3) con Esperanza = 5
ppois(3, 5)
# Inversa Poisson
qpois(0.95, 5)

# Chi Cuadrado

#Visualización
x <- seq(0,40,0.2)
# N(0,1)
plot(x,dchisq(x,4),"l",col="red")
# N(1,1)
lines(x,dchisq(x,10),col="blue")
# N(1,2)
lines(x,dchisq(x,20),col="green")


# Calcular la probabilidad para P(X < 7.4) con Grados de Libertad = 10
pchisq(7.4, 10)
# Inversa
qchisq(0.95, 10)

# T de Student

#Visualización
x <- seq(-5,5,0.2)
# N(0,1)
plot(x,dt(x,50),"l",col="red")
# N(1,1)
lines(x,dt(x,1),col="blue")
# N(1,2)
lines(x,dt(x,4),col="green")


# Calcular la probabilidad para P(X < -0.4) con Grados de Libertad = 4
pt(-0.4, 4)
# Inversa
qt(0.95, 4)

# F de Fisher

#Visualización
x <- seq(0,8,0.1)
# N(0,1)
plot(x,df(x,3,12),"l",col="red")
# N(1,1)
lines(x,df(x,5,2),col="blue")
# N(1,2)
lines(x,df(x,10,10),col="green")


# Calcular la probabilidad para P(X < 1.4) con GL1 = 2 y GL2 = 5
pf(1.4, 2,5)
# Inversa
qf(0.95, 2,5)