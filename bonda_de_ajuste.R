# Ejercicio 148

# datos
li <- c(0, 10, 20, 30, 40, 50, 60)
ls <- c(10, 20, 30, 40, 50, 60, 70)
frec <- c(10, 60, 80, 100, 90, 70, 10)

# calcular la probabilidad de una normal.
# la media.intervalos y variancia.intervalos estan en utils.R
media <- media.intervalos(li,ls,frec)
desvio <- sqrt(variancia.intervalos(li,ls,frec))

prob.normal <- pnorm(ls,media,desvio) - pnorm(li,media,desvio)

esperado <- sum(frec) * prob.normal
chisq.test(frec, esperado)

# Ejercicio 151

defectos <- c(0, 1, 2, 3)
observado <- c(32, 15, 9, 4)

# Obtenemos la media = lambda para poder obtener poisson
media <- sum(defectos*observado)/sum(observado)
esperado <- ppois(defectos, media)

chisq.test(observado, esperado)