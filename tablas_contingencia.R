# Ejercicio 152

x <- c(123, 145,
       153, 150)

# transformamos los datos en una matriz
x <- matrix(x, nrow=2)

chisq.test(x)