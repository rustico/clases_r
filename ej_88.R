# Ejercicio 88

datos <-c(102.6,115.2,98.2,91.5,85.7,96.6,74.9,94.6,96.3,99,94.9,111,89.8,88.5,
          78.6,85.1,85.2,104.7,103.3,85.8, 82.8,93.9,75,90.2,92.1,101,101.4,84.3,
          112.6,95,110.3,88.6,102.7,114.8,101.8,89.3,89.6,92.9,101.7,119,102.7,
          114.8,101.8,89.3,89.6,92.9,101.7,119,110.8,80.9,100.3,90.3,105.5,104.4,
          93.6,105.1,78.3,94,96.5,89.6,99,91.4,94.5,106.6,101.9,79.4,103.7,96.2,
          86.6,99.7,101.3,92.9,107.9,121.4,85,105.6,87.4,77.4,100.2,80.1,73.4,105.1,
          108.7,94.2,103.3,90.5,104.1,96.2,96.2,107,91.2,73.4,100.4,102,90,89,101.1,
          94.9,89.8,90.5,106.9,89.9,87.6,94.9,88.9,99.8,86.8,92.9,93.1,111.7,90,84.8,93)

# Obtenemos el histograma
resultado <- hist(datos)

# Frecuencia absoluta
absoluta <- resultado$counts

# Frecuencia relativa
relativa <- resultado$counts / length(datos)

# Frecuencia acumulada absoluta
cumsum(absoluta)

# Frecuencia acumulada relativa
cumsum(relativa)

# Calcular la media
mean(datos)

# Calcualar la variancia
var(datos)

#boxplot
boxplot(datos,horizontal=T)

summary(datos)

# Percentiles
quantile(datos,c(0,1,0.25))

median(datos)

#Asimetría y curtosis
install.packages("fBasics")
library("fBasics")

#asimetría
skewness(datos,method="moment")
kurtosis(datos,method="moment")