# Ejercicio 142

obs <- c(5, 4, 8, 6, 3,
         9, 7, 8, 6, 9,
         3, 5, 2, 3, 7,
         2, 3, 4, 1, 4,
         7, 6, 9, 4, 7)

trat <- c(rep("A", 5), rep("B", 5), rep("C", 5), rep("D", 5) ,rep("E", 5))

# Interpretar gráficamente los datos
boxplot(obs ~ trat)

# Prueba de Bartlett para la homocedasticidad
bartlett.test(obs ~ trat)

# ANOVA 
summary(aov(obs ~ trat)) 

# Si se desea realizar una prueba de cochran
install.packages("outliers")
library("outliers")
cochran.test(obs ~ trat)