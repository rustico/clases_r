# calcula la variancia poblacional
varp(x) <- function(x) var(x)*(length(x) - 1)/length(x)

# Visualiza el histograma cuando los datos se encuentran agrupados por 
# intervalos
hist.intevalos <- function(li,ls,frec) {
  marcas <- (ls + li)/2
  muestra <- c()
  for (i in 1:length(marcas)) {
    muestra <-c(muestra,rep(marcas[i],frec[i]))
  }
  hist(muestra,breaks=c(li,ls[length(ls)]))
}

# Calcula la media cuando los datos se encuentran agrupados por intervalos
media.intervalos <- function(li,ls, frec) {
  marcas <- (ls + li)/2
  # devuelve el siguiente valor
  sum(marcas*frec)/sum(frec)
}

variancia.intervalos <- function(li,ls, frec) {
  marcas <- (ls + li)/2
  m <- media.intervalos(li,ls,frec)
  sum((marcas-m)^2*frec)/sum(frec)
}