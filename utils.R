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

## Funciones aportadas por Gabriel Jaime

# Calcula la moda para frecuencias en intervalos de igual y distinta amplitud
modo <- function(li,ls,frec) {
  
  a <- 1
  max <-frec[1]
  for (i in 1:length(li))
   { if (max < frec[i]) 
    { a <- i ; max<-frec[i] }
  }
  
 li[a] +( ( ( frec[a] / (ls[a]-li[a]))-(frec[a-1] / (ls[a-1]-li[a-1]))) / (((frec[a] / (ls[a]-li[a]))-(frec[a-1] / (ls[a-1]-li[a-1])))+((frec[a] / (ls[a]-li[a]))-(frec[a+1] / (ls[a+1]-li[a+1])))))  * (ls[a]-li[a])
  
}

# Calcula los percentiles que se solicitan por parametro per
percentil <- function(li,ls,frec, per) 
{
  
  sm <- sum(frec)
  fb <- (per * sm) /100
  a <- 1
  fracu <-frec[1]
  while (fracu < fb) 
  { a <- a+1 ; fracu <- fracu + frec[a]  } 
    
  li[a] + ((fb - (fracu - frec[a]) )/ frec[a]) * (ls[a]-li[a])
  
  
}

# Calcula la asimetria 
asimetria <- function(li,ls,frec) 
{
  ( media.intervalos(li,ls,frec) - modo(li,ls,frec)) /sqrt(variancia.intervalos(li,ls,frec))
  
  
}

# Calcula el coeficiente de asimetria 
coe.asiemtria <- function(li,ls,frec) 
{
 
  marcas <- (ls + li)/2
  
  m <- media.intervalos(li,ls,frec)
  marcas <- (ls + li)/2
  n <- sum(frec)
  
  (1/n * sum((marcas - m)^3 * frec ))/( 1/n * sum( (marcas - m)^2 * frec ))^(3/2)
  
  
}

# Calcula el coeficiente de curtosis
coe.curtosis <- function(li,ls,frec) 
{
 
  marcas <- (ls + li)/2
  
  m <- media.intervalos(li,ls,frec)
  marcas <- (ls + li)/2
  n <- sum(frec)
  
  ((1/n * sum((marcas - m)^4 * frec ))/( 1/n * sum( (marcas - m)^2 * frec ))^(2))-3
  
  
}
