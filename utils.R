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

## Funciones aportadas por Nicolás Bases

idc.para_u_con_desvio_conocido <- function(media, desvio, n, alfa = 0.05) {
  z <- -qnorm(alfa / 2)
  li <- media - (z * desvio / sqrt(n))
  ls <- media + z * desvio / sqrt(n)
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)
}

idc.para_u_sin_desvio_conocido <- function(media, s, n, alfa = 0.05) {
  tstu <- -qt(alfa / 2, n - 1)
  li <- media - (tstu * s / sqrt(n))
  ls <- media + (tstu * s / sqrt(n))  
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)
}

idc.para_proporcion <- function(x, n, alfa = 0.05) {
  p <- x / n
  z <- -qnorm(alfa / 2)
  li <- p - (z * sqrt(p * (1-p) / n))
  ls <- p + (z * sqrt(p * (1-p) / n))  
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)
}

# xA: la cantidad de exitos
# nA: total
# calcular previamente xA/nA y xB/nB y ubicarlos de mayor a menos
idc.para_diferencia_de_proporciones <- function(xA, nA, xB, nB, alfa = 0.05) {
  pA <- xA / nA
  pB <- xB / nB
  z <- -qnorm(alfa / 2)
  li <- pA - pB - (z * sqrt( (pA * (1-pA) / nA) + (pB * (1-pB) / nB) ))
  ls <- pA - pB + (z * sqrt( (pA * (1-pA) / nA) + (pB * (1-pB) / nB) ))  
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)
}

idc.para_varianza_poblacional <- function(n, s, alfa = 0.05) {
  li_chi <- qchisq(1 - alfa / 2, n - 1)
  li <- (n - 1) * s / li_chi
  ls_chi <- qchisq(alfa / 2, n - 1)
  ls <- (n - 1) * s / ls_chi
  
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)  
  
}

idc.para_diferencia_de_medias_con_desvios <- function(mediaA, desvioA, nA, mediaB, desvioB, nB, alfa = 0.05) {
  z <- -qnorm(alfa / 2)
  li <- mediaA - mediaB - z * sqrt(desvioA / nA + desvioB / nB)
  ls <- mediaA - mediaB + z * sqrt(desvioA / nA + desvioB / nB)  
    
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)  
  
}

idc.para_diferencia_de_medias_sin_desvios_iguales <- function(mediaA, sA, nA, mediaB, sB, nB, alfa = 0.05) {
  tstu <- -qt(alfa / 2, nA + nB - 2)
  sa <- ((nA - 1)*sA + (nB - 1)*sB) / (nA + nB - 2)
  li <- mediaA - mediaB - tstu * sa * sqrt(1 / nA + 1 / nB)
  ls <- mediaA - mediaB + tstu * sa * sqrt(1 / nA + 1 / nB)
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)  
  
}

idc.para_diferencia_de_medias_sin_desvios_no_iguales <- function(mediaA, sA, nA, mediaB, sB, nB, alfa = 0.05) {
  v_numerador <- (sA / nA + sB / nB)^2 
  v_denominadorA <- ((sA / nA)^2) / (nA - 1)
  v_denominadorB <- ((sB / nB)^2) / (nB - 1)  
  v <- v_numerador / (v_denominadorA + v_denominadorA)
  tstu <- -qt(alfa / 2, v)  
  li <- mediaA - mediaB - tstu * sqrt(sA / nA + sB / nB)
  ls <- mediaA - mediaB + tstu * sqrt(sA / nA + sB / nB)
  paste("*Limite Inferior:", li, "*Limiste Superior:", ls, "*Error: ", ls - li)  
  
}

