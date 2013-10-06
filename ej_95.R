# Ejercicio 95

li <- c(0.15,0.25,0.35,0.45,0.50,0.55,0.60,0.65,0.7)
ls <- c(0.25,0.35,0.45,0.50,0.55,0.60,0.65,0.7,0.75)
frec <- c(8,14,28,24,39,51,106,84,11)

hist.intevalos <- function(li,ls,frec) {
    marcas <- (ls + li)/2
    muestra <- c()
    for (i in 1:length(marcas)) {
         muestra <-c(muestra,rep(marcas[i],frec[i]))
    }
    hist(muestra,breaks=c(li,ls[length(ls)]))
}

hist.intevalos(li,ls,frec)

#Calcular la media
marcas <- (ls + li)/2
media <- sum(marcas*frec)/sum(frec)
# dibuja la media
points(media,0,col="red",pch=18)

