##selecciona() es una función que elige entre
##--------------------------------------------------------
selecciona <- function(poblacion, trials, fitness, ufitness, fitness_w, ufitness_w){
    ## VERSION LENTA Y SEGURA
    ## Iteramos sobre todos los individuos y comprobamos en que escenario
    ## estamos dentro de la ec(9). Cambiamos si estamos en el primero o en
    ## el segundo, siguiendo el paper. A medida que se cambia un individuo
    ## de la poblacion por un trial se incrementa NSG en 1 (NSG es el
    ## numero de individuos cambiados por un trial).
    NSG <- 0
    for (i in 1:NROW(poblacion)){
    cond1 <- ufitness[i] < fitness[i]
    cond2 <- ufitness_w[i] <= fitness_w[i] & i != 1
    change <- cond1 | cond2
        if (change){
            NSG <- NSG + 1
            poblacion[i] <- trials[i]
        }
    ## VERSION RAPIDA PERO QUE NO FUNCIONA
    #change <- ufitness < fitness | ( ufitness_w <= fitness_w & poblacion != poblacion[1,] )
    #poblacion[change] <- trials[change]
    #NSG <- sum( change[,1] )   
    aux <- list("pob" = poblacion, "NSG" = NSG)
    return(aux)
    }
}
