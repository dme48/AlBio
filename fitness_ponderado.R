##Definimos la funcion fitness ponderada. "ind" son los individuos a evaluar
## y fun_fitness es la funcion del fitness original. alpha es un valor rand
## calculado en genetico.R. El alpha utilizado tiene que ser el mismo cuando
##fitness_ponderada se calcula sobre los padres y sobre los trials.
fitness_ponderada <- function(ind, fitness, alpha)
{
    ## Definimos las dimensiones de poblacion:
    m <- NROW(ind)
    n <- NCOL(ind)
    ## PRIMER TERMINO
    ##-------------------------
    fmin <- min(fitness)
    fmax <- max(fitness)
    ter1 <- alpha * (fitness - fmin) / (fmax - fmin)
    ## SEGUNDO TERMINO
    ##-------------------------
    ##Buscamos la posición del individuo con menor fitness
    ibest <- min(fitness)$ix
    ##Creamos una matriz en la que se repite el mejor de los individuos:
    xbest <- matrix(rep(ind[ibest,], each=m),nrow=m)
    ##Calculamos distancias y guardamos la mayor de ellas
    distancias <- colSums( (xbest - poblacion) * (xbest - poblacion) )
    Dmax <- max(distancias)
    ##guardamos el segundo termino
    ter2 <- (1 - alpha) * (Dmax - distancias)/(Dmax + distancias)
    return(ter1 + ter2)
}
