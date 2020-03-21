##Definimos la funcion fitness ponderada. "ind" son los individuos a evaluar
## y fun_fitness es la funcion del fitness original. alpha es un valor rand
## calculado en genetico.R. El alpha utilizado tiene que ser el mismo cuando
##fitness_ponderada se calcula sobre los padres y sobre los trials.

# Dependencia de la poblacion ordenada.

fitnessPonderado <- function(population, fitness, alpha) {
    ## Definimos las dimensiones de poblacion:
    indlen = length(population[1,])  # Longitud de los individuos.
    sizepop = length(population[,1]) # Numero de individuos en la poblacion.
    ## PRIMER TERMINO
    ##-------------------------
    fmin = fitness[1]                # Min de la fun obj en la poblacion.
    fmax = fitness[sizepop]          # Max de la fun obj en la poblacion.
    if((fmax-fmin) > .Machine$double.eps) {
        term1 = alpha * (fitness-fmin)/(fmax-fmin)
    } else {
        term1 = rep(0,sizepop)
    }
    ## SEGUNDO TERMINO
    ##-------------------------
    ##Buscamos la posición del individuo con menor fitness
    xbest = population[1,]
    ##Creamos una matriz en la que se repite el mejor de los individuos:
    xbestMat = matrix(rep(xbest, each=sizepop), nrow=sizepop)
    ##Calculamos distancias y guardamos la mayor de ellas
    dist = rowSums((xbestMat-population)*(xbestMat-population))
    Dmax = max(dist)
    ##guardamos el segundo termino
    if((Dmax + min(dist)) > .Machine$double.eps) {
        term2 = (1 - alpha) * (Dmax - dist)/(Dmax + dist)
    } else {
        term2 = rep(0,sizepop)
    }
    
    return(term1+term2)
}
