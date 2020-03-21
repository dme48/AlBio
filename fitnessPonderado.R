# Esta funcion se encarga de calcular el fitnes ponderado.
#
# El input es:
#     - population         Lista con la poblacion sobre la que crear las mutaciones.
#     - fitness            Lista ordenada con el fitness de cada individuo.
#     - alpha              Peso de los terminos.
#
# El output es:
#     - term1+term2   Poblacion mutada a evaluar.
#
# Dependencias y Observaciones:
#     - La poblacion que se parsea tiene que estar ordenada segun la funcion objetivo.
#     - Esta pensado para un problema de minimizacion.

fitnessPonderado <- function(population, fitness, alpha) {
    
    ## Inicializa las dimensiones.
    indlen = length(population[1,])  # Longitud de los individuos.
    sizepop = length(population[,1]) # Numero de individuos en la poblacion.
    
    ## Calculo del primer termino.
    fmin = fitness[1]                # Min de la fun obj en la poblacion.
    fmax = fitness[sizepop]          # Max de la fun obj en la poblacion.
    if((fmax-fmin) > .Machine$double.eps) {
        term1 = alpha * (fitness-fmin)/(fmax-fmin)
    } else {
        term1 = rep(0,sizepop)
    }
    
    ## Calculo del segundo termino.
    xbest = population[1,]
    xbestMat = matrix(rep(xbest, each=sizepop), nrow=sizepop)
    dist = rowSums((xbestMat-population)*(xbestMat-population))
    Dmax = max(dist)
    if((Dmax + min(dist)) > .Machine$double.eps) {
        term2 = (1 - alpha) * (Dmax - dist)/(Dmax + dist)
    } else {
        term2 = rep(0,sizepop)
    }
    
    return(term1+term2)
}
