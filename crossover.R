# Esta funcion se encarga de combinar los indivuos originales con los vectores
# de mutacion para generar una poblacion de indivuos nuevos.
#
# El input es:
#     - population         Lista con la poblacion sobre la que crear las mutaciones.
#     - mutationMat        Lista ordenada con el fitness de cada individuo.
#     - CR                 Coeficiente de recombinacion. Probabilidad de mutacion de
#                          las componentes de los individuos de mutar,
#
# El output es:
#     - trialIndividuals   Poblacion mutada a evaluar.
#
# Dependencias y Observaciones:
#     - La poblacion que se parsea tiene que estar ordenada segun la funcion objetivo.
#     - Esta pensado para un problema de minimizacion.

crossover <- function(population, mutationMat, CR) {
  
    ## Inicializa las dimensiones.
    indlen = length(population[1,])   # Longitud de los individuos.
    sizepop = length(population[,1])  # Numero de individuos en la poblacion.
    
    ## Inicializamos a los trials, (la poblacion a mutar), con la poblacion de padres.
    trialIndividuals = population
      # En el caso 1D "todas" las componentes mutan.
    if(indlen == 1) {
      return(mutationMat)
    }
      # Para el caso general usamos (ec. 10) para mutar a los padres.
    else {
        # Posicion aleatorias para cada padre en las que se garantiza una mutacion.
      randni = sample(1:indlen, sizepop, replace = TRUE)
      for(index in 1:sizepop) {
          compToMutate = runif(indlen, 0, 1) <= CR # Mutacion con cierta probabilidad.
          compToMutate[randni[index]] = TRUE
          trialIndividuals[index,compToMutate] = mutationMat[index,compToMutate]
       }
    }
    
    return(trialIndividuals)
 }
