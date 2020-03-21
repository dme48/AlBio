

crossover <- function(population, mutationMat, CR) {
  
  indlen = length(population[1,])
  sizepop = length(population[,1])
    ## Inicializamos a los trials con la poblacion
    trialIndividuals = population
    ## En el caso 1D "todas" las componentes se sustituyen
    if(indlen == 1) {
      return(mutationMat)
    }
    ## Para el caso general usamos ec. 10
    else {
      randni <- sample(1:indlen, sizepop, replace = TRUE)   # Componente que garantiza q haya una mut
      for(index in 1:sizepop) {
          compToMutate = runif(indlen, 0, 1) <= CR
          compToMutate[randni[index]] = TRUE
          trialIndividuals[index,compToMutate] = mutationMat[index,compToMutate]
       }
    }
    return(trialIndividuals)
 }
