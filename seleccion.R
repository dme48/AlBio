##seleccion() es una función que elige entre
##--------------------------------------------------------
seleccion <- function(population, trialIndividuals, fitness, ufitness, fitness_w, ufitness_w) {

    cond1 = ufitness < fitness
    cond2 = ufitness_w < fitness_w
    cond2[1] = FALSE
    
    changeFilter = cond1 | cond2
    population[changeFilter,] = trialIndividuals[changeFilter,]
    
    NSG = length(which(changeFilter))

    return(list("population" = population, "NSG" = NSG))
}