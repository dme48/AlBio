# Esta funcion se encarga de seleccionar los mejores individuos entre los padres
# y los individuos mutados.
#
# El input es:
#     - population         Lista con la poblacion de padres.
#     - trialIndividuals   Lista con la poblacion mutada.
#     - fitness            Lista ordenada con el fitness de los padres.
#     - ufitness           Lista ordenada con el fitness de los individuos mutados.
#     - fitness_w          Lista ordenada con el fitness ponderado de los padres.
#     - ufitness_w         Lista ordenada con el fitness ponderado de los indviduos mutados.
#
# El output es:
#     - iguide             Poblacion seleccionada para la siguiente generacion.
#     - NSG                Numero de inviduos nuevos en la nueva generacion.
#
# Dependencias y Observaciones:
#     - La poblacion que se parsea tiene que estar ordenada segun la funcion objetivo.
#     - Esta pensado para un problema de minimizacion.

seleccion <- function(population, trialIndividuals, fitness, ufitness, fitness_w, ufitness_w) {

    # Condiciones para sustituir a los padres y renovacion.
    cond1 = ufitness < fitness
    cond2 = ufitness_w < fitness_w
    cond2[1] = FALSE
    
    changeFilter = cond1 | cond2
    population[changeFilter,] = trialIndividuals[changeFilter,]
    
    # Conteo de los individuos mutados que han sustituido a los padres.
    NSG = length(which(changeFilter))

    return(list("population" = population, "NSG" = NSG))
}