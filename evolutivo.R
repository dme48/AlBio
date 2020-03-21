#
#
# MISSING
# Algoritmo diseñado para maximizar
#


evolutivo <- function(seedE, problem, sizepop, GMAX, fileE) {
  
  ## Inicializa el problema y la semilla.
  set.seed(seedE)                # Inicializa la seed del rand para obtener los mismos
                                 # resultados en cada ejecución.
  info = inicializador(problem)  # Inicializa los datos del problema.
  indlen = info$n                # Longitud de los individuos. (Numero de variables).
  l = info$l;                    # Cotas inferiores de las variables
  u = info$u;                    # Cotas superiores de las variables
  rm(info)                       # Borrado de la lista info.
  
  ## Información general de la ejecucion, se muestra en la consola.
  cat(sprintf("Procesando problema: %s en dimension %d\n", problem, indlen))
  cat(sprintf("Semilla: %d\n", seedE))
  cat(sprintf("Numero maximo iteraciones: %d\n", GMAX))
  
  ti = proc.time()  #Guardamos comienzo de ejecución
 
  ## Inicializamos la población y parámetros iniciales para la primera iteracion.
  NSG = sizepop                           # Inicializamos el num de indiv que pasan a
                                          # la siguiente iteracion a toda la poblacion.
  
   # Inicializamos la poblacion con individuos aleatorios entre las cotas.
  population = matrix(NA, sizepop, indlen)
  for(i in 1:indlen){
    population[,i] = runif(sizepop, l[i], u[i])
  }
  
  ## HASTA AQUI
  
  ## Iteramos GMAX generaciones:
 for(G in 1:GMAX){                           #desde 0 a GMAX, GMAX generaciones
   t = G/(GMAX+1)
   # Calculamos el alpha para esta iteracion
   alpha = rnorm(1, 0.9, 0.05)
   if(alpha > 1) alpha = 1
   if(alpha < 0.8) alpha = 0.8
   # Calculamos fitness
   # La funcion evaluadora devuelve la funcion objetivo de un problema de maximo.
   # El algoritmo es de un problema de minimos.
   fitness = -evaluadora(population)

   ## Ordenamos poblacion y fitness por fitness creciente, ya
   ## que queremos minimizar fitness
   aux <- sort(fitness, decreasing = FALSE, index.return = TRUE)
   fitness <- aux$x
   orden_fitness <- aux$ix
   population <- population[orden_fitness,] # Ordena la pob por fitness.
   rm(orden_fitness) # Eliminamos ya que la pob ya esta ordenada por fitness
   
   
   ## Calculamos la fitness ponderada:
   fitness_w <- fitnessPonderado(population, fitness, alpha)
   #print(fitness_w)

   ##Generamos una mutacion de todos los genes de todos los individuos
   mutationOut = mutacion(population, fitness, l, u, t, NSG)
   Rg = mutationOut[["Rg"]] # Indice del guiding individual en la pob ordenada
   mutationMat = mutationOut[["mutationMat"]]
   
   ## Calculamos el CR de cada individuo
   ## CR es la probabilidad de que se hereden mutaciones en el "trial individual"
   CR = 1-Rg/sizepop
   if(CR < 0.05) CR = 0.05
   if(CR > 0.95) CR = 0.95
   
   ## Generamos los trials a partir de la población y los mutados
   trialIndividuals <- crossover(population, mutationMat, CR)

   ## Calculamos el fitness y fitness ponderado de los trials.
   ufitness = -evaluadora(trialIndividuals)
   ufitness_w = fitnessPonderado(trialIndividuals, ufitness, alpha)

   ## Seleccionamos individuos entre la poblacion y los trials para la nueva generacion.
   seleccionOut = seleccion(population, trialIndividuals, fitness, ufitness, fitness_w, ufitness_w)
   population = seleccionOut[["population"]]
   NSG = seleccionOut[["NSG"]]

   ## CRITERIO DE PARADA
 }


 finEjecucion = proc.time()-ti
 #print(finEjecucion)
 fileConn <- file(fileE,open="at")                        #apertura del fichero de resultados
 writeLines(sprintf("%s\t%9d\t%3d\t%4d\t%f\t%f ",         #escribiendo información básica
                    problem,seedE,sizepop, GMAX,max(fitness),finEjecucion[3]), fileConn)
 close(fileConn)                                          #cierre de fichero de resultados
 
 cat(sprintf("Maximo Fun Obj = %f\n\n",max(fitness)))             #vfo alcanzado
 cat(sprintf("X(%d)= %8.6f ",1:length(l),population[which.max(fitness),]))
 cat("\n\n")
 
 return()
}
