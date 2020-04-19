# Orquestra todas las componentes del algoritmo genetico: mutacion, crossover
# y seleccion de padres, ademas de la evaluacion de la fun obj y la ordenacion
# de la poblacion en base a fun obj para el ranking.
#
# El input es:
#     - seedE         Semilla para generar mismos parametros aleatorios por ejecucion.
#     - problem       Problema a resolver.
#     - sizepop       Tamano maximo de la poblacion.
#     - GMAX          Numero maximo de iteraciones.
#     - fileE         Fichero para escribir la salida de datos.
#
# El output es:
#     - NA
#
# Dependencias y Observaciones:
#     - Esta pensado para un problema de minimizacion.

evolutivo <- function(seedE, problem, sizepop, GMAX, fileE) {
  
  ## Inicializa el problema y la semilla.
  set.seed(seedE)                # Inicializa la seed del rand para obtener los mismos
                                 # resultados en cada ejecucion.
  info = inicializador(problem)  # Inicializa los datos del problema.
  indlen = info$n                # Longitud de los individuos. (Numero de variables).
  l = info$l;                    # Cotas inferiores de las variables
  u = info$u;                    # Cotas superiores de las variables
  rm(info)                       # Borrado de la lista info.
  
  ## Informacion general de la ejecucion, se muestra en la consola.
  cat(sprintf("Procesando problema: %s en dimension %d\n", problem, indlen))
  cat(sprintf("Semilla: %d\n", seedE))
  cat(sprintf("Numero maximo iteraciones: %d\n", GMAX))
  
  ti = proc.time()  #Guardamos comienzo de ejecucion
 
  ## Inicializamos la poblacion y parametros iniciales para la primera iteracion.
  NSG = sizepop                           # Inicializamos el num de indiv que pasan a
                                          # la siguiente iteracion a toda la poblacion.
  
   # Inicializamos la poblacion con individuos aleatorios entre las cotas.
  population = matrix(NA, sizepop, indlen)
  for(i in 1:indlen){
    population[,i] = runif(sizepop, l[i], u[i])
  }
  
  ## Iteramos hasta GMAX generaciones.
  for(G in 1:GMAX) {
     
   # Calculamos los parametros t y alpha.
   t = G/(GMAX+1)
   alpha = rnorm(1, 0.9, 0.05)
   if(alpha > 1) alpha = 1
   if(alpha < 0.8) alpha = 0.8
   
   # Calculamos fitness.
   # La funcion evaluadora devuelve la funcion objetivo de un problema de maximo.
   # El algoritmo es de un problema de minimos por lo que cambiamos el signo.
   fitness = -evaluadora(population)

   ## Ordenamos poblacion y fitness por fitness decreciente, ya que queremos
   ## la poblacion con fun obj mas baja en los primeros puestos para el ranking.
   aux = sort(fitness, decreasing = FALSE, index.return = TRUE)
   fitness = aux$x
   orden_fitness = aux$ix
   population = population[orden_fitness,] # Ordena la poblacion por fitness.
   rm(orden_fitness) # Eliminamos ya que la poblacion ya esta ordenada por fitness.
   
   # Calculamos la fitness ponderado.
   fitness_w <- fitnessPonderado(population, fitness, alpha)

   # Generamos una mutacion de todos los genes de todos los individuos.
   mutationOut = mutacion(population, fitness, l, u, t, NSG)
   Rg = mutationOut[["Rg"]] # Indice del guiding individual en el ranking de poblacion ordenada,
   mutationMat = mutationOut[["mutationMat"]] # Matriz con las mutaciones para los padres.
   
   # Calculamos el CR de cada individuo
   CR = 1-Rg/sizepop # CR es la prob que los padres tengan mutaciones.
   if(CR < 0.05) CR = 0.05
   if(CR > 0.95) CR = 0.95
   
   # Generamos los trials (padres mutados) a partir de la poblacion de padres y la matriz de mutacion.
   trialIndividuals = crossover(population, mutationMat, CR)

   # Calculamos el fitness y fitness ponderado de los trials (nuevos padres mutados).
   ufitness = -evaluadora(trialIndividuals)
   ufitness_w = fitnessPonderado(trialIndividuals, ufitness, alpha)

   # Seleccionamos los mejores individuos entre el padre y su trial para la nueva generacion.
   seleccionOut = seleccion(population, trialIndividuals, fitness, ufitness, fitness_w, ufitness_w)
   population = seleccionOut[["population"]]
   NSG = seleccionOut[["NSG"]]

   ## CRITERIO DE PARADA TBD
 }


 finEjecucion = proc.time()-ti
 fileConn <- file(fileE,open="at")                        #apertura del fichero de resultados
 writeLines(sprintf("%s\t%9d\t%3d\t%4d\t%f\t%f ",         #escribiendo informacion basica
                    problem,seedE,sizepop, GMAX,max(fitness),finEjecucion[3]), fileConn)
 close(fileConn)                                          #cierre de fichero de resultados
 
 cat(sprintf("Minimo Fun Obj = %f\n\n",min(fitness)))     #vfo alcanzado
 cat(sprintf("X(%d)= %8.6f ",1:length(l),population[which.min(fitness),]))
 cat("\n\n")
 
 return()
}
