#Esta función se encarga de aplicar un algoritmo evolutivo (se llama genético porque 
#los operadores que proporciona este esqueleto corresponden a algoritmos que en general 
#son de tipo genético)

#Los argumentos que recibe son:
#
# semilla:           semilla utilizada para inicializar generadores de valores aleatorios,
#                    de esta forma se pueden realizar las resoluciones de un mismo problema 
#                    planteando las mismas condiciones iniciales o distintas condiciones iniciales.
# sizePoblacion:     tamaño de la poblacion ##Eliminado
# pm:                probabilidad de mutacion, si -1 se transforma en 1/n ##Eliminado
# pr:                probabiliad de recombinacion ##Eliminado
# numMaxIteraciones: numero de iteraciones (generaciones de poblaciones) a realizar
# fichero:           nombre del fichero en el que se guardarán los resultados
# problema:          nombre del problema a optimizar
#
# Si no se indican parámetros toma los defectos que aparecen en la declaracion  de la funcion
#semilla=9876543, tamaño de población igual a 100 individuos, probabilidad de mutación 0.1, 
#probabilidad de recombinación 0.8, genera 50 poblaciones, guarda los resultados en el 
#fichero 'result.txt' y aproxima el óptimo de la función 'Easom'
genetico <- function(semilla=9876543,sizePoblacion,numIteraciones=50,fichero="result.txt",problema="Easom")
{
 
  #Comienza, mediante la llamada a la función 'inicia' encargada de establecer
  #la informacion asociada al problema que se va a resolver, su numero de variables, cotas, etc. 
  #devuelve una lista con tres elementos info$l, info$u e info$n. 'l' y 'u'  vectores de cotas inferiores
  #y superiores, respectivamente, del valor de las variables y 'n' número de variables del problema.
  #También se encarga de cargar la función con la definición del problema a resolver.
  info<-inicia(problema)
  sizeCromosoma <- info$n                                 #tamaño del cromosma (número de variables)
  l <- info$l;                                            #cotas inferiores de las variables
  u <- info$u;                                            #cotas superiores de las variables
  rm(info)                                                #borramos la lista info
  
  set.seed(semilla)                                       #inicia generador de números aleatorios
  
  #Información general de la ejecucion, se muestra en la consola.
  cat(sprintf("Procesando problema: %s (%d)\n",problema,sizeCromosoma))
  cat(sprintf("Semilla: %d\n",semilla))
  cat(sprintf("Numero iteraciones: %d\n",numIteraciones))
  ti <- proc.time()                                       #Guardamos comienzo de ejecución
  poblacion <- matrix(NA,sizePoblacion, sizeCromosoma)   #creando matriz para almacenar poblacion
 
  ##La inicializacion de la poblacion concuerda con la del paper, aleatoria entre l y u
  for(i in 1:sizePoblacion){
    poblacion[i,] <- runif(sizeCromosoma, l, u)
  }
  ##Inicializamos el SR a 0:
  SR <- 0
  ##Inicializamos F2 a 0.5
  F2 <- 0.5
  ##Inicializamos NSG (offspring que pasa a nueva generacion) a sizePoblacion
  NSG <- sizePoblacion
  ##numIter es "G" en el paper
 for(numIter in 1:numIteraciones){                           #desde 0 a numIteraciones, numIteraciones poblaciones
   t <- numIter / ( numIteraciones + 1)
   ##Calculamos el alpha para esta iteracion
   alpha <- rnorm(1, 0.9, 0.05)
   if(alpha > 1) alpha <- 1
   if(alpha < 0.8) alpha <- 0.8
   ##                   CALCULO FITNESS
   ##-------------------------------------------------------
   ## Calculamos fitness y ordenamos segun ella
   fitness <- -evaluadora(poblacion)
   aux <- sort(fitness, decreasing = FALSE, index.return = TRUE)
   fitness <- aux$x
   index <- aux$ix
   poblacion <- poblacion[index,]
   ## Calculamos la fitness ponderada:
   fitness_w <- fitness_ponderada(poblacion, fitness, alpha)
   ##                   MUTACION
   ##-------------------------------------------------------
   ##Mutamos, generando una nueva generacion de individuos
   mutados <- mutacion(poblacion, fitness, l, u, t, NSG)
   ##Calculamos el CR de cada individuo
   ##CR es la probabilidad de que se hereden mutaciones en el "trial individual"
   CR <- 1 - index / sizePoblacion
   altos <- CR > 0.95
   bajos <- CR < 0.05
   CR[altos] <- 0.95
   CR[bajos] <- 0.05

   ##               GENERACION DE TRIALS
   ##-------------------------------------------------- 
   #Definimos la dimension y el vector de trials
   dim <- NCOL(poblacion)
   trials <- 0 * poblacion
   ##En el caso 1D "todas" las componentes se sustituyen
   if(dim == 1){
      trials <- mutados
   }
   ##Para el caso general usamos ec. 10
   else{
      for(d in 1:dim){ 
          rand_i <- sample(1:sizePoblacion, sizePoblacion, replace = TRUE)   
          ind_mutados <- runif(sizePoblacion) < CR  |  c(1:sizePoblacion) != rand_i
          ind_iguales <- ind_mutados != TRUE
          trials[,d] <- ind_iguales * poblacion[,d] + ind_mutados * mutados[,d]
       }
   } 
   ##               CALCULO FITNESS TRIALS
   ##--------------------------------------------------------
   ##Que inicie por u indica que pertenece a los trials y no a poblacion:
   ufitness <- -evaluadora(trials)
   ufitness_w <- fitness_ponderada(trials, ufitness, alpha)
   ##               SELECCION DE TRIALS
   ##--------------------------------------------------------
   NSG <- 0
   for (ind in 1:sizePoblacion){
    change <- ufitness[ind] < fitness[ind] | ( ufitness_w[ind] <= fitness_w[ind] & ind != 1 )
    if (change){
        NSG <- NSG + 1
        poblacion[ind] <- trials[ind]
    }
   }
   #change <- ufitness < fitness | ( ufitness_w <= fitness_w & poblacion != poblacion[1,] )
   #poblacion[change] <- trials[change]
   #NSG <- sum( change[,1] )   
 }


 finEjecucion=proc.time()-ti
 fileConn<-file(fichero,open="at")                        #apertura del fichero de resultados
 writeLines(sprintf("%s\t%9d\t%3d\t%4d\t%f\t%f ",         #escribiendo información básica
                    problema,semilla,sizePoblacion, numIteraciones,max(fitness),finEjecucion[3]), fileConn)
 close(fileConn)                                          #cierre de fichero de resultados
 
 cat(sprintf("Maximo FO =%f\n\n",max(fitness)))             #vfo alcanzado
 cat(sprintf("X(%d)= %8.6f ",1:length(l),poblacion[which.max(fitness),]))
 cat("\n\n")
}
