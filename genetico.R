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
genetico <- function(semilla=9876543,numIteraciones=50,fichero="result.txt",problema="Easom")
  
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
  #cat(sprintf("Tamano de Poblacion: %d\n",sizePoblacion))
  cat(sprintf("Numero iteraciones: %d\n",numIteraciones))
  #cat(sprintf("Probabilidad de mutación: %6.4f\n",pm))
  #cat(sprintf("Probabilidad de recombinación: %6.4f\n",pr))
  t <- proc.time()                                       #Guardamos comienzo de ejecución
  fitness <- vector("numeric",sizePoblacion)             #creando vector para fitness.
  poblacion <- matrix(NA,sizePoblacion, sizeCromosoma)   #creando matriz para almacenar poblacion
 
  ##La inicializacion de la poblacion concuerda con la del paper.
  for (i in 1:sizePoblacion){
    poblacion[i,] <- l+runif(sizeCromosoma)*(u-l)         #asignando valores a cada elemento de población
                                                          #en ausencia de otra información se inicializa con
                                                          #valores aleatorios entre l[j] y u[j], l[j]<=x[j]<=u[j]
  }
  fitness <- evaluadora(poblacion)                        #evaluando la población. El fitness corresponde
                                                          #con el valor de la función objetivo (los 
                                                          #problemas de la colección están puestos en forma 
                                                          #de máximo para directamente máximizar el fitness)
                                                          #La función evaluadora está definida de forma que 
                                                          #tiene en cuenta el nombre del problema pasado en la 
                                                          #llamada a la función genetico cuando (de esto se encargo
                                                          #la función 'inicia').
  

  ##numIter es "G" en el paper
 numIter<-0
 while(numIter<numIteraciones){                           #desde 0 a numIteraciones, numIteraciones poblaciones
   numIter <- numIter+1
   ##El shift no tiene pinta de dar problemas, pero habría que verlo.
   shift_ <- min(fitness)                                 #cálculo del mínimo fitness para restarlo y 
                                                          #garantizar que el fitness sea positivo y además
                                                          #se reduce rango
    ##Toda esta parte nuestro paper lo hace diferente. Comentamos y cambiamos todo. 
   indicePadres <- seleccionPadres(fitness-shift_,        #selección de padres para reproducción, devuelve 
                                           sizePoblacion) #la lista de los índices de los padres seleccionados
                                              
   
   variados <- poblacion[indicePadres,]                   #conjunto de padres a reproducir      
   
   variados <- recombinacion(variados,pr,l,u)             #recombinación de los padres seleccionados 
   
   variados <- mutacion(variados,pm,l,u,0.25)             #mutación de los padres ya recombinados
   
   fitnessVariados <- evaluadora(variados)                #cálculo del fitness de los nuevos hijos
   
   supervivientes <- seleccionSupervivientes(poblacion,   #selección de supervivientes que formarán
                 variados,fitness,fitnessVariados,       #la próxima población, devuelve una lista
                                      sizePoblacion,0.5)  #con los individuos seleccionados y su 
                                                          #fitness
   
   poblacion <- supervivientes$pob                        #reasignando los individuos seleccionados
   fitness <- supervivientes$fit                          #reasignando sus fitness 
   rm(supervivientes)
   #cat(sprintf("Maximo FO =%f en iteracion %d\n",max(fitness),numIter))
 }


 finEjecucion=proc.time()-t                               #registro de tiempo de ejecución transcurrido
 fileConn<-file(fichero,open="at")                        #apertura del fichero de resultados
 writeLines(sprintf("%s\t%9d\t%3d\t%4d\t%f\t%f ",         #escribiendo información básica
                    problema,semilla,sizePoblacion, numIteraciones,max(fitness),finEjecucion[3]), fileConn)
 close(fileConn)                                          #cierre de fichero de resultados
 
 cat(sprintf("Maximo FO =%f\n\n",max(fitness)))             #vfo alcanzado
 cat(sprintf("X(%d)= %8.6f ",1:length(l),poblacion[which.max(fitness),]))
 cat("\n\n")
 
 
}
