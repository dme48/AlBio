genetico <- function(semilla=9876543,m,GMAX=50,fichero="result.txt",problema="Easom")
{
  info<-inicia(problema)
  n <- info$n                                             #número de variables
  l <- info$l;                                            #cotas inferiores de las variables
  u <- info$u;                                            #cotas superiores de las variables
  rm(info)                                                #borramos la lista info
  
  set.seed(semilla)                                       #inicia generador de números aleatorios
  
  #Información general de la ejecucion, se muestra en la consola.
  cat(sprintf("Procesando problema: %s (%d)\n",problema,n))
  cat(sprintf("Semilla: %d\n",semilla))
  cat(sprintf("Numero iteraciones: %d\n",GMAX))
  ti <- proc.time()                                       #Guardamos comienzo de ejecución
  ##Poblacion es una matriz de m x n (individuos * variables) elementos.
  poblacion <- matrix(NA,m, n)   #creando matriz para almacenar poblacion
 
  ##La inicializacion de la poblacion concuerda con la del paper, aleatoria entre l y u
  for(i in 1:n){
    poblacion[,i] <- runif(m, l[i], u[i])
  }
  ##Inicializamos el SR a 0:
  SR <- 0
  ##Inicializamos F2 a 0.5
  F2 <- 0.5
  ##Inicializamos NSG (offspring que pasa a nueva generacion) a m
  NSG <- m
  ##G es "G" en el paper
 for(G in 1:GMAX){                           #desde 0 a GMAX, GMAX poblaciones
   t <- G / ( GMAX + 1)
   ##Calculamos el alpha para esta iteracion
   alpha <- rnorm(1, 0.9, 0.05)
   if(alpha > 1) alpha <- 1
   if(alpha < 0.8) alpha <- 0.8
   ## Calculamos fitness
   fitness <- -evaluadora(poblacion)
   ## Ordenamos poblacion y fitness por fitness creciente, ya
   ## que queremos minimizar fitness
   aux <- sort(fitness, decreasing = FALSE, index.return = TRUE)
   fitness <- aux$x
   orden_fitness <- aux$ix
   poblacion <- poblacion[orden_fitness,]
   ## Calculamos la fitness ponderada:
   fitness_w <- fitness_ponderada(poblacion, fitness, alpha)


   ##Mutamos, generando una nueva generacion de individuos
   mutados <- mutacion(poblacion, fitness, l, u, t, NSG)
   ##Calculamos el CR de cada individuo
   ##CR es la probabilidad de que se hereden mutaciones en el "trial individual"
   CR <- 1 - orden_fitness / m
   print(CR)
   altos <- CR > 0.95
   bajos <- CR < 0.05
   CR[altos] <- 0.95
   CR[bajos] <- 0.05
   print(CR)

   ## Generamos los trials a partir de la población y los mutados
   trials <- genera_trials(poblacion, mutados, CR)

   ##Calculamos el fitness y fitness ponderado de los trials.
   ufitness <- -evaluadora(trials)
   ufitness_w <- fitness_ponderada(trials, ufitness, alpha)

   ## Seleccionamos individuos entre la poblacion y los trials para la nueva generacion.
   poblacion <- selecciona(poblacion, trials, fitness, ufitness, fitness_w, ufitness_w)
 }


 finEjecucion=proc.time()-ti
 fileConn<-file(fichero,open="at")                        #apertura del fichero de resultados
 writeLines(sprintf("%s\t%9d\t%3d\t%4d\t%f\t%f ",         #escribiendo información básica
                    problema,semilla,m, GMAX,max(fitness),finEjecucion[3]), fileConn)
 close(fileConn)                                          #cierre de fichero de resultados
 
 cat(sprintf("Maximo FO =%f\n\n",max(fitness)))             #vfo alcanzado
 cat(sprintf("X(%d)= %8.6f ",1:length(l),poblacion[which.max(fitness),]))
 cat("\n\n")
}
