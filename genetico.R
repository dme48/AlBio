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
   CR <- 1 - index / m
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
          rand_i <- sample(1:m, m, replace = TRUE)   
          ind_mutados <- runif(m) < CR  |  c(1:m) != rand_i
          ind_iguales <- ind_mutados != TRUE
          trials[,d] <- ind_iguales * poblacion[,d] + ind_mutados * mutados[,d]
       }
   } 
   ##               CALCULO FITNESS TRIALS
   ##--------------------------------------------------------
   ##Que inicie por u indica que pertenece a los trials y no a poblacion:
   ufitness <- -evaluadora(trials)
   ufitness_w <- fitness_ponderada(trials, ufitness, alpha)
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
