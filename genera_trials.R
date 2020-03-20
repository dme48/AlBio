genera_trials <- function(poblacion, mutados, CR){
    n <- NCOL(poblacion)
    ## Inicializamos a los trials con la poblacion
    trials <- poblacion
    ##En el caso 1D "todas" las componentes se sustituyen
    if(n == 1){
      return(mutados)
    }
    ##Para el caso general usamos ec. 10
    else{
      for(d in 1:n){ 
          rand_i <- sample(1:m, m, replace = TRUE)   
          ind_mutar <- runif(m) < CR  |  c(1:m) != rand_i
          trials[ind_mutar,d] <- mutados[ind_mutar, d]
       }
    }
    return(trials)
 }
