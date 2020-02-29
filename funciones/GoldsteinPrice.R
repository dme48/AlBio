
evaluadora<- function(poblacion)
{
  
  # Goldstein-Price function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
   
    for(j in 1:d[1])
    {
      fact1a <- (poblacion[j,1] + poblacion[j,2] + 1)^2
      fact1b <- 19 - 14*poblacion[j,1] + 3*poblacion[j,1]^2 - 14*poblacion[j,2] + 6*poblacion[j,1]*poblacion[j,2] + 3*poblacion[j,2]^2
      fact1 <- 1 + fact1a*fact1b
      
      fact2a <- (2*poblacion[j,1] - 3*poblacion[j,2])^2
      fact2b <- 18 - 32*poblacion[j,1] + 12*poblacion[j,1]^2 + 48*poblacion[j,2] - 36*poblacion[j,1]*poblacion[j,2] + 27*poblacion[j,2]^2
      fact2 <- 30 + fact2a*fact2b
      
      f[j] <- fact1*fact2
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


