evaluadora<- function(poblacion)
{
    
  # Forrester function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      fact1 <- (6*poblacion[j,1] - 2)^2
      fact2 <- sin(12*poblacion[j,1] - 4)
      f[j] <- fact1 * fact2
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}