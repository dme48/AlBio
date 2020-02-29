
evaluadora<- function(poblacion)
{
  
  # Drop-wave function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      frac1 <- 1 + cos(12*sqrt(poblacion[j,1]^2+poblacion[j,2]^2))
      frac2 <- 0.5*(poblacion[j,1]^2+poblacion[j,2]^2) + 2
      f[j] <- -frac1/frac2
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}



