
evaluadora<- function(poblacion)
{
  
  # Bohachevsky no. 1 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      term1 <- poblacion[j,1]^2
      term2 <- 2*poblacion[j,2]^2
      term3 <- -0.3 * cos(3*pi*poblacion[j,1])
      term4 <- -0.4 * cos(4*pi*poblacion[j,2])
      f[j] <- term1 + term2 + term3 + term4 + 0.7
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}

