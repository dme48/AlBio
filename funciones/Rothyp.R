evaluadora<- function(poblacion)
{
  
  # ROTATED HYPER-ELLIPSOID function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      xxmat <- matrix(rep(poblacion[j,],times=d[2]), d[2], d[2], byrow=TRUE)
      xxmatlow <- xxmat
      xxmatlow[upper.tri(xxmatlow)] <- 0  
      inner <- rowSums(xxmatlow^2)
      outer <- sum(inner)
      f[j] <- outer
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}