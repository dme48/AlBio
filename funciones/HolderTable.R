
evaluadora<- function(poblacion)
{
  
  # HolderTable function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      fact1 <- sin(poblacion[j,1])*cos(poblacion[j,2])
      fact2 <- exp(abs(1 - sqrt(poblacion[j,1]^2+poblacion[j,2]^2)/pi))
      f[j] <- -abs(fact1*fact2)
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


