

evaluadora<- function(poblacion)
{
  
  # Bukin6 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      term1 <- 100 * sqrt(abs(poblacion[j,2] - 0.01*poblacion[j,1]^2))
      term2 <- 0.01 * abs(poblacion[j,1]+10)
      f[j] <- term1 + term2
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}