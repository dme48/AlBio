evaluadora<- function(poblacion)
{
  
  # TRID function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])  
    ii <- c(1:d[2])
    for(j in 1:d[1])
    {
      xi <- poblacion[j,2:d[2]]
      xold <- poblacion[j,1:(d[2]-1)]
      sum1 = (poblacion[j,1]-1)^2 + sum((xi-1)^2)
      sum2 <- sum(xi*xold)
      f[j] <- sum1 - sum2
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}