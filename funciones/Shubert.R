
evaluadora<- function(poblacion)
{
  
  # Shubert function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    ii <- c(1:5)
    for(j in 1:d[1])
    {
     sum1 <- sum(ii * cos((ii+1)*poblacion[j,1]+ii))
     sum2 <- sum(ii * cos((ii+1)*poblacion[j,2]+ii))
     f[j] <- sum1 * sum2
     f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


