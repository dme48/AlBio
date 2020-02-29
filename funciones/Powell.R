
evaluadora<- function(poblacion)
{
  
  # Powell function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      xxa <- poblacion[j,seq(1, d[2]-3, 4)]
      xxb <- poblacion[j,seq(2, d[2]-2, 4)]
      xxc <- poblacion[j,seq(3, d[2]-1, 4)]
      xxd <- poblacion[j,seq(4, d[2], 4)]
      
      sumterm1 <- (xxa + 10*xxb)^2
      sumterm2 <- 5 * (xxc - xxd)^2
      sumterm3 <- (xxb - 2*xxc)^4
      sumterm4 <- 10 * (xxa - xxd)^4
      f[j] <- sum(sumterm1 + sumterm2 + sumterm3 + sumterm4)
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


