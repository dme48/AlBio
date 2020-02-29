
evaluadora<- function(poblacion)
{
  
  # De Jong no. 5 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    A = matrix(0, 2, 25)    
    a <- c(-32, -16, 0, 16, 32)
    A[1,] <- rep(a, times=5)
    A[2,] <- rep(a, each=5)
    sumterm1 <- c(1:25)
    for(j in 1:d[1])
    {
      sumterm2 <- (poblacion[j,1] - A[1,1:25])^6
      sumterm3 <- (poblacion[j,2] - A[2,1:25])^6
      sum <- sum(1 / (sumterm1+sumterm2+sumterm3))
      f[j] <- 1 / (0.002 + sum)
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


