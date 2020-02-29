
evaluadora<- function(poblacion)
{
  
  # Hartmann 4-D function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    alpha <- c(1.0, 1.2, 3.0, 3.2)
    A <- c(10, 3, 17, 3.5, 1.7, 8,
           0.05, 10, 17, 0.1, 8, 14,
           3, 3.5, 1.7, 10, 17, 8,
           17, 8, 0.05, 10, 0.1, 14)
    A <- matrix(A, 4, 6, byrow=TRUE)
    P <- 10^(-4) * c(1312, 1696, 5569, 124, 8283, 5886,
                     2329, 4135, 8307, 3736, 1004, 9991,
                     2348, 1451, 3522, 2883, 3047, 6650,
                     4047, 8828, 8732, 5743, 1091, 381)
    P <- matrix(P, 4, 6, byrow=TRUE)
    for(j in 1:d[1])
    {      
      xxmat <- matrix(rep(poblacion[j,],times=4), 4, 4, byrow=TRUE)
      inner <- rowSums(A[,1:4]*(xxmat-P[,1:4])^2)
      outer <- sum(alpha * exp(-inner))
      f[j] <- (1.1 - outer) / 0.839
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


