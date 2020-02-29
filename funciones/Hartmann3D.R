
evaluadora<- function(poblacion)
{
  
  # Hartmann 3-D function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    alpha <- c(1.0, 1.2, 3.0, 3.2)
    A <- c(3.0, 10, 30,
           0.1, 10, 35,
           3.0, 10, 30,
           0.1, 10, 36)
    A <- matrix(A, 4, 3, byrow=TRUE)
    P <- 10^(-4) * c(3689, 1170, 2673,
                     4699, 4387, 7470,
                     1091, 8732, 5547,
                     381, 5743, 8828)
    P <- matrix(P, 4, 3, byrow=TRUE)
    for(j in 1:d[1])
    {      
      xxmat <- matrix(rep(poblacion[j,],times=4), 4, 3, byrow=TRUE)
      inner <- rowSums(A[,1:3]*(xxmat-P[,1:3])^2)
      outer <- sum(alpha * exp(-inner))
      f[j] <- -outer
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


