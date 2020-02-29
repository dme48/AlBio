
evaluadora<- function(poblacion)
{
  
  # Shekel function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])   
    m <- 10
    b <- 0.1 * c(1, 2, 2, 4, 4, 6, 3, 7, 5, 5)
    C <- c(4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
           4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.0,
           4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
           4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.0)
    C <- matrix(C, 4, 10, byrow=TRUE)
    Ct <- t(C)    
    for(j in 1:d[1])
    {      
      
      xxmat <- matrix(rep(poblacion[j,],times=m), m, 4, byrow=TRUE)
      inner <- rowSums((xxmat-Ct[,1:4])^2)
      outer <- sum(1/(inner+b))
      f[j] <- -outer
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


