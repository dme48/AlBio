evaluadora<- function(poblacion)
{
  
  # Langermann function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    m<-5
    cvec <- c(1,2,5,2,3)
    A <- matrix(c(3,5,5,2,2,1,1,4,7,9),5,2,byrow=TRUE)
    for(j in 1:d[1])
    {
      xxmat <- matrix(rep(poblacion[j,],times=m), m, d[2], byrow=TRUE)    
      inner <- rowSums((xxmat-A[,1:d[2]])^2)  
      outer <- sum(cvec * exp(-inner/pi) * cos(pi*inner))
      f[j] <- outer
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}