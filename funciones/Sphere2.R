evaluadora<- function(poblacion)
{
  
  # Sphere no. 2 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])  
    ii <- c(1:6)
    for(j in 1:d[1])
    {
      sum <- sum((poblacion[j,]^2)*(2^ii))
      f[j] <- (sum - 1745) / 899
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}