evaluadora<- function(poblacion)
{
  # Zakharov function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    for (j in 1:d[1])
    {
      ii <- c(1:d[2])
      s1 <- sum(poblacion[j,]^2)
      s2 <- sum(0.5*ii*poblacion[j,])
      f[j] <- s1 + s2^2 + s2^4      
      f[j]=-f[j]
    }
  }
  else
    stop
  return(f)
}