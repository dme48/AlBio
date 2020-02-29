evaluadora<- function(poblacion)
{
  
  # Levy function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      w <- 1 + (poblacion[j,] - 1)/4
      term1 <- (sin(pi*w[1]))^2 
      term3 <- (w[d[2]]-1)^2 * (1+1*(sin(2*pi*w[d[2]]))^2)
      wi <- w[1:(d[2]-1)]
      sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))
      f[j] <- term1 + sum + term3
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}