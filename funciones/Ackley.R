evaluadora<- function(poblacion)
{
    
  # Ackley function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    a <- 20; b <- 0.2; c <- 2*pi;
    for(j in 1:d[1])
    {
      s1 <- sum(poblacion[j,]^2)
      s2 <- sum(cos(c*poblacion[j,]))
      term1 <- -a * exp(-b*sqrt(s1/d[2]))
      term2 <- -exp(s2/d[2])      
      f[j] <- term1 + term2 + a + exp(1)
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}