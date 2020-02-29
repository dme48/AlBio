evaluadora<- function(poblacion)
{
    
  # Colville function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      term1 <- 100 * (poblacion[j,1]^2-poblacion[j,2])^2
      term2 <- (poblacion[j,1]-1)^2
      term3 <- (poblacion[j,3]-1)^2
      term4 <- 90 * (poblacion[j,3]^2-poblacion[j,4])^2
      term5 <- 10.1 * ((poblacion[j,2]-1)^2 + (poblacion[j,4]-1)^2)
      term6 <- 19.8*(poblacion[j,2]-1)*(poblacion[j,4]-1)
      f[j] <- term1 + term2 + term3 + term4 + term5 + term6
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}