evaluadora<- function(poblacion)
{
    
  # Dixon-Price function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    ii <- c(2:d[2])
    for(j in 1:d[1])
    {
      x1 <- poblacion[j,1]    
      term1 <- (x1-1)^2      
      xi <- poblacion[j,2:d[2]]
      xold <- poblacion[j,1:(d[2]-1)]
      sum <- sum(ii * (2*xi^2 - xold)^2)
      f[j] <- term1 + sum
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}