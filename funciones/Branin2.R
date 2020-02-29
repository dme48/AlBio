
evaluadora<- function(poblacion)
{
  
  # Branin no. 2 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      x1bar <- 15*poblacion[j,1] - 5
      x2bar <- 15 * poblacion[j,2]
      term1 <- x2bar - 5.1*x1bar^2/(4*pi^2) + 5*x1bar/pi - 6
      term2 <- (10 - 10/(8*pi)) * cos(x1bar)
      f[j] <- (term1^2 + term2 - 44.81) / 51.95
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


