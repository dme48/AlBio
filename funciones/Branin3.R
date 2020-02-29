
evaluadora<- function(poblacion)
{
  
  # Branin no. 1 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # Septiembre 2014
  if(is.matrix(poblacion)  ){
    d <- dim(poblacion) #d[1] filas/individuos, d[2] columnas/numero variables
    f <- vector(mode="numeric",length=d[1])
    a<-1; b<-5.1/(4*pi^2); c<-5/pi; r<-6; s<-10; t<-1/(8*pi)
    for(j in 1:d[1])
    {
      term1 <- a * (poblacion[j,2] - b*poblacion[j,1]^2 + c*poblacion[j,1] - r)^2
      term2 <- s*(1-t)*cos(poblacion[j,1])    
      f[j] <- term1 + term2 + s +5*poblacion[j,1]
      f[j] <- -f[j]
    }
  }
  else 
    stop
  return(f)
}


