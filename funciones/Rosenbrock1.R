evaluadora<- function(poblacion)
{
    
  # Rosembrock 1 function adpated by P. Mateo from codes in ww.sfu.ca/~ssurjano/index.html
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
      xi <- poblacion[j,1:(d[2]-1)]
      xnext <- poblacion[j,2:d[2]]
      f[j] <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)
      f[j] <- -f[j];      
    }
  }
  else
    stop
  return(f)
  
}