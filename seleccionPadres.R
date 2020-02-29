#Esta función se encarga de seleccionar los padres para su
#posterior reproducción. Aplica una selección proporcional al 
#fitness con una ruleta con reemplazamiento. 
#
#Los argumentos que recibe son:
#
# fit:                        vector con el fitness de la población actual 
# numPadres:                  número de padres a seleccionar
#
#Devuelve un vector con los índices de los elementos seleccionados.
#
#En caso de selecciones por torneo u otras, habrá
#que modificar adecuadamente la función y sus argumentos.
#
seleccionPadres <- function(fit,numPadres){ 
  
  if(!is.vector(fit))                                     #control de datos
    stop("La función seleccionPadres() debe llamarse con un vector\n");
  
  numElem <- length(fit)                                  #número de individuos en la poblacion actual

  if(sum(fit)<1E-15)                                      #todos los fitness prácticamente iguales  
    fit <- rep(1/numElem,numElem)                         #asignamos equiprobabilidad
  else   
    fit<- fit/sum(fit)                                    #fitness distintos, calculamos probabilidad
                                                          #proporcional al fitness
  
  selecc <- sample(1:numElem,size=numPadres,              #sorteo de tampPob elementos con probabilidades
                        replace=TRUE,prob=fit)            #fit del conjunto de 1 a numElem individuos
  return(selecc)
}

