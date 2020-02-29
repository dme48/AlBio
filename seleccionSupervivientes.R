#Esta función se encarga de seleccionar los supervivientes para la próxima
#generación. Utiliza una selección proporcional al fitness con reemplazamiento y
#un porcentaje de elitismo fijo 'porcent'
#
#Los argumentos que recibe son:
#
#pobcurr:                    población actual 
#pobnew:                     subpoblacion seleccionada
#fcurr:                      fitness de la población actual 
#fnew:                       fitnes de la población seleccionada
#cuantos:                    tamaño de la población a seleccionar
#porcent:                    porcentaje de elitismo utilizado 

#
#Deveuelve una lista con dos elementos, la población de seleccionados y 
#su fitness.
#
seleccionSupervivientes <- function(pobcurr,pobnew,fcurr,fnew,cuantos,porcent){
  elitismo <- porcent                                     #el 'porcent'% de las mejores se mantiene siempre
  
  pob <- rbind(pobcurr,pobnew)                            #Unimos las dos poblaciones
  f <- c(fcurr,fnew)                                      #Unimos los dos fitness
  mm <- min(f)                                            #minimo fitness
  nIndivTotal <- dim(pob)[1]
  fcorr <- (f-mm)                                         #reescalado y correción a valores no negativos
  a <-order(-fcorr)                                        #obtencion de los indices ordenados segun fcorr

  if(sum(fcorr)<1E-15)    
    fcorr <- rep(1/nIndivTotal,nIndivTotal)               #si suma de fitness pequeña equiprobabilidad forzada
  else   
    fcorr<-fcorr/sum(fcorr)                               #prob proporcional al fitness
  
  elitismo<- ceiling(elitismo*cuantos)                    #estableciendo número de soluciones elitistas
  cuantos <- cuantos -elitismo                            #estableciendo resto de soluciones 
  
  quien <- sample(1:nIndivTotal,cuantos                   #muestra de individuos con prob proporcional al fitness
                                   ,replace=TRUE,fcorr)
  individuos <- NULL                                     #creamos lista con
  individuos$pob <-                                      #individuos seleccionados y el 'porcent'% de los mejores
      rbind(pob[quien,],pob[a[1:elitismo],])
  individuos$fit <- c(f[quien],f[a[1:elitismo]])         #fitness correspondiente
  return(individuos)
  }