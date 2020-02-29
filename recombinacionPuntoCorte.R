#Esta función se encarga de recombinar la subpoblación de padres
#aplicando una recombinación de tipo 'punto de corte'
#
#Los argumentos que recibe son:
#
# subPob:                     subpoblación de padres para recombinar
# pr:                         probabilidad de recombinación
# l,u:                        vector de cotas inferiores y superiores 
#                             de las variables de problema
#En el caso particular de la recombinación de punto de corte, solo se
#utilizan los dos primeros argumentos, población de padres y probabilidad de 
#recombinación.
#La función devuelve la población con los padres que se han recombiando
#y los que no.
#
recombinacion <- function(pobDePadres,pr,l,u){
  numIndividuos=dim(pobDePadres)[1]                       #Número de padres
  numVar=dim(pobDePadres)[2]                              #Tamaño cromosoma, número de variables
  if(numIndividuos==1)
  {
    cat(paste("En esta recombinacion implementada no hay nada que hacer",
           "ya que solo hay una variable. Se devuelve toda la poblacion"))
    return(pobDePadres)
  }
  
  aRecombinar <-which(runif(numIndividuos)<pr)            #seleccionamos los ínidices de los individuos 
                                                          #que se recombinan
  sizeARecombinar <- length(aRecombinar)
  if(sizeARecombinar%%2!=0)                               #si el numero es impar se quita uno
    sizeARecombinar <- sizeARecombinar-1 
  aRecombinar <- sample(aRecombinar)                      #desordenamos los seleccionados
  
  recombinados <- pobDePadres                             #Creamos matriz que contendrá a los recombinados, 
                                                          #inicialmente igual a la población de padres
  if(sizeARecombinar>=2)  
  {
    for(i in seq(1,sizeARecombinar-1,2)){                 #para cada pareja en recombinados
      puntoCorte=sample(2:(numVar),1)                     #sorteamos punto de corte
      recombinados[aRecombinar[i],puntoCorte:numVar]=   
        pobDePadres[aRecombinar[i+1],puntoCorte:numVar]   #intercambiamos colas, las cabeceras
      recombinados[aRecombinar[i+1],puntoCorte:numVar]=   #se han copiado al hacer la asignación
        pobDePadres[aRecombinar[i],puntoCorte:numVar]     #de recombinados
    }
  }
  else
  {
    return(pobDePadres)                                   #Si no se ha recombinado ningún inidividuo
  }                                                       #se devuelve la población de padres inicial
  return(recombinados)
} 

