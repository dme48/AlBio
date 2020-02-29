#Esta función se encarga de mutar la subpoblación de padres
#ya recombinados aplicando una mutación aleatoria con un 
#cierto delta
#
#Los argumentos que recibe son:
#
# subPop:                     subpoblación de padres recombinados para mutar
# pm:                         probabilidad de mutacion
# l,u:                        vector de cotas inferiores y superiores 
#                             de las variables de problema
# delta:                      amplitud máxima de la mutación
#
#La función devuelve la población con los padres que se han mutado
#y los que no.
#
mutacion <- function(subPop,pm,l,u,delta){
  
   numIndiv=dim(subPop)[1]                                #filas/individuos
   numVar=dim(subPop)[2]                                  #columnas/numero variables

   probs <- matrix(runif(numIndiv*numVar)<pm,             #Sorteo de todas las posiciones 
                                numIndiv,numVar)          #de todos los individuos que se van a mutar
   
   if(length(probs)==0) return(subPop)                    #Si no se muta ninguna componente devolvemos 
                                                          #la población tal y como esta
    
    
    for(ii in 1:numIndiv){                                #para cada individuo
      for(jj in 1:numVar){                                #para cada variable
       if(probs[ii,jj]){                                  #si hay que mutar 
        valor <- l[jj]-0.1;      
        while(valor < l[jj] || valor > u[jj]){            #generamos mientras no estemos con un valor
          valor <-                                        #factible, entre las cotas 'l' y 'u'
            subPop[ii,jj]+delta*runif(1,-1,1)*(u[jj]-l[jj])
        }
        subPop[ii,jj] <- valor                            #asignamos el valor mutado
      }
     }
    }
    return(subPop)
  
}

