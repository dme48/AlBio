#Esta funcion se encarga de mutar la subpoblacion de padres
#ya recombinados aplicando una estrategia combinada que varia entre
#una mutaci?n de tipo DE/current/1 y una DE/best/1 con una cierta
#probabilidad
#
#Los argumentos que recibe son:
#
# subPop:                     subpoblacion de padres mutar
# pm:                         probabilidad de mutacion
# l,u:                        vector de cotas inferiores y superiores 
#                             de las variables de problema
# delta:                      amplitud máxima de la mutación
#
#La funcion devuelve la poblacion con los padres que se han mutado
#y los que no.
#

mutacion <- function(poblacion, xi1) {
   
   F1 = 0.5
   F2 = 0.5
   xguide = poblacion[0]
   G = 10
   Gmax = 100
   
   
   # Calcula el valor de t y xi_2
   t = G/Gmax
   xi2 = (1+9*10^(5*(t-1)))/100
  
   
   # Calcula la cota superior e inferior de cada
   # una de las componentes de la poblacion de padres
   L = poblacion[0]
   U = poblacion[0]
   
   for(individuo in poblacion){
      for(j in 1:length(individuo)){
         if(individuo[j] < L[j]){
             L[j] = indiv[j]
         }
         if(individuo[j] > U[j]){
             U[j] = indiv[j]
         }
      }
   }
   
   
   # Calcula la variaci?n de la mutacion
   v = rep(rep(0,length(poblacion[0])),length(poblacion))
   for(i in length(poblacion)) {
      xcur = poblacion[i]
      rand = runif(1, 0, 1)
      
      dr2 = rep(0,length(xcur))
      xrand2 = #Seleccione de elemento aleatorio
      for(j in length(xcur)){
         if(runif(1, 0, 1) < xi2){
            dr2[j] = L[j]+runif(1, 0, 1)*(U[j]-L[j])
         }else{
            dr2[j] = xrand2[j]
         }
      }
      
      if(rand < xi1){
         v = xrand+f1*(xguide-xrand)+f2*(xrand1-dr2)
      }else{
         v = xcur+f1*(xguide-xcur)+f2*(xrand1-dr2)
      }
   }
   
   return(v)
}