# Esta funcion se encarga de crear un vector de mutacion para la poblacion
# de padres. La estrategia seguida es una mutacion de evolucion diferencial
# usando un guiding individual de una elite, el cual se combina linealmente
# con un hibrido de current-to-guiding y random-to-guiding.
#
# El input es:
#     - population         Lista con la poblacion sobre la que crear las mutaciones.
#     - fitness            Lista ordenada con el fitness de cada individuo.
#     - l                  Lower bound del problema.
#     - u                  Upper bounde del problema.
#     - t                  Cociente G/Gmax, iteración actual/numero max de iteraciones.
#     - NSG                Numero de indiv que fueron mutados en la iteracion anterior.
#
# El output es:
#     - iguide             El indice del guiding individual.
#     - v                  Matriz con los vectores de mutacion sobre la poblacion.
#
# Dependencias y Observaciones:
#     - La poblacion que se parsea tiene que estar ordenada segun la funcion objetivo.
#     - Esta pensado para un problema de maximizacion. (dependecia en fmin fmax).

mutacion <- function(population, fitness, l, u, t, NSG) {
   
   ## Inicializacion de parametros y variables auxiliares.
   v = list()                       # Lista de mutaciones.
   indlen = length(population[1,])  # Longitud de los individuos.
   sizepop = length(population[,1]) # Numero de individuos en la poblacion.
   dr2 = rep(0,indlen)              # Vector para la mutacion puramente aleatorio.
   vi = rep(0,indlen)               # Vector de mutacion final.
   fmin = fitness[1]                # Min de la fun obj en la poblacion.
   fmax = fitness[sizepop]          # Max de la fun obj en la poblacion.
   MAX_TRY = 10                     # Num maximo de intentos de los sorteos.
   
   ## Parametros especiales del problema.
   Pt = 1-t**3       # Expresion del maximo indice donde elegir el guiding indiv.
   SR = NSG/sizepop  # Numero de mutaciones que mejoraron su predecesor / tama?o pob.
   xi1 = 0.05 # 0.2 para problemas complicados.
   xi2 = (1+9*10^(5*(t-1)))/100
   xi3 = 0.05
   
   ## Seleccion del guiding individual.
   if(SR < xi3) {
      top10EliteLim = round(sizepop*0.1)  # Indice del ultimo elemento de POPs.
      iguide = sample(1:top10EliteLim, 1)
   } else {
      topPtEliteLim = round(sizepop*Pt)   # Indice del ultimo elemento de POPg.
      iguide = sample(1:topPtEliteLim, 1)
   }
   xguide = population[iguide,]
   fguide = fitness[iguide]
   ## Construccion del vector de mutacion.
   for(index in 1:sizepop) {
      # Determinacion del current individual y sus parametros de combinacion.
      xcur = population[index,]
      fcur = fitness[index]
      if(fcur > fguide) {
         F1 = (1+((fmax-fguide)/(fmax-fmin)))/2
      }
      else {
         F1 = -rnorm(n = 1, mean = 0.5, sd = 0.2)
         if(F1>-0.05) {
            F1 = -0.05
         }
         if(F1 < -0.95) {
            F1 = -0.95
         }
      }
      F2 = 0.5
      
      # Sortea los indices de r1 y r2 (si tras MAX_TRY sorteos no encuentra
      # indices diferentes entre si de xcur, xr1, xr2 devuelve un warning, no crea
      # la mutacion y pasa al siguiente individuo).
      k = 0
      while(k <= MAX_TRY) {
         r2 = sample(1:sizepop, 1)
         if(r2 != index){
            break
         }
         k = k+1
      }
      k = 0
      while(k <= MAX_TRY) {
         r1 = sample(1:sizepop, 1)
         if(r1 != index && r1 != r2) {
            break
         }
         k = k+1
      }
      if(k > MAX_TRY) {
         warning(paste0("Tras ", MAX_TRY, " intentos de muestreo,",
                        "no se han conseguido indices diferente de xcur, xr1, xr2."))
         v[[index]] <- xcur
         next
      }
      xr2 = population[r2,]
      xr1 = population[r1,]
      
      # Calcula el vector dr2 que interviene en la componente
      # fija de aleatoriedad de la mutacion.
      for(comp in indlen) {
         if(runif(1, 0, 1) < xi2) {
            dr2 = l[comp]+runif(1, 0, 1)*(u[comp]-l[comp])
         } else { 
            dr2 = xr2[comp]
         }
      }
      
      # Encuentra el vector v de mutacion y lo anade a la lista.
      if(runif(1, 0, 1) < xi1) {
         xrand = population[sample(1:sizepop, 1),]
         vi = xrand+F1*(xguide-xrand)+F2*(xr1-dr2)
      } else {
         vi = xcur+F1*(xguide-xcur)+F2*(xr1-dr2)
      }
      
      v[[index]] = vi
   }
   
   v_matrix = matrix(as.numeric(unlist(vi)), nrow = sizepop, ncol = indlen)
   return(list("Rg" = iguide, "mutationMat" = v_matrix))
}