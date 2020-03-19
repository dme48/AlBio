# Este fichero se encarga de lanzar la ejecucion de los algoritmos y de guardar la informacion correspondiente
#Cargamos las funciones necesarias, genetico.R, mutacionUniforme.R, etc.
setwd("~/Documents/Zaragoza/AB/Trabajo Evolucion Diferencial/Scripts")
source("genetico.R")
source("mutacion.R")         #define una función llamada mutacion()
source("fitness_ponderado.R")          #Calcula fitness_w
source("inicia.R")                   #inicia el problema a resolver y sus caracteristicas
       
#Lista de problemas incorporados (se podría ampliar)
#
prob <- c("Ackley", "Bukin6", "CrossInTray", "DropWave" ,"Eggholder" ,"GramacyLee12","Griewank","HolderTable",
"Langermann","Levy" ,"Levy13", "Rastrigin","Schaffer2","Schaffer4","Schwefel","Shubert","Bohachevsky1","Bohachevsky2",
"Bohachevsky3","Perm0db","Rothyp","Sphere","Sphere2","Sumpow","Sumsqu","Trid","Booth","Matyas","Mccormick","Powersum",
"Zakharov","Camel3","Camel6","DixonPrice","Rosenbrock1","Rosenbrock2","DeJong5","Easom","Michalewicz","Beale",
"Branin1","Branin2","Branin3","Colville","Forrester","GoldsteinPrice","Hartmann3D","Hartmann4D","Hartmann6D","Permdb",
"Powell","Shekel","StyblinskiTang")

#Conjunto de semillas para inicializar el generador de numeros aleatorios
semilla <- c( 352668,  628434,  492990,  528643,  477348,  855426,  570702,  957864, 1019818,  849154,  982709,  991540,  776820,  302260,  509101,
             1104259,  778274,  937185, 1102620,  514412, 1026644,  288393,  848117, 1153861,  473884,  578922,  465690, 1092241,  538478,  764238,
             1005899,  434185,  681939, 1065173, 1177813,  178308, 1123423, 1159720,  280842,  563670,  694785,  918578,  854191, 1179079,  845770,
             1154990,  474168,  675549,  417239, 1007395)

#Parametros de ejecucion, que deberan fijarse de acuerdo a vuestro experimento
#
numRepeticiones <- 5  #cuantas veces se resuelve cada problema

sizePopulation <- 100 #tamaño de la población ##NP en el paper
numIteraciones <- 1000  #numero de iteraciones del algoritmo ## en el paper G_max

## pm y pr tienen equivalentes pero se redefinen en cada iteracion, las comentamos aqui
#pm <- -1.0            #probabilidad de mutación (si procede), si pm=-1, el programa asume como
                       #pm 1/numero_de_variables 
#pr <- 0.8             #probabilidad de recombinación (si procede)

#Indicamos que problemas se van a resolver
#Todos problemas <- 1:53
#Los diez primeros problemas <-1:10
#Los diez últimos problemas <- 44:53
#Unos cuantos
#problemas <- c(1,3,7,9,12,22,34)
problemas<-c(1)

#Nombre del fichero en el que se almacenan los resultados. 
#Si no existe lo crea y copia los resultados,
#si existe añade los nuevos resultados al contenido previo.
ficheroRes <- "resultados.txt"


#Bucle principal, para cada problema ejecuta el algoritmo 
#de optimización numRepeticiones veces, obteniendo una
#colección de valores de tiempo de ejecución y valor alcanzado
#con el que elaborar las estadisticas que se estimen oportunas
for (i in problemas){
  for (j in 1:numRepeticiones){  
   genetico(semilla[j], sizePopulation,numIteraciones,ficheroRes,prob[i])
  }
}

