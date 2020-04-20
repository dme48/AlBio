""" filtro.py se encarga de leer los resultados escritos en
el fichero "resultados.txt", promediar los tiempos y
posiciones finales y crear un fichero "resultadosPromediados.txt" que
contenga estos resultados.
"""

import numpy as np

#Numero de veces que se resuelve cada problema
repetitions = 25
#Dirección de "resultados.txt"
inp = "Scripts/resultados.txt"
#Dirección donde se guarda la info. procesada:
out = "Scripts/resultadosPromediados.txt"
#Definimos las listas donde guardaremos los nombres de los
#problemas, los tiempos las posiciones y el numero de explosiones.
nombres, tiempos, posiciones, explosiones = [], [], [], []
#Abrimos el fichero con los resultados
f = open(inp, 'r')
#Init. la variable iterable lineNumber, que marca la
#linea actual.
lineNumber = 0
#Leemos todas las lineas en "resultados.txt"
for line in f:
    #Veces que hemos resuelto el prob. actual
    trialNumber = lineNumber % repetitions
    #Numero del problema actual
    problemNumber = int( lineNumber / repetitions )
    #Desglosamos la linea en palabras
    words = line.split()
    print(words[0], ", pos=", words[4], ", time=", words[5])
    #Cuando llegamos a un nuevo problema añadimos
    #un elemento a las listas
    if trialNumber == 0:
        nombres.append( words[0] )
        posiciones.append(0.0)
        tiempos.append(0.0)
        explosiones.append(0)
    #Vamos promediando a medida que recorremos las lineas
    tiempos[problemNumber] += float( words[5] ) / repetitions
    if abs(float(words[4])) > 100:
        explosiones[problemNumber] += 1
    else:
        posiciones[problemNumber] += float( words[4] )
    lineNumber += 1
f.close()
#Ahora escribimos los datos procesados
f = open(out, "w")
f.write("#Problema, posicionFinal, runtime\n")
for i in range(len(nombres)):
    #Si todos los intentos han explotado, no escribimos una solución.
    if int(repetitions-explosiones[i])==0:
        f.write(nombres[i] + ', explosión, ' + str(tiempos[i]) + '\n')
    else:
        #Promediamos entre el numero de ejemplos que no han explotado
        posiciones[i] /= repetitions - explosiones[i]
        f.write(nombres[i] + ', ' + str(posiciones[i]) + ', ' + str(tiempos[i]) + '\n')
f.close()
