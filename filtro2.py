""" filtro.py se encarga de leer los resultados escritos en
el fichero "resultados.txt", promediar los tiempos y
posiciones finales y crear un fichero "resultadosPromediados.txt" que
contenga estos resultados.
"""

import numpy as np
import math

#Numero de veces que se resuelve cada problema
repetitions = 25
#Dirección de "resultados.txt"
inp = "resultados.txt"
#Dirección donde se guarda la info. procesada:
out = "resultadosAgregados.txt"
#Definimos las listas donde guardaremos los nombres de los
#problemas, los tiempos las posiciones y el numero de explosiones.
nombres, tiempos, posiciones, explosiones, minlist, medlist = [], [], [], [], [], []
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
        elementos = []
    if trialNumber == repetitions-1:
        if elementos:
            # Minimo
            minlist.append(min(elementos))
            # Mediana
            elemento_central = math.floor(len(elementos)/2)
            medlist.append(elementos[elemento_central])
        else:
            minlist.append("NoConvergence")
            medlist.append("NoConvergence")
    #Tiempo Total Acumulado
    tiempos[problemNumber] += float( words[5] )
    if abs(float(words[4])) > 100:
        explosiones[problemNumber] += 1
    else:
        elementos.append(float(words[4]))
    lineNumber += 1
f.close()

#Ahora escribimos los datos procesados
f = open(out, "w")
f.write('{:<15} {:>25} {:>25} {:>25} \n'.format('Problema','minFObj','medianaFObj','runtime'))
for i in range(len(nombres)):
    f.write('{:<15} {:>25} {:>25} {:>25} \n'.format(nombres[i],minlist[i],medlist[i],tiempos[i]))
f.close()
