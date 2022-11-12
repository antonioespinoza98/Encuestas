
# Marco Antonio
# Daniel 
# Genesis
#Paola 
# Antonio

library(haven)

base = read_sav("ACTUALIDADES2022_poderacion_estudiantes.sav")

# Cuadro 1 # de lineas por persona 
table(base$CS10)  
(total=sum(table(base$CS10)))

# Segun la tabla anterior se observaron 18 individuos que no respondieron (999)
#pero es necesario sabes en qué filas específicas estan 
which(base$CS10==999)

#generamos las probabilidades aleatorias
set.seed(123)
(aleat = runif(18, min=0 , max=1))

#Creamos una base sin los 999 para ver la proporción acumulada de lineas telefónicas
library(dplyr)
base1=subset(base,base$CS10!=999)
cumsum(prop.table(table(base1$CS10,base1$Complete)))  
# Procedimiento completo en excel, ver cuadro 


#limpiamos la base, quitando los 999 por el # de lineas de celular obtenidas aleatoriamente para cada individuo
base[c(13,33,196,264,295,471,828,922,1338,1346,1368,1518,1696,1700,1759,1797,1802,1884), 
      148] <- c(1,2,1,2,2,1,1,2,1,1,2,1,1,1,1,2,1,1)

#verificar que si se hizo el cambio
(asegurar= base[c(13,33,196,264,295,471,828,922,1338,1346,1368,1518,1696,1700,1759,1797,1802,1884), 
                 148])


# Cuadro 2. Lineas por persona sin valores faltantes 
table(base$CS10)

#Creamos el factor de 
base2$fb = 1/base2$CS10

 #Cuadro 3
summary(base2$fb) 

#....................................................................................................

#Paso 2. Ajuste por no respuesta
library(car)        
detach(package:dplyr)

#SEXO=CS1, EDAD=CS2, EDUCACION=CS3

#Obtenemos la persona con edad más avanzada para realizar la recodificación
mayor=subset(base,base$CS2!=999) 
max(mayor$CS2)

#volvemos nivel de educacion numerica
base$CS3=as.numeric(base$CS3)

#recodificamos la variable nivel de educacion
base$educa=recode(base$CS3,"1:2='Primaria';3:4='Secundaria';5:6='Universitaria'")

#recodificamos la variable edad
base$edad=recode(base$CS2,"18:29 ='18-29';30:49='30-49';50:89='50 o mas'")


####Creamos tabla con valores NA para cada uno de los sexos. El cuadro se realiza en excel
#Cuadro 4
table(base$edad,base$educa,base$CS1)  

#................................................................................................

#Para el cuadro 5 se deben de imputar los valores faltantes 

table(base$CS3)  
# Presentamos 16 valores con NAS en el nivel educativo 

table(base$CS2)  
# Presentamos 19 valores con NAS en la edad  


#Primero para el nivel educativo 
#Eliminamos los NAS para crear la distribución marginal 
base2=subset(base,base$CS3!=9)

#tabla
table(base2$CS3)
#distribucion marginal
cumsum(prop.table(table(base2$CS3)))  #debería ser con educa no?

#numeros aleatorios
set.seed(123)
(aleat = runif(16, min=0 , max=1))

#como se pusieron el 3, 6, ... está en excel?

#cambiamos 9 por los imputados

#En qué filas específicas estan los 9
which(base$CS3==9)

#Número de la columna en la que se ddeben de cambiar los valores
which(colnames(base)=="CS3") 

base[c(815,1156,1264,1338,1498,1501,1508,1696,1700,1759,1768,1772,1779,1789,1797,1800), 
      132] <- c(3,6,4,6,6,1,4,6,5,4,6,4,6,5,2,6)

(asegurar1= base[c(815,1156,1264,1338,1498,1501,1508,1696,1700,1759,1768,1772,1779,1789,1797,1800), 
                 132])

#Valores del nivel educativo imputados
table(base$CS3) 



#Continuamos con los valores de la edad

#eliminamos los Na
base3=subset(base,base$CS2!=999)
table(base3$CS2)

# podemos realizarlo así o con las edades sin agrupar, consultar con la profe
#probabilidad  marginal
cumsum(prop.table(table(base3$edad)))  

#numeros aleatorios
set.seed(123)
(aleat = runif(19, min=0 , max=1))

#el resto está en excel?

#CAMBIAMOS LOS 999 POR LOS IMPUTADOS

#en qué filas específica está
which(base$CS2==999)
sum(table(base$edad))


#.........................................................................................

