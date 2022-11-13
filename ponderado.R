# Estudiantes:
# Paola Arrieta Vega - B90742
# Marco Espinoza Marín - B82760
# aniel López Juárez - B94351
# Génesis Solano Villegas - B57991
# Antonio Suárez Quesada - B94351

# Cuadro 1: número de líneas de lineas por persona 

# Para leer un archivo .sav utilizamos la librería Haven

# Librerias
library(haven)
library(dplyr)
library(survey)


base = read_sav("ACTUALIDADES2022_ponderacion_estudiantes.sav")

# Cuadro 1: Número de líneas por persona

table(base$CS10)  
(total=sum(table(base$CS10)))

# Segun la tabla anterior se observaron 18 individuos que no respondieron (999)
#pero es necesario sabes en qué filas específicas estan 
which(base$CS10==999)

#generamos las probabilidades aleatorias
set.seed(123)
(aleat = runif(18, min=0 , max=1))

#Creamos una base sin los 999 para ver la proporción acumulada de lineas telefónicas

# Hay que resolver esto
base %>%
       filter(CS10 != 999) %>%
       group_by(CS10) %>%
       summarise(
              n = n(),
              prop = CS10/sum(CS10),
              .groups = 'rowwise'
       )

base1 = subset(base,base$CS10 != 999)
cumsum(prop.table( table(base1$CS10,base1$Complete) ) )  

#limpiamos la base, quitando los 999 por el número de lineas de celular obtenidas aleatoriamente 
# para cada individuo
base[c(13,33,196,264,295,471,828,922,1338,1346,1368,1518,1696,1700,1759,1797,1802,1884), 
      148] <- c(1,2,1,2,2,1,1,2,1,1,2,1,1,1,1,2,1,1)

#verificar que si se hizo el cambio
(asegurar= base[c(13,33,196,264,295,471,828,922,1338,1346,1368,1518,1696,1700,1759,1797,1802,1884), 
                 148])


# Cuadro 2. Lineas por persona sin valores faltantes 
table(base$CS10)

#Creamos el factor de 
base$fb = 1/base$CS10

 #Cuadro 3
summary(base$fb) 

#....................................................................................................

#Paso 2. Ajuste por no respuesta
library(car)        


#SEXO=CS1, EDAD=CS2, EDUCACION=CS3

#Obtenemos la persona con edad más avanzada para realizar la recodificación
base %>% 
      filter(CS2 != 999) %>%
      summarise(
            edad_max = max(CS2)
      )

#volvemos nivel de educacion numerica
base$CS3=as.numeric(base$CS3)

#recodificamos la variable nivel de educacion

base$educa = car::recode(base$CS3,"1:2='Primaria';3:4='Secundaria';5:6='Universitaria'")

#recodificamos la variable edad
base$edad = car::recode(base$CS2,"18:29 ='18-29';30:49='30-49';50:89='50 o mas'")


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
base2 = base %>%
       filter(CS3 != 9)

base2 %>%
       group_by(CS3) %>%
       summarise(
              n = n()
       )

#distribucion marginal
cumsum(prop.table(table(base2$CS3)))  #debería ser con educa no?

#numeros aleatorios
set.seed(123)
(aleat = runif(16, min=0 , max=1))

#cambiamos 9 por los imputados

#En qué filas específicas estan los 9
which(base$CS3 == 9)

#Número de la columna en la que se deben de cambiar los valores
which(colnames(base) == "CS3") 

base[c(815,1156,1264,1338,1498,1501,1508,1696,1700,1759,1768,1772,1779,1789,1797,1800), 
      132] <- c(3,6,4,6,6,1,4,6,5,4,6,4,6,5,2,6)

(asegurar1= base[c(815,1156,1264,1338,1498,1501,1508,1696,1700,1759,1768,1772,1779,1789,1797,1800), 
                 132])

#Valores del nivel educativo imputados
table(base$CS3) 
#Continuamos con los valores de la edad

#eliminamos los Na
base3 = base %>%
       filter(CS2 != 999)

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

#Numero de la columna en la que se deben de cambiar los valores
which(colnames(base)=="CS2") 
base[c(196,391,662,767,922,1100,1338,1346,1477,1492,1525,1696,1700,1759,1768,1779,1781,1797,1800), 
      131] <- c(30,50,30,50,50,18,30,50,30,30,50,30,30,30,18,50,18,30,30)

(asegurar3= base[c(196,391,662,767,922,1100,1338,1346,1477,1492,1525,1696,1700,1759,1768,1779,1781,1797,1800), 
                 131])

#Valores de edad imputados
table(base$CS2) 
base$edad=recode(base$CS2,"18:29 ='18-29';30:49='30-49';50:89='50 o mas'") # volvemos a recodificar debido a la imputación
base$educa=recode(base$CS3,"1:2='Primaria';3:4='Secundaria';5:6='Universitaria'")
#.........................................................................................

#CON LA ENCUESTA DE LA ENAHO

ENAHO2021 = read_sav("ENAHO 2021.sav")

#Recodificación de variables
# Primero filtramos la base
ENAHO2021 = ENAHO2021 %>% filter(A5 >= 18)

#recodificamos
ENAHO2021$A5 = as.numeric(ENAHO2021$A5)
ENAHO2021$NivInst = as.numeric(ENAHO2021$NivInst)

ENAHO2021$edadE = factor(car::recode(ENAHO2021$A5,"18:29 ='18-29';30:49='30-49';50:97='50+'"))

ENAHO2021$educaE = factor(car::recode(ENAHO2021$NivInst,"0:2='Primaria';3:6='Secundaria';7:8='Universitaria'"))

# Volvemos a filtrar la base eliminando los valores missing

baseE = ENAHO2021 %>%
              filter(educaE != 99)


diseno <- svydesign(ids=~1, data=baseE, weight=~FACTOR)

#Tablas con valores asignados por el factor
(tablaEdu = svytable( ~ educaE + edadE + A4, design=diseno))

#Tablas de frecuencia relativa
tablaEdu/sum(tablaEdu)

#Buscar valores faltantes
table(ENAHO2021$A4); table(ENAHO2021$A5)

max(ENAHO2021$A5)

table(ENAHO2021$educaE)
#Hay 7 valores faltantes en educacion

#Buscamos la posición de los valores faltantes
ENAHO2021[ENAHO2021$educaE==99,1:5]


#numeros aleatorios para la educacion
set.seed(123)
(aleat.ed = runif(7, min=0 , max=1))

#Sustituir los valores

baseE2$A5=as.numeric(baseE2$A5)
baseE2$NivInst=as.numeric(baseE2$NivInst)
baseE2$edadE=as.factor(recode(baseE2$A5,"18:29 ='18-29';30:49='30-49';50:97='50 o mas'"))
baseE2$educaE=as.factor(recode(baseE2$NivInst,"0:2='Primaria';3:6='Secundaria';7:8='Universitaria'"))

baseE2[c(1027,3666,3710,3760,28801,30705,30706),"educaE"]=c("Primaria","Universitaria","Secundaria","Universitaria","Universitaria","Primaria","Secundaria")

#Verificamos que se hizo el cambio
baseE2[c(1027,3666,3710,3760,28801,30705,30706),"educaE"]

#Realizamos de nuevo los cuadros, ahora sin valores faltantes
baseE2=subset(baseE2,A5>=18)
baseE2$edadE=as.factor(recode(baseE2$A5,"18:29 ='18-29';30:49='30-49';50:97='50 o mas'"))
diseno2<-svydesign(ids=~1, data=baseE2, weight=~FACTOR)

#Tablas con valores asignados por el factor
B=svytable(~educaE+edadE+A4,design=diseno2)
B

#Tablas de frecuencia relativa
B/sum(B)

#Nota: Para los siguientes pasos se debe usar baseE2 (esta tiene los datos ya imputados).
#...........................................................................................
#Ahora se debe deben de incluir los pesos conforme la edad y el nivel de educación 
#Se utiliza la última base creada de la encuesta de actualidades
base$edd.ed.s=paste(base$edad,"-",base$educa,"-",base$CS1)
library(dplyr)

base$fnr=ifelse(base$edd.ed.s=="18-29 - Primaria - 1",3.99717918,
              ifelse(base$edd.ed.s=="30-49 - Primaria - 1",1.906555126,
                     ifelse(base$edd.ed.s=="50 o mas - Primaria - 1",2.895604239,
                            ifelse(base$edd.ed.s=="18-29 - Primaria - 2",2.111208687,
                                   ifelse(base$edd.ed.s=="30-49 - Primaria - 2",1.937595546,
                                          ifelse(base$edd.ed.s=="50 o mas - Primaria - 2",2.881831991,
                                                 ifelse(base$edd.ed.s=="18-29 - Secundaria - 1",0.998827491,
                                                        ifelse(base$edd.ed.s=="30-49 - Secundaria - 1",0.877964903,
                                                               ifelse(base$edd.ed.s=="50 o mas - Secundaria - 1",0.996420893,
                                                                      ifelse(base$edd.ed.s=="18-29 - Secundaria - 2", 1.153196909,
                                                                             ifelse(base$edd.ed.s=="30-49 - Secundaria - 2", 0.940507408,
                                                                                    ifelse(base$edd.ed.s=="50 o mas - Secundaria - 2",1.06069511,
                                                                                           ifelse(base$edd.ed.s=="18-29 - Universitaria - 1",0.480170893,
                                                                                                  ifelse(base$edd.ed.s=="30-49 - Universitaria - 1",0.438345104,
                                                                                                         ifelse(base$edd.ed.s=="50 o mas - Universitaria - 1",0.46213578,
                                                                                                                ifelse(base$edd.ed.s=="18-29 - Universitaria - 2",0.53377378,
                                                                                                                       ifelse(base$edd.ed.s=="30-49 - Universitaria - 2",0.512632706,
                                                                                                                              0.71766584)))))))))))))))))
sort(table(base$fnr,base$Complete))
sort(table(base$edd.ed.s,base$Complete))  #Note que todos los valores están asignados

base$factor=base$fb*base$fnr

summary(base$factor)   #Cuadro 8 


