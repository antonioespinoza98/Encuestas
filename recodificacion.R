# Tarea de recodificación, observaciones de Marco Espinoza

#-------------
#LIBRERIAS
library(haven)
library(dplyr)
library(writexl)
#-------------
base = data.frame(read_sav('ACTUALIDADES2022_raw.sav'))
# revisamos la  base, vamos a utilizar Srvyr para filtrar por las que yo realicé

base = base %>% 
        filter(Srvyr == "marco")
# Una vez filtrada la base, empezamos con la recodificación

# ------------------- RECODIFICACIÓN ---------------------------------------------

#--------------------  DROGAS M1_O -----------------------------------------------

base %>% 
    group_by(M1_O) %>% 
    summarise(
        n = n()
    )

# pasamos a numeric para poder hacer el recode

base = base %>% 
        mutate(M1_O = car::recode(M1_O, " 'LSD' = 1;  'alcohol' = 9") )

#--------------------  BTG2_O Biotec -----------------------------------------------

base %>% 
    group_by(BT2G_O) %>% 
    summarise(
        n = n()
    )

base = base %>% 
        mutate(BT2G_O = car::recode(BT2G_O, "'busqueda de carrera' = 1") )

#--------------------  R13 -----------------------------------------------

base %>% 
    group_by(R13) %>% 
    summarise(
        n = n()
    )

base = base %>% 
        mutate(R13 = car::recode(R13, "  'Con recursos humanos. sintió discriminación' = 1; 
        'De pensarlo sí, pero no supo como hacerlo. Por una situación que no quiso decir' = 1;
        'En el trabaja discriminaban a las personas nicaraguenses y ven.' = 3;
        'La gente le gusto discriminarme' = 1   ") )



#--------------------  S_31_5 -----------------------------------------------

# No tuve que recodificar
base %>% 
    group_by(S_31_5) %>% 
    summarise(
        n = n()
    )

#--------------------  JL3 -----------------------------------------------
base %>% 
    group_by(JL3) %>% 
    summarise(
        n = n()
    )

# Por alguna razón, el recode no se puede hacer en un solo código

base$JL3 = car::recode(base$JL3, " 'Es pensionado' = 2;
'Le parece una manera disfrasada de sobrecargar a las personas, y en otros países es un feracos' = 9;
'Le sirve porque es cansado pero le sirve para el estudio.' = 5;
'Más familiar porque tenemos más integración y le parece que se puede aprovechar' = 1;
'más tiemop' = 5;
'Más tiempo con los guilas' = 1;
'Más tiempo libre' = 5;
'Mejor las jornadas de 8 por las costumbre' = 11;
'mucho tiempo' = 2;
'No le gustaria' = 9 ")

base$JL3 = car::recode(base$JL3, " 'Por el tiempo, más de descanso' = 2;
'Porque disponer de más tiempo para estudiar' = 5;
'Porque es más cansado y el aumento de horas' = 9;
'Porque es mas cansado, es más que siuficiente' = 9;
'Porque es más tiempo más cansado' = 9;
'Porque estudia, entrena todos los días y tendría más tiempo' = 5;
'Porque le parece más útil condensar las actividades' = 5;
'Porque le queda un poco más de espacio uno, más tiempo libre' =5;
'Porque más comonidad mejor 8 horas.' = 11;
'Porque más tiempo' = 5;
'Porque mi horario de trabajo son 6 días 12 horas por 6 días' = 4;
'Porque queda el tiempo libre se aprovecha mucho más.' = 5 ")


base$JL3 = car::recode(base$JL3, "  'Porque quedan más tiempo de descanso pára aprovechar' = 2;
'Porque rinde menos el tiempo, hay que trabajar 8 horas un poquito más o menos' = 5;
'Porque sentiría que me rinde más los días' = 5;
'Porque sería lo mismo' = 4;
'porque tiene más días seguidos y 212 horas no es tan pesado' = 2;
'Porque tiene más tiempo' = 5;
'Porque tiene más tiempo libre' = 5;
'porque trabajar 12 horas es más pesado, el ha trabajado 13 horas y se deteriora mucho la persona y es más cansado' = 8;
'Porque ya la persona despues de 8 horas va a estar cansada' = 9; 
'Porque yo trabajando bajaría de nivel si mentalmente hago 12 horas, no es sostenible' = 9;
'Trabaja 16 horas, podría 12 horas.' = 4;
'Trabajar 8 horas serían 4 horas, lo que dura en traslado sería mejor tener un día de descanso' = 2  ") 

table(base$JL3)

#--------------------  Q_48_S -----------------------------------------------

base %>% 
    group_by(Q_48_S) %>% 
    summarise(
        n = n()
    )

# Aquí el problema es que por alguna razón, Corea del Sur aparece pero si juega el mundial

#--------------------  Q_52_S -----------------------------------------------

base %>% 
    group_by(Q_52_S) %>% 
    summarise(
        n = n()
    )

# Esta variable la tengo vacia


#--------------------  Q_55_S -----------------------------------------------

base %>% 
    group_by(Q_55_S) %>% 
    summarise(
        n = n()
    )

base = base %>% 
        mutate(Q_55_S = car::recode(Q_55_S, " 'Adventista' = 3; 'catolica' = 1; 'Catolica' = 1;
        'Catolicismo' = 1; 'catolico' = 1; 'Catolico' = 1; 'evagelica' = 2; 'Evagelico' = 2;
        'Evangelica' = 2; 'Evangelico' = 2;  'Protestante' = 3;  'TEstigo de Jehova' = 2  ") )

#--------------------  S_56_6 -----------------------------------------------

base %>% 
    group_by(S_56_6) %>% 
    summarise(
        n = n()
    )

# No hay nada que codificar

# Ahora hay que pasar esto al archivo. En vez de typear uno por uno, vamos a hacer algo más sencillo

basemarco = base %>%
    select(SbjNum, Srvyr, M1_O, BT2G_O, BT3, R13, S_31_5, JL3, Q_48_S, Q_52_S, Q_55_S, S_56_6) %>%
    filter(Srvyr == 'marco')


# Utilizamos esta función para guardarlos

write_xlsx(basemarco,"basemarco.xlsx")






















