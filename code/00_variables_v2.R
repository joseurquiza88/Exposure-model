#################################################################

# Ejemplo 02 de modelo de exposicion basado en la movilidad y actividades
# 0. Correr librerias necesarias para el modelo. Sino estan instaladas
# instalar. Revisar cuales son  importantes!!
# 00_library

#1.  Correr las funciones de los archivos .R:
          # 01_recorridos_request.R
          # 02_funcion_point-to-line.R
          # 03_horas_v2.R
          # 04_exposicion_V04.R
          # 06_seleccion_puntos.R

#2. Ubicarse en el path donde se encuentran las grillas de CALPUFF
setwd("D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp")
#3. Variables

# ------------01. Puntos de origen-destino
distritos <- "D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/shape/distritos_interes.shp"
grilla <- "D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/grilla/grilla_utm.shp"
puntosOD <- seleccion_puntos_poblacion(distritos,grilla)
lista_viaje <- data.frame(long =c(puntosOD$x[2],puntosOD$x[4]),
                          lat = c(puntosOD$y[2],puntosOD$y[4]))
#num2
lista_viaje <- data.frame(long =c(-68.8361,-68.8212),
                          lat = c(-32.9563,-32.9247))
# V3
lista_viaje <- data.frame(long =c(-68.8212,-68.8325), #Guauymallen
                          lat = c(-32.8994,-32.9800)) #capital

  
# ------------02. Key tom-tom
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_1

# ------------03. Modos de transporte 
# Siempre considerar el ida - vuelta
modo = c("Auto","Auto")
modo = c("Pie","Pie")
modo = c("Bicicleta","Bicicleta")
# ------------04. Directorio con Grilla CALPUFF
# las grillas estan por dia-hora
directorio_grillas <- "D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp"
setwd(directorio_grillas)
concentraciones_grilla<-directorio_grillas

# ------------05. Tipo de ruta seleccionada
seleccion <- c("Mas rapida","Mas rapida")#,"Menos contaminada")
seleccion <- c("Mas corta","Mas corta")#,"Menos contaminada")
seleccion <- c("Mas contaminada","Mas contaminada")
seleccion <- c("Menos contaminada","Menos contaminada")#,"Menos contaminada")
seleccion <- c("Mas exposicion","Mas exposicion")#,"Menos contaminada")
seleccion <- c("Menos exposicion","Menos exposicion")#,"Menos contaminada")

## ------------07. Horas de Salida del hogar por primera vez. Dato aleatorio
dia <- "2018-08-01"
# Funcion que Trae datos aleatorios del horario_salida_hogar, tipo_transp,tiempo_act y las actividades a realizar)
#df_datos_aleatorios <- datos_aleatorios(1)
df_datos_aleatorios <-"12:01"

# solo tomamos la salida del hogar por primera vez
#horario_salida_hogar<- paste(dia," ",funcion_horas (as.numeric(df_datos_aleatorios[1])),":00 -03",sep = "")
#horario_salida_hogar<- paste(dia," ",funcion_horas (as.numeric(df_datos_aleatorios)),":00 -03",sep = "")
horario_salida_hogar<- paste(dia," ",df_datos_aleatorios,":00 -03",sep = "")

## ------------08. Tiempo de duracion de cada actividad. Dato aleatorio
#minutosActividades<-tiempo_act_aleatorios(num_actividad=1)
minutosActividades<-data.frame(tiempo_act=300)
setwd(directorio_grillas)
# Pruebas varias
# prueba_df <- exposicion_total(lista_viaje, modo, concentraciones_grilla=directorio_grillas ,key,
#                               seleccion,salida_exp="df",horario_salida_hogar, minutosActividades)
# 09:56
# Pruebas varias
# EL CALPUF DE LAS 00:00 HAY QUE PONERLE 00:01 PORQUE SINO NO ENCUENTRA LA HORA
setwd(directorio_grillas)
tipo = "df"
tipo = "plot"

prueba2 <- exposicion_total(lista_viaje, modo, concentraciones_grilla=directorio_grillas ,key,
                              seleccion,salida_exp=tipo,horario_salida_hogar, minutosActividades)

09:37
# ------------ Guardamos ejemplos
# Path local

# # Salida DF
# write.csv(prueba_df,"./prueba_df.csv")
# # Salida plot
# htmlwidgets::saveWidget(prueba_df2, "prueba_plot2.html")

name_save_csv <- paste("prueba_",seleccion[1],"-",seleccion[2],"_",modo[1],"-",modo[2],"punto3.csv",sep="")
name_save_plot <- paste("prueba_",seleccion[1],"-",seleccion[2],"_",modo[1],"-",modo[2],"punto3.html",sep="")
setwd("D:/Josefina/Proyectos/salud/pruebas_varias")
# Salida DF
tipo
write.csv(prueba,name_save_csv)
# Salida plot
htmlwidgets::saveWidget(prueba, name_save_plot)
seleccion
