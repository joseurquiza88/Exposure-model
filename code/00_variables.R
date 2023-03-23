
#######################################################################
# Prueba de la funcion:
# ------------             ELECCION DE LA MEJOR RUTA     -------------   
# ------------ VARIABLES   
# ------------01. Key tom-tom
# CONTRASEÑAS VARIAS CUENTAS DE TOM TOM. 2500 Request por dia cada una
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_1

# ------------02. Modos de transporte

modo = "Camion"
modo = "Colectivo"
modo = "Bicicleta"
modo = "Motocicleta"
modo = "Pie"
modo ="Auto"
#modo="car"
# ------------03. Coordenadas de origen
#Ejemplo 1
origen <-"-33.05324,-68.774"
#Ejemplo 2
origen <-"-32.79679,-68.816" # CAPDEVILLA
# ------------03. Coordenadas de destino
#Ejemplo 1
destino <- "-32.90212,-68.761"
#Ejemplo 2
destino <-"-32.86246,-68.859" #CIENEGUITA

# ------------04. Grilla de contaminacion local
concentraciones_grilla <- "D:/Josefina/Proyectos/salud/movilidad_7/grilla_contaminantes/buffer_wgs.shp"#st_read("D:/Josefina/Proyectos/salud/movilidad_7/grilla_contaminantes/buffer_wgs.shp")
concentraciones_grilla <- "D:/Josefina/Proyectos/salud/movilidad_7/grillas/grilla_00.shp"#st_read("D:/Josefina/Proyectos/salud/movilidad_7/grilla_contaminantes/buffer_wgs.shp")

# ------------05. hora de salida
horas_interes <- "2023-02-09 12:20:00 -03"

# ------------05. Corremos la funcion global
prueba_alternativas1 <- alternativas_recorridos (origen=origen,destino=destino,modo="Auto",key=key_1,salida = "df",horario = horas_interes)
prueba_plot <- alternativas_recorridos (origen=origen,destino=destino,modo="Auto",key=key,salida = "plot",horario = horas_interes)
prueba_poly3 <- alternativas_recorridos (origen=origen,destino=destino,modo="Auto", concentraciones_grilla,key=key,salida = "polyline",horario = horas_interes)
prueba_1_plot <- plot_recorridos (origen=origen,destino=destino,modo="Auto", concentraciones_grilla,key=key_2)

# ------------06. Lo podemos guardar 
#Guardar en en un HTML y visualizarlo en el Chrome

setwd("D:/Josefina/Proyectos/salud/movilidad_7/")
htmlwidgets::saveWidget(prueba_plot, "alternativas_recorridos_plot.html")

#Guardar en en un shape
writeOGR(prueba_poly3,"D:/Josefina/Proyectos/salud/movilidad_7","alternativas_recorridos_polyline", driver="ESRI Shapefile")

#Guardar en en un cv
write.csv(prueba_alternativas1,"alternativas_recorridos_df.csv")



#######################################################################
# Prueba de la funcion:
# ------------             ESTIMACION DE EXPOSICION FINAL V02     -------------   
# VARIABLES   
# ------------01. Key tom-tom
# CONTRASEÑAS VARIAS CUENTAS DE TOM TOM. 2500 Request por dia cada una
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_4
# ------------02. Modos de transporte
modo = c("Auto","Auto")#,"Pie")

# ------------03. Lista con Coordenadas de origen-Destino

lista_viaje <- data.frame(long =c(-68.789,-68.864),#,-68.789),
                         lat = c(-32.88204,-32.91051))#,-32.87568))

# ------------04. Grilla de contaminacion local. 
# Contiene las grillas de PM de la salida de CALPUFF
#directorio_grillas <- "D:/Josefina/Proyectos/salud/movilidad_7/grillas"
directorio_grillas <- "D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp"

# ------------05. Tipo de ruta seleccionada
seleccion <- c("Menos contaminada","Menos contaminada")#,"Menos contaminada")

## ------------07. Horas de Salida de los sitios de interes

horas_interes<- c("2018-08-05 07:50:00 -03","2018-08-05 11:10:00 -03")
var_interes<- "t2m"


# ------------07 Corremos la funcion global
05:37
prueba_3 <- exposicion_total(lista_viaje,tiempo_actividad, modo, concentraciones_grilla=directorio_grillas ,key,
                                 seleccion,salida_exp="plot",horario = horas_interes,calc_meteo =T,var_interes)
prueba_3 <- exposicion_total(lista_viaje,tiempo_actividad, modo, concentraciones_grilla=directorio_grillas ,key,
                             seleccion,salida_exp="plot",horario = horas_interes)

15:04
prueba_df <- exposicion_total(lista_viaje,tiempo_actividad, modo, concentraciones_grilla=directorio_grillas ,key,
                             seleccion,salida_exp="df",horario = horas_interes)
06:29
#Guardar en en un cv
write.csv(prueba_df,"D:/Josefina/Proyectos/salud/movilidad_7/ejemplos_varios/prueba_df_exposicion_V2.csv")

setwd("D:/Josefina/Proyectos/salud/movilidad_7/ejemplos_varios")

# ------------08 Guardamos el html en la carpeta
htmlwidgets::saveWidget(prueba_3, "prueba_plot3.html")
htmlwidgets::saveWidget(prueba_plot2, "prueba_plot6.html")

getwd()
setwd("D:/Josefina/Proyectos/salud/movilidad_7/")
