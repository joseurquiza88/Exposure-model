#######################################################################
# ------------        ESTIMACION DE LA EXPOSICION TOTAL     -------------     
# ------------                    VERSION 04 -              -------------     
# CAMBIOS: cambian las variables de ingreso. Se ingresa la hora de salida 
# del hogar por primera vez y luego los tiempos de cada actividad.
# de esta forma la hora en origen es = a hora de arrivo + minutos actividad

#Este codigo permite estimar la exposicion total diaria
# La idea es ingresar una lista () de coordenadas con los puntos de O-D
# El ultimo viaje siempre sera el hogar, es decir el primer punto ingresado
#

exposicion_total <- function (lista_viaje, modo, concentraciones_grilla,key,seleccion,salida_exp,
                              horario_salida_hogar, minutosActividades){
  df_salida <- data.frame()
  rbind_df_1 <- data.frame()

  rbind_ruta_selec <- data.frame()
  # ------- Calculo para mas de un viaje diario
  for (i in 1:length(lista_viaje$long)){
    print(paste("Punto",i,sep = " "))
    #si es 1 es el valor de salida
      if(i==1){
        # Si i=1 considerar el horario de salida ingresado en la functin
    origen_coords <- paste((lista_viaje[i,2]),(lista_viaje[i,1]),sep = ",")
    destino_coords <- paste((lista_viaje[i+1,2]),(lista_viaje[i+1,1]),sep = ",")
    ruta_seleccionada <-  alternativas_recorridos (origen=origen_coords,destino=destino_coords,modo=modo[i], concentraciones_grilla,key=key_1,salida = "df",horario = horario_salida_hogar)
        
    }
   else if (i == length(lista_viaje$long)){

     
     # Si las coordenadas corresponden al ultimo punto ingresado destino vuelve al punto 1 (hogar)
     
      origen_coords <- paste((lista_viaje[i,2]),(lista_viaje[i,1]),sep = ",")
      origen_coords_1 <- paste((lista_viaje[1,2]),(lista_viaje[1,1]),sep = ",")
      ruta_seleccionada <-  alternativas_recorridos (origen=origen_coords,destino=origen_coords_1,modo=modo[i], concentraciones_grilla,key=key_1,salida = "df",horario =hora_prox_salida)
      
    }else {


      origen_coords <- paste((lista_viaje[i,2]),(lista_viaje[i,1]),sep = ",")
      destino_coords <- paste((lista_viaje[i+1,2]),(lista_viaje[i+1,1]),sep = ",")
      ruta_seleccionada <-  alternativas_recorridos (origen=origen_coords,destino=destino_coords,modo=modo[i], concentraciones_grilla,key=key_1,salida = "df",horario = hora_prox_salida)
      
    }
    # --------Selecciono la alternativa elegida
    # porej. La ruta menos contaminada
    ruta_seleccionada <- ruta_seleccionada[ruta_seleccionada$tipo == seleccion[i],]
    ruta_seleccionada$i <- paste ("Ruta",i, sep = " ") 
    # ------- Datos destino
    datos_destino <-  ruta_seleccionada[ruta_seleccionada$ID == (length(ruta_seleccionada$ID)),]
    lat_destino <- datos_destino$lat
    long_destino <- datos_destino$long
   
    # ------- Horarios que usamos para tomar la grilla de Destino
   
  if(i==1){

    # Si es el primer viaje, hora de llegada + tiempo de activdad
    hora_llegada <-  as.POSIXct(strptime(ruta_seleccionada$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))
    hora_prox_salida<-as.POSIXct(strptime(ruta_seleccionada$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))+minutes(minutosActividades[i,])
    #hora_prox_salida  <-  as.POSIXct(strptime(horario[i+1], format = "%Y-%m-%d %H:%M:%S"))
    tiempo_destino <- as.numeric(difftime(hora_prox_salida,hora_llegada ,unit ="mins"))
    
  }
    
    #else if (i == length(minutosActividades)+1){
    else if (i == nrow(minutosActividades)+1){

    # Si es la ultima hora ingresada ==> hora ingresada - 23:59
    hora_prox_salida <-  as.POSIXct(strptime((paste(substr(horario_salida_hogar,1,10)," 23:59:59 -03",sep ="")), format = "%Y-%m-%d %H:%M:%S"))
    hora_llegada  <-  as.POSIXct(strptime(ruta_seleccionada$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))
    tiempo_destino <- as.numeric(difftime(hora_prox_salida,hora_llegada ,unit ="mins"))
    }else{

    # Sino ==> hora de arribo - hora + 1 
    hora_prox_salida<-as.POSIXct(strptime(ruta_seleccionada$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))+minutes(minutosActividades[i,])
    hora_llegada <-  as.POSIXct(strptime(ruta_seleccionada$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))
    tiempo_destino <- as.numeric(difftime(hora_prox_salida,hora_llegada ,unit ="mins"))
    
    }
    
    # ------- Datos trayecto
    tiempo_trayecto <- ruta_seleccionada$travelTimeInSeconds[1]
    distancia_trayecto <- ruta_seleccionada$lengthInMeters[1]
    conc_trayecto <- mean (ruta_seleccionada$PMDIARIO, na.rm = T) 
    # ------- Datos origen
    datos_origen <-  ruta_seleccionada[ruta_seleccionada$ID == 1,]
    lat_origen <- datos_origen$lat
    long_origen <- datos_origen$long
    
    # ------- Datos de concentraciones en Destino
    df_concentraciones <- rbind(datos_origen,datos_destino)
    coordinates(df_concentraciones) <- ~long+lat
    proj4string(df_concentraciones) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    writeOGR(df_concentraciones,"./temp","temp_punto", driver="ESRI Shapefile")
    punto <- st_read("./temp/temp_punto.shp",quiet = TRUE)
    # Funcion que busca las grillas de los horarios de interes
    grilla <- busqueda_grilla(hora_inicio=hora_llegada,hora_fin=hora_prox_salida ,directorio_grillas=concentraciones_grilla,formato_hora="%Y-%m-%d %H:%M:%S")
    names(grilla)<- c("GRI1_ID","X_COORD","Y_COORD" , "PMDIARIO" ,"len","geometry")
    interseccion_punto <- st_intersection(punto,grilla)
    conc_destino<- interseccion_punto[interseccion_punto$PMDIARIO == interseccion_punto$PMDIARIO[interseccion_punto$ID == max(interseccion_punto$ID)],]
    conc_destino <- conc_destino$PMDIARIO

        
    if (i==1){

      # ------- Datos de concentraciones de O-D puntos EN ORIGEN
      hora_inicial <- paste (substr(hora_llegada,1,10)," 00:00:01 -03",sep="")
      grilla_origen <- busqueda_grilla(hora_inicio=hora_inicial, hora_fin=horario_salida_hogar,directorio_grillas=concentraciones_grilla,formato_hora="%Y-%m-%d %H:%M:%S")
      names(grilla_origen )<- c("GRI1_ID","X_COORD","Y_COORD" , "PMDIARIO" ,"len","geometry")
      
      #punto <- st_read("./temp/temp_punto.shp",quiet = TRUE)
      #interseccion_punto_origen <- st_intersection(punto,grilla_origen)
      conc_origen<- interseccion_punto[interseccion_punto$PMDIARIO == interseccion_punto$PMDIARIO[interseccion_punto$ID == min(interseccion_punto$ID)],]
      
      #conc_origen<- interseccion_punto_origen[interseccion_punto_origen$PMDIARIO == interseccion_punto_origen$PMDIARIO[interseccion_punto_origen$ID == min(interseccion_punto_origen$ID)],]
      conc_origen <- conc_origen$PMDIARIO
      tiempo_origen <- as.numeric(difftime(horario_salida_hogar,hora_inicial ,unit ="mins"))
      
    }else{
      conc_origen <- NA
      tiempo_origen <- NA
      }
###
    file.remove(file.path("./temp", dir(path="./temp" ,pattern="temp_punto.*")))
    
    df_1 <- data.frame(lat_origen,long_origen, lat_destino, long_destino,
                       conc_origen, conc_trayecto, conc_destino,
                       tiempo_trayecto,tiempo_destino,tiempo_origen,i)
    rbind_df_1<- rbind(rbind_df_1, df_1)
    
    # ---- Si queremos un plot 
    rbind_ruta_selec<- rbind(rbind_ruta_selec,ruta_seleccionada)
    
    
  }
  # ------- Variables finales
   #Exposicion total trayecto
  rbind_df_1$exp_tot_trayecto <- rbind_df_1$conc_trayecto *  rbind_df_1$tiempo_trayecto
  rbind_df_1$exp_tot_destino <-   rbind_df_1$conc_destino *   rbind_df_1$tiempo_destino
  rbind_df_1$exp_tot_origen <-   rbind_df_1$conc_origen *   rbind_df_1$tiempo_origen
  
  # ------- Calculamos exposiciones por hora  mins
  exp_origen <- mean (rbind_df_1$exp_tot_origen, na.rm=T)
  exp_trayecto <- mean (rbind_df_1$exp_tot_trayecto, na.rm=T)
  exp_destino <- mean (rbind_df_1$exp_tot_destino, na.rm=T)
  exp_tot <- round((mean(c(exp_origen,exp_destino,exp_trayecto), na.rm=T)/60),2)
  rbind_df_1$exp_tot <- exp_tot
  
  
  # ------- Calculamos exposiciones por hora  mins VERSION 2
  exp_origen2 <- sum(rbind_df_1$exp_tot_origen, na.rm=T)
  exp_trayecto2 <- sum(rbind_df_1$exp_tot_trayecto, na.rm=T)
  exp_destino2 <- sum (rbind_df_1$exp_tot_destino, na.rm=T)
  exp_tot2 <- round((sum(c(exp_origen2,exp_destino2,exp_trayecto2), na.rm=T)/60),2)
  
  rbind_df_1$exp_tot_sum <- exp_tot2
  rbind_df_1$ruta <- paste("Ruta",rbind_df_1$i,sep=" " )
  rbind_df_1$modo <- modo
  tiempo_origen_function <- funcion_horas(sum(rbind_df_1$tiempo_destino[max(rbind_df_1$i)]+rbind_df_1$tiempo_origen[!is.na(rbind_df_1$tiempo_origen)]))
  tiempo_trayecto_function <- funcion_horas((sum(rbind_df_1$tiempo_trayecto)))
  tiempo_destino_function <- funcion_horas(sum(rbind_df_1$tiempo_destino[1:(length(rbind_df_1$tiempo_destino)-1)]))
  
  #  --- Transformacion de puntos a lineas
  ruta_line<- points_to_line(data = rbind_ruta_selec, 
                             long = "long", 
                             lat = "lat", 
                             id_field = "i",
                             #id_field = NULL,
                             sort_field = "ID")
  #  --- Caracteristicas en HTML del titulo
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 10px;
  }
"))
  #  --- Titulo del mapa
  # INGLES
  title <- tags$div(tag.map.title, HTML(paste(sep = "<br/>", 
                                              paste0("<center><b>Total daily exposure estimate</b></center>"),
                                              #paste0("<b>Exposicion: </b>",  exp_tot ," µg m-3/h"),
                                              paste0("<b>Exposure: </b>",  exp_tot2 ," µg m-3/h"),
                                              paste0("<b>Origin time: </b>",  tiempo_origen_function,"hs"),
                                              paste0("<b>Activities time: </b>",  tiempo_destino_function ," hs"),
                                              paste0("<b>Travel time: </b>",  tiempo_trayecto_function ," hs"))))
  #ESPAÑOL
  # title <- tags$div(tag.map.title, HTML(paste(sep = "<br/>", 
  #                                             paste0("<center><b>Estimacion de la exposicion total diaria </b></center>"),
  #                                             #paste0("<b>Exposicion: </b>",  exp_tot ," µg m-3/h"),
  #                                             paste0("<b>Exposicion: </b>",  exp_tot2 ," µg m-3/h"),
  #                                             paste0("<b>Tiempo en origen: </b>",  tiempo_origen_function,"hs"),
  #                                             paste0("<b>Tiempo en actividades: </b>",  tiempo_destino_function ," hs"),
  #                                             paste0("<b>Tiempo en viaje: </b>",  tiempo_trayecto_function ," hs"))))
  
  #  --- ID lista viaje
  num_rows<-  nrow(lista_viaje)
  id <- c(1:num_rows)
  lista_viaje<- cbind(id , lista_viaje)
  
  #  --- Categorias grilla ingles
  grilla$categorias = case_when(grilla$PMDIARIO<=12.1 ~ 'Good',
                                grilla$PMDIARIO>12.1 & grilla$PMDIARIO <= 35.4  ~ 'Moderate',
                                grilla$PMDIARIO >35.4 & grilla$PMDIARIO <= 55.4  ~ 'Unhealthy for sensible groups',
                                grilla$PMDIARIO > 55.4 & grilla$PMDIARIO <= 150.4  ~ 'Unhealthy',
                                grilla$PMDIARIO > 150.4 & grilla$PMDIARIO <= 250.4  ~ 'Very unhealthy',
                                grilla$PMDIARIO > 250.4 ~ 'Hazardous' )
  # grilla$categorias = case_when(grilla$PMDIARIO<=12.1 ~ 'Bueno',
  #                               grilla$PMDIARIO>12.1 & grilla$PMDIARIO <= 35.4  ~ 'Moderado',
  #                               grilla$PMDIARIO >35.4 & grilla$PMDIARIO <= 55.4  ~ 'Insalubre para personas sensibles',
  #                               grilla$PMDIARIO > 55.4 & grilla$PMDIARIO <= 150.4  ~ 'Insalubre',
  #                               grilla$PMDIARIO > 150.4 & grilla$PMDIARIO <= 250.4  ~ 'Muy Insalubre',
  #                               grilla$PMDIARIO > 250.4 ~ 'Peligroso' )
  
  #  --- Colores grilla
  paleta_grilla <- c("#abdda4","#f8fd66","#fdde61","#d74a4c","#b687ba","#590e63")
  paleta_ruta <- c("#023858","#49006a","#00441b","#e7298a","#feb24c","#3690c0","#016c59","#8c510a","#f03b20")
  palfac <- colorFactor(paleta_grilla, domain = grilla$categorias)
  pal <- colorFactor(paleta_ruta, domain = rbind_df_1$i)
  # ---  Plot 
  mapa <- leaflet() %>%
    addTiles() %>%
    
    addAwesomeMarkers(lista_viaje,lng=lista_viaje$long,lat =lista_viaje$lat,label = paste("Punto",lista_viaje$id,sep = " ")) %>%
    addPolylines(data = ruta_line,stroke = TRUE,opacity = 0.8,weight = c((rbind_df_1$i)+3), color = c(pal(rbind_df_1$i)),group = (as.character(rbind_df_1$ruta))) %>%
    addPolygons(data = grilla,color = "#636363" ,
                group = "Concentraciones",
                weight = 2,
                smoothFactor = 0.1,
                opacity = 0.1,
                fillOpacity = 0.5,
                fillColor = ~palfac(grilla$categorias))%>%
    addTiles() %>%
    addControl(title, position = "topleft", className="map-title")%>%
    addLegend(data = grilla,position = "bottomleft", pal = palfac, values = ~grilla$categorias, 
              title = "US AQI Level - PM2.5 (µg m-3)")%>%
    #title = "Concentraciones PM2.5 (µg m-3)")%>%
    # Layers control
    addLayersControl(
      overlayGroups = c("Concentraciones",c(rbind_df_1$ruta)))#,"Ruta menos contaminada", "Ruta mas contaminada", "Ruta mas corta","Ruta mas rapida"))#,
  
  if (salida_exp == "df"){
    return(rbind_df_1)
  }
  if (salida_exp == "plot"){
    return(mapa)
    
  } 
  if (salida_exp == "polyline"){
    return( ruta_line)
    
  } 
}



