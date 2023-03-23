
#######################################################################
# ------------             ELECCION DE LA MEJOR RUTA     -------------     

#Este codigoo permite conocer diferentes alternativas de rutas para
# diferentes tipos de movilidades. 
# Segun ruta mas rapida, mas corta, mas contaminada, menos contaminada
# segun TOM-TOM 


##
alternativas_recorridos <- function(origen,destino,modo,concentraciones_grilla="D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp",#"D:/Josefina/Proyectos/salud/movilidad_7/grillas",
                                    key,salida,horario =NULL){
# ------------             BUSQUEDA DE RECORRIDOS      ---------------- 
# Busqueda de alternativas segun tom-tom
  recorridos_tomtom <- function(origen,destino,modo,horario_recorrido=horario){
    df_rbind <- data.frame()
    num_alternativas<-5
    resp_df_competo <- data.frame()
    #---  Horario de departure
    if (is.null(horario_recorrido)){
      horario_recorrido <- Sys.time()
    }else{
      horario_recorrido <- horario_recorrido 
    }
    dia<- substr (horario_recorrido,1,10)
    hora <- substr(horario_recorrido,12,13)
    minutos <- substr(horario_recorrido,15,16)
    horario_format <- paste(dia,"T",hora,"%3A",minutos,"%3A00-03%3A00",sep = "")

    url_part_1 <- "https://api.tomtom.com/routing/1/calculateRoute/" 
    url_part_2 <- paste("/json?maxAlternatives=",num_alternativas,"&departAt=",horario_format,"&routeRepresentation=polyline&computeTravelTimeFor=all&traffic=true&travelMode=",sep="" )
    #url_part_2 <- paste("/json?maxAlternatives=",num_alternativas,"&departAt=",horario_format,"&routeRepresentation=polyline&computeTravelTimeFor=all&routeType=fastest&traffic=true&travelMode=",sep="" )
    #--- Tipos de recorrido - ingles-español
    if (modo =="Camion"){
      mode_transp <- "truck"
    }
    if (modo=="Colectivo"){
      mode_transp <- "bus"
    }
    if (modo=="Bicicleta"){
      mode_transp  <- "bicycle"
    }
    if (modo=="Motocicleta"){
      mode_transp  <- "motocycle"
    }
    
    if (modo=="Pie"){
      mode_transp  <- "pedestrian"
    }
    if (modo=="Auto"){
      mode_transp  <- "car"
    }
    
    url_part_3="&vehicleEngineType=combustion&key="
    
    df_rbind <- data.frame()
    df_rbind_salida <- data.frame()
    resp_df_competo <- data.frame()
    origin_lat  <- strsplit(origen, ",")[[1]][1]
    origin_long<- strsplit(origen, ",")[[1]][2]
    destination_lat  <- strsplit(destino, ",")[[1]][1]
    destination_long<- strsplit(destino, ",")[[1]][2]
    url<- paste0(url_part_1,origin_lat,"%2C",origin_long,"%3A",destination_lat,"%2C",destination_long,url_part_2,mode_transp,url_part_3,key)
    #---  Request en la API
    response <- GET(url)
    resp_json <- fromJSON(content(response, as = "text"))
    for (j in 1:length(resp_json[["routes"]][["legs"]])){
        resp<- data.frame( long = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["longitude"]],
                           lat = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["latitude"]],
                           # --- Tiempo de salida y llegada --
                           departureTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["departureTime"]],
                           arrivalTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["arrivalTime"]],
                           # --   Distancias  ---
                           lengthInMeters = (resp_json[["routes"]][["legs"]][[j]][["summary"]][["lengthInMeters"]]/1000),
                           trafficLengthInMeters=resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficLengthInMeters"]],
                           travelMode=resp_json[["routes"]][["sections"]][[1]][["travelMode"]][1],
                           # --- Tiempo de demora
                           trafficDelayInSeconds=resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficDelayInSeconds"]],
                           
                           # ---  Tiempo real con trafico ---
                           travelTimeInSeconds = round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["travelTimeInSeconds"]]/60),2),
                           liveTrafficIncidentsTravelTimeInSeconds=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["liveTrafficIncidentsTravelTimeInSeconds"]]/60),2),
                           # ---  Tiempo historico  ---
                           historicTrafficTravelTimeInSeconds=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["historicTrafficTravelTimeInSeconds"]]/60),2),
                           #   ---  Tiempo sin trafico  ---
                           noTrafficTravelTimeInSeconds= round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["noTrafficTravelTimeInSeconds"]]/60),2),
                           alternativa = paste("alternativa_",j,sep=""))
    
        num_rows<-  nrow(resp)
        ID <- c(1:num_rows)
        data_frame_resp <- cbind(ID , resp)
        df_rbind <- rbind(data_frame_resp,df_rbind)  
        
      }
      
      df_rbind_salida<- rbind(df_rbind,df_rbind_salida)  
      names(df_rbind_salida) <- c("ID" , "long","lat" ,"departureTime", 
                                  "arrivalTime", "lengthInMeters", 
                                  "trafficLengthInMeters","travelMode", 
                                  "trafficDelayInSeconds","travelTimeInSeconds" ,                   
                                  "liveTrafficIncidentsTravelTimeInSeconds",
                                   "historicTrafficTravelTimeInSeconds",
                                  "noTrafficTravelTimeInSeconds",           
                                   "alternativa")
      return(df_rbind_salida)
  }
  recorrido<- recorridos_tomtom(origen,destino,modo = modo, horario_recorrido=horario )

  # ------------             PASO DE PUNTOS A LINEAS      ----------------
  datos <- recorrido
  v_lines <- points_to_line(data = datos, 
                            long = "long", 
                            lat = "lat", 
                            id_field = "alternativa",
                            sort_field = "ID")
  
  id_df <- data.frame()
  datos%>%
    group_by(alternativa) %>%  
    group_split() -> dat_agrupado
  
  for (x in 1:length(v_lines@lines)){
    id <- v_lines@lines[[x]]@ID
    origen <- origen
    destino <- destino
    departureTime <- dat_agrupado[[x]][["departureTime"]][1]
    arrivalTime<- dat_agrupado[[x]][["arrivalTime"]][1]
    lengthInMeters<- dat_agrupado[[x]][["lengthInMeters"]][1]
    trafficLengthInMeters <- dat_agrupado[[x]][["trafficLengthInMeters"]][1]
    travelMode <- dat_agrupado[[x]][["travelMode"]][1]
    trafficDelayInSeconds<-  dat_agrupado[[x]][["trafficDelayInSeconds"]][1]
    travelTimeInSeconds<- dat_agrupado[[x]][["travelTimeInSeconds"]][1]
    liveTrafficIncidentsTravelTimeInSeconds<- dat_agrupado[[x]][["liveTrafficIncidentsTravelTimeInSeconds"]][1]
    historicTrafficTravelTimeInSeconds <- dat_agrupado[[x]][["historicTrafficTravelTimeInSeconds"]][1]
    noTrafficTravelTimeInSeconds<- dat_agrupado[[x]][["noTrafficTravelTimeInSeconds"]][1]
    alternativa<-dat_agrupado[[x]][["alternativa"]][1]
    data_frame_1 <- data.frame(id , origen,destino ,departureTime, 
                                 arrivalTime, lengthInMeters, 
                                 trafficLengthInMeters,travelMode, 
                                 trafficDelayInSeconds,travelTimeInSeconds ,                   
                                 liveTrafficIncidentsTravelTimeInSeconds,
                                 historicTrafficTravelTimeInSeconds,
                                 noTrafficTravelTimeInSeconds,           
                                 alternativa)
    names (data_frame_1)<- c("id" , "origen","destino" ,"departureTime", 
                             "arrivalTime", "lengthInMeters", 
                             "trafficLengthInMeters","travelMode", 
                             "trafficDelayInSeconds","travelTimeInSeconds",                   
                             "liveTrafficIncidentsTravelTimeInSeconds",
                             "historicTrafficTravelTimeInSeconds",
                             "noTrafficTravelTimeInSeconds",           
                             "alternativa")
    
    id_df <- rbind(id_df,data_frame_1)
  }
  df2<-SpatialLinesDataFrame(v_lines, id_df , match.ID = F)
  proj4string(df2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # ------------                  ELECCION DE RUTA         ----------------
  #Guardamos la informacion en un .shp temporal
   writeOGR(df2,"./temp","temp", driver="ESRI Shapefile")
   df3 <- st_read("./temp/temp.shp",quiet = TRUE)
    #---  Busqueda de la grilla de interes segun el horario ingresado
    # Datos de CALPUFF guardados localmente para I == 2 ES VALUUEEE
    grilla<- busqueda_grilla(hora_inicio = df3$dprtrTm[1],hora_fin=df3$arrvlTm[length(df3$arrvlTm)],directorio_grillas = concentraciones_grilla,formato_hora="%Y-%m-%dT%H:%M:%S")
    interseccion_grilla <- st_intersection(df3,grilla)

    #file.remove(file.path("./temp", dir(path="./temp" ,pattern="temp.*")))
    file.remove(file.path("./temp", dir(path="./temp" ,pattern="temp.*")))
    
    interseccion_grilla%>%
      group_by(altrntv) %>% 
      group_split() -> dataSplit_interseccion
    suma_df <- data.frame()
    for (i in 1:length(dataSplit_interseccion)){
      origen <- dataSplit_interseccion[[i]][["origen"]][1]
      destino <- dataSplit_interseccion[[i]][["destino"]][1]
      alternativa<- dataSplit_interseccion[[i]][["altrntv"]][1]
      #dprtrTm<- dataSplit_interseccion[[i]][["dprtrTm"]][1]
      #arrvlTm<-dataSplit_interseccion[[i]][["arrvlTm"]][1]#[length(dataSplit_interseccion[[i]])]
      PMDIARIO <- round(mean(dataSplit_interseccion[[i]][["value"]],na.rm=T),2)
      
      #PMDIARIO <- round(mean(dataSplit_interseccion[[i]][["PMDIARIO"]],na.rm=T),2)
      #PMHORARIO <- round(mean(dataSplit_interseccion[[i]][["PMHORARIO"]],na.rm=T),2)

      df <- data.frame(alternativa,PMDIARIO)#,dprtrTm,arrvlTm)#,PMHORARIO
      names(df) <- c("alternativa","PMDIARIO")#,"dprtrTm","arrvlTm")#"PMHORARIO"
      suma_df<- rbind(suma_df,df)
      names(suma_df) <-c("alternativa","PMDIARIO")#,"dprtrTm","arrvlTm")#"PMHORARIO"
    }
    df_merge <- merge(recorrido,suma_df, #
                      by = "alternativa")
                        
    recorrido<- df_merge
    recorrido$exposicion <- round(((recorrido$PMDIARIO * recorrido$liveTrafficIncidentsTravelTimeInSeconds)/60),2)
    # ------------ 01. RUTA MAS RAPIDA
    #Tiempo real con trafico
    ruta_rapida <- recorrido[recorrido$liveTrafficIncidentsTravelTimeInSeconds == min(recorrido$travelTimeInSeconds),]
    ruta_rapida$tipo <- "Mas rapida"
    # ------------ 02. RUTA MAS CORTA
    ruta_corta <- recorrido[recorrido$lengthInMeters == min(recorrido$lengthInMeters),]
    ruta_corta$tipo <- "Mas corta"
    # ------------ 03. RUTA MENOS CONTAMINADA
    ruta_menos_contaminada <- recorrido[recorrido$PMDIARIO == min(recorrido$PMDIARIO),]
    ruta_menos_contaminada$tipo <- "Menos contaminada"
    # ------------ 04. RUTA MAS CONTAMINADA
    ruta_mas_contaminada <- recorrido[recorrido$PMDIARIO == max(recorrido$PMDIARIO),]
    ruta_mas_contaminada$tipo <- "Mas contaminada"
    # ------------ 0.4 Mas exposicion
    ruta_mas_exposicion <- recorrido[recorrido$exposicion == max(recorrido$exposicion),]
    ruta_mas_exposicion$tipo <- "Mas exposicion"
    
    # ------------ 0.5 Menos exposicion
    ruta_menos_exposicion <- recorrido[recorrido$exposicion == min(recorrido$exposicion),]
    ruta_menos_exposicion$tipo <- "Menos exposicion"
    
    # ------------ 0.6 Balanceada - tiempo - contaminacion
    #ruta_balanceada <- recorrido[(recorrido$PMDIARIO == min(recorrido$PMDIARIO)) && (recorrido$liveTrafficIncidentsTravelTimeInSeconds == min(recorrido$travelTimeInSeconds)) ,]
    #ruta_balanceada$tipo <- "Balanceada"
    # ------------ 0.7 Mas emisiones - Menos emisiones
  # Aparecen 2 warnings no darle importancia!
    df_salida <- data.frame()
    # ------- SALIDA DF
    if (salida=="df"){
    df_salida <- rbind(ruta_rapida,ruta_corta,ruta_mas_contaminada,ruta_menos_contaminada,ruta_mas_exposicion,ruta_menos_exposicion)
    return(df_salida)
    }
  # ------------                  PLOTEO DE RUTA        ----------------
  # Ploteamos las distintas alternativas consideradas
    ruta_menos_contaminada_line<- points_to_line(data = ruta_menos_contaminada, 
                                long = "long", 
                                lat = "lat", 
                                id_field = NULL,
                                sort_field = "ID")
    ruta_mas_contaminada_line<- points_to_line(data = ruta_mas_contaminada, 
                                             long = "long", 
                                             lat = "lat", 
                                             id_field = NULL,
                                             sort_field = "ID")
    ruta_corta_line<- points_to_line(data = ruta_corta, 
                                        long = "long", 
                                        lat = "lat", 
                                        id_field = NULL,
                                        sort_field = "ID")
    ruta_rapida_line<- points_to_line(data = ruta_rapida, 
                                        long = "long", 
                                        lat = "lat", 
                                        id_field = NULL,
                                        sort_field = "ID")
    
    ruta_mas_exposicion_line<- points_to_line(data = ruta_mas_exposicion, 
                                      long = "long", 
                                      lat = "lat", 
                                      id_field = NULL,
                                      sort_field = "ID")
    ruta_menos_exposicion_line<- points_to_line(data = ruta_menos_exposicion, 
                                      long = "long", 
                                      lat = "lat", 
                                      id_field = NULL,
                                      sort_field = "ID")

    # ------- SALIDA MAPA - PLOT
    #  --- Titulo del mapa
    if (salida == "plot"){
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
    font-size: 18px;
  }
"))
      #  --- Titulo
     title <- tags$div(tag.map.title, #HTML("Alternativas de viaje con el modo:") ) 
                        HTML(paste("<center><b>Alternativas de viaje con el modo: </b></center>",modo)))

    #  --- Contenido del plot
      content_mas_cont <- paste(sep = "<br/>",
                                paste0("<center><b>Ruta mas contaminada: </b></center>"),
                                paste0("<b>Duracion: </b>", ruta_mas_contaminada$travelTimeInSeconds," min"),
                                paste0("<b>Distancia: </b>", ruta_mas_contaminada$lengthInMeters," km"),
                                paste0("<b>Concentraciones PM: </b>", ruta_mas_contaminada$PMDIARIO," µg m-3"),
                                paste0("<b>Exposicion PM: </b>", ruta_mas_contaminada$exposicion," µg m-3/h"))
      content_menos_cont <- paste(sep = "<br/>",
                                paste0("<center><b>Ruta menos contaminada: </b></center>"),
                                paste0("<b>Duracion: </b>", ruta_menos_contaminada$travelTimeInSeconds," min"),
                                paste0("<b>Distancia: </b>", ruta_menos_contaminada$lengthInMeters," km"),
                                paste0("<b>Concentraciones PM: </b>", ruta_menos_contaminada$PMDIARIO," µg m-3"),
                                paste0("<b>Exposicion PM: </b>", ruta_menos_contaminada$exposicion," µg m-3/h"))
      
      content_corta <- paste(sep = "<br/>",
                                paste0("<center><b>Ruta mas contaminada: </b></center>"),
                                paste0("<b>Duracion: </b>",ruta_corta$travelTimeInSeconds," min"),
                                paste0("<b>Distancia: </b>", ruta_corta$lengthInMeters," km"),
                                paste0("<b>Concentraciones PM: </b>", ruta_corta$PMDIARIO," µg m-3"),
                             paste0("<b>Exposicion PM: </b>", ruta_corta$exposicion," µg m-3/h"))
      
      content_rapida <- paste(sep = "<br/>",
                                paste0("<center><b>Ruta mas rapida: </b></center>"),
                                paste0("<b>Duracion: </b>", ruta_rapida$travelTimeInSeconds," min"),
                                paste0("<b>Distancia: </b>", ruta_rapida$lengthInMeters," km"),
                                paste0("<b>Concentraciones PM: </b>", ruta_rapida$PMDIARIO," µg m-3"),
                              paste0("<b>Exposicion PM: </b>", ruta_rapida$exposicion," µg m-3/h"))

      content_menos_exp<- paste(sep = "<br/>",
                              paste0("<center><b>Ruta menos exposicion: </b></center>"),
                              paste0("<b>Duracion: </b>", ruta_menos_exposicion$travelTimeInSeconds," min"),
                              paste0("<b>Distancia: </b>", ruta_menos_exposicion$lengthInMeters," km"),
                              paste0("<b>Concentraciones PM: </b>", ruta_menos_exposicion$PMDIARIO," µg m-3"),
                              paste0("<b>Exposicion PM: </b>", ruta_menos_exposicion$exposicion," µg m-3/h"))
      
      
      content_mas_exp <- paste(sep = "<br/>",
                              paste0("<center><b>Ruta mas rapida: </b></center>"),
                              paste0("<b>Duracion: </b>", ruta_mas_exposicion$travelTimeInSeconds," min"),
                              paste0("<b>Distancia: </b>", ruta_mas_exposicion$lengthInMeters," km"),
                              paste0("<b>Concentraciones PM: </b>", ruta_mas_exposicion$PMDIARIO," µg m-3"),
                              paste0("<b>Exposicion PM: </b>", ruta_mas_exposicion$exposicion," µg m-3/h"))
      
            
      #  --- Categorias grilla
      grilla$categorias = case_when(grilla$PMDIARIO<=12.1 ~ 'Bueno',
                                    grilla$PMDIARIO>12.1 & grilla$PMDIARIO <= 35.4  ~ 'Moderado',
                                    grilla$PMDIARIO >35.4 & grilla$PMDIARIO <= 55.4  ~ 'Insalubre para personas sensibles',
                                    grilla$PMDIARIO > 55.4 & grilla$PMDIARIO <= 150.4  ~ 'Insalubre',
                                    grilla$PMDIARIO > 150.4 & grilla$PMDIARIO <= 250.4  ~ 'Muy Insalubre',
                                    grilla$PMDIARIO > 250.4 ~ 'Peligroso' )
      
      #  --- Colores grilla
      paleta_grilla <- c("#abdda4","#f8fd66","#fdde61","#d74a4c","#b687ba","#590e63")
      palfac <- colorFactor(paleta_grilla, domain = grilla$categorias)
      # ---  Plot
       mapa <- leaflet() %>%
        addTiles() %>%
        addAwesomeMarkers(
          lat = as.numeric(strsplit(origen, ",")[[1]][1]),
          lng = as.numeric(strsplit(origen, ",")[[1]][2]),
          label = "Origen") %>%
        addAwesomeMarkers(
          lat = as.numeric(strsplit(destino, ",")[[1]][1]),
          lng = as.numeric(strsplit(destino, ",")[[1]][2]), 
          label = "Destino") %>%
        addPolylines(data = ruta_rapida_line,weight = 2,stroke = TRUE, color ="#FF0000FF",label = "Ruta mas rapida",popup=content_rapida,group="Ruta mas rapida") %>%
        addPolylines(data = ruta_corta_line,weight = 2,stroke = TRUE,color ="#ae017e",label = "Ruta mas corta",popup=content_corta,group="Ruta mas corta") %>%
        addPolylines(data = ruta_mas_contaminada_line,weight = 2,stroke = TRUE, color ="#00FF66FF",label = "Ruta mas contaminada",popup=content_mas_cont,group="Ruta mas contaminada")%>%
       addPolylines(data = ruta_menos_contaminada_line,weight = 2, color ="#08306b",label = "Ruta menos contaminada",popup=content_menos_cont,group="Ruta menos contaminada")%>%
         addPolylines(data = ruta_menos_exposicion_line,weight = 2, color ="#016c59",label = "Ruta menos exposicion",popup=content_menos_exp,group="Ruta menos exposicion")%>%
         addPolylines(data = ruta_mas_exposicion_line,weight = 2, color ="#cc4c02",label = "Ruta mas exposicion",popup=content_mas_exp,group="Ruta mas exposicion")%>%
         
         addPolygons(data = grilla,color = "#636363" ,
                     group = "Concentraciones",
                     weight = 2,
                     smoothFactor = 0.1,
                     opacity = 0.1,
                     fillOpacity = 0.5,
                     fillColor = ~palfac(grilla$categorias)
         )%>%
         addTiles() %>%
         addControl(title, position = "topleft", className="map-title")%>%
         addLegend(data = grilla,position = "bottomleft", pal = palfac, values = ~grilla$categorias, 
                   title = "Concentraciones PM2.5 (µg m-3)")%>%
         # Layers control
       addLayersControl(
         overlayGroups = c("Concentraciones","Ruta menos contaminada", "Ruta mas contaminada", "Ruta mas corta","Ruta mas rapida", "Ruta menos exposicion","Ruta mas exposicion"))#,

       mapa_alternativas <- mapa
       return(mapa_alternativas)
  }
  #################################################################################
  # ------- SALIDA POLYLINE
       if (salida == "polyline"){
         df_salida <- rbind(ruta_rapida,ruta_corta,ruta_mas_contaminada,ruta_menos_contaminada)
         
         polyline_salida<- points_to_line(data = df_salida, 
                                          long = "long", 
                                          lat = "lat", 
                                          id_field = "tipo",
                                          sort_field = "ID")
         
         id_df_salida <- data.frame()
         df_salida%>%
           group_by(tipo) %>%  
           group_split() -> dat_agrupado_salida
         
         for (p in 1:length(polyline_salida@lines)){
           id <- polyline_salida@lines[[p]]@ID
           origen <- origen
           destino <- destino
           
           departureTime <- dat_agrupado_salida[[p]][["departureTime"]][1]
           arrivalTime<- dat_agrupado_salida[[p]][["arrivalTime"]][1]
           lengthInMeters<- dat_agrupado_salida[[p]][["lengthInMeters"]][1]
           trafficLengthInMeters <- dat_agrupado_salida[[p]][["trafficLengthInMeters"]][1]
           travelMode <- dat_agrupado_salida[[p]][["travelMode"]][1]
           # El retraso en segundos en comparación con las condiciones de flujo libre según
           #la información de tráfico en tiempo real.
           trafficDelayInSeconds<-  dat_agrupado_salida[[p]][["trafficDelayInSeconds"]][1]
          #El tiempo de viaje estimado en segundos. Tenga en cuenta que incluso cuando traffic=false,
           #travelTimeInSeconds aún incluye el retraso debido al tráfico.
           travelTimeInSeconds<- dat_agrupado_salida[[p]][["travelTimeInSeconds"]][1]
           
           #El tiempo de viaje estimado en segundos calculado utilizando datos de 
           # velocidad en tiempo real.
           liveTrafficIncidentsTravelTimeInSeconds<- dat_agrupado_salida[[p]][["liveTrafficIncidentsTravelTimeInSeconds"]][1]
           
           #El tiempo de viaje estimado en segundos calculado utilizando datos de tráfico históricos dependientes del tiempo
            historicTrafficTravelTimeInSeconds <- dat_agrupado_salida[[p]][["historicTrafficTravelTimeInSeconds"]][1]
           
            # noTrafficTravelTimeInSeconds: El tiempo de viaje estimado en segundos calculado
           # como si no hubiera demoras en la ruta debido a las condiciones del tráfico 
           # (p. ej., congestión).
         
           noTrafficTravelTimeInSeconds<- dat_agrupado_salida[[p]][["noTrafficTravelTimeInSeconds"]][1]
           alternativa<-dat_agrupado_salida[[p]][["alternativa"]][1]
           tipo <- dat_agrupado_salida[[p]][["tipo"]][1]
           PMDIARIO<-dat_agrupado_salida[[p]][["PMDIARIO"]][1]
           # PMHORARIO<- dat_agrupado_salida[[p]][["PMHORARIO"]][1]
           #PMDIARIO<-dat_agrupado_salida[[p]][["value"]][1]

           data_frame_salida <- data.frame(id , origen,destino ,departureTime, 
                                      arrivalTime, lengthInMeters, 
                                      trafficLengthInMeters,travelMode, 
                                      trafficDelayInSeconds,travelTimeInSeconds ,                   
                                      liveTrafficIncidentsTravelTimeInSeconds,
                                      historicTrafficTravelTimeInSeconds,
                                      noTrafficTravelTimeInSeconds,           
                                      alternativa, tipo, PMDIARIO)#, PMHORARIO)
           names (data_frame_salida)<- c("id" , "origen","destino" ,"departureTime", 
                                    "arrivalTime", "lengthInMeters", 
                                    "trafficLengthInMeters","travelMode", 
                                    "trafficDelayInSeconds","travelTimeInSeconds",                   
                                    "liveTrafficIncidentsTravelTimeInSeconds",
                                    "historicTrafficTravelTimeInSeconds",
                                    "noTrafficTravelTimeInSeconds",           
                                    "alternativa","tipo","PMDIARIO")#, "PMHORARIO")
           
           id_df_salida <- rbind(id_df_salida,data_frame_salida)
         }
         df2_salida<-SpatialLinesDataFrame(polyline_salida, id_df_salida , match.ID = F)
         proj4string(df2_salida) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
       } 
    return(df2_salida)
}

########## ------ Ejemplos

origen <-"-32.79679,-68.816" # lat- long
destino <- "-32.90212,-68.761" # lat- long
origen <-"-32.84890,-68.81760" # lat- long
destino <- "-32.84890,-68.38362" # lat- long


modo <- "Auto"
concentraciones_grilla="D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp"
horario <- "2018-08-01 07:50:00 -03"

request_df2<-alternativas_recorridos (origen,destino,modo,concentraciones_grilla,
                                    key,salida="df",horario)
  
request_plot<-alternativas_recorridos (origen,destino,modo,concentraciones_grilla,
                                       key,salida="plot",horario)
request_polyline<-alternativas_recorridos (origen,destino,modo,concentraciones_grilla,
                                           key,salida="polyline",horario)
# ------------ Guardamos ejemplos
# Path local
setwd("D:/Josefina/Proyectos/salud/movilidad_7/ejemplos_varios")
# Salida DF
write.csv(request_df,"./func_alternativas_recorridos_DF.csv")
# Salida plot
htmlwidgets::saveWidget(request_plot, "func_alternativas_recorridos_PLOT.html")

# Salida polyline
writeOGR(request_polyline,".","func_alternativas_recorridos_POLYLINE_2", driver="ESRI Shapefile")

# Salida puntos request tom-tom
prueba_recorrido_tomtom<-recorridos_tomtom(origen,destino,modo,horario_recorrido=horario)
write.csv(prueba_recorrido_tomtom,"./prueba_recorrido_tomtom.csv")
