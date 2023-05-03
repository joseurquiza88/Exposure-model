
#######################################################################
# ------------             CHOICE OF THE BEST ROUTE    -------------     

#This code allows to know different route alternatives for
# different types of mobility.
# According to the fastest, shortest, most polluted, least polluted route
# according to TOM-TOM API


##
alternative_trajectories <- function(origin,dest,mode,concentrations_grid,
                               key,output,hour =NULL){
# ------------             Search of trajectories      ---------------- 
# Search for alternative according to tom-tom
  trajectories_tomtom <- function(origin,dest,mode,hour_trajectory=hour){
    df_rbind <- data.frame()
    num_alternative<-5
    resp_df_complete <- data.frame()
    #---  hour de departure
    if (is.null(hour_trajectory)){
      hour_trajectory <- Sys.time()
    }else{
      hour_trajectory <- hour_trajectory 
    }
    day<- substr (hour_trajectory,1,10)
    hour <- substr(hour_trajectory,12,13)
    minutes <- substr(hour_trajectory,15,16)
    hour_format <- paste(day,"T",hour,"%3A",minutes,"%3A00-03%3A00",sep = "")

    url_part_1 <- "https://api.tomtom.com/routing/1/calculateRoute/" 
    url_part_2 <- paste("/json?maxAlternatives=",num_alternative,"&departAt=",hour_format,"&routeRepresentation=polyline&computeTravelTimeFor=all&traffic=true&travelMode=",sep="" )
    
    
    #--- Types de trajectory - ingles-espa?ol

    mode_transp <- mode

    url_part_3="&vehicleEngineType=combustion&key="

    df_rbind <- data.frame()
    df_rbind_output <- data.frame()
    resp_df_complete <- data.frame()
    origin_lat  <- strsplit(origin, ",")[[1]][1]
    origin_long<- strsplit(origin, ",")[[1]][2]
    destination_lat  <- strsplit(dest, ",")[[1]][1]
    destination_long<- strsplit(dest, ",")[[1]][2]
    url<- paste0(url_part_1,origin_lat,"%2C",origin_long,"%3A",destination_lat,"%2C",destination_long,url_part_2,mode_transp,url_part_3,key)
    #---  Request en la API
    response <- GET(url)
    resp_json <- fromJSON(content(response, as = "text"))
    for (j in 1:length(resp_json[["routes"]][["legs"]])){
        resp<- data.frame( long = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["longitude"]],
                           lat = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["latitude"]],
                           # --- Arrival and departure time --
                           departureTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["departureTime"]],
                           arrivalTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["arrivalTime"]],
                           # --   Distance  ---
                           lengthInMeters = (resp_json[["routes"]][["legs"]][[j]][["summary"]][["lengthInMeters"]]/1000),
                           trafficLengthInMeters=resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficLengthInMeters"]],
                           travelMode=resp_json[["routes"]][["sections"]][[1]][["travelMode"]][1],
                           # --- Delay Time
                           trafficDelayInSeconds=resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficDelayInSeconds"]],
                           
                           # ---  Real Time with traffic ---
                           travelTimeInSeconds = round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["travelTimeInSeconds"]]/60),2),
                           liveTrafficIncidentsTravelTimeInSeconds=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["liveTrafficIncidentsTravelTimeInSeconds"]]/60),2),
                           # ---  Historic Traffic time  ---
                           historicTrafficTravelTimeInSeconds=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["historicTrafficTravelTimeInSeconds"]]/60),2),
                           #   ---  Time without traffic  ---
                           noTrafficTravelTimeInSeconds= round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["noTrafficTravelTimeInSeconds"]]/60),2),
                           alternative = paste("alternative_",j,sep=""))
    
        num_rows<-  nrow(resp)
        ID <- c(1:num_rows)
        data_frame_resp <- cbind(ID , resp)
        df_rbind <- rbind(data_frame_resp,df_rbind)  
        
      }
      
      df_rbind_output<- rbind(df_rbind,df_rbind_output)  
      names(df_rbind_output) <- c("ID" , "long","lat" ,"departureTime", 
                                  "arrivalTime", "lengthInMeters", 
                                  "trafficLengthInMeters","travelMode", 
                                  "trafficDelayInSeconds","travelTimeInSeconds" ,                   
                                  "liveTrafficIncidentsTravelTimeInSeconds",
                                   "historicTrafficTravelTimeInSeconds",
                                  "noTrafficTravelTimeInSeconds",           
                                   "alternative")
      return(df_rbind_output)
  }
  trajectory<- trajectories_tomtom(origin,dest,mode = mode, hour_trajectory=hour )

  # ------------             PASO DE PUNTOS A LINEAS      ----------------
  data_trajectory <- trajectory
  v_lines <- points_to_line(data = data_trajectory, 
                            long = "long", 
                            lat = "lat", 
                            id_field = "alternative",
                            sort_field = "ID")
  
  id_df <- data.frame()
  data_trajectory%>%
    group_by(alternative) %>%  
    group_split() -> dat_agrupado
  
  for (x in 1:length(v_lines@lines)){
    id <- v_lines@lines[[x]]@ID
    origin <- origin
    destination <- dest
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
    alternative<-dat_agrupado[[x]][["alternative"]][1]
    data_frame_1 <- data.frame(id , origin,destination ,departureTime, 
                                 arrivalTime, lengthInMeters, 
                                 trafficLengthInMeters,travelMode, 
                                 trafficDelayInSeconds,travelTimeInSeconds ,                   
                                 liveTrafficIncidentsTravelTimeInSeconds,
                                 historicTrafficTravelTimeInSeconds,
                                 noTrafficTravelTimeInSeconds,           
                                 alternative)
    names (data_frame_1)<- c("id" , "origin","destination" ,"departureTime", 
                             "arrivalTime", "lengthInMeters", 
                             "trafficLengthInMeters","travelMode", 
                             "trafficDelayInSeconds","travelTimeInSeconds",                   
                             "liveTrafficIncidentsTravelTimeInSeconds",
                             "historicTrafficTravelTimeInSeconds",
                             "noTrafficTravelTimeInSeconds",           
                             "alternative")
    
    id_df <- rbind(id_df,data_frame_1)
  }
  df2<-SpatialLinesDataFrame(v_lines, id_df , match.ID = F)
  proj4string(df2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # ------------                  ELECCION DE RUTA         ----------------
  #Guardamos la informacion en un .shp temporal
   writeOGR(df2,"./temp","temp", driver="ESRI Shapefile")
   df3 <- st_read("./temp/temp.shp",quiet = TRUE,crs = 4326)
  
     #---  Search of the grid of interest according to the hour entered
   grid<- temporary_grid_search(start_hour = df3$dprtrTm[1],end_hour=df3$arrvlTm[length(df3$arrvlTm)],dir_grid = concentrations_grid,time_format="%Y-%m-%dT%H:%M:%S")
   intersection_grid <- st_intersection(df3,grid)
   names(grid)<-c("GRI1_ID" ,"X_COORD","Y_COORD","dailyPM","len","geometry")
   file.remove(file.path("./temp", dir(path="./temp" ,pattern="temp.*")))
    
   intersection_grid%>%
      group_by(altrntv) %>% 
      group_split() -> dataSplit_intersection
   sum_df <- data.frame()
    # CALPUFF PM output value name is "value"
   for (i in 1:length(dataSplit_intersection)){
      origin <- dataSplit_intersection[[i]][["origin"]][1]
      destination <- dataSplit_intersection[[i]][["destination"]][1]
      alternative<- dataSplit_intersection[[i]][["altrntv"]][1]
      dailyPM <- round(mean(dataSplit_intersection[[i]][["value"]],na.rm=T),2)
      df <- data.frame(alternative,dailyPM)
      names(df) <- c("alternative","dailyPM")
      sum_df<- rbind(sum_df,df)
      names(sum_df) <-c("alternative","dailyPM")
    }
    df_merge <- merge(trajectory,sum_df, #
                      by = "alternative")
                        
    trajectory<- df_merge
    trajectory$exposure <- round(((trajectory$dailyPM * trajectory$liveTrafficIncidentsTravelTimeInSeconds)/60),2)
    # ------------ 01. FASTER ROUTE
    #Tiempo real con trafico
    faster_route <- trajectory[trajectory$liveTrafficIncidentsTravelTimeInSeconds == min(trajectory$travelTimeInSeconds),]
    faster_route$type <- "Faster route"
    # ------------ 02. SHORTER ROUTE
    shorter_route <- trajectory[trajectory$lengthInMeters == min(trajectory$lengthInMeters),]
    shorter_route$type <- "Shorter route"
    # ------------ 03. LESS CONTAMINATED ROUTE
    less_polluted_route<- trajectory[trajectory$dailyPM == min(trajectory$dailyPM),]
    less_polluted_route$type <- "Less polluted"
    # ------------ 04. MORE CONTAMINATED ROUTE
    more_polluted_route <- trajectory[trajectory$dailyPM == max(trajectory$dailyPM),]
    more_polluted_route$type <- "More polluted"
    # ------------ 0.5 MORE EXPOSURE
    more_exposure_route <- trajectory[trajectory$exposure == max(trajectory$exposure),]
    more_exposure_route$type <- "More exposure"
    
    # ------------ 0.7 LESS EXPOSURE
    less_exposure_route <- trajectory[trajectory$exposure == min(trajectory$exposure),]
    less_exposure_route$type <- "Less exposure"
    
    df_output <- data.frame()
    # ------- If you want a dataframe output with  trajectory exposure and origin-destination exposure
    if (output=="df"){
    df_output <- rbind(faster_route,shorter_route,more_polluted_route,less_polluted_route,more_exposure_route,less_exposure_route)
    return(df_output)
    }
  # ------------                  ROUTE PLOT        ----------------
  # Plot the different alternatives considered
    less_polluted_route_line<- points_to_line(data = less_polluted_route, 
                                long = "long", 
                                lat = "lat", 
                                id_field = NULL,
                                sort_field = "ID")
      more_polluted_route_line<- points_to_line(data = more_polluted_route, 
                                             long = "long", 
                                             lat = "lat", 
                                             id_field = NULL,
                                             sort_field = "ID")
    shorter_route_line<- points_to_line(data = shorter_route, 
                                        long = "long", 
                                        lat = "lat", 
                                        id_field = NULL,
                                        sort_field = "ID")
    faster_route_line<- points_to_line(data = faster_route, 
                                        long = "long", 
                                        lat = "lat", 
                                        id_field = NULL,
                                        sort_field = "ID")
    
    more_exposure_route_line<- points_to_line(data = more_exposure_route, 
                                      long = "long", 
                                      lat = "lat", 
                                      id_field = NULL,
                                      sort_field = "ID")
    less_exposure_route_line<- points_to_line(data = less_exposure_route, 
                                      long = "long", 
                                      lat = "lat", 
                                      id_field = NULL,
                                      sort_field = "ID")
    ##CRS
    proj4string(less_polluted_route_line) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    proj4string(more_polluted_route_line) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    proj4string(shorter_route_line) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    proj4string(faster_route_line) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    proj4string(more_exposure_route_line) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    proj4string(less_exposure_route_line) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
       

    # ------- output: map
    #  --- Title map
    if (output == "plot"){
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
      #  --- Title
     title <- tags$div(tag.map.title, 
                        HTML(paste("<center><b>travel alternative with mode: </b></center>",mode)))

    #  --- Contenido del plot
      content_more_cont <- paste(sep = "<br/>",
                                paste0("<center><b>More polluted route: </b></center>"),
                                paste0("<b>Duration: </b>", more_polluted_route$travelTimeInSeconds," min"),
                                paste0("<b>Distance: </b>", more_polluted_route$lengthInMeters," km"),
                                paste0("<b>Concentrations PM: </b>", more_polluted_route$dailyPM," ?g m-3"),
                                paste0("<b>PM Exposure: </b>", more_polluted_route$exposure," ?g m-3/h"))
      content_less_cont <- paste(sep = "<br/>",
                                paste0("<center><b>Less polluted route: </b></center>"),
                                paste0("<b>Duration: </b>", less_polluted_route$travelTimeInSeconds," min"),
                                paste0("<b>Distance: </b>", less_polluted_route$lengthInMeters," km"),
                                paste0("<b>PM Concentrations: </b>", less_polluted_route$dailyPM," ?g m-3"),
                                paste0("<b>PM exposure: </b>", less_polluted_route$exposure," ?g m-3/h"))
      
      content_short <- paste(sep = "<br/>",
                                paste0("<center><b>Shorter route: </b></center>"),
                                paste0("<b>Duration: </b>",shorter_route$travelTimeInSeconds," min"),
                                paste0("<b>Distance: </b>", shorter_route$lengthInMeters," km"),
                                paste0("<b>PM Concentrations: </b>", shorter_route$dailyPM," ?g m-3"),
                             paste0("<b>PM exposure: </b>", shorter_route$exposure," ?g m-3/h"))
      
      content_fast <- paste(sep = "<br/>",
                                paste0("<center><b>Faster route: </b></center>"),
                                paste0("<b>Duration: </b>", faster_route$travelTimeInSeconds," min"),
                                paste0("<b>Distance: </b>", faster_route$lengthInMeters," km"),
                                paste0("<b>PM Concentrations: </b>", faster_route$dailyPM," ?g m-3"),
                              paste0("<b>PM exposure: </b>", faster_route$exposure," ?g m-3/h"))

      content_less_exp<- paste(sep = "<br/>",
                              paste0("<center><b>Ruta Less exposure: </b></center>"),
                              paste0("<b>Duration: </b>", less_exposure_route$travelTimeInSeconds," min"),
                              paste0("<b>Distance: </b>", less_exposure_route$lengthInMeters," km"),
                              paste0("<b>PM Concentrations: </b>", less_exposure_route$dailyPM," ?g m-3"),
                              paste0("<b>PM exposure: </b>", less_exposure_route$exposure," ?g m-3/h"))
      
      
      content_more_exp <- paste(sep = "<br/>",
                              paste0("<center><b>More exposure route: </b></center>"),
                              paste0("<b>Duration: </b>", more_exposure_route$travelTimeInSeconds," min"),
                              paste0("<b>Distance: </b>", more_exposure_route$lengthInMeters," km"),
                              paste0("<b>PM Concentrations: </b>", more_exposure_route$dailyPM," ?g m-3"),
                              paste0("<b>PM exposure: </b>", more_exposure_route$exposure," ?g m-3/h"))
      
            
      #  --- Grid category
      grid$category = case_when(grid$dailyPM<=12.1 ~ 'Good',
                                    grid$dailyPM>12.1 & grid$dailyPM <= 35.4  ~ 'Moderate',
                                    grid$dailyPM >35.4 & grid$dailyPM <= 55.4  ~ 'Unhealthy for sensitive groups',
                                    grid$dailyPM > 55.4 & grid$dailyPM <= 150.4  ~ 'Unhealthy',
                                    grid$dailyPM > 150.4 & grid$dailyPM <= 250.4  ~ 'Very Unhealthy',
                                    grid$dailyPM > 250.4 ~ 'Hazardous' )
      
      #  --- Colorgrid
      palette_grid <- c("#abdda4","#f8fd66","#fdde61","#d74a4c","#b687ba","#590e63")
      palfac <- colorFactor(palette_grid, domain = grid$category)
      # ---  Plot
    
       map <- leaflet() %>%
        addTiles() %>%

        addAwesomeMarkers(
          
          lat = as.numeric(strsplit(origin, ",")[[1]][1]),
          lng = as.numeric(strsplit(origin, ",")[[1]][2]),
          label = "origin") %>%
          
        addAwesomeMarkers(
          
          lat = as.numeric(strsplit(dest, ",")[[1]][1]),
          lng = as.numeric(strsplit(dest, ",")[[1]][2]), 
          label = "destination") %>%
         
        addPolylines(data = faster_route_line,weight = 2,stroke = TRUE, color ="#FF0000FF",label = "Faster route",popup=content_fast,group="Faster route") %>%
        addPolylines(data = shorter_route_line,weight = 2,stroke = TRUE,color ="#ae017e",label = "Shorter route",popup=content_short,group="Shorter route") %>%
        addPolylines(data = more_polluted_route_line,weight = 2,stroke = TRUE, color ="#00FF66FF",label = "More polluted route",popup=content_more_cont,group="More polluted route")%>%
        addPolylines(data = less_polluted_route_line,weight = 2, color ="#08306b",label = "Less polluted route",popup=content_less_cont,group="Less polluted route")%>%
         addPolylines(data = less_exposure_route_line,weight = 2, color ="#016c59",label = "Less exposure route",popup=content_less_exp,group="Less exposure route")%>%
         addPolylines(data = more_exposure_route_line,weight = 2, color ="#cc4c02",label = "More exposure route",popup=content_more_exp,group="More exposure route")%>%
         
         addPolygons(data = grid,color = "#636363" ,
                     group = "Concentrations",
                     weight = 2,
                     smoothFactor = 0.1,
                     opacity = 0.1,
                     fillOpacity = 0.5,
                     fillColor = ~palfac(grid$category)
         )%>%
         addTiles() %>%
         addControl(title, position = "topleft", className="map-title")%>%
         addLegend(data = grid,position = "bottomleft", pal = palfac, values = ~grid$category, 
                   title = "PM2.5 Concentrations (μg m-3)")%>%
         # Layers control
       addLayersControl(
         overlayGroups = c("Concentrations","Less polluted route", "More polluted route", "Shorter route","Faster route", "Less exposure route","More exposure route"))#,

       alternative_map <- map
       return(alternative_map)
  }
  #################################################################################
  # ------- output POLYLINE
       if (output == "polyline"){
         df_output <- rbind(faster_route,shorter_route,more_polluted_route,less_polluted_route)
         
         polyline_output<- points_to_line(data = df_output, 
                                          long = "long", 
                                          lat = "lat", 
                                          id_field = "type",
                                          sort_field = "ID")
         
         id_df_output <- data.frame()
         df_output%>%
           group_by(type) %>%  
           group_split() -> group_dat_output
         
         for (p in 1:length(polyline_output@lines)){
           id <- polyline_output@lines[[p]]@ID
           origin <- origin
           destination <- dest
           
           departureTime <- group_dat_output[[p]][["departureTime"]][1]
           arrivalTime<- group_dat_output[[p]][["arrivalTime"]][1]
           lengthInMeters<- group_dat_output[[p]][["lengthInMeters"]][1]
           trafficLengthInMeters <- group_dat_output[[p]][["trafficLengthInMeters"]][1]
           travelMode <- group_dat_output[[p]][["travelMode"]][1]
           trafficDelayInSeconds<-  group_dat_output[[p]][["trafficDelayInSeconds"]][1]
           travelTimeInSeconds<- group_dat_output[[p]][["travelTimeInSeconds"]][1]
           liveTrafficIncidentsTravelTimeInSeconds<- group_dat_output[[p]][["liveTrafficIncidentsTravelTimeInSeconds"]][1]
           historicTrafficTravelTimeInSeconds <- group_dat_output[[p]][["historicTrafficTravelTimeInSeconds"]][1]
         
           noTrafficTravelTimeInSeconds<- group_dat_output[[p]][["noTrafficTravelTimeInSeconds"]][1]
           alternative<-group_dat_output[[p]][["alternative"]][1]
           type <- group_dat_output[[p]][["type"]][1]
           dailyPM<-group_dat_output[[p]][["dailyPM"]][1]

           data_frame_output <- data.frame(id , origin,destination ,departureTime, 
                                      arrivalTime, lengthInMeters, 
                                      trafficLengthInMeters,travelMode, 
                                      trafficDelayInSeconds,travelTimeInSeconds ,                   
                                      liveTrafficIncidentsTravelTimeInSeconds,
                                      historicTrafficTravelTimeInSeconds,
                                      noTrafficTravelTimeInSeconds,           
                                      alternative, type, dailyPM)
           names (data_frame_output)<- c("id" , "origin","destination" ,"departureTime", 
                                    "arrivalTime", "lengthInMeters", 
                                    "trafficLengthInMeters","travelMode", 
                                    "trafficDelayInSeconds","travelTimeInSeconds",                   
                                    "liveTrafficIncidentsTravelTimeInSeconds",
                                    "historicTrafficTravelTimeInSeconds",
                                    "noTrafficTravelTimeInSeconds",           
                                    "alternative","type","dailyPM")
           
           id_df_output <- rbind(id_df_output,data_frame_output)
         }
         df2_output<-SpatialLinesDataFrame(polyline_output, id_df_output , match.ID = F)
         proj4string(df2_output) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
       } 
    return(df2_output)
}

########## ------ Examples

origin <-"-32.79679,-68.816" # lat- long
dest <- "-32.90212,-68.761" # lat- long
origin <-"-32.84890,-68.81760" # lat- long
destination <- "-32.84890,-68.38362" # lat- long
key = #xxxxxx

mode <- "car"
concentrations_grid="D:/Josefina/Proyectos/CALPUFF/Resultados/V10/temp/"
hour <- "2019-08-01 07:50:00 -03"

#
request_df2<-alternative_trajectories (origin,dest,mode,concentrations_grid,
                                    key,output="df",hour)
  
request_plot<-alternative_trajectories (origin,dest,mode,concentrations_grid,
                                       key,output="plot",hour)
request_polyline<-alternative_trajectories (origin,dest,mode,concentrations_grid,
                                           key,output="polyline",hour)
# ------------ Save examples
# Path local
setwd("D:/Josefina/Proyectos/salud/movilidad_7/ejemplos_varios")
# output DF
write.csv(request_df,"./func_alternative_trajectories_DF.csv")
# output plot
htmlwidgets::saveWidget(request_plot, "func_alternative_trajectories_PLOT.html")

# output polyline
writeOGR(request_polyline,".","func_alternative_trajectories_POLYLINE_2", driver="ESRI Shapefile")

# output puntos request tom-tom
prueba_trajectory_tomtom<-trajectories_tomtom(origin,dest,mode,hour_trajectory=hour)
write.csv(prueba_trajectory_tomtom,"./prueba_trajectory_tomtom.csv")
