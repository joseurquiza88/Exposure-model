# Exposicion diaria
#Punto interes
point<- data.frame(lat = -32.88199,
                   long=-68.78944)
coordinates(point) <- ~long+lat
proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
punto <- st_as_sf(point, crs = 4326)
#Funcion grilla - hora 
prueba_busqueda_grilla_2<-busqueda_grilla(hora_inicio="2018-08-01 00:50:00 -03",hora_fin="2018-08-01 23:50:00 -03",directorio_grillas="D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp/",formato_hora="%Y-%m-%d %H:%M:%S")

interseccion_punto <- st_intersection(punto,grilla)

