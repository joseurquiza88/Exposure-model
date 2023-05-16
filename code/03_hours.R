
#######################################################################
# ------------             SEARCH THE TIME GRID     ------------- 

#This function allows you to enter 2 different dates
# one at the beginning and the other at the end we obtain the grid of interest
# In the event that the times of the dates are different, for example:
# start_time <- "2023-01-19 06:50:00 -03" -  end_time <- "2023-01-19 09:50:00 -03"
# The function searches the grids available for that period (06,07,08,09)
# and a pixel-by-pixel average is generated.

#The output is a data.frame ready to temporarily save to .shp

temporary_grid_search <- function(start_hour, end_hour=NULL,dir_grids,time_format){  
  # --- Function that looks for the grid (.shp) corresponding to the hour of interest entered
  hourly_grid <- function(hour, time_format = time_format,dir_grids=dir_grids){
    input_hour <- as.POSIXct(strptime(hour, format = time_format))
    hour_exposure<- hour(input_hour)
    exposure_day <- date(input_hour)
    setwd(dir_grids)
  file_list <- dir(dir_grids,pattern = ".shp")
   
    table_files <-as.POSIXct(strptime( substr(file_list,1,15), format = "%Y-%m-%d_%H%M"))
    searched_date <- which((date(table_files)) == exposure_day)
    
    table_files <- table_files[searched_date] 
    searched_hour <- which((hour(table_files))== hour_exposure)
    
    file <- table_files[searched_hour] 
    name_file<- paste(substr(file,1,10),"_",substr(file,12,13),substr(file,15,16),".shp",sep = "")

    return(name_file)
  }
  trajectory_grid_rbind <- data.frame()
  only_start_hour <- hour(as.POSIXct(strptime(start_hour, format = time_format)))
  only_end_hour <- hour(as.POSIXct(strptime(end_hour, format = time_format)))
  
  #  --- 
  if (is.null(end_hour)){

    df_start_grids <- st_read(hourly_grid(start_hour, time_format = time_format,dir_grids),quiet = TRUE)
    df_start_grid<-st_transform(df_start_grids,crs = 4326)
    }
  #  --- When there is only one grid
  else if (only_start_hour == only_end_hour ){

    trajectory_grid <- st_read(hourly_grid(start_hour, time_format = time_format,dir_grids),quiet = TRUE)
    salida<-st_transform(trajectory_grid,crs = 4326)
  }else{
    # --- When there are several grids we do an average per pixel
    for(j in only_start_hour:only_end_hour){

      if (j < 10){
        j_hour <- paste("0",j,sep = "")
      }else{
        j_hour <- j
      }
      
      day <- paste(substr(start_hour,1,10),paste(j_hour,":00:00",sep = ""), "-03",sep = " ")

      trajectory_grid <- st_read(hourly_grid(day, time_format = "%Y-%m-%d %H:%M:%S",dir_grids),quiet = TRUE)
      trajectory_grid$hour <- day
      trajectory_grid_rbind <- rbind(trajectory_grid_rbind,trajectory_grid)
    }
    ## ------------ Group by the ID of the grid and make the mean of each pixel
    trajectory_grid_rbind %>%
      group_by(GRI1_ID) %>%  
      group_split() -> data_grilla
    
    df_grilla <- data.frame()
    for (p in 1:length(data_grilla)){

      GRI1_ID <- data_grilla[[p]][["GRI1_ID"]][1]
      X_COORD <- data_grilla[[p]][["x"]][1]
      Y_COORD <- data_grilla[[p]][["y"]][1]
      dailyPM<- mean(data_grilla[[p]][["value"]],na.rm = T)
      geometry <- data_grilla[[p]][["geometry"]][1]
      len <- length(data_grilla[[p]][["geometry"]])
      
      df <- data.frame(GRI1_ID,X_COORD,Y_COORD,dailyPM,geometry,len)
      
      names(df) <- c("GRI1_ID","X_COORD","Y_COORD","value","geometry","len")
      df_grilla <- rbind(df_grilla ,df)
      names(df_grilla) <- c("GRI1_ID","X_COORD","Y_COORD","value","geometry","len")
    }

    
    st_write(df_grilla,"./temp/temp_grid.shp",delete_layer = TRUE,quiet = TRUE)
    
    trajectory_grid<- st_read("./temp/temp_grid.shp",quiet = TRUE)
    salida<-st_transform(trajectory_grid,crs = 4326)
  }
  if(is.null(end_hour)){

    return(df_start_grid)
    }else{
      return(salida)
    }
}

#---- Examples
# With an only start hour
test_temporary_grid_search<-temporary_grid_search(start_hour="2018-08-01T07:50:00 -03",end_hour=NULL,dir_grids="D:/Josefina/paper_git/paper_exposure_model/grid_example",time_format="%Y-%m-%dT%H:%M:%S")


#With a different start and end time
test_temporary_grid_search_2<-temporary_grid_search(start_hour="2018-08-01 00:50:00 -03",end_hour="2018-08-01 02:50:00 -03",dir_grids="D:/Josefina/paper_git/paper_exposure_model/grid_example",time_format="%Y-%m-%d %H:%M:%S")

#With a the same start and end time
test_temporary_grid_search_3<-temporary_grid_search(start_hour="2018-08-01 00:10:00 -03",end_hour="2018-08-01 00:50:00 -03",dir_grids="D:/Josefina/paper_git/paper_exposure_model/grid_example",time_format="%Y-%m-%d %H:%M:%S")
