
# ------- Function of transformation of points to lines 
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))

      
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

# ------- Function of transformation of minutes to hours
function_hours <- function(minutes){
  minutes <- round(minutes)
  hs_tot <- (minutes/60)
  integer <- floor(hs_tot)
  mins <- round((decimal*60/1),1)
  if (entero<=9){
    integer_2 <- paste("0",integer,sep = "")
  }else{
    integer_2<- integer
  }
  
  if (mins<=9){
    mins_2 <- paste("0",mins,sep = "")
  }else{
    mins_2<- mins
  }
  output <- (paste (integer_2,mins_2,sep=":"))
  
  return (output)
}

