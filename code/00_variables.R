#################################################################

# Example  of an exposure model based on mobility and activities
# 0. Run necessary libraries for the model. If they are not installed
# install. 
#00_library

#1. Running the functions of the .R files:
# 01_trajectories_request.R
# 02_function_point-to-line.R
# 03_hours.R
# 04_exposure.R
#2. Locate yourself on the path where the CALPUFF grids are located
#local path
#setwd("D:/Josefina/Proyectos/CALPUFF/Resultados/V10/temp/")
setwd("D:/Josefina/paper_git/paper_exposure_model/grid_example")

#3. Variables

# ------------01. Origin-Destination points
travel_list <- data.frame(long =c(-68.8361,-68.822523),
                          lat = c(-32.9563,-32.922720))

# ------------02. Key tom-tom

key_1 <- "YOdvX5qKwpk9YRl9v0JzqC5qSYNOwbDc"
# ------------03. Transport mode 
# Always consider the round trip
mode = c("car","car")

#------------04. Path with all CALPUFF Grid
# the grids must be by day-hour
#concentrations_grid<- "D:/Josefina/Proyectos/CALPUFF/Resultados/V10/temp/"
concentrations_grid<- "D:/Josefina/paper_git/paper_exposure_model/grid_example"

setwd(concentrations_grid)


#------------05. Type of route selected
selection <- c("Faster route","Faster route")
selection <- c("shorter_route","shorter_route")
selection <- c("more_polluted_routea","more_polluted_route")
selection <- c("less_polluted_route","less_polluted_route")
selection <- c("more_exposure_route","more_exposure_route")
selection <- c("less_exposure_route","less_exposure_route")

## ------------07. Departure time from home for the first time.
departure_time_home <- "2018-08-01 07:50:00 -03"
## ------------08. Duration of each activity.

activity_minutes<-data.frame(activity_minutes=300)

#------- Examples

example_df <- total_exposure (travel_list, mode, concentrations_grid,key,selection,output_exp="df",
                            departure_time_home, activity_minutes)
example_plot <- total_exposure (travel_list, mode, concentrations_grid,key,selection,output_exp="plot",
                              departure_time_home, activity_minutes)


