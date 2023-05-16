
#######################################################################
# ------------        TOTAL EXPOSURE - TRADITIONAL METHODOLOGY   -------------     

#This code allows estimating the total daily exposure with the tradional methodology.
#
# In these case, the daily exposure is considered as the point of origin for the total time, that is, 24 hours.

# ----    The point is where the person lives
origin_point<- data.frame(latitude = -32.86620008642829,
                   longitude=-68.847007110213)
#----    Spatial transformation
coordinates(origin_point) <- ~longitude+latitude
proj4string(origin_point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#----    Put the Coordinate reference system
origin_point <- st_as_sf(origin_point, crs = 4326)
#----    Use the "temporary_grid_search" function to search and estimate the daily concentration. 

hour_00 <- "2018-08-01T00:01:00 -03"
hour_23 <- "2018-08-01T23:59:00 -03"
grid_search<-temporary_grid_search(start_hour = hour_00, end_hour = hour_23,dir_grids="D:/Josefina/paper_git/paper_exposure_model/grid_example",time_format="%Y-%m-%dT%H:%M:%S")


#----    Take the point where the person with the average concentration of PM lives
intersection_point <- st_intersection(origin_point ,grid_search)

# Calculate the daily expossure -  μg m-3 - 24hs

daily_exposure <- (intersection_point$value * 24)

print(daily_exposure) 
