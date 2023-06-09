### get the array ID

args <- commandArgs(TRUE)

if (length(args) == 1) {
  for(i in 1:length(args)){
    eval(parse(text = args))
  }
} else {
  stop()
}
print (arrayid)

### Load libraries
library(ggplot2)
library(ncdf4)
library(tidyverse)
library(chron)
library(maps)
options(bitmapType='cairo') 
### get the ncdf file list

backwardFiles <- list.files("/home/datawork-flopped-nos/simuTale/goodArea/Backward/results/", pattern = ".nc", full.names = T)
forwardFiles <- list.files("/home/datawork-flopped-nos/simuTale/goodArea/Forward/results/", pattern = ".nc", full.names = T)

# create data for world coordinates using 
# # map_data() function
# world_coordinates <- map_data("world")
# netcdf <- nc_open(filename = "~/Desktop/drakkar/simu_1964_11_forward_ichthyop-run202305290233.nc")
# longitude <- ncvar_get(netcdf, varid="lon")
# latitude <- ncvar_get(netcdf, varid="lat")
# timetmp <- month.day.year(ncvar_get(netcdf, "time")/(3600*24), origin. = c(month = 1, day = 1, year = 1958))
# monthtmp <- timetmp[["month"]]
# yeartmp <- timetmp[["year"]]
# daytmp  <- timetmp[["day"]]
# date_nc <- as.POSIXct(paste(yeartmp, monthtmp, daytmp, sep="-"), format ="%Y-%m-%d", tz="GMT")
# 
# whichday <- 11
# data <- data.frame(LongitudeDay30= longitude[,whichday], LatitudeDay30= latitude[,whichday], date=date_nc[whichday])
# # dataset <- nc_open('D:/modelisationLarve/data/simuTale/forward/simu_1959_01_forward_ichthyop-run202305281402.nc')
# input_folder <- 'D:/modelisationLarve/data/simuTale/test/'
# output_folder <- 'C:/Users/dexin/Documents/Travail/COOOL/Travail/lagrangian/graphics/'
# 
# p <- ggplot(data, aes(x=LongitudeDay30, y=LatitudeDay30)) + geom_density_2d_filled(alpha = 0.5) + 
#   geom_map(  data = world_coordinates, map = world_coordinates,  aes(long, lat, map_id = region)) + xlim(40, 130) + ylim(-50,30)+ggtitle(label = paste(date_nc[whichday]) )
# p
# # Get the names of all files in the folder
# file_names <- list.files(path = input_folder, full.names = TRUE)
world_coordinates <- map_data("world")
if (arrayid <335){
  fileBack <- backwardFiles[arrayid]
  
  # Iterate over each file
  dataset <- nc_open(fileBack)
  latitude <- ncvar_get(dataset, 'lat')
  longitude <- ncvar_get(dataset, 'lon')
  timetmp <- month.day.year(ncvar_get(dataset, 'time')/(3600*24), origin. = c(month = 1, day = 1, year = 1958))
  #timetmp
  monthtmp <- timetmp[["month"]]
  yeartmp <- timetmp[["year"]]
  daytmp  <- timetmp[["day"]]
  date_nc <- as.POSIXct(paste(yeartmp, monthtmp, daytmp, sep="-"), format ="%Y-%m-%d", tz="GMT")
  print(date_nc)
  
  whichday <- 4 # backward: 8, 5, 1 // forward: 4, 7, 11
  
  while(whichday <= length(date_nc)) {
    print(date_nc[whichday])
    df <- data.frame(Longitude = longitude[,whichday], Latitude = latitude[,whichday], date=date_nc[whichday])
    # Plot creation
    p <- ggplot(df, aes(x=Longitude, y=Latitude)) + 
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
      scale_fill_distiller(palette=4, direction=-1) + # color palette customization
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(
        legend.position='none' # remove legend
      ) +
      geom_map(data = world_coordinates, map = world_coordinates, aes(x=long, y=lat, map_id = region)) + 
      xlim(40,130) + ylim(-50,30) +
      ggtitle(label = paste(date_nc[whichday]) )
    
    # Generate the output file path
    file_name <- unlist(strsplit(tools::file_path_sans_ext(basename(fileBack)), split="-"))[1]
    output_file <- paste0("/home/datawork-flopped-nos/simuTale/goodArea/Backward/Figures/", file_name, "_", paste(date_nc[whichday]), ".png")
    print(whichday)
    print(output_file)
    # Save the file as a PNG file
    ggsave(output_file, plot = p, width = 7, height = 5, dpi = 300)
    
    # Increment whichday by 3
    whichday <- whichday + 3
  }
  
}
  fileForw <- forwardFiles[arrayid]
  dataset <- nc_open(fileForw)
  latitude <- ncvar_get(dataset, 'lat')
  longitude <- ncvar_get(dataset, 'lon')
  timetmp <- month.day.year(ncvar_get(dataset, 'time')/(3600*24), origin. = c(month = 1, day = 1, year = 1958))
  #timetmp
  monthtmp <- timetmp[["month"]]
  yeartmp <- timetmp[["year"]]
  daytmp  <- timetmp[["day"]]
  date_nc <- as.POSIXct(paste(yeartmp, monthtmp, daytmp, sep="-"), format ="%Y-%m-%d", tz="GMT")
  print(date_nc)
  
  whichday <- 4 # backward: 8, 5, 1 // forward: 4, 7, 11
  #world_coordinates <- map_data("world")  
  while(whichday <= length(date_nc)) {
    print(date_nc[whichday])
    df <- data.frame(Longitude = longitude[,whichday], Latitude = latitude[,whichday], date=date_nc[whichday])
    # Plot creation
    p <- ggplot(df, aes(x=Longitude, y=Latitude)) + 
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
      scale_fill_distiller(palette=4, direction=-1) + # color palette customization
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme(
        legend.position='none' # remove legend
      ) +
      geom_map(data = world_coordinates, map = world_coordinates, aes(long, lat, map_id = region)) + 
      xlim(40,130) + ylim(-50,30) +
      ggtitle(label = paste(date_nc[whichday]) )
    
    # Generate the output file path
    file_name <- unlist(strsplit(tools::file_path_sans_ext(basename(fileForw)), split="-"))[1]
    
    output_file <- paste0("/home/datawork-flopped-nos/simuTale/goodArea/Forward/Figures/", file_name, "_", paste(date_nc[whichday]), ".png")
    
    # Save the file as a PNG file
    ggsave(output_file, plot = p, width = 7, height = 5, dpi = 300)
    
    # Increment whichday by 3
    whichday <- whichday + 3
  }
  print("Finished")