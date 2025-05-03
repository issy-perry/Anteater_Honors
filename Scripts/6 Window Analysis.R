# 6 Window Analysis
# This script includes the following:
#     1. Extract land types from rasters to create covariates for RSFs
#     2. Fit RSFs using tel data and AKDEs against each covariate

#The moving window code is primarily adapted from Kat Chhen's analysis for her masters thesis 
#This code is based on Ryan Gill's moving window (which in turn is based on Dr. Michael Noonan's code)

#load packages 
library(ctmm) #working with ctmm telemetry objects, fitting movement models and UDs, calculating mean speeds, and using RSFs
library(lutz) #working with times
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate) #working with dates
library(tictoc) #seeing how long each individual takes to run
library(sf)
library(terra) #working with Spatrasters
library(raster) #converting Spatrasters to raster classes for RSFs
library(crayon) #adding colored bands to printed announcements for tictoc
library(weights) # For calculating the weighted proportions of the land class types


#load telemetry data
load("~/Giant_Anteater_Orphan/FINAL/DATA/Orphaned/Data_telemetry.rda") #orphaned 
load("~/Giant_Anteater_Orphan/FINAL/DATA/Wild_raised/Data_telemetry.rda") #wild-raised


#rename for convenience
DATA_orphan <- DATA_list
DATA_wild <- DATA_TELEMETRY
rm(DATA_list, DATA_TELEMETRY) #clear up environment space



#individuals need to be separated by year so that 
#wild-raised
DATA_17 <- DATA_wild[c("Anthony", "Bumpus", "Cate", "Christoffer", "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao", 
                       "Puji", "Segre")]
DATA_18 <- DATA_wild[c("Alexander", "Annie", "Beto", "Hannah", "Jane", "Larry", "Luigi", "Margaret", "Maria", "Reid", 
                       "Rodolfo", "Sheron", "Thomas", "Delphine", "Gala")]
#orphaned
DATA_19 <- DATA_orphan[c("Arya", "Capitu", "Dumbo_1", "Dumbo_2")]
DATA_20 <- DATA_orphan[c("Tim_1")] 
DATA_21 <- DATA_orphan[c("Renee_1", "Renee_2", "Renee_3", "Renee_4", "Tim_2")]
DATA_22 <- DATA_orphan[c("Cláudio", "Colete", "Heather", "Juju_1", "Juju_2", "Mulan", "Peter", "Rita", "Tim_3")] 
DATA_23 <- DATA_orphan[c("Bahia", "Beezie", "Bella", "Dom", "Erick", "George", "Jacobina", "Nancy", "Nayeli")]

#clear environment space
rm(DATA_wild, DATA_orphan)



#set time periods for window
#window was determined by META output of tau position for range-resident orphans (2.1 days est) and wild-raised(1.4 days est) 
dt <- 1 %#% "day" #shift 1 day
win <- 2 %#% "day" #window covers 2 days

#create folders to hold results
folder_list <- c("Fits", 
                 "AKDEs",
                 "Mean_Speed",
                 "Land_Use",
                 "RSF_Select")


#set directory path
dir_path <- "~/Giant_Anteater_Orphan/FINAL/RESULTS/Windows/"


# create every folder in the folder list in the directory path
for (folder in folder_list) {
  dir.create(paste0(dir_path, folder), 
             recursive = TRUE, showWarnings = TRUE)
}



#2017 moving window ------------------------------------------------------------------------------------------------------------------------
#set up rasters
cover_2017 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2017_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2017
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2017
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)

#window analysis
for(i in 1:length(DATA_17)){
  tic("window analysis")
  # subset out an individual
  DATA <- DATA_17[[i]]
  
  # set up the window segments ......................................................
  # generate start times with a 2 day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  #ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  #convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()
  
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", 
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2017), res = res(cover_2017))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2017), res = res(cover_2017))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2017, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity

        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda"))
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 

  gc() # free up computational resources
  
  # end of outer loop, goes back to a new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, DATA_17, cover_2017)

#2018 moving window ------
#set up rasters
cover_2018 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2018_cover.tif")
`%notin%` <- Negate(`%in%`) #adding function in again in case needing to run separately
cover <- cover_2018
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2018
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)

#window analysis
for(i in 1:length(DATA_18)){
  tic("window analysis")
  # subset out an individual 
  DATA <- DATA_18[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 2 day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  # convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01",
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2018), res = res(cover_2018))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2018), res = res(cover_2018))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2018, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity
        
        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 

  gc() # free up computational resources
  
  # end of outer loop, strt at top with new anteater
  toc()
  
}

rm(FITS, wAKDEs , DATA_18, speed_mean, RSFs, cover_2018)





#2019 moving window -----
#set up rasters
cover_2019 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2019_cover.tif")
`%notin%` <- Negate(`%in%`) #adding function in again in case needing to run separately
cover <- cover_2019
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2019
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)


#window analysis
for(i in 1:length(DATA_19)){
  tic("window analysis")
  # subset out an individual 
  DATA <- DATA_19[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 2 day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", 
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2019), res = res(cover_2019))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2019), res = res(cover_2019))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2019, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity
        
        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        

        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 

  gc() # free up computational resources
  
  
  # end of outer loop, start at top with new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, RSFs, DATA_19, cover_2019)




#2020 moving window -----
#set up rasters
cover_2020 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2020_cover.tif")
`%notin%` <- Negate(`%in%`) #adding function in again in case needing to run separately
cover <- cover_2020
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2020
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)

#window analysis
for(i in 1:length(DATA_20)){
  tic("window analysis")
  # subset an individual out
  DATA <- DATA_20[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 2 day segment for the individual, using t column instead of timestamp column because of the nature of how this is set up and using ctmm objects
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", 
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2020), res = res(cover_2020))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2020), res = res(cover_2020))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2020, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity
        
        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 

  gc() # free up computational resources
  
  
  # end of outer loop, start at top with new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, RSFs, DATA_20, cover_2020)


#2021 moving window -----
#set up rasters
cover_2021 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2021_cover.tif")
`%notin%` <- Negate(`%in%`) #adding function in again in case needing to run separately
cover <- cover_2021
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2021
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)

#window analysis
for(i in 1:length(DATA_21)){
  tic("window analysis")
  # subset an individual out
  DATA <- DATA_21[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 2 day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", 
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2021), res = res(cover_2021))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2021), res = res(cover_2021))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2021, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity
        
        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 

  gc() # free up computational resources
  
  
  # end of outer loop, start at the top with a new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, DATA_21, cover_2021)


#2022 moving window -----
#set up raster
cover_2022 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2022_cover.tif")
`%notin%` <- Negate(`%in%`) #adding function in again in case needing to run separately
cover <- cover_2022
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2022
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)

#window analysis
for(i in 1:length(DATA_22)){
  tic("window analysis")
  # subset out an individual
  DATA <- DATA_22[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 2 day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2022), res = res(cover_2022))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2022), res = res(cover_2022))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2022, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity
        
        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 

  gc() # free up computational resources
  

  
  # end of outer loop, start at the top with a new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, RSFs, DATA_22, cover_2022)





#2023 moving window -----
#load raster
cover_2023 <- rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2023_cover.tif")
`%notin%` <- Negate(`%in%`) #adding function in again in case needing to run separately
cover <- cover_2023
cover[cover %notin% c(1,3,4,5,6,9,49,29)]<- FALSE #Native Forest and Forestry
cover[cover %in% c(1,3,4,5,6,9,49,29)]<- TRUE #Native Forest and Forestry
cover <- raster(cover)
non_cover <- cover_2023
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- FALSE #Pasture and Agriculture (Water and Development excluded)
non_cover[non_cover %in% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,38)] <- TRUE #Pasture and Agriculture (Water and Development excluded)
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)
rm(cover, non_cover)

#window analysis
for(i in 1:length(DATA_23)){
  tic("window analysis")
  # subset out an individual
  DATA <- DATA_23[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 2 day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 2 day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], 
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()
  
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (2 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", 
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment 
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      #movement model fits
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        #UDs
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        
        #RSFs
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        
        #mean speeds
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #weighted proportion of land use
        HR <- rast(raster(AKDES, DF = "PMF"))
        HR2 <- project(HR, crs(cover_2023), res = res(cover_2023))
        HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        HR <- project(HR, crs(cover_2023), res = res(cover_2023))
        HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        HR.df$land_class <- extract(cover_2023, HR.df[,1:2])[,2]
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        #combinind pasture and agriculture for noncover and forestry and native forest for cover
        HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29","9")] <- "Cover"
        HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48","12","15")] <- "Noncover"
        
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        PROPS2 <- data.frame(class = names(PROPS),
                             proportion = as.numeric(PROPS))
        PROPS <- data.frame(t(PROPS2))[2,]
        names(PROPS) <- PROPS2$class
        
        
        res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        res$ID <- AKDES@info$identity
        
        res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        

        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  } #end of inner loop
  
  # save all the outputs as a rds for future analysis 
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF, USE) 
  
  gc() # free up computational resources
  
  # end of outer loop, start at top with new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, RSFs, DATA_23, cover_2023)

















































 

