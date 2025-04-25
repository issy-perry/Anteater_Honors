#based on Kat's code for movement models but modified for RSFs and my dataset
#load packages 
library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)
library(sf)
library(raster)
library(terra)
library(crayon)
#library(weights)# For calculating the weighted proportions of the land class types


# ---------------------------------moving window model based on Kat Chenn's code (which is based on Ryan Gill's code which is based on Dr. Noonan's code)---------------------------------------

#load data and wAKDEs ----
load("~/Giant_Anteater_Orphan/FIXED/Data/Orphan/Data_telemetry.rda") #telemetry object of orphaned telemetry data

load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda") #telemetry objects of wild-raised telemetry data


#rename for convenience
DATA_orphan <- DATA_list
DATA_wild <- DATA_TELEMETRY
rm(DATA_list, DATA_TELEMETRY) #clear up environment space

#individuals need to be separated by year so that 
DATA_17 <- DATA_wild[c(3,5,6,7,8,10,12,14,16,19,26)] #Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, and Puji
DATA_18 <- DATA_wild[c(1,2,4,9,11,13,15,17,18,20,21,22,23,24,25)] #Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, and Thomas
DATA_19 <- DATA_orphan[c(1,5,9,10)] #Arya, Capitu, Dumbo_1, and Dumbo_2
DATA_20 <- DATA_orphan[c(26)] #Tim_1
DATA_21 <- DATA_orphan[c(17,21,22,23,24,27)] #Mulan, Renee_1, Renee_2, Renee_3, Renee_4, and Tim_2
DATA_22 <- DATA_orphan[c(6,7,13,15,16,20)] #Cláudio, Colete, Heather, Juju_1, Juju_2, Peter, and Tim_3
DATA_23 <- DATA_orphan[c(2,3,4,6,8,11,12,14,18,19,25)] #Bahia, Beezie, Bella, Dom, Erick, George, Jacobina, Nancy, Nayeli, and Rita
rm(DATA_wild, DATA_orphan) #clear up environment space




#set time periods for window
#shift was determined by META output of tau position for range-resident orphans (2.1 days est) and wild-raised(1.4 days est) 
dt <- 2 %#% "day" #shift 2 day
win <- 5 %#% "day" #window covers 5 days

#create folders to hold results
folder_list <- c("Fits", 
                 "AKDEs",
                 "Mean_Speed",
                 "Land_Use",
                 "RSF_Select")


#set directory path
dir_path <- "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/"


# create every folder in the folder list in the directory path
for (folder in folder_list) {
  dir.create(paste0(dir_path, folder), 
             recursive = TRUE, showWarnings = TRUE)
}



#2017 moving window ------------------------------------------------------------------------------------------------------------------------
#set up rasters
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2017_cover.tif")
cover_2017 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2017_cover.tif")
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
rm(cover, non_cover, cover_17)

#window analysis
for(i in 1:length(DATA_17)){
  tic("window analysis")
  # subset out an individual
  DATA <- DATA_17[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 5-day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 2 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 5-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  #USE <- list()
  speed_mean <- list()
  
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (5 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
     
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
         #                    proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity

        #res <- cbind(res,PROPS)
        
        
        
        
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) # remember this is a df and not a list
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF) 

  gc() # free up computational resources
  
  
  # end of outer loop, goes back to a new anteater
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, DATA_17)

#2018 moving window -------------------------------------------------------------------------------------------------------------------
#set up rasters
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2018_cover.tif")
cover_2018 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2018_cover.tif")
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
rm(cover, non_cover, cover_18)

#window analysis
for(i in 1:length(DATA_18)){
  tic("window analysis")
  # subset out an individual 
  DATA <- DATA_18[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 5-day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 2 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 5-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  #USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (5 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error = TRUE), interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
        #                    proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity
        
        #res <- cbind(res,PROPS)
        
        
        
        
        #message(yellow("SPEED analyses"))
        #SPEED_MEAN <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        #SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/ZNEW_", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/ZNEW_", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "RSF_Select/RSF_", DATA@info[1], ".rda")) 
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF) #SPEED_MEAN, SPEEDS_INSTA,
  #SPEED_MEAN, SPEEDS_INSTA)
  gc() # free up computational resources
  
  # beep(3)
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW ANTEATER
  toc()
  
}

rm(FITS, wAKDEs , DATA_18, speed_mean, RSFs)


#combining 2018 and 2017 results for the wild-raised orphans ----



#2019 moving window -------------------------------------
#set up rasters
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2019_cover.tif")
cover_2019 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2019_cover.tif")
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
rm(cover, non_cover, cover_2019)


#window analysis
for(i in 1:length(DATA_19)){
  tic("window analysis")
  # subset out an individual 
  DATA <- DATA_19[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 5-day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 2 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 5-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  #USE <- list()
  speed_mean <- list()
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (5 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 2)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        #HR.df$land_class[HR.df$land_class %in% c("11")] <- "Wetland"
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
        #                     proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity
        #res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
        #res$max_HFI <- max(HR.df2$HFI)
        #res <- cbind(res,PROPS)
        
        
        
        
        #message(yellow("SPEED analyses"))
        #SPEED_MEAN <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        #SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "rsfs/TEST_", DATA@info[1], ".rda")) # remember this is a df and not a list
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, USE, RSF) #SPEED_MEAN, SPEEDS_INSTA,
  #SPEED_MEAN, SPEEDS_INSTA)
  gc() # free up computational resources
  
  # beep(3)
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW ANTEATER
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, RSFs, DATA_19)




#2020 moving window -------------------------------------
#set up rasters
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2020_cover.tif")
cover_2020 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2020_cover.tif")
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
rm(cover, non_cover, cover_2020)

#window analysis
for(i in 1:length(DATA_20)){
  tic("window analysis")
  # subset an individual out
  DATA <- DATA_20[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 3-day segment for the individual, using t column instead of timestamp column because of the nature of how this is set up and using ctmm objects
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 3-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  USE <- list()
  speed_mean <- list()
  #speeds_insta <- list()
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (3 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 2)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        #HR.df$land_class[HR.df$land_class %in% c("11")] <- "Wetland"
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
        #                     proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity
        #res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
        #res$max_HFI <- max(HR.df2$HFI)
        #res <- cbind(res,PROPS)
        
        
        
        
        #message(yellow("SPEED analyses"))
        #SPEED_MEAN <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        #SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "rsfs/TEST_", DATA@info[1], ".rda")) # remember this is a df and not a list
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, SPEED, RSF) 

  gc() # free up computational resources
  
  # beep(3)
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW ANTEATER
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, RSFs, DATA_20)


#2021 moving window -------------------------------------------------------------------------------------------------------------------------
#set up rasters
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2021_cover.tif")
cover_2021 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2021_cover.tif")
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
rm(cover, non_cover, cover_2021)

#window analysis
for(i in 1:length(DATA_21)){
  tic("window analysis")
  # subset an individual out
  DATA <- DATA_21[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 5-day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 2 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 5-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) 
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  #USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (5 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) #fast
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) #fast 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        #HR.df$land_class[HR.df$land_class %in% c("11")] <- "Wetland"
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
        #                     proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity
        #res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
        #res$max_HFI <- max(HR.df2$HFI)
        #res <- cbind(res,PROPS)
        
        
        
        
        #message(yellow("SPEED analyses"))
        #SPEED_MEAN <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        #SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "rsfs/TEST_", DATA@info[1], ".rda")) # remember this is a df and not a list
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, USE, SPEED, RSF) 

  gc() # free up computational resources
  
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW ANTEATER
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, land_types, DATA_21)


#2022 moving window --------------------------------------------------------------------------------------------------------------
#set up raster
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2022_cover.tif")
cover_2022 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2022_cover.tif")
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
rm(cover, non_cover, cover_2022)

#window analysis
for(i in 1:length(DATA_22)){
  tic("window analysis")
  # subset out an individual
  DATA <- DATA_22[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 5-day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 2 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 5-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) # fast method is used because all the data are in the same timezone, adjust if they cross timezone boundaries
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  #USE <- list()
  speed_mean <- list()

  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (5 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        #HR.df$land_class[HR.df$land_class %in% c("11")] <- "Wetland"
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
        #                     proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity
        #res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
        #res$max_HFI <- max(HR.df2$HFI)
        #res <- cbind(res,PROPS)
        
        
        
        
        #message(yellow("SPEED analyses"))
        #SPEED_MEAN <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        #SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "rsfs/TEST_", DATA@info[1], ".rda")) 
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, USE, SPEED, RSF) 

  gc() # free up computational resources
  

  
  # END OF OUTER LOOP, START AT TOP WITH A NEW ANTEATER
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, land_types, RSFs, DATA_22)





#2023 moving window --------------------------------------------------------------------------------------------------------------
#set up raster
#land_types <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2023_cover.tif")
cover_2023 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2023_cover.tif")
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
rm(cover, non_cover, cover_2023)

#window analysis
for(i in 1:length(DATA_23)){
  tic("window analysis")
  # subset out an individual
  DATA <- DATA_23[[i]]
  
  # Set up the window segments ......................................................
  # Generate start times with a 5-day segment for the individual
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 2 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 5-day windows and not time specific
  #Convert Unix timestamps to POSIXct
  times <- as.POSIXct(as.numeric(as.character(times)), origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1], #1970-01-01
                                                                                                          DATA$longitude[1],
                                                                                                          method = "accurate")) # fast method is used because all the data are in the same timezone, adjust if they cross timezone boundaries
  
  # set up list to store
  FITS <- list()
  wAKDEs <- list()
  RSFs <- list()
  #USE <- list()
  speed_mean <- list()
  
  
  
  # Analysis on the window segment ......................................
  for (j in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[j] <= DATA$t & DATA$t <= times[j] + win,] # +win means window size (5 days)
    
    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", j, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", #1970-01-01 as numeric as character
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", #1970-01-01 #as numeric as character
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((j), "of", length(times), "iterations. Window segment:",
                        WINDOW_START, "to", WINDOW_END,
                        "for anteater:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FIT <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FIT, "ctmm")) {
        AKDES <- akde(SUBSET, FIT, weights = TRUE)
        RSF <- rsf.select(SUBSET, UD = AKDES, R = covers, cores = 4)
        SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
        #HR <- rast(raster(AKDES, DF = "PMF"))
        #HR2 <- project(HR, crs(land_types), res = res(land_types))
        #HR.df2 <- terra::as.data.frame(HR2, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df2$layer <- HR.df2$layer/sum(HR.df2$layer)
        #HR <- project(HR, crs(land_types), res = res(land_types))
        #HR.df <- terra::as.data.frame(HR, xy = TRUE, na.rm = TRUE)
        #Renormalize
        #HR.df$layer <- HR.df$layer/sum(HR.df$layer)
        
        #Extract habitat values
        #HR.df2$HFI <- extract(HFI, HR.df2[,1:2])[,2]/1000 
        #HR.df$land_class <- extract(land_types, HR.df[,1:2])[,2]
        #HR.df$land_class[HR.df$land_class %in% c("1","3", "4","5","6","49","29")] <- "Native_forest"
        #HR.df$land_class[HR.df$land_class %in% c("11")] <- "Wetland"
        #HR.df$land_class[HR.df$land_class %in% c("9")] <- "Forestry"
        #HR.df$land_class[HR.df$land_class %in% c("12","15")] <- "Pasture"
        #HR.df$land_class[HR.df$land_class %in% c("18","19","20","21","39","40","41","62","36","46","47","35","48")] <- "Agriculture"
        #HR.df$land_class[HR.df$land_class %in% c("24","25","30")] <- "Development"
        #HR.df$land_class[HR.df$land_class %in% c("11","26","33")] <- "Water"
        
        # Use the home range PDF to calculate the weighted proportions of time spent the different land class types
        #PROPS <- round(wpct(HR.df$land_class, HR.df$layer)*100,2)
        #PROPS2 <- data.frame(class = names(PROPS),
        #                     proportion = as.numeric(PROPS))
        #PROPS <- data.frame(t(PROPS2))[2,]
        #names(PROPS) <- PROPS2$class
        
        
        #res <- data.frame(binomial = "Myrmecophaga_tridactyla")
        #res$ID <- AKDES@info$identity
        #res$mean_HFI <- sum(HR.df2$layer*HR.df2$HFI)
        #res$max_HFI <- max(HR.df2$HFI)
        #res <- cbind(res,PROPS)
        
        
        
        
        #message(yellow("SPEED analyses"))
        #SPEED_MEAN <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        #SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1)
        
        # store models/UDs in a list, name the entry based on anteater name and subset window start date, not the times[i] as that is in unix format
        FITS[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FIT
        wAKDEs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED
        RSFs[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        #USE[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- res
        
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", j, "-", e$message, "\n")
    })
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(underline(bgGreen(white(paste("saving output for anteater", DATA@info[1])))))
  save(FITS, file = paste0(dir_path, "Fits/", DATA@info[1], ".rda"))
  save(wAKDEs, file = paste0(dir_path, "AKDEs/", DATA@info[1], ".rda"))
  save(speed_mean, file = paste0(dir_path, "Mean_Speed/speed_", DATA@info[1], ".rda"))
  save(RSFs, file = paste0(dir_path, "rsfs/TEST_", DATA@info[1], ".rda")) 
  #save(USE, file = paste0(dir_path, "Land_Use/Use_", DATA@info[1], ".rda"))
  
  # clean up environment
  rm(FIT, AKDES, USE, SPEED, RSF) 
  
  gc() # free up computational resources
  
  
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW ANTEATER
  toc()
  
}

rm(FITS, wAKDEs, speed_mean, land_types, RSFs, DATA_23)

















































 

