
#load packages
library(ctmm) # fitting variograms
library(dplyr)
library(tictoc)



#import data and convert to telemetry ----
GPS_data_tbl <- readRDS("~/Giant_Anteater_Orphan/FIXED/Data/clean_GPS_data2025-03-07-18-36.rds")
class(GPS_data_tbl) #dataset was imported as a tbl_df
for(i in 1:length(GPS_data_tbl$tel)){ #Convert to a dataframe
  GPS_data_tbl$tel[[i]]$ID <- GPS_data_tbl$animal[[i]]  # add in  IDs
}
DATA_orphan <- do.call(rbind, GPS_data_tbl$tel) #converts tiblle to a dataframe
class(DATA_orphan) #works
DATA_orphan <- as.telemetry(DATA_orphan, mark.rm = TRUE) #Convert to telemetry object and list and mark.rm = TRUE will drop outliers
class(DATA_orphan) #works
plot(DATA_orphan, error = FALSE, type = "l", col = rainbow(length(DATA_orphan))) #Check that it worked



#UERE ----
#UERE needs to be estimated so that models will consider uncertainties of points when fitting (puts less weight on a data point with higher HDOP)
#this is because data was likely not calibrated correctly 
uere(DATA_orphan) <- c(20,10) #assigns RMS UERE (2D, and 3D respectively)
UERE <- uere(DATA_orphan) #puts values into dataset to run
summary(UERE)
UERE$DOF[] <- 2 #set degrees of freedom because they are currently set to infinity
summary(UERE)
uere(DATA_orphan) <- UERE #assign values to dataset
#now models can be run while accounting for potential measurement error

save(DATA_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Data/Orphan/Data_telemetry.rda") #save file


#fit movement models ------------------------orphaned telemetry data ----------------------------------------------------------------------
FITS_orphan <- list()
for(i in 1:length(DATA_orphan)){
  
  #Extract individual
  DATA <- DATA_orphan[[i]]
  
  # create variograms based on individuals
  GUESS <- ctmm.guess(DATA, CTMM=ctmm(error = TRUE), interactive = FALSE) 
  
  # fit models to variograms
  FITS_orphan[[i]] <- ctmm.select(DATA, GUESS, trace = TRUE, cores = -1) 
  
}

names(FITS_orphan) <- names(DATA_orphan) #transfers names to new list FIT
summary(FITS_orphan) #check on models
save(FITS_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Data/Fits_orphan.rda") #save file




#AKDEs
#typically AKDEs are all fit in one line; however, these data are being finicky. So, individual movement model fits and telemetry data are being extracted prior to fitting AKDEs
AKDE_orphan <- list()
for(i in 1:length(DATA_orphan)){
  
  #extract individual
  DATA <- DATA_orphan[[i]]
  
  #extract movement model fits
  FIT <- FITS_orphan[[i]]
  
  #calculate wAKDE
  AKDE_orphan[[i]] <- akde(DATA, FIT, weights = TRUE) #weights = TRUE because we do have dispersing populations
}
names(AKDE_orphan) <- names(DATA_orphan)
summary(AKDE_orphan) #check on models
save(AKDE_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/wAKDEs_orphan.rda")


#calculate mean speed
SPEED_orphan <- list()
for(i in 1:length(DATA_orphan)){
  #extract individual's telemetry data
  DATA <- DATA_orphan[[i]]
  #extract individual's movement model fit
  FIT <- FITS_orphan[[i]]
  #calculate average speed and input into list
  SPEED_orphan[[i]] <- speed(object = DATA, CTMM = FIT, robust = TRUE, units = FALSE) #in m/s
}#end of loop
names(SPEED_orphan) <- names(FITS_orphan)
save(SPEED_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Results/Speed/Speed_orphans.rda")









#------------------------------------------------------------------ wild-raised -----------------------------------------------------------------
#load data of wild-raised individuals
DATA_wild <- read.csv("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Anteater_Data_Clean_2.csv")
#change name of outlier column to be the same as the tbl for orphaned anteaters
DATA_wild <- DATA_wild %>% rename(outlier = OUT)
#convert to a telemetry object
DATA_wild <- as.telemetry(DATA_wild, mark.rm = TRUE) #Convert to telemetry object and list and mark.rm = TRUE will drop outliers

#fit movement models ---------------------- wild-raised telemetry data --------------------------------------------------------------------
FITS_wild <- list()
for(i in 1:length(DATA_wild)){
  tic("individual fit")
  #Extract individual
  DATA <- DATA_wild[[i]]
  
  # create variograms based on individuals
  GUESS <- ctmm.guess(DATA, CTMM=ctmm(error = TRUE), interactive = FALSE) 
  
  # fit models to variograms
  FITS_wild[[i]] <- ctmm.select(DATA, GUESS, trace = TRUE, cores = -1) #uses all but one core to parallelize
  
  toc()
}

names(FITS_wild) <- names(DATA_wild) #transfers names to new list FIT
summary(FITS_wild) #check on models
save(FITS_wild, file = "~/Giant_Anteater_Orphan/FIXED/Data/Fits_wild.rda") #save file




#make AKDEs ---- 
#typically AKDEs are all fit in one line; however, these data are being finicky. So, individual movement model fits and telemetry data are being extracted prior to fitting AKDEs
AKDE_wild <- list()
for(i in 1:length(DATA_wild)){
  
  #extract individual
  DATA <- DATA_wild[[i]]
  
  #extract movement model fits
  FIT <- FITS_wild[[i]]
  
  #calculate wAKDE
  AKDE_wild[[i]] <- akde(DATA, FIT, weights = TRUE) #weights = TRUE because we do have dispersing populations
}
names(AKDE_wild) <- names(DATA_wild)
summary(AKDE_wild) #check on models
save(AKDE_wild, file = "~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/wAKDEs_wild.rda")




#get speed for each orphaned anteater
SPEED_wild <- list()
for(i in 1:length(DATA_wild)){
  #extract individual's telemetry data
  DATA <- DATA_wild[[i]]
  #extract individual's movement model fit
  FIT <- FITS_wild[[i]]
  #calculate average speed and input into list
  SPEED_wild[[i]] <- speed(object = DATA, CTMM = FIT, robust = TRUE, units = FALSE) #in m/s
}#end of loop

names(SPEED_wild) <- names(FITS_wild)
save(SPEED_wild, file = "~/Giant_Anteater_Orphan/FIXED/Results/Speed/Speed_wild_raised.rda")


