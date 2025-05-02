# 1 Fit Movement Models
# This script includes the following:
#     1. Fit movement models (orphaned and wild-raised populations separately)
#     2. Estimate utilization distributions, or UDs (orphaned and wild-raised populations separately)
#     3. Estimate mean speeds (orphaned and wild-raised populations separately)


#load packages
library(ctmm) #working with tel data and fitting models
library(tictoc) #prints out time a model takes for each individual

#orphaned----
#load data
load("~/Giant_Anteater_Orphan/FINAL/DATA/Orphaned/Data_telemetry.rda") #save file

#fit movement models
#make a list to hold the models
FITS_orphan <- list()

#for.loop for models
for(i in 1:length(DATA_orphan)){
  
  tic("individual")
  
  #Extract individual
  DATA <- DATA_orphan[[i]]
  
  # create variograms based on individuals
  GUESS <- ctmm.guess(DATA, CTMM=ctmm(error = TRUE), interactive = FALSE) 
  
  # fit models to variograms
  FITS_orphan[[i]] <- ctmm.select(DATA, GUESS, trace = TRUE, cores = -1)
  
  toc()
}

#transfer names from tel data list to the movement model list
names(FITS_orphan) <- names(DATA_orphan)

#save output
save(FITS_orphan, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/Fits/Fits_orphan.rda") #save file




#AKDEs
#typically AKDEs are all fit in one line; however, these models are being finicky (likely due to dispersing behaviors). 
#So, we will fit individuals separately in a for.loop
#make a list to hold the UDs
AKDE_orphan <- list()

#for.loop for calculating UDs
for(i in 1:length(DATA_orphan)){
  
  #extract individual
  DATA <- DATA_orphan[[i]]
  
  #extract movement model fits
  FIT <- FITS_orphan[[i]]
  
  #calculate wAKDE
  AKDE_orphan[[i]] <- akde(DATA, FIT, weights = TRUE) #weights = TRUE because we do have dispersing populations
}

#transfer names from tel daa list to models
names(AKDE_orphan) <- names(DATA_orphan)

#save output
save(AKDE_orphan, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/AKDEs/UDs_orphan.rda")



#mean speed
#make a list to hold results
SPEED_orphan <- list()

#for.loop for calculating mean speed
for(i in 1:length(DATA_orphan)){
  
  #extract individual's telemetry data
  DATA <- DATA_orphan[[i]]
  
  #extract individual's movement model fit
  FIT <- FITS_orphan[[i]]
  
  #calculate average speed and input into list
  SPEED_orphan[[i]] <- speed(object = DATA, CTMM = FIT, robust = TRUE, units = FALSE, cores = -1) #in m/s
}

#transfer names from tel data to speed estimations
names(SPEED_orphan) <- names(FITS_orphan)

#save output
save(SPEED_orphan, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/Speed/Speed_orphans.rda")









#wild-raised ----
#load data of wild-raised individuals
DATA_wild <- read.csv("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Anteater_Data_Clean_2.csv")
#change name of outlier column to be the same as the tbl for orphaned anteaters
DATA_wild <- DATA_wild %>% rename(outlier = OUT)
#convert to a telemetry object
DATA_wild <- as.telemetry(DATA_wild, mark.rm = TRUE) #Convert to telemetry object and list and mark.rm = TRUE will drop outliers

#fit movement models
#make a list to hold movement model results
FITS_wild <- list()

#for.loop for models
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

#transfer names from tel data
names(FITS_wild) <- names(DATA_wild) 

#save output
save(FITS_wild, file = "~/Giant_Anteater_Orphan/FIXED/Data/Fits_wild.rda") #save file



#AKDEs
#typically AKDEs are all fit in one line; however, these models are being finicky (likely due to dispersing behaviors). 
#So, we will fit individuals separately in a for.loop
#make a list to hold the UDs
AKDE_wild <- list()

#for.loop for calculating UDs
for(i in 1:length(DATA_wild)){
  
  #extract individual
  DATA <- DATA_wild[[i]]
  
  #extract movement model fits
  FIT <- FITS_wild[[i]]
  
  #calculate wAKDE
  AKDE_wild[[i]] <- akde(DATA, FIT, weights = TRUE) #weights = TRUE because we do have dispersing populations
}

#transfer names from tel data
names(AKDE_wild) <- names(DATA_wild)

#save output
save(AKDE_wild, file = "~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/wAKDEs_wild.rda")



#mean speed
#make a list to hold results
SPEED_orphan <- list()

#for.loop for calculating mean speed
SPEED_wild <- list()
for(i in 1:length(DATA_wild)){
  
  #extract individual's telemetry data
  DATA <- DATA_wild[[i]]
  
  #extract individual's movement model fit
  FIT <- FITS_wild[[i]]
  
  #calculate average speed and input into list
  SPEED_wild[[i]] <- speed(object = DATA, CTMM = FIT, robust = TRUE, units = FALSE) #in m/s
}

#transfer names from tel data
names(SPEED_wild) <- names(FITS_wild)

#save output
save(SPEED_wild, file = "~/Giant_Anteater_Orphan/FIXED/Results/Speed/Speed_wild_raised.rda")


