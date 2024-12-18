
#load packages
library(ctmm) # fitting variograms
library(ggplot2) #creating boxplots of home ranges



#import data and convert to telemetry ----
GPS_data_tbl <- readRDS("~/Giant_Anteater_Orphan/Data/clean_GPS_data2024-11-22-12-49.rds")
class(GPS_data_tbl) #dataset was imported as a tbl_df
for(i in 1:length(GPS_data_tbl$tel)){ #Convert to a dataframe
  GPS_data_tbl$tel[[i]]$ID <- GPS_data_tbl$animal[[i]]  # add in  IDs
}
DATA_list <- do.call(rbind, GPS_data_tbl$tel) #converts tiblle to a dataframe
class(DATA_list) #works
DATA_list <- as.telemetry(DATA_list, mark.rm = TRUE) #Convert to telemetry object and list and mark.rm = TRUE will drop outliers
class(DATA_list) #works
plot(DATA_list$Renee_3, error = FALSE, type = "l", col = rainbow(length(DATA_list))) #Check that it worked

#UERE ----
#UERE needs to be assessed so that models will consider uncertainties of points when fitting (puts less weight on a data point with higher HDOP)
#however, this data may not be calibrated correctly, so we need to 
uere(DATA_list) <- c(20,10) #assigns RMS UERE (2D, and 3D respectively)
UERE <- uere(DATA_list) #puts values into dataset to run
summary(UERE)
UERE$DOF[] <- 2 #set degrees of freedom because they are currently set to infinity
summary(UERE)
uere(DATA_list) <- UERE #assign values to dataset
#now models can be run while accounting for potential measurement error




#fitting models ----
#because the data is formatted in a list, we must create a for.loop to fit the data to models 

FITS <- list()
for(i in 1:length(DATA_list)){
  
  #Extract individual
  DATA <- DATA_list[[i]]
  
  # create variograms based on individuals
  GUESS <- ctmm.guess(DATA, CTMM=ctmm(error = TRUE), interactive = FALSE) 
  
  # fit models to variograms
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = TRUE, cores = 1) 
  
}



names(FITS) <- names(DATA_list) #transfers names to new list FIT
#overlap(FIT) will overlap the models with one another 
summary(FITS) #check on models
save(FITS, file = "~/Giant_Anteater_Orphan/Data/Fits.rda") #save file


#refit Heather and Renee_3 ----
#both Heather and Renee_3 were fit to OU anisotropic error models, which do not include speed and tau velocity, 
#but we will need speed and tau velocity for future evaluations we will need to manually fit them to models that will have these variables
load("~/Giant_Anteater_Orphan/Data/Fits.rda") #load previously saved list
Heather <- DATA_list[[4]]
Guess_H <- ctmm.guess(Heather, CTMM=ctmm(error = TRUE), interactive = FALSE) #automated model guess
M.OUF_Heather <- ctmm.fit(Heather, Guess_H) #includes both speed and tau velocity
summary(M.OUF_Heather)
rm(Heather) #remove so we can rename
Heather <- M.OUF_Heather
#still not registering speed or
Renee_3 <- DATA_list[[8]]
Guess_R <- ctmm.guess(Renee_3, CTMM=ctmm(error = TRUE), interactive = FALSE)
M.OUF_Renee_3<- ctmm.fit(Renee_3, Guess_R) #includes both speed and tau velocity
summary(M.OUF_Renee_3)
rm(Renee_3) #remove so we can rename
Renee_3 <- M.OUF_Renee_3

#rename so that it does not double name the objects
Arya <- FITS[[1]]
Capitu <- FITS[[2]]
Dumbo_1 <- FITS [[3]]
Juju <- FITS [[5]]
Renee_1 <- FITS[[6]]
Renee_2 <- FITS[[7]]
Renee_4 <- FITS [[9]]
Tim_1 <- FITS [[10]]
Tim_3 <- FITS [[11]]

#make a new list including newly fit anteaters
FITS_NEW <- list(Arya, Capitu, Dumbo_1, Heather, Juju, Renee_1, Renee_2, Renee_3, Renee_4, Tim_1, Tim_3)
names(FITS_NEW) <- names(FITS) #transfers names to new list FIT
View(FITS_NEW) #worked
save(FITS_NEW, file = "~/Giant_Anteater_Orphan/Data/Updated_Fits.rda") #save file


#make AKDEs ---- 
#AKDEs are currently being fit separately because Renee_1 and Renee_3 will not fit unless subsetted
#AKDEs are weighted due to sampling schedule not being uniform for all
load("~/Giant_Anteater_Orphan/Data/Updated_Fits.rda") #load previously saved list
AKDE_Arya <-akde(DATA_list[[1]], FITS_NEW[[1]], weights = TRUE)
AKDE_Capitu <-akde(DATA_list[[2]], FITS_NEW[[2]], weights = TRUE)
AKDE_Dumbo <-akde(DATA_list[[3]], FITS_NEW[[3]], weights = TRUE)
AKDE_Heather <-akde(DATA_list[[4]], FITS_NEW[[4]], weights = TRUE)
AKDE_Juju <-akde(DATA_list[[5]], FITS_NEW[[5]], weights = TRUE)
AKDE_Renee_1 <-akde(DATA_list[[6]], FITS_NEW[[6]], weights = TRUE) #missing dt value (find out what that means)
AKDE_Renee_2 <-akde(DATA_list[[7]], FITS_NEW[[7]], weights = TRUE)
AKDE_Renee_3 <-akde(DATA_list[[8]], FITS_NEW[[8]], weights = TRUE) #missing dt value (find out what that means)
AKDE_Renee_4 <-akde(DATA_list[[9]], FITS_NEW[[9]], weights = TRUE)
AKDE_Tim_1 <-akde(DATA_list[[10]], FITS_NEW[[10]], weights = TRUE)
AKDE_Tim_3 <-akde(DATA_list[[11]], FITS_NEW[[11]], weights = TRUE)
wAKDEs <- list(AKDE_Arya, AKDE_Capitu, AKDE_Dumbo, AKDE_Heather, AKDE_Juju, AKDE_Renee_1, AKDE_Renee_2, AKDE_Renee_3, AKDE_Renee_4, AKDE_Tim_1, AKDE_Tim_3)
names(wAKDEs) <- names(FITS_NEW)
save(wAKDEs, file = "~/Giant_Anteater_Orphan/Data/wAKDEs.rda") #save file
save(AKDE_Arya, file = "~/Giant_Anteater_Orphan/Data/AKDE_Arya.rda") #save file
save(AKDE_Capitu, file = "~/Giant_Anteater_Orphan/Data/AKDE_Capitu.rda") #save file
save(AKDE_Dumbo, file = "~/Giant_Anteater_Orphan/Data/AKDE_Dumbo.rda") #save file
save(AKDE_Heather, file = "~/Giant_Anteater_Orphan/Data/AKDE_Heather.rda") #save file
save(AKDE_Juju, file = "~/Giant_Anteater_Orphan/Data/AKDE_Juju.rda") #save file
save(AKDE_Renee_1, file = "~/Giant_Anteater_Orphan/Data/AKDE_Renee_1.rda") #save file
save(AKDE_Renee_2, file = "~/Giant_Anteater_Orphan/Data/AKDE_Renee_2.rda") #save file
save(AKDE_Renee_3, file = "~/Giant_Anteater_Orphan/Data/AKDE_Renee_3.rda") #save file
save(AKDE_Renee_4, file = "~/Giant_Anteater_Orphan/Data/AKDE_Renee_4.rda") #save file
save(AKDE_Tim_1, file = "~/Giant_Anteater_Orphan/Data/AKDE_Tim_1.rda") #save file
save(AKDE_Tim_3, file = "~/Giant_Anteater_Orphan/Data/AKDE_Tim_3.rda") #save file



#plotting home ranges---- 
#to make sure wAKDEs were made succesffuly, we will plot them here
wAKDES <- load("~/Giant_Anteater_Orphan/Data/wAKDEs.rda") #load AKDE list
plot(wAKDEs$Arya)
plot(wAKDEs$Capitu)
plot(wAKDEs$Dumbo_1)
plot(wAKDEs$Juju)
plot(wAKDEs$Heather)
plot(wAKDEs$Renee_1)
plot(wAKDEs$Renee_2)
plot(wAKDEs$Renee_3)
plot(wAKDEs$Renee_4)
plot(wAKDEs$Tim_1)
plot(wAKDEs$Tim_3)













































































#important for later -----------------------------------------------------------------------------------------------------------------------------------

#head(DATA[[1]]@info) #gives important information (i.e. identity, time zone, and projection [which will be needed to fit the model])

