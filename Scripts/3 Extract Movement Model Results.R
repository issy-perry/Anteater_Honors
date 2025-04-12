#load packages
library(ctmm)
#load data
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Orphaned/Fits.rda") #orphaned population (first half)
old <- FITS
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Orphaned/New_Orphan_Fits.rda") #orphanned population (second half)
new <- FITS
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Wild_raised/Lists/FITS_wild_total.rda") #wild population
#rename movement model fits

#combine new fits with old fits
FITS_orphan <- append(old, new)

#akdes
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Wild_raised/Lists/AKDE_wild_total.rda") #wild-raised wAKDEs
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Orphaned/wAKDEs.rda") #orphaned wAKDEs
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Orphaned/wAKDEs_orphan_new.rda") # new orphaned wAKDEs
AKDE_orphan <- append(AKDEs, AKDE_orphan_new)
rm(AKDE_orphan_new, AKDEs)
#free environment space
rm(FITS, old, new)


#speed
load("~/Giant_Anteater_Orphan/FIXED/Results/Speed/Speed_wild_raised.rda") #wild-raised population
load("~/Giant_Anteater_Orphan/FIXED/Results/Speed/Speed_orphans.rda") #orphaned population
#extracting from movement models -----------------------------------------------------------------------------------------------------------------
#orphan ----
#create a dataframe to hold results for whole population 
FITS_orphan_df <- data.frame()
for(i in 1:length(FITS_orphan)){
  #create a dataframe to hold summary outputs of movement models for EACH individual
  fit_test <- data.frame()
  #extract information from summary output of an individual's movement model
  summary_df <- data.frame(t(summary(FITS_orphan[[i]], units = FALSE)[["CI"]])) #units = FALSE makes all values the same scientific standard
  #if value is available in summary output, extract it into the individual's dataframe. if value is not
  if ("area..square.meters." %in% colnames(summary_df)) {
    fit_test[i, "area_low"] <- summary_df["low", "area..square.meters."]
  }else{
    fit_test[i, "area_low"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    fit_test[i, "area_est"] <- summary_df["est", "area..square.meters."]
  }else{
    fit_test[i, "area_est"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    fit_test[i, "area_high"] <- summary_df["high", "area..square.meters."]
  }else{
    fit_test[i, "area_high"] <- NA
  }
  if ("τ.position...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τposition_low"] <- summary_df["low", "τ.position...seconds."]
  }else{
    fit_test[i, "τposition_low"] <- NA
  }
  if ("τ.position...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τposition_est"] <- summary_df["est", "τ.position...seconds."]
  }else{
    fit_test[i, "τposition_est"] <- NA
  }
  if ("τ.position...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τposition_high"] <- summary_df["high", "τ.position...seconds."]
  }else{
    fit_test[i, "τposition_high"] <- NA
  }
  if ("τ.velocity...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τvelocity_low"] <- summary_df["low", "τ.velocity...seconds."]
  }else{
    fit_test[i, "τvelocity_low"] <- NA
  }
  if ("τ.velocity...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τvelocity_est"] <- summary_df["est", "τ.velocity...seconds."]
  }else{
    fit_test[i, "τvelocity_est"] <- NA
  }
  if ("τ.velocity...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τvelocity_high"] <- summary_df["high", "τ.velocity...seconds."]
  }else{
    fit_test[i, "τvelocity_high"] <- NA
  }
  if ("speed..meters.second." %in% colnames(summary_df)) {
    fit_test[i, "speed_low"] <- summary_df["low", "speed..meters.second."]
  }else{
    fit_test[i, "speed_low"] <- NA
  }
  if ("speed..meters.second." %in% colnames(summary_df)) {
    fit_test[i, "speed_est"] <- summary_df["est", "speed..meters.second."]
  }else{
    fit_test[i, "speed_est"] <- NA
  }
  if ("speed..meters.second." %in% colnames(summary_df)) {
    fit_test[i, "speed_high"] <- summary_df["high", "speed..meters.second."]
  }else{
    fit_test[i, "speed_high"] <- NA
  }
  if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
    fit_test[i, "diffusion_low"] <- summary_df["low", "diffusion..square.meters.second."]
  }else{
    fit_test[i, "diffusion_low"] <- NA
  }
  if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
    fit_test[i, "diffusion_est"] <- summary_df["est", "diffusion..square.meters.second."]
  }else{
    fit_test[i, "diffusion_est"] <- NA
  }
  if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
    fit_test[i, "diffusion_high"] <- summary_df["high", "diffusion..square.meters.second."]
  }else{
    fit_test[i, "diffusion_high"] <- NA
  }
   #end of inner loop
  #assign name of giant anteater to the individual's dataframe
  fit_test$ID <- FITS_orphan[[i]]@info[1]
  #bind individuals dataframe to total dataframe
  FITS_orphan_df <- rbind(FITS_orphan_df, fit_test) #stringAsFactors arguement allows us to just keep adding the new windows to the dataframe
} #end of outer loop

#remove extra NA values
FITS_orphan_df <- FITS_orphan_df[!is.na(FITS_orphan_df$area_low),]
#remove any random 0s that shouldn't be there (no one should have a total area of 0)
FITS_orphan_df <- FITS_orphan_df[FITS_orphan_df$area_low > 0.001,]

#convert units to all be the same and easily interpretable
#area
FITS_orphan_df[,"area_low"] <- "km^2" %#% FITS_orphan_df[,"area_low"] #converts to km^2 rather than m^2
FITS_orphan_df[, "area_est"] <- "km^2" %#% FITS_orphan_df[,"area_est"] #converts to km^2 rather than m^2
FITS_orphan_df[, "area_high"] <- "km^2" %#% FITS_orphan_df[,"area_high"] #converts to km^2 rather than m^2
#τposition
FITS_orphan_df[, "τposition_low"] <- "day" %#% FITS_orphan_df[,"τposition_low"] #converts to days rather than seconds
FITS_orphan_df[, "τposition_est"] <- "day" %#% FITS_orphan_df[,"τposition_est"] #converts to days rather than seconds
FITS_orphan_df[, "τposition_high"] <- "day" %#% FITS_orphan_df[,"τposition_high"] #converts to days rather than seconds
#τvelocity
FITS_orphan_df[, "τvelocity_low"] <- "minutes" %#% FITS_orphan_df[,"τvelocity_low"] #converts to minutes rather than seconds
FITS_orphan_df[, "τvelocity_est"] <- "minutes" %#% FITS_orphan_df[,"τvelocity_est"] #converts to minutes rather than seconds
FITS_orphan_df[, "τvelocity_high"] <- "minutes" %#% FITS_orphan_df[,"τvelocity_high"] #converts to minutes rather than seconds
#speed
FITS_orphan_df[, "speed_low"] <- "km/day" %#% FITS_orphan_df[, "speed_low"]#converts to km/day rather than m/s
FITS_orphan_df[, "speed_est"] <- "km/day" %#% FITS_orphan_df[, "speed_est"]#converts to km/day rather than m/s
FITS_orphan_df[, "speed_high"] <- "km/day" %#% FITS_orphan_df[, "speed_high"]#converts to km/day rather than m/s
#diffusion
FITS_orphan_df[, "diffusion_low"] <- "km^2/day" %#% FITS_orphan_df[, "diffusion_low"] #converts to km^2/day rather than m^2/s
FITS_orphan_df[, "diffusion_est"] <- "km^2/day" %#% FITS_orphan_df[, "diffusion_est"] #converts to km^2/day rather than m^2/s
FITS_orphan_df[, "diffusion_high"] <- "km^2/day" %#% FITS_orphan_df[, "diffusion_high"] #converts to km^2/day rather than m^2/s


#wild-raised ----
#create a dataframe to hold results for whole population 
FITS_wild_df <- data.frame()
for(i in 1:length(FITS_wild)){
  #create a dataframe to hold summary outputs of movement models for EACH individual
  fit_test <- data.frame()
  #extract information from summary output of an individual's movement model
  summary_df <- data.frame(t(summary(FITS_wild[[i]], units = FALSE)[["CI"]]))
  #if value is available in summary output, extract it into the individual's dataframe. if value is not
  if ("area..square.meters." %in% colnames(summary_df)) {
    fit_test[i, "area_low"] <- summary_df["low", "area..square.meters."]
  }else{
    fit_test[i, "area_low"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    fit_test[i, "area_est"] <- summary_df["est", "area..square.meters."]
  }else{
    fit_test[i, "area_est"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    fit_test[i, "area_high"] <- summary_df["high", "area..square.meters."]
  }else{
    fit_test[i, "area_high"] <- NA
  }
  if ("τ.position...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τposition_low"] <- summary_df["low", "τ.position...seconds."]
  }else{
    fit_test[i, "τposition_low"] <- NA
  }
  if ("τ.position...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τposition_est"] <- summary_df["est", "τ.position...seconds."]
  }else{
    fit_test[i, "τposition_est"] <- NA
  }
  if ("τ.position...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τposition_high"] <- summary_df["high", "τ.position...seconds."]
  }else{
    fit_test[i, "τposition_high"] <- NA
  }
  if ("τ.velocity...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τvelocity_low"] <- summary_df["low", "τ.velocity...seconds."]
  }else{
    fit_test[i, "τvelocity_low"] <- NA
  }
  if ("τ.velocity...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τvelocity_est"] <- summary_df["est", "τ.velocity...seconds."]
  }else{
    fit_test[i, "τvelocity_est"] <- NA
  }
  if ("τ.velocity...seconds." %in% colnames(summary_df)) {
    fit_test[i, "τvelocity_high"] <- summary_df["high", "τ.velocity...seconds."]
  }else{
    fit_test[i, "τvelocity_high"] <- NA
  }
  if ("speed..meters.second." %in% colnames(summary_df)) {
    fit_test[i, "speed_low"] <- summary_df["low", "speed..meters.second."]
  }else{
    fit_test[i, "speed_low"] <- NA
  }
  if ("speed..meters.second." %in% colnames(summary_df)) {
    fit_test[i, "speed_est"] <- summary_df["est", "speed..meters.second."]
  }else{
    fit_test[i, "speed_est"] <- NA
  }
  if ("speed..meters.second." %in% colnames(summary_df)) {
    fit_test[i, "speed_high"] <- summary_df["high", "speed..meters.second."]
  }else{
    fit_test[i, "speed_high"] <- NA
  }
  if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
    fit_test[i, "diffusion_low"] <- summary_df["low", "diffusion..square.meters.second."]
  }else{
    fit_test[i, "diffusion_low"] <- NA
  }
  if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
    fit_test[i, "diffusion_est"] <- summary_df["est", "diffusion..square.meters.second."]
  }else{
    fit_test[i, "diffusion_est"] <- NA
  }
  if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
    fit_test[i, "diffusion_high"] <- summary_df["high", "diffusion..square.meters.second."]
  }else{
    fit_test[i, "diffusion_high"] <- NA
  }
   #end of inner loop
  #assign name of giant anteater to the individual's dataframe
  fit_test$ID <- FITS_wild[[i]]@info[1]
  #bind individuals dataframe to total dataframe
  FITS_wild_df <- rbind(FITS_wild_df, fit_test, stringAsFactors = FALSE) #stringAsFactors arguement allows us to just keep adding the new windows to the dataframe
} #end of outer loop

#remove extra NA values
FITS_wild_df <- FITS_wild_df[!is.na(FITS_wild_df$area_low),]
#remove any random 0s that shouldn't be there (no one should have a total area of 0)
FITS_wild_df <- FITS_wild_df[FITS_wild_df$area_low > 0.001,]

#convert units to all be the same and easily interpretable
#area
FITS_wild_df[,"area_low"] <- "km^2" %#% FITS_wild_df[,"area_low"] #converts to km^2 rather than m^2
FITS_wild_df[, "area_est"] <- "km^2" %#% FITS_wild_df[,"area_est"] #converts to km^2 rather than m^2
FITS_wild_df[, "area_high"] <- "km^2" %#% FITS_wild_df[,"area_high"] #converts to km^2 rather than m^2
#τposition
FITS_wild_df[, "τposition_low"] <- "day" %#% FITS_wild_df[,"τposition_low"] #converts to days rather than seconds
FITS_wild_df[, "τposition_est"] <- "day" %#% FITS_wild_df[,"τposition_est"] #converts to days rather than seconds
FITS_wild_df[, "τposition_high"] <- "day" %#% FITS_wild_df[,"τposition_high"] #converts to days rather than seconds
#τvelocity
FITS_wild_df[, "τvelocity_low"] <- "minutes" %#% FITS_wild_df[,"τvelocity_low"] #converts to minutes rather than seconds
FITS_wild_df[, "τvelocity_est"] <- "minutes" %#% FITS_wild_df[,"τvelocity_est"] #converts to minutes rather than seconds
FITS_wild_df[, "τvelocity_high"] <- "minutes" %#% FITS_wild_df[,"τvelocity_high"] #converts to minutes rather than seconds
#speed
FITS_wild_df[, "speed_low"] <- "km/day" %#% FITS_wild_df[, "speed_low"]#converts to km/day rather than m/s
FITS_wild_df[, "speed_est"] <- "km/day" %#% FITS_wild_df[, "speed_est"]#converts to km/day rather than m/s
FITS_wild_df[, "speed_high"] <- "km/day" %#% FITS_wild_df[, "speed_high"]#converts to km/day rather than m/s
#diffusion
FITS_wild_df[, "diffusion_low"] <- "km^2/day" %#% FITS_wild_df[, "diffusion_low"] #converts to km^2/day rather than m^2/s
FITS_wild_df[, "diffusion_est"] <- "km^2/day" %#% FITS_wild_df[, "diffusion_est"] #converts to km^2/day rather than m^2/s
FITS_wild_df[, "diffusion_high"] <- "km^2/day" %#% FITS_wild_df[, "diffusion_high"] #converts to km^2/day rather than m^2/s




#AKDE wild-raised
AKDE_wild_df <- data.frame()
for(i in 1:length(AKDE_wild)){
  akde_test <- data.frame()
  summary_df <- data.frame(t(summary(AKDE_wild[[i]], units = FALSE)[["CI"]])) 
  if ("area..square.meters." %in% colnames(summary_df)) {
    akde_test[i, "HR_low"] <- summary_df["low", "area..square.meters."]
  }else{
    akde_test[i, "HR_low"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    akde_test[i, "HR_est"] <- summary_df["est", "area..square.meters."]
  }else{
    akde_test[i, "HR_est"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    akde_test[i, "HR_high"] <- summary_df["high", "area..square.meters."]
  }else{
    akde_test[i, "HR_high"] <- NA
  }
  akde_test$ID <- AKDE_wild[[i]]@info[1]
  AKDE_wild_df <- rbind(AKDE_wild_df, akde_test, stringAsFactors = FALSE) #stringAsFactors arguement allows us to just keep adding the new windows to the dataframe
  
} #end of outer loop

AKDE_wild_df <- AKDE_wild_df[!is.na(AKDE_wild_df$HR_low),]
AKDE_wild_df <- AKDE_wild_df[AKDE_wild_df$HR_low > 0.001,]  
#convert to km^2
AKDE_wild_df[,"HR_low"] <- "km^2" %#% AKDE_wild_df[,"HR_low"] #converts to km^2 rather than m^2 
AKDE_wild_df[, "HR_est"] <- "km^2" %#% AKDE_wild_df[,"HR_est"] #converts to km^2 rather than m^2 
AKDE_wild_df[, "HR_high"] <- "km^2" %#% AKDE_wild_df[,"HR_high"] #converts to km^2 rather than m^2  

#AKDE orphaned
AKDE_orphan_df <- data.frame()
for(i in 1:length(AKDE_orphan)){
  akde_test <- data.frame()
  summary_df <- data.frame(t(summary(AKDE_orphan[[i]], units = FALSE)[["CI"]])) 
  if ("area..square.meters." %in% colnames(summary_df)) {
    akde_test[i, "HR_low"] <- summary_df["low", "area..square.meters."]
  }else{
    akde_test[i, "HR_low"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    akde_test[i, "HR_est"] <- summary_df["est", "area..square.meters."]
  }else{
    akde_test[i, "HR_est"] <- NA
  }
  if ("area..square.meters." %in% colnames(summary_df)) {
    akde_test[i, "HR_high"] <- summary_df["high", "area..square.meters."]
  }else{
    akde_test[i, "HR_high"] <- NA
  }
  akde_test$ID <- AKDE_orphan[[i]]@info[1]
  AKDE_orphan_df <- rbind(AKDE_orphan_df, akde_test, stringAsFactors = FALSE) #stringAsFactors arguement allows us to just keep adding the new windows to the dataframe
  
} #end of outer loop

AKDE_orphan_df <- AKDE_orphan_df[!is.na(AKDE_orphan_df$HR_low),]
AKDE_orphan_df <- AKDE_orphan_df[AKDE_orphan_df$HR_low > 0.001,]  
#convert to km^2
AKDE_orphan_df[,"HR_low"] <- "km^2" %#% AKDE_orphan_df[,"HR_low"] #converts to km^2 rather than m^2 
AKDE_orphan_df[, "HR_est"] <- "km^2" %#% AKDE_orphan_df[,"HR_est"] #converts to km^2 rather than m^2 
AKDE_orphan_df[, "HR_high"] <- "km^2" %#% AKDE_orphan_df[,"HR_high"] #converts to km^2 rather than m^2 





#mean speed orphan
SPEED_orphan_df <- data.frame() 
for(i in 1:length(SPEED_orphan)){
  #create a dataframe to house the individual's results
  speed_test <- data.frame()
  #extract individual
  IND <- SPEED_orphan[[i]]
  #extract confidence intervals of speed from individual
  df <- IND[[2]] 
  if ("low" %in% colnames(df)) {
    speed_test[i, "Mean_Speed_low"] <- df[1, "low"]
  }else{
    speed_test[i, "Mean_Speed_low"] <- NA
  }
  if ("est" %in% colnames(df)) {
    speed_test[i, "Mean_Speed_est"] <- df[1, "est"]
  }else{
    speed_test[i, "Mean_Speed_est"] <- NA
  }
  if ("high" %in% colnames(df)) {
    speed_test[i, "Mean_Speed_high"] <- df[1, "high"]
  }else{
    speed_test[i, "Mean_Speed_high"] <- NA
  }
  SPEED_orphan_df <- rbind(SPEED_orphan_df, speed_test, stringAsFactors = FALSE)
} #end of inner loop
#remove NAs
SPEED_orphan_df <- SPEED_orphan_df[!is.na(SPEED_orphan_df$Mean_Speed_low),]
#keeps values with Inf high speeds
SPEED_orphan_df <- SPEED_orphan_df[SPEED_orphan_df$Mean_Speed_high > 0.001,]  


#convert units to our standard
SPEED_orphan_df[, "Mean_Speed_low"] <- "km/day" %#% SPEED_orphan_df[, "Mean_Speed_low"]#converts to km/day rather than m/s
SPEED_orphan_df[, "Mean_Speed_est"] <- "km/day" %#% SPEED_orphan_df[, "Mean_Speed_est"]#converts to km/day rather than m/s
SPEED_orphan_df[, "Mean_Speed_high"] <- "km/day" %#% SPEED_orphan_df[, "Mean_Speed_high"]#converts to km/day rather than m/s


#mean speed wild
SPEED_wild_df <- data.frame() 
for(i in 1:length(SPEED_wild)){
  #create a dataframe to house the individual's results
  speed_test <- data.frame()
  #extract individual
  IND <- SPEED_wild[[i]]
  #extract confidence intervals of speed from individual
  df <- IND[[2]] 
  if ("low" %in% colnames(df)) {
    speed_test[i, "Mean_Speed_low"] <- df[1, "low"]
  }else{
    speed_test[i, "Mean_Speed_low"] <- NA
  }
  if ("est" %in% colnames(df)) {
    speed_test[i, "Mean_Speed_est"] <- df[1, "est"]
  }else{
    speed_test[i, "Mean_Speed_est"] <- NA
  }
  if ("high" %in% colnames(df)) {
    speed_test[i, "Mean_Speed_high"] <- df[1, "high"]
  }else{
    speed_test[i, "Mean_Speed_high"] <- NA
  }
  SPEED_wild_df <- rbind(SPEED_wild_df, speed_test, stringAsFactors = FALSE)
} #end of inner loop
#remove NAs
SPEED_wild_df <- SPEED_wild_df[!is.na(SPEED_wild_df$Mean_Speed_low),]
#keeps values with Inf high speeds
SPEED_wild_df <- SPEED_wild_df[SPEED_wild_df$Mean_Speed_high > 0.001,]  


#convert units to our standard
SPEED_wild_df[, "Mean_Speed_low"] <- "km/day" %#% SPEED_wild_df[, "Mean_Speed_low"]#converts to km/day rather than m/s
SPEED_wild_df[, "Mean_Speed_est"] <- "km/day" %#% SPEED_wild_df[, "Mean_Speed_est"]#converts to km/day rather than m/s
SPEED_wild_df[, "Mean_Speed_high"] <- "km/day" %#% SPEED_wild_df[, "Mean_Speed_high"]#converts to km/day rather than m/s




#add indicators for rows for orhaned, wild-raised, successful, unsuccesful
ADKE_orphan_df$Status <- "Orphaned"
AKDE_wild_df$Status <- "Wild-raised"

#Successfullness
AKDE_orphan_df[c(7,9,11,12,13,15,16), "Success"] <- c("Successful")
AKDE_orphan_df[c(1,2,3,4,5,6,8,10,14,17,18,19,20), "Success"] <- c("Unsuccessful")
AKDE_wild_df$Success <- "Wild-raised"

#Residency
AKDE_orphan_df[c(), "Residency"] <- c("Range-resident")
AKDE_orphan_df[c(), "Residency"] <- c("Disperser")
AKDE_wild_df[c(), "Residency"] <- c("Range-resident")
AKDE_wild_df[c(), "Residency"] <- c("Disperser")


#combine dataframes into one movement model
Orphan_df <- cbind(FITS_orphan_df, SPEED_orphan_df, AKDE_orphan_df)
Wild_df <- cbind(FITS_wild_df, SPEED_wild_df, AKDE_wild_df)
Movement_df <- rbind(Orphan_df, Wild_df)

#save output
save(Movement_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/Fits/Total_movement_df.rda")



#meta function in ctmm calculates means of different movement parameters in a more informed manner than (i.e. taking sample size into account) and essentially conducts a meta analysis on the population
h
#speed
SPEED <- ctmm::meta(SPEED_orphan, variable = "speed", level = 0.95, units = FALSE)
SPEED <- as.data.frame(SPEED)
SPEED <- SPEED[-c(2,3),]
SPEED[,"low"] <- "km/day" %#% SPEED[,"low"] #converts to km/day rather than m/s
SPEED[,"est"] <- "km/day" %#% SPEED[, "est"] #converts to km/day rather than m/s
SPEED[,"high"] <- "km/day" %#% SPEED[,"high"] #converts to km/day rather than m/s
colnames(SPEED) <- c("speed_low", "speed_est", "speed_high")
#diffusion
DIFFUSION <- ctmm::meta(FITS_orphan, variable = "diffusion", level = 0.95, units = FALSE)
DIFFUSION <- as.data.frame(DIFFUSION)
DIFFUSION <- DIFFUSION[-c(2,3),]
DIFFUSION[,"low"] <- "km^2/day" %#% DIFFUSION[,"low"] #converts to km/day rather than m/s
DIFFUSION[,"est"] <- "km^2/day" %#% DIFFUSION[, "est"] #converts to km/day rather than m/s
DIFFUSION[,"high"] <- "km^2/day" %#% DIFFUSION[,"high"] #converts to km/day rather than m/s
colnames(DIFFUSION) <- c("diffusion_low", "diffusion_est", "diffusion_high")
#tau velocity
TAUVELOCITY <- ctmm::meta(FITS_orphan, variable = "tau velocity", level = 0.95, units= FALSE)
TAUVELOCITY <- as.data.frame(TAUVELOCITY)
TAUVELOCITY <- TAUVELOCITY[-c(2,3),]
TAUVELOCITY[,"low"] <- "minutes" %#% TAUVELOCITY[,"low"] #converts to km/day rather than m/s
TAUVELOCITY[,"est"] <- "minutes" %#% TAUVELOCITY[, "est"] #converts to km/day rather than m/s
TAUVELOCITY[,"high"] <- "minutes" %#% TAUVELOCITY[,"high"] #converts to km/day rather than m/s
colnames(TAUVELOCITY) <- c("τvelocity_low", "τvelocity_est", "τvelocity_high")
#home range
HR <- ctmm::meta(AKDE_orphan, variable = "area", level = 0.95, units = FALSE)
HR <- as.data.frame(HR)
HR <- HR[-c(2,3),]
HR[,"low"] <- "km^2" %#% HR[,"low"] #converts to km/day rather than m/s
HR[,"est"] <- "km^2" %#% HR[, "est"] #converts to km/day rather than m/s
HR[,"high"] <- "km^2" %#% HR[,"high"] #converts to km/day rather than m/s
colnames(HR) <- c("HR_low", "HR_est", "HR_high")
#combine all
META_total_o <- cbind(SPEED, DIFFUSION, TAUVELOCITY, HR)



#speed
SPEED <- ctmm::meta(SPEED_wild, variable = "speed", level = 0.95, units = FALSE)
SPEED <- as.data.frame(SPEED)
SPEED <- SPEED[-c(2,3),]
SPEED[,"low"] <- "km/day" %#% SPEED[,"low"] #converts to km/day rather than m/s
SPEED[,"est"] <- "km/day" %#% SPEED[, "est"] #converts to km/day rather than m/s
SPEED[,"high"] <- "km/day" %#% SPEED[,"high"] #converts to km/day rather than m/s
colnames(SPEED) <- c("speed_low", "speed_est", "speed_high")
#diffusion
DIFFUSION <- ctmm::meta(FITS_wild, variable = "diffusion", level = 0.95, units = FALSE)
DIFFUSION <- as.data.frame(DIFFUSION)
DIFFUSION <- DIFFUSION[-c(2,3),]
DIFFUSION[,"low"] <- "km^2/day" %#% DIFFUSION[,"low"] #converts to km/day rather than m/s
DIFFUSION[,"est"] <- "km^2/day" %#% DIFFUSION[, "est"] #converts to km/day rather than m/s
DIFFUSION[,"high"] <- "km^2/day" %#% DIFFUSION[,"high"] #converts to km/day rather than m/s
colnames(DIFFUSION) <- c("diffusion_low", "diffusion_est", "diffusion_high")
#tau velocity
TAUVELOCITY <- ctmm::meta(FITS_wild, variable = "tau velocity", level = 0.95, units= FALSE)
TAUVELOCITY <- as.data.frame(TAUVELOCITY)
TAUVELOCITY <- TAUVELOCITY[-c(2,3),]
TAUVELOCITY[,"low"] <- "minutes" %#% TAUVELOCITY[,"low"] #converts to km/day rather than m/s
TAUVELOCITY[,"est"] <- "minutes" %#% TAUVELOCITY[, "est"] #converts to km/day rather than m/s
TAUVELOCITY[,"high"] <- "minutes" %#% TAUVELOCITY[,"high"] #converts to km/day rather than m/s
colnames(TAUVELOCITY) <- c("τvelocity_low", "τvelocity_est", "τvelocity_high")
#home range
HR <- ctmm::meta(AKDE_wild, variable = "area", level = 0.95, units = FALSE)
HR <- as.data.frame(HR)
HR <- HR[-c(2,3),]
HR[,"low"] <- "km^2" %#% HR[,"low"] #converts to km/day rather than m/s
HR[,"est"] <- "km^2" %#% HR[, "est"] #converts to km/day rather than m/s
HR[,"high"] <- "km^2" %#% HR[,"high"] #converts to km/day rather than m/s
colnames(HR) <- c("HR_low", "HR_est", "HR_high")
#combine all
META_total_w <- cbind(SPEED, DIFFUSION, TAUVELOCITY, HR)

#bind orphaned and wild-raised together
META_df <- rbind(META_total_o, META_total_w)

#save output
save(META_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/Fits/META_df.rda")























