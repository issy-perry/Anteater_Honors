#load packages
library(ctmm) #movement
library(dplyr) #dataframes
library(ggplot2) #making plots
library(ggpubr) #arranging plots in multiples
library(lubridate) #dates for the moving window model




#load data
load("~/Giant_Anteater_Orphan/FIXED/Data/Orphan/TOTAL_Data.rda") 
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda")
DATA_orphan <- DATA_list
DATA_wild <- DATA_TELEMETRY
rm(DATA_list, DATA_TELEMETRY)

#create file path for importing window analysis results
Select_path_o <- paste("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/RSF_Select/Orphaned", sep = "")
Select_path_w <- paste("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/RSF_Select/Wild_raised", sep = "")
AKDE_path_o <- paste("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/AKDEs/Orphan", sep = "")
AKDE_path_w <- paste("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/AKDEs/Wild_raised", sep = "")
FIT_path_o <- paste ("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Fits/Orphan", sep = "")
FIT_path_w <- paste ("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Fits/Wild_raised", sep = "")
SPEED_path_o <- paste("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Mean_Speed/Orphan", sep = "")
SPEED_path_w <- paste("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Mean_Speed/Wild_raised", sep = "")


#totals -----------------------------------------------------------------------------------------
DATA <- DATA_wild
SELECT_wild <- data.frame()
SPEED_wild <- data.frame()
AKDE_wild <- data.frame()
FIT_wild <- data.frame()



for(j in 1:length(DATA)){
  
  tryCatch({
    SELECTPATH <- file.path(Select_path_w,
                            paste("RSF_",
                                  DATA[[j]]@info[1],
                                  ".rda",
                                  sep = ""))
    #extract an individual 
    Selected <- get(load(SELECTPATH))
  }, error = function(e) {
    cat("Individual does not have model", j, "-", e$message, "\n")
  }) #end of trycatch
  
  #create a dataframe to hold all observations from the window analysis for one individual
  select_win <- data.frame(ID = character(length(Selected))) 
  for(i in 1:length(Selected)){
    SELECT <- Selected[[i]]
    select_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(Selected)[i])
    summary_df <- data.frame(t(summary(SELECT, units = FALSE)[["CI"]]))
    if ("cover..1.cover." %in% colnames(summary_df)) {
      select_win[i, "Cover_low"] <- summary_df["low", "cover..1.cover."]
    }else{
      select_win[i, "Cover_low"] <- NA
    }
    if ("cover..1.cover." %in% colnames(summary_df)) {
      select_win[i, "Cover_est"] <- summary_df["est", "cover..1.cover."]
    }else{
      select_win[i, "Cover_est"] <- NA
    }
    if ("cover..1.cover." %in% colnames(summary_df)) {
      select_win[i, "Cover_high"] <- summary_df["high", "cover..1.cover."]
    }else{
      select_win[i, "Cover_high"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      select_win[i, "Non_cover_low"] <- summary_df["low", "non_cover..1.non_cover."]
    }else{
      select_win[i, "Non_cover_low"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      select_win[i, "Non_cover_est"] <- summary_df["est", "non_cover..1.non_cover."]
    }else{
      select_win[i, "Non_cover_est"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      select_win[i, "Non_cover_high"] <- summary_df["high", "non_cover..1.non_cover."]
    }else{
      select_win[i, "Non_cover_high"] <- NA
    }
  } #end of inner loop
  select_win$ID <- SELECT@info$identity
  SELECT_wild <- rbind(SELECT_wild, select_win, stringAsFactors = FALSE) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe
  #load AKDE path
  AKDEPATH <- file.path(AKDE_path_w,
                        paste("AKDE_",
                              DATA[[j]]@info[1],
                              ".rda",
                              sep = ""))
  #load one individual's AKDE
  AKDE <- get(load(AKDEPATH))
  #make a dataframe long enough to hold all windows from the window analysis
  akde_win <- data.frame(ID = character(length(AKDE))) 
  for(i in 1:length(AKDE)){
    #extract one window
    HR <- AKDE[[i]]
    #add a window to the dataframe
    akde_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(AKDE)[i])
    summary_df <- data.frame(t(summary(HR, units = FALSE)[["CI"]])) 
    if ("area..square.meters." %in% colnames(summary_df)) {
      akde_win[i, "HR_low"] <- summary_df[1, "area..square.meters."]
    }else{
      akde_win[i, "HR_low"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      akde_win[i, "HR_est"] <- summary_df[1, "area..square.meters."]
    }else{
      akde_win[i, "HR_est"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      akde_win[i, "HR_high"] <- summary_df[1, "area..square.meters."]
    }else{
      akde_win[i, "HR_high"] <- NA
    }
  } #end of inner loop
  akde_win$ID <- HR@info$identity
  AKDE_wild<- rbind(AKDE_wild, akde_win, stringAsFactors = FALSE)
  #load speed path
  SPEEDPATH <- file.path(SPEED_path_w,
                         paste("speed_",
                               DATA[[j]]@info[1],
                               ".rda",
                               sep = ""))
  SPEED <- get(load(SPEEDPATH))
  speed_win <- data.frame(ID = character(length(SPEED))) 
  for(i in 1:length(SPEED)){
    speed_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(SPEED)[i])
    speedind <- SPEED[[i]]
    df <- speedind[[2]] #extracts confidence intervals from speed 
    if ("low" %in% colnames(df)) {
      speed_win[i, "Mean_Speed_low"] <- df[1, "low"]
    }else{
      speed_win[i, "Mean_Speed_low"] <- NA
    }
    if ("est" %in% colnames(df)) {
      speed_win[i, "Mean_Speed_est"] <- df[1, "est"]
    }else{
      speed_win[i, "Mean_Speed_est"] <- NA
    }
    if ("high" %in% colnames(df)) {
      speed_win[i, "Mean_Speed_high"] <- df[1, "high"]
    }else{
      speed_win[i, "Mean_Speed_high"] <- NA
    }
  } #end of inner loop
  #bind to total dataframe
  SPEED_wild <- rbind(SPEED_wild, speed_win, stringAsFactors = FALSE)
  FITPATH <- file.path(FIT_path_w,
                       paste("FIT_",
                             DATA[[j]]@info[1],
                             ".rda",
                             sep = ""))
  #load one individual's fit
  FIT <- get(load(FITPATH))
  #make a dataframe long enough to hold all windows from the window analysis
  fit_win <- data.frame(ID = character(length(FIT))) 
  for(i in 1:length(FIT)){
    #extract window from individual
    fits <- FIT[[i]]
    #add a window to the dataframe
    fit_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(FIT)[i])
    #extract the summary() output from the movement model fit
    summary_df <- data.frame(t(summary(fits, units = FALSE)[["CI"]])) 
    if ("area..square.meters." %in% colnames(summary_df)) {
      fit_win[i, "area_low"] <- summary_df["low", "area..square.meters."]
    }else{
      fit_win[i, "area_low"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      fit_win[i, "area_est"] <- summary_df["est", "area..square.meters."]
    }else{
      fit_win[i, "area_est"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      fit_win[i, "area_high"] <- summary_df["high", "area..square.meters."]
    }else{
      fit_win[i, "area_high"] <- NA
    }
    if ("τ.position...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τposition_low"] <- summary_df["low", "τ.position...seconds."]
    }else{
      fit_win[i, "τposition_low"] <- NA
    }
    if ("τ.position...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τposition_est"] <- summary_df["est", "τ.position...seconds."]
    }else{
      fit_win[i, "τposition_est"] <- NA
    }
    if ("τ.position...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τposition_high"] <- summary_df["high", "τ.position...seconds."]
    }else{
      fit_win[i, "τposition_high"] <- NA
    }
    if ("τ.velocity...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τvelocity_low"] <- summary_df["low", "τ.velocity...seconds."]
    }else{
      fit_win[i, "τvelocity_low"] <- NA
    }
    if ("τ.velocity...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τvelocity_est"] <- summary_df["est", "τ.velocity...seconds."]
    }else{
      fit_win[i, "τvelocity_est"] <- NA
    }
    if ("τ.velocity...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τvelocity_high"] <- summary_df["high", "τ.velocity...seconds."]
    }else{
      fit_win[i, "τvelocity_high"] <- NA
    }
    if ("speed..meters.second." %in% colnames(summary_df)) {
      fit_win[i, "speed_low"] <- summary_df["low", "speed..meters.second."]
    }else{
      fit_win[i, "speed_low"] <- NA
    } 
    if ("speed..meters.second." %in% colnames(summary_df)) {
      fit_win[i, "speed_est"] <- summary_df["est", "speed..meters.second."]
    }else{
      fit_win[i, "speed_est"] <- NA
    } 
    if ("speed..meters.second." %in% colnames(summary_df)) {
      fit_win[i, "speed_high"] <- summary_df["high", "speed..meters.second."]
    }else{
      fit_win[i, "speed_high"] <- NA
    } 
    if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
      fit_win[i, "diffusion_low"] <- summary_df["low", "diffusion..square.meters.second."]
    }else{
      fit_win[i, "diffusion_low"] <- NA
    } 
    if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
      fit_win[i, "diffusion_est"] <- summary_df["est", "diffusion..square.meters.second."]
    }else{
      fit_win[i, "diffusion_est"] <- NA
    } 
    if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
      fit_win[i, "diffusion_high"] <- summary_df["high", "diffusion..square.meters.second."]
    }else{
      fit_win[i, "diffusion_high"] <- NA
    } 
  } #end of inner loop
  fit_win$ID <- fits@info$identity
  FIT_wild <- rbind(FIT_wild, fit_win, stringAsFactors = FALSE)
  
  
} #end of outer loop

#bind some of the windows together
window_wild <- cbind(SPEED_wild, AKDE_wild, FIT_wild)
window_wild <- subset(window_wild, select = -ID) 
window_wild <- subset(window_wild, select = -window_start.2) #this was initially kept in to ensure that dataframes were binding properly
window_wild <- subset(window_wild, select = -window_start.1)
window_wild <- subset(window_wild, window_start != FALSE) #removes random falses throughout dataframe
#fix date fromatting
#total window
window_wild$window_start <- as.POSIXct(window_wild$window_start, format = "%Y-%m-%d %H:%M:%S")
window_wild$month_day <- format(window_wild$window_start, "%m/%d")
window_wild$month_day <- as.Date(window_wild$month_day, format = "%m/%d")
window_wild$numeric_date <- as.numeric(window_wild$month_day) #for fitting gam

#land use window
SELECT_wild$window_start <- as.POSIXct(SELECT_wild$window_start, format = "%Y-%m-%d %H:%M:%S")
SELECT_wild$month_day <- format(SELECT_wild$window_start, "%m/%d")
SELECT_wild$month_day <- as.Date(SELECT_wild$month_day, format = "%m/%d")
SELECT_wild$numeric_date <- as.numeric(SELECT_wild$month_day) #for fitting gam
#SELECT_wild <- SELECT_wild[!is.na(SELECT_wild$window_start),]
SELECT_wild$Cover <- as.numeric(SELECT_wild$Cover_low)
SELECT_wild$Cover <- as.numeric(SELECT_wild$Cover_est)
SELECT_wild$Cover <- as.numeric(SELECT_wild$Cover_high)
SELECT_wild$Non_cover <- as.numeric(SELECT_wild$Non_cover_low)
SELECT_wild$Non_cover <- as.numeric(SELECT_wild$Non_cover_est)
SELECT_wild$Non_cover <- as.numeric(SELECT_wild$Non_cover_high)
SELECT_wild <- subset(SELECT_wild, ID != FALSE)





#convert units
#area
window_wild[,"area_low"] <- "km^2" %#% window_wild[,"area_low"] #converts to km^2 rather than m^2
window_wild[, "area_est"] <- "km^2" %#% window_wild[,"area_est"] #converts to km^2 rather than m^2
window_wild[, "area_high"] <- "km^2" %#% window_wild[,"area_high"] #converts to km^2 rather than m^2
#τposition
window_wild[, "τposition_low"] <- "day" %#% window_wild[,"τposition_low"] #converts to days rather than seconds
window_wild[, "τposition_est"] <- "day" %#% window_wild[,"τposition_est"] #converts to days rather than seconds
window_wild[, "τposition_high"] <- "day" %#% window_wild[,"τposition_high"] #converts to days rather than seconds
#τvelocity
window_wild[, "τvelocity_low"] <- "minutes" %#% window_wild[,"τvelocity_low"] #converts to minutes rather than seconds
window_wild[, "τvelocity_est"] <- "minutes" %#% window_wild[,"τvelocity_est"] #converts to minutes rather than seconds
window_wild[, "τvelocity_high"] <- "minutes" %#% window_wild[,"τvelocity_high"] #converts to minutes rather than seconds
#speed
window_wild[, "speed_low"] <- "km/day" %#% window_wild[, "speed_low"]#converts to km/day rather than m/s
window_wild[, "speed_est"] <- "km/day" %#% window_wild[, "speed_est"]#converts to km/day rather than m/s
window_wild[, "speed_high"] <- "km/day" %#% window_wild[, "speed_high"]#converts to km/day rather than m/s
#diffusion
window_wild[, "diffusion_low"] <- "km^2/day" %#% window_wild[, "diffusion_low"] #converts to km^2/day rather than m^2/s
window_wild[, "diffusion_est"] <- "km^2/day" %#% window_wild[, "diffusion_est"] #converts to km^2/day rather than m^2/s
window_wild[, "diffusion_high"] <- "km^2/day" %#% window_wild[, "diffusion_high"] #converts to km^2/day rather than m^2/s
#HR
window_wild[,"HR_low"] <- "km^2" %#% window_wild[,"HR_low"] #converts to km^2 rather than m^2
window_wild[,"HR_est"] <- "km^2" %#% window_wild[,"HR_est"] #converts to km^2 rather than m^2
window_wild[,"HR_high"] <- "km^2" %#% window_wild[,"HR_high"] #converts to km^2 rather than m^2
#mean speed
window_wild[, "Mean_Speed_low"] <- "km/day" %#% window_wild[, "Mean_Speed_low"]#converts to km/day rather than m/s
window_wild[, "Mean_Speed_est"] <- "km/day" %#% window_wild[, "Mean_Speed_est"]#converts to km/day rather than m/s
window_wild[, "Mean_Speed_high"] <- "km/day" %#% window_wild[, "Mean_Speed_high"]#converts to km/day rather than m/s


save(window_wild, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Wild_raised_df.rda")
save(SELECT_wild, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Wild_raised_select_df.rda")



#total orphan
DATA <- DATA_orphan
SELECT_orphan <- data.frame()
SPEED_orphan <- data.frame()
AKDE_orphan <- data.frame()
FIT_orphan <- data.frame()



for(j in 1:length(DATA)){
  
  tryCatch({
    SELECTPATH <- file.path(Select_path_o,
                            paste("RSF_",
                                  DATA[[j]]@info[1],
                                  ".rda",
                                  sep = ""))
    #extract an individual 
    Selected <- get(load(SELECTPATH))
  }, error = function(e) {
    cat("Individual does not have model", j, "-", e$message, "\n")
  }) #end of trycatch
  
  #create a dataframe to hold all observations from the window analysis for one individual
  select_win <- data.frame(ID = character(length(Selected))) 
  for(i in 1:length(Selected)){
    SELECT <- Selected[[i]]
    select_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(Selected)[i])
    summary_df <- data.frame(t(summary(SELECT, units = FALSE)[["CI"]]))
    if ("cover..1.cover." %in% colnames(summary_df)) {
      select_win[i, "Cover_low"] <- summary_df["low", "cover..1.cover."]
    }else{
      select_win[i, "Cover_low"] <- NA
    }
    if ("cover..1.cover." %in% colnames(summary_df)) {
      select_win[i, "Cover_est"] <- summary_df["est", "cover..1.cover."]
    }else{
      select_win[i, "Cover_est"] <- NA
    }
    if ("cover..1.cover." %in% colnames(summary_df)) {
      select_win[i, "Cover_high"] <- summary_df["high", "cover..1.cover."]
    }else{
      select_win[i, "Cover_high"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      select_win[i, "Non_cover_low"] <- summary_df["low", "non_cover..1.non_cover."]
    }else{
      select_win[i, "Non_cover_low"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      select_win[i, "Non_cover_est"] <- summary_df["est", "non_cover..1.non_cover."]
    }else{
      select_win[i, "Non_cover_est"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      select_win[i, "Non_cover_high"] <- summary_df["high", "non_cover..1.non_cover."]
    }else{
      select_win[i, "Non_cover_high"] <- NA
    }
  } #end of inner loop
  select_win$ID <- SELECT@info$identity
  SELECT_orphan <- rbind(SELECT_orphan, select_win, stringAsFactors = FALSE) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe
  #load AKDE path
  AKDEPATH <- file.path(AKDE_path_o,
                        paste("AKDE_",
                              DATA[[j]]@info[1],
                              ".rda",
                              sep = ""))
  #load one individual's AKDE
  AKDE <- get(load(AKDEPATH))
  #make a dataframe long enough to hold all windows from the window analysis
  akde_win <- data.frame(ID = character(length(AKDE))) 
  for(i in 1:length(AKDE)){
    #extract one window
    HR <- AKDE[[i]]
    #add a window to the dataframe
    akde_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(AKDE)[i])
    summary_df <- data.frame(t(summary(HR, units = FALSE)[["CI"]])) 
    if ("area..square.meters." %in% colnames(summary_df)) {
      akde_win[i, "HR_low"] <- summary_df[1, "area..square.meters."]
    }else{
      akde_win[i, "HR_low"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      akde_win[i, "HR_est"] <- summary_df[1, "area..square.meters."]
    }else{
      akde_win[i, "HR_est"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      akde_win[i, "HR_high"] <- summary_df[1, "area..square.meters."]
    }else{
      akde_win[i, "HR_high"] <- NA
    }
  } #end of inner loop
  akde_win$ID <- HR@info$identity
  AKDE_orphan<- rbind(AKDE_orphan, akde_win, stringAsFactors = FALSE)
  #load speed path
  SPEEDPATH <- file.path(SPEED_path_o,
                         paste("speed_",
                               DATA[[j]]@info[1],
                               ".rda",
                               sep = ""))
  SPEED <- get(load(SPEEDPATH))
  speed_win <- data.frame(ID = character(length(SPEED))) 
  for(i in 1:length(SPEED)){
    speed_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(SPEED)[i])
    speedind <- SPEED[[i]]
    df <- speedind[[2]] #extracts confidence intervals from speed 
    if ("low" %in% colnames(df)) {
      speed_win[i, "Mean_Speed_low"] <- df[1, "low"]
    }else{
      speed_win[i, "Mean_Speed_low"] <- NA
    }
    if ("est" %in% colnames(df)) {
      speed_win[i, "Mean_Speed_est"] <- df[1, "est"]
    }else{
      speed_win[i, "Mean_Speed_est"] <- NA
    }
    if ("high" %in% colnames(df)) {
      speed_win[i, "Mean_Speed_high"] <- df[1, "high"]
    }else{
      speed_win[i, "Mean_Speed_high"] <- NA
    }
  } #end of inner loop
  #bind to total dataframe
  SPEED_orphan <- rbind(SPEED_orphan, speed_win, stringAsFactors = FALSE)
  FITPATH <- file.path(FIT_path_o,
                       paste("FIT_",
                             DATA[[j]]@info[1],
                             ".rda",
                             sep = ""))
  #load one individual's fit
  FIT <- get(load(FITPATH))
  #make a dataframe long enough to hold all windows from the window analysis
  fit_win <- data.frame(ID = character(length(FIT))) 
  for(i in 1:length(FIT)){
    #extract window from individual
    fits <- FIT[[i]]
    #add a window to the dataframe
    fit_win$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(FIT)[i])
    #extract the summary() output from the movement model fit
    summary_df <- data.frame(t(summary(fits, units = FALSE)[["CI"]])) 
    if ("area..square.meters." %in% colnames(summary_df)) {
      fit_win[i, "area_low"] <- summary_df["low", "area..square.meters."]
    }else{
      fit_win[i, "area_low"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      fit_win[i, "area_est"] <- summary_df["est", "area..square.meters."]
    }else{
      fit_win[i, "area_est"] <- NA
    }
    if ("area..square.meters." %in% colnames(summary_df)) {
      fit_win[i, "area_high"] <- summary_df["high", "area..square.meters."]
    }else{
      fit_win[i, "area_high"] <- NA
    }
    if ("τ.position...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τposition_low"] <- summary_df["low", "τ.position...seconds."]
    }else{
      fit_win[i, "τposition_low"] <- NA
    }
    if ("τ.position...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τposition_est"] <- summary_df["est", "τ.position...seconds."]
    }else{
      fit_win[i, "τposition_est"] <- NA
    }
    if ("τ.position...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τposition_high"] <- summary_df["high", "τ.position...seconds."]
    }else{
      fit_win[i, "τposition_high"] <- NA
    }
    if ("τ.velocity...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τvelocity_low"] <- summary_df["low", "τ.velocity...seconds."]
    }else{
      fit_win[i, "τvelocity_low"] <- NA
    }
    if ("τ.velocity...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τvelocity_est"] <- summary_df["est", "τ.velocity...seconds."]
    }else{
      fit_win[i, "τvelocity_est"] <- NA
    }
    if ("τ.velocity...seconds." %in% colnames(summary_df)) {
      fit_win[i, "τvelocity_high"] <- summary_df["high", "τ.velocity...seconds."]
    }else{
      fit_win[i, "τvelocity_high"] <- NA
    }
    if ("speed..meters.second." %in% colnames(summary_df)) {
      fit_win[i, "speed_low"] <- summary_df["low", "speed..meters.second."]
    }else{
      fit_win[i, "speed_low"] <- NA
    } 
    if ("speed..meters.second." %in% colnames(summary_df)) {
      fit_win[i, "speed_est"] <- summary_df["est", "speed..meters.second."]
    }else{
      fit_win[i, "speed_est"] <- NA
    } 
    if ("speed..meters.second." %in% colnames(summary_df)) {
      fit_win[i, "speed_high"] <- summary_df["high", "speed..meters.second."]
    }else{
      fit_win[i, "speed_high"] <- NA
    } 
    if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
      fit_win[i, "diffusion_low"] <- summary_df["low", "diffusion..square.meters.second."]
    }else{
      fit_win[i, "diffusion_low"] <- NA
    } 
    if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
      fit_win[i, "diffusion_est"] <- summary_df["est", "diffusion..square.meters.second."]
    }else{
      fit_win[i, "diffusion_est"] <- NA
    } 
    if ("diffusion..square.meters.second." %in% colnames(summary_df)) {
      fit_win[i, "diffusion_high"] <- summary_df["high", "diffusion..square.meters.second."]
    }else{
      fit_win[i, "diffusion_high"] <- NA
    } 
  } #end of inner loop
  fit_win$ID <- fits@info$identity
  FIT_orphan <- rbind(FIT_orphan, fit_win, stringAsFactors = FALSE)
  
  
  #convert all units to proper units
  
} #end of outer loop

#bind some of the windows together
window_orphan <- cbind(SPEED_orphan, AKDE_orphan, FIT_orphan)
window_orphan <- subset(window_orphan, select = -ID) 
window_orphan <- subset(window_orphan, select = -window_start.2) #this was initially kept in to ensure that dataframes were binding properly
window_orphan <- subset(window_orphan, select = -window_start.1)
window_orphan <- subset(window_orphan, window_start != FALSE) #removes random falses throughout dataframe
#fix date fromatting
#total window
window_orphan$window_start <- as.POSIXct(window_orphan$window_start, format = "%Y-%m-%d %H:%M:%S")
window_orphan$month_day <- format(window_orphan$window_start, "%m/%d")
window_orphan$month_day <- as.Date(window_orphan$month_day, format = "%m/%d")
window_orphan$numeric_date <- as.numeric(window_orphan$month_day) #for fitting gam

#land use window
SELECT_orphan$window_start <- as.POSIXct(SELECT_orphan$window_start, format = "%Y-%m-%d %H:%M:%S")
SELECT_orphan$month_day <- format(SELECT_orphan$window_start, "%m/%d")
SELECT_orphan$month_day <- as.Date(SELECT_orphan$month_day, format = "%m/%d")
SELECT_orphan$numeric_date <- as.numeric(SELECT_orphan$month_day) #for fitting gam
#SELECT_orphan <- SELECT_orphan[!is.na(SELECT_orphan$window_start),]
SELECT_orphan$Cover <- as.numeric(SELECT_orphan$Cover_low)
SELECT_orphan$Cover <- as.numeric(SELECT_orphan$Cover_est)
SELECT_orphan$Cover <- as.numeric(SELECT_orphan$Cover_high)
SELECT_orphan$Non_cover <- as.numeric(SELECT_orphan$Non_cover_low)
SELECT_orphan$Non_cover <- as.numeric(SELECT_orphan$Non_cover_est)
SELECT_orphan$Non_cover <- as.numeric(SELECT_orphan$Non_cover_high)
SELECT_orphan <- subset(SELECT_orphan, ID != FALSE)




#convert units
#area
window_orphan[,"area_low"] <- "km^2" %#% window_orphan[,"area_low"] #converts to km^2 rather than m^2
window_orphan[, "area_est"] <- "km^2" %#% window_orphan[,"area_est"] #converts to km^2 rather than m^2
window_orphan[, "area_high"] <- "km^2" %#% window_orphan[,"area_high"] #converts to km^2 rather than m^2
#τposition
window_orphan[, "τposition_low"] <- "day" %#% window_orphan[,"τposition_low"] #converts to days rather than seconds
window_orphan[, "τposition_est"] <- "day" %#% window_orphan[,"τposition_est"] #converts to days rather than seconds
window_orphan[, "τposition_high"] <- "day" %#% window_orphan[,"τposition_high"] #converts to days rather than seconds
#τvelocity
window_orphan[, "τvelocity_low"] <- "minutes" %#% window_orphan[,"τvelocity_low"] #converts to minutes rather than seconds
window_orphan[, "τvelocity_est"] <- "minutes" %#% window_orphan[,"τvelocity_est"] #converts to minutes rather than seconds
window_orphan[, "τvelocity_high"] <- "minutes" %#% window_orphan[,"τvelocity_high"] #converts to minutes rather than seconds
#speed
window_orphan[, "speed_low"] <- "km/day" %#% window_orphan[, "speed_low"]#converts to km/day rather than m/s
window_orphan[, "speed_est"] <- "km/day" %#% window_orphan[, "speed_est"]#converts to km/day rather than m/s
window_orphan[, "speed_high"] <- "km/day" %#% window_orphan[, "speed_high"]#converts to km/day rather than m/s
#diffusion
window_orphan[, "diffusion_low"] <- "km^2/day" %#% window_orphan[, "diffusion_low"] #converts to km^2/day rather than m^2/s
window_orphan[, "diffusion_est"] <- "km^2/day" %#% window_orphan[, "diffusion_est"] #converts to km^2/day rather than m^2/s
window_orphan[, "diffusion_high"] <- "km^2/day" %#% window_orphan[, "diffusion_high"] #converts to km^2/day rather than m^2/s
#HR
window_orphan[,"HR_low"] <- "km^2" %#% window_orphan[,"HR_low"] #converts to km^2 rather than m^2
window_orphan[,"HR_est"] <- "km^2" %#% window_orphan[,"HR_est"] #converts to km^2 rather than m^2
window_orphan[,"HR_high"] <- "km^2" %#% window_orphan[,"HR_high"] #converts to km^2 rather than m^2
#mean speed
window_orphan[, "Mean_Speed_low"] <- "km/day" %#% window_orphan[, "Mean_Speed_low"]#converts to km/day rather than m/s
window_orphan[, "Mean_Speed_est"] <- "km/day" %#% window_orphan[, "Mean_Speed_est"]#converts to km/day rather than m/s
window_orphan[, "Mean_Speed_high"] <- "km/day" %#% window_orphan[, "Mean_Speed_high"]#converts to km/day rather than m/s


save(window_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Orphaned_df.rda")
save(SELECT_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Orphaned_select_df.rda")


