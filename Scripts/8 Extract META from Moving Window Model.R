#load packages
library(ctmm)
library(stringr) #extract letters of list name
#load telemetry data for names


#load comparison data of population average
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Wild_raised/Lists/AKDE_wild_total.rda") #wild-raised HRS
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Wild_raised/Lists/FITS_wild_total.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/Speed/Speed_wild_raised.rda")


#remove fractal models from speed
SPEED_wild <- SPEED_wild[-c(8,26)]

fits_folder_o = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Fits/Orphan"
fits_folder_w = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Fits/Wild_raised"
akde_folder_o = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/AKDEs/Orphan"
akde_folder_w = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/AKDEs/Wild_raised"
speed_folder_o = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Mean_Speed/Orphan"


#success and unsuccess
#fits_folder_s = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Fits/Orphan/SUCCESS"
#akde_folder_s = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/AKDEs/Orphan/SUCCESS"
#fits_folder_u = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/Fits/Orphan/UNSUCCESS"
#akde_folder_u = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/AKDEs/Orphan/UNSUCCESS"



#total orphaned population--------------------------------------------------------------------------------
#Fits
#make file path of orphans
fits_o <- list.files(path = fits_folder_o,
                     pattern = NULL,
                     full.names = TRUE)
#import fits for orphans
FITS_orphan <- lapply(fits_o, function(cur_file) {
  load(cur_file)
  return(FITS)
})
#rename the lists based on identity rather than another named list as these are now alphabetical
for(i in 1:length(FITS_orphan)) {
  FITS_ID <- FITS_orphan[[i]]
  names(FITS_orphan)[i] <- FITS_ID[[1]]@info$identity
}

#AKDEs
#make file path of orphans
akde_o <- list.files(path = akde_folder_o,
                     pattern = NULL,
                     full.names = TRUE)
#import fits for orphans
AKDE_orphan <- lapply(akde_o, function(cur_file) {
  load(cur_file)
  return(wAKDEs)
})
#rename the lists based on identity rather than another named list as these are now alphabetical
for(i in 1:length(AKDE_orphan)) {
  AKDE_ID <- AKDE_orphan[[i]]
  names(AKDE_orphan)[i] <- AKDE_ID[[1]]@info$identity
}



#also import total wild-raised population if you want to compare individual windows against windows
#fits
#make file path of wild-raised
fits_w <- list.files(path = fits_folder_w,
                     pattern = NULL,
                     full.names = TRUE)
#import fits for orphans
FITS_wild <- lapply(fits_w, function(cur_file) {
  load(cur_file)
  return(FITS)
})
#rename the lists based on identity rather than another named list as these are now alphabetical
for(i in 1:length(FITS_wild)) {
  FITS_ID <- FITS_wild[[i]]
  names(FITS_wild)[i] <- FITS_ID[[1]]@info$identity
}

#AKDEs
#make file path of wild-raised individuals
akde_w <- list.files(path = akde_folder_w,
                     pattern = NULL,
                     full.names = TRUE)
#import fits for orphans
AKDE_wild <- lapply(akde_w, function(cur_file) {
  load(cur_file)
  return(wAKDEs)
})
#rename the lists based on identity rather than another named list as these are now alphabetical
for(i in 1:length(AKDE_wild)) {
  AKDE_ID <- AKDE_wild[[i]]
  names(AKDE_wild)[i] <- AKDE_ID[[1]]@info$identity
}







# TOTAL POPULATION

#create a dataframe to house the information
META_win_orphan <- data.frame()
#META_win_orphan <- as.data.frame(matrix(0, ncol = 12, nrow = 135))
for(j in 1:135){
  
  #make list to hold extracted windows
  # fits_win <- list()
  fits_win <- list()
  for(i in 1:length(FITS_orphan)){
      IND <- FITS_orphan[[i]] # pull out individual 
      
      if(j <= length(IND)) {fits_win[[length(fits_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
# end of trycatch
  }# end of first inner loop
  
  #extract AKDEs
  #akde_win <- vector(mode = "list", length = length(AKDE_orphan))
  akde_win <- list()
  for(i in 1:length(AKDE_orphan)){
      IND <- AKDE_orphan[[i]] # pull out individual 
      
      if(j <= length(IND)) {akde_win[[length(akde_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
  }# end of first inner loop
  
  
  akde_wild <- list()
  for(h in 1:length(AKDE_wild)){
    IND <- AKDE_wild[[i]] #pull out individual
    
    if(j <= length(IND)) {akde_wild[[length(akde_wild)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    
  }
  
  speed_win <- list()
  for(i in 1:length(SPEED_orphan)){
    IND <- SPEED_orphan[[i]] # pull out individual 
    
    if(j <= length(IND)) {speed_win[[length(speed_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
  }# end of first inner loop
  
  
  #home range
  HR <- ctmm::meta(list(orphans = akde_win, wild = akde_wild), variable = "area", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  HR_o <- as.data.frame(HR)
  HR_o$HR_low <- HR_o["orphans/", "/wild.low"]
  HR_o$HR_est <- HR_o["orphans/", "/wild.est"]
  HR_o$HR_high <- HR_o["orphans/", "/wild.high"]
  HR_o <- HR_o[-c(2), -c(1,2,3,4,5,6)] #removes column 2 and first six rows

  
  #diffusion
  DIFFUSION <- ctmm::meta(list(orphans = fits_win, wild = FITS_wild), variable = "diffusion", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  DIFFUSION_o <- as.data.frame(DIFFUSION)
  DIFFUSION_o$diffusion_low <- DIFFUSION_o["orphans/", "/wild.low"]
  DIFFUSION_o$diffusion_est <- DIFFUSION_o["orphans/", "/wild.est"]
  DIFFUSION_o$diffusion_high <- DIFFUSION_o["orphans/", "/wild.high"]
  DIFFUSION_o <- DIFFUSION_o[-c(2), -c(1,2,3,4,5,6)] #removes row two and first six columns 


  
  #make total dataframe
  META_o <- cbind(DIFFUSION_o, HR_o)

  META_win_orphan <- rbind(META_win_orphan, META_o) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe

  }#end of outer loop

META_win_orphan$time_since_release = seq(2, by = 2, length.out = nrow(META_o_df)) #days since release for plotting

#save output
save(META_win_orphan, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Total_meta.rda")





# SUCCESS VS UNSUCCESS ---------------------------------------------------------------------------------------------------

#subset based on whether rehabilitation was successful or not
SUCCESS_FIT <- FITS_orphan[c(3,4,5,12,14,15,16)]
SUCCESS_AKDE <- AKDE_orphan[c(3,4,5,12,14,15,16)]
UNSUCCESS_FIT <- FITS_orphan[c(1,2,6,7,8,9,10,11,13,17,18,19,20)]
UNSUCCESS_AKDE <- AKDE_orphan[c(1,2,6,7,8,9,10,11,13,17,18,19,20)]

#create a dataframe to house the information


#META_win_orphan <- as.data.frame(matrix(0, ncol = 12, nrow = 135))
META_o_suc_df <- data.frame()
for(j in 1:101){
  
  #make list to hold extracted windows
  # fits_win <- list()
  fits_win <- list()
  for(i in 1:length(SUCCESS_FIT)){
    IND <- SUCCESS_FIT[[i]] # pull out individual 
    
    if(j <= length(IND)) {fits_win[[length(fits_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    # end of trycatch
  }# end of first inner loop
  
  #extract AKDEs
  #akde_win <- vector(mode = "list", length = length(AKDE_orphan))
  akde_win <- list()
  for(i in 1:length(SUCCESS_AKDE)){
    IND <- SUCCESS_AKDE[[i]] # pull out individual 
    
    if(j <= length(IND)) {akde_win[[length(akde_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
  }# end of first inner loop
  
  
  akde_wild <- list()
  for(h in 1:length(AKDE_wild)){
    IND <- AKDE_wild[[i]] #pull out individual
    
    if(j <= length(IND)) {akde_wild[[length(akde_wild)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    
  }
  

  
  #home range
  HR <- ctmm::meta(list(orphans = akde_win, wild = akde_wild), variable = "area", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  HR_o <- as.data.frame(HR)
  HR_o$HR_low <- HR_o["orphans/", "/wild.low"]
  HR_o$HR_est <- HR_o["orphans/", "/wild.est"]
  HR_o$HR_high <- HR_o["orphans/", "/wild.high"]
  HR_o <- HR_o[-c(2), -c(1,2,3,4,5,6)] #removes column 2 and first six rows
  
  #diffusion
  DIFFUSION <- ctmm::meta(list(orphans = fits_win, wild = FITS_wild), variable = "diffusion", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  DIFFUSION_o <- as.data.frame(DIFFUSION)
  DIFFUSION_o$diffusion_low <- DIFFUSION_o["orphans/", "/wild.low"]
  DIFFUSION_o$diffusion_est <- DIFFUSION_o["orphans/", "/wild.est"]
  DIFFUSION_o$diffusion_high <- DIFFUSION_o["orphans/", "/wild.high"]
  DIFFUSION_o <- DIFFUSION_o[-c(2), -c(1,2,3,4,5,6)] #removes row two and first six columns 

  
  
  #make total dataframe
  META_s <- cbind(DIFFUSION_o, HR_o)
  META_o_suc_df <- rbind(META_o_suc_df, META_s) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe

}#end of outer loop
META_o_suc_df$time_since_release = seq(2, by = 2, length.out = nrow(META_o_suc_df)) #days since release for plotting







#META_win_orphan <- as.data.frame(matrix(0, ncol = 12, nrow = 135))
META_o_unsuc_df<- data.frame()
for(j in 1:135){
  
  #make list to hold extracted windows
  # fits_win <- list()
  fits_win <- list()
  for(i in 1:length(UNSUCCESS_FIT)){
    IND <- UNSUCCESS_FIT[[i]] # pull out individual 
    
    if(j <= length(IND)) {fits_win[[length(fits_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    # end of trycatch
  }# end of first inner loop
  
  #extract AKDEs
  #akde_win <- vector(mode = "list", length = length(AKDE_orphan))
  akde_win <- list()
  for(i in 1:length(UNSUCCESS_AKDE)){
    IND <- UNSUCCESS_AKDE[[i]] # pull out individual 
    
    if(j <= length(IND)) {akde_win[[length(akde_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
  }# end of first inner loop
  
  
  akde_wild <- list()
  for(h in 1:length(AKDE_wild)){
    IND <- AKDE_wild[[i]] #pull out individual
    
    if(j <= length(IND)) {akde_wild[[length(akde_wild)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    
  }
  
  
  
  #home range
  HR <- ctmm::meta(list(orphans = akde_win, wild = akde_wild), variable = "area", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  HR_o <- as.data.frame(HR)
  HR_o$HR_low <- HR_o["orphans/", "/wild.low"]
  HR_o$HR_est <- HR_o["orphans/", "/wild.est"]
  HR_o$HR_high <- HR_o["orphans/", "/wild.high"]
  HR_o <- HR_o[-c(2), -c(1,2,3,4,5,6)] #removes column 2 and first six rows

  
  #diffusion
  DIFFUSION <- ctmm::meta(list(orphans = fits_win, wild = FITS_wild), variable = "diffusion", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  DIFFUSION_o <- as.data.frame(DIFFUSION)
  DIFFUSION_o$diffusion_low <- DIFFUSION_o["orphans/", "/wild.low"]
  DIFFUSION_o$diffusion_est <- DIFFUSION_o["orphans/", "/wild.est"]
  DIFFUSION_o$diffusion_high <- DIFFUSION_o["orphans/", "/wild.high"]
  DIFFUSION_o <- DIFFUSION_o[-c(2), -c(1,2,3,4,5,6)] #removes row two and first six columns 

  
  
  #make total dataframe
  META_s <- cbind(DIFFUSION_o, HR_o)
  META_o_unsuc_df <- rbind(META_o_unsuc_df, META_s) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe
  
}#end of outer loop
META_o_unsuc_df$time_since_release = seq(2, by = 2, length.out = nrow(META_o_unsuc_df)) #days since release for plotting

#save outputs
save(META_o_suc_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Successful_meta.rda")
save(META_o_unsuc_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Unsuccessful_meta.rda")








#RANGE_RESIDENTS VS DISPERSERS--------------------------------------------------------------------------------------------------------------------------
#subset based on if individuals were dispersers or range-residents
#separate dispersers and range-residents
FITS_dis_o <- FITS_orphan[c(2,3,4,9,14,15,16,17,19)]
FITS_dis_w <- FITS_wild[c(24,25,26)]
FITS_RR_o <- FITS_orphan[c(1,5,6,7,8,10,11,12,13,18,20)]
FITS_RR_w <- FITS_wild[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
AKDE_dis_o <- AKDE_orphan[c(2,3,4,9,14,15,16,17,19)]
AKDE_dis_w <- AKDE_wild[c(24,25,26)]
AKDE_RR_o <- AKDE_orphan[c(1,5,6,7,8,10,11,12,13,18,20)]
AKDE_RR_w <- AKDE_wild[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]



#create a dataframe to house the information
META_dis_df <- data.frame()
META_RR_df <- data.frame()


#dispersers
#META_win_orphan <- as.data.frame(matrix(0, ncol = 12, nrow = 135))
for(j in 1:101){
  
  #make list to hold extracted windows
  # fits_win <- list()
  fits_o_win <- list()
  for(i in 1:length(FITS_dis_o)){
    IND <- FITS_dis_o[[i]] # pull out individual 
    
    if(j <= length(IND)) {fits_o_win[[length(fits_o_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    # end of trycatch
  }# end of first inner loop
  
  #extract AKDEs
  #akde_win <- vector(mode = "list", length = length(AKDE_orphan))
  akde_o_win <- list()
  for(i in 1:length(AKDE_dis_o)){
    IND <- AKDE_dis_o[[i]] # pull out individual 
    
    if(j <= length(IND)) {akde_o_win[[length(akde_o_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
  }# end of first inner loop
  
  

  
  
  
  
  #home range
  HR <- ctmm::meta(list(orphans = akde_o_win, wild = AKDE_dis_w), variable = "area", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  HR_o <- as.data.frame(HR)
  HR_o$HR_low <- HR_o["orphans/", "/wild.low"]
  HR_o$HR_est <- HR_o["orphans/", "/wild.est"]
  HR_o$HR_high <- HR_o["orphans/", "/wild.high"]
  HR_o <- HR_o[-c(2), -c(1,2,3,4,5,6)] #removes column 2 and first six rows

  
  #diffusion
  DIFFUSION <- ctmm::meta(list(orphans = fits_o_win, wild = FITS_dis_w), variable = "diffusion", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  DIFFUSION_o <- as.data.frame(DIFFUSION)
  DIFFUSION_o$diffusion_low <- DIFFUSION_o["orphans/", "/wild.low"]
  DIFFUSION_o$diffusion_est <- DIFFUSION_o["orphans/", "/wild.est"]
  DIFFUSION_o$diffusion_high <- DIFFUSION_o["orphans/", "/wild.high"]
  DIFFUSION_o <- DIFFUSION_o[-c(2), -c(1,2,3,4,5,6)] #removes row two and first six columns 

  
  
  #make total dataframe
  META_dis <- cbind(DIFFUSION_o, HR_o)
  META_dis_df <- rbind(META_dis_df, META_dis) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe

}#end of outer loop

META_dis_df$time_since_release = seq(2, by = 2, length.out = nrow(META_dis_df)) #days since release for plotting



META_RR_df <- data.frame()


#dispersers
#META_win_orphan <- as.data.frame(matrix(0, ncol = 12, nrow = 135))
for(j in 1:101){
  
  #make list to hold extracted windows
  # fits_win <- list()
  fits_o_win <- list()
  for(i in 1:length(FITS_RR_o)){
    IND <- FITS_RR_o[[i]] # pull out individual 
    
    if(j <= length(IND)) {fits_o_win[[length(fits_o_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
    # end of trycatch
  }# end of first inner loop
  
  #extract AKDEs
  #akde_win <- vector(mode = "list", length = length(AKDE_orphan))
  akde_o_win <- list()
  for(i in 1:length(AKDE_RR_o)){
    IND <- AKDE_RR_o[[i]] # pull out individual 
    
    if(j <= length(IND)) {akde_o_win[[length(akde_o_win)+1]] <- IND[[j]]} # pull out window and stick it into the next slot
  }# end of first inner loop
  
  
  
  
  
  
  
  #home range
  HR <- ctmm::meta(list(orphans = akde_o_win, wild = AKDE_dis_w), variable = "area", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  HR_o <- as.data.frame(HR)
  HR_o$HR_low <- HR_o["orphans/", "/wild.low"]
  HR_o$HR_est <- HR_o["orphans/", "/wild.est"]
  HR_o$HR_high <- HR_o["orphans/", "/wild.high"]
  HR_o <- HR_o[-c(2), -c(1,2,3,4,5,6)] #removes column 2 and first six rows
  
  
  #diffusion
  DIFFUSION <- ctmm::meta(list(orphans = fits_o_win, wild = FITS_dis_w), variable = "diffusion", level = 0.95, units = FALSE) #units don't matter, it's a ratio
  DIFFUSION_o <- as.data.frame(DIFFUSION)
  DIFFUSION_o$diffusion_low <- DIFFUSION_o["orphans/", "/wild.low"]
  DIFFUSION_o$diffusion_est <- DIFFUSION_o["orphans/", "/wild.est"]
  DIFFUSION_o$diffusion_high <- DIFFUSION_o["orphans/", "/wild.high"]
  DIFFUSION_o <- DIFFUSION_o[-c(2), -c(1,2,3,4,5,6)] #removes row two and first six columns 
  
  
  
  #make total dataframe
  META_RR <- cbind(DIFFUSION_o, HR_o)
  META_RR_df <- rbind(META_RR_df, META_RR) #stringAsFactors argument allows us to just keep adding the new windows to the dataframe
  
}#end of outer loop

META_RR_df$time_since_release = seq(2, by = 2, length.out = nrow(META_RR_df)) #days since release for plotting


#save outputs
save(META_dis_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Disperser_meta.rda")
save(META_RR_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/RR_meta.rda")




























# ADDING SPEED LATER----------------------------------------------------------------------
#make file path of orphans
speed_o <- list.files(path = speed_folder_o,
                      pattern = NULL,
                      full.names = TRUE)
#import fits for orphans
SPEED_orphan <- lapply(speed_o, function(cur_file) {
  load(cur_file)
  return(speed_mean)
})
#rename the lists based on identity rather than another named list as these are now alphabetical
for(i in 1:length(SPEED_orphan)) {
  SPEED_ID <- SPEED_orphan[[i]]
  names(SPEED_orphan)[i] <- substr(names(SPEED_ID)[1], 1,6)
}



rm(AKDE_ID, FITS_ID, SPEED_ID)


speed_win <- list()
for(g in 1:length(SPEED_orphan)){
  tryCatch({
    IND <- SPEED_orphan[[g]] # pull out individual 
    
    TEST <- IND[[j]] # pull out window
    #speed_win <- append(speed_win, TEST)
    speed_win[[g]] <- TEST
  }, error = function(e){ 
    cat("Individual has no more windows", g, "-", e$message, "\n")
  }) # end of trycatch
}# end of first inner loop
