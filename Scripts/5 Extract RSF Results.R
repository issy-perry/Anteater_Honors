# 5 Extract RSF Results
# The main steps carried out by this script are:
#       1. Reorganize RSFs
#       2. Extract result output


#load packages
library(ctmm) #working with ctmm objects
library(dplyr) #working with dataframes


#load RSFs
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2019.rda") #2019 orphaned
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2020.rda") #2020 orphaned
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2021.rda") #2021 orphaned
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2022.rda") #2022 orphaned
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2023.rda") #2023 orphaned
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_wild_raised.rda") # wild-raised




#fixing RSF lists to remove years
RSF_orphan <- c(RSF_19, RSF_20, RSF_21, RSF_22, RSF_23)

#save output
save(RSF_orphan, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_orphan.rda")

#clear up environment space
rm(RSF_19, RSF_20, RSF_21, RSF_22, RSF_23)


#wild-raised -----------------------------------------
#create a dataframe to house total information
RSF_wild_all_df <- data.frame()

#for.loop for extracting RSF results
for(j in 1:length(RSF_wild_all)){
  
  #create a dataframe to house information for each individual
  RSF_extract <- data.frame()
  
  #extract summary output for an individual
  df <- data.frame(t(summary(RSF_wild_all[[j]], units = FALSE)[["CI"]]))
  
  #if value is available in summary output, extract it into the individual's dataframe. 
  #if value is not available in summary output, include an NA in the column
  for(i in 1:1) {
    #Agriculture
    if ("Agriculture..1.Agriculture." %in% colnames(df)) {
      RSF_extract [i, "Agriculture_low"] <- df["low", "Agriculture..1.Agriculture."]
    }else{
      RSF_extract[i, "Agriculture_low"] <- NA
    }
    if ("Agriculture..1.Agriculture." %in% colnames(df)) {
      RSF_extract [i, "Agriculture_est"] <- df["est", "Agriculture..1.Agriculture."]
    }else{
      RSF_extract[i, "Agriculture_est"] <- NA
    }
    if ("Agriculture..1.Agriculture." %in% colnames(df)) {
      RSF_extract [i, "Agriculture_high"] <- df["high", "Agriculture..1.Agriculture."]
    }else{
      RSF_extract[i, "Agriculture_high"] <- NA
    }
    #Development
    if ("Development..1.Development." %in% colnames(df)) {
      RSF_extract [i, "Development_low"] <- df["low", "Development..1.Development."]
    }else{
      RSF_extract[i, "Development_low"] <- NA
    }
    if ("Development..1.Development." %in% colnames(df)) {
      RSF_extract [i, "Development_est"] <- df["est", "Development..1.Development."]
    }else{
      RSF_extract[i, "Development_est"] <- NA
    }
    if ("Development..1.Development." %in% colnames(df)) {
      RSF_extract [i, "Development_high"] <- df["high", "Development..1.Development."]
    }else{
      RSF_extract[i, "Development_high"] <- NA
    }
    #Forestry
    if ("Forestry..1.Forestry." %in% colnames(df)) {
      RSF_extract [i, "Forestry_low"] <- df["low", "Forestry..1.Forestry."]
    }else{
      RSF_extract[i, "Forestry_low"] <- NA
    }
    if ("Forestry..1.Forestry." %in% colnames(df)) {
      RSF_extract [i, "Forestry_est"] <- df["est", "Forestry..1.Forestry."]
    }else{
      RSF_extract[i, "Forestry_est"] <- NA
    }
    if ("Forestry..1.Forestry." %in% colnames(df)) {
      RSF_extract [i, "Forestry_high"] <- df["high", "Forestry..1.Forestry."]
    }else{
      RSF_extract[i, "Forestry_high"] <- NA
    }
    #Native Forest
    if ("Native_Forest..1.Native_Forest." %in% colnames(df)) {
      RSF_extract [i, "Native_Forest_low"] <- df["low", "Native_Forest..1.Native_Forest."]
    }else{
      RSF_extract[i, "Native_Forest_low"] <- NA
    }
    if ("Native_Forest..1.Native_Forest." %in% colnames(df)) {
      RSF_extract [i, "Native_Forest_est"] <- df["est", "Native_Forest..1.Native_Forest."]
    }else{
      RSF_extract[i, "Native_Forest_est"] <- NA
    }
    if ("Native_Forest..1.Native_Forest." %in% colnames(df)) {
      RSF_extract [i, "Native_Forest_high"] <- df["high", "Native_Forest..1.Native_Forest."]
    }else{
      RSF_extract[i, "Native_Forest_high"] <- NA
    }
    #Pasture
    if ("Pasture..1.Pasture." %in% colnames(df)) {
      RSF_extract [i, "Pasture_low"] <- df["low", "Pasture..1.Pasture."]
    }else{
      RSF_extract[i, "Pasture_low"] <- NA
    }
    if ("Pasture..1.Pasture." %in% colnames(df)) {
      RSF_extract [i, "Pasture_est"] <- df["est", "Pasture..1.Pasture."]
    }else{
      RSF_extract[i, "Pasture_est"] <- NA
    }
    if ("Pasture..1.Pasture." %in% colnames(df)) {
      RSF_extract [i, "Pasture_high"] <- df["high", "Pasture..1.Pasture."]
    }else{
      RSF_extract[i, "Pasture_high"] <- NA
    }
    #Water
    if ("Water..1.Water." %in% colnames(df)) {
      RSF_extract [i, "Water_low"] <- df["low", "Water..1.Water."]
    }else{
      RSF_extract[i, "Water_low"] <- NA
    }
    if ("Water..1.Water." %in% colnames(df)) {
      RSF_extract [i, "Water_est"] <- df["est", "Water..1.Water."]
    }else{
      RSF_extract[i, "Water_est"] <- NA
    }
    if ("Water..1.Water." %in% colnames(df)) {
      RSF_extract [i, "Water_high"] <- df["high", "Water..1.Water."]
    }else{
      RSF_extract[i, "Water_high"] <- NA
    }} #end of inner loop

  #add ID of individual
  RSF_extract$ID <- RSF_wild_all[[j]]@info$identity
  
  #bind individual results to total dataframe
  RSF_wild_all_df <- rbind(RSF_wild_all_df, RSF_extract)
  
} #end of outer loop

#save output
save(RSF_wild_all_df, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/DF_RSF_wild.rda")




#orphan extraction ---------------------------------------------------
#create a dataframe to hold total results
RSF_orphan_all_df <- data.frame()

#for.loop for extracting RSF results
for(j in 1:length(RSF_orphan_all)){
  
  #create a dataframe to hold an individual's results
  RSF_extract <- data.frame()
  
  #extract summary output for an individual
  df <- data.frame(t(summary(RSF_orphan_all[[j]], units = FALSE)[["CI"]]))
  #if value is available in summary output, extract it into the individual's dataframe. 
  #if value is not available in summary output, include an NA in the column
  for(i in 1:1) {
    #Agriculture
    if ("Agriculture..1.Agriculture." %in% colnames(df)) {
      RSF_extract [i, "Agriculture_low"] <- df["low", "Agriculture..1.Agriculture."]
    }else{
      RSF_extract[i, "Agriculture_low"] <- NA
    }
    if ("Agriculture..1.Agriculture." %in% colnames(df)) {
      RSF_extract [i, "Agriculture_est"] <- df["est", "Agriculture..1.Agriculture."]
    }else{
      RSF_extract[i, "Agriculture_est"] <- NA
    }
    if ("Agriculture..1.Agriculture." %in% colnames(df)) {
      RSF_extract [i, "Agriculture_high"] <- df["high", "Agriculture..1.Agriculture."]
    }else{
      RSF_extract[i, "Agriculture_high"] <- NA
    }
    #Development
    if ("Development..1.Development." %in% colnames(df)) {
      RSF_extract [i, "Development_low"] <- df["low", "Development..1.Development."]
    }else{
      RSF_extract[i, "Development_low"] <- NA
    }
    if ("Development..1.Development." %in% colnames(df)) {
      RSF_extract [i, "Development_est"] <- df["est", "Development..1.Development."]
    }else{
      RSF_extract[i, "Development_est"] <- NA
    }
    if ("Development..1.Development." %in% colnames(df)) {
      RSF_extract [i, "Development_high"] <- df["high", "Development..1.Development."]
    }else{
      RSF_extract[i, "Development_high"] <- NA
    }
    #Forestry
    if ("Forestry..1.Forestry." %in% colnames(df)) {
      RSF_extract [i, "Forestry_low"] <- df["low", "Forestry..1.Forestry."]
    }else{
      RSF_extract[i, "Forestry_low"] <- NA
    }
    if ("Forestry..1.Forestry." %in% colnames(df)) {
      RSF_extract [i, "Forestry_est"] <- df["est", "Forestry..1.Forestry."]
    }else{
      RSF_extract[i, "Forestry_est"] <- NA
    }
    if ("Forestry..1.Forestry." %in% colnames(df)) {
      RSF_extract [i, "Forestry_high"] <- df["high", "Forestry..1.Forestry."]
    }else{
      RSF_extract[i, "Forestry_high"] <- NA
    }
    #Native Forest
    if ("Native_Forest..1.Native_Forest." %in% colnames(df)) {
      RSF_extract [i, "Native_Forest_low"] <- df["low", "Native_Forest..1.Native_Forest."]
    }else{
      RSF_extract[i, "Native_Forest_low"] <- NA
    }
    if ("Native_Forest..1.Native_Forest." %in% colnames(df)) {
      RSF_extract [i, "Native_Forest_est"] <- df["est", "Native_Forest..1.Native_Forest."]
    }else{
      RSF_extract[i, "Native_Forest_est"] <- NA
    }
    if ("Native_Forest..1.Native_Forest." %in% colnames(df)) {
      RSF_extract [i, "Native_Forest_high"] <- df["high", "Native_Forest..1.Native_Forest."]
    }else{
      RSF_extract[i, "Native_Forest_high"] <- NA
    }
    #Pasture
    if ("Pasture..1.Pasture." %in% colnames(df)) {
      RSF_extract [i, "Pasture_low"] <- df["low", "Pasture..1.Pasture."]
    }else{
      RSF_extract[i, "Pasture_low"] <- NA
    }
    if ("Pasture..1.Pasture." %in% colnames(df)) {
      RSF_extract [i, "Pasture_est"] <- df["est", "Pasture..1.Pasture."]
    }else{
      RSF_extract[i, "Pasture_est"] <- NA
    }
    if ("Pasture..1.Pasture." %in% colnames(df)) {
      RSF_extract [i, "Pasture_high"] <- df["high", "Pasture..1.Pasture."]
    }else{
      RSF_extract[i, "Pasture_high"] <- NA
    }
    #Water
    if ("Water..1.Water." %in% colnames(df)) {
      RSF_extract [i, "Water_low"] <- df["low", "Water..1.Water."]
    }else{
      RSF_extract[i, "Water_low"] <- NA
    }
    if ("Water..1.Water." %in% colnames(df)) {
      RSF_extract [i, "Water_est"] <- df["est", "Water..1.Water."]
    }else{
      RSF_extract[i, "Water_est"] <- NA
    }
    if ("Water..1.Water." %in% colnames(df)) {
      RSF_extract [i, "Water_high"] <- df["high", "Water..1.Water."]
    }else{
      RSF_extract[i, "Water_high"] <- NA
    }} #end of inner loop
  
  #add individual's ID
  RSF_extract$ID <- RSF_orphan_all[[j]]@info$identity
  
  #add individual dataframe to total dataframe
  RSF_orphan_all_df <- rbind(RSF_orphan_all_df, RSF_extract) 
  
} #end of outer loop


#save output
save(RSF_orphan_all_df, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/DF_RSF_orphan.rda")



