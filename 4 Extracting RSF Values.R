#load packages
library(ctmm) #movement
library(dplyr)
library(ggplot2) #making plots
library(ggpubr) #arranging plots in multiples



#load RSFs
load("~/Giant_Anteater_Orphan/Results/RSFs/Projection_Fix_CORRECT/RSF_2017.rda")
load("~/Giant_Anteater_Orphan/Results/RSFs/Projection_Fix_CORRECT/RSF_2018.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2019.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2020.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2021.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2022.rda")
load("~/Giant_Anteater_Orphan/FIXED/Data/Orphan/TOTAL_Data.rda")
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda")




#RSFs----------
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2017.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2017_sERGE.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2018.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/DELPHINE.rda")


#separate wild into just subadults for all
RSF_wild_all <- list()
RSF_wild_all <- append(RSF_wild_all, NEW_RSF_17)
RSF_wild_all <- append(RSF_wild_all, NEW_RSF_18)
RSF_wild_all <- append(RSF_wild_all, RSF_17)
RSF_wild_all <- append(RSF_wild_all, RSF_18)
RSF_wild_all <- RSF_wild_all[-c(1,2)] #remove extras

#fixing RSF lists to remove years
RSF_orphan <- list(RSF_19[[1]], RSF_19[[2]], RSF_19[[3]], RSF_22[[1]], RSF_22[[2]], RSF_21[[1]], RSF_21[[2]], RSF_21[[3]], RSF_22[[3]], RSF_20[[1]], RSF_21[[4]])
names(RSF_orphan) <- names(DATA_list)
save(RSF_orphan, file = "~/Giant_Anteater_Orphan/Results/RSFs/Projection_Fix/RSF_orphan.rda")
RSF_wild <- list(RSF_18[[1]], RSF_18[[2]], RSF_17[[1]], RSF_18[[3]], RSF_17[[2]], RSF_17[[3]], RSF_17[[4]], RSF_17[[5]], RSF_18[[4]], RSF_17[[6]], RSF_18[[5]], 
                  RSF_17[[7]], RSF_18[[6]], RSF_17[[8]], RSF_18[[7]], RSF_17[[9]], RSF_18[[8]], RSF_18[[9]], RSF_17[[10]], RSF_18[[10]], RSF_18[[11]], RSF_18[[12]], RSF_18[[13]])
names(RSF_wild) <- names(DATA_TELEMETRY)
save(RSF_wild, file = "~/Giant_Anteater_Orphan/Results/RSFs/Projection_Fix/RSF_wild.rda")
rm(RSF_17, RSF_18, RSF_19, RSF_20, RSF_21, RSF_22)


mean(RSF_wild)
mean(RSF_orphan)



#wild-raised -----------------------------------------

RSF_wild_all_df <- data.frame()
for(j in 1:length(RSF_wild_all)){
  RSF_extract <- data.frame()
  df <- data.frame(t(summary(RSF_wild_all[[j]], units = FALSE)[["CI"]]))
  for(i in 1:1) {
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
    }}
  RSF_extract$ID <- RSF_wild_all[[j]]@info$identity
  RSF_wild_all_df <- rbind(RSF_wild_all_df, RSF_extract, stringAsFactors = FALSE) #stringAsFactors arguement allows us to just keep adding the new windows to the dataframe
  
} #end of outer loop
RSF_wild_all_df <- RSF_wild_all_df[!(RSF_wild_all_df$ID %in% "FALSE"),]

RSF_wild_all_df <- RSF_wild_all_df[-c(1),]
RSF_wild_all_df <- RSF_wild_all_df[-c(26),]

#save output
save(RSF_wild_all_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_wild_all_covariates_df.rda")

#orphan extraction ---------------------------------------------------
RSF_orphan_all_df <- data.frame()
for(j in 1:length(RSF_orphan_all)){
  RSF_extract <- data.frame()
  df <- data.frame(t(summary(RSF_orphan_all[[j]], units = FALSE)[["CI"]]))
  for(i in 1:1) {
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
  }}
  RSF_extract$ID <- RSF_orphan_all[[j]]@info$identity
  RSF_orphan_all_df <- rbind(RSF_orphan_all_df, RSF_extract, stringAsFactors = FALSE) #stringAsFactors arguement allows us to just keep adding the new windows to the dataframe
  
} #end of outer loop
RSF_orphan_all_df <- RSF_orphan_all_df[!(RSF_orphan_all_df$ID %in% "FALSE"),]
#save output
save(RSF_orphan_all_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_orphan_all_df.rda")



#extract cover/no cover
RSF_wild_cover_df <- data.frame()
for(j in 1:length(RSF_wild_cover)){
  SELECT <- RSF_wild_cover[[j]]
  RSF_extract <- data.frame()
  summary_df <- data.frame(t(summary(SELECT, units = FALSE)[["CI"]]))
  for(i in 1:1) {
  if ("cover..1.cover." %in% colnames(summary_df)) {
    RSF_extract[i, "Cover_low"] <- summary_df["low", "cover..1.cover."]
  }else{
    RSF_extract[i, "Cover_low"] <- NA
  }
  if ("cover..1.cover." %in% colnames(summary_df)) {
    RSF_extract[i, "Cover_est"] <- summary_df["est", "cover..1.cover."]
  }else{
    RSF_extract[i, "Cover_est"] <- NA
  }
  if ("cover..1.cover." %in% colnames(summary_df)) {
    RSF_extract[i, "Cover_high"] <- summary_df["high", "cover..1.cover."]
  }else{
    RSF_extract[i, "Cover_high"] <- NA
  }
  if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
    RSF_extract[i, "Non_cover_low"] <- summary_df["low", "non_cover..1.non_cover."]
  }else{
    RSF_extract[i, "Non_cover_low"] <- NA
  }
  if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
    RSF_extract[i, "Non_cover_est"] <- summary_df["est", "non_cover..1.non_cover."]
  }else{
    RSF_extract[i, "Non_cover_est"] <- NA
  }
  if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
    RSF_extract[i, "Non_cover_high"] <- summary_df["high", "non_cover..1.non_cover."]
  }else{
    RSF_extract[i, "Non_cover_high"] <- NA
  }
  }
  RSF_extract$ID <- RSF_wild_cover[[j]]@info$identity
  RSF_wild_cover_df <- rbind(RSF_wild_cover_df, RSF_extract)
} #end of inner loop
RSF_wild_cover_df <- RSF_wild_cover_df[-c(1),]
RSF_wild_cover_df <- RSF_wild_cover_df[-c(26),]


save(RSF_wild_cover_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_wild_cover_df.rda")

RSF_orphan_cover_df <- data.frame()
for(j in 1:length(RSF_orphan_cover)){
  SELECT <- RSF_orphan_cover[[j]]
  RSF_extract <- data.frame()
  summary_df <- data.frame(t(summary(SELECT, units = FALSE)[["CI"]]))
  for(i in 1:1) {
    if ("cover..1.cover." %in% colnames(summary_df)) {
      RSF_extract[i, "Cover_low"] <- summary_df["low", "cover..1.cover."]
    }else{
      RSF_extract[i, "Cover_low"] <- NA
    }
    if ("cover..1.cover." %in% colnames(summary_df)) {
      RSF_extract[i, "Cover_est"] <- summary_df["est", "cover..1.cover."]
    }else{
      RSF_extract[i, "Cover_est"] <- NA
    }
    if ("cover..1.cover." %in% colnames(summary_df)) {
      RSF_extract[i, "Cover_high"] <- summary_df["high", "cover..1.cover."]
    }else{
      RSF_extract[i, "Cover_high"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      RSF_extract[i, "Non_cover_low"] <- summary_df["low", "non_cover..1.non_cover."]
    }else{
      RSF_extract[i, "Non_cover_low"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      RSF_extract[i, "Non_cover_est"] <- summary_df["est", "non_cover..1.non_cover."]
    }else{
      RSF_extract[i, "Non_cover_est"] <- NA
    }
    if ("non_cover..1.non_cover." %in% colnames(summary_df)) {
      RSF_extract[i, "Non_cover_high"] <- summary_df["high", "non_cover..1.non_cover."]
    }else{
      RSF_extract[i, "Non_cover_high"] <- NA
    }
  }
  RSF_extract$ID <- RSF_orphan_cover[[j]]@info$identity
  RSF_orphan_cover_df <- rbind(RSF_orphan_cover_df, RSF_extract)
} #end of inner loop


save(RSF_orphan_cover_df, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_orphan_cover_df.rda")
