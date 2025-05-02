# 0 Data Cleaning Script
#This script includes the following:
#     1. Cleaning data
#     2. Converting to a ctmm telemetry object
#     3. Estimating UEREs from mortality information


#load packages
library(ctmm) #converting data into a telemetry object
library(tidyverse) #formatting of tbl for Stefano's functions
library(sf)
library(lubridate) #included in the structure of Stefano's functions
library(mapview) #included in the structure of Stefano's functions

#loud source functions that Stefano Mezzini created (see his GitHub repo for functions)
source("~/Giant_Anteater_Orphan/Functions_Stefano/plot_adj.R") # to plot 20 adjacent locations
source("~/Giant_Anteater_Orphan/Functions_Stefano/flag_outlier.R") # to mark outliers
source("~/Giant_Anteater_Orphan/Functions_Stefano/remove_outlier_flags.R") # to start over with an animal
source("~/Giant_Anteater_Orphan/Functions_Stefano/outlier_plots.R") # to plot outlier diagnostic plots
source("~/Giant_Anteater_Orphan/Functions_Stefano/check_animal.R") # to run diagnostic plots

#determine current working directory
getwd()

#import data into R
#we are skipping the first 22 rows because this contains no GPS data and messes up importation
Arya_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/709199A_Datalog.csv", skip = 22)
Bahia_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Bahia/729088A_1_Datalog.csv", skip = 22)
Beezie_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Beezie/729090A_1_Datalog.csv", skip = 22)
Bella_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Bella/729093A_1_Datalog.csv", skip = 22)
Capitu_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/709201A_Datalog.csv", skip = 22)
Cláudio_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Cláudio/716785A_2_Datalog.csv", skip = 22)
Colete_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Tim_Colete_2/724183A_1_Datalog.csv", skip = 22)
Dom_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Dom/736078A_1_Datalog.csv", skip = 22)
Dumbo_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/709203A_Datalog.csv", skip = 22)
Erick_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Erick/729102A_1_Datalog.csv", skip = 22)
George_gps <- read.csv("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/George/729106A_1_Datalog.csv", skip = 22)
Heather_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/Heather.csv", skip = 22)
Jacobina_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Jacobina/736260A_1_Datalog.csv", skip = 22)
Juju_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/687743B_Datalog.csv", skip = 22)
Mulan_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Mulan/716786A_1_Datalog.csv", skip = 22)
Nayeli_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Nayeli/729091A_1_Datalog.csv", skip = 22)
Peter_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Peter/687745B_1_Datalog.csv", skip = 22)
Renee_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/687746B_Datalog.csv", skip = 22)
Tim_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/tim.csv", skip = 22)
Tim2_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Tim_Colete_2/724183A_1_Datalog.csv", skip = 22)
RandN_gps <-read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Rita_e_Nancy/729101A_1_Datalog.csv", skip = 22)

#remove NA values using the GPS Latitude column (no succesful fix should have an NA for location)
Arya_gps <- Arya_gps [!is.na(Arya_gps$GPS.Latitude),]
Bahia_gps <- Bahia_gps [!is.na(Bahia_gps$GPS.Latitude),]
Beezie_gps <- Beezie_gps [!is.na(Beezie_gps$GPS.Latitude),]
Bella_gps <- Bella_gps [!is.na(Bella_gps$GPS.Latitude),]
Capitu_gps <- Capitu_gps [!is.na(Capitu_gps$GPS.Latitude),]
Cláudio_gps <- Cláudio_gps [!is.na(Cláudio_gps$GPS.Latitude),]
Colete_gps <- Colete_gps [!is.na(Colete_gps$GPS.Latitude),]
Dom_gps <- Dom_gps [!is.na(Dom_gps$GPS.Latitude),]
Dumbo_gps <- Dumbo_gps [!is.na(Dumbo_gps$GPS.Latitude),]
Erick_gps <- Erick_gps [!is.na(Erick_gps$GPS.Latitude),]
George_gps <- George_gps [!is.na(George_gps$GPS.Latitude),]
Heather_gps <- Heather_gps [!is.na(Heather_gps$GPS.Latitude),]
Jacobina_gps <- Jacobina_gps [!is.na(Jacobina_gps$GPS.Latitude),]
Juju_gps <- Juju_gps [!is.na(Juju_gps$GPS.Latitude),]
Mulan_gps <- Mulan_gps [!is.na(Mulan_gps$GPS.Latitude),]
Nayeli_gps <- Nayeli_gps [!is.na(Nayeli_gps$GPS.Latitude),]
Peter_gps <- Peter_gps [!is.na(Peter_gps$GPS.Latitude),]
Renee_gps <- Renee_gps [!is.na(Renee_gps$GPS.Latitude),]
RandN_gps <- RandN_gps [!is.na(RandN_gps$GPS.Latitude),]
Tim_gps <- Tim_gps [!is.na(Tim_gps$GPS.Latitude),]
Tim2_gps <- Tim2_gps [!is.na(Tim2_gps$GPS.Latitude),]

#format time stamp
Arya_gps$GPS.Fix.Time <- as.POSIXct(Arya_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Bahia_gps$GPS.Fix.Time <- as.POSIXct(Bahia_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Beezie_gps$GPS.Fix.Time <- as.POSIXct(Beezie_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Bella_gps$GPS.Fix.Time <- as.POSIXct(Bella_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Capitu_gps$GPS.Fix.Time <- as.POSIXct(Capitu_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Cláudio_gps$GPS.Fix.Time <- as.POSIXct(Cláudio_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Colete_gps$GPS.Fix.Time <- as.POSIXct(Colete_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Dom_gps$GPS.Fix.Time <- as.POSIXct(Dom_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Dumbo_gps$GPS.Fix.Time <- as.POSIXct(Dumbo_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Erick_gps$GPS.Fix.Time <- as.POSIXct(Erick_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
George_gps$GPS.Fix.Time <- as.POSIXct(George_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Heather_gps$GPS.Fix.Time <- as.POSIXct(Heather_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Jacobina_gps$GPS.Fix.Time <- as.POSIXct(Jacobina_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Juju_gps$GPS.Fix.Time <- as.POSIXct(Juju_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Mulan_gps$GPS.Fix.Time <- as.POSIXct(Mulan_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Nayeli_gps$GPS.Fix.Time <- as.POSIXct(Nayeli_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Peter_gps$GPS.Fix.Time <- as.POSIXct(Peter_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Renee_gps$GPS.Fix.Time <- as.POSIXct(Renee_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
RandN_gps$GPS.Fix.Time <- as.POSIXct(RandN_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Tim_gps$GPS.Fix.Time <- as.POSIXct(Tim_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Tim2_gps$GPS.Fix.Time <- as.POSIXct(Tim2_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")

#based on a glance of the GPS data, collar was likely active before placed on anteater, so these points need to be excluded
#it is impossible these anteaters to be above 0 latitudinally, so we can safely exclude these points
Arya_gps <- Arya_gps[Arya_gps$GPS.Latitude < 0,]
Bahia_gps <- Bahia_gps[Bahia_gps$GPS.Latitude < 0,]
Beezie_gps <- Beezie_gps[Beezie_gps$GPS.Latitude < 0,]
Bella_gps <- Bella_gps[Bella_gps$GPS.Latitude < 0,]
Capitu_gps <- Capitu_gps[Capitu_gps$GPS.Latitude < 0,]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$GPS.Latitude < 0,]
Colete_gps <- Colete_gps[Colete_gps$GPS.Latitude < 0,]
Dom_gps <- Dom_gps[Dom_gps$GPS.Latitude < 0,]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$GPS.Latitude < 0,]
Erick_gps <- Erick_gps[Erick_gps$GPS.Latitude < 0,]
George_gps <- George_gps[George_gps$GPS.Latitude < 0,]
Heather_gps <- Heather_gps[Heather_gps$GPS.Latitude < 0,]
Jacobina_gps <- Jacobina_gps[Jacobina_gps$GPS.Latitude < 0,]
Juju_gps <- Juju_gps[Juju_gps$GPS.Latitude < 0,]
Mulan_gps <- Mulan_gps[Mulan_gps$GPS.Latitude < 0,]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$GPS.Latitude < 0,]
Peter_gps <- Peter_gps[Peter_gps$GPS.Latitude < 0,]
Renee_gps <- Renee_gps[Renee_gps$GPS.Latitude < 0,]
RandN_gps <- RandN_gps[RandN_gps$GPS.Latitude < 0,]
Tim_gps <- Tim_gps[Tim_gps$GPS.Latitude < 0,]
Tim2_gps <- Tim2_gps[Tim2_gps$GPS.Latitude < 0,]


#we will be subsetting based on translocations and release dates later, but we need to separate Rita and Nancy's information now to make it easier for us
Rita_gps <- RandN_gps [RandN_gps$GPS.Fix.Time >= ("2022-12-14 13:20:26") & RandN_gps$GPS.Fix.Time <= ("2023-02-12 16:01:31"),]
Nancy_gps <- RandN_gps [RandN_gps$GPS.Fix.Time >= ("2023-04-20 04:00:24") & RandN_gps$GPS.Fix.Time <= ("2023-06-16 13:20:12"),]

#need to also subset data that was collected predeployment of collar and animal
Arya_gps <- Arya_gps[Arya_gps$Predeployment.Data == "No",]
Bahia_gps <- Bahia_gps[Bahia_gps$Predeployment.Data == "No",]
Beezie_gps <- Beezie_gps[Beezie_gps$Predeployment.Data == "No",]
Bella_gps <- Bella_gps[Bella_gps$Predeployment.Data == "No",]
Capitu_gps <- Capitu_gps[Capitu_gps$Predeployment.Data == "No",]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$Predeployment.Data == "No",]
Colete_gps <- Colete_gps[Colete_gps$Predeployment.Data == "No",]
Dom_gps <- Dom_gps[Dom_gps$Predeployment.Data == "No",]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$Predeployment.Data == "No",]
Erick_gps <- Erick_gps[Erick_gps$Predeployment.Data == "No",]
George_gps <- George_gps[George_gps$Predeployment.Data == "No",]
Heather_gps <- Heather_gps[Heather_gps$Predeployment.Data == "No",]
Jacobina_gps <- Jacobina_gps[Jacobina_gps$Predeployment.Data == "No",]
Juju_gps <- Juju_gps[Juju_gps$Predeployment.Data == "No",]
Mulan_gps <- Mulan_gps[Mulan_gps$Predeployment.Data == "No",]
Nancy_gps <- Nancy_gps[Nancy_gps$Predeployment.Data == "No",]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$Predeployment.Data == "No",]
Peter_gps <- Peter_gps[Peter_gps$Predeployment.Data == "No",]
Renee_gps <- Renee_gps[Renee_gps$Predeployment.Data == "No",]
Rita_gps <- Rita_gps[Rita_gps$Predeployment.Data == "No",]
Tim_gps <- Tim_gps[Tim_gps$Predeployment.Data == "No",]
Tim2_gps <- Tim2_gps[Tim2_gps$Predeployment.Data == "No",]


#need to remove mortality values (either dead or this is when vest was removed and remained on for who knows why)
Arya_gps <- Arya_gps[Arya_gps$Mortality == "No*",]
Bahia_gps <- Bahia_gps[Bahia_gps$Mortality == "No*",]
Beezie_gps <- Beezie_gps[Beezie_gps$Mortality == "No*",]
Bella_gps <- Bella_gps[Bella_gps$Mortality == "No*",]
Capitu_gps <- Capitu_gps[Capitu_gps$Mortality == "No*",]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$Mortality == "No*",]
Colete_gps <- Colete_gps[Colete_gps$Mortality == "No*",]
Dom_gps <- Dom_gps[Dom_gps$Mortality == "No*",]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$Mortality == "No*",]
Erick_gps <- Erick_gps[Erick_gps$Mortality == "No*",]
George_gps <- George_gps[George_gps$Mortality == "No*",]
Heather_gps <- Heather_gps[Heather_gps$Mortality == "No*",]
Jacobina_gps <- Jacobina_gps[Jacobina_gps$Mortality == "No*",]
Juju_gps <- Juju_gps[Juju_gps$Mortality == "No*",]
Mulan_gps <- Mulan_gps[Mulan_gps$Mortality == "No*",]
Nancy_gps <- Nancy_gps[Nancy_gps$Mortality == "No*",]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$Mortality == "No*",]
Peter_gps <- Peter_gps[Peter_gps$Mortality == "No*",]
Renee_gps <- Renee_gps[Renee_gps$Mortality == "No*",]
Rita_gps <- Rita_gps[Rita_gps$Mortality == "No*",]
#Tim_gps <- Tim_gps[Tim_gps$Mortality == "No*",] #Tim's mortality sensor is falsely indicating and removing based on this removes actual data

#add outlier column for each of the anteaters to use Stefano's plot_adj function
Arya_gps$outlier <- replicate (1473, 0)
Bahia_gps$outlier <- replicate (1644, 0)
Beezie_gps$outlier <- replicate (1285, 0)
Bella_gps$outlier <- replicate (3874, 0)
Capitu_gps$outlier <- replicate (2143, 0)
Cláudio_gps$outlier <- replicate(749, 0)
Colete_gps$outlier <- replicate(4252, 0)
Dom_gps$outlier <- replicate (3886, 0)
Dumbo_gps$outlier <- replicate (4861, 0)
Erick_gps$outlier <- replicate (730, 0)
George_gps$outlier <- replicate (3345, 0)
Heather_gps$outlier <- replicate (4106, 0)
Jacobina_gps$outlier <- replicate (1811, 0)
Juju_gps$outlier <- replicate (3095, 0)
Mulan_gps$outlier <- replicate(1207, 0)
Nancy_gps$outlier <- replicate(900, 0)
Nayeli_gps$outlier <- replicate (1785, 0)
Peter_gps$outlier <- replicate (1049, 0)
Renee_gps$outlier <- replicate (4985, 0)
Rita_gps$outlier <- replicate (556, 0)
Tim_gps$outlier <- replicate (8865, 0)
Tim2_gps$outlier <- replicate (4271, 0)

#remove dates the vest was on the individual prior to release
Bahia_gps <- Bahia_gps[Bahia_gps$GPS.Fix.Time >= ("2023-12-03 01:20:27") & Bahia_gps$GPS.Fix.Time <= ("2024-02-24 22:40:10"),]
Beezie_gps <- Beezie_gps[Beezie_gps$GPS.Fix.Time >= ("2023-12-03 00:02:07") & Beezie_gps$GPS.Fix.Time <= ("2024-02-06 20:01:36"),]
Bella_gps <- Bella_gps[Bella_gps$GPS.Fix.Time >= ("2022-12-14 08:02:33") & Bella_gps$GPS.Fix.Time <= ("2023-09-27 09:20:12"),]
Capitu_gps <- Capitu_gps[Capitu_gps$GPS.Fix.Time >= ("2019-06-10 01:00:09"),]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$GPS.Fix.Time >= ("2022-03-01 00:01:07") & Cláudio_gps$GPS.Fix.Time <= ("2022-03-29 13:20:13"),]
Colete_gps <- Colete_gps[Colete_gps$GPS.Fix.Time >= ("2022-05-27 14:26:38"),]
Dom_gps <- Dom_gps[Dom_gps$GPS.Fix.Time >= ("2024-05-10 00:00:51") & Dom_gps$GPS.Fix.Time <= ("2024-11-19 14:40:08"),]
Erick_gps <- Erick_gps[Erick_gps$GPS.Fix.Time >= ("2023-04-04 04:00:19"),]
George_gps <- George_gps[George_gps$GPS.Fix.Time <= ("2023-12-20 22:40:24"),]
Heather_gps <- Heather_gps[Heather_gps$GPS.Fix.Time >= ("2022-03-01 00:01:12"),]
Jacobina_gps <- Jacobina_gps[Jacobina_gps$GPS.Fix.Time >= ("2024-12-22 00:01:51"),]
Mulan_gps <- Mulan_gps[Mulan_gps$GPS.Fix.Time >= ("2021-11-29 00:01:05") & Mulan_gps$GPS.Fix.Time <= ("2022-02-18 16:00:50"),]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$GPS.Fix.Time >= ("2023-12-03 12:00:46"),]
Peter_gps <- Peter_gps[Peter_gps$GPS.Fix.Time >= ("2021-02-28 00:02:34") & Peter_gps$GPS.Fix.Time <= ("2022-05-20 00:01:20"),]




#based on odd clusters of movement, will need to segment data for Dumbo
Dumbo_1_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2019-06-10 00:01:06") & Dumbo_gps$GPS.Fix.Time <= ("2019-10-30 00:00:54"),] 
Dumbo_2_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2019-11-12 00:01:00") & Dumbo_gps$GPS.Fix.Time <= ("2019-12-24 00:00:54"),]

#based on csv information file, Juju was moved multiple times as well, so we need to segment her data
Juju_1_gps <- Juju_gps[Juju_gps$GPS.Fix.Time >= ("2021-02-28 00:01:15") & Juju_gps$GPS.Fix.Time <= ("2022-07-07 16:01:18"),] #brought in for treatment
Juju_2_gps <- Juju_gps[Juju_gps$GPS.Fix.Time >= ("2022-08-19 14:40:07") & Juju_gps$GPS.Fix.Time <= ("2022-11-30 22:40:27"),] #vest removed due to injuries
#based on movement data, Renee was moved a couple of times, so will need to segment data
#Renee_1_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-03-04 16:02:34") & Renee_gps$GPS.Fix.Time <= ("2021-05-16 14:40:09"),] #dates are different from csv
#Renee_2_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-05-16 16:01:29") & Renee_gps$GPS.Fix.Time <= ("2021-12-09 18:40:09"),] #dates are different from csv
#Renee_3_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-12-09 20:00:15") & Renee_gps$GPS.Fix.Time <= ("2022-01-04 17:20:14"),] #dates are different from csv
#Renee_4_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2022-01-04 18:40:11") & Renee_gps$GPS.Fix.Time <= ("2023-01-13 02:39:59"),] #dates are different from csv

Renee_1_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-04-07 00:01:06") & Renee_gps$GPS.Fix.Time <= ("2021-05-16 14:40:09"),] #taken in after being close to a highway
Renee_2_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-05-24 00:02:05") & Renee_gps$GPS.Fix.Time <= ("2021-11-09 20:00:09"),] #moved because she wandered into a town (gap from previous due to stay in pen)
Renee_3_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-11-10 01:20:07") & Renee_gps$GPS.Fix.Time <= ("2021-12-09 18:40:09"),] #translocation not indicated on csv but is very apparent in data
Renee_4_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-12-09 20:00:15") & Renee_gps$GPS.Fix.Time <= ("2022-01-06 18:40:27"),] #translocation not indicated on csv but is very apparent in data
#based on odd clusters of movement, will need to segment data for Tim as well
#clusters happened because his vest was taken off multiple times due to treatment recieved
Tim_1_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2020-10-28 12:59:32") & Tim_gps$GPS.Fix.Time <= ("2021-01-25 16:01:11"),] #maybe change hour but we'll see
Tim_2_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2021-10-23 15:00:10") & Tim_gps$GPS.Fix.Time <= ("2022-02-03 19:00:23"),]
Tim_3_gps <- Tim2_gps[Tim2_gps$GPS.Fix.Time >= ("2022-04-09 00:01:51") & Tim2_gps$GPS.Fix.Time <= ("2022-05-17 22:40:09"),] #third Tim shared a vest with Colete, so that is why his data for this time is in a separate CSV
Tim_4_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2022-05-28 12:59:58"),]


#remove NAs from these again from that top row idk
Dom_gps <- Dom_gps [!is.na(Dom_gps$GPS.Latitude),]
Renee_1_gps <- Renee_1_gps [!is.na(Renee_1_gps$GPS.Latitude),]
Renee_2_gps <- Renee_2_gps [!is.na(Renee_2_gps$GPS.Latitude),]
Renee_3_gps <- Renee_3_gps [!is.na(Renee_3_gps$GPS.Latitude),]
Renee_4_gps <- Renee_4_gps [!is.na(Renee_4_gps$GPS.Latitude),]
Juju_2_gps <- Juju_2_gps [!is.na(Juju_2_gps$GPS.Latitude),]

#remove extra column in some of the dataframes
Dom_gps <- subset(Dom_gps, select = -c(Temperature))
Jacobina_gps <- subset(Jacobina_gps, select = -c(Temperature))

#this creates a tibble to make analysis easier for the future and removes NA values using the is.na function
d <- tibble(animal = c('Arya','Bahia','Beezie', 'Bella', 'Capitu', 'Cláudio', 'Colete', 'Dom', 'Dumbo_1', 'Dumbo_2', 'Erick', 'George', 'Heather','Jacobina', 'Juju_1', 'Juju_2', 'Mulan',
                       'Nancy', 'Nayeli', 'Peter', 'Rita', 'Renee_1', 'Renee_2', 'Renee_3', 'Renee_4', 'Tim_1', 'Tim_2', "Tim_3"),
            tel = purrr::map(animal, function(.name) get(paste0(.name, '_gps')) %>%
                               filter(! is.na(GPS.Latitude))))


#when cleaning this data, we want to see a depleting pattern in both the turning angle and time between locations plots.
#in the median deviation plot, points will likely be spread evenly throughout (with more of a dense cluster towards 0 on the y axis)
      #however, a cluster around 0 on the y and x axis is a big red flag that outliers are still present in the dataset


#clean based on trends on an individual level (some may have different max speeds than others) but consider the study species' typical movement patterns while cleaning



#Arya (709199A)
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_distance = 50000, value = 1) #remove distance outlier
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.5, max_distance = 1500, value = 1) #removes clear speed outlier pattern
out <- check_animal("Arya") 
flag_outlier(id = "Arya", max_speed = 0.2, max_angle = 150, value = 1) #removing more of that pattern
out <- check_animal("Arya") 
flag_outlier(id = "Arya", max_speed = 0.1, max_angle = 135, value = 1) #high speed with turning angle based on pattern
out <- check_animal("Arya") #better but still some outliers
flag_outlier(id = "Arya", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_angle = 179, value = 1)
out <- check_animal("Arya")





#Bahia (729088A)
out <- check_animal("Bahia")
flag_outlier(id = "Bahia", max_distance = 100000, value = 1) #distance outlier
out <- check_animal("Bahia")
#point looks like it connects back to release origin but maybe is an outlier?? let's plot over time
filter(d$tel[[which(d$animal == 'Bahia')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()
#based on the fixes and gap from removed mortality, likely just her transportation back after she was found dead so we can remove the two fixes
flag_outlier(id = "Bahia", max_speed = 2, value = 1) #removes point of her last translocation
out <- check_animal("Bahia")
flag_outlier(id = "Bahia", max_speed = 1, value = 1) #removes speed outlier
out <- check_animal("Bahia")
flag_outlier(id = "Bahia", max_speed = 0.4, max_angle = 135, max_distance = 10000, value = 1) #removes high speed/turning angle points
out <- check_animal("Bahia") #removed some but not all
flag_outlier(id = "Bahia", max_speed = 0.2, max_angle = 155, max_distance = 10000, value = 1) #removes weird cluster up of high speed/turning angle points along edge
out <- check_animal("Bahia") #partially worked
#removing without the distance argument removes a couple of odd but possible movement paths
#flag_outlier(id = "Bahia", max_speed = 0.1, max_angle = 150, value = 1) #continue removal pattern
#out <- check_animal("Bahia")
#hmm double check because this may have removed too much
filter(d$tel[[which(d$animal == 'Bahia')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()
flag_outlier(id = "Bahia", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Bahia") #better

#----------------------------------------------------------------------------
flag_outlier(id = "Bahia", max_angle = 135, max_speed = 0.65, value = 1)
out <- check_animal("Bahia") #better but maybe take out this line???
#-----------------------------------------------------------------------------

flag_outlier(id = "Bahia", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Bahia") #better but maybe take out this line???

#total # removed is 98 (7.6% of dataset)





#Beezie (729090A)
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_speed = 0.8, value = 1) #remove outliers above speed plateau
out <- check_animal("Beezie")
#----------------------------------
flag_outlier(id = "Beezie", max_speed = 0.6, value = 1) #remove speed outliers
out <- check_animal("Beezie")
#-------------------------------
flag_outlier(id = "Beezie", max_speed = 0.1, max_angle = 170, value = 1) #remove weird high turning angle/speed cluster
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_speed = 0.025, max_angle = 175, value = 1) #remove weird high turning angle/speed cluster
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_speed = 0.015, max_angle = 177, value = 1) #remove weird high turning angle/speed cluster
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_angle = 179, value = 1)
out <- check_animal("Beezie")
# total number of outliers removed is 101 (10.5% of dataset)





#Bella (729093A)
out <- check_animal("Bella")
flag_outlier(id = "Bella", max_distance = 300000, value = 1) #clear distance outlier
out <- check_animal("Bella")
#plot points over time + with long/lat information to see if she's just being translocated
filter(d$tel[[which(d$animal == 'Bella')]], outlier == 0) %>%
 mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()
#aligns with translocation locations and times
#additionally, speed at which she moved back does not make sense unless a car was involved
flag_outlier(id = "Bella", max_distance = 20000, value = 1)#removing all of these points because these are high speeds for this distance and the location matches previous recaptures for adjusting her vest 
                                                          #(meaning she was likely recaptured multiple times, but it was not stated)
out <- check_animal("Bella")
flag_outlier(id = "Bella", max_speed = 0.5, max_angle = 135, max_distance = 4000, value = 1) #remove the pattern of high speeds with respective turning angles
out <- check_animal("Bella") #better
flag_outlier(id = "Bella", max_speed = 0.1, max_angle = 170, max_distance = 2000, value = 1) #continue removing pattern
out <- check_animal("Bella") #better
flag_outlier(id = "Bella", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Bella")
#high weird speed that is spiking, but I can't see without narrowing down the points
plot_adj("Bella", n_adj = 20,
         min_speed = 0.4, max_speed = 0.2,
         min_distance = 4000, max_distance = 3200)
#movement actually makes sense, very exploratory (so don't remove point on the left)
#points on the right are probably fine as they indicate escape behavio

#this spike however is clearly an outlier and doesn't make sense based on the pattern (so do remove)
flag_outlier(id = "Bella", max_speed = 0.5, max_angle = 165, value = 1) #remove the spike!!!!
out <- check_animal("Bella") #worked
plot_adj("Bella", n_adj = 20,
         min_speed = 0.4, max_speed = 0.2,
         min_distance = 4000, max_distance = 3200)

#fine for now but will need to manually remove a few points
#total # of outliers removed is 348 (including removal of translocations)
#EXCLUDING removal of translocations, total # removed is 167 (4.9% of dataset)





#Capitu (709201A)
out <- check_animal("Capitu")
flag_outlier("Capitu", max_distance = 20000, value = 1) #remove distance outlier
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 1.5, value = 1) #remove speed outlier
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 0.2, max_angle = 135, value = 1) #remove cluster of turning angle outliers
out <- check_animal("Capitu") #looks pretty good
flag_outlier(id = "Capitu", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Capitu")
# total # of removed outliers is 42 (2.2% of dataset)





#Cláudio (716785A)
out <- check_animal("Cláudio")
flag_outlier("Cláudio", max_speed = 0.4, max_angle = 135, value = 1) #remove clear speed outliers
out <- check_animal("Cláudio")
#can't tell if weird movement is just outliers or an escape
plot_adj("Cláudio", n_adj = 20,
         min_speed = 0.55, max_speed = 0.48)
#not sure, leaving it for now but will get back to it it's possible it just doesn't make any sense
flag_outlier("Cláudio", max_speed = 0.05, max_angle = 170, value = 1) #remove speed/turning angle outliers
out <- check_animal("Cláudio")
flag_outlier("Cláudio", max_speed = 0.025, max_angle = 175, value = 1) #remove speed/turning angle outliers
out <- check_animal("Cláudio")





#Colete (724183A)
out <- check_animal("Colete")
flag_outlier("Colete", max_distance = 5000, value = 1) #remove distance outliers
out <- check_animal("Colete")
plot_adj("Colete", n_adj = 10,
        min_distance = 3500, max_distance = 3000)
#unsure on pattern, will leave it for now
flag_outlier("Colete", max_speed = 0.4, value = 1)
out <- check_animal("Colete")
flag_outlier("Colete", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Colete")
flag_outlier("Colete", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Colete")
#total # of outliers removed is 123 (3.5% of dataset)





#Dom (736078A)
out <- check_animal("Dom")
flag_outlier("Dom", max_distance = 50000, value = 1) #clear distance outlier
out <- check_animal("Dom")
flag_outlier("Dom", max_distance = 2000, max_speed = 0.75, value = 1) #clear distance outlier
out <- check_animal("Dom")
flag_outlier("Dom", max_speed = 0.1, max_angle = 160, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", n_adj = 10,
         min_distance = 3500, max_distance = 2900)
#could be a walk-about
flag_outlier("Dom", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Dom")
flag_outlier("Dom", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Dom")
#total # of outliers removed is 132 (4.3% of dataset)




#Dumbo's first period (709203A)
out <- check_animal("Dumbo_1")
flag_outlier("Dumbo_1", max_distance = 6000, value = 1) #remove distance outliers
out <- check_animal("Dumbo_1")
flag_outlier("Dumbo_1", max_speed = 0.75, max_distance = 1500, value = 1) #remove speed/distance pattern
out <- check_animal("Dumbo_1")
flag_outlier("Dumbo_1", max_speed = 0.5, max_distance = 1500, value = 1) #remove speed/distance pattern
out <- check_animal("Dumbo_1")
flag_outlier("Dumbo_1", max_speed = 0.1, max_angle = 170, value = 1)
out <- check_animal("Dumbo_1")
flag_outlier("Dumbo_1", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", n_adj = 20,
         min_distance = 2000, max_distance = 1500,
         min_speed = 0.1)
#not likely
flag_outlier("Dumbo_1", max_distance = 1750, max_angle = 165, value = 1) #remove distance outliers
out <- check_animal("Dumbo_1")

#new removals
flag_outlier("Dumbo_1", max_distance = 1250, value = 1)
out <- check_animal("Dumbo_1")
flag_outlier("Dumbo_1", max_distance = 950, max_speed = 0.25, max_angle = 155, value = 1)
out <- check_animal("Dumbo_1")
#total # of removed outliers is 91 (2.9% of dataset)





#Dumbo's second period (709203A)
out <- check_animal("Dumbo_2")
flag_outlier("Dumbo_2", max_distance = 10000, value = 1)
out <- check_animal("Dumbo_2")
flag_outlier("Dumbo_2", max_speed = 0.5, value = 1)
out <- check_animal("Dumbo_2")
flag_outlier("Dumbo_2", max_speed = 0.2, max_angle = 150, value = 1)
out <- check_animal("Dumbo_2")
flag_outlier("Dumbo_2", max_speed = 0.1, max_angle = 170, value = 1)
out <- check_animal("Dumbo_2")
flag_outlier("Dumbo_2", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Dumbo_2")
flag_outlier("Dumbo_2", max_distance = 1000, max_angle = 175, value = 1) #remove weird spike outlier
out <- check_animal("Dumbo_2")
#total # of outliers removed is 60 (6.8% of dataset)





#Erick
out <- check_animal("Erick")
flag_outlier("Erick", max_distance = 50000, value = 1)
out <- check_animal("Erick")
flag_outlier("Erick", max_speed = 1.5, value = 1)
out <- check_animal("Erick")
flag_outlier("Erick", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Erick")
flag_outlier("Erick", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Erick")
#total # of removed outliers is 43 (7.6% of dataset)





#George (729106A)
out <- check_animal("George")
flag_outlier("George", max_distance = 100000, value = 1) #clear distance outlier
out <- check_animal("George")
flag_outlier("George", max_distance = 15000, value = 1) #clear distance outlier
out <- check_animal("George")
flag_outlier("George", max_speed = 1.5, value = 1)
out <- check_animal("George")
flag_outlier("George", max_speed = 0.2, max_angle = 155, value = 1)
out <- check_animal("George")
flag_outlier("George", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("George")
#total # of outliers removed is 276 (8.3% of dataset)





#Heather
out <- check_animal("Heather")
flag_outlier("Heather", max_distance = 10000, value = 1) #remove distance outlier
out <- check_animal("Heather")
flag_outlier("Heather", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Heather")
flag_outlier("Heather", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Heather")
flag_outlier("Heather", max_speed = 0.025, max_angle = 177, value = 1)
out <- check_animal("Heather")
#total # of removed outliers is 293 (7.7% of dataset)




#Jacobina (736260A)
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_distance = 10000, value = 1) #distance outlier
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_speed = 0.2, max_angle = 155, value = 1) #distance outlier
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_speed = 0.1, max_angle = 170, value = 1) #distance outlier
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_speed = 0.03, max_angle = 175, value = 1) #distance outlier
out <- check_animal("Jacobina")





#Juju's first period (687743B)
out <- check_animal("Juju_1")
flag_outlier("Juju_1", max_distance = 50000, value = 1) #remove distance outlier
out <- check_animal("Juju_1")
flag_outlier("Juju_1", max_speed = 0.8, value = 1) #remove speed outlier
out <- check_animal("Juju_1")
flag_outlier("Juju_1", max_speed = 0.2, max_angle = 160, value = 1)
out <- check_animal("Juju_1")
flag_outlier("Juju_1", max_speed = 0.1, max_angle = 170, value = 1)
out <- check_animal("Juju_1")
plot_adj("Juju_1", n_adj = 15,
         min_angle = 170, max_angle = 135,
         min_speed = 0.6, max_speed = 0.4)
#hmmm leave for now but thetwo spikes on the left could be outliers
flag_outlier("Juju_1", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Juju_1")


#remove more outliers
flag_outlier("Juju_1", max_speed = 0.4, max_angle = 135, value = 1)
out <- check_animal("Juju_1")
# total # of removed outliers is 98 (6% of dataset)





#Juju's second period (687743B)
out <- check_animal("Juju_2")
flag_outlier("Juju_2", max_distance = 20000, value = 1)
out <- check_animal("Juju_2")
flag_outlier("Juju_2", max_speed = 1, value = 1)
out <- check_animal("Juju_2")
flag_outlier("Juju_2", max_speed = 0.2, max_angle = 155, value = 1)
out <- check_animal("Juju_2")
flag_outlier("Juju_2", max_speed = 0.1, max_angle = 170, value = 1)
out <- check_animal("Juju_2")
flag_outlier("Juju_2", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Juju_2")
flag_outlier("Juju_2", max_distance = 1500, max_angle = 175, value = 1)
out <- check_animal("Juju_2")
#total # of removed outliers is 87 (6.1% of dataset)





#Mulan (716786A)
out <- check_animal("Mulan")
flag_outlier("Mulan", max_speed = 0.3, max_angle = 150, value = 1)
out <- check_animal("Mulan")
flag_outlier("Mulan", max_speed = 0.15, max_angle = 150, value = 1)
out <- check_animal("Mulan")
flag_outlier("Mulan", max_speed = 0.05, max_angle = 165, value = 1)
out <- check_animal("Mulan")
filter(d$tel[[which(d$animal == 'Mulan')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()
# big triangle could be an example of escape behavior when she was hunted
flag_outlier("Mulan", max_angle = 179, max_distance = 1500, value = 1)
out <- check_animal("Mulan")
#still some points to remove but you can do that manually, since fiddling with it is messing up the actual data
#total # of removed outliers is 50 (4.5% of dataset)





#Nancy (729101A)
out <- check_animal("Nancy")
flag_outlier("Nancy", max_speed = 1.2, value = 1)
out <- check_animal("Nancy")
flag_outlier("Nancy", max_speed = 0.2, max_angle = 170, value = 1)
out <- check_animal("Nancy")
flag_outlier("Nancy", max_speed = 0.1, max_angle = 175, value = 1)
out <- check_animal("Nancy")
flag_outlier("Nancy", max_speed = 0.05, max_angle = 177, value = 1)
out <- check_animal("Nancy")
plot_adj("Nancy", n_adj = 10,
         min_distance = 1500, max_distance = 1000,
         min_angle = 177, max_angle = 170)
flag_outlier("Nancy", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Nancy")
flag_outlier("Nancy", max_speed = 0.025, max_angle = 177, value = 1)
out <- check_animal("Nancy")
#there's a couple of outliers you should remove but we can do that manually
#total # of removed outliers is 39 (4.3% of dataset)





#Nayeli (729091A)
out <- check_animal("Nayeli")
flag_outlier("Nayeli", max_distance = 10000, value = 1)
out <- check_animal("Nayeli")
flag_outlier("Nayeli", max_speed = 0.3, max_angle = 155, value = 1)
out <- check_animal("Nayeli")
flag_outlier("Nayeli", max_speed = 0.15, max_angle = 165, value = 1)
out <- check_animal("Nayeli")
flag_outlier("Nayeli", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Nayeli")
flag_outlier("Nayeli", max_speed = 0.025, max_angle = 177, value = 1)
out <- check_animal("Nayeli")
#total # of removed outliers is 113 (7.6% of dataset)





#Peter (687745B)
out <- check_animal("Peter")
flag_outlier("Peter", max_distance = 6000, value = 1)
out <- check_animal("Peter")
flag_outlier("Peter", max_speed = 0.2, max_angle = 155, value = 1) #remove above speed plateau
out <- check_animal("Peter")
flag_outlier("Peter", max_speed = 0.05, max_angle = 175, value = 1) #remove above speed plateau
out <- check_animal("Peter")
flag_outlier("Peter", max_speed = 0.05, max_angle = 170, value = 1) #remove above speed plateau
out <- check_animal("Peter")
flag_outlier("Peter", max_speed = 0.015, max_angle = 175, value = 1) #remove above speed plateau
out <- check_animal("Peter")
flag_outlier("Peter", max_angle = 179, value = 1) #remove above speed plateau
out <- check_animal("Peter")
#total # of removed outliers is 103 (___% of dataset)





#Renee_1 (687746B)
out <- check_animal("Renee_1")
flag_outlier("Renee_1", max_distance = 100000, value = 1)
out <- check_animal("Renee_1")
#flag_outlier("Renee_1", max_speed = 0.2, max_angle = 150, value = 1)
#out <- check_animal("Renee_1")
flag_outlier("Renee_1", max_speed = 0.1, max_angle = 170, value = 1)
out <- check_animal("Renee_1")
flag_outlier("Renee_1", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Renee_1")
#flag_outlier("Renee_1", max_speed = 0.025, max_angle = 177, value = 1)
#out <- check_animal("Renee_1")
filter(d$tel[[which(d$animal == 'Renee_1')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()
plot_adj("Renee_1", n_adj = 10,
         max_speed = 0.4)
#potential problem points have high hdops so it's not a big deal to leave in (less weight will be put on it in models)
#total # of outliers removed is 39 (5.7% of dataset)





#Renee_2 (687746B)
out <- check_animal("Renee_2")
flag_outlier("Renee_2", max_speed = 0.4, max_angle = 100, value = 1)
out <- check_animal("Renee_2")
flag_outlier("Renee_2", max_speed = 0.2, max_angle = 150, value = 1)
out <- check_animal("Renee_2")
flag_outlier("Renee_2", max_speed = 0.1, max_angle = 165, value = 1)
out <- check_animal("Renee_2")
flag_outlier("Renee_2", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Renee_2")
#total # of removed outliers is 146 (5.4% of dataset)





#Renee_3 (687746B)
out <- check_animal("Renee_3")
flag_outlier("Renee_3", max_distance = 75000, value = 1) #32 fixes removed in this translocation
out <- check_animal("Renee_3")
flag_outlier("Renee_3", max_speed = 0.2, max_angle = 160, value = 1)
out <- check_animal("Renee_3")
flag_outlier("Renee_3", max_speed = 0.075, max_angle = 170, value = 1)
out <- check_animal("Renee_3")
plot_adj("Renee_3", n_adj = 10,
         max_speed = 0.5, max_angle = 90)
#this seems fine, as she stopped here for awhile, and the hdop is pretty large, so it will be weighted less
#total # of removed outliers is 43 (10.7% of dataset)





#Renee_4 (687746B)
out <- check_animal("Renee_4")
flag_outlier("Renee_4", max_distance = 40000, value = 1)
out <- check_animal("Renee_4")
flag_outlier("Renee_4", max_distance = 5000, value = 1)
out <- check_animal("Renee_4")
flag_outlier("Renee_4", max_speed = 0.15, max_angle = 150, value = 1)
out <- check_animal("Renee_4")

flag_outlier("Renee_4", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Renee_4")
flag_outlier("Renee_4", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Renee_4")
plot_adj("Renee_4", n_adj = 20,
         max_speed = 0.175, max_angle = 90)
#high hdop values so it will be weighted less if it's wrong so we can keep in
#total # of removed outliers is 52 (13% of dataset)





#Rita (729101A)
out <- check_animal("Rita")
flag_outlier("Rita", max_distance = 6000, value = 1)
out <- check_animal("Rita")
flag_outlier("Rita", max_speed = 0.2, max_angle = 155, value = 1)
out <- check_animal("Rita")
flag_outlier("Rita", max_speed = 0.1, max_angle = 170, value = 1)
out <- check_animal("Rita")
plot_adj("Rita", n_adj = 10,
         min_distance = 3200, max_distance = 2900)
filter(d$tel[[which(d$animal == 'Rita')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()
flag_outlier("Rita", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Rita")
# total # of removed outliers is 66 (11.9% of dataset)





#Tim 1
out <- check_animal("Tim_1")
flag_outlier("Tim_1", max_distance = 100000, value = 1)
out <- check_animal("Tim_1")
flag_outlier("Tim_1", max_speed = 1, max_angle = 135, value = 1)
out <- check_animal("Tim_1")
flag_outlier("Tim_1", max_speed = 0.4, max_angle = 150, value = 1)
out <- check_animal("Tim_1")
flag_outlier("Tim_1", max_speed = 0.2, max_angle = 165, value = 1)
out <- check_animal("Tim_1")
flag_outlier("Tim_1", max_speed = 0.075, max_angle = 170, value = 1)
out <- check_animal("Tim_1")
flag_outlier("Tim_1", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Tim_1")
plot_adj("Tim_1", n_adj = 10,
         max_speed = 0.6)
#high hdop values so we can leave
#total # of removed outliers is 89 (4.9%)





#Tim_2
out <- check_animal("Tim_2")
flag_outlier("Tim_2", max_speed = 0.4, max_angle = 150, value = 1)
out <- check_animal("Tim_2")
flag_outlier("Tim_2", max_speed = 0.2, max_angle = 160, value = 1)
out <- check_animal("Tim_2")
flag_outlier("Tim_2", max_speed = 0.125, max_angle = 170, value = 1)
out <- check_animal("Tim_2")
flag_outlier("Tim_2", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("Tim_2")
flag_outlier("Tim_2", max_speed = 0.025, max_angle = 177, value = 1)
out <- check_animal("Tim_2")
flag_outlier("Tim_2", max_angle = 179, value = 1)
out <- check_animal("Tim_2")

#fine for the most part but may need some additional fixes removed
#total # of removed outliers is 82 (3.5% of dataset)





#Tim_3 
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_distance = 15000, value = 1)
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_distance = 12000, value = 1)
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_speed = 0.2, max_angle = 135, value = 1)
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_speed = 0.1, max_angle = 150, value = 1)
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_speed = 0.03, max_angle = 170, value = 1)
out <- check_animal("Tim_3")
flag_outlier("Tim_3", max_speed = 0.015, max_angle = 175, value = 1)
out <- check_animal("Tim_3")
#total # of removed outliers is 32 (4.6% of dataset)





#save cleaned data
saveRDS(object = d,
        file = paste0("~/Giant_Anteater_Orphan/FINAL/DATA/clean_GPS_data",
                      format(Sys.time(), '%Y-%m-%d-%H-%M'),
                      '.rds'))


#manually clean outliers ----------------------------------------------------------------------------------------------------
#there are still a few small outliers that will need to be removed manually
#import file
GPS_data_tbl <- readRDS("~/Giant_Anteater_Orphan/FINAL/DATA/clean_GPS_data2025-04-28-17-59.rds")
#check structure
class(GPS_data_tbl) #dataset was imported as a tbl_df
for(i in 1:length(GPS_data_tbl$tel)){ 
  GPS_data_tbl$tel[[i]]$ID <- GPS_data_tbl$animal[[i]]  # add in  IDs
}
#converts tbl to a dataframe
DATA_orphan <- do.call(rbind, GPS_data_tbl$tel) 
#check structure
class(DATA_orphan) #works
#convert to a ctmm telemetry object and remove outliers
DATA_orphan <- as.telemetry(DATA_orphan, mark.rm = TRUE)



#now we can manually remove outliers by creating a bounding box around them

#Bella
orphan <- DATA_orphan[["Bella"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot to find outlier
plot(orphan[orphan$x >= 37000 & orphan$x <= 37900 &
              orphan$y >= -9800 & orphan$y <= -9200,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= 37000 & orphan$x <= 37900 &
                             orphan$y >= -9800 & orphan$y <= -9200,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#plot to find outlier
plot(orphan[orphan$x >= 36800 & orphan$x <= 37400 &
              orphan$y >= -11350 & orphan$y <= -10900,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= 36800 & orphan$x <= 37400 &
                             orphan$y >= -11350 & orphan$y <= -10900,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Bella"]] <- DATA_orphan[["Bella"]][!(rownames(DATA_orphan[["Bella"]]) %in% test_flag),]





#Colete
orphan <- DATA_orphan[["Colete"]] #subset out an individual
outlie(orphan)
#make an individual list to hold some outliers but not all
test_flag <- list()
plot(orphan[orphan$x >= -7500 & orphan$x <= -7300 &
              orphan$y >= 1100 & orphan$y <= 1400,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -7500 & orphan$x <= -7300 &
                             orphan$y >= 1100 & orphan$y <= 1400,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Colete"]] <- DATA_orphan[["Colete"]][!(rownames(DATA_orphan[["Colete"]]) %in% test_flag),]





#Dom
orphan <- DATA_orphan[["Dom"]] #subset out an individual
outlie(orphan)
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= -8000 & orphan$x <= -7700 &
              orphan$y >= 950 & orphan$y <= 1100,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -8000 & orphan$x <= -7700 &
                             orphan$y >= 950 & orphan$y <= 1100,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)

#remove outliers from individual
DATA_orphan[["Dom"]] <- DATA_orphan[["Dom"]][!(rownames(DATA_orphan[["Dom"]]) %in% test_flag),]





#Dumbo 1
orphan <- DATA_orphan[["Dumbo_1"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
test_flag <- list()

plot(orphan[orphan$x >= -7900 & orphan$x <= -7500 &
              orphan$y >= 1000 & orphan$y <= 1250,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -7900 & orphan$x <= -7500 &
                             orphan$y >= 1000 & orphan$y <= 1250,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Dumbo_1"]] <- DATA_orphan[["Dumbo_1"]][!(rownames(DATA_orphan[["Dumbo_1"]]) %in% test_flag),]





#Juju 1
orphan <- DATA_orphan[["Juju_1"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= -90000 & orphan$x <= -88900 &
              orphan$y >= 10000 & orphan$y <= 11400,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -90000 & orphan$x <= -88450 &
                             orphan$y >= 10600 & orphan$y <= 11300,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)

#plot outliers
plot(orphan[orphan$x >= -96500 & orphan$x <= -96000 &
              orphan$y >= 12000 & orphan$y <= 13000,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -96500 & orphan$x <= -96000 &
                             orphan$y >= 12000 & orphan$y <= 13000,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)

#remove outliers from individual
DATA_orphan[["Juju_1"]] <- DATA_orphan[["Juju_1"]][!(rownames(DATA_orphan[["Juju_1"]]) %in% test_flag),]





#Juju_2
orphan <- DATA_orphan[["Juju_2"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= -93300 & orphan$x <= -93000 &
              orphan$y >= 11000 & orphan$y <= 11300,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -93300 & orphan$x <= -93000 &
                             orphan$y >= 11000 & orphan$y <= 11300,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#plot outliers
plot(orphan[orphan$x >= -93450 & orphan$x <= -93100 &
              orphan$y >= 10700 & orphan$y <= 10900,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -93450 & orphan$x <= -93100 &
                             orphan$y >= 10700 & orphan$y <= 10900,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#plot outliers
plot(orphan[orphan$x >= -93650 & orphan$x <= -93200 &
              orphan$y >= 12000 & orphan$y <= 12200,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -93650 & orphan$x <= -93200 &
                             orphan$y >= 12000 & orphan$y <= 12200,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#plot outliers
plot(orphan[orphan$x >= -94850 & orphan$x <= -94500 &
              orphan$y >= 12500 & orphan$y <= 12750,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -94850 & orphan$x <= -94500 &
                             orphan$y >= 12500 & orphan$y <= 12750,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot without outlier
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)

#remove outliers from individual
DATA_orphan[["Juju_2"]] <- DATA_orphan[["Juju_2"]][!(rownames(DATA_orphan[["Juju_2"]]) %in% test_flag),]





#Mulan
orphan <- DATA_orphan[["Mulan"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= -9800 & orphan$x <= -9600 &
              orphan$y >= -300 & orphan$y <= 200,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -9800 & orphan$x <= -9600 &
                             orphan$y >= -300 & orphan$y <= 200,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot outliers
plot(orphan[orphan$x >= -7850 & orphan$x <= -7750 &
              orphan$y >= 900 & orphan$y <= 1100,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -7850 & orphan$x <= -7750 &
                             orphan$y >= 900 & orphan$y <= 1100,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#plot outliers
plot(orphan[orphan$x >= -8500 & orphan$x <= -8100 &
              orphan$y >= 800 & orphan$y <= 1050,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -8500 & orphan$x <= -8100 &
                             orphan$y >= 800 & orphan$y <= 1050,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Mulan"]] <- DATA_orphan[["Mulan"]][!(rownames(DATA_orphan[["Mulan"]]) %in% test_flag),]





#Nancy
orphan <- DATA_orphan[["Nancy"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= -87000 & orphan$x <= -86500 &
              orphan$y >= 10000 & orphan$y <= 10500,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -87000 & orphan$x <= -86500 &
                             orphan$y >= 10000 & orphan$y <= 10500,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Nancy"]] <- DATA_orphan[["Nancy"]][!(rownames(DATA_orphan[["Nancy"]]) %in% test_flag),]





#Tim 1
orphan <- DATA_orphan[["Tim_1"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= 12300 & orphan$x <= 12700 &
              orphan$y >= -9200 & orphan$y <= -8300,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= 12300 & orphan$x <= 12700 &
                             orphan$y >= -9200 & orphan$y <= -8300,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Tim_1"]] <- DATA_orphan[["Tim_1"]][!(rownames(DATA_orphan[["Tim_1"]]) %in% test_flag),]





#Tim 2
orphan <- DATA_orphan[["Tim_2"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outliers
plot(orphan[orphan$x >= -9300 & orphan$x <= -9100 &
              orphan$y >= 400 & orphan$y <= 600,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -9300 & orphan$x <= -9100 &
                             orphan$y >= 400 & orphan$y <= 600,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
DATA_orphan[["Tim_2"]] <- DATA_orphan[["Tim_2"]][!(rownames(DATA_orphan[["Tim_2"]]) %in% test_flag),]





#Tim_3
orphan <- DATA_orphan[["Tim_3"]] #subset out an individual
outlie(orphan) #make same diagnostic plots
#make an individual list to hold some outliers but not all
test_flag <- list()
#plot outlier
plot(orphan[orphan$x >= -9500 & orphan$x <= -9000 &
              orphan$y >= 150 & orphan$y <= 300,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -9500 & orphan$x <= -9000 &
                             orphan$y >= 150 & orphan$y <= 300,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)

#plot outliers
plot(orphan[orphan$x >= -8230 & orphan$x <= -8100 &
              orphan$y >= 440 & orphan$y <= 500,])
#flag outlier
flagged <- rownames(orphan[orphan$x >= -8230 & orphan$x <= -8100 &
                             orphan$y >= 440 & orphan$y <= 500,])
#add outlier row to a list
test_flag <- c(test_flag, flagged)
#plot to check
TEST <- orphan[!(rownames(orphan) %in% test_flag),]
outlie(TEST)
#remove outliers from individual
DATA_orphan[["Tim_3"]] <- DATA_orphan[["Tim_3"]][!(rownames(DATA_orphan[["Tim_3"]]) %in% test_flag),]


#save telemetry output
save(DATA_orphan, file = "~/Giant_Anteater_Orphan/FINAL/Data/Orphaned/Data_telemetry_NO_UERE.rda") #save file













#getting mortality for calibration---------------------------------------------------------------
#as our vests were not calibrated properly, we can extract all mortality fixes where the individual is stagnate to calibrate the GPS properly
#reimport data for individuals who died
Bahia_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Bahia/729088A_1_Datalog.csv", skip = 22)
Capitu_gps <- read.csv ("~/Giant_Anteater_Orphan/Old_Code_Results/Old/Data/Raw_Data/709201A_Datalog.csv", skip = 22)
Cláudio_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Cláudio/716785A_2_Datalog.csv", skip = 22)
Mulan_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Mulan/716786A_1_Datalog.csv", skip = 22)
Nayeli_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Nayeli/729091A_1_Datalog.csv", skip = 22)
RandN_gps <-read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Rita_e_Nancy/729101A_1_Datalog.csv", skip = 22)
#skipping Arya, as she drowned in a pool and may not have been completely stagnate


#remove NA values using the GPS Latitude column (no succesful fix should have an NA for location)
Bahia_gps <- Bahia_gps [!is.na(Bahia_gps$GPS.Latitude),]
Capitu_gps <- Capitu_gps [!is.na(Capitu_gps$GPS.Latitude),]
Cláudio_gps <- Cláudio_gps [!is.na(Cláudio_gps$GPS.Latitude),]
Mulan_gps <- Mulan_gps [!is.na(Mulan_gps$GPS.Latitude),]
Nayeli_gps <- Nayeli_gps [!is.na(Nayeli_gps$GPS.Latitude),]
RandN_gps <- RandN_gps [!is.na(RandN_gps$GPS.Latitude),]

#format time stamp
Bahia_gps$GPS.Fix.Time <- as.POSIXct(Bahia_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Capitu_gps$GPS.Fix.Time <- as.POSIXct(Capitu_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Cláudio_gps$GPS.Fix.Time <- as.POSIXct(Cláudio_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Mulan_gps$GPS.Fix.Time <- as.POSIXct(Mulan_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Nayeli_gps$GPS.Fix.Time <- as.POSIXct(Nayeli_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
RandN_gps$GPS.Fix.Time <- as.POSIXct(RandN_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")

#based on a glance of the GPS data, collar was likely active before placed on anteater, so these points need to be excluded
#it is impossible these anteaters to be above 0 latitudinally, so we can safely exclude these points
Bahia_gps <- Bahia_gps[Bahia_gps$GPS.Latitude < 0,]
Capitu_gps <- Capitu_gps[Capitu_gps$GPS.Latitude < 0,]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$GPS.Latitude < 0,]
Mulan_gps <- Mulan_gps[Mulan_gps$GPS.Latitude < 0,]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$GPS.Latitude < 0,]
RandN_gps <- RandN_gps[RandN_gps$GPS.Latitude < 0,]


#we will be subsetting based on translocations and release dates later, but we need to separate Rita and Nancy's information now to make it easier for us
Rita_gps <- RandN_gps [RandN_gps$GPS.Fix.Time >= ("2022-12-14 13:20:26") & RandN_gps$GPS.Fix.Time <= ("2023-02-12 16:01:31"),]
rm(RandN_gps)

#need to also subset data that was collected predeployment of collar and animal
Bahia_gps <- Bahia_gps[Bahia_gps$Predeployment.Data == "No",]
Capitu_gps <- Capitu_gps[Capitu_gps$Predeployment.Data == "No",]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$Predeployment.Data == "No",]
Mulan_gps <- Mulan_gps[Mulan_gps$Predeployment.Data == "No",]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$Predeployment.Data == "No",]
Rita_gps <- Rita_gps[Rita_gps$Predeployment.Data == "No",]


#need to remove mortality values (either dead or this is when vest was removed and remained on for who knows why)
Bahia_gps <- Bahia_gps[Bahia_gps$Mortality == "Yes*",]
Capitu_gps <- Capitu_gps[Capitu_gps$Mortality == "Yes*",]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$Mortality == "Yes*",]
Mulan_gps <- Mulan_gps[Mulan_gps$Mortality == "Yes*",]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$Mortality == "Yes*",]
Rita_gps <- Rita_gps[Rita_gps$Mortality == "Yes*",]

#remove if there was a clear movement/translocation
Bahia_gps <- Bahia_gps[Bahia_gps$GPS.Fix.Time <= ("2024-02-24 18:40:16"),] 

#drop Cláudio, as he does not have enough fixes (only 3)
rm(Cláudio_gps)

#make an outlier column
Bahia_gps$outlier <- replicate (59, 0)
Capitu_gps$outlier <- replicate(48, 0)
Mulan_gps$outlier <- replicate(103, 0)
Nayeli_gps$outlier <- replicate (23, 0)
Rita_gps$outlier <- replicate (19, 0)

#this creates a tibble to make analysis easier for the future and removes NA values using the is.na function
#we are making another tibble to see if the animals are being transported or moving, as any movements would disrupt calibration
d <- tibble(animal = c('Bahia', 'Capitu', 'Mulan', 'Nayeli', 'Rita'),
            tel = purrr::map(animal, function(.name) get(paste0(.name, '_gps')) %>%
                               filter(! is.na(GPS.Latitude))))

#check each individual to make sure their bodies were not moved (we only want to see one cluster of points with some outlier spiking)
#Bahia
out <- check_animal("Bahia")
#just outliers

#Capitu
out <- check_animal("Capitu")
#just outliers

#Mulan
out <- check_animal("Mulan")
#just outliers

#Nayeli
out <- check_animal("Nayeli")
#just outliers

#Rita
out <- check_animal("Rita")
#just outliers

#now we can save the data since there is not crazy movement
#save mortality output
saveRDS(object = d,
        file = paste0("~/Giant_Anteater_Orphan/FINAL/DATA/mortality_data",
                      format(Sys.time(), '%Y-%m-%d-%H-%M'),
                      '.rds'))


#assigning UERE------------------------------------------------------------------------------------
#import data and convert to telemetry ----
mortality_tbl <- readRDS("~/Giant_Anteater_Orphan/FINAL/Data/mortality_data2025-04-28-20-39.rds")
#check class
class(mortality_tbl) #dataset was imported as a tbl_df

#add IDs back in
for(i in 1:length(mortality_tbl$tel)){ 
  mortality_tbl$tel[[i]]$ID <- mortality_tbl$animal[[i]] 
}
#convert the tbl into a dataframe
mortality <- do.call(rbind, mortality_tbl$tel) #converts tiblle to a dataframe

#check class to see if change wen through
class(mortality) #works

#convert the dataframe to a ctmm telemetry object (will output a nested list)
mortality <- as.telemetry(mortality, mark.rm = TRUE) #Convert to telemetry object and list and mark.rm = TRUE will drop outliers

#save file
save(mortality, file = "~/Giant_Anteater_Orphan/FINAL/Data/Orphaned/Mortality_tel.rda") #save file


#now we can use the telemetry points from when the individuals were dead and laying there as a means of calibration
#we need to do this because the collar was likely calibrated on an individual within a pen (little bits of movement go a long way)
#this will result with more accurately calibrated data, as we know that the collar was not moving

#load mortality and orphan data
load("~/Giant_Anteater_Orphan/FINAL/Data/Orphaned/Data_telemetry_NO_UERE.rda") #orphaned telemetry data
load("~/Giant_Anteater_Orphan/FINAL/Data/Orphaned/Mortality_tel.rda") #mortality telemetry data

#estimate the RMS UERE for the dead individuals
UERE_2 <- uere.fit(mortality)

#plot to see difference
plot(DATA_orphan[[7]]) #before

#assign the estimated UERE to the whole dataset
uere(DATA_orphan) <- UERE_2

#plot difference
plot(DATA_orphan[[7]]) #after

#save telemetry data
save(DATA_orphan, file = "~/Giant_Anteater_Orphan/FINAL/DATA/Orphaned/Data_telemetry.rda") #save file


