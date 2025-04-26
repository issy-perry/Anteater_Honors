

#load packages
library(ctmm)
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)

#loud source functions that Stefano Mezzini created (see his GitHub repo for functions)
source("~/Giant_Anteater_Orphan/Functions_Stefano/plot_adj.R") # to plot 20 adjacent locations
source("~/Giant_Anteater_Orphan/Functions_Stefano/flag_outlier.R") # to mark outliers
source("~/Giant_Anteater_Orphan/Functions_Stefano/remove_outlier_flags.R") # to start over with an animal
source("~/Giant_Anteater_Orphan/Functions_Stefano/outlier_plots.R") # to plot outlier diagnostic plots
source("~/Giant_Anteater_Orphan/Functions_Stefano/check_animal.R") # to run diagnostic plots

#determine current working directory
getwd()

#import data into R
Arya_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/709199A_Datalog.csv", skip = 22)
Bella_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Bella/729093A_1_Datalog.csv", skip = 22)
Beezie_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Beezie/729090A_1_Datalog.csv", skip = 22)
Capitu_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/709201A_Datalog.csv", skip = 22)
Cláudio_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Cláudio/716785A_2_Datalog.csv", skip = 22)
Dom_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Dom/736078A_1_Datalog.csv", skip = 22)
Dumbo_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/709203A_Datalog.csv", skip = 22)
George_gps <- read.csv("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/George/729106A_1_Datalog.csv", skip = 22)
Heather_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/Heather.csv", skip = 22)
Jacobina_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Jacobina/736260A_1_Datalog.csv", skip = 22)
Juju_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/687743B_Datalog.csv", skip = 22)
Mulan_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Mulan/716786A_1_Datalog.csv", skip = 22)
Nayeli_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Nayeli/729091A_1_Datalog.csv", skip = 22)
Peter_gps <- read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Peter/687745B_1_Datalog.csv", skip = 22)
Renee_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/687746B_Datalog.csv", skip = 22)
Tim_gps <- read.csv ("~/Giant_Anteater_Orphan/Old/Data/Raw_Data/tim.csv", skip = 22)
RandN_gps <-read.csv ("~/Giant_Anteater_Orphan/FIXED/Data/New_Orphans/Rita_e_Nancy/729101A_1_Datalog.csv", skip = 22)

#remove NA values using is.na function
Arya_gps <- Arya_gps [!is.na(Arya_gps$GPS.Latitude),]
Bella_gps <- Bella_gps [!is.na(Bella_gps$GPS.Latitude),]
Beezie_gps <- Beezie_gps [!is.na(Beezie_gps$GPS.Latitude),]
Capitu_gps <- Capitu_gps [!is.na(Capitu_gps$GPS.Latitude),]
Cláudio_gps <- Cláudio_gps [!is.na(Cláudio_gps$GPS.Latitude),]
Dom_gps <- Dom_gps [!is.na(Dom_gps$GPS.Latitude),]
Dumbo_gps <- Dumbo_gps [!is.na(Dumbo_gps$GPS.Latitude),]
George_gps <- George_gps [!is.na(George_gps$GPS.Latitude),]
Heather_gps <- Heather_gps [!is.na(Heather_gps$GPS.Latitude),]
Jacobina_gps <- Jacobina_gps [!is.na(Jacobina_gps$GPS.Latitude),]
Juju_gps <- Juju_gps [!is.na(Juju_gps$GPS.Latitude),]
Mulan_gps <- Mulan_gps [!is.na(Mulan_gps$GPS.Latitude),]
Nayeli_gps <- Nayeli_gps [!is.na(Nayeli_gps$GPS.Latitude),]
Peter_gps <- Peter_gps [!is.na(Peter_gps$GPS.Latitude),]
Renee_gps <- Renee_gps [!is.na(Renee_gps$GPS.Latitude),]
Tim_gps <- Tim_gps [!is.na(Tim_gps$GPS.Latitude),]
RandN_gps <- RandN_gps [!is.na(RandN_gps$GPS.Latitude),]

#separate files that are joint individuals


#format time stamp
Arya_gps$GPS.Fix.Time <- as.POSIXct(Arya_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Bella_gps$GPS.Fix.Time <- as.POSIXct(Bella_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Beezie_gps$GPS.Fix.Time <- as.POSIXct(Beezie_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Capitu_gps$GPS.Fix.Time <- as.POSIXct(Capitu_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Cláudio_gps$GPS.Fix.Time <- as.POSIXct(Cláudio_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Dom_gps$GPS.Fix.Time <- as.POSIXct(Dom_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Dumbo_gps$GPS.Fix.Time <- as.POSIXct(Dumbo_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
George_gps$GPS.Fix.Time <- as.POSIXct(George_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Heather_gps$GPS.Fix.Time <- as.POSIXct(Heather_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Jacobina_gps$GPS.Fix.Time <- as.POSIXct(Jacobina_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Juju_gps$GPS.Fix.Time <- as.POSIXct(Juju_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Mulan_gps$GPS.Fix.Time <- as.POSIXct(Mulan_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
#Nancy_gps$GPS.Fix.Time <- as.POSIXct(Nancy_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Nayeli_gps$GPS.Fix.Time <- as.POSIXct(Nayeli_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Peter_gps$GPS.Fix.Time <- as.POSIXct(Peter_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Renee_gps$GPS.Fix.Time <- as.POSIXct(Renee_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
RandN_gps$GPS.Fix.Time <- as.POSIXct(RandN_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Tim_gps$GPS.Fix.Time <- as.POSIXct(Tim_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")


Rita_gps <- RandN_gps [RandN_gps$GPS.Fix.Time >= ("2022-12-14 13:20:26") & RandN_gps$GPS.Fix.Time <= ("2023-02-12 16:01:31"),]
Nancy_gps <- RandN_gps [RandN_gps$GPS.Fix.Time >= ("2023-04-20 04:00:24") & RandN_gps$GPS.Fix.Time <= ("2023-06-16 13:20:12"),]
#based on a glance of the GPS data, collar was likely active before placed on anteater, so these points need to be excluded
Arya_gps <- Arya_gps[Arya_gps$GPS.Latitude < 0,]
Bella_gps <- Bella_gps[Bella_gps$GPS.Latitude < 0,]
Beezie_gps <- Beezie_gps[Beezie_gps$GPS.Latitude < 0,]
Capitu_gps <- Capitu_gps[Capitu_gps$GPS.Latitude < 0,]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$GPS.Latitude < 0,]
Dom_gps <- Dom_gps[Dom_gps$GPS.Latitude < 0,]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$GPS.Latitude < 0,]
George_gps <- George_gps[George_gps$GPS.Latitude < 0,]
Heather_gps <- Heather_gps[Heather_gps$GPS.Latitude < 0,]
Jacobina_gps <- Jacobina_gps[Jacobina_gps$GPS.Latitude < 0,]
Juju_gps <- Juju_gps[Juju_gps$GPS.Latitude < 0,]
Mulan_gps <- Mulan_gps[Mulan_gps$GPS.Latitude < 0,]
Nancy_gps <- Nancy_gps[Nancy_gps$GPS.Latitude < 0,]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$GPS.Latitude < 0,]
Peter_gps <- Peter_gps[Peter_gps$GPS.Latitude < 0,]
Renee_gps <- Renee_gps[Renee_gps$GPS.Latitude < 0,]
Rita_gps <- Rita_gps[Rita_gps$GPS.Latitude < 0,]
Tim_gps <- Tim_gps[Tim_gps$GPS.Latitude < 0,]

#need to also subset data that was collected predeployment of collar and animal
Arya_gps <- Arya_gps[Arya_gps$Predeployment.Data == "No",]
Bella_gps <- Bella_gps[Bella_gps$Predeployment.Data == "No",]
Beezie_gps <- Beezie_gps[Beezie_gps$Predeployment.Data == "No",]
Capitu_gps <- Capitu_gps[Capitu_gps$Predeployment.Data == "No",]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$Predeployment.Data == "No",]
Dom_gps <- Dom_gps[Dom_gps$Predeployment.Data == "No",]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$Predeployment.Data == "No",]
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

#need to remove mortality values (either dead or this is when vest was removed and remained on for who knows why)
Arya_gps <- Arya_gps[Arya_gps$Mortality == "No*",]
Bella_gps <- Bella_gps[Bella_gps$Mortality == "No*",]
Beezie_gps <- Beezie_gps[Beezie_gps$Mortality == "No*",]
Capitu_gps <- Capitu_gps[Capitu_gps$Mortality == "No*",]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$Mortality == "No*",]
Dom_gps <- Dom_gps[Dom_gps$Mortality == "No*",]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$Mortality == "No*",]
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
Bella_gps$outlier <- replicate (3874, 0)
Beezie_gps$outlier <- replicate (1285, 0)
Capitu_gps$outlier <- replicate (2143, 0)
Cláudio_gps$outlier <- replicate(749, 0)
Dom_gps$outlier <- replicate (3886, 0)
Dumbo_gps$outlier <- replicate (4861, 0)
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

#remove dates the vest was on the individual prior to release
Dom_gps <- Dom_gps[Dom_gps$GPS.Fix.Time >= ("2024-05-10 00:00:51") & Dom_gps$GPS.Fix.Time <= ("2024-11-19 14:40:08"),]
Bella_gps <- Bella_gps[Bella_gps$GPS.Fix.Time >= ("2022-12-14 08:02:33") & Bella_gps$GPS.Fix.Time <= ("2023-09-27 09:20:12"),]
Peter_gps <- Peter_gps[Peter_gps$GPS.Fix.Time >= ("2021-02-28 00:02:34") & Peter_gps$GPS.Fix.Time <= ("2022-05-20 00:01:20"),]
Beezie_gps <- Beezie_gps[Beezie_gps$GPS.Fix.Time >= ("2023-12-03 00:02:07") & Beezie_gps$GPS.Fix.Time <= ("2024-02-06 20:01:36"),]
Cláudio_gps <- Cláudio_gps[Cláudio_gps$GPS.Fix.Time >= ("2022-03-01 00:01:07") & Cláudio_gps$GPS.Fix.Time <= ("2022-03-29 13:20:13"),]
Nayeli_gps <- Nayeli_gps[Nayeli_gps$GPS.Fix.Time >= ("2023-12-03 12:00:46"),]
Jacobina_gps <- Jacobina_gps[Jacobina_gps$GPS.Fix.Time >= ("2024-12-22 00:01:51"),]
Mulan_gps <- Mulan_gps[Mulan_gps$GPS.Fix.Time <= ("2022-02-18 16:00:50"),]

#remove NAs from these again from that top row idk
Dom_gps <- Dom_gps [!is.na(Dom_gps$GPS.Latitude),]
Bella_gps <- Bella_gps [!is.na(Bella_gps$GPS.Latitude),]
#based on odd clusters of movement, will need to segment data for Dumbo
Dumbo_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2019-05-29 19:00:19") & Dumbo_gps$GPS.Fix.Time <= ("2019-12-24 00:00:54"),] 
#Dumbo_2_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2019-12-23 22:00:13") & Dumbo_gps$GPS.Fix.Time <= ("2020-03-04 12:00:11"),] 
#Dumbo_3_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2020-03-04 17:00:14") & Dumbo_gps$GPS.Fix.Time <= ("2020-09-01 12:00:17"),] 

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
Renee_4_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-12-09 20:00:15") & Renee_gps$GPS.Fix.Time <= ("2023-01-06 -----"),] #translocation not indicated on csv but is very apparent in data
#based on odd clusters of movement, will need to segment data for Tim as well
#clusters happened because his vest was taken off multiple times due to treatment recieved
Tim_1_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2020-10-28 12:59:32") & Tim_gps$GPS.Fix.Time <= ("2021-01-25 16:01:11"),] #maybe change hour but we'll see
#Tim_2_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2021-01-25 17:00:06") & Tim_gps$GPS.Fix.Time <= ("2021-10-13 14:00:07"),]
Tim_2_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2021-10-23 15:00:10") & Tim_gps$GPS.Fix.Time <= ("2022-02-03 19:00:23"),]
#Tim_4_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2022-02-03 20:00:06") & Tim_gps$GPS.Fix.Time <= ("2022-12-12 11:00:13"),]
Tim_3_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2022-05-28 12:59:58") & Tim_gps$GPS.Fix.Time <= ("2023-02-10 00:00:00"),]




#we will not include Dumbo_2, Dumbo_3, Tim_2, or Tim_4 in this analysis, for they reflect a stagnate GPS (i.e. animal is not active for a prolonged time so device has likely been removed and/or mortality has occurred)

#this creates a tibble to make analysis easier for the future and removes NA values using the is.na function
d <- tibble(animal = c('Arya','Beezie', 'Bella','Cláudio', 'Capitu', 'Dom', 'Dumbo', 'George', 'Heather','Jacobina', 'Juju_1', 'Juju_2','Mulan','Nancy', 'Nayeli', 'Peter', 'Rita', 'Renee_1', 'Renee_2', 'Renee_3', 'Renee_4', 'Tim_1', 'Tim_2', "Tim_3"),
            tel = purrr::map(animal, function(.name) get(paste0(.name, '_gps')) %>%
                               filter(! is.na(GPS.Latitude))))


d <- tibble(animal = c('Beezie','Cláudio', 'Dom', 'George', 'Jacobina','Nancy', 'Nayeli', 'Peter', 'Rita'),
            tel = purrr::map(animal, function(.name) get(paste0(.name, '_gps')) %>%
                               filter(! is.na(GPS.Latitude))))


#look at a basic plot of latitude and longitude
plot(x = orphan_anteater_gps$GPS.Latitude,
     y = orphan_anteater_gps$GPS.Longitude)

#begin diagnostic plots for anteaters to determine outliers using Steffano's functions
if(FALSE) { # initial diagnostic plots
  N <- nrow(d)
  
  for(i in 1:N) {
    cat('Animal', i, 'of', N, '.\n')
    png(filename = paste0('~/Giant_Anteater_Orphan/Raw_data/figures/',
                          d$animal[i], '.png'),
        width = 10, height = 10, units = 'in', res = 300)
    check_animal(id = d$animal[[i]], return_out = FALSE)
    dev.off()
  }
}

#outlier 0 = ok, 1 = outlier, 2 =unsure (come back and check)
unique(unnest(d, tel)$outlier)



#clean Arya's (709199A) data

#new
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 40, value = 1) # unreasonable speed in median deviation plot
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_distance = 5000, value = 1) # vertical line in distance plot
out <- check_animal("Arya") 
plot_adj("Arya", max_speed = 0.4)
plot_adj("Arya", max_speed = 0.4, max_angle = 179)
flag_outlier(id = "Arya", max_speed = 0.4, max_angle = 179, value = 1) # likely GPS errors
out <- check_animal("Arya") #ok 
flag_outlier(id = "Arya", max_speed = 0.2, max_angle = 135, value = 1) # test
out <- check_animal("Arya") #ok 
flag_outlier(id = "Arya", max_speed = 0.4, max_angle = 90, value = 1) # likely speed outliers
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.6, value = 1) # still likely outlier
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_distance = 1250, value = 1)
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.2, max_distance = 400, value = 1) # test 
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.15, max_dt = 2.5, value = 1) # test
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.1, max_angle = 135, value = 1) # test
out <- check_animal("Arya")
plot_adj("Arya", max_angle = 179) 
flag_outlier(id = "Arya", max_speed = 0.05, max_angle = 170, value = 1) # high speed for turning angle
out <- check_animal("Arya") 
flag_outlier(id = "Arya", max_speed = 0.025, max_angle = 175, value = 1) # high speed for turning angle
out <- check_animal("Arya") 
plot_adj("Arya", max_angle = 179) 
flag_outlier(id = "Arya", max_speed = 0.1, value = 1) #take out above plateau
out <- check_animal("Arya") 
plot_adj("Arya", max_angle = 179) 
flag_outlier(id = "Arya", max_angle = 150, max_speed = 0.05, value = 1)
out <- check_animal("Arya") 
plot_adj("Arya", max_angle = 179) 
flag_outlier(id = "Arya", max_distance = 200, max_angle = 160, value = 1) # test
out <- check_animal("Arya") 
plot_adj("Arya", max_angle = 179) 

flag_outlier(id = "Arya", max_distance = 200, max_angle = 150, value = 1) # test
out <- check_animal("Arya") #still large error circles but not much more to do
flag_outlier(id = "Arya", max_distance = 150, max_angle = 165, value = 1) # test
out <- check_animal("Arya") 
plot_adj("Arya", max_angle = 179) #clean
#total number of removed outliers is 169 (11.5% of dataset)
flag_outlier(id = "Arya", max_distance = 70, max_angle = 175, value = 1) # test
out <- check_animal("Arya")
plot_adj("Arya", max_angle = 179)
#total number of removed outliers is 210 (14.3% of dataset)



out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 40, value = 1) # unreasonable speed in median deviation plot
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_distance = 5000, value = 1) # vertical line in distance plot
out <- check_animal("Arya") 
plot_adj("Arya", max_speed = 0.4)
plot_adj("Arya", max_speed = 0.4, max_angle = 179)
flag_outlier(id = "Arya", max_speed = 0.4, max_angle = 179, value = 1) # likely GPS errors
out <- check_animal("Arya") #ok 
flag_outlier(id = "Arya", max_speed = 0.2, max_angle = 135, value = 1) # test
out <- check_animal("Arya") #ok 
flag_outlier(id = "Arya", max_speed = 0.4, max_angle = 90, value = 1) # likely speed outliers
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.6, value = 1) # still likely outlier
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_distance = 1250, value = 1)
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.2, max_distance = 400, value = 1) # test 
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.15, max_dt = 2.5, value = 1) # test
out <- check_animal("Arya")
flag_outlier(id = "Arya", max_speed = 0.1, max_angle = 135, value = 1) # test
out <- check_animal("Arya")
plot_adj("Arya", max_angle = 179) #large circles
flag_outlier(id = "Arya", max_speed = 0.05, max_angle = 175, value = 1) # high speed for turning angle
out <- check_animal("Arya") 
flag_outlier(id = "Arya", max_distance = 200, max_angle = 150, value = 1) # test
out <- check_animal("Arya") #still large error circles but not much more to do
flag_outlier(id = "Arya", max_distance = 150, max_angle = 165, value = 1) # test
out <- check_animal("Arya") 
plot_adj("Arya", max_angle = 179) #clean
#total number of removed outliers is 169 (11.5% of dataset)
flag_outlier(id = "Arya", max_distance = 70, max_angle = 175, value = 1) # test
out <- check_animal("Arya")
plot_adj("Arya", max_angle = 179)
#total number of removed outliers is 210 (14.3% of dataset)

#clean Beezie's data (72909A)
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Beezie")
flag_outlier(id = "Beezie", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Beezie")
plot_adj("Beezie", max_angle = 179)
flag_outlier(id = "Beezie", max_distance = 1500, max_angle = 175, value = 1)
out <- check_animal("Beezie")
plot_adj("Beezie", max_angle = 179)
#clean total # of removed outliers is 157 (16.3% of dataset)


#clean Bella's (729093A) data
out <- check_animal("Bella")
flag_outlier(id = "Bella", max_speed = 14, value = 1) #obvious speed outliers
out <- check_animal("Bella")
flag_outlier(id = "Bella", max_distance = 80000, value = 1) #distance outliers
out <- check_animal("Bella")
flag_outlier(id = "Bella", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Bella")
flag_outlier(id = "Bella", max_distance = 10000, value = 1) #remove these points as this is when she was likely recaptured to get her vest adjusted
out <- check_animal("Bella") #that removed



filter(d$tel[[which(d$animal == 'Bella')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()


#clean Capitu's (709201A) data
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 14, max_distance = 20000, value = 1) # high speed in median deviation plot at high distance
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_distance = 20000, value = 1)
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 3.5, max_angle = 135, value = 1) # high speed with high combined turning angle
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 1, max_distance = 13000, value = 1) # high speed with high deviation
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 0.4, max_angle = 135, value = 1) # high speed for turning angle
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 0.2, max_angle = 135, value = 1) # high speed for turning angle
out <- check_animal("Capitu")
plot_adj("Capitu", max_angle = 179) # still a few visible outliers
flag_outlier(id = "Capitu", max_speed = 0.1, max_angle = 170, value = 1) # high speed for turning angle
out <- check_animal("Capitu")
plot_adj("Capitu", max_angle = 179) 
flag_outlier(id = "Capitu", max_speed = 0.2, max_angle = 100, value = 1) # high speed for turning angle
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 0.1, max_angle = 170, value = 1) # high speed for turning angle
out <- check_animal("Capitu")
flag_outlier(id = "Capitu", max_speed = 0.05, max_angle = 175, value = 1) # high speed for turning angle
out <- check_animal("Capitu") #ok
plot_adj("Capitu", max_angle = 179) 
flag_outlier(id = "Capitu", max_distance = 1500, max_angle = 160, value = 1) # test
out <- check_animal("Capitu") #clean
plot_adj("Capitu", max_angle = 179) #clean
# total number of removed outliers is 355 (16.6% of dataset)

#clean Cláudio's (716785A) data
out <- check_animal("Cláudio")
flag_outlier(id = "Cláudio", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Cláudio")
flag_outlier(id = "Cláudio", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Cláudio")
flag_outlier(id = "Cláudio", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Cláudio")
plot_adj("Cláudio", max_angle = 179) #not terrible but needs a bit more for sure
flag_outlier(id = "Cláudio", max_distance = 800, max_angle = 175, value = 1)
out <- check_animal("Cláudio")
plot_adj("Cláudio", max_angle = 179) #not terrible but needs a bit more for sure
flag_outlier(id = "Cláudio", max_distance = 100, max_angle = 170, value = 1)
out <- check_animal("Cláudio")
plot_adj("Cláudio", max_angle = 179) #not terrible but needs a bit more for sure
flag_outlier(id = "Cláudio", max_distance = 50, max_angle = 177, value = 1)
out <- check_animal("Cláudio")
plot_adj("Cláudio", max_angle = 179) #better
flag_outlier(id = "Cláudio", max_distance = 800, max_angle = 170, value = 1)
out <- check_animal("Cláudio")
plot_adj("Cláudio", max_angle = 179) #better
flag_outlier(id = "Cláudio", max_distance = 100, max_angle = 177, value = 1)
out <- check_animal("Cláudio")
plot_adj("Cláudio", max_angle = 179) #better
#remove weird triangle
#clean total # of removed outliers is 56 (12.8% of dataset)

filter(d$tel[[which(d$animal == "Cláudio")]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()




#clean Dom's (736078A) data 
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_distance = 50000, value = 1) #clear distance outliers
out <- check_animal("Dom") #dom is dispersing a little bit maybe
flag_outlier(id = "Dom", max_speed = 0.4, max_angle = 90, value = 1) #speed and angle outliers
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_speed = 0.15, max_angle = 135, value = 1) #speed and angle outliers
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers
flag_outlier(id = "Dom", max_speed = 0.35, value = 1) #remove speed outlier
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_speed = 0.18, max_distance = 1500, value = 1)
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_speed = 0.1, max_angle = 135, value = 1) #speed and angle outliers
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but definitely better
flag_outlier(id = "Dom", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers
flag_outlier(id = "Dom", max_distance = 1400, max_speed = 0.175, value = 1) #likely still a distance outlier based on ping
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_distance = 500, max_angle = 167, value = 1)
out <- check_animal("Dom")
flag_outlier(id = "Dom", max_distance = 150, max_angle = 170, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but better
flag_outlier(id = "Dom", max_distance = 75, max_angle = 175, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but definitely better
flag_outlier(id = "Dom", max_distance = 50, max_angle = 177, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but definitely better
flag_outlier(id = "Dom", max_distance = 15, max_angle = 179, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but definitely better
flag_outlier(id = "Dom", max_speed = 0.1, max_angle = 135, value = 1) #speed and angle outliers
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but definitely better
flag_outlier(id = "Dom", max_distance = 100, max_angle = 170, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers but definitely better
flag_outlier(id = "Dom", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers
flag_outlier(id = "Dom", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers
flag_outlier(id = "Dom", max_distance = 15, max_angle = 175, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) #still too many outliers
flag_outlier(id = "Dom", max_distance = 150, max_angle = 170, value = 1)
out <- check_animal("Dom")
plot_adj("Dom", max_angle = 179) 
#clean 382 outliers removed (12.5%)









filter(d$tel[[which(d$animal == 'Dom')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()









#clean Dumbo's (709203A) site 1 data
#new
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 200000, value = 1) #clear distance outlier
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 2, max_angle = 135, value = 1) #high turning angles for speed
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 4000, max_speed = 1, value = 1) #test for outliers
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.4, max_angle = 90, value = 1) #possibly high speeds for turning angles
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.1, max_angle = 135, value = 1) #possibly high speeds for turning angles
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 2000, value = 1) #test for a distance outlier
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 1500, value = 1) #test 
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 1400, value = 1) #test on possible distance outlier
out <- check_animal("Dumbo") 
flag_outlier(id = "Dumbo", max_speed = 0.05, max_angle = 170, value = 1) # test
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.025, max_angle = 175, value = 1) # test
out <- check_animal("Dumbo")
plot_adj("Dumbo", max_angle = 179)
flag_outlier(id = "Dumob", max_angle = )






out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 200000, value = 1) #clear distance outlier
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 2, max_angle = 135, value = 1) #high turning angles for speed
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 4000, max_speed = 1, value = 1) #test for outliers
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.5, max_angle = 135, value = 1) #possibly high speeds for turning angles
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.3, max_angle = 150, value = 1) #still high
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 2000, value = 1) #test for a distance outlier
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 1500, value = 1) #test 
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.2, max_angle = 135, value = 1) #remove high speeds for high turning angles
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.3, max_angle = 90, value = 1) #speed outlier based on turning angle
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 1400, value = 1) #test on possible distance outlier
out <- check_animal("Dumbo") 
flag_outlier(id = "Dumbo", max_speed = 0.1, max_angle = 160, value = 1) # high speed for turning angle
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.05, max_angle = 175, value = 1) # high speed for turning angle
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 500, max_angle = 165, value = 1) # test
out <- check_animal("Dumbo")
plot_adj("Dumbo", max_angle = 180)
flag_outlier(id = "Dumbo", max_distance = 850, value = 1) # test 
out <- check_animal("Dumbo")
#plot_adj("Dumbo", max_angle = 179)
flag_outlier(id = "Dumbo", max_speed = 0.1, max_angle = 135, value = 1) # high speed for turning angle
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.05, max_angle = 170, value = 1) # test
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_speed = 0.1, max_angle = 120, value = 1) # high speed for turning angle
out <- check_animal("Dumbo")
flag_outlier(id = "Dumbo", max_distance = 500, max_angle = 150, value = 1) # test
out <- check_animal("Dumbo")
plot_adj("Dumbo", max_angle = 179)
flag_outlier(id = "Dumbo", max_distance = 300, max_angle = 165, value = 1) # test
out <- check_animal("Dumbo") #maybe ok
plot_adj("Dumbo", max_angle = 179)
flag_outlier(id = "Dumbo", max_distance = 150, max_angle = 170, value = 1) # test
out <- check_animal("Dumbo")
plot_adj("Dumbo", max_angle = 179)
flag_outlier(id = "Dumbo", max_distance = 100, max_angle = 175, value = 1) # test
out <- check_animal("Dumbo")
plot_adj("Dumbo", max_angle = 179)
flag_outlier(id = "Dumbo", max_distance = 50, max_angle = 178, value = 1) # test
out <- check_animal("Dumbo")
plot_adj("Dumbo", max_angle = 179) #clean
#total number of removed outliers is 711 (15.4% of dataset) 


#clean George's site (729106A) data
out <- check_animal("George")
flag_outlier(id = "George", max_distance = 10000, value = 1) #clear distance outlier
out <- check_animal("George")
flag_outlier(id = "George", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("George")
flag_outlier(id = "George", max_speed = 0.2, max_angle = 135, value = 1)
out <- check_animal("George")
flag_outlier(id = "George", max_speed = 0.05, max_angle = 175, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) #still outliers
#flag_outlier(id = "George", max_speed = 0.1, max_angle = 135, value = 1)
#out <- check_animal("George")
flag_outlier(id = "George", max_distance = 5000, max_angle = 170, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) #still outliers but definitely better
flag_outlier(id = "George", max_distance = 250, max_angle = 175, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_distance = 150, max_angle = 178, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_angle = 175, max_distance = 250, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_angle = 175, max_distance = 250, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179)
flag_outlier(id = "George", max_distance =1250, max_angle = 173, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179)


#5000 170
flag_outlier(id = "George", max_distance = 250, max_angle = 170, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_distance = 75, max_angle = 175, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_distance = 150, max_angle = 170, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_distance = 50, max_angle = 179, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) #better 

#flag_outlier(id = "George", max_distance = 75, max_angle = 175, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) 
flag_outlier(id = "George", max_distance = 50, max_angle = 179, value = 1)
out <- check_animal("George")
plot_adj("George", max_angle = 179) #better 


filter(d$tel[[which(d$animal == 'George')]], outlier == 0) %>%
  mutate(dec_date = decimal_date(as_datetime(GPS.Fix.Time))) %>%
  ggplot(aes(GPS.Longitude, GPS.Latitude)) +
  geom_path(lwd = 0.75) +
  geom_path(aes(color = dec_date)) +
  khroma::scale_color_smoothrainbow()



#clean Heather's data 
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_distance = 10000, value = 1) # clear distance outlier
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_speed = 0.4, max_angle = 90, value = 1) # high turning angles for speed
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_speed = 0.2, max_angle = 120, value = 1) # high turning angles for speed
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_speed = 0.05, max_angle = 175, value = 1) # high turning angles for speed
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_distance = 2300, max_angle = 155, value = 1) # test
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_angle = 165, max_distance = 1000, value = 1) # test
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_angle = 170, max_distance = 600, value = 1) # test
out <- check_animal("Heather")
plot_adj("Heather", max_angle = 179)
flag_outlier(id = "Heather", max_speed = 0.1, max_angle = 120, value = 1) # high turning angles for speed
out <- check_animal("Heather")
flag_outlier(id = "Heather", max_speed = 0.05, max_angle = 170, value = 1) # high turning angles for speed
out <- check_animal("Heather")
plot_adj("Heather", max_angle = 179)
flag_outlier(id = "Heather", max_angle = 170, max_distance = 200, value = 1) # test
out <- check_animal("Heather") 
flag_outlier(id = "Heather", max_angle = 175, max_distance = 100, value = 1) # test
out <- check_animal("Heather") 
flag_outlier(id = "Heather", max_angle = 178, max_distance = 50, value = 1) # test
out <- check_animal("Heather") 
plot_adj("Heather", max_angle = 179) #clean
#total number of removed outliers is 629 (15.3% of dataset)



#clean Juju's (687743B) data
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_speed = 10, max_angle = 90, value = 1) # high speed with turning angle
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_distance = 200000, value = 1) # clear distance outlier
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_speed = 12, value = 1) # still high speeds
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_distance = 10000, value = 1) # distance outlier
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_distance = 6000, max_speed = 1.2, value = 1) # test 
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_speed = 0.4, max_angle = 90, value = 1) # high turning angles for speed
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_speed = 0.2, max_angle = 135, value = 1) # high turning angles for speed
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_speed = 0.05, max_angle = 175, value = 1) # high turning angles for speed
#out <- check_animal("Juju")
#plot_adj("Juju", max_angle = 179)
#flag_outlier(id = "Juju", max_speed = 0.1, max_angle = 120, value = 1) # high turning angles for speed

#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_speed = 0.05, max_angle = 170, value = 1) # high turning angles for speed
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_angle = 160, max_distance = 1450, value = 1) # test
#out <- check_animal("Juju")
#plot_adj("Juju", max_angle = 179) 
#flag_outlier(id = "Juju", max_distance = 300, max_angle = 170, value = 1) # test
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_distance = 150, max_angle = 175, value = 1) # test
#out <- check_animal("Juju")
#flag_outlier(id = "Juju", max_distance = 75, max_angle = 178, value = 1) # test
#out <- check_animal("Juju")
#plot_adj("Juju", max_angle = 179) 
#total number of removed outliers is 625 (20.2% of dataset)

#clean Jacobina's (736260A) data
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_distance = 10000, value = 1) #distance outlier
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_speed = 0.4, max_angle = 90, value = 1) #distance outlier
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_speed = 0.1, max_angle = 135, value = 1) #distance outlier
out <- check_animal("Jacobina")
flag_outlier(id = "Jacobina", max_speed = 0.05, max_angle = 170, value = 1) #distance outlier
out <- check_animal("Jacobina")
plot_adj("Jacobina", max_angle = 179) #shockingly not bad
flag_outlier(id = "Jacobina", max_speed = 0.025, max_angle = 175, value = 1)
out <- check_animal("Jacobina")
plot_adj("Jacobina", max_angle = 179) #shockingly not bad


#Juju's 1st site
out <- check_animal("Juju_1")
flag_outlier(id = "Juju_1", max_distance = 13000, value = 1) #clear distance outlier
out <- check_animal("Juju_1")
flag_outlier(id = "Juju_1", max_speed = 0.4, max_angle = 90, value = 1) # high turning angles for speed
out <- check_animal("Juju_1")
flag_outlier(id = "Juju_1", max_speed = 0.2, max_angle = 135, value = 1) # high turning angles for speed
out <- check_animal("Juju_1")
flag_outlier(id = "Juju_1", max_speed = 0.05, max_angle = 175, value = 1) # high turning angles for speed
out <- check_animal("Juju_1")
flag_outlier(id = "Juju_1", max_speed = 0.05, max_angle = 170, value = 1) # high turning angles for speed
out <- check_animal("Juju_1")
flag_outlier(id = "Juju_1", max_angle = 160, max_distance = 1450, value = 1) # test
out <- check_animal("Juju_1")
plot_adj("Juju_1", max_angle = 179)
flag_outlier(id = "Juju_1", max_distance = 300, max_angle = 170, value = 1) # test
out <- check_animal("Juju_1")
plot_adj("Juju_1", max_angle = 179)
#total number of removed outliers is 354 (21% of dataset)

#Juju's second site
out <- check_animal("Juju_2")
flag_outlier(id = "Juju_2", max_distance = 10000, value = 1) #clear distance outlier
out <- check_animal("Juju_2")
flag_outlier(id = "Juju_2", max_distance = 1500, value = 1) #further distance outliers
out <- check_animal("Juju_2")
flag_outlier(id = "Juju_2", max_speed = 0.2, max_angle = 135, value = 1)
out <- check_animal("Juju_2")
flag_outlier(id = "Juju_2", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Juju_2")
plot_adj("Juju_2", max_angle = 179)
flag_outlier(id = "Juju_2", max_distance = 500, max_angle = 170, value = 1)
out <- check_animal("Juju_2")
plot_adj("Juju_2", max_angle = 179)
flag_outlier(id = "Juju_2", max_speed = 0.2, max_angle = 135, value = 1)
out <- check_animal("Juju_2")


#clean Mulan's (716786A) data
out <- check_animal("Mulan")
flag_outlier(id = "Mulan", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Mulan")
flag_outlier(id = "Mulan", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Mulan")
flag_outlier(id = "Mulan", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 500, max_angle = 177, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 250, max_angle = 175, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 100, max_angle = 177, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 50, max_angle = 179, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 50, max_angle = 177, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 35, max_angle = 179, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_distance = 35, max_angle = 179, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)
flag_outlier(id = "Mulan", max_angle = 90, max_speed = 0.15, value = 1)
out <- check_animal("Mulan")
plot_adj("Mulan", max_angle = 179)


#clean Nancy's data
out <- check_animal("Nancy")
flag_outlier(id = "Nancy", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Nancy")
flag_outlier(id = "Nancy", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Nancy")
flag_outlier(id = "Nancy", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Nancy")
plot_adj("Nancy", max_angle = 179) #not terrible but clearly still needs work
flag_outlier(id = "Nancy", max_distance = 250, max_angle = 175, value = 1)
out <- check_animal("Nancy")
plot_adj("Nancy", max_angle = 179) #not terrible but clearly still needs work
flag_outlier(id = "Nancy", max_distance = 100, max_angle = 177, value = 1)
out <- check_animal("Nancy")
plot_adj("Nancy", max_angle = 179) #not terrible but clearly still needs work
flag_outlier(id = "Nancy", max_distance = 950, max_angle = 170, value = 1)
out <- check_animal("Nancy")
plot_adj("Nancy", max_angle = 179) #not terrible but clearly still needs work
#total # of removed outliers is 132 (13.7% of dataset)




#clean Nayeli's (729091A) data
out <- check_animal("Nayeli")
flag_outlier(id = "Nayeli", max_distance = 10000, value = 1)
out <- check_animal("Nayeli")
flag_outlier(id = "Nayeli", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Nayeli")
flag_outlier(id = "Nayeli", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Nayeli")
flag_outlier(id = "Nayeli", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Nayeli")
plot_adj("Nayeli", max_angle = 179)
flag_outlier(id = "Nayeli", max_distance = 50, max_angle = 177, value = 1)
out <- check_animal("Nayeli")
plot_adj("Nayeli", max_angle = 179)
#clean total # of removed outliers is 201 (13.4% of dataset)


#clean Peter's data 
out <- check_animal("Peter")
flag_outlier(id = "Peter", max_distance = 20000, value = 1) #clear distance outlier
out <- check_animal("Peter")
flag_outlier(id = "Peter", max_speed = 0.4, max_angle = 90, value = 1) #angle speed outliers
out <- check_animal("Peter")
flag_outlier(id = "Peter", max_speed = 0.2, max_angle = 135, value = 1) #angle speed outliers
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #not terrible but not good
flag_outlier(id = "Peter", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_speed = 0.1, max_angle = 135, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_distance = 1500, max_angle = 175, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_distance = 1000, max_angle = 175, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_distance = 250, max_angle = 175, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready



flag_outlier(id = "Peter", max_distance = 1500, max_angle = 175, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_distance = 1000, max_angle = 175, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
#flag_outlier(id = "Peter", max_distance = 750, max_angle = 170, value = 1) #worse
#out <- check_animal("Peter")
#plot_adj("Peter", max_angle = 179) 
flag_outlier(id = "Peter", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_speed = 0.05, max_angle = 160, value = 1)
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
flag_outlier(id = "Peter", max_distance = 250, max_angle = 170, value = 1) #test
out <- check_animal("Peter")
plot_adj("Peter", max_angle = 179) #better but not ready
#clean total # of removed ouliers is 210 (20.4% of dataset)

#clean Rita's data
out <- check_animal("Rita")
flag_outlier(id = "Rita", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Rita")
flag_outlier(id = "Rita", max_speed = 0.1, max_angle = 135, value = 1)
out <- check_animal("Rita")
flag_outlier(id = "Rita", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Rita")
plot_adj("Rita", max_angle = 179)
flag_outlier(id = "Rita", max_angle = 175, max_distance = 250, value = 1)
out <- check_animal("Rita")
plot_adj("Rita", max_angle = 179)
#clean total # of removed outliers is 126 (22.7% of dataset)



#clean Renee's  site 1 data
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_distance = 100000, value = 1) #clear distance outlier
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_speed = 0.4, max_angle = 90, value = 1) #high speed and turning angles
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_speed = 0.2, max_angle = 135, value = 1) #high speed and turning angles
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_speed = 0.05, max_angle = 175, value = 1) #high speed and turning angles
out <- check_animal("Renee_1")
plot_adj("Renee_1", max_angle = 179) #keep going
flag_outlier(id = "Renee_1", max_distance = 1000, max_angle = 160, value = 1) #test
out <- check_animal("Renee_1")
plot_adj("Renee_1", max_angle = 179) #keep going
flag_outlier(id = "Renee_1", max_distance = 50, max_angle = 175, value = 1) # test
out <- check_animal("Renee_1")
plot_adj("Renee_1", max_angle = 179) #keep going
#total # of outliers removed was 104 (17.9% of dataset)

#flag_outlier(id = "Renee_1", max_speed = 0.1, max_angle = 120, value = 1) #high speed and turning angles
#out <- check_animal("Renee_1")
#flag_outlier(id = "Renee_1", max_distance = 100, max_angle = 165, value = 1) # test
#out <- check_animal("Renee_1")
#flag_outlier(id = "Renee_1", max_distance = 75, max_angle = 170, value = 1) # test
#out <- check_animal("Renee_1")
#flag_outlier(id = "Renee_1", max_distance = 50, max_angle = 175, value = 1) # test
#out <- check_animal("Renee_1")
#flag_outlier(id = "Renee_1", max_distance = 30, max_angle = 178, value = 1) # test
#out <- check_animal("Renee_1")
#plot_adj("Renee_1", max_angle = 179) #ok
#flag_outlier(id = "Renee_1", max_speed = 0.07, max_angle = 135, value = 1)
#out <- check_animal("Renee_1")
#plot_adj("Renee_1", max_angle = 179) #ok
# total number of removed outliers is 204 (18.1% of dataset)


#clean Renee's site 2 data
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.4, max_angle = 90, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.2, max_angle = 135, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.05, max_angle = 175, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.4, max_angle = 75, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.1, max_angle = 120, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.05, max_angle = 170, value = 1) #high speed for turning angle
out <- check_animal("Renee_2") 
flag_outlier(id = "Renee_2", max_distance = 250, max_angle = 165, value = 1) # test
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_distance = 100, max_angle = 170, value = 1) # test
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_distance = 100, max_angle = 175, value = 1) # test
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_distance = 75, max_angle = 178, value = 1) # test
out <- check_animal("Renee_2")
plot_adj("Renee_2", max_angle = 179)
# total number of removed outliers is 589 (18.3% of dataset)


#clean Renee's site 3 data
out <- check_animal("Renee_3")
flag_outlier(id = "Renee_3", max_distance = 10000, value = 1) #clear distance outlier
out <- check_animal("Renee_3")
flag_outlier(id = "Renee_3", max_speed = 0.4, max_angle = 90, value = 1) #high speeds and turning angles
out <- check_animal("Renee_3")
flag_outlier(id = "Renee_3", max_speed = 0.1, max_angle = 120, value = 1) #high speeds and turning angles
out <- check_animal("Renee_3") 
flag_outlier(id = "Renee_3", max_speed = 0.05, max_angle = 170, value = 1) #high speed for turning angle
out <- check_animal("Renee_3")
plot_adj("Renee_3", max_angle = 179)
flag_outlier(id = "Renee_3", max_angle = 165, max_distance = 350, value = 1) # test
out <- check_animal("Renee_3")
flag_outlier(id = "Renee_3", max_angle = 170, max_distance = 150, value = 1) # test
out <- check_animal("Renee_3")
flag_outlier(id = "Renee_3", max_angle = 175, max_distance = 75, value = 1) #test
out <- check_animal("Renee_3")
plot_adj("Renee_3", max_angle = 179) 
# total number of removed outliers is 66 (17.4% of dataset)


#clean Renee's site 4 data
out <- check_animal("Renee_4")
flag_outlier(id = "Renee_4", max_distance = 14000, value = 1) #clear distance outlier
out <- check_animal("Renee_4")
flag_outlier(id = "Renee_4", max_speed = 0.6, max_angle = 60, value = 1) #high speed for turning angle
out <- check_animal("Renee_4")
flag_outlier(id = "Renee_4", max_speed = 0.4, max_angle = 90, value = 1) #high speed for turning angle
out <- check_animal("Renee_4")
flag_outlier(id = "Renee_4", max_speed = 0.1, max_angle = 120, value = 1) #high speed for turning angle
out <- check_animal("Renee_4")
flag_outlier(id = "Renee_4", max_speed = 0.05, max_angle = 170, value = 1) #high speed for turning angle
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
flag_outlier(id = "Renee_4", max_speed = 0.35, max_angle = 75, value = 1) #potential speed outlier
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
#flag_outlier("Renee_4", max_distance = 100, max_angle = 165, value = 1) #test
#out <- check_animal("Renee_4")
#plot_adj("Renee_4", max_angle = 179)
#flag_outlier("Renee_4", max_distance = 50, max_angle = 170, value = 1)
#out <- check_animal("Renee_4")
#plot_adj("Renee_4", max_angle = 179)

#total number of removed outliers is 80 (29.6% of dataset)




#clean Tim's site 1 data
out <- check_animal("Tim_1")
flag_outlier(id = "Tim_1", max_distance = 50000, value = 1) #clear distance outlier
out <- check_animal("Tim_1")
flag_outlier(id = "Tim_1", max_speed = 0.4, max_angle = 90, value = 1) #high speed for turning angle
out <- check_animal("Tim_1")
flag_outlier(id = "Tim_1", max_speed = 0.1, max_angle = 120, value = 1) #high speed for turning angle
out <- check_animal("Tim_1")
flag_outlier(id = "Tim_1", max_speed = 0.05, max_angle = 170, value = 1) #high speed for turning angle
out <- check_animal("Tim_1")
flag_outlier(id = "Tim_1", max_angle = 175, max_distance = 10000, value = 1) #test
out <- check_animal("Tim_1")
plot_adj("Tim_1", max_angle = 179)
flag_outlier(id = "Tim_1", max_distance = 14000, max_angle = 175, value = 1) #test
out <- check_animal("Tim_1")
plot_adj("Tim_1", max_angle = 179) 
# total number of removed outliers is 234 (16.2% of dataset)


#clean Tim's site 2 data
out <- check_animal("Tim_2")
flag_outlier(id = "Tim_2", max_distance = 3500, value = 1) #clear distance outlier
out <- check_animal("Tim_2")
flag_outlier(id = "Tim_2", max_speed = 0.4, max_angle = 90, value = 1)
out <- check_animal("Tim_2")
flag_outlier(id = "Tim_2", max_speed = 0.2, max_angle = 132, value = 1)
out <- check_animal("Tim_2")
flag_outlier(id = "Tim_2", max_speed = 0.05, max_angle = 170, value = 1)
out <- check_animal("Tim_2")
plot_adj("Tim_2", max_angle = 179) #keep going
flag_outlier(id = "Tim_2", max_speed = 0.1, max_angle = 150, value = 1)
out <- check_animal("Tim_2")
plot_adj("Tim_2", max_angle = 179) #keep going
flag_outlier(id = "Tim_2", max_angle = 165, max_distance = 150, value = 1) #test
out <- check_animal("Tim_2")
plot_adj("Tim_2", max_angle = 179)
#total # of removed outliers is 254 (10.9% of dataset)

#clean Tim's site 3 data ---------------i dont think i should even do this -----------------------------------
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 20000, value = 1)
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 10000, value = 1)
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 5000, value = 1)
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 1000, max_angle = 160, value = 1)
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 500, max_angle = 150, value = 1)
out <- check_animal("Tim_3")
plot_adj("Tim_3", max_angle = 180)
flag_outlier(id = "Tim_3", max_distance = 3000, value = 1)
out <- check_animal("Tim_3")
plot_adj("Tim_3", max_angle = 180)

#save cleaned data
saveRDS(object = d,
        file = paste0("~/Giant_Anteater_Orphan/FIXED/Data/clean_NEW_GPS_data",
                      format(Sys.time(), '%Y-%m-%d-%H-%M'),
                      '.rds'))





#make a csv file for Dr.Noonan

