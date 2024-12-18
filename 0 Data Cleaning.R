

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
Arya_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/709199A_Datalog.csv", skip = 22)
Capitu_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/709201A_Datalog.csv", skip = 22)
Dumbo_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/709203A_Datalog.csv", skip = 22)
Heather_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/Heather.csv", skip = 22)
Juju_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/687743B_Datalog.csv", skip = 22)
Renee_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/687746B_Datalog.csv", skip = 22)
Tim_gps <- read.csv ("~/Giant_Anteater_Orphan/Data/Raw_Data/tim.csv", skip = 22)

#remove NA values using is.na function
Arya_gps <- Arya_gps [!is.na(Arya_gps$GPS.Latitude),]
Capitu_gps <- Capitu_gps [!is.na(Capitu_gps$GPS.Latitude),]
Dumbo_gps <- Dumbo_gps [!is.na(Dumbo_gps$GPS.Latitude),]
Heather_gps <- Heather_gps [!is.na(Heather_gps$GPS.Latitude),]
Juju_gps <- Juju_gps [!is.na(Juju_gps$GPS.Latitude),]
Renee_gps <- Renee_gps [!is.na(Renee_gps$GPS.Latitude),]
Tim_gps <- Tim_gps [!is.na(Tim_gps$GPS.Latitude),]

#format time stamp
Arya_gps$GPS.Fix.Time <- as.POSIXct(Arya_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Capitu_gps$GPS.Fix.Time <- as.POSIXct(Capitu_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Dumbo_gps$GPS.Fix.Time <- as.POSIXct(Dumbo_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Heather_gps$GPS.Fix.Time <- as.POSIXct(Heather_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Juju_gps$GPS.Fix.Time <- as.POSIXct(Juju_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Renee_gps$GPS.Fix.Time <- as.POSIXct(Renee_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")
Tim_gps$GPS.Fix.Time <- as.POSIXct(Tim_gps$GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S")

#based on a glance of the GPS data, collar was likely active before placed on anteater, so these points need to be excluded
Arya_gps <- Arya_gps[Arya_gps$GPS.Latitude < 0,]
Capitu_gps <- Capitu_gps[Capitu_gps$GPS.Latitude < 0,]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$GPS.Latitude < 0,]
Heather_gps <- Heather_gps[Heather_gps$GPS.Latitude < 0,]
Juju_gps <- Juju_gps[Juju_gps$GPS.Latitude < 0,]
Renee_gps <- Renee_gps[Renee_gps$GPS.Latitude < 0,]
Tim_gps <- Tim_gps[Tim_gps$GPS.Latitude < 0,]

#need to also subset data that was collected predeployment of collar and animal
Arya_gps <- Arya_gps[Arya_gps$Predeployment.Data == "No",]
Capitu_gps <- Capitu_gps[Capitu_gps$Predeployment.Data == "No",]
Dumbo_gps <- Dumbo_gps[Dumbo_gps$Predeployment.Data == "No",]
Heather_gps <- Heather_gps[Heather_gps$Predeployment.Data == "No",]
Juju_gps <- Juju_gps[Juju_gps$Predeployment.Data == "No",]
Renee_gps <- Renee_gps[Renee_gps$Predeployment.Data == "No",]
Tim_gps <- Tim_gps[Tim_gps$Predeployment.Data == "No",]


#add outlier column for each of the anteaters to use Stefano's plot_adj function
Arya_gps$outlier <- replicate (1473, 0)
Capitu_gps$outlier <- replicate (2191, 0)
Dumbo_gps$outlier <- replicate (8229, 0)
Heather_gps$outlier <- replicate (4106, 0)
Juju_gps$outlier <- replicate (3190, 0)
Renee_gps$outlier <- replicate (6500, 0)
Tim_gps$outlier <- replicate (8865, 0)


#based on odd clusters of movement, will need to segment data for Dumbo
Dumbo_1_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2019-05-29 19:00:19") & Dumbo_gps$GPS.Fix.Time <= ("2019-12-23 21:00:07"),] 
Dumbo_2_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2019-12-23 22:00:13") & Dumbo_gps$GPS.Fix.Time <= ("2020-03-04 12:00:11"),] 
Dumbo_3_gps <- Dumbo_gps[Dumbo_gps$GPS.Fix.Time >= ("2020-03-04 17:00:14") & Dumbo_gps$GPS.Fix.Time <= ("2020-09-01 12:00:17"),] 

#based on movement data, Renee was moved a couple of times, so will need to segment data
Renee_1_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-03-04 16:02:34") & Renee_gps$GPS.Fix.Time <= ("2021-05-16 14:40:09"),]
Renee_2_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-05-16 16:01:29") & Renee_gps$GPS.Fix.Time <= ("2021-12-09 18:40:09"),]
Renee_3_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2021-12-09 20:00:15") & Renee_gps$GPS.Fix.Time <= ("2022-01-04 17:20:14"),]
Renee_4_gps <- Renee_gps[Renee_gps$GPS.Fix.Time >= ("2022-01-04 18:40:11") & Renee_gps$GPS.Fix.Time <= ("2023-01-13 02:39:59"),]


#based on odd clusters of movement, will need to segment data for Tim as well
Tim_1_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2020-10-28 12:59:32") & Tim_gps$GPS.Fix.Time <= ("2021-01-25 16:01:11"),]
Tim_2_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2021-01-25 17:00:06") & Tim_gps$GPS.Fix.Time <= ("2021-10-13 14:00:07"),]
Tim_3_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2021-10-13 15:00:10") & Tim_gps$GPS.Fix.Time <= ("2022-02-03 19:00:23"),]
Tim_4_gps <- Tim_gps[Tim_gps$GPS.Fix.Time >= ("2022-02-03 20:00:06") & Tim_gps$GPS.Fix.Time <= ("2022-12-12 11:00:13"),]


#combine the data into one dataframe to make cleaning easier
orphan_anteater_gps <- rbind(Arya_gps,
                             Capitu_gps,
                             Dumbo_gps,
                             Heather_gps,
                             Juju_gps,
                             Renee_gps,
                             Tim_gps)

#we will not include Dumbo_2, Dumbo_3, Tim_2, or Tim_4 in this analysis, for they reflect a stagnate GPS (i.e. animal is not active for a prolonged time so device has likely been removed and/or mortality has occurred)

#this creates a tibble to make analysis easier for the future and removes NA values using the is.na function
d <- tibble(animal = c('Arya', 'Capitu', 'Dumbo_1', 'Heather', 'Juju', 'Renee_1', 'Renee_2', 'Renee_3', 'Renee_4', 'Tim_1', 'Tim_3'),
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
# total number of removed outliers is 315 (14.4% of dataset)


#clean Dumbo's (709203A) site 1 data
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 200000, value = 1) #clear distance outlier
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 2, max_angle = 135, value = 1) #high turning angles for speed
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 4000, max_speed = 1, value = 1) #test for outliers
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.5, max_angle = 135, value = 1) #possibly high speeds for turning angles
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.3, max_angle = 150, value = 1) #still high
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 2000, value = 1) #test for a distance outlier
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 1500, value = 1) #test 
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.2, max_angle = 135, value = 1) #remove high speeds for high turning angles
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.3, max_angle = 90, value = 1) #speed outlier based on turning angle
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 1400, value = 1) #test on possible distance outlier
out <- check_animal("Dumbo_1") 
flag_outlier(id = "Dumbo_1", max_speed = 0.1, max_angle = 160, value = 1) # high speed for turning angle
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.05, max_angle = 175, value = 1) # high speed for turning angle
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 500, max_angle = 165, value = 1) # test
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", max_angle = 180)
flag_outlier(id = "Dumbo_1", max_distance = 850, value = 1) # test 
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", max_angle = 179)
flag_outlier(id = "Dumbo_1", max_speed = 0.1, max_angle = 135, value = 1) # high speed for turning angle
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.05, max_angle = 170, value = 1) # test
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_speed = 0.1, max_angle = 120, value = 1) # high speed for turning angle
out <- check_animal("Dumbo_1")
flag_outlier(id = "Dumbo_1", max_distance = 500, max_angle = 150, value = 1) # test
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", max_angle = 179)
flag_outlier(id = "Dumbo_1", max_distance = 300, max_angle = 165, value = 1) # test
out <- check_animal("Dumbo_1") #maybe ok
plot_adj("Dumbo_1", max_angle = 179)
flag_outlier(id = "Dumbo_1", max_distance = 150, max_angle = 170, value = 1) # test
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", max_angle = 179)
flag_outlier(id = "Dumbo_1", max_distance = 100, max_angle = 175, value = 1) # test
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", max_angle = 179)
flag_outlier(id = "Dumbo_1", max_distance = 50, max_angle = 178, value = 1) # test
out <- check_animal("Dumbo_1")
plot_adj("Dumbo_1", max_angle = 179) #clean
#total number of removed outliers is 721 (15.7% of dataset) 


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
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_speed = 10, max_angle = 90, value = 1) # high speed with turning angle
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_distance = 200000, value = 1) # clear distance outlier
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_speed = 12, value = 1) # still high speeds
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_distance = 10000, value = 1) # distance outlier
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_distance = 6000, max_speed = 1.2, value = 1) # test 
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_speed = 0.4, max_angle = 90, value = 1) # high turning angles for speed
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_speed = 0.2, max_angle = 135, value = 1) # high turning angles for speed
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_speed = 0.05, max_angle = 175, value = 1) # high turning angles for speed
out <- check_animal("Juju")
plot_adj("Juju", max_angle = 179)
flag_outlier(id = "Juju", max_speed = 0.1, max_angle = 120, value = 1) # high turning angles for speed
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_speed = 0.05, max_angle = 170, value = 1) # high turning angles for speed
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_angle = 160, max_distance = 1450, value = 1) # test
out <- check_animal("Juju")
plot_adj("Juju", max_angle = 179) 
flag_outlier(id = "Juju", max_distance = 300, max_angle = 170, value = 1) # test
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_distance = 150, max_angle = 175, value = 1) # test
out <- check_animal("Juju")
flag_outlier(id = "Juju", max_distance = 75, max_angle = 178, value = 1) # test
out <- check_animal("Juju")
plot_adj("Juju", max_angle = 179) 
#total number of removed outliers is 720 (22.6% of dataset)


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
flag_outlier(id = "Renee_1", max_distance = 1000, max_angle = 160, value = 1) #test
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_speed = 0.1, max_angle = 120, value = 1) #high speed and turning angles
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_speed = 0.05, max_angle = 170, value = 1) #high speed and turning angles
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_distance = 100, max_angle = 165, value = 1) # test
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_distance = 75, max_angle = 170, value = 1) # test
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_distance = 50, max_angle = 175, value = 1) # test
out <- check_animal("Renee_1")
flag_outlier(id = "Renee_1", max_distance = 30, max_angle = 178, value = 1) # test
out <- check_animal("Renee_1")
plot_adj("Renee_1", max_angle = 179) #ok
flag_outlier(id = "Renee_1", max_speed = 0.07, max_angle = 135, value = 1)
out <- check_animal("Renee_1")
plot_adj("Renee_1", max_angle = 179) #ok
# total number of removed outliers is 162 (14.3% of dataset)


#clean Renee's site 2 data
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 10, value = 1) #speed seems suspicious on median deviation plot
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 6, value = 1) #speed is suspicious on median deviation plot
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.4, max_angle = 90, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.2, max_angle = 135, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_speed = 0.05, max_angle = 175, value = 1) #high speed for turning angle
out <- check_animal("Renee_2")
flag_outlier(id = "Renee_2", max_distance = 100000, value = 1) #unreasonable distance traveled during time period 
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
flag_outlier(id = "Renee_4", max_speed = 0.35, max_angle = 75, value = 1) #potential speed outlier
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
flag_outlier("Renee_4", max_distance = 100, max_angle = 165, value = 1) #test
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
flag_outlier("Renee_4", max_distance = 50, max_angle = 170, value = 1)
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
flag_outlier("Renee_4", max_distance = 15, max_angle = 175, value = 1) #test
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
flag_outlier(id = "Renee_4", max_speed = 0.1, max_angle = 120, value = 1) #high speed for turning angle
out <- check_animal("Renee_4")
flag_outlier(id = "Renee_4", max_speed = 0.05, max_angle = 170, value = 1) #high speed for turning angle
out <- check_animal("Renee_4")
plot_adj("Renee_4", max_angle = 179)
#total number of removed outliers is 602 (33.7% of dataset)


#clean Tim's site 1 data
out <- check_animal("Tim_1")
flag_outlier(id = "Tim_1", max_distance = 50000, value = 1) #clear distancce outlier
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
flag_outlier(id = "Tim_1", max_distance = 6000, max_angle = 160, value = 1) #test
out <- check_animal("Tim_1")
plot_adj("Tim_1", max_angle = 179) 
# total number of removed outliers is 234 (12.9% of dataset)


#clean Tim's site 3 data
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 10000, value = 1) #clear distance outlier
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_distance = 3500, value = 1) #clear distance outlier
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_speed = 0.1, max_angle = 120, value = 1) #high speed for turning angle
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 170, max_speed = 0.05, value = 1) #high speed for turning angle
out <- check_animal("Tim_3")
plot_adj("Tim_3", max_angle = 179)
flag_outlier(id = "Tim_3", max_angle = 160, max_distance = 400, value = 1) #test
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 165, max_distance = 150, value = 1) #test
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 170, max_distance = 100, value = 1) #test
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 175, max_distance = 75, value = 1) #test
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 178, max_distance = 20, value = 1) #test 
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 175, max_distance = 500, value = 1) #test
out <- check_animal("Tim_3")
plot_adj("Tim_3", max_angle = 179)
flag_outlier(id = "Tim_3", max_speed = 0.09, max_angle = 120, value = 1) #high speed for turning angle
out <- check_animal("Tim_3")
flag_outlier(id = "Tim_3", max_angle = 170, max_speed = 0.045, value = 1) #high speed for turning angle
out <- check_animal("Tim_3")
plot_adj("Tim_3", max_angle = 179)
#total number of removed outliers is 361 (11.6% of dataset)



#save cleaned data
saveRDS(object = d,
file = paste0("~/Giant_Anteater_Orphan/Data/clean_GPS_data",
              format(Sys.time(), '%Y-%m-%d-%H-%M'),
              '.rds'))











