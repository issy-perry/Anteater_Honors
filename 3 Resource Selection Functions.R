#load packages 
library(ctmm) #analysis
library(terra) #working with spatrasters before turning into rasterclasses
library(raster) #turn spatrasters into raster classes before running analysis


#load data
load("~/Giant_Anteater_Orphan/FIXED/Data/Data_telemetry.rda") #telemetry object of orphan movement data (including those who disperse)
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Orphaned/wAKDEs.rda") #AKDEs of orphaned anteaters (including those who disperse)
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda") #telemetry objects of wild-raised, non-orphaned movement data
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Wild_raised/Lists/AKDE_wild_total.rda") # AKDEs of wild-raised, non-orphaned

#rename for convenience
AKDE_orphan <- AKDEs
DATA_orphan <- DATA_list
#AKDE_wild <- AKDE
DATA_wild <- DATA_TELEMETRY
rm(AKDE, DATA_list, DATA_TELEMETRY, AKDEs) #clear up environment space


#preparing data to be separated by years
DATA_17 <- DATA_wild[c(3,5,6,7,8,10,12,14,16,19)] #Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, and Puji
DATA_18 <- DATA_wild[c(1,2,4,9,11,13,15,17,18,20,21,22,23)] #Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, and Thomas
DATA_19 <- DATA_orphan[c(1,2,3)] #Arya, Capitu, and Dumbo
DATA_20 <- DATA_orphan[c(10)] #Tim_1
DATA_21 <- DATA_orphan[c(6,7,8,11)] #Renee_1, Renee_2, Renee_3, and Tim_3
DATA_22 <- DATA_orphan[c(4,5,9)] #Heather, Juju, and Renee_4
AKDE_17 <- AKDE_wild[c(3,5,6,7,8,10,12,14,16,19)] #Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, and Puji
AKDE_18 <- AKDE_wild[c(1,2,4,9,11,13,15,17,18,20,21,22,23)] #Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, and Thomas
AKDE_19 <- AKDE_orphan[c(1,2,3)] #Arya, Capitu, and Dumbo
AKDE_20 <- AKDE_orphan[c(10)] #Tim_1
AKDE_21 <- AKDE_orphan[c(6,7,8,11)] #Renee_1, Renee_2, Renee_3, and Tim_3
AKDE_22 <- AKDE_orphan[c(4,5,9)] #Heather, Juju, and Renee_4
rm(DATA_wild, DATA_orphan, AKDE_wild, AKDE_orphan) #clear up environment space
#make lists for new individuals to run
DATA_NEW_17 <- DATA_wild[c(26)]
DATA_NEW_18 <- DATA_wild[c(24,25)]
AKDE_NEW_17 <- AKDE_wild[c(26)]
AKDE_NEW_18 <- AKDE_wild[c(24,25)]

#converting Spatrasters to rasterclasses 2017 ----
cover_2017 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2017_cover.tif")
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2017
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2017
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2017
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2017
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2017
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2017
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2017) #free up environment space



#RSF fit for 2017 ---- 
# Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, and Puji
RSF_17 <- list()
for(i in 1:length(DATA_17)){
  
  #extract individual
  DATA <- DATA_17[[i]]
  
  #extract AKDE
  AKDE <- AKDE_17[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSFs
  RSF_17[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)

} #close the loop


names(RSF_17) <- names(DATA_17)
save(RSF_17, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2017.rda")
rm(RSF_17, DATA_17, AKDE_17) #free environment space 

#converting Spatrasters to rasterclasses 2018 ----
cover_2018 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2018_cover.tif")
Native_Forest <- cover_2018
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
plot(Native_Forest)
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2018
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
plot(Forestry)
Forestry <- raster(Forestry)
Pasture <- cover_2018
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
plot(Pasture)
Pasture <- raster(Pasture)
Agriculture <- cover_2018
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
plot(Agriculture)
Agriculture <- raster(Agriculture)
Development <- cover_2018
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
plot(Development)
Development <- raster(Development)
Water <- cover_2018
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
plot(Water)
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2018) #free environment space



#RSF fit for 2018 ----
#Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, and Thomas
RSF_18 <- list()
for(i in 1:length(DATA_18)){
  #extract individual
  DATA <- DATA_18[[i]]
  
  #extract AKDE
  AKDE <- AKDE_18[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_18[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop


names(RSF_18) <- names(DATA_18)
save(RSF_18, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2018.rda")
rm(RSF_18, DATA_18, AKDE_18) #save environment space 



#converting Spatrasters to rasterclasses 2019 ----
cover_2019 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2019_cover.tif")
Native_Forest <- cover_2019
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2019
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2019
Pasture[Pasture %notin% c(12,15)] <- FALSE 
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2019
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2019
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2019
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2019) #free environment space


#RSF fit for 2019 ----
#Arya, Capitu, and Dumbo
RSF_19 <- list()
for(i in 1:length(DATA_19)){
  #extract individual
  DATA <- DATA_19[[i]]
  
  #extract AKDE
  AKDE <- AKDE_19[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_19[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop

names(RSF_19) <- names(DATA_19)
save(RSF_19, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2019.rda")
rm(RSF_19, DATA_19, AKDE_19) #save environment space 


#converting Spatrasters to rasterclasses 2020 ----
cover_2020 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2020_cover.tif")
Native_Forest <- cover_2020
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2020
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2020
Pasture[Pasture %notin% c(12,15)] <- FALSE 
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2020
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2020
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2020
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2020) #free environment space

#RSF fit for 2020 ----
#Tim_1
RSF_20 <- list()
for(i in 1:length(DATA_20)){
  
  #extract individual
  DATA <- DATA_20[[i]]
  
  #extract AKDE
  AKDE <- AKDE_20[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_20[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop

names(RSF_20) <- names(DATA_20)
save(RSF_20, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2020.rda")
rm(RSF_20, AKDE_20, DATA_20) #save environment space 





#converting Spatrasters to rasterclasses 2021 ----
cover_2021 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2021_cover.tif")
Native_Forest <- cover_2021
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2021
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2021
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2021
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2021
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2021
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2021) #free environment space


#RSF fit for 2021 ----
#Renee_1, Renee_2, Renee_3, and Tim_3
RSF_21 <- list()
for(i in 1:length(DATA_21)){
  
  #extract individual
  DATA <- DATA_21[[i]]
  
  #extract AKDE
  AKDE <- AKDE_21[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_21[[i]] <- rsf.select(DATA, UD = AKDE, R = covers) 
  
  
} #close the loop
names(RSF_21) <- names(DATA_21)
save(RSF_21, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2021.rda")
rm(RSF_21, DATA_21, AKDE_21) #save environment space 





#converting Spatrasters to rasterclasses 2017 ----
cover_2022 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2022_cover.tif")
Native_Forest <- cover_2022
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2022
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2022
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2022
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2022
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2022
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
plot(Water)

rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2022) #free environment space

#RSF fit for 2022 ----
#Heather, Juju, and Renee_4
RSF_22 <- list()
for(i in 1:length(DATA_22)){
  
  #extract individual
  DATA <- DATA_22[[i]]
  
  #extract AKDE
  AKDE <- AKDE_22[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_22[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
  
} #close the loop
names(RSF_22) <- names(DATA_22)
save(RSF_22, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2022.rda")









#new giant anteaters added --------------------------------------------------------------------------------------------------

#converting Spatrasters to rasterclasses 2017 ----
cover_2017 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2017_cover.tif")
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2017
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2017
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2017
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2017
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2017
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2017
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2017) #free up environment space



#RSF fit for 2017 ---- 
# Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, and Puji
RSF_NEW_17 <- list()
for(i in 1:length(DATA_NEW_17)){
  
  #extract individual
  DATA <- DATA_NEW_17[[i]]
  
  #extract AKDE
  AKDE <- AKDE_NEW_17[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSFs
  RSF_NEW_17[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop


names(RSF_NEW_17) <- names(DATA_NEW_17)

#converting Spatrasters to rasterclasses 2018 ----
cover_2018 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2018_cover.tif")
Native_Forest <- cover_2018
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)
Forestry <- cover_2018
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)
Pasture <- cover_2018
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)
Agriculture <- cover_2018
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)
Development <- cover_2018
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)
Water <- cover_2018
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)
covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2018) #free environment space



#RSF fit for 2018 ----
#Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, and Thomas
RSF_NEW_18 <- list()
for(i in 1:length(DATA_NEW_18)){
  #extract individual
  DATA <- DATA_NEW_18[[i]]
  
  #extract AKDE
  AKDE <- AKDE_NEW_18[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_NEW_18[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop


names(RSF_NEW_18) <- names(DATA_NEW_18)


#add these individuals back into the years
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2017.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2018.rda")

Segre <- RSF_NEW_17[[1]]
Delphine <- RSF_NEW_18[[1]]
Gala <- RSF_NEW_18[[2]]

RSF_17 <- append(RSF_17, list(Segre = Segre))

RSF_18 <- append(RSF_18, list(Delphine = Delphine,
                              Gala = Gala))

#save amended files
save(RSF_17, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2017.rda")
save(RSF_18, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2018.rda")








#fit RSFs to a cover/no cover covariates -----------------------------------------------------------------------------------------------------------------------------------------------
#clean environment and reload data
library(ctmm) #analysis
library(terra) #working with spatrasters before turning into rasterclasses
library(raster) #turn spatrasters into raster classes before running analysis


#load data
load("~/Giant_Anteater_Orphan/FIXED/Data/Data_telemetry.rda") #telemetry object of orphan movement data (including those who disperse)
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Orphaned/wAKDEs.rda") #AKDEs of orphaned anteaters (including those who disperse)
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda") #telemetry objects of wild-raised, non-orphaned movement data
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Wild_raised/Lists/AKDE_wild_total.rda") # AKDEs of wild-raised, non-orphaned
DATA_wild <- DATA_TELEMETRY
DATA_orphan <- DATA_list
AKDE_orphan <- AKDEs
#preparing data to be separated by years
DATA_17 <- DATA_wild[c(3,5,6,7,8,10,12,14,16,19,26)] #Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, Puji, and Serge
DATA_18 <- DATA_wild[c(1,2,4,9,11,13,15,17,18,20,21,22,23,24,25)] #Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, Thomas, Delphine, and Gala
DATA_19 <- DATA_orphan[c(1,2,3)] #Arya, Capitu, and Dumbo
DATA_20 <- DATA_orphan[c(10)] #Tim_1
DATA_21 <- DATA_orphan[c(6,7,8,11)] #Renee_1, Renee_2, Renee_3, and Tim_3
DATA_22 <- DATA_orphan[c(4,5,9)] #Heather, Juju, and Renee_4
AKDE_17 <- AKDE_wild[c(3,5,6,7,8,10,12,14,16,19,26)] #Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, Puji, and Serge
AKDE_18 <- AKDE_wild[c(1,2,4,9,11,13,15,17,18,20,21,22,23,24,25)] #Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, Thomas, Delphine, and Gala
AKDE_19 <- AKDE_orphan[c(1,2,3)] #Arya, Capitu, and Dumbo
AKDE_20 <- AKDE_orphan[c(10)] #Tim_1
AKDE_21 <- AKDE_orphan[c(6,7,8,11)] #Renee_1, Renee_2, Renee_3, and Tim_3
AKDE_22 <- AKDE_orphan[c(4,5,9)] #Heather, Juju, and Renee_4
rm(DATA_wild, DATA_orphan, AKDE_wild, AKDE_orphan) #clear up environment space
#make lists for new individuals to run


#converting Spatrasters to rasterclasses 2017 ----
cover_2017 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2017_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2017
cover[cover %notin% c(1,3,4,5,6,49,29,9)] <- FALSE #Native forest and forestry
cover[cover %in% c(1,3,4,5,6,49,29,9)] <- TRUE #Native forest and forestry
cover <- raster(cover)
non_cover <- cover_2017
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE #Pasture and Agriculture
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE #Pasture and Agriculture
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)


#RSF fit for 2017 ---- 
# Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, and Puji
RSF_17 <- list()
for(i in 1:length(DATA_17)){
  
  #extract individual
  DATA <- DATA_17[[i]]
  
  #extract AKDE
  AKDE <- AKDE_17[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSFs
  RSF_17[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop


names(RSF_17) <- names(DATA_17)
save(RSF_17, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Cover_no_cover/RSF_2017.rda")
rm(RSF_17, DATA_17, AKDE_17) #free environment space 

#converting Spatrasters to rasterclasses 2018 ----
cover_2018 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2018_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2018
cover[cover %notin% c(1,3,4,5,6,49,29,9)] <- FALSE #Native forest and forestry
cover[cover %in% c(1,3,4,5,6,49,29,9)] <- TRUE #Native forest and forestry
cover <- raster(cover)
non_cover <- cover_2018
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE #Pasture and Agriculture
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE #Pasture and Agriculture
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)


#RSF fit for 2018 ----
#Alexander, Annie, Beto, Hannah, Jane, Larry, Luigi, Margaret, Maria, Redi, Rodolfo, Sherom, and Thomas
RSF_18 <- list()
for(i in 1:length(DATA_18)){
  #extract individual
  DATA <- DATA_18[[i]]
  
  #extract AKDE
  AKDE <- AKDE_18[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_18[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop


names(RSF_18) <- names(DATA_18)
save(RSF_18, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Cover_no_cover/RSF_2018.rda")
rm(RSF_18, DATA_18, AKDE_18) #save environment space 



#converting Spatrasters to rasterclasses 2019 ----
cover_2019 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2019_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2019
cover[cover %notin% c(1,3,4,5,6,49,29,9)] <- FALSE #Native forest and forestry
cover[cover %in% c(1,3,4,5,6,49,29,9)] <- TRUE #Native forest and forestry
cover <- raster(cover)
non_cover <- cover_2019
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE #Pasture and Agriculture
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE #Pasture and Agriculture
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)



#RSF fit for 2019 ----
#Arya, Capitu, and Dumbo
RSF_19 <- list()
for(i in 1:length(DATA_19)){
  #extract individual
  DATA <- DATA_19[[i]]
  
  #extract AKDE
  AKDE <- AKDE_19[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_19[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop

names(RSF_19) <- names(DATA_19)
save(RSF_19, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Cover_no_cover/RSF_2019.rda")
rm(RSF_19, DATA_19, AKDE_19) #save environment space 


#converting Spatrasters to rasterclasses 2020 ----
cover_2020 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2020_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2020
cover[cover %notin% c(1,3,4,5,6,49,29,9)] <- FALSE #Native forest and forestry
cover[cover %in% c(1,3,4,5,6,49,29,9)] <- TRUE #Native forest and forestry
cover <- raster(cover)
non_cover <- cover_2020
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE #Pasture and Agriculture
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE #Pasture and Agriculture
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)


#RSF fit for 2020 ----
#Tim_1
RSF_20 <- list()
for(i in 1:length(DATA_20)){
  
  #extract individual
  DATA <- DATA_20[[i]]
  
  #extract AKDE
  AKDE <- AKDE_20[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_20[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
} #close the loop

names(RSF_20) <- names(DATA_20)
save(RSF_20, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Cover_no_cover/RSF_2020.rda")
rm(RSF_20, AKDE_20, DATA_20) #save environment space 





#converting Spatrasters to rasterclasses 2021 ----
cover_2021 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2021_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2021
cover[cover %notin% c(1,3,4,5,6,49,29,9)] <- FALSE #Native forest and forestry
cover[cover %in% c(1,3,4,5,6,49,29,9)] <- TRUE #Native forest and forestry
cover <- raster(cover)
non_cover <- cover_2021
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE #Pasture and Agriculture
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE #Pasture and Agriculture
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)


#RSF fit for 2021 ----
#Renee_1, Renee_2, Renee_3, and Tim_3
RSF_21 <- list()
for(i in 1:length(DATA_21)){
  
  #extract individual
  DATA <- DATA_21[[i]]
  
  #extract AKDE
  AKDE <- AKDE_21[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_21[[i]] <- rsf.select(DATA, UD = AKDE, R = covers) 
  
  
} #close the loop
names(RSF_21) <- names(DATA_21)
save(RSF_21, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Cover_no_cover/RSF_2021.rda")
rm(RSF_21, DATA_21, AKDE_21) #save environment space 





#converting Spatrasters to rasterclasses 2017 ----
cover_2022 <- rast("~/Giant_Anteater_Orphan/Data/Mapbiomas/2022_cover.tif")
`%notin%` <- Negate(`%in%`)
cover <- cover_2022
cover[cover %notin% c(1,3,4,5,6,49,29,9)] <- FALSE #Native forest and forestry
cover[cover %in% c(1,3,4,5,6,49,29,9)] <- TRUE #Native forest and forestry
cover <- raster(cover)
non_cover <- cover_2022
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE #Pasture and Agriculture
non_cover[non_cover %notin% c(12,15,18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE #Pasture and Agriculture
non_cover <- raster(non_cover)
covers <- list(cover = cover,
               non_cover = non_cover)

#RSF fit for 2022 ----
#Heather, Juju, and Renee_4
RSF_22 <- list()
for(i in 1:length(DATA_22)){
  
  #extract individual
  DATA <- DATA_22[[i]]
  
  #extract AKDE
  AKDE <- AKDE_22[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_22[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
  
} #close the loop
names(RSF_22) <- names(DATA_22)
save(RSF_22, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Cover_no_cover/RSF_2022.rda")




































