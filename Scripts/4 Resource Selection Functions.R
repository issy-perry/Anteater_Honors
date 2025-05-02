# 4 Resource Selection Functions
# This script includes the following:
#     1. Extract land types from rasters to create covariates for RSFs
#     2. Fit RSFs using tel data and AKDEs against each covariate


#load packages 
library(ctmm) #analysis
library(terra) #working with spatrasters before turning into rasterclasses
library(raster) #turn spatrasters into raster classes before running analysis


#load tel data
load("~/Giant_Anteater_Orphan/FINAL/DATA/Orphaned/Data_telemetry.rda") #orphaned 
load("~/Giant_Anteater_Orphan/FINAL/DATA/Wild_raised/Data_telemetry.rda") #wild-raised

#rename for convenience
DATA_wild <- DATA_TELEMETRY
rm(DATA_TELEMETRY)

#load UDs
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/AKDEs/UDs_orphan.rda") #orphaned
load("~/Giant_Anteater_Orphan/FINAL/RESULTS/AKDEs/UDs_wild.rda") #wild-raised



#need to separate the data depending on when the individual was monitored so that the results for land selection are as accurate as possible
#wild-raised
DATA_17 <- DATA_wild[c("Anthony", "Bumpus", "Cate", "Christoffer", "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao", 
                       "Puji", "Segre")]
DATA_18 <- DATA_wild[c("Alexander", "Annie", "Beto", "Hannah", "Jane", "Larry", "Luigi", "Margaret", "Maria", "Reid", 
                       "Rodolfo", "Sheron", "Thomas", "Delphine", "Gala")]
#orphaned
DATA_19 <- DATA_orphan[c("Arya", "Capitu", "Dumbo_1", "Dumbo_2")]
DATA_20 <- DATA_orphan[c("Tim_1")] 
DATA_21 <- DATA_orphan[c("Renee_1", "Renee_2", "Renee_3", "Renee_4", "Tim_2")]
DATA_22 <- DATA_orphan[c("Cláudio", "Colete", "Heather", "Juju_1", "Juju_2", "Mulan", "Peter", "Rita", "Tim_3")] 
DATA_23 <- DATA_orphan[c("Bahia", "Beezie", "Bella", "Dom", "Erick", "George", "Jacobina", "Nancy", "Nayeli")]




#subset AKDEs
#wild-raised
AKDE_17 <- AKDE_wild[c("Anthony", "Bumpus", "Cate", "Christoffer", "Elaine", "Jackson", "Kyle", "Little_Rick", "Makao", 
                       "Puji", "Segre")]
AKDE_18 <- AKDE_wild[c("Alexander", "Annie", "Beto", "Hannah", "Jane", "Larry", "Luigi", "Margaret", "Maria", "Reid", 
                       "Rodolfo", "Sheron", "Thomas", "Delphine", "Gala")]
#orphaned
AKDE_19 <- AKDE_orphan[c("Arya", "Capitu", "Dumbo_1", "Dumbo_2")]
AKDE_20 <- AKDE_orphan[c("Tim_1")] 
AKDE_21 <- AKDE_orphan[c("Renee_1", "Renee_2", "Renee_3", "Renee_4", "Tim_2")]
AKDE_22 <- AKDE_orphan[c("Cláudio", "Colete", "Heather", "Juju_1", "Juju_2", "Mulan", "Peter", "Rita", "Tim_3")] 
AKDE_23 <-AKDE_orphan[c("Bahia", "Beezie", "Bella", "Dom", "Erick", "George", "Jacobina", "Nancy", "Nayeli")]


rm(DATA_wild, DATA_orphan, AKDE_orphan, AKDE_wild) #clear up environment space
#make lists for new individuals to run


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

#free up environment space
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2017)



#RSF fit for 2017 ---- 
# Anthony, Bumpus, Cate, Christoffer, Elaine, Jackson, Kyle, Little Rick, Makao, Puji, and Segre
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

#transfer names to new RSF list
names(RSF_17) <- names(DATA_17)

#save output
save(RSF_17, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2017.rda")

#free environment space
rm(RSF_17, DATA_17, AKDE_17) 




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
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#free environment space
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

#transfer names to new list of RSFs
names(RSF_18) <- names(DATA_18)

#save output
save(RSF_18, file = "~/Giant_Anteater_Orphan/FIXED/Results/RSFs/RSF_2018.rda")

#free up environment space
rm(RSF_18, DATA_18, AKDE_18) 



#converting Spatrasters to rasterclasses 2019 ----
cover_2019 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2019_cover.tif")
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

#free environment space
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2019) 


#RSF fit for 2019 ----
#Arya, Capitu, Dumbo_1, and Dumbo_2
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

#transfer names to new list of RSFs
names(RSF_19) <- names(DATA_19)

#save output
save(RSF_19, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2019.rda")

#free environment space
rm(RSF_19, DATA_19, AKDE_19) 





#converting Spatrasters to rasterclasses 2020 ----
cover_2020 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2020_cover.tif")
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

#free environment space
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2020) 

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

#transfer names to new list of RSFs
names(RSF_20) <- names(DATA_20)

#save output
save(RSF_20, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2020.rda")

#free environment space
rm(RSF_20, AKDE_20, DATA_20)





#converting Spatrasters to rasterclasses 2021 ----
cover_2021 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2021_cover.tif")
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

#free environment space
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2021) 


#RSF fit for 2021 ----
#Renee_1, Renee_2, Renee_3, Renee_4, Tim_2
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

#transfer names to new list of RSFs
names(RSF_21) <- names(DATA_21)

#save output
save(RSF_21, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2021.rda")

#free environment space
rm(RSF_21, DATA_21, AKDE_21) 





#converting Spatrasters to rasterclasses 2017 ----
cover_2022 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2022_cover.tif")
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

#free environment space
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2022) #free environment space

#RSF fit for 2022 ----
#Cláudio, Colete, Heather, Juju_1, Juju_2, Mulan, Peter, Rita, Tim_3
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

#transfer names to new list of RSFs
names(RSF_22) <- names(DATA_22)

#save output
save(RSF_22, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2022.rda")

#free environment space
rm(RSF_22, DATA_22, AKDE_22) 


#converting Spatrasters to rasterclasses 2017 ----
cover_2023 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2023_cover.tif")
Native_Forest <- cover_2023
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- FALSE
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- TRUE
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2023
Forestry[Forestry %notin% c(9)] <- FALSE
Forestry[Forestry %in% c(9)] <- TRUE
Forestry <- raster(Forestry)

Pasture <- cover_2023
Pasture[Pasture %notin% c(12,15)] <- FALSE
Pasture[Pasture %in% c(12,15)] <- TRUE
Pasture <- raster(Pasture)

Agriculture <- cover_2023
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- FALSE
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- TRUE
Agriculture <- raster(Agriculture)

Development <- cover_2023
Development[Development %notin% c(24,25,30)] <- FALSE
Development[Development %in% c(24,25,30)] <- TRUE
Development <- raster(Development)

Water <- cover_2023
Water[Water %notin% c(11,26,33)] <- FALSE
Water[Water %in% c(11,26,33)] <- TRUE
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#free environment space
rm(Native_Forest, Forestry, Pasture, Agriculture, Development, Water, cover_2023)


#RSF fit for 2023 ----
#Bahia, Beezie, Bella, Dom, Erick, George, Jacobina, Nancy, Nayeli
RSF_23 <- list()
for(i in 1:length(DATA_23)){
  
  #extract individual
  DATA <- DATA_23[[i]]
  
  #extract AKDE
  AKDE <- AKDE_23[[i]]
  
  #ensure projections are the same
  ctmm::projection(DATA) <- ctmm::projection(AKDE)
  
  #fit RSF
  RSF_22[[i]] <- rsf.select(DATA, UD = AKDE, R = covers)
  
  
} #close the loop

#transfer names to new list of RSFs
names(RSF_23) <- names(DATA_23)

#save output
save(RSF_23, file = "~/Giant_Anteater_Orphan/FINAL/RESULTS/RSFs/RSF_2023.rda")

























