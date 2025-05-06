# 000 Converting Categorical to Continuous Rasters
# This script includes the following:
#     1. Reprojecting rasters
#     2. Changing values to distance from


#SAVING METHOD DOES NOT WORK SO I NEED TO IRON THAT OUT

#load packages
library(terra)
library(raster)

#2023
cover_2023 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2023_cover.tif")

#Change out lat/long
cover_2023 <- as.factor(cover_2023)

#Reproject
cover_2023 <- project(cover_2023, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2023
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2023
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2023
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2023
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2023
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2023
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#save output
#terra::writeRaster(covers, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Continuous/2023_cover.tif", filetype = "GTiff")

#save(covers, file = "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Continuous/2023_cover.rda")
raster::writeRaster(covers, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Continuous/2023_cover.tif", filetype = "GTiff")
#check if it worked
rm(covers)

covers <- load("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Continuous/2023_cover.rda")
#clear environment
#rm(covers, cover_2023, Water, Development, Agriculture, Pasture, Forestry, Native_forest)








#clear environ

cover_2022 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2022_cover.tif")
#Change out lat/long
cover_2022 <- as.factor(cover_2022)

#Reproject
cover_2022 <- project(cover_2022, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2022
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2022
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2022
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2022
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2022
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2022
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#clear environment
rm(covers, cover_2022, Water, Development, Agriculture, Pasture, Forestry, Native_forest)

#2021 ----
cover_2021 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2021_cover.tif")
#Change out lat/long
cover_2021 <- as.factor(cover_2021)

#Reproject
cover_2021 <- project(cover_2021, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2021
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2021
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2021
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2021
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2021
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2021
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#clear environment
rm(covers, cover_2021, Water, Development, Agriculture, Pasture, Forestry, Native_forest)

cover_2020 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2020_cover.tif")
#Change out lat/long
cover_2020 <- as.factor(cover_2020)

#Reproject
cover_2020 <- project(cover_2020, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2020
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2020
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2020
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2020
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2020
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2020
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#clear environment
rm(covers, cover_2020, Water, Development, Agriculture, Pasture, Forestry, Native_forest)

cover_2019 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2019_cover.tif")
#Change out lat/long
cover_2019 <- as.factor(cover_2019)

#Reproject
cover_2019 <- project(cover_2019, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2019
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2019
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2019
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2019
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2019
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2019
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#clear environment
rm(covers, cover_2019, Water, Development, Agriculture, Pasture, Forestry, Native_forest)

cover_2018 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2018_cover.tif")
#Change out lat/long
cover_2018 <- as.factor(cover_2018)

#Reproject
cover_2018 <- project(cover_2018, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2018
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2018
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2018
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2018
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2018
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2018
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#clear environment
rm(covers, cover_2018, Water, Development, Agriculture, Pasture, Forestry, Native_forest)

cover_2017 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/Categorical/2017_cover.tif")
#Change out lat/long
cover_2017 <- as.factor(cover_2017)

#Reproject
cover_2017 <- project(cover_2017, "EPSG:5641")

#Create a raster for each covariate
`%notin%` <- Negate(`%in%`)
Native_Forest <- cover_2017
Native_Forest[Native_Forest %notin% c(1,3,4,5,6,49,29)] <- NA
Native_Forest[Native_Forest %in% c(1,3,4,5,6,49,29)] <- 1
crs(Native_Forest) <- "local"
Native_Forest <- terra::distance(Native_Forest)
Native_Forest <- raster(Native_Forest)

Forestry <- cover_2017
Forestry[Forestry %notin% c(9)] <- NA
Forestry[Forestry %in% c(9)] <- 1
crs(Forestry) <- "local"
Forestry <- terra::distance(Forestry)
Forestry <- raster(Forestry)

Pasture <- cover_2017
Pasture[Pasture %notin% c(12,15)] <- NA
Pasture[Pasture %in% c(12,15)] <- 1
crs(Pasture) <- "local"
Pasture <- terra::distance(Pasture)
Pasture <- raster(Pasture)

Agriculture <- cover_2017
Agriculture[Agriculture %notin% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- NA
Agriculture[Agriculture %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48)] <- 1
crs(Agriculture) <- "local"
Agriculture <- terra::distance(Agriculture)
Agriculture <- raster(Agriculture)

Development <- cover_2017
Development[Development %notin% c(24,25,30)] <- NA
Development[Development %in% c(24,25,30)] <- 1
crs(Development) <- "local"
Development <- terra::distance(Development)
Development <- raster(Development)

Water <- cover_2017
Water[Water %notin% c(11,26,33)] <- NA
Water[Water %in% c(11,26,33)] <- 1
crs(Water) <- "local"
Water <- terra::distance(Water)
Water <- raster(Water)

covers <- list(Native_Forest = Native_Forest,
               Forestry = Forestry,
               Pasture = Pasture,
               Agriculture = Agriculture,
               Development = Development,
               Water = Water)

#clear environment
rm(covers, cover_2017, Water, Development, Agriculture, Pasture, Forestry, Native_forest)
