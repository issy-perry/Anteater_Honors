#load packages
library(terra) #main package we need to use for cropping and saving our rasters
library(raster) #just in case

#load in land coverage data
land_cover_2023 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2023.tif")
land_cover_2022 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2022.tif")
land_cover_2021 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2021.tif")
land_cover_2020 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2020.tif")
land_cover_2019 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2019.tif")
land_cover_2018 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2018.tif")
land_cover_2017 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2017.tif")


#crop data based on extent of our two populations movements, simply plotting the GPS fixes will suffice for now
#make sure to add a decent buffer around 
eo <- ext(-49,-46.5,-19.3,-18) #orphaned population extent
ew <- ext(-54.5, -51.5,-22.5,-19.5) #wild-raised population extent


#crop rasters using previously made extents
cover_2023 <- crop(land_cover_2023, eo)
cover_2022 <- crop(land_cover_2022, eo)
cover_2021 <- crop(land_cover_2021, eo)
cover_2020 <- crop(land_cover_2020, eo)
cover_2019 <- crop(land_cover_2019, eo)
cover_2018 <- crop(land_cover_2018, ew)
cover_2017 <- crop(land_cover_2017, ew)


#plot to test that it worked
plot(cover_2023) #worked
plot(cover_2017) #worked

rm(land_cover_2023, land_cover_2022, land_cover_2021, land_cover_2020, land_cover_2019, land_cover_2018, land_cover_2017, ew, eo)

#save files using writeRaster function
#some sites say we can use saveRDS(), but just keep it clean and simple 
#save() absolutely will not working for saving these files
terra::writeRaster(cover_2023, "2023_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2022, "2022_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2021, "2021_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2020, "2020_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2019, "2019_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2018, "2018_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2017, "2017_cover.tif", filetype = "GTiff")