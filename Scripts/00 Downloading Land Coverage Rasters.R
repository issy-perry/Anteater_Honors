# 00 Downloading Land Coverage Rasters
# This script includes the following:
#     1. Download rasters for entire country of Brazil
#     2. Extract extents of telemetry data for cropping the rasters
#     3. Crop rasters
#     4. Save rasters


#load packages
library(terra) #main package we need to use for dowloading, cropping, and saving our rasters
library(sf) #getting the extents of datasets

#load in land coverage data
land_cover_2023 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2023.tif")
land_cover_2022 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2022.tif")
land_cover_2021 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2021.tif")
land_cover_2020 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2020.tif")
land_cover_2019 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2019.tif")
land_cover_2018 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2018.tif")
land_cover_2017 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2017.tif")


#load tel data
load("~/Giant_Anteater_Orphan/FINAL/DATA/Orphaned/Data_telemetry.rda") #orphaned 
load("~/Giant_Anteater_Orphan/FINAL/DATA/Wild_raised/Data_telemetry.rda") #wild-raised


#rename for convenience
DATA_wild <- DATA_TELEMETRY
rm(DATA_TELEMETRY)



#get extents from tel data to use for cropping the rasters
#wild-raised
#convert back to a dataframe
DATA_wild <- do.call(rbind.data.frame, DATA_wild)
#convert to an sf object to get extents of tel data
DATA_wild_sf <- st_as_sf(DATA_wild, coords = c("longitude", "latitude"), 
                         crs = crs(land_cover_2017))
#get extent barrier of tel data
DATA_wild_ext <- ext(DATA_wild_sf)
#print to see
print(DATA_wild_ext)
#SpatExtent : -54.12446, -53.40578, -21.842928, -20.46201 (xmin, xmax, ymin, ymax)



#orphaned
#convert back to a dataframe
DATA_orphan <- do.call(rbind.data.frame, DATA_orphan)
#convert to an sf object to get extents of tel data
DATA_orphan_sf <- st_as_sf(DATA_orphan, coords = c("longitude", "latitude"), 
                         crs = crs(land_cover_2019))
#get extent barrier of tel data
DATA_orphan_ext <- ext(DATA_orphan_sf)
#print to see
print(DATA_orphan_ext)
#SpatExtent : -48.796641, -47.502037, -19.210794, -18.362662 (xmin, xmax, ymin, ymax)



#crop data based on extent of our two populations movements
#make sure to add a decent buffer around the extents (especially for orphans because of large HDOPs)
ew <- ext(-54.5, -53, -22.5, -20) #wild-raised population extent
eo <- ext(-49.5, -46, -20, -17.5) #orphaned population extent

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
terra::writeRaster(cover_2023, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2023_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2022, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2022_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2021, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2021_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2020, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2020_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2019, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2019_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2018, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2018_cover.tif", filetype = "GTiff")
terra::writeRaster(cover_2017, "~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2017_cover.tif", filetype = "GTiff")


#test to see that it worked
cover_2023 <- terra::rast("~/Giant_Anteater_Orphan/FINAL/DATA/Mapbiomas/2023_cover.tif")
plot(cover_2023)
