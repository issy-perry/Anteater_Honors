#load packages
library(terra)



#import data from 2023 link on Mapbiomas (links from 2016-2022 are not working so just alter year on 2023 link)
land_cover_2023 <- rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2023.tif")
land_cover_2022 <- rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2022.tif")
land_cover_2021 <- rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2021.tif")
land_cover_2020 <- rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2020.tif")
land_cover_2019 <- rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2019.tif")


#crop data based on movement plots of anteaters (based on GPS data from anteaters)
e <- ext(-49,-46.5,-19.3,-18) #create extent barriers so that we can crop a certain amount out
#e1 <- ext(-49,-46.5,-19,-18.3)
#e2 <- ext(-49,-46.5,-19,-18.3)
#currently excludes Dumbo_3's area; however, unsure if that will be included so TBD
cover_2023 <- crop(land_cover_2023, e)
cover_2022 <- crop(land_cover_2022, e)
cover_2021 <- crop(land_cover_2021, e)
cover_2020 <- crop(land_cover_2020, e)
cover_2019 <- crop(land_cover_2019, e)
plot(cover_2023) #test if it worked
plot(cover_2019) #test if it worked



#save tifs
save(cover_2023, file = "~/Giant_Anteater_Orphan/Data/23cover_Land.rds") 
save(cover_2022, file = "~/Giant_Anteater_Orphan/Data/22cover_Land.rds") 
save(cover_2021, file = "~/Giant_Anteater_Orphan/Data/21cover_Land.rds") 
save(cover_2020, file = "~/Giant_Anteater_Orphan/Data/20cover_Land.rds")
save(cover_2019, file = "~/Giant_Anteater_Orphan/Data/19cover_Land.rds") 


#check if it saved properly
load("~/Giant_Anteater_Orphan/Data/23cover_Land.rds") #worked















