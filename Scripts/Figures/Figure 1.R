#load packages
library(ggplot2)
library(ctmm)
library(terra)
library(sf)
library(tidyterra)
library(gridExtra)
library(rphylopic)
library(ggspatial)
library(tidyr) #pivot_longer()

#load raster data
cover_2017 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2017_cover.tif")
cover_2018 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2018_cover.tif")
cover_2019 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2019_cover.tif")
cover_2020 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2020_cover.tif")
cover_2021 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2021_cover.tif")
cover_2022 <- rast("~/Giant_Anteater_Orphan/FIXED/Data/Mapbiomas/2022_cover.tif")
cover_2023 <- rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_9/lclu/coverage/brasil_coverage_2022.tif")
eo <- ext(-49,-46.5,-19.3,-18) #create extent barriers so that we can crop a certain amount out for orphaned anteaters
cover_2023 <- crop(cover_2023, eo)


#load tel data
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda") # adult AKDE 
load("~/Giant_Anteater_Orphan/FIXED/Data/Orphan/TOTAL_Data.rda") # adult AKDE  


#telemetry overlayed on raster ------------------------------------------------------------------------------------------------------------

land_types <- cover_2018
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda") # adult AKDE 
DATA_wild <- DATA_TELEMETRY
rm(DATA_TELEMETRY)
DATA_w_RR <- DATA_wild[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
DATA_w_RR <- do.call(rbind.data.frame, DATA_w_RR)
DATA_w_RR_sf <- st_as_sf(DATA_w_RR, coords = c("longitude", "latitude"), 
                         crs = crs(land_types))
DATA_w_dis <- DATA_wild[c(24,25,26)]
DATA_w_dis <- do.call(rbind.data.frame, DATA_w_dis)
DATA_w_dis_sf <- st_as_sf(DATA_w_dis, coords = c("longitude", "latitude"), 
                          crs = crs(land_types))



DATA_wild <- do.call(rbind.data.frame, DATA_wild)
DATA_wild_sf <- st_as_sf(DATA_wild, coords = c("longitude", "latitude"), 
                         crs = crs(land_types))
DATA_wild_ext <- ext(DATA_wild_sf)
#SpatExtent : -54.12446, -53.40578, -21.842928, -20.46201 (xmin, xmax, ymin, ymax) 
total_ext <- ext(-54.2, -53.39, -21.85, -20.46) #create extent barriers so that we can crop a certain amount out for orphaned anteaters
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)





wild_plot <- ggplot() +
  geom_spatraster(data = land_cropped, maxcell = 5e+07,
                  alpha = 0.7, aes(fill = brasil_coverage_2018)) +
  geom_sf(data = DATA_wild_sf, size = 0.9, alpha = 0.6, col = "black", shape = 16) +
  scale_fill_manual(breaks = c(94,95,96,97,98,99),
                    labels = c("Forestry", "Water", "Development", "Agriculture", "Pasture", "Native Forest"),
                    values = c("#B19B95",  "#9CCEDE", "#E6E6E6", "#9FCC8E", "#F3E6A3", "#44735F"),
                    name = "Land Class") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        #legend.position.inside = c(0.28,0.12),
        legend.title = element_text(size=18, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=18, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = .5, size = 18, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
  labs(title = "Wild-raised Population") 
#annotation_scale(height = unit(0.010, "npc"),
# width_hint = 0.4,
# line_width = 0.2,
# pad_x = unit(0.07, "npc"),
# pad_y = unit(0.07, "npc"),
# text_pad = unit(0.01, "npc"),
# text_cex = .2,
# text_family = "sans",
# text_face = "bold")
plot(wild_plot)
#plot(AKDE_wild, add = TRUE)

wild_plot + 
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("darkgray", "white"),
    text_family = "sans") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("darkgray", "lightgray"),
      line_col = "black",
      text_family = "sans",
      text_face = "bold")
  )






load("~/Giant_Anteater_Orphan/FIXED/Data/Orphan/TOTAL_Data.rda") # adult AKDE 
land_types <- cover_2021
DATA_orphan <- do.call(rbind.data.frame, DATA_list)
DATA_orphan_sf <- st_as_sf(DATA_orphan, coords = c("longitude", "latitude"), 
                           crs = crs(land_types))
DATA_orphan_ext <- ext(DATA_orphan_sf)

DATA_o_RR <- DATA_list[c(1,3,4,5,11,14,15,17,18,19,20)]
DATA_o_RR <- do.call(rbind.data.frame, DATA_o_RR)
DATA_o_RR_sf <- st_as_sf(DATA_o_RR, coords = c("longitude", "latitude"), 
                         crs = crs(land_types))
DATA_o_dis <- DATA_list[c(2,6,7,8,9,10,12,13,16)]
DATA_o_dis <- do.call(rbind.data.frame, DATA_o_dis)
DATA_o_dis_sf <- st_as_sf(DATA_o_dis, coords = c("longitude", "latitude"), 
                          crs = crs(land_types))
#SpatExtent : -48.796641, -47.503857, -18.993105, -18.36321 (xmin, xmax, ymin, ymax)
#separate into multiple study sites so that we can actually see the telemetry data
total_ext <- ext(-48.8, -47.49, -19.19, -18.35)

land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

orphan_plot <- ggplot() +
  geom_spatraster(data = land_cropped, maxcell = 5e+07,
                  alpha = 0.7, aes(fill = brasil_coverage_2021)) +
  geom_sf(data = DATA_orphan_sf, size = 1.45, alpha = 0.6, col = "black", shape = 16) +
  
  scale_fill_manual(breaks = c(94,95,96,97,98,99),
                    labels = c("Forestry", "Water", "Development", "Agriculture", "Pasture", "Native Forest"),
                    values = c("#9C8D6D",  "#9CCEDE", "#E6E6E6", "#9FCC8E", "#F3E6A3", "#44735F"),
                    name = "Land Class") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        #legend.position.inside = c(0.28,0.12),
        legend.position = "right",
        legend.title = element_text(size=18, family = "sans", face = "bold", hjust = 0.5),
        legend.text = element_text(size=16, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(hjust = .5, size = 18, family = "sans", face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks = element_blank(),
        strip.background=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
  labs(title = "Orphaned Population")  

plot(orphan_plot)

orphan_plot + 
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("darkgray", "white"),
    text_family = "sans") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("darkgray", "lightgray"),
      line_col = "black",
      text_family = "sans",
      text_face = "bold")
  )














#bar plots of average land type ------------------------------------------------------------------------------------------

#load data
#wAKDEs
#load("~/Giant_Anteater_Orphan/FIXED/Results/wAKDEs.rda") #AKDEs of orphaned anteaters (including those who disperse)
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Wild_raised/Lists/AKDE_wild_total.rda") # AKDEs of wild-raised, non-orphaned



#proportion of land type in each area -------------------------------wild--------------------
load("~/Giant_Anteater_Orphan/FIXED/Data/Wild_raised/Data_telemetry.rda") # adult AKDE 
land_types <- cover_2017
DATA_wild <- DATA_TELEMETRY
DATA_wild <- do.call(rbind.data.frame, DATA_wild)
DATA_wild_sf <- st_as_sf(DATA_wild, coords = c("longitude", "latitude"), 
                         crs = crs(land_types))
DATA_wild_ext <- ext(DATA_wild_sf)
#SpatExtent : -54.12446, -53.40578, -21.842928, -20.46201 (xmin, xmax, ymin, ymax) 
total_ext <- ext(-54.2, -53.39, -21.85, -20.46) #create extent barriers so that we can crop a certain amount out for orphaned anteaters
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

prop_17 <- data.frame()
prop_17[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_17[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_17[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_17[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_17[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_17[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster





#2018
land_types <- cover_2018
#total_ext <- ext(-53.8, -53.47, -21.79, -21.08) #create extent barriers so that we can crop a certain amount out for orphaned anteaters
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

prop_18 <- data.frame()
prop_18[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_18[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_18[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_18[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_18[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_18[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster



prop_17$Year <- c("2017")
prop_18$Year <- c("2018")

prop_w <- rbind(prop_17, prop_18)
prop_w$Site <- c("Wild-raised")


#proportion of land type in each area -------------------------------orphan--------------------
load("~/Giant_Anteater_Orphan/FIXED/Data/Orphan/TOTAL_Data.rda") # adult AKDE 

land_types <- cover_2019

DATA_list <- do.call(rbind.data.frame, DATA_list)
DATA_list_sf <- st_as_sf(DATA_list, coords = c("longitude", "latitude"), 
                         crs = crs(land_types))
DATA_list_ext <- ext(DATA_list_sf)

#SpatExtent : -48.796641, -47.503857, -18.993105, -18.36321 (xmin, xmax, ymin, ymax)
#separate into multiple study sites so that we can actually see the telemetry data
total_ext <- ext(-48.8, -47.49, -19.19, -18.35)

#2019 ----

land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)
prop_19 <- data.frame()
prop_19[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_19[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_19[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_19[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_19[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_19[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster


#2020 ----
land_types <- cover_2020
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

prop_20 <- data.frame()
prop_20[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_20[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_20[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_20[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_20[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_20[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster



#2021 ----
land_types <- cover_2021
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

prop_21 <- data.frame()
prop_21[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_21[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_21[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_21[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_21[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_21[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster




#2022 ----
land_types <- cover_2022
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

prop_22 <- data.frame()
prop_22[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_22[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_22[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_22[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_22[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_22[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster


#2023 ----
land_types <- cover_2023
land_cropped <- crop(land_types,
                     total_ext,
                     snap = "out")

#Process the land class raster for easy plotting
land_cropped <- ifel(land_cropped %in% c(1,3,4,5,6,49,29), 99, land_cropped) #99 = Native_forest
land_cropped <- ifel(land_cropped %in% c(12,15), 98, land_cropped) #98 = Pasture
land_cropped <- ifel(land_cropped %in% c(18,19,20,21,39,40,41,62,36,46,47,35,48), 97, land_cropped) # 97 = Agriculture
land_cropped <- ifel(land_cropped %in% c(24,25,50), 96, land_cropped) # 96 = Development
land_cropped <- ifel(land_cropped %in% c(11,26,33), 95, land_cropped) # 95 = water
land_cropped <- ifel(land_cropped %in% c(9), 94, land_cropped) # 94 = Forestry
land_cropped <- ifel(land_cropped %in% 0:93, 93, land_cropped) # 93 = other
land_cropped <- as.factor(land_cropped)

prop_23 <- data.frame()
prop_23[1, "Agriculture"] <- prop.table(table(as.vector(land_cropped))) ["97"] #proportion of agriculture land type in raster
prop_23[1, "Development"] <- prop.table(table(as.vector(land_cropped))) ["96"] #proportion of development land type in raster
prop_23[1, "Forestry"] <- prop.table(table(as.vector(land_cropped))) ["94"] #proportion of forestry land type in raster
prop_23[1, "Native_Forest"] <- prop.table(table(as.vector(land_cropped))) ["99"] #proportion of native forest land type in raster
prop_23[1, "Pasture"] <- prop.table(table(as.vector(land_cropped))) ["98"] #proportion of pasture land type in raster
prop_23[1, "Water"] <- prop.table(table(as.vector(land_cropped))) ["95"] #proportion of agriculture land type in raster




prop_19$Year <- c("2019")
prop_20$Year <- c("2020")
prop_21$Year <- c("2021")
prop_22$Year <- c("2022")
prop_23$Year <- c("2023")

prop_o <- rbind(prop_19, prop_20, prop_21, prop_22, prop_23)
prop_o$Site <- c("Orphan")



#means of proportions across years wild-raised site
mean_w <- data.frame()
mean_w[1, "Agriculture"] <- mean(prop_w[["Agriculture"]])
mean_w[1, "Development"] <- mean(prop_w[["Development"]])
mean_w[1, "Forestry"] <- mean(prop_w[["Forestry"]])
mean_w[1, "Native_Forest"] <- mean(prop_w[["Native_Forest"]])
mean_w[1, "Pasture"] <- mean(prop_w[["Pasture"]])
mean_w[1, "Water"] <- mean(prop_w[["Water"]])

mean_w$Year <- c("Average_W")
mean_w$Site <- c("Wild-raised")

#means of proportions across years orphan site
mean_o <- data.frame()
mean_o[1, "Agriculture"] <- mean(prop_o[["Agriculture"]])
mean_o[1, "Development"] <- mean(prop_o[["Development"]])
mean_o[1, "Forestry"] <- mean(prop_o[["Forestry"]])
mean_o[1, "Native_Forest"] <- mean(prop_o[["Native_Forest"]])
mean_o[1, "Pasture"] <- mean(prop_o[["Pasture"]])
mean_o[1, "Water"] <- mean(prop_o[["Water"]])

mean_o$Year <- c("Average_O")
mean_o$Site <- c("Orphan")

#combine dataframes
prop_w <- rbind(prop_w, mean_w)
prop_o <- rbind(prop_o, mean_o)
prop_land <-rbind(prop_w, prop_o)
#remove extra dataframes to free environment space
rm(prop_17, prop_18, mean_w, prop_19, prop_20, prop_21, prop_22,prop_23, mean_o)
save(prop_land, file = "~/Giant_Anteater_Orphan/FIXED/Data/Land_Use/Total_proportion.rda")

#pivot to make plotting easier
prop_land_piv <- prop_land %>% pivot_longer(cols=c("Water", "Agriculture", "Development", "Pasture", "Forestry", "Native_Forest"),
                                            names_to = "Land_Type",
                                            values_to = "Proportion")
save(prop_land_piv, file = "~/Giant_Anteater_Orphan/FIXED/Data/Land_Use/Total_proportion_pivoted.rda")
load("~/Giant_Anteater_Orphan/FIXED/Data/Land_Use/Total_proportion_pivoted.rda")

#remove years for now just to get average
prop_land_piv <- subset(prop_land_piv, Year != 2017)
prop_land_piv <- subset(prop_land_piv, Year != 2018)
prop_land_piv <- subset(prop_land_piv, Year != 2019)
prop_land_piv <- subset(prop_land_piv, Year != 2020)
prop_land_piv <- subset(prop_land_piv, Year != 2021)
prop_land_piv <- subset(prop_land_piv, Year != 2022)
prop_land_piv <- subset(prop_land_piv, Year != 2023)



#plot proportions ----------
bars<- ggplot(data = subset(prop_land_piv), aes(x = Land_Type, y= Proportion, fill = Land_Type)) +
  geom_bar (stat = "identity", width = 1) +
  scale_fill_manual("Land Class", values = c("Agriculture" = "#9FCC8E", "Development" = "#E6E6E6", "Forestry" = "#9C8D6D", 
                                             "Native_Forest" = "#44735F", "Pasture"= "#F3E6A3", "Water" =  "#9CCEDE")) + #changes colors of individuals
  #geom_text(aes(label = Proportion), position = position_stack(vjust = 0.5)) +
  #geom_text(aes(y = ypos, label = Proportion), color = "black", size = 5) +
  #geom_text(aes(label = paste0(round(Proportion, 2))), position = position_stack(vjust = 0.5), color = "black", size = 6) +
  #theme(legend.position = "none") + #remove double legend before redoing it 
  #scale_color_manual(name = "Land Types", values = c("Agriculture" = "#999933", "Development" = "#DDDDDD", "Forestry" = "#44AA99", "Native_Forest" = "#117733", "Pasture"= "#DDCC77", "Water" =  "#88CCEE"))+
  #ggtitle("Orphanned Site") +
  labs(x = element_blank(), y = "Proportion of Cover in Study Site") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA), legend.position = "bottom", strip.text = element_text(face = "bold", size = 12, family = "sans")) +
  facet_wrap(vars(Year), labeller = labeller(Year = c("Average_O" = "Orphaned Study Site", "Average_W" = "Wild-raised Study Site")))
plot(bars)


