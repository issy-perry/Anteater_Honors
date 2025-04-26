library(ctmm) #movement
library(dplyr)
library(ggplot2) #making plots
library(ggpubr) #arranging plots in multiples
library(tidyr) #drop_na function
#load data
#RSFs----------
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2017.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2017_sERGE.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/RSF_2018.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Wild_raised/DELPHINE.rda")
#Fits
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Wild_raised/Lists/FITS_wild_total.rda")
#AKDEs
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Wild_raised/Lists/AKDE_wild_total.rda")

#separate wild into just subadults for all
RSF_wild_all <- list()
RSF_wild_all <- append(RSF_wild_all, NEW_RSF_17)
RSF_wild_all <- append(RSF_wild_all, NEW_RSF_18)
RSF_wild_all <- append(RSF_wild_all, RSF_17)
RSF_wild_all <- append(RSF_wild_all, RSF_18)

#subadults are anthony, gala, kyle, little_rick, and reid
SUBADULT_RSF <- RSF_wild_all[c(3,28,9,10,23)]
SUBADULT_FIT <- FITS_wild [c(3,12,14,20,25)]
SUBADULT_AKDE <- AKDE_wild[c(3,12,14,20,25)]

ADULT_FIT <- FITS_wild[c(1,2,4,5,6,7,8,9,10,11,13,15,16,17,18,19,21,22,23,24,26)]
ADULT_AKDE <- AKDE_wild[c(1,2,4,5,6,7,8,9,10,11,13,15,16,17,18,19,21,22,23,24,26)]
ADULT_RSF <- RSF_wild_all[c(4,5,6,7,8,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,27)]
#rm(AKDE_wild, FITS_wild, RSF_wild_all) #free environment space
rm(RSF_17, RSF_18, NEW_RSF_17, NEW_RSF_18)
#load in orphan data
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2019.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2020.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2021.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2022.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2022_NEW.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/All_covariates/Orphaned/RSF_2023_NEW.rda")
#Fits
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Orphaned/TOTAL_FITS.rda")
ORPHAN_FIT <- FITS
#AKDEs
load("~/Giant_Anteater_Orphan/FIXED/Results/AKDEs/Orphaned/TOTAL_AKDE.rda")
ORPHAN_AKDE <- AKDEs
#combine into one list
RSF_orphan_all <- list()
RSF_orphan_all <- append(RSF_orphan_all, RSF_19) 
RSF_orphan_all <- append(RSF_orphan_all, RSF_20)
RSF_orphan_all <- append(RSF_orphan_all, RSF_21)
RSF_orphan_all <- append(RSF_orphan_all, RSF_22)
RSF_orphan_all <- append(RSF_orphan_all, NEW_RSF_22)
RSF_orphan_all <- append(RSF_orphan_all, NEW_RSF_23)
rm(NEW_RSF_22, NEW_RSF_23, RSF_19, RSF_20, RSF_21, RSF_22, FITS, AKDEs)


#load in total dataframe for plotting
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Total_Movement_df.rda")
#remove values for individuals we will not be plotting

Movement_df[c(21,23,24,25,27,28,29,30,31,32,33,34,36,38,39,40,41,42,44,45,46), "Status"] <- "Adult"

Movement_df[c(43,37,35,26,22), "Status"] <- "Subadult"


#make a new meta dataframe of population means since the other one includes adults

#orphan
HR_O <- ctmm::meta(ORPHAN_AKDE, variable = "area", level = 0.95, units = FALSE)
HR_O <- as.data.frame(HR_O)
HR_O <- HR_O[-c(2,3),]
HR_O[,"low"] <- "km^2" %#% HR_O[,"low"] #converts to km/day rather than m/s
HR_O[,"est"] <- "km^2" %#% HR_O[, "est"] #converts to km/day rather than m/s
HR_O[,"high"] <- "km^2" %#% HR_O[,"high"] #converts to km/day rather than m/s
colnames(HR_O) <- c("HR_low", "HR_est", "HR_high")

#wild
HR_J <- ctmm::meta(SUBADULT_AKDE, variable = "area", level = 0.95, units = FALSE)
HR_J <- as.data.frame(HR_J)
HR_J <- HR_J[-c(2,3),]
HR_J[,"low"] <- "km^2" %#% HR_J[,"low"] #converts to km/day rather than m/s
HR_J[,"est"] <- "km^2" %#% HR_J[, "est"] #converts to km/day rather than m/s
HR_J[,"high"] <- "km^2" %#% HR_J[,"high"] #converts to km/day rather than m/s
colnames(HR_J) <- c("HR_low", "HR_est", "HR_high")

#wild adult
HR_A <- ctmm::meta(ADULT_AKDE, variable = "area", level = 0.95, units = FALSE)
HR_A <- as.data.frame(HR_A)
HR_A <- HR_A[-c(2,3),]
HR_A[,"low"] <- "km^2" %#% HR_A[,"low"] #converts to km/day rather than m/s
HR_A[,"est"] <- "km^2" %#% HR_A[, "est"] #converts to km/day rather than m/s
HR_A[,"high"] <- "km^2" %#% HR_A[,"high"] #converts to km/day rather than m/s
colnames(HR_A) <- c("HR_low", "HR_est", "HR_high")


#orphan
SPEED_O <- ctmm::meta(ORPHAN_FIT, variable = "speed", level = 0.95, units = FALSE)
SPEED_O <- as.data.frame(SPEED_O)
SPEED_O <- SPEED_O[-c(2,3),]
SPEED_O[,"low"] <- "km/day" %#% SPEED_O[,"low"] #converts to km/day rather than m/s
SPEED_O[,"est"] <- "km/day" %#% SPEED_O[, "est"] #converts to km/day rather than m/s
SPEED_O[,"high"] <- "km/day" %#% SPEED_O[,"high"] #converts to km/day rather than m/s
colnames(SPEED_O) <- c("speed_low", "speed_est", "speed_high")

#wild subadult
SPEED_J <- ctmm::meta(SUBADULT_FIT, variable = "speed", level = 0.95, units = FALSE)
SPEED_J <- as.data.frame(SPEED_J)
SPEED_J <- SPEED_J[-c(2,3),]
SPEED_J[,"low"] <- "km/day" %#% SPEED_J[,"low"] #converts to km/day rather than m/s
SPEED_J[,"est"] <- "km/day" %#% SPEED_J[, "est"] #converts to km/day rather than m/s
SPEED_J[,"high"] <- "km/day" %#% SPEED_J[,"high"] #converts to km/day rather than m/s
colnames(SPEED_J) <- c("speed_low", "speed_est", "speed_high")

#wild adult
SPEED_A <- ctmm::meta(ADULT_FIT, variable = "speed", level = 0.95, units = FALSE)
SPEED_A <- as.data.frame(SPEED_A)
SPEED_A <- SPEED_A[-c(2,3),]
SPEED_A[,"low"] <- "km/day" %#% SPEED_A[,"low"] #converts to km/day rather than m/s
SPEED_A[,"est"] <- "km/day" %#% SPEED_A[, "est"] #converts to km/day rather than m/s
SPEED_A[,"high"] <- "km/day" %#% SPEED_A[,"high"] #converts to km/day rather than m/s
colnames(SPEED_A) <- c("speed_low", "speed_est", "speed_high")


#orphan
VELOCITY_O <- ctmm::meta(ORPHAN_FIT, variable = "tauvelocity", level = 0.95, units = FALSE)
VELOCITY_O <- as.data.frame(VELOCITY_O)
VELOCITY_O <- VELOCITY_O[-c(2,3),]
VELOCITY_O[,"low"] <- "minutes" %#% VELOCITY_O[,"low"] #converts to km/day rather than m/s
VELOCITY_O[,"est"] <- "minutes" %#% VELOCITY_O[, "est"] #converts to km/day rather than m/s
VELOCITY_O[,"high"] <- "minutes" %#% VELOCITY_O[,"high"] #converts to km/day rather than m/s
colnames(VELOCITY_O) <- c("τvelocity_low", "τvelocity_est", "τvelocity_high")

#wild subadult
VELOCITY_W <- ctmm::meta(SUBADULT_FIT, variable = "tauvelocity", level = 0.95, units = FALSE)
VELOCITY_W <- as.data.frame(VELOCITY_W)
VELOCITY_W <- VELOCITY_W[-c(2,3),]
VELOCITY_W[,"low"] <- "minutes" %#% VELOCITY_W[,"low"] #converts to km/day rather than m/s
VELOCITY_W[,"est"] <- "minutes" %#% VELOCITY_W[, "est"] #converts to km/day rather than m/s
VELOCITY_W[,"high"] <- "minutes" %#% VELOCITY_W[,"high"] #converts to km/day rather than m/s
colnames(VELOCITY_W) <- c("τvelocity_low", "τvelocity_est", "τvelocity_high")

#wild adult
VELOCITY_A <- ctmm::meta(ADULT_FIT, variable = "tauvelocity", level = 0.95, units = FALSE)
VELOCITY_A <- as.data.frame(VELOCITY_A)
VELOCITY_A <- VELOCITY_A[-c(2,3),]
VELOCITY_A[,"low"] <- "minutes" %#% VELOCITY_A[,"low"] #converts to km/day rather than m/s
VELOCITY_A[,"est"] <- "minutes" %#% VELOCITY_A[, "est"] #converts to km/day rather than m/s
VELOCITY_A[,"high"] <- "minutes" %#% VELOCITY_A[,"high"] #converts to km/day rather than m/s
colnames(VELOCITY_A) <- c("τvelocity_low", "τvelocity_est", "τvelocity_high")
#bind all together
ORPHAN <- cbind(HR_O, SPEED_O, VELOCITY_O)
JUV <- cbind(HR_J, SPEED_J, VELOCITY_W)
ADULT <- cbind(HR_A, SPEED_A, VELOCITY_A)

ORPHAN$Status <- "Orphaned"
JUV$Status <- "Subadult"
ADULT$Status <- "Adult"

META_df <- rbind(ORPHAN, JUV, ADULT)
rm(HR_O, HR_W, SPEED_O, SPEED_W, VELOCITY_O, VELOCITY_W, ORPHAN, WILD)

Movement_df$ID <- as.character(Movement_df$ID) #for fitting gam


HR <- ctmm::meta(list(adult = ADULT_AKDE, juv = SUBADULT_AKDE), variable = "area", level = 0.95, units = FALSE) #units don't matter, it's a ratio
SPEED <- ctmm::meta(list(adult = ADULT_FIT, juv = SUBADULT_FIT), variable = "speed", level = 0.95, units = FALSE)
VELOCITY <- ctmm::meta(list(adult = ADULT_FIT, juv = SUBADULT_FIT), variable = "tauvelocity", level = 0.95, units = FALSE)






HR <- ggplot() +
  #total wild-raised
  geom_pointrange(data = subset(Movement_df, Status == "Adult"), aes(x = HR_est, y = ID, xmin = HR_low, xmax = HR_high, color = "Adult"), shape = 19) +
  geom_vline(xintercept = META_df[3,"HR_est"], color = "#DC058E", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[3,], aes(xmin = HR_low, xmax = HR_high, ymin = -Inf, ymax = Inf, color = "Adult"), color = NA, fill = "#DC058E", alpha = 0.045) +
  #total orphaned
  geom_pointrange(data = subset(Movement_df, Status == "Subadult"), aes(x = HR_est, y = ID, xmin = HR_low, xmax = HR_high, color = "Subadult"), shape = 17) +
  geom_vline(xintercept = META_df[2,"HR_est"], color = "#CEC416", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[2,], aes(xmin = HR_low, xmax = HR_high, ymin = -Inf, ymax = Inf, color = "Subadult"), color = NA, fill = "#ECD811", alpha = 0.065) +
  scale_color_manual("Individual Type", values = c("Subadult" = "#CEC416", "Adult" = "#DC058E")) +
  labs(title = "Home Range Size Comparison (log-scaled)", y = element_blank(), x = "Area (km^2)") +
  scale_x_continuous(trans = "log10") +
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5))  
plot(HR)
tauvelocity <- ggplot() +
  #total wild-raised
  geom_pointrange(data = subset(Movement_df, Status == "Adult"), aes(x = τvelocity_est, y = ID, xmin = τvelocity_low, xmax = τvelocity_high, color = "Adult"), shape = 19) +
  geom_vline(xintercept = META_df[3,"τvelocity_est"], color = "#DC058E", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[3,], aes(xmin = τvelocity_low, xmax = τvelocity_high, ymin = -Inf, ymax = Inf, color = "Adult"), color = NA, fill = "#DC058E", alpha = 0.045) +
  #total orphaned
  geom_pointrange(data = subset(Movement_df, Status == "Subadult"), aes(x = τvelocity_est, y = ID, xmin = τvelocity_low, xmax = τvelocity_high, color = "Subadult"), shape = 17) +
  geom_vline(xintercept = META_df[2,"τvelocity_est"], color = "#CEC416", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[2,], aes(xmin = τvelocity_low, xmax = τvelocity_high, ymin = -Inf, ymax = Inf, color = "Subadult"), color = NA, fill = "#ECD811", alpha = 0.065) +
  scale_color_manual("Individual Type", values = c("Subadult" = "#CEC416", "Adult" = "#DC058E")) +
  labs(title = "τvelocity Comparison", y = element_blank(), x = "τvelocity (minutes)") +
  #scale_x_continuous(trans = "log10") +
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5))  
plot(tauvelocity)





speed <- ggplot() +
  #total wild-raised
  geom_pointrange(data = subset(Movement_df, Status == "Adult"), aes(x = speed_est, y = ID, xmin = speed_low, xmax = speed_high, color = "Adult"), shape = 19) +
  geom_vline(xintercept = META_df[3,"speed_est"], color = "#DC058E", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[3,], aes(xmin = speed_low, xmax = speed_high, ymin = -Inf, ymax = Inf, color = "Adult"), color = NA, fill = "#DC058E", alpha = 0.045) +
  #total orphaned
  geom_pointrange(data = subset(Movement_df, Status == "Subadult"), aes(x = speed_est, y = ID, xmin = speed_low, xmax = speed_high, color = "Subadult"), shape = 17) +
  geom_vline(xintercept = META_df[2,"speed_est"], color = "#CEC416", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[2,], aes(xmin = speed_low, xmax = speed_high, ymin = -Inf, ymax = Inf, color = "Subadult"), color = NA, fill = "#ECD811", alpha = 0.065) +
  scale_color_manual("Individual Type", values = c("Subadult" = "#CEC416", "Adult" = "#DC058E")) +
  labs(title = "Speed Comparison (log-scaled)", y = element_blank(), x = "Speed (km/day)") +
  #scale_x_continuous(trans = "log10") +
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5))  
plot(speed)






Movement_param <- ggarrange(HR, tauvelocity, speed,
                            ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

Movement_param <- annotate_figure(Movement_param, top = text_grob("Comparison of Mean Movement Parameters between Adult and Subadult Wild-raised Individuals", face = "bold", size = 32))
plot(Movement_param)

all_plot <- ggarrange(Movement_param, total,
                      ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

plot(all_plot)




#window analysis
#load data
load("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/New_Dataframes/Wild_raised_df.rda")
#extract juveniles/subadults from this to plot against adults
Juveniles <- window_wild %>% filter(ID == "Anthony" | ID == "Gala" | ID == "Kyle" | ID == "Little_Rick" | ID == "Reid")
Adults <- window_wild %>% filter( ID %in% c("Alexander", "Annie", "Beto", "Bumpus", "Cate", "Christoffer", "Elaine", "Hannah", "Jackson", "Jane",
                                            "Larry", "Luigi", "Makao", "Margaret", "Maria", "Puji", "Rodolfo", "Sheron", "Thomas", "Delphine",
                                            "Segre"))
ANTHONY <- subset(Juveniles, ID == "Anthony")
ANTHONY$time_since_release = seq(2, by = 2, length.out = nrow(ANTHONY))
GALA <- subset(Juveniles, ID == "Gala")
GALA$time_since_release = seq(2, by = 2, length.out = nrow(GALA))
KYLE <- subset(Juveniles, ID == "Kyle")
KYLE$time_since_release = seq(2, by = 2, length.out = nrow(KYLE))
RICK <- subset(Juveniles, ID == "Little_Rick")
RICK$time_since_release = seq(2, by = 2, length.out = nrow(RICK))
REID <- subset(Juveniles, ID == "Reid")
REID$time_since_release = seq(2, by = 2, length.out = nrow(REID))
Juveniles <- rbind(ANTHONY, GALA, KYLE, RICK, REID)


ALEX <- subset(Adults, ID == "Alexander")
ALEX$time_since_release = seq(2, by = 2, length.out = nrow(ALEX))
ANNIE <- subset(Adults, ID == "Annie")
ANNIE$time_since_release = seq(2, by = 2, length.out = nrow(ANNIE))
BETO <- subset(Adults, ID == "Beto")
BETO$time_since_release = seq(2, by = 2, length.out = nrow(BETO))
BUMP <- subset(Adults, ID == "Bumpus")
BUMP$time_since_release = seq(2, by = 2, length.out = nrow(BUMP))
CATE <- subset(Adults, ID == "Cate")
CATE$time_since_release = seq(2, by = 2, length.out = nrow(CATE))
CHRIS <- subset(Adults, ID == "Christoffer")
CHRIS$time_since_release = seq(2, by = 2, length.out = nrow(CHRIS))
ELAINE <- subset(Adults, ID == "Elaine")
ELAINE$time_since_release = seq(2, by = 2, length.out = nrow(ELAINE))
HANNAH <- subset(Adults, ID == "Hannah")
HANNAH$time_since_release = seq(2, by = 2, length.out = nrow(HANNAH))
JACK <- subset(Adults, ID == "Jackson")
JACK$time_since_release = seq(2, by = 2, length.out = nrow(JACK))
JANE <- subset(Adults, ID == "Jane")
JANE$time_since_release = seq(2, by = 2, length.out = nrow(JANE))
LARRY <- subset(Adults, ID == "Larry")
LARRY$time_since_release = seq(2, by = 2, length.out = nrow(LARRY))
LUIGI <- subset(Adults, ID == "Luigi")
LUIGI$time_since_release = seq(2, by = 2, length.out = nrow(LUIGI))
MAK <- subset(Adults, ID == "Makao")
MAK$time_since_release = seq(2, by = 2, length.out = nrow(MAK))
MARG <- subset(Adults, ID == "Margaret")
MARG$time_since_release = seq(2, by = 2, length.out = nrow(MARG))
MAR <- subset(Adults, ID == "Maria")
MAR$time_since_release = seq(2, by = 2, length.out = nrow(MAR))
PUJI <- subset(Adults, ID == "Puji")
PUJI$time_since_release = seq(2, by = 2, length.out = nrow(PUJI))
ROD <- subset(Adults, ID == "Rodolfo")
ROD$time_since_release = seq(2, by = 2, length.out = nrow(ROD))
SHER <- subset(Adults, ID == "Sheron")
SHER$time_since_release = seq(2, by = 2, length.out = nrow(SHER))
TOM <- subset(Adults, ID == "Thomas")
TOM$time_since_release = seq(2, by = 2, length.out = nrow(TOM))
DEL <- subset(Adults, ID == "Delphine")
DEL$time_since_release = seq(2, by = 2, length.out = nrow(DEL))
SEG <- subset(Adults, ID == "Segre")
SEG$time_since_release = seq(2, by = 2, length.out = nrow(SEG))

Adults <- rbind(ALEX, ANNIE, BETO, BUMP, CATE, CHRIS, ELAINE, HANNAH, JACK, JANE, LARRY, LUIGI, MAK, MARG, MAR, PUJI, ROD, SHER, TOM, DEL, SEG)


AKDE_plot <- ggplot() +
  geom_point(data = subset(Adults, !is.na(c(HR_est))), aes(x = time_since_release, y = HR_est,  group = ID, color = "Adults"), shape = 19, alpha = 0.1) + #individual points
  geom_smooth(data = subset(Adults, !is.na(c(HR_est))), aes(x = time_since_release, y = HR_est, color = "Adults"), fill = "#DC058E") + #smoothed trend line
  geom_point(data = subset(Juveniles, !is.na(c(HR_est))), aes(x = time_since_release,  y = HR_est,  group = ID, color = "Juveniles"), shape = 17, alpha = 0.3) + #individual points
  geom_smooth(data = subset(Juveniles, !is.na(c(HR_est))), aes(x = time_since_release, y = HR_est, color = "Juveniles"), fill = "#CEC416") + #smoothed trend line
  #geom_vline(xintercept = as.numeric(as.Date("2025-02-22")), col = "grey70", linetype = "dashed") + #first dashed line
  #geom_vline(xintercept = as.numeric(as.Date("2025-05-19")), col = "grey70", linetype = "dashed") + #first dashed line
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5)) + #remove if you want the grid
  #scale_x_date(date_labels = "%b") + #changes x axis to just show months
  scale_color_manual("Individual Type", values = c("Adults" = "#DC058E", "Juveniles" = "#CEC416")) + #changes colors of individuals
  scale_y_continuous(trans = "log10") +
  labs(y = "Home Range Size (km^2)", x = element_blank(), title = "Home Range Size (log-scaled) over Time") 
plot(AKDE_plot)



SPEED_plot <- ggplot() +
  geom_point(data = subset(Adults, !is.na(c(Mean_Speed_est)) & Mean_Speed_est != "Inf"), aes(x = time_since_release, y = Mean_Speed_est, group = ID, color = "Adults"), shape = 19, alpha = 0.1) + #individual points
  geom_smooth(data = subset(Adults, !is.na(c(Mean_Speed_est)) & Mean_Speed_est != "Inf"), aes(x = time_since_release, y = Mean_Speed_est, color = "Adults"), fill = "#DC058E") + #smoothed trend line
  geom_point(data = subset(Juveniles, !is.na(c(Mean_Speed_est)) & Mean_Speed_est != "Inf"), aes(x = time_since_release, y = Mean_Speed_est, group = ID, color = "Juveniles"), shape = 17, alpha = 0.3) + #individual points
  geom_smooth(data = subset(Juveniles, !is.na(c(Mean_Speed_est)) & Mean_Speed_est != "Inf"), aes(x = time_since_release, y = Mean_Speed_est, color = "Juveniles"), fill = "#CEC416") + #smoothed trend line
  #geom_vline(xintercept = as.numeric(as.Date("2025-02-22")), col = "grey70", linetype = "dashed") + #first dashed line
  #geom_vline(xintercept = as.numeric(as.Date("2025-05-19")), col = "grey70", linetype = "dashed") + #first dashed line
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"),
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5)) + #remove if you want the grid
  #scale_x_date(date_labels = "%b") + #changes x axis to just show months
  scale_color_manual("Individual Type", values = c("Adults" = "#DC058E", "Juveniles" = "#CEC416")) + #changes colors of individuals
  labs(y = "Speed (km/day)", x = element_blank(), title = "Mean Speed over Time") 
plot(SPEED_plot)




Tauvelocity_plot <- ggplot() +
  geom_point(data = subset(Adults, !is.na(c(τvelocity_est))), aes(x = time_since_release, y = τvelocity_est, group = ID, color = "Adults"), shape = 19, alpha = 0.1) + #individual points
  geom_smooth(data = subset(Adults, !is.na(c(τvelocity_est))), aes(x = time_since_release, y = τvelocity_est, color = "Adults"), fill = "#DC058E") + #smoothed trend line
  geom_point(data = subset(Juveniles, !is.na(c(τvelocity_est))), aes(x = time_since_release, y = τvelocity_est, group = ID, color = "Juveniles"), shape = 17, alpha = 0.3) + #individual points
  geom_smooth(data = subset(Juveniles, !is.na(c(τvelocity_est))), aes(x = time_since_release, y = τvelocity_est, color = "Juveniles"), fill = "#CEC416") + #smoothed trend line
  #geom_vline(xintercept = as.numeric(as.Date("2025-02-22")), col = "grey70", linetype = "dashed") + #first dashed line
  #geom_vline(xintercept = as.numeric(as.Date("2025-05-19")), col = "grey70", linetype = "dashed") + #first dashed line
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5)) + #remove if you want the grid
  #scale_x_date(date_labels = "%b") + #changes x axis to just show months
  scale_color_manual("Individual Type", values = c("Adults" = "#DC058E", "Juveniles" = "#CEC416")) + #changes colors of individuals
  labs(y = "τ velocity (minutes)", x = element_blank(), title = "τ Velocity over Time") 
plot(Tauvelocity_plot)



total <- ggarrange(AKDE_plot, Tauvelocity_plot, SPEED_plot,
                   ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

total <- annotate_figure(total, top = text_grob("Window Analysis of Movement Parameters Comparing Adult and Subadult Wild-raised Individuals", face = "bold", size = 32))
plot(total)



ALL <- ggarrange (Movement_param, total,
                  ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")
plot(ALL)

