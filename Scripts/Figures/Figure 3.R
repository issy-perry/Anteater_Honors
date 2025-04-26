#load package
library(ctmm)
#library(tidyverse)
library(ggplot2)
library(ggpubr)


#load data
#load files
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Total_META_df.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/Fits/Total_Movement_df.rda")
#change values to numeric I guess

Movement_df$ID <- as.character(Movement_df$ID) #for fitting gam
META_df <- META_df[-c(1,2,4,5),]



HR <- ggplot() +
  #total wild-raised
  geom_pointrange(data = subset(Movement_df, Status == "Wild-raised"), 
                  aes(x = HR_est, y = ID, xmin = HR_low, xmax = HR_high, color = "Wild-raised"), 
                  shape = 17) +
  geom_vline(xintercept = META_df[2,"HR_est"],
             color = "#EA8109", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[2,], aes(xmin = HR_low, xmax = HR_high, ymin = -Inf, ymax = Inf, color = "Wild-raised"), 
            color = NA, fill = "#EA8109", alpha = 0.15) +
  #total orphaned
  geom_pointrange(data = subset(Movement_df, Status == "Orphaned"), 
                  aes(x = HR_est, y = ID, xmin = HR_low, xmax = HR_high, color = "Orphaned"), 
                  shape = 15) +
  geom_vline(xintercept = META_df[1,"HR_est"], color = "#23C3A8", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[1,], 
            aes(xmin = HR_low, xmax = HR_high, ymin = -Inf, ymax = Inf, color = "Orphaned"), 
            color = NA, fill = "#23C3A8", alpha = 0.1) +
  scale_color_manual("Individual Type", 
                     values = c("Orphaned" = "#23C3A8", "Wild-raised" = "#EA8109")) +
  labs(title = "Home Range Size Comparison (log-scaled)", y = element_blank(), x = "Area (km^2)") +
  scale_x_continuous(trans = "log10") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5, vjust = 0.2),      
        legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size=16, family = "sans", face = "bold"),)  
plot(HR)




tauvelocity <- ggplot() +
  #total wild-raised
  geom_pointrange(data = subset(Movement_df, Status == "Wild-raised"), 
                  aes(x = τvelocity_est, y = ID, xmin = τvelocity_low, xmax = τvelocity_high, color = "Wild-raised"), 
                  shape = 17) +
  geom_vline(xintercept = META_df[2,"τvelocity_est"], 
             color = "#EA8109", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[2,], 
            aes(xmin = τvelocity_low, xmax = τvelocity_high, ymin = -Inf, ymax = Inf, color = "Wild-raised"), 
            color = NA, fill = "#EA8109", alpha = 0.15) +
  #total orphaned
  geom_pointrange(data = subset(Movement_df, Status == "Orphaned"), 
                  aes(x = τvelocity_est, y = ID, xmin = τvelocity_low, xmax = τvelocity_high, color = "Orphaned"), 
                  shape = 15) +
  geom_vline(xintercept = META_df[1,"τvelocity_est"], 
             color = "#23C3A8", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[1,], 
            aes(xmin = τvelocity_low, xmax = τvelocity_high, ymin = -Inf, ymax = Inf, color = "Orphaned"), 
            color = NA, fill = "#23C3A8", alpha = 0.15) +
  scale_color_manual("Individual Type", values = c("Orphaned" = "#23C3A8", "Wild-raised" = "#EA8109")) +
  labs(title = "τvelocity Comparison (log-scaled)",
       y = element_blank(),
       x = "τvelocity (minutes)") +
  scale_x_continuous(trans = "log10") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5, vjust = 0.2),      
        legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size=16, family = "sans", face = "bold"),)  
plot(tauvelocity)



speed <- ggplot() +
  #total wild-raised
  geom_pointrange(data = subset(Movement_df, Status == "Wild-raised"), 
                  aes(x = speed_est, y = ID, xmin = speed_low, xmax = speed_high, color = "Wild-raised"), 
                  shape = 17) +
  geom_vline(xintercept = META_df[2,"speed_est"], 
             color = "#EA8109", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[2,], 
            aes(xmin = speed_low, xmax = speed_high, ymin = -Inf, ymax = Inf, color = "Wild-raised"), 
            color = NA, fill = "#EA8109", alpha = 0.15) +
  #total orphaned
  geom_pointrange(data = subset(Movement_df, Status == "Orphaned"),
                  aes(x = speed_est, y = ID, xmin = speed_low, xmax = speed_high, color = "Orphaned"), 
                  shape = 15) +
  geom_vline(xintercept = META_df[1,"speed_est"], 
             color = "#23C3A8", lty = "dashed", lwd = 1) + #area_est
  geom_rect(data = META_df[1,], 
            aes(xmin = speed_low, xmax = speed_high, ymin = -Inf, ymax = Inf, color = "Orphaned"), 
            color = NA, fill = "#23C3A8", alpha = 0.15) +
  scale_color_manual("Individual Type", values = c("Orphaned" = "#23C3A8", "Wild-raised" = "#EA8109")) +
  labs(title = "Speed Comparison (log-scaled)", y = element_blank(), x = "Speed (km/day)") +
  scale_x_continuous(trans = "log10") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5, vjust = 0.2),      
        legend.title = element_blank(),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size=16, family = "sans", face = "bold"),)  
plot(speed)


movement_plot <- ggarrange(HR, tauvelocity, speed,
                           ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

movement_plot <- annotate_figure(movement_plot, top = text_grob("Comparison of Mean Movement Parameters Between the Orphaned and Wild-raised Populations", face = "bold", size = 32))
plot(movement_plot)


ggsave(movement_plot, file = "~/Giant_Anteater_Orphan/FIXED/Mean_Movement_Params_RR_dis.png", width = 17, height = 8.5, units = "in", dpi = 600, bg = "transparent")

