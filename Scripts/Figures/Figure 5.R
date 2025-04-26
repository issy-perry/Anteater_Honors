#load packages
library(ctmm) #movement
library(dplyr)
library(ggplot2) #making plots
library(ggpubr) #arranging plots in multiples
library(tidyr) #pivot_longer()




# cover/non cover seasonally
load("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/New_Dataframes/Wild_raised_select_df.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/New_Dataframes/Orphaned_select_df.rda")


SELECT_wild$group <- "Wild-raised"
SELECT_orphan$group <- "Orphaned"
#SELECT_orphan <- SELECT_orphan[-c(14)]
SELECT <- rbind(SELECT_orphan, SELECT_wild)
SELECT$group <- as.factor(SELECT$group)




#non-cover with good GAM ---------------------------------------------------------------------------------------------non-cover-----------------------------------------------------
SMOOTH <- ggplot(SELECT, aes(x = numeric_date, y = Non_cover_est, color = group)) +
  geom_smooth(aes(x = numeric_date, y = Non_cover_high, group = group), method = loess, se = FALSE)+
  geom_smooth(aes(x = numeric_date, y = Non_cover_low, group = group), method = loess, se = FALSE) +
  #scale_x_date(date_labels = "%b") + #changes x axis to just show months
  scale_y_continuous(limits = c(-100, 100)) 
plot(SMOOTH)
#https://stackoverflow.com/questions/32772454/geom-ribbon-error-aesthetics-must-either-be-length-one
ggsm <- ggplot_build(SMOOTH)

d1 <- ggsm$data[[1]]
d2 <- ggsm$data[[2]]

df1 <- data.frame(x = d1[d1$group == 1,]$x,
                  ymin = d2[d2$group == 1,]$y,
                  ymax = d1[d1$group == 1,]$y)

df2 <- data.frame(x = d1[d1$group == 2,]$x,
                  ymin = d2[d2$group == 2,]$y,
                  ymax = d1[d1$group == 2,]$y)

NONCOVER <- ggplot() + 
  geom_ribbon(data = df1, aes(x=x, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = "#2AC7AD", color = NA, alpha = 0.3) +
  geom_ribbon(data = df2, aes(x=x, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = "#EA8109", color = NA, alpha = 0.4) +
  geom_pointrange(data = subset(SELECT_orphan, !is.na(c(Non_cover_low, Non_cover_est, Non_cover_high))), aes(x = numeric_date, ymin = Non_cover_low, y = Non_cover_est, ymax = Non_cover_high, color = "Orphaned"), shape = 15, alpha = 0.3) + #individual points
  geom_pointrange(data = subset(SELECT_wild, !is.na(c(Non_cover_low, Non_cover_est, Non_cover_high))), aes(x = numeric_date, ymin = Non_cover_low, y = Non_cover_est, ymax = Non_cover_high, color = "Wild-raised"), shape = 17, alpha = 0.3) + #individual points
  geom_hline(yintercept = 0, col = "#797979", linetype = "dashed", linewidth = 1) +
  scale_color_manual("Individual Type", values = c("Orphaned" = "#2AC7AD", "Wild-raised" = "#EA8109")) + #changes colors of individuals
  #scale_x_date(date_breaks = "1 month", date_labels = "%b") +       
  scale_y_continuous(limits = c(-100, 100)) +
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5, vjust = 0.2), axis.title.y = element_text(size = 14), legend.title = element_blank(),
        legend.text = element_text(size=16, family = "sans", face = "bold") ) + #remove if you want the grid       
  labs(y = "Poisson Regression Coefficients", x = element_blank(), title = "Selection for Non-cover Land Types over Time") 

plot(NONCOVER)







#-----------------------------------------------------------------------------------------------------------cover
SMOOTH <- ggplot(SELECT, aes(x = numeric_date, y = Cover_est, color = group)) +
  geom_smooth(aes(x = numeric_date, y = Cover_high, group = group), method = loess, se = FALSE)+
  geom_smooth(aes(x = numeric_date, y = Cover_low, group = group), method = loess, se = FALSE) +
  #scale_x_date(date_labels = "%b") + #changes x axis to just show months
  scale_y_continuous(limits = c(-100, 100)) 
plot(SMOOTH)
#https://stackoverflow.com/questions/32772454/geom-ribbon-error-aesthetics-must-either-be-length-one
ggsm <- ggplot_build(SMOOTH)

d1 <- ggsm$data[[1]]
d2 <- ggsm$data[[2]]

df1 <- data.frame(x = d1[d1$group == 1,]$x,
                  ymin = d2[d2$group == 1,]$y,
                  ymax = d1[d1$group == 1,]$y)

df2 <- data.frame(x = d1[d1$group == 2,]$x,
                  ymin = d2[d2$group == 2,]$y,
                  ymax = d1[d1$group == 2,]$y)

COVER <- ggplot()+
  geom_ribbon(data = df1, aes(x=x, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = "#2AC7AD", alpha = 0.4) +
  geom_ribbon(data = df2, aes(x=x, ymin = ymin, ymax = ymax), inherit.aes = FALSE, fill = "#EA8109", alpha = 0.35) +
  geom_pointrange(data = SELECT_orphan, aes(x = numeric_date, ymin = Cover_low, y = Cover_est, ymax = Cover_high, color = "Orphaned"), shape = 15, alpha = 0.3) + #individual points
  geom_pointrange(data = SELECT_wild, aes(x = numeric_date, ymin = Cover_low, y = Cover_est, ymax = Cover_high, color = "Wild-raised"), shape = 17, alpha = 0.3) + #individual points
  geom_hline(yintercept = 0, col = "#797979", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(limits = c(-100, 100)) +
  scale_color_manual("Individual Type", values = c("Orphaned" = "#2AC7AD", "Wild-raised" = "#EA8109")) + #changes colors of individuals
  #scale_x_date(date_breaks = "1 month", date_labels = "%b") +       
  theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"),
        legend.text = element_text(size=16, family = "sans", face = "bold"), axis.title.y = element_text(size = 14), legend.title = element_blank(), plot.title = element_text(face = "bold", size = 20, family = "sans", hjust = 0.5, vjust = 0.2)) + #remove if you want the grid       
  labs(y = "Poisson Regression Coefficients", x = element_blank(), title = "Selection for Non-cover Land Types over Time") 

plot(COVER)


COVER_PLOTS <- ggarrange(COVER, NONCOVER,
                         nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
COVER_PLOTS <- annotate_figure(COVER_PLOTS, top = text_grob("Comparison of Habitat Selection Between Both Populations over Time", face = "bold", size = 32))
plot(COVER_PLOTS)

#geom_pointrange(data = subset(SELECT_orphan, !is.na(c(Non_cover_low, Non_cover_est, Non_cover_high))), aes(x = month_day, ymin = Non_cover_low, y = Non_cover_est, ymax = Non_cover_high, color = "Orphaned"), shape = 15, alpha = 0.3) + #individual points
# geom_pointrange(data = subset(SELECT_wild, !is.na(c(Non_cover_low, Non_cover_est, Non_cover_high))), aes(x = month_day, ymin = Non_cover_low, y = Non_cover_est, ymax = Non_cover_high, color = "Wild-raised"), shape = 17, alpha = 0.3) + #individual points
#  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
#geom_vline(xintercept = as.numeric(as.Date("2025-02-22")), col = "grey70", linetype = "dashed") + #first dashed line
#geom_vline(xintercept = as.numeric(as.Date("2025-05-19")), col = "grey70", linetype = "dashed") + #first dashed line
# theme(panel.background = element_blank(), axis.line = element_line(color = "darkgray"), 
#      plot.title = element_text(face = "bold", size = 13, family = "sans", hjust = 0.5)) + #remove if you want the grid
#scale_x_date(date_labels = "%b") + #changes x axis to just show months

#scale_color_manual("Individual Type", values = c("Orphaned" = "#2AC7AD", "Wild-raised" = "#EA8109")) + #changes colors of individuals
#  scale_y_continuous(limits = c(-100, 100)) +
#  labs(y = "Poisson Regression Coefficients", x = element_blank(), title = "Selection for Non-cover Land Types over Time") 





SELECT <- ggarrange(COVER_plot, NONCOVER_plot,
                    nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")


#ratios of movement behavior ---------------------------------
#load data
load("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Total_meta.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Successful_meta.rda")
load("~/Giant_Anteater_Orphan/FIXED/Results/Window_Analysis/META/Unsuccessful_meta.rda")



HR_win <- ggplot() +
  geom_line(data = META_o_df, 
            aes(x = time_since_release, y = HR_est, color = "Ratio of Orphaned to Wild-Raised Population"), 
            size = 1) +
  geom_ribbon(data = META_o_df, 
              aes (x = time_since_release, ymin = HR_low, ymax = HR_high, fill = "Ratio of Orphaned to Wild-Raised Population"), 
              alpha = 0.1) +
  geom_hline(yintercept = 1, 
             col = "grey", 
             linetype = "dashed",
             size = 0.75) +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(values = c("Ratio of Orphaned to Wild-Raised Population" = "#23C3A8")) + 
  scale_color_manual(values = c("Ratio of Orphaned to Wild-Raised Population" = "#23C3A8")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "darkgray"), 
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 24, family = "sans", hjust = 0.5, vjust = 0.2),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18)) + #remove if you want the grid
  labs(y = "Home Range Size Ratio (Orphaned/Wild-raised)", x = "Time Since Release (days)", title = "Ratio for Home Range Size over Time (log-scaled)") 
plot(HR_win)




#options(scipen = 999)
dif_win <- ggplot() +
  geom_line(data = META_o_df, 
            aes(x = time_since_release, y = diffusion_est, color = "Ratio of Orphaned to Wild-Raised Population"), 
            size = 1) +
  geom_ribbon(data = META_o_df, 
              aes (x = time_since_release, ymin = diffusion_low, ymax = diffusion_high, fill = "Ratio of Orphaned to Wild-Raised Population"), 
              alpha = 0.1) +
  geom_hline(yintercept = 1, 
             col = "grey", 
             linetype = "dashed",
             size = 0.75) +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(values = c("Ratio of Orphaned to Wild-Raised Population" = "#23C3A8")) + 
  scale_color_manual(values = c("Ratio of Orphaned to Wild-Raised Population" = "#23C3A8")) + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "darkgray"), 
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 24, family = "sans", hjust = 0.5, vjust = 0.2),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18)) + #remove if you want the grid
  labs(y = "Diffusion Ratio (Orphaned/Wild-raised)", x = "Time Since Release (days)", title = "Ratio for Diffusion over Time (log-scaled)") 
plot(dif_win)


WIND <- ggarrange(HR_win, dif_win,
                  nrow = 1, ncol = 2)


WINDOW_TOTAL <- ggarrange(COVER_PLOTS, WIND,
                          nrow = 2, ncol = 1)


plot(WINDOW_TOTAL)


