#load packages
library(ctmm) #movement
library(dplyr)
library(ggplot2) #making plots
library(ggpubr) #arranging plots in multiples
library(tidyr) #drop_na function

load("~/Giant_Anteater_Orphan/FIXED/Results/RSFs/Mean_Orphan_Wild.rda")
O_W <- mean_all %>% 
  select(Status, Residence, contains("_low")) %>% 
  pivot_longer(cols = -c(Status, Residence), names_to = "type", values_to = "low") %>% 
  mutate(type = gsub("_low", "", type)) %>% 
  full_join(mean_all %>% 
              select(Status, Residence, contains("_est")) %>% 
              pivot_longer(cols = -c(Status, Residence), names_to = "type", values_to = "est") %>% 
              mutate(type = gsub("_est", "", type)), 
            by = c("Status", "Residence", "type")) %>% 
  full_join(mean_all %>% 
              select(Status, Residence, contains("_high")) %>% 
              pivot_longer(cols = -c(Status, Residence), names_to = "type", values_to = "high") %>% 
              mutate(type = gsub("_high", "", type)), 
            by = c("Status", "Residence", "type")) %>% 
  
  
  
  ggplot() +
  geom_errorbar(aes(x = est, xmin = low, xmax = high, y = type, color = Status), width = 0.5, size = 2.5,  position = position_dodge(width = 0.5)) +
  scale_color_manual("Population", values = c("Orphaned" = "#EA8109", "Wild-raised" = "#23C3A8")) +
  geom_vline(xintercept = 0, col = "grey70", linetype = "dashed", size = 1) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(color = "darkgray"), 
        plot.title = element_text(face = "bold", size = 32, family = "sans", hjust = 0.5, vjust = 0.2),      
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=16, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  labs(y = element_blank(), x = "Poisson Regression Coefficients", title = "Comparison of Habitat Selection Means Between Orphaned and Wild-raised Populations") 

plot(O_W)

COMP <- ggarrange(O_W, MEAN_S,
                  nrow = 1, ncol = 2)
COMP  <- annotate_figure(COMP, top = text_grob("Comparison of Habitat Selection Between Populations", face = "bold", size = 32))
plot(COMP)

