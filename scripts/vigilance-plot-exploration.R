library(ggplot2)
library(dplyr)
library(patchwork)

model_output <- read.csv("data_derived/model_global_results_test_vigilance.csv") %>% 
  filter(is.na(Covariate) == FALSE)

# B
vigilance_plotB <- model_output %>% 
  filter(Response == "Vigilance_all") %>% 
  ggplot(aes(x = Level, y = Mean, col = Covariate)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
  geom_point(position = position_dodge(width = 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + # switch x and y coordinates
  labs(y = "Beta Coefficient", col = "Covariate", title = "Vigilance_all",
       subtitle = "All types of vigilance") +
  scale_color_manual(values = c("#0077B6", "#1e9b56",  "#4c00b0", "#f27229", "#ea3633", "#f8be1d"))

# C
vigilance_plotC <- model_output %>% 
  filter(Response == "Vigilance_scanning") %>% 
  ggplot(aes(x = Level, y = Mean, col = Covariate)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
  geom_point(position = position_dodge(width = 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + # switch x and y coordinates
  labs(y = "Beta Coefficient", col = "Covariate", title = "Vigilance_scanning",
       subtitle = "Scanning only") +
  scale_color_manual(values = c("#0077B6", "#1e9b56",  "#4c00b0", "#f27229", "#ea3633", "#f8be1d"))

# D
vigilance_plotD <- model_output %>% 
  filter(Response == "Vigilance_lookABR") %>% 
  ggplot(aes(x = Level, y = Mean, col = Covariate)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
  geom_point(position = position_dodge(width = 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + # switch x and y coordinates
  labs(y = "Beta Coefficient", col = "Covariate", title = "Vigilance_lookABR", 
       subtitle = "Staring, walking/staring, standing/staring, startling") +
  scale_color_manual(values = c("#0077B6", "#1e9b56",  "#4c00b0", "#f27229", "#ea3633", "#f8be1d"))


# E
vigilance_plotE <- model_output %>% 
  filter(Response == "Vigilance_notlookABR") %>% 
  ggplot(aes(x = Level, y = Mean, col = Covariate)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
  geom_point(position = position_dodge(width = 1)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + # switch x and y coordinates
  labs(y = "Beta Coefficient", col = "Covariate", title = "Vigilance_notlookABR",
       subtitle = "Scanning, walking non-vigilant") +
  scale_color_manual(values = c("#0077B6", "#1e9b56",  "#4c00b0", "#f27229", "#ea3633", "#f8be1d"))


(combined_plot <- (vigilance_plotB + vigilance_plotC + vigilance_plotD + vigilance_plotE) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14)))

# refined subset
(combined_plot <- (vigilance_plotB + vigilance_plotC + vigilance_plotD) +
    plot_annotation(
      tag_levels = 'A', 
      tag_suffix = ""
    ) &
    theme(plot.tag = element_text(size = 14)))

# Version with all on one plot
model_output %>%
  filter(Response %in% c("Vigilance_all", "Vigilance_scanning", "Vigilance_lookABR")) %>% 
  ggplot(aes(x = Level, y = Mean, col = Response)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, 
                position = position_dodge(width = 0.6), alpha=.75) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + 
  scale_color_manual(values = c(
    "Vigilance_all" = "#D55E00",
    "Vigilance_scanning" = "#0072B2",
    "Vigilance_lookABR" = "#009E73"),
    labels = c(
      "All",
      "Scanning",
      "Looking at ABR"
    )
    
  ) +
  labs(y = "Beta Coefficient", col = "Vigilance definition") 

ggsave("figures/vigilance-sensitivity.png", width = 6, height = 6, dpi = 300)

  

model_output %>%
  filter(Response %in% c("Vigilance_all", "Vigilance_scanning", "Vigilance_lookABR", "Vigilance_lookABR_cons")) %>% 
  ggplot(aes(x = Level, y = Mean, col = Response)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, 
                position = position_dodge(width = 0.6), alpha=.75) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + 
  scale_color_manual(values = c(
    "Vigilance_all" = "#D55E00",
    "Vigilance_scanning" = "#0072B2",
    "Vigilance_lookABR" = "#009E73",
    "Vigilance_lookABR_cons" = "purple"),
    labels = c(
      "All",
      "Scanning",
      "Looking at ABR",
      "Looking at ABR (conservative)"
    )
    
  ) +
  labs(y = "Beta Coefficient", col = "Vigilance definition") 
