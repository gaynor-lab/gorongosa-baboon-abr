library(ggplot2)
library(dplyr)
library(patchwork)

model_output <- read.csv("data_derived/model_global_results.csv") %>% 
  filter(Level != "Intercept")

# Flight
flight_plot <- model_output %>% 
  filter(Response == "Flight") %>% 
  ggplot(aes(x = Level, y = Mean, col = Covariate)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold")) +
  ylim(c(-1.25,1.75)) +
  coord_flip() + # switch x and y coordinates
  labs(y = "Beta Coefficient", col = "Covariate", title = "Flight") +
  scale_color_manual(values = c("#0077B6", "#1e9b56",  "#4c00b0", "#f27229", "#ea3633", "#f8be1d"))


# Vigilance
vigilance_plot <- model_output %>% 
  filter(Response == "Vigilance") %>% 
  ggplot(aes(x = Level, y = Mean, col = Covariate)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
  geom_point(position = position_dodge(width = 1),
             size = 3) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold")) +
  coord_flip() + # switch x and y coordinates
  #ylim(c(-0.5,1.1)) +
  labs(y = "Beta Coefficient", col = "Covariate", title = "Vigilance") +
  scale_color_manual(values = c("#0077B6", "#1e9b56",  "#4c00b0", "#f27229", "#ea3633", "#f8be1d"))

combined_plot <- (flight_plot + vigilance_plot) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14))

combined_plot

ggsave("figures/combined_beta_coefficients_global.png", combined_plot, width = 10, height = 5)
ggsave("figures/publication/Figure3.png", combined_plot, width = 10, height = 5)
