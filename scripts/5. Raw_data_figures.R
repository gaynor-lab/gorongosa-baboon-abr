#Script for graphing raw data 

#load packages
library(ggpattern)
library(ggplot2)
library(dplyr)
library(patchwork)


#import dataframes
Baboon_vigilance_stats_both <- readRDS("data_derived/Baboon_vigilance_stats_both.rds")
Baboon_flight_stats_both <- readRDS("data_derived/Baboon_flight_stats_both.rds") 
Baboon_vigilance_stats_both_nocontrol <- readRDS("data_derived/Baboon_vigilance_stats_both_nocontrol.rds") 
Baboon_flight_stats_both_nocontrol <- readRDS("data_derived/Baboon_flight_stats_both_nocontrol.rds") 


# Vigilance plots ---------------------------------------------------------

#Reorder predator cues for graphing
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Control", "Cheetah", "Wild dog", "Hyena", "Leopard","Lion"))) %>%
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#PROPORTION VIGILANCE PREDATOR CUE
vigilance_pred_plot <- ggplot(Baboon_vigilance_stats_both, 
                              aes(x = predator_cue, y = proportion_vigilant)) +
  geom_jitter(position = position_jitter(width = 0.2),
              size = 1.5, alpha = 0.3, color = "#023743") +
  geom_boxplot(fill = "#023743", alpha = 0.6, outlier.shape = NA, 
               position = position_dodge(width = 0.8)) +
  labs(x = "Predator Cue", y = "Proportion Vigilant") +
  scale_x_discrete(labels = c(
    "Control" = "Control",
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "none")   


#PROPORTION VIGILANCE YEAR
vigilance_year_plot <- ggplot(Baboon_vigilance_stats_both_nocontrol, aes(x = year, y = proportion_vigilant)) +
  geom_jitter(color = "#023743FF",  
              position = position_jitter(width = 0.2),  
              size = 1.5, alpha = 0.3) +  
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) + 
  labs(x = "Year", y = "Proportion Vigilant") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),                         
        axis.title.x = element_text(size = 12),                        
        panel.grid = element_blank(),
        legend.position = "none")  


#PROPORTION VIGILANCE HABITAT
vigilance_habitat_plot <- ggplot(Baboon_vigilance_stats_both_nocontrol, aes(x = Habitat, y = proportion_vigilant)) +
  geom_jitter(color = "#023743FF",  
              position = position_jitter(width = 0.2),  
              size = 1.5, alpha = 0.3) +  
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  labs(x = "Habitat", y = "Proportion Vigilant") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),                        
        axis.title.x = element_text(size = 12),                       
        axis.title.y = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "none") 

#PROPORTION VIGILANCE PREY
Baboon_vigilance_stats_both_nocontrol <- Baboon_vigilance_stats_both_nocontrol %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class 
  ))

vigilance_prey_plot <- 
  ggplot(Baboon_vigilance_stats_both_nocontrol, aes(x = age_sex_class, y = proportion_vigilant)) +
  geom_jitter(color = "#023743FF",  
              position = position_jitter(width = 0.2),  
              size = 1.5, alpha = 0.3) +  
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  labs(x = "Age and Sex Class", y = "Proportion Vigilant") +
  scale_x_discrete(labels = c(
    "Female no offspring" = "Female no \noffspring",
    "Female with offspring" = "Female w/ \noffspring",
    "Male" = "Male",
    "Juvenile" = "Juvenile"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),                         
        axis.title.x = element_text(size = 12),                        
        axis.title.y = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "none")  


vigilance_combined <- (vigilance_pred_plot + vigilance_year_plot) /
  (vigilance_habitat_plot + vigilance_prey_plot) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14))

ggsave(vigilance_combined, 
       filename = "figures/vigilance_summary.png", 
       width = 6, height = 6)

# Flight plots ------------------------------------------------------------


#FLIGHT BY PREDATOR CUE
#calculate flight frequency
flight_pred <- Baboon_flight_stats_both %>%
  group_by(predator_cue) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight = flight_present / total, 
    se = sqrt((flight * (1 - flight)) / total),  # standard error
    .groups = "drop"
  )

#Reorder predator cues for graphing
flight_pred <- flight_pred %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Control", "Cheetah", "Wild dog", "Hyena", "Leopard","Lion"))) %>%
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA


flight_pred_plot <- ggplot(flight_pred, aes(x = predator_cue, y = flight)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9),
           fill = "#023743",  
           color = "black",
           alpha = 0.8) +
  geom_errorbar(aes(ymin = flight - se, ymax = flight + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Predator Cue", y = "Proportion Fleeing") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog",
    "Control" = "Control"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "right", 
        panel.grid = element_blank())


#FLIGHT BY YEAR
#calculate flight frequency
flight_year <- Baboon_flight_stats_both_nocontrol %>%
  group_by(year) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight = flight_present / total,  
    se = sqrt((flight * (1 - flight)) / total),  # standard error
    .groups = "drop"
  )

flight_year_plot <-ggplot(flight_year, aes(x = year, y = flight)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  geom_errorbar(aes(ymin = flight - se, ymax = flight + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Year", y = "Proportion Fleeing") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),                         
        axis.title.x = element_text(size = 12),                        
        axis.title.y = element_text(size = 12),
        axis.title = element_text(size = 12))


#FLIGHT HABITAT

#calculate flight frequency
flight_habitat <- Baboon_flight_stats_both_nocontrol %>%
  group_by(Habitat) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight = flight_present / total,  
    se = sqrt((flight * (1 - flight)) / total),  # standard error
    .groups = "drop"
  )

flight_habitat_plot <- ggplot(flight_habitat, aes(x = Habitat, y = flight, fill = Habitat)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = flight - se, ymax = flight + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Open" = "#023743FF", "Closed" = "#023743FF")) +
  labs(x = "Habitat", y = "Proportion Fleeing") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),                         
        axis.title.x = element_text(size = 12),                        
        axis.title.y = element_text(size = 12),
        axis.title = element_text(size = 12))

#FLIGHT PREY

#calculate flight frequency
Baboon_flight_graph_both <- Baboon_flight_stats_both_nocontrol %>%
  group_by(age_sex_class) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight = flight_present / total,  
    se = sqrt((flight * (1 - flight)) / total),  # standard error
    .groups = "drop"
  )

Baboon_flight_graph_both <- Baboon_flight_graph_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    age_sex_class %in% c("Juvenile") ~ "Juvenile",
    TRUE ~ age_sex_class  
  ))

flight_prey_plot <- 
  ggplot(Baboon_flight_graph_both, aes(x = age_sex_class, y = flight)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  geom_errorbar(aes(ymin = flight - se, ymax = flight + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Age and Sex Class", y = "Proportion Fleeing") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),                         
        axis.title.x = element_text(size = 12),                        
        axis.title.y = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
  scale_x_discrete(labels = c(
    "Female no offspring" = "Female no \noffspring",
    "Female with offspring" = "Female w/ \noffspring",
    "Male" = "Male",
    "Juvenile" = "Juvenile"))

# make all y-axes the same
common_ylims <- c(0, 0.32)
flight_pred_plot   <- flight_pred_plot + ylim(common_ylims)
flight_year_plot   <- flight_year_plot + ylim(common_ylims)
flight_habitat_plot <- flight_habitat_plot + ylim(common_ylims)
flight_prey_plot    <- flight_prey_plot + ylim(common_ylims)

flight_combined <- (flight_pred_plot + flight_year_plot) /
  (flight_habitat_plot + flight_prey_plot) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14))

ggsave(flight_combined, 
       filename = "figures/flight_summary.png", 
       width = 6, height = 6)
