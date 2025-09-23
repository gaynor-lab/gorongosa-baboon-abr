#Script for graphing raw data 

#load packages
library(ggpattern)
library(ggplot2)

#PROPORTION VIGILANCE PREDATOR CUE
vigilance_pred_plot <- ggplot(Baboon_vigilance_graph_both, 
                              aes(x = predator_cue, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743", alpha = 0.6, outlier.shape = NA, 
               position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitter(width = 0.2),
              size = 1.5, alpha = 0.8, color = "#023743") +
  labs(x = "Predator Cue", y = "Proportion Vigilant") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none")   


#PROPORTION VIGILANCE YEAR
vigilance_year_plot <- ggplot(Baboon_vigilance_stats_both, aes(x = year, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) + 
  geom_jitter(color = "#023743FF",  
              position = position_jitter(width = 0.2),  
              size = 1.5, alpha = 0.8) +  
  labs(x = "Year", y = "Proportion Vigilant") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         
        axis.title.x = element_text(size = 16),                        
        panel.grid = element_blank(),
        legend.position = "none")  


#PROPORTION VIGILANCE HABITAT

#Strip plot for proportion of vigilance by habitat
vigilance_habitat_plot <- ggplot(Baboon_vigilance_stats_both, aes(x = Habitat, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  
              position = position_jitter(width = 0.2),  
              size = 1.5, alpha = 0.8) +  
  labs(x = "Habitat", y = "Proportion Vigilant") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                        
        axis.title.x = element_text(size = 16),                       
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none") 

#PROPORTION VIGILANCE PREY
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class 
  ))

vigilance_prey_plot <- 
  ggplot(Baboon_vigilance_stats_both, aes(x = age_sex_class, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  
              position = position_jitter(width = 0.2),  
              size = 1.5, alpha = 0.8) +  
  labs(x = "Age and Sex Class", y = "Proportion Vigilant") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         
        axis.title.x = element_text(size = 16),                        
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none")  


#FREQUENCY OF FLIGHT BY PREDATOR CUE
#calculate flight frequency
flight_frequency_pred_only <- Baboon_frequency_stats_both %>%
  group_by(predator_cue) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total, 
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

#Reorder predator cues for graphing
flight_frequency_pred_only <- flight_frequency_pred_only %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog", "Hyena", "Leopard","Lion", "Control"))) %>%
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA


frequency_pred_plot <- ggplot(flight_frequency_pred_only, aes(x = predator_cue, y = flight_frequency)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9),
           fill = "#023743",  
           color = "black",
           alpha = 0.8) +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Predator Cue", y = "Frequency of Flight") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "right", 
        panel.grid = element_blank())


#FREQUENCY OF FLIGHT BY YEAR
#calculate flight frequency
flight_frequency_year <- Baboon_frequency_stats_both %>%
  group_by(year) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

frequency_year_plot <-ggplot(flight_frequency_year, aes(x = year, y = flight_frequency)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Year", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         
        axis.title.x = element_text(size = 16),                        
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 12))


#FREQUENCY OF FLIGHT HABITAT

#calculate flight frequency
flight_frequency_habitat <- Baboon_frequency_stats_both %>%
  group_by(Habitat) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

frequency_habitat_plot <- ggplot(flight_frequency_habitat, aes(x = Habitat, y = flight_frequency, fill = Habitat)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Open" = "#023743FF", "Closed" = "#023743FF")) +
  labs(x = "Habitat", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         
        axis.title.x = element_text(size = 16),                        
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 12))

#FLIGHT FREQUENCY PREY

#calculate flight frequency
Baboon_frequency_graph_both <- Baboon_frequency_stats_both %>%
  group_by(age_sex_class) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

Baboon_frequency_graph_both <- Baboon_frequency_graph_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    age_sex_class %in% c("Juvenile") ~ "Juvenile",
    TRUE ~ age_sex_class  
  ))

frequency_prey_plot <- 
  ggplot(Baboon_frequency_graph_both, aes(x = age_sex_class, y = flight_frequency)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Age and Sex Class", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         
        axis.title.x = element_text(size = 16),                        
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 12))
