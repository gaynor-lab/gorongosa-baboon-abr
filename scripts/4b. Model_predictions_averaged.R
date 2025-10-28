#Model predictions averaged

#load packages
library(ggeffects)
library(ggplot2)
library(dplyr)

#generate marginal means for each response variable

# Predicted vigilance by predator cue (averaging across all other variables)
vigilance_avg_pred_only <- ggpredict(Vigilance_global_model_both, terms = "predator_cue", bias_correction = TRUE) %>%
  rename(predator_cue = x)

# Predicted vigilance by year
vigilance_avg_year_only <- ggpredict(Vigilance_global_model_both, terms = "year", bias_correction = TRUE) %>%
  rename(year = x)

# Predicted vigilance by habitat
vigilance_avg_habitat_only <- ggpredict(Vigilance_global_model_both, terms = "Habitat", bias_correction = TRUE) %>%
  rename(habitat = x)

# Predicted vigilance by age-sex class
vigilance_avg_prey_only <- ggpredict(Vigilance_global_model_both, terms = "age_sex_class", bias_correction = TRUE) %>%
  rename(age_sex_class = x)

# Predicted flight frequency by predator cue
frequency_avg_pred_only <- ggpredict(Frequency_global_model_both, terms = "predator_cue", bias_correction = TRUE) %>%
  rename(predator_cue = x)

# Predicted flight frequency by year
frequency_avg_year_only <- ggpredict(Frequency_global_model_both, terms = "year", bias_correction = TRUE) %>%
  rename(year = x)

# Predicted flight frequency by habitat
frequency_avg_habitat_only <- ggpredict(Frequency_global_model_both, terms = "Habitat", bias_correction = TRUE) %>%
  rename(habitat = x)

# Predicted flight frequency by age-sex class
frequency_avg_prey_only <- ggpredict(Frequency_global_model_both, terms = "age_sex_class", bias_correction = TRUE) %>%
  rename(age_sex_class = x)

#Plot model predictions

#Vigilance by predator cue

#reorder predator cues for graphing
vigilance_avg_pred_only <- vigilance_avg_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog","Hyena", "Leopard", "Lion", "Control"))) 

predicted_avg_vigilance_pred_plot <- ggplot(vigilance_avg_pred_only, aes(x = predator_cue, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Predator Cue", y = "Predicted Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#vigilance by year
predicted_avg_vigilance_year_plot <- ggplot(vigilance_avg_year_only, aes(x = year, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Year", y = "Predicted Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#vigilance by habitat
predicted_avg_vigilance_habitat_plot <- ggplot(vigilance_avg_habitat_only, aes(x = habitat, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Habitat", y = "Predicted Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#vigilance by age-sex class

#change age-sex class names for graphing
vigilance_avg_prey_only <- vigilance_avg_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class
  ))

#remove added NA rows
vigilance_avg_prey_only <- vigilance_avg_prey_only %>%
  filter(!if_all(everything(), is.na))

predicted_avg_vigilance_prey_plot <- ggplot(vigilance_avg_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Age-sex class", y = "Predicted Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#flight frequency by predator cue

#reorder predator cues for graphing
frequency_avg_pred_only <- frequency_avg_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog","Hyena", "Leopard", "Lion", "Control"))) 

predicted_avg_frequency_pred_plot <- ggplot(frequency_avg_pred_only, aes(x = predator_cue, y = predicted)) +
geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Predator Cue", y = "Predicted Frequency of Flight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Flight frequency by year
predicted_avg_frequency_year_plot <- ggplot(frequency_avg_year_only, aes(x = year, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Year", y = "Predicted Frequency of Flight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Flight frequency by habitat
predicted_avg_frequency_habitat_plot <- ggplot(frequency_avg_habitat_only, aes(x = habitat, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Habitat", y = "Predicted Frequency of Flight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Flight frequency by age-sex class
#change age-sex class names for graphing
frequency_avg_prey_only <- frequency_avg_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class
  ))

#remove added NA rows
frequency_avg_prey_only <- frequency_avg_prey_only %>%
  filter(!if_all(everything(), is.na))

predicted_avg_frequency_prey_plot <- ggplot(frequency_avg_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(size = 3, color = "#023743FF") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Age-sex class", y = "Predicted Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


