#Model predictions averaged

#load packages
library(ggeffects)
library(ggplot2)
library(dplyr)

#Import dataframes
Vigilance_global_model_both <- readRDS("data_derived/Vigilance_global_model_both.rds")
Frequency_global_model_both <- readRDS("data_derived/Frequency_global_model_both.rds")

#generate marginal means for each response variable

# Predicted vigilance by predator cue (averaging across all other variables)
vigilance_avg_pred_only <- ggpredict(Vigilance_global_model_both, terms = "predator_cue", bias_correction = TRUE) %>%
  rename(predator_cue = x)

vigilance_avg_pred_only <- vigilance_avg_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog","Hyena", "Leopard", "Lion", "Control"))) 

# Predicted vigilance by year
vigilance_avg_year_only <- ggpredict(Vigilance_global_model_both, terms = "year", bias_correction = TRUE) %>%
  rename(year = x)

# Predicted vigilance by habitat
vigilance_avg_habitat_only <- ggpredict(Vigilance_global_model_both, terms = "Habitat", bias_correction = TRUE) %>%
  rename(habitat = x)

# Predicted vigilance by age-sex class
vigilance_avg_prey_only <- ggpredict(Vigilance_global_model_both, terms = "age_sex_class", bias_correction = TRUE) %>%
  rename(age_sex_class = x)

#remove added NA rows
vigilance_avg_prey_only <- vigilance_avg_prey_only %>%
  filter(!if_all(everything(), is.na))

# Predicted flight frequency by predator cue
frequency_avg_pred_only <- ggpredict(Frequency_global_model_both, terms = "predator_cue", bias_correction = TRUE) %>%
  rename(predator_cue = x)

#reorder predator cues for graphing
frequency_avg_pred_only <- frequency_avg_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog","Hyena", "Leopard", "Lion", "Control"))) 

# Predicted flight frequency by year
frequency_avg_year_only <- ggpredict(Frequency_global_model_both, terms = "year", bias_correction = TRUE) %>%
  rename(year = x)

# Predicted flight frequency by habitat
frequency_avg_habitat_only <- ggpredict(Frequency_global_model_both, terms = "Habitat", bias_correction = TRUE) %>%
  rename(habitat = x)

# Predicted flight frequency by age-sex class
frequency_avg_prey_only <- ggpredict(Frequency_global_model_both, terms = "age_sex_class", bias_correction = TRUE) %>%
  rename(age_sex_class = x)

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