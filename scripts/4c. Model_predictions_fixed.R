#Model predictions fixed effects

# Predicted vigilance by predator cue
vigilance_fixed_pred_only <- ggpredict(
  Vigilance_global_model_both,
  terms = "predator_cue",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(predator_cue = x)

# Predicted vigilance by year
vigilance_fixed_year_only <- ggpredict(
  Vigilance_global_model_both,
  terms = "year",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(year = x)

# Predicted vigilance by habitat
vigilance_fixed_habitat_only <- ggpredict(
  Vigilance_global_model_both,
  terms = "Habitat",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(habitat = x)

# Predicted vigilance by age-sex class
vigilance_fixed_prey_only <- ggpredict(
  Vigilance_global_model_both,
  terms = "age_sex_class",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(age_sex_class = x)

#change age-sex class names for graphing
vigilance_fixed_prey_only <- vigilance_fixed_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class
  ))
View(vigilance_fixed_prey_only)

#remove added NA rows
vigilance_fixed_prey_only <- vigilance_fixed_prey_only %>%
  filter(!if_all(everything(), is.na))

# Predicted flight frequency by predator cue
frequency_fixed_pred_only <- ggpredict(
  Frequency_global_model_both,
  terms = "predator_cue",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(predator_cue = x)

# Predicted flight frequency by year
frequency_fixed_year_only <- ggpredict(
  Frequency_global_model_both,
  terms = "year",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(year = x)

# Predicted flight frequency by habitat
frequency_fixed_habitat_only <- ggpredict(
  Frequency_global_model_both,
  terms = "Habitat",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(habitat = x)

# Predicted flight frequency by age-sex class
frequency_fixed_prey_only <- ggpredict(
  Frequency_global_model_both,
  terms = "age_sex_class",
  type = "fixed",
  bias_correction = TRUE
) %>%
  rename(age_sex_class = x)

#change age-sex class names for graphing
frequency_fixed_prey_only <- frequency_fixed_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class
  ))

#remove added NA rows
frequency_fixed_prey_only <- frequency_fixed_prey_only %>%
  filter(!if_all(everything(), is.na))
