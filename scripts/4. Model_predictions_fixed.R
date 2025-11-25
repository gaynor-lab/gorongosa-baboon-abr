#Script for generating and graphing model predictions for each behavioural response variable 

#load packages
library(ggplot2)

#Import dataframes
Baboon_vigilance_stats_both <- readRDS("data_derived/Baboon_vigilance_stats_both.rds")
Baboon_frequency_stats_both <- readRDS("data_derived/Baboon_frequency_stats_both.rds")

#PROPORTION VIGILANCE BY PREDATOR CUE
# Create a new dataset with combinations of explanatory variables
vigilance_pred_only <- expand.grid(
  predator_cue = unique(Baboon_vigilance_stats_both$predator_cue), # Vary predator cue
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),  
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
vigilance_pred_only$predicted <- predict(Vigilance_global_model_both, 
                                         newdata = vigilance_pred_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
pred_with_se <- predict(Vigilance_global_model_both, 
                        newdata = vigilance_pred_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
vigilance_pred_only$se <- pred_with_se$se.fit
vigilance_pred_only$lower <- plogis(pred_with_se$fit - 1.96 * pred_with_se$se.fit)  # Back-transform to response
vigilance_pred_only$upper <- plogis(pred_with_se$fit + 1.96 * pred_with_se$se.fit)

#reorder predator cues for graphing
vigilance_pred_only <- vigilance_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog","Hyena", "Leopard", "Lion", "Control"))) 

#plot for predicted vigilance by predator cue
predicted_vigilance_pred_plot <-
  ggplot(vigilance_pred_only, aes(x = predator_cue, y = predicted)) +
  geom_point(size = 3, color = "#023743") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2) +
  labs(
    x = "Predator Cue",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  
  )

#PROPORTION VIGILANCE BY YEAR
# Create a new dataset with combinations of explanatory variables
vigilance_year_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),  
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = unique(Baboon_vigilance_stats_both$year)
)

# Get predictions on the response scale
vigilance_year_only$predicted <- predict(Vigilance_global_model_both, 
                                         newdata = vigilance_year_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
year_with_se <- predict(Vigilance_global_model_both, 
                        newdata = vigilance_year_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
vigilance_year_only$se <- year_with_se$se.fit
vigilance_year_only$lower <- plogis(year_with_se$fit - 1.96 * year_with_se$se.fit)  # Back-transform to response
vigilance_year_only$upper <- plogis(year_with_se$fit + 1.96 * year_with_se$se.fit)

#plot for predicted vigilance by year
predicted_vigilance_year_plot <-
  ggplot(vigilance_year_only, aes(x = year, y = predicted)) +
  geom_point(size = 3, color = "#023743") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2) +
  labs(
    x = "Year",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  
  )

#PROPORTION VIGILANCE BY HABITAT
# Create a new dataset with combinations of explanatory variables
vigilance_habitat_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),  
  Habitat = unique(Baboon_vigilance_stats_both$Habitat),
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024")) 
)

# Get predictions on the response scale
vigilance_habitat_only$predicted <- predict(Vigilance_global_model_both, 
                                            newdata = vigilance_habitat_only, 
                                            type = "response", 
                                            re.form = NA)  # Exclude random effects
View(vigilance_habitat_only)
# Get predictions with standard errors
habitat_with_se <- predict(Vigilance_global_model_both, 
                           newdata = vigilance_habitat_only, 
                           type = "link",  # Get predictions on link scale for CIs
                           se.fit = TRUE,
                           re.form = NA)

# Add confidence intervals on response scale
vigilance_habitat_only$se <- habitat_with_se$se.fit
vigilance_habitat_only$lower <- plogis(habitat_with_se$fit - 1.96 * habitat_with_se$se.fit)  # Back-transform to response
vigilance_habitat_only$upper <- plogis(habitat_with_se$fit + 1.96 * habitat_with_se$se.fit)


#plot for predicted vigilance by habitat type
predicted_vigilance_habitat_plot <-
  ggplot(vigilance_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(color = "#023743FF", position = position_dodge(width = 0.5), size = 3) +  # Set single color for points
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                color = "#023743FF",  
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#PROPORTION VIGILANCE BY AGE AND SEX CLASS
# Create a new dataset with combinations of explanatory variables
vigilance_prey_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),  
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),
  age_sex_class = unique(Baboon_vigilance_stats_both$age_sex_class),
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  offspring = factor("No", levels = levels(Baboon_vigilance_stats_both$offspring)),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
vigilance_prey_only$predicted <- predict(Vigilance_global_model_both, 
                                         newdata = vigilance_prey_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
prey_with_se <- predict(Vigilance_global_model_both, 
                        newdata = vigilance_prey_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
vigilance_prey_only$se <- prey_with_se$se.fit
vigilance_prey_only$lower <- plogis(prey_with_se$fit - 1.96 * prey_with_se$se.fit)  # Back-transform to response
vigilance_prey_only$upper <- plogis(prey_with_se$fit + 1.96 * prey_with_se$se.fit)

#change age sex class names for graphing
vigilance_prey_only <- vigilance_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

#plot for predicted vigilance by habitat type
predicted_vigilance_prey_plot <- 
  ggplot(vigilance_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#023743FF") +  
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Age and Sex Class",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#FREQUENCY BY PREDATOR CUE
frequency_pred_only <- expand.grid(
  predator_cue = unique(Baboon_frequency_stats_both$predator_cue), # Vary predator cue
  Habitat = factor("Open", levels = levels(Baboon_frequency_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_frequency_stats_both$age_sex_class)),  
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
frequency_pred_only$predicted <- predict(Frequency_global_model_both, 
                                         newdata = frequency_pred_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
frequency_with_se <- predict(Frequency_global_model_both, 
                             newdata = frequency_pred_only, 
                             type = "link",  # Get predictions on link scale for CIs
                             se.fit = TRUE,
                             re.form = NA)

# Add confidence intervals on response scale
frequency_pred_only$se <- frequency_with_se$se.fit
frequency_pred_only$lower <- frequency_with_se$fit - 1.96 * frequency_with_se$se.fit
frequency_pred_only$upper <- frequency_with_se$fit + 1.96 * frequency_with_se$se.fit

#convert onto same scale
frequency_pred_only$lower_resp <- plogis(frequency_pred_only$lower)
frequency_pred_only$upper_resp <- plogis(frequency_pred_only$upper)

#reorder predator cues for graphing
frequency_pred_only <- frequency_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah","Wild dog","Hyena", "Leopard", "Lion", "Control"))) 

#plot frequency of flight by predator cue
predicted_frequency_pred_plot <-
  ggplot(frequency_pred_only, aes(x = predator_cue, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(x = "Predator Cue", y = "Predicted Frequency of Flight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#FREQUENCY BY YEAR
frequency_year_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_frequency_stats_both$predator_cue)), 
  Habitat = factor("Open", levels = levels(Baboon_frequency_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_frequency_stats_both$age_sex_class)),  
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = unique(Baboon_frequency_stats_both$year)
)

# Get predictions on the response scale
frequency_year_only$predicted <- predict(Frequency_global_model_both, 
                                         newdata = frequency_year_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
frequency_with_se <- predict(Frequency_global_model_both, 
                             newdata = frequency_year_only, 
                             type = "link",  # Get predictions on link scale for CIs
                             se.fit = TRUE,
                             re.form = NA)

# Add confidence intervals on response scale
frequency_year_only$se <- frequency_with_se$se.fit
frequency_year_only$lower <- frequency_with_se$fit - 1.96 * frequency_with_se$se.fit
frequency_year_only$upper <- frequency_with_se$fit + 1.96 * frequency_with_se$se.fit

#convert onto same scale
frequency_year_only$lower_resp <- plogis(frequency_year_only$lower)
frequency_year_only$upper_resp <- plogis(frequency_year_only$upper)


#plot frequency of flight by predator cue
predicted_frequency_year_plot <-
  ggplot(frequency_year_only, aes(x = year, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(x = "Year", y = "Predicted Frequency of Flight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#FLIGHT FREQUENCY BY HABITAT
frequency_habitat_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_frequency_stats_both$predator_cue)),  
  Habitat = unique(Baboon_frequency_stats_both$Habitat),
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_frequency_stats_both$age_sex_class)),
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),  
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024")) 
)

# Get predictions on the response scale
frequency_habitat_only$predicted <- predict(Frequency_global_model_both, 
                                            newdata = frequency_habitat_only, 
                                            type = "response", 
                                            re.form = NA)  # Exclude random effects
# Get predictions with standard errors
habitat_with_se <- predict(Frequency_global_model_both, 
                           newdata = frequency_habitat_only, 
                           type = "link",  # Get predictions on link scale for CIs
                           se.fit = TRUE,
                           re.form = NA)

# Add confidence intervals on response scale
frequency_habitat_only$se <- habitat_with_se$se.fit
frequency_habitat_only$lower <- habitat_with_se$fit - 1.96 * habitat_with_se$se.fit 
frequency_habitat_only$upper <- habitat_with_se$fit + 1.96 * habitat_with_se$se.fit

#convert onto same scale
frequency_habitat_only$lower_resp <- plogis(frequency_habitat_only$lower)
frequency_habitat_only$upper_resp <- plogis(frequency_habitat_only$upper)


#plot for predicted frequency of flight by habitat type
predicted_frequency_habitat_plot <-
  ggplot(frequency_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(color = "#023743FF", position = position_dodge(width = 0.5), size = 3) +  
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                color = "#023743FF",  # Set single color for error bars
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = "Predicted Frequency of Flight"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#FREQUENCY OF FLIGHT BY AGE AND SEX CLASS
frequency_prey_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_frequency_stats_both$predator_cue)),  
  Habitat = factor("Open", levels = levels(Baboon_frequency_stats_both$Habitat)),
  age_sex_class = unique(Baboon_frequency_stats_both$age_sex_class),
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),
  day_number = mean(Baboon_vigilance_stats_both$day_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
frequency_prey_only$predicted <- predict(Frequency_global_model_both, 
                                         newdata = frequency_prey_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
prey_with_se <- predict(Frequency_global_model_both, 
                        newdata = frequency_prey_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
frequency_prey_only$se <- prey_with_se$se.fit
frequency_prey_only$lower <- prey_with_se$fit - 1.96 * prey_with_se$se.fit
frequency_prey_only$upper <- prey_with_se$fit + 1.96 * prey_with_se$se.fit

#convert onto same scale
frequency_prey_only$lower_resp <- plogis(frequency_prey_only$lower)
frequency_prey_only$upper_resp <- plogis(frequency_prey_only$upper)


#change age sex class names for graphing
frequency_prey_only <- frequency_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

#plot for flight freqeuncy by age and sex class
predicted_frequency_prey_plot <- 
  ggplot(frequency_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#023743FF") +  
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Age and Sex Class",
    y = "Predicted Frequency of Flight"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
