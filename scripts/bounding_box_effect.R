#Script for model averaging for proportion vigilance and flight frequency

#load packages
library(glmmTMB)
library(dplyr)
library(MuMIn)
library(performance)
library(stringr)
library(ggplot2)
library(ggeffects)
library(patchwork)

# Vigilance models --------------------------------------------------------

# Import data
Baboon_vigilance_df <- readRDS("data_derived/Baboon_vigilance_df.rds") %>% 
  filter(age_sex_class != "Unknown")

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df <- Baboon_vigilance_df %>% 
  filter(nonoccluded_frames > 30) 

#Fix typo in file_name 
Baboon_vigilance_df$file_name <- gsub(
  "^2024_F07_7260057_Baboon\\.AVI$",
  "2024_F07_07260057_Baboon.AVI",
  Baboon_vigilance_df$file_name
)

#make new column with month and day to test for sound habituation
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))

#change Wild dog name to match in both datasets
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix issue with spacing in predator cues
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(predator_cue = str_trim(predator_cue))

#Transform data for beta distribution using Smithson & Verkuilen transformation
#this is needed because beta distribution requires values to be 0<x<1 but in proportion_vigilance we have exact 0s and 1s
#this transformation compresses the scale of the data, taking values away from exactly 0 and 1
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n())



#Ensure reference levels are consisent across all models

#set 2021 as reference level
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(year = factor(year, levels = c(2021, 2024)))

#set cheetah as reference level for predator cue
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Cheetah"))

#set female adult as reference level
Baboon_vigilance_df$age_sex_class <- factor(Baboon_vigilance_df$age_sex_class)
Baboon_vigilance_df$age_sex_class <- relevel(Baboon_vigilance_df$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_vigilance_df$Habitat <- factor(Baboon_vigilance_df$Habitat)
Baboon_vigilance_df$Habitat <- relevel(Baboon_vigilance_df$Habitat, ref = "Open")

#Filter to only predator cues
Baboon_vigilance_df_nocontrol <- Baboon_vigilance_df %>% 
  dplyr::filter(predator_cue != "Control")

# Scale numerical variables
Baboon_vigilance_df_nocontrol <- Baboon_vigilance_df_nocontrol %>%
  mutate(group_number_scaled = scale(group_number),
         day_number_scaled = scale(day_number),
         initial_max_dimension_scaled = scale(initial_max_dimension),
         max_dimension_scaled = scale(max_dimension))


#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_vigilant_beta ~ 
                                    age_sex_class*initial_max_dimension_scaled  + (1|site),
                                  data = Baboon_vigilance_df_nocontrol,
                                  family = beta_family(),
                                  na.action = na.fail) 

#Get results
summary(Vigilance_global_model)
print(Vigilance_global_model)

# 95% confidence intervals
confint(Vigilance_global_model, level = 0.95)

#R-squared 
r.squaredGLMM(Vigilance_global_model)

# Extract coefficients
coefs_vigilance <- summary(Vigilance_global_model)$coefficients$cond %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") 

# Extract confidence intervals
cis_vigilance <- confint(Vigilance_global_model, level = 0.95) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") 

# merge estimates + CIs
plot_df <- coefs_vigilance %>%
  select(term, Estimate, `Std. Error`) %>%
  left_join(
    cis_vigilance %>%
      select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(term = reorder(term, Estimate))

# quick forest plot
ggplot(plot_df, aes(x = Estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Effect size (estimate ± 95% CI)",
    y = NULL
  )

library(purrr)

# Get original mean and SD (before scaling)
orig_mean <- mean(Baboon_vigilance_df_nocontrol$initial_max_dimension, na.rm = TRUE)
orig_sd   <- sd(Baboon_vigilance_df_nocontrol$initial_max_dimension, na.rm = TRUE)

# Find empirical ranges per group (UNSCALED)
ranges <- Baboon_vigilance_df_nocontrol %>%
  group_by(age_sex_class) %>%
  summarise(
    xmin = min(initial_max_dimension, na.rm = TRUE),
    xmax = max(initial_max_dimension, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # convert to scaled units (since model uses scaled predictor)
  mutate(
    xmin_scaled = (xmin - orig_mean) / orig_sd,
    xmax_scaled = (xmax - orig_mean) / orig_sd
  )

# Generate marginal effects per group
me <- map_dfr(1:nrow(ranges), function(i) {
  
  g <- as.character(ranges$age_sex_class[i])
  
  pred <- ggpredict(
    Vigilance_global_model,
    terms = c(
      paste0(
        "initial_max_dimension_scaled [",
        ranges$xmin_scaled[i], ":",
        ranges$xmax_scaled[i],
        " by=0.1]"
      ),
      "age_sex_class"
    ),
    bias_correction = TRUE
  ) %>%
    as.data.frame()
  
  # keep only this group
  pred <- pred %>% filter(group == g)
  
  return(pred)
})

# Back-transform x-axis
me <- me %>%
  mutate(
    x_original = x * orig_sd + orig_mean
  )

# Plot
a <- ggplot(me, aes(x = x_original, y = predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, color = NA) +
  geom_rug(
    data = Baboon_vigilance_df_nocontrol %>%
      rename(group = age_sex_class),
    aes(x = initial_max_dimension),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  ylim(0, 1) +
  facet_wrap(~ group,
             labeller = labeller(
               group = c(
                 "Female_Adult_no_offspring" = "Female adult (no offspring)",
                 "Female_Adult_with_offspring" = "Female adult (with offspring)",
                 "Juvenile" = "Juvenile",
                 "Male_Adult" = "Adult male"
               ))
             ) +
  theme_bw() +
  labs(
    x = "Proximity to camera (initial max dimension, pixels)",
    y = "Predicted proportion vigilant"
  ) +
  theme(legend.position = "none")






# Flight models -----------------------------------------------------------

# Import data
Baboon_flight_df <- readRDS("data_derived/Baboon_flight_binary_df.rds") %>% 
  filter(age_sex_class != "Unknown")

#Fix typo in file_name 
Baboon_flight_df$file_name <- gsub(
  "^2024_F07_7260057_Baboon\\.AVI$",
  "2024_F07_07260057_Baboon.AVI",
  Baboon_flight_df$file_name
)

#make new columun with month and day to test for sound habituation
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))

#change Wild dog name to match in both datasets
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix spacing issue
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(predator_cue = str_trim(predator_cue))

#Ensure all reference levels are consistent across models

#set control as reference level for Predator.cue
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Cheetah"))

#set 2021 as reference level
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(year = factor(year, levels = c(2021, 2024)))

#set female adult as reference level
Baboon_flight_df$age_sex_class <- factor(Baboon_flight_df$age_sex_class)
Baboon_flight_df$age_sex_class <- relevel(Baboon_flight_df$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_flight_df$Habitat <- factor(Baboon_flight_df$Habitat)
Baboon_flight_df$Habitat <- relevel(Baboon_flight_df$Habitat, ref = "Open")

#Filter to only predator cues
Baboon_flight_df_nocontrol <- Baboon_flight_df %>% 
  dplyr::filter(predator_cue != "Control")

# Drop NA
Baboon_flight_df_nocontrol <- Baboon_flight_df_nocontrol %>% 
  tidyr::drop_na()

# Scale numerical variables
Baboon_flight_df_nocontrol <- Baboon_flight_df_nocontrol %>%
  mutate(group_number_scaled = scale(group_number),
         day_number_scaled = scale(day_number),
         initial_max_dimension_scaled = scale(initial_max_dimension),
         max_dimension_scaled = scale(max_dimension))


#Global GLMM with binomial distribution
Flight_global_model <- glmmTMB(flight_present ~ 
                                 age_sex_class * initial_max_dimension_scaled + (1|site),
                               data = Baboon_flight_df_nocontrol,
                               family = binomial(),
                               na.action = na.fail)

# Model summary
summary(Flight_global_model)

# 95% confidence intervals for averaged parameters
confint(Flight_global_model, level = 0.95)

# R² squared
r2(Flight_global_model)

# Extract coefficients
coefs_flight <- summary(Flight_global_model)$coefficients$cond %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") 

# Extract confidence intervals
cis_flight <- confint(Flight_global_model, level = 0.95) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") 

# merge estimates + CIs
plot_df <- coefs_flight %>%
  select(term, Estimate, `Std. Error`) %>%
  left_join(
    cis_flight %>%
      select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(term = reorder(term, Estimate))

# quick forest plot
ggplot(plot_df, aes(x = Estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Effect size (estimate ± 95% CI)",
    y = NULL
  )


# Get original mean and SD (before scaling)
orig_mean <- mean(Baboon_flight_df_nocontrol$initial_max_dimension, na.rm = TRUE)
orig_sd   <- sd(Baboon_flight_df_nocontrol$initial_max_dimension, na.rm = TRUE)

# Find empirical ranges per group (UNSCALED)
ranges <- Baboon_flight_df_nocontrol %>%
  group_by(age_sex_class) %>%
  summarise(
    xmin = min(initial_max_dimension, na.rm = TRUE),
    xmax = max(initial_max_dimension, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # convert to scaled units (because model uses scaled predictor)
  mutate(
    xmin_scaled = (xmin - orig_mean) / orig_sd,
    xmax_scaled = (xmax - orig_mean) / orig_sd
  )

# Generate marginal effects per group (THIS IS THE KEY FIX)
me <- map_dfr(1:nrow(ranges), function(i) {
  
  g <- as.character(ranges$age_sex_class[i])
  
  # generate predictions only within this group's range
  pred <- ggpredict(
    Flight_global_model,
    terms = c(
      paste0(
        "initial_max_dimension_scaled [",
        ranges$xmin_scaled[i], ":",
        ranges$xmax_scaled[i],
        " by=0.1]"
      ),
      "age_sex_class"
    ),
    bias_correction = TRUE
  ) %>%
    as.data.frame()
  
  # KEEP ONLY this group (ggpredict returns all groups otherwise)
  pred <- pred %>% filter(group == g)
  
  return(pred)
})

# Back-transform x-axis
me <- me %>%
  mutate(
    x_original = x * orig_sd + orig_mean
  )

# Plot
b <- ggplot(me, aes(x = x_original, y = predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, color = NA) +
  geom_rug(
    data = Baboon_flight_df_nocontrol %>%
      rename(group = age_sex_class),
    aes(x = initial_max_dimension),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  ylim(0, 1) +
  facet_wrap(~ group,
             labeller = labeller(
               group = c(
                 "Female_Adult_no_offspring" = "Female adult (no offspring)",
                 "Female_Adult_with_offspring" = "Female adult (with offspring)",
                 "Juvenile" = "Juvenile",
                 "Male_Adult" = "Adult male"
               ))
             ) +
  theme_bw() +
  labs(
    x = "Proximity to camera (initial max dimension, pixels)",
    y = "Predicted probability of flight"
  ) +
  theme(legend.position = "none")

(combined_plot <- (b / a) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14)))

ggsave("figures/bounding-box-plot.png", width = 8, height = 10, dpi = 300)
