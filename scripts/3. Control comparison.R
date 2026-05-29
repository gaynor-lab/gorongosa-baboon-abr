# Compare the control and predator cues as a first pass

library(dplyr)
library(stringr)
library(glmmTMB)


# Import and prep data ----------------------------------------------------

#Import derived dataframes
Baboon_vigilance_stats <- readRDS("data_derived/Baboon_vigilance_stats.rds")
Baboon_vigilance_stats_24 <- readRDS("data_derived/Baboon_vigilance_stats_24.rds")
Baboon_flight_binary_stats <- readRDS("data_derived/Baboon_flight_binary_stats.rds")
Baboon_flight_binary_stats_24 <- readRDS("data_derived/Baboon_flight_binary_stats_24.rds")

#PROPORTION OF TIME SPENT VIGILANT

#Join 2021 and 2024 vigilance stats
Baboon_vigilance_stats_both <- bind_rows(Baboon_vigilance_stats, Baboon_vigilance_stats_24)

#Fix typo in file_name 
Baboon_vigilance_stats_both$file_name <- gsub(
  "^2024_F07_7260057_Baboon\\.AVI$",
  "2024_F07_07260057_Baboon.AVI",
  Baboon_vigilance_stats_both$file_name
)

#make new column with month and day to test for sound habituation
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
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
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix issue with spacing in predator cues
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = str_trim(predator_cue))

#Transform data for beta distribution using Smithson & Verkuilen transformation
#this is needed because beta distribution requires values to be 0<x<1 but in proportion_vigilance we have exact 0s and 1s
#this transformation compresses the scale of the data, taking values away from exactly 0 and 1
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n())

# Create variable for predator vs control
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>% 
  mutate(cue_type = case_when(predator_cue == "Control" ~ "Control",
                              predator_cue %in% c("Leopard", "Lion", "Wild dog", "Cheetah", "Hyena") ~ "Predator"))

#Join 2021 and 2024 flight stats
Baboon_flight_binary_stats_both <- bind_rows(Baboon_flight_binary_stats, Baboon_flight_binary_stats_24)


#Fix typo in file_name 
Baboon_flight_binary_stats_both$file_name <- gsub(
  "^2024_F07_7260057_Baboon\\.AVI$",
  "2024_F07_07260057_Baboon.AVI",
  Baboon_flight_binary_stats_both$file_name
)

#make new columun with month and day to test for sound habituation
Baboon_flight_binary_stats_both <- Baboon_flight_binary_stats_both %>%
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
Baboon_flight_binary_stats_both <- Baboon_flight_binary_stats_both %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix spacing issue
Baboon_flight_binary_stats_both <- Baboon_flight_binary_stats_both %>%
  mutate(predator_cue = str_trim(predator_cue))


# Create variable for predator vs control
Baboon_flight_binary_stats_both <- Baboon_flight_binary_stats_both %>% 
  mutate(cue_type = case_when(predator_cue == "Control" ~ "Control",
                              predator_cue %in% c("Leopard", "Lion", "Wild dog", "Cheetah", "Hyena") ~ "Predator"))



# Comparison --------------------------------------------------------------

vigilance_comparison <- glmmTMB(proportion_vigilant_beta ~ cue_type + (1|site),
                                       data = Baboon_vigilance_stats_both,
                                       family = beta_family(),
                                       na.action = na.fail) 

summary(vigilance_comparison)
# P = 0.034

flight_comparison <- glmmTMB(flight_present ~ cue_type + (1|site),
                                       data = Baboon_flight_binary_stats_both,
                                       family = binomial(),
                                       na.action = na.fail)

summary(flight_comparison)
# P = 0.031

library(ggplot2)

ggplot(Baboon_vigilance_stats_both, aes(x = cue_type, y = proportion_vigilant_beta)) + 
  geom_boxplot() +
  theme_bw()

# Count up flight frequency across treatments
count(Baboon_flight_binarystats_both, cue_type, flight_present)
13/(105+13) # 11% fled from control
139/(605+139) # 19% fled from predator

# Look into vigilance a bit more
pred_only <- Baboon_vigilance_stats_both %>% filter(cue_type == "Predator")
pred_vigilant_only <- pred_only %>% filter(proportion_vigilant > 0)
nrow(pred_vigilant_only)/nrow(pred_only) # 82.2% of videos (excluding flight)
nrow(pred_vigilant_only)/744 # 71% of videos (including flight)
mean(pred_only$proportion_vigilant)
sd(pred_only$proportion_vigilant)

control_only <- Baboon_vigilance_stats_both %>% filter(cue_type == "Control")
control_vigilant_only <- control_only %>% filter(proportion_vigilant > 0)
nrow(control_vigilant_only)/nrow(control_only) # 72.5% of videos
mean(control_only$proportion_vigilant)
sd(control_only$proportion_vigilant)
