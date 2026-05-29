#Script for model averaging for proportion vigilance and flight frequency

#load packages
library(glmmTMB)
library(dplyr)
library(MuMIn)
library(performance)
library(stringr)


# Vigilance models --------------------------------------------------------

#Import derived dataframes
Baboon_vigilance_stats <- readRDS("data_derived/Baboon_vigilance_stats.rds")
Baboon_vigilance_stats_24 <- readRDS("data_derived/Baboon_vigilance_stats_24.rds")

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



#Ensure reference levels are consisent across all models

#set 2021 as reference level
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(year = factor(year, levels = c(2021, 2024)))

#set cheetah as reference level for predator cue
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Cheetah"))

#set female adult as reference level
Baboon_vigilance_stats_both$age_sex_class <- factor(Baboon_vigilance_stats_both$age_sex_class)
Baboon_vigilance_stats_both$age_sex_class <- relevel(Baboon_vigilance_stats_both$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_vigilance_stats_both$Habitat <- factor(Baboon_vigilance_stats_both$Habitat)
Baboon_vigilance_stats_both$Habitat <- relevel(Baboon_vigilance_stats_both$Habitat, ref = "Open")

#Filter to only predator cues
Baboon_vigilance_stats_both_nocontrol <- Baboon_vigilance_stats_both %>% 
  dplyr::filter(predator_cue != "Control")

#Global GLMM using beta distribution
Vigilance_global_model_both <- glmmTMB(proportion_vigilant_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                       data = Baboon_vigilance_stats_both_nocontrol,
                                       family = beta_family(),
                                       na.action = na.fail) 

#Get results
summary(Vigilance_global_model_both)
print(Vigilance_global_model_both)

# 95% confidence intervals
confint(Vigilance_global_model_both, level = 0.95)

#R-squared 
r.squaredGLMM(Vigilance_global_model_both)


# Flight models -----------------------------------------------------------

#Import derived dataframes
Baboon_flight_binary_stats <- readRDS("data_derived/Baboon_flight_binary_stats.rds")
Baboon_flight_binary_stats_24 <- readRDS("data_derived/Baboon_flight_binary_stats_24.rds")

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

#Ensure all reference levels are consistent across models

#set control as reference level for Predator.cue
Baboon_flight_binary_stats_both <- Baboon_flight_binary_stats_both %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Cheetah"))

#set 2021 as reference level
Baboon_flight_binary_stats_both <- Baboon_flight_binary_stats_both %>%
  mutate(year = factor(year, levels = c(2021, 2024)))

#set female adult as reference level
Baboon_flight_binary_stats_both$age_sex_class <- factor(Baboon_flight_binary_stats_both$age_sex_class)
Baboon_flight_binary_stats_both$age_sex_class <- relevel(Baboon_flight_binary_stats_both$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_flight_binary_stats_both$Habitat <- factor(Baboon_flight_binary_stats_both$Habitat)
Baboon_flight_binary_stats_both$Habitat <- relevel(Baboon_flight_binary_stats_both$Habitat, ref = "Open")

#Filter to only predator cues
Baboon_flight_binary_stats_both_nocontrol <- Baboon_flight_binary_stats_both %>% 
  dplyr::filter(predator_cue != "Control")

#Global GLMM with binomial distribution
Flight_global_model_both <- glmmTMB(flight_present ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                       data = Baboon_flight_binary_stats_both_nocontrol,
                                       family = binomial(),
                                       na.action = na.fail)

# Model summary
summary(Flight_global_model_both)

# 95% confidence intervals for averaged parameters
confint(Flight_global_model_both, level = 0.95)

# R² squared
r2(Flight_global_model_both)

#Extract top models
Flight_model_selection_table <- as.data.frame(Flight_models_both)
flight_top_models <- subset(Flight_model_selection_table, delta < 2)

#export data frames
saveRDS(Baboon_flight_binary_stats_both, "data_derived/Baboon_flight_stats_both.rds")
saveRDS(Baboon_flight_binary_stats_both_nocontrol, "data_derived/Baboon_flight_stats_both_nocontrol.rds")
saveRDS(Flight_global_model_both, "data_derived/Flight_global_model_both.rds")
