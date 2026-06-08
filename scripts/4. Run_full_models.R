#Script for proportion vigilance and flight frequency (global models)

#load packages
library(glmmTMB)
library(dplyr)
library(MuMIn)
library(performance)
library(stringr)


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
  mutate(group_number = scale(group_number),
         day_number = scale(day_number))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_vigilant_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
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

# Join together
results_vigilance <- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Group size",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Group size",
      
      term == "year2024" ~ "Year = 2024",
      
      term == "HabitatClosed" ~ "Habitat = Closed",
      
      term == "age_sex_classFemale_Adult_with_offspring" ~
        "Class = Female adult w offspring",
      
      term == "age_sex_classJuvenile" ~
        "Class = Juvenile",
      
      term == "age_sex_classMale_Adult" ~
        "Class = Male adult",
      
      term == "predator_cueHyena" ~
        "Species = Hyena",
      
      term == "predator_cueLeopard" ~
        "Species = Leopard",
      
      term == "predator_cueLion" ~
        "Species = Lion",
      
      term == "predator_cueWild dog" ~
        "Species = Wild dog"
    ),
    
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)



# Flight models -----------------------------------------------------------

# Import data
Baboon_flight_df <- readRDS("data_derived/Baboon_flight_binary_df.rds")

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
  mutate(group_number = scale(group_number),
         day_number = scale(day_number))

#Global GLMM with binomial distribution
Flight_global_model <- glmmTMB(flight_present ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
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

# Join together
results_flight <- coefs_flight %>%
  select(term, Estimate) %>%
  left_join(
    cis_flight %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Flight",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Group size",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Group size",
      
      term == "year2024" ~ "Year = 2024",
      
      term == "HabitatClosed" ~ "Habitat = Closed",
      
      term == "age_sex_classFemale_Adult_with_offspring" ~
        "Class = Female adult w offspring",
      
      term == "age_sex_classJuvenile" ~
        "Class = Juvenile",
      
      term == "age_sex_classMale_Adult" ~
        "Class = Male adult",
      
      term == "predator_cueHyena" ~
        "Species = Hyena",
      
      term == "predator_cueLeopard" ~
        "Species = Leopard",
      
      term == "predator_cueLion" ~
        "Species = Lion",
      
      term == "predator_cueWild dog" ~
        "Species = Wild dog"
    ),
    
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

# Combine and export

results_all <- bind_rows(results_vigilance, results_flight)

write.csv(results_all, "data_derived/model_global_results.csv", row.names = FALSE)

