#Script for proportion vigilance and flight frequency (global models)

# Load packages
library(glmmTMB)
library(MuMIn)
library(dplyr)
library(performance)
library(stringr)


# Vigilance models --------------------------------------------------------

# Import data
Baboon_vigilance_df <- readRDS("data_derived/Baboon_vigilance_df.rds") 

# Get sample sizes
Baboon_vigilance_df %>% 
  count(year, cue_type)
Baboon_vigilance_df %>% 
  count(cue_type)

# Define reference levels
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(year = factor(year, levels = c(2021, 2024)),
         predator_cue = relevel(factor(predator_cue), ref = "Cheetah"),
         age_sex_class = relevel(factor(age_sex_class), ref = "Female_Adult_no_offspring"),
         Habitat = relevel(factor(Habitat), ref = "Open"))

#Filter to only predator cues and drop NA
Baboon_vigilance_df_nocontrol <- Baboon_vigilance_df %>% 
  dplyr::filter(predator_cue != "Control") %>% 
  filter(age_sex_class != "Unknown")

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
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue",
      grepl("^(Intercept)", term) ~ "Intercept"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Adult F (w/ offspring)",
      term == "age_sex_classJuvenile" ~ "Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Adult M",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~ "Species = Wild dog",
      term == "(Intercept)" ~ "Intercept"
    ),
    
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)



# Flight models -----------------------------------------------------------

# Import data
Baboon_flight_df <- readRDS("data_derived/Baboon_flight_binary_df.rds") 

# Get sample sizes
Baboon_flight_df %>% 
  count(cue_type)

# Define reference levels
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(year = factor(year, levels = c(2021, 2024)),
         predator_cue = relevel(factor(predator_cue), ref = "Cheetah"),
         age_sex_class = relevel(factor(age_sex_class), ref = "Female_Adult_no_offspring"),
         Habitat = relevel(factor(Habitat), ref = "Open"))

# Filter to only predator cues and drop NA
Baboon_flight_df_nocontrol <- Baboon_flight_df %>% 
  dplyr::filter(predator_cue != "Control") %>% 
  tidyr::drop_na() %>% 
  filter(age_sex_class != "Unknown")

# Scale numerical variables
Baboon_flight_df_nocontrol <- Baboon_flight_df_nocontrol %>%
  mutate(group_number = scale(group_number),
         day_number = scale(day_number))

# Global GLMM with binomial distribution
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
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue",
      grepl("^(Intercept)", term) ~ "Intercept"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Adult F (w/ offspring)",
      term == "age_sex_classJuvenile" ~ "Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Adult M",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~ "Species = Wild dog",
      term == "(Intercept)" ~ "Intercept"
    ),
    
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

# Combine and export

results_all <- bind_rows(results_vigilance, results_flight)

write.csv(results_all, "data_derived/model_global_results.csv", row.names = FALSE)

