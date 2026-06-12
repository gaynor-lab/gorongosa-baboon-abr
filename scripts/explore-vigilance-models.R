# Explore different vigilance response variables

#load packages
library(glmmTMB)
library(dplyr)
library(MuMIn)
library(performance)
library(stringr)
library(tidyr)

# Vigilance data --------------------------------------------------------

# Import data
Baboon_vigilance_df <- readRDS("data_derived/Baboon_vigilance_df.rds") %>% 
  filter(age_sex_class != "Unknown")


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
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n(),
         proportion_scanning_beta = (proportion_scanning * (n() - 1) + 0.5) / n(),
         proportion_look_at_abr_beta = (proportion_look_at_abr * (n() - 1) + 0.5) / n(),
         proportion_look_at_abr_cons_beta = (proportion_look_at_abr_cons * (n() - 1) + 0.5) / n(),
         proportion_look_not_abr_beta = (proportion_look_not_abr * (n() - 1) + 0.5) / n()
         )

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



# Prevalence of types of vigilance ----------------------------------------

# Remove any videos where the baboon was present for <2 seconds
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) %>% 
  mutate(proportion_walking_nv = proportion_look_not_abr - proportion_scanning)

hist(Baboon_vigilance_df_nocontrol_2sec$proportion_vigilant)
hist(Baboon_vigilance_df_nocontrol_2sec$proportion_look_at_abr)
hist(Baboon_vigilance_df_nocontrol_2sec$proportion_scanning)
hist(Baboon_vigilance_df_nocontrol_2sec$proportion_look_not_abr)
hist(Baboon_vigilance_df_nocontrol_2sec$proportion_look_at_abr_cons)

mean(Baboon_vigilance_df_nocontrol_2sec$proportion_vigilant)
mean(Baboon_vigilance_df_nocontrol_2sec$proportion_look_at_abr)
mean(Baboon_vigilance_df_nocontrol_2sec$proportion_scanning)
mean(Baboon_vigilance_df_nocontrol_2sec$proportion_look_not_abr)

# Calculate some totals
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(total_vigilant_frames = proportion_vigilant * nonoccluded_frames,
         total_look_at_abr_frames = proportion_look_at_abr * nonoccluded_frames,
         total_scanning_frames = proportion_scanning * nonoccluded_frames,
         total_look_not_abr_frames = proportion_look_not_abr * nonoccluded_frames)

Baboon_vigilance_df_nocontrol_2sec %>% 
  summarise(total_vigilant_frames = sum(total_vigilant_frames),
            total_look_at_abr_frames = sum(total_look_at_abr_frames),
            total_scanning_frames = sum(total_scanning_frames),
            total_look_not_abr_frames = sum(total_look_not_abr_frames)) %>% 
  pivot_longer(cols = everything(), names_to = "vigilance_type", values_to = "total_frames") %>% 
  mutate(proportion = total_frames / total_frames[vigilance_type == "total_vigilant_frames"])

# Correlations

library(ggcorrplot)

df_corr <- Baboon_vigilance_df_nocontrol_2sec %>%
  select(
    proportion_vigilant,
    proportion_look_at_abr,
    proportion_scanning,
    proportion_look_not_abr,
    proportion_look_at_abr_cons,
    proportion_walking_nv
  ) %>%
  na.omit()

cor_mat <- cor(df_corr)

ggcorrplot(
  cor_mat,
  lab = TRUE,                # show values
  type = "lower",
  colors = c("#B2182B", "white", "#2166AC"),
  lab_size = 4
) 


  # All vigilance, 1 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_1sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 30) 

# Scale covariates
Baboon_vigilance_df_nocontrol_1sec <- Baboon_vigilance_df_nocontrol_1sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
               group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_vigilant_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_1sec,
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
results_Vigilance_1s_all <- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_1s_all",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)


# All vigilance, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 

# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
         group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_vigilant_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_all <- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_all",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)


# Scanning, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 


# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
               group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_scanning_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_scanning <- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_scanning",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

# Look at ABR, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 


# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
               group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_look_at_abr_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_lookABR<- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_lookABR",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

# Look NOT at ABR, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 

# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
               group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_look_not_abr_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_notlookABR<- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_notlookABR",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

# Look at ABR, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 


# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
         group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_look_at_abr_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_lookABR<- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_lookABR",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)


# Look at ABR conservative, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 


# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
               group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_look_at_abr_cons_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_lookABRcons<- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_lookABR_cons",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

# Look NOT at ABR, 2 second -------------------------------------------------

# Remove any videos where the baboon was present for <1 second
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol %>% 
  filter(nonoccluded_frames > 60) 

# Scale covariates
Baboon_vigilance_df_nocontrol_2sec <- Baboon_vigilance_df_nocontrol_2sec %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
               group_number = scale(group_number, center = TRUE, scale = TRUE))

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_look_not_abr_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance_df_nocontrol_2sec,
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
results_Vigilance_2s_notlookABR<- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_2s_notlookABR",
    
    Covariate = case_when(
      grepl("^day_number", term) ~ "Day of study",
      grepl("^year", term) ~ "Year",
      grepl("^Habitat", term) ~ "Habitat",
      grepl("^group_number", term) ~ "Number of neighbors",
      grepl("^age_sex_class", term) ~ "Age-sex class",
      grepl("^predator_cue", term) ~ "Cue"
    ),
    
    Level = case_when(
      term == "day_number" ~ "Day of study",
      term == "group_number" ~ "Number of neighbors",
      term == "year2024" ~ "Year = 2024",
      term == "HabitatClosed" ~ "Habitat = Closed",
      term == "age_sex_classFemale_Adult_with_offspring" ~ "Class = Female adult w offspring",
      term == "age_sex_classJuvenile" ~"Class = Juvenile",
      term == "age_sex_classMale_Adult" ~ "Class = Male adult",
      term == "predator_cueHyena" ~ "Species = Hyena",
      term == "predator_cueLeopard" ~ "Species = Leopard",
      term == "predator_cueLion" ~ "Species = Lion",
      term == "predator_cueWild dog" ~"Species = Wild dog"
    ),
    Mean = Estimate,
    LCI = `2.5 %`,
    UCI = `97.5 %`,
  ) %>%
  select(Response, Covariate, Level, Mean, LCI, UCI)

results_all <- bind_rows(results_Vigilance_1s_all,
                         results_Vigilance_2s_all,
                         results_Vigilance_2s_scanning,
                         results_Vigilance_2s_lookABR,
                         results_Vigilance_2s_notlookABR,
                         results_Vigilance_2s_lookABRcons)

write.csv(results_all, "data_derived/model_global_results_test_vigilance.csv", row.names = FALSE)

