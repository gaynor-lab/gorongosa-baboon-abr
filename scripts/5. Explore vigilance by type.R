# Summarize different types of vigilance

# Load packages
library(tidyverse)
library(glmmTMB)
library(MuMIn)
library(performance)
library(ggcorrplot)

# Data cleaning -----------------------------------------------------------

# Import data
Final_2024 <- readRDS("data_derived/Final_2024.rds")
Final_2021 <- readRDS("data_derived/Final_2021.rds")

# Combine years
Final_2021_2024 <- bind_rows(Final_2021, Final_2024)

# Remove poor sound quality & controls
Final_2021_2024 <- Final_2021_2024 %>%
  filter(!(sound_quality %in% c("Poor", "None")),
         !(predator_cue == "No_sound"),
         !(predator_cue == "Control"))

# Order frames in chronological order
Final_2021_2024 <- Final_2021_2024 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

# Remove frames before the audio cue is played
Final_2021_2024 <- Final_2021_2024 %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()


# Calculate/format other covariates
Final_2021_2024 <- Final_2021_2024 %>%
  mutate(Habitat = case_when(
    site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    sex == "J" & age == "J" ~ "Juvenile",
    sex == "F" & age == "A" & offspring == 1 ~ "Female_Adult_with_offspring",
    sex == "F" & age == "A" & offspring == 0 ~ "Female_Adult_no_offspring",
    sex == "M" & age == "A" ~ "Male_Adult",
    sex == "Unknown" ~ "Unknown",
    age == "Unknown" ~ "Unknown",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>% 
  mutate(neighbours = group_number - 1)


# Calculate days of study (after correcting reset ABRs as needed)

# Make month/day columns
Final_2021_2024 <- Final_2021_2024 %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day   = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)))

# Bring in date/time offsets for correction
offsets <- read.csv("data/files_requiring_offset.csv")

# Join offsets
Final_2021_2024 <- left_join(Final_2021_2024, offsets, by = "file_name")

# Convert offset strings to numeric, and apply correction
Final_2021_2024 <- Final_2021_2024 %>%
  mutate(
    month_offset_num = as.numeric(month_offset),
    day_offset_num   = as.numeric(day_offset),
    raw_date = make_date(year = year, month = month, day = day),
    date_corrected = case_when(
      is.na(month_offset_num) & is.na(day_offset_num) ~ raw_date,
      TRUE ~ raw_date %m+% months(coalesce(month_offset_num, 0)) + days(coalesce(day_offset_num, 0))
    )
  )

# Days since first deployment, calculated separately per year
Final_2021_2024 <- Final_2021_2024 %>%
  group_by(year) %>%
  mutate(day_number = as.numeric(date_corrected - min(date_corrected, na.rm = TRUE)) + 1) %>%
  ungroup()

# Create new column where Walking_V, Staring, standing and staring, Scanning, Startling = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_vigilance <- Final_2021_2024 %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare","Startling") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))


#exclude videos where baboon fled immediately as they display not proportion of vigilance
Baboon_vigilance <- Baboon_vigilance %>%
  group_by(file_name) %>%
  filter(first(Behaviour) != "Flight") %>%  # Remove groups where the first row's Behaviour is "Flight"
  ungroup()

# Remove unknown sex
Baboon_vigilance <- Baboon_vigilance %>% 
  filter(age_sex_class != "Unknown")


# Summary stats -----------------------------------------------------------

# determine length of each video (non-occluded frames), remove videos with <60 non-occluded frames (2 seconds)
Baboon_video_length <- Baboon_vigilance %>%
  filter(Behaviour != "Occluded") %>% 
  count(file_name) %>% 
  filter(n > 60) %>% 
  rename(total_frames = n)

# count behaviours per file
Baboon_vigilance_frames <- Baboon_vigilance %>% 
  filter(Behaviour %in% c("Staring", "Walking_V", "Startling", "Stand_stare", "Scanning")) %>% 
  count(file_name, Behaviour) %>% 
  pivot_wider(names_from = Behaviour, values_from = n, values_fill = 0) 

# join
Baboon_vigilance_frames_proportions <- left_join(Baboon_video_length, Baboon_vigilance_frames) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(across(
    -c(file_name, total_frames),
    ~ .x / total_frames
  ))

# Mean time spent in each behaviour
mean(Baboon_vigilance_frames_proportions$Walking_V) # 20.0%
mean(Baboon_vigilance_frames_proportions$Staring) # 12.7%
mean(Baboon_vigilance_frames_proportions$Startling) # 5.3%
mean(Baboon_vigilance_frames_proportions$Stand_stare) # 0.3%
mean(Baboon_vigilance_frames_proportions$Scanning) # 7.0%

# Count number of videos in which each behaviour appears
Baboon_vigilance_frames_proportions %>%
  summarise(across(-c(file_name, total_frames), ~ sum(.x > 0, na.rm = TRUE)))

# Count percent of videos in which each behaviour appears
Baboon_vigilance_frames_proportions %>%
  summarise(across(-c(file_name, total_frames), ~ mean(.x > 0, na.rm = TRUE) * 100))

# Count percent of videos in which each behaviour appears - out of only those with vigilance at all
Baboon_vigilance_frames_proportions %>%
  filter(file_name %in% Baboon_vigilance_frames$file_name) %>% 
  summarise(across(-c(file_name, total_frames), ~ mean(.x > 0, na.rm = TRUE) * 100))



# Prep vigilance models ---------------------------------------------------

# Calculate proportion time spent vigilant 
Baboon_vigilance <- Baboon_vigilance %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    looking_abr_frames = sum(Behaviour %in% c("Staring", "Startling", "Walking_V")), # Count looking at ABR frames
    scanning_frames = sum(Behaviour == "Scanning"), # Count scanning frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    nonoccluded_frames = total_frames - occluded_frames,
    
    # Calculate vigilance proportion
    proportion_vigilant = if_else(total_frames == occluded_frames, NA, vigilant_frames / nonoccluded_frames),  # Compute proportion or set NA if occluded frames = total frames
    proportion_look_at_abr = if_else(total_frames == occluded_frames, NA, looking_abr_frames / nonoccluded_frames),  # Compute proportion or set NA if occluded frames = total frames
    proportion_scanning = if_else(total_frames == occluded_frames, NA, scanning_frames / nonoccluded_frames),  # Compute proportion or set NA if occluded frames = total frames
    
    # Transform data for beta distribution using Smithson & Verkuilen transformation
    proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n(),
    proportion_look_at_abr_beta = (proportion_look_at_abr * (n() - 1) + 0.5) / n(),
    proportion_scanning_beta = (proportion_scanning * (n() - 1) + 0.5) / n()
  ) %>%
  ungroup() 

# Remove any videos where the baboon was present for <2 seconds
Baboon_vigilance <- Baboon_vigilance %>% 
  filter(nonoccluded_frames > 60) 


# Dataframe for proportion vigilance model
Baboon_vigilance <- Baboon_vigilance %>%
  select(file_name, Habitat, age_sex_class, site, predator_cue,
         group_number, offspring, year, day_number,
         total_frames, nonoccluded_frames, occluded_frames,
         proportion_vigilant, proportion_vigilant_beta,
         proportion_look_at_abr, proportion_look_at_abr_beta,
         proportion_scanning, proportion_scanning_beta) %>%
  unique() %>% 
  drop_na(proportion_vigilant)


#Ensure reference levels are consisent across all models

# Define reference levels
Baboon_vigilance <- Baboon_vigilance %>%
  mutate(year = factor(year, levels = c(2021, 2024)),
         predator_cue = relevel(factor(predator_cue), ref = "Cheetah"),
         age_sex_class = relevel(factor(age_sex_class), ref = "Female_Adult_no_offspring"),
         Habitat = relevel(factor(Habitat), ref = "Open"))

# Look at correlations
df_corr <- Baboon_vigilance %>%
  select(
    proportion_vigilant,
    proportion_look_at_abr,
    proportion_scanning
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


# Scale covariates
Baboon_vigilance <- Baboon_vigilance %>% 
  mutate(day_number = scale(day_number, center = TRUE, scale = TRUE),
         group_number = scale(group_number, center = TRUE, scale = TRUE))

# All vigilance, 2 second -------------------------------------------------

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_vigilant_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance,
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
results_Vigilance_all <- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_all",
    
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

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_scanning_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance,
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
results_Vigilance_scanning <- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_scanning",
    
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

#Global GLMM using beta distribution
Vigilance_global_model <- glmmTMB(proportion_look_at_abr_beta ~ predator_cue + year + Habitat + age_sex_class + group_number + day_number + (1|site),
                                  data = Baboon_vigilance,
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
results_Vigilance_lookABR<- coefs_vigilance %>%
  select(term, Estimate) %>%
  left_join(
    cis_vigilance %>% select(term, `2.5 %`, `97.5 %`),
    by = "term"
  ) %>%
  mutate(
    Response = "Vigilance_lookABR",
    
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


results_all <- bind_rows(results_Vigilance_all,
                         results_Vigilance_scanning,
                         results_Vigilance_lookABR)

results_all %>%
  drop_na() %>% 
  ggplot(aes(x = Level, y = Mean, col = Response)) + 
  geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, 
                position = position_dodge(width = 0.6), alpha=.75) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(labels = c("Class = Female adult w offspring" = "Class = Female adult \nwith offspring")) +
  coord_flip() + 
  scale_color_manual(values = c(
    "Vigilance_all" = "#D55E00",
    "Vigilance_scanning" = "#0072B2",
    "Vigilance_lookABR" = "#009E73"),
    labels = c(
      "All",
      "Scanning",
      "Looking at ABR"
    )
    
  ) +
  labs(y = "Beta Coefficient", col = "Vigilance definition") 

ggsave("figures/vigilance-sensitivity.png", width = 6, height = 6, dpi = 300)
ggsave("figures/publication/FigureS4.png", width = 6, height = 6, dpi = 300)

