install.packages("survival")
install.packages("survminer")
library(survival)
library(survminer)
library(dplyr)
library(stringr)

#Create dataframe for flight including non-flight "censored" data - 2024
#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_KM <- Final_2024 %>%
  filter(
    !(sound_quality %in% c("Poor", "None"))) # Exclude Poor and None Sound_quality

#order frames in chronological order
Baboon_flight_KM <- Baboon_flight_KM %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#remove the number of frames before the audio cue is played
Baboon_flight_KM <- Baboon_flight_KM %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_KM <- Baboon_flight_KM %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()


# Calculate latency to flee per file_name
Baboon_flight_KM <- Baboon_flight_KM %>%
  group_by(file_name) %>%
  arrange(frame) %>%
  mutate(
    total_rows = n(),
    first_frame = min(frame),
    first_flight_frame = if (any(Behaviour == "Flight")) min(frame[Behaviour == "Flight"]) else NA_integer_,
    rows_until_flight = if_else(
      flight_present == 1 & !is.na(first_flight_frame),
      first_flight_frame - first_frame,
      total_rows
    ),
    latency_to_flee_s = rows_until_flight / 30
  ) %>%
  # NOW collapse to 1 row per video
  summarise(
    latency_to_flee_s = first(latency_to_flee_s),
    flight_present = first(flight_present),
    year = first(year),
    predator_cue = first(predator_cue),
    sex = first(sex),
    age = first(age),
    group_number = first(group_number),
    offspring = first(offspring),
    .groups = "drop"
  )

#Create dataframe for flight including non-flight "censored" data - 2021
#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_KM_21 <- Final_2021 %>%
  filter(
    !(Sound_quality %in% c("Poor", "None"))) # Exclude Poor and None Sound_quality

View(Final_2021)
#order frames in chronological order
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#change sound_delay_s column into numeric
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  mutate(Sound_delay_s = if_else(Sound_delay_s == "None", "0", Sound_delay_s),
         Sound_delay_s = as.numeric(Sound_delay_s))

View(Baboon_flight_KM_21)
#remove the number of frames before the audio cue is played
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  # Step 1: Replace NAs and "None" with 0
  mutate(Sound_delay_s = ifelse(Sound_delay_s == "None" | is.na(Sound_delay_s), 0, Sound_delay_s),
         Sound_delay_s = as.numeric(Sound_delay_s)) %>%
  
  # Step 2: Remove pre-sound frames
  group_by(file_name) %>%
  slice((Sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()


# Calculate latency to flee per file_name
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  group_by(file_name) %>%
  arrange(frame) %>%
  mutate(
    total_rows = n(),
    first_frame = min(frame),
    first_flight_frame = if (any(Behaviour == "Flight")) min(frame[Behaviour == "Flight"]) else NA_integer_,
    rows_until_flight = if_else(
      flight_present == 1 & !is.na(first_flight_frame),
      first_flight_frame - first_frame,
      total_rows
    ),
    latency_to_flee_s = rows_until_flight / 30
  ) %>%
  # NOW collapse to 1 row per video
  summarise(
    latency_to_flee_s = first(latency_to_flee_s),
    flight_present = first(flight_present),
    Year = first(Year),
    Predator.cue = first(Predator.cue),
    Focal.individual.sex = first(Focal.individual.sex),
    Focal.individual.age = first(Focal.individual.age),
    Number.of.individuals = first(Number.of.individuals),
    Presence.of.offspring = first(Presence.of.offspring),
    .groups = "drop"
  )

View(Baboon_flight_KM_21)

#change offspring row to be numeric
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  mutate(
    offspring = if_else(Presence.of.offspring == "None", 0L, 1L)
  ) %>%
  select(-Presence.of.offspring)  # remove the original column

#change column names to match
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  rename(
    year = Year,
    predator_cue = Predator.cue,
    sex = Focal.individual.sex,
    age = Focal.individual.age,
    group_number = Number.of.individuals
  )

Baboon_flight_KM_all <- bind_rows(Baboon_flight_KM, Baboon_flight_KM_21)

View(Baboon_flight_KM_all)

#change Wild dog name to match in both datasets
Baboon_flight_KM_all <- Baboon_flight_KM_all %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix spacing issue in predator names
Baboon_flight_KM_all <- Baboon_flight_KM_all %>%
  mutate(predator_cue = str_trim(predator_cue))

# Load required libraries
library(survival)
library(survminer)

#remove outliers
Baboon_flight_KM_all <- Baboon_flight_KM_all %>%
  filter(!file_name %in% c("2021_E02_08010107_Baboon.AVI", "2021_E02_08030006_Baboon.AVI"))

# Create the survival object
surv_obj <- Surv(time = Baboon_flight_KM_all$latency_to_flee_s, 
                 event = Baboon_flight_KM_all$flight_present)

# Fit the Kaplan-Meier survival curve for all data combined
fit <- survfit(surv_obj ~ 1)

# Plot the survival curve
ggsurvplot(fit, 
           data = Baboon_flight_KM_all,   # add your dataframe here
           risk.table = TRUE,
           conf.int = TRUE,
           xlab = "Latency to flee (seconds)",
           ylab = "Probability of not fleeing",
           title = "Kaplan-Meier Survival Curve for Baboon Flight",
           censor.shape = "|",
           ggtheme = theme_minimal())

summary(Baboon_flight_KM_all$latency_to_flee_s)

#BY PREDATOR CUE
fit_predator <- survfit(surv_obj ~ predator_cue, data = Baboon_flight_KM_all)

ggsurvplot(fit_predator,
           data = Baboon_flight_KM_all,
           pval = TRUE,  # Log-rank test
           conf.int = FALSE,
           xlab = "Latency to flee (seconds)",
           ylab = "Probability of not fleeing",
           title = "Survival Curves by Predator Cue",
           legend.title = "Predator",
           risk.table = TRUE,
           ggtheme = theme_minimal())

survdiff(surv_obj ~ predator_cue, data = Baboon_flight_KM_all)

#by year
fit_year <- survfit(surv_obj ~ year, data = Baboon_flight_KM_all)

ggsurvplot(fit_year,
           data = Baboon_flight_KM_all,
           pval = TRUE,
           title = "Survival Curves by Year",
           legend.title = "Year",
           risk.table = TRUE)

survdiff(surv_obj ~ year, data = Baboon_flight_KM_all)


#by sex
fit_sex <- survfit(surv_obj ~ sex, data = Baboon_flight_KM_all)

ggsurvplot(fit_sex,
           data = Baboon_flight_KM_all,
           pval = TRUE,
           title = "Survival Curves by Sex",
           legend.title = "Sex",
           risk.table = TRUE)

survdiff(surv_obj ~ sex, data = Baboon_flight_KM_all)


#by age
fit_age <- survfit(surv_obj ~ age, data = Baboon_flight_KM_all)

ggsurvplot(fit_age,
           data = Baboon_flight_KM_all,
           pval = TRUE,
           title = "Survival Curves by Age Class",
           legend.title = "Age Class",
           risk.table = TRUE)

survdiff(surv_obj ~ age, data = Baboon_flight_KM_all)
