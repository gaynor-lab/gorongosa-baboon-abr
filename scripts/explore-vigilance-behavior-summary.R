# Summarize different types of vigilance

# Load packages
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

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
mean(Baboon_vigilance_frames_proportions$Walking_V) # 19.91%
mean(Baboon_vigilance_frames_proportions$Staring) # 12.65%
mean(Baboon_vigilance_frames_proportions$Startling) # 5.27%
mean(Baboon_vigilance_frames_proportions$Stand_stare) # 0.29%
mean(Baboon_vigilance_frames_proportions$Scanning) # 7.03%

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


