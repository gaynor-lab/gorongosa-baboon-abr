#Creation of dataframes for each response variable (proportion vigilance, latency to flee, flight)

# Load packages
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

# Import data
Final_2024 <- readRDS("data_derived/Final_2024.rds")
Final_2021 <- readRDS("data_derived/Final_2021.rds")

# Bring in bounding box summary
bbox <- read.csv("data_derived/bounding_box_summary.csv")

# Combine years
Final_2021_2024 <- bind_rows(Final_2021, Final_2024)

# Remove poor sound quality
Final_2021_2024 <- Final_2021_2024 %>%
  filter(!(sound_quality %in% c("Poor", "None")),
         !(predator_cue == "No_sound"))

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

# Calculate other covariates
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
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>% 
  mutate(neighbours = group_number - 1)


# DATAFRAME FOR VIGILANCE ANALYSIS ----------------------------------------

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

#calculate proportion time spent vigilant 
Baboon_vigilance <- Baboon_vigilance %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    proportion_vigilant = if_else(total_frames == occluded_frames, NA, vigilant_frames / (total_frames - occluded_frames))  # Compute proportion or set NA if occluded frames = total frames
  ) %>%
  ungroup()

#Dataframe for proportion vigilance model
Baboon_vigilance <- Baboon_vigilance %>%
  group_by(file_name, Habitat, age_sex_class, site, predator_cue, group_number, offspring, year) %>%
  summarise(
    proportion_vigilant = first(na.omit(proportion_vigilant)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(proportion_vigilant, predator_cue, Habitat, age_sex_class, group_number, offspring) #need to drop NAs from proportion vigilant where total_frames = occluded_frames

# Join bounding boxes
Baboon_vigilance <- Baboon_vigilance %>%
  left_join(bbox, by = "file_name")

#export dataframe 
saveRDS(Baboon_vigilance, "data_derived/Baboon_vigilance_df.rds")


# DATAFRAME FOR FLIGHT ANALYSIS ----------------------------------------

# Create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_binary <- Final_2021_2024 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup() %>% 
  select(file_name, Habitat, age_sex_class, site, predator_cue, group_number, offspring, flight_present, year) %>% 
  unique()

# Join bounding boxes
Baboon_flight_binary <- Baboon_flight_binary %>%
  left_join(bbox, by = "file_name")

#export dataset
saveRDS(Baboon_flight_binary, "data_derived/Baboon_flight_binary_df.rds")



# DATAFRAME FOR LATENCY TO FLEE ----------------------------------------

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_latency <- Final_2021_2024 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#filter for videos where flight is present (flight_present = 1) 
Baboon_flight_latency <- Baboon_flight_latency %>%
  filter(flight_present == 1)

#calculate latency to flee
Baboon_flight_latency <- Baboon_flight_latency %>%
  group_by(file_name) %>%
  arrange(frame) %>%  # Arrange by frame within each file_name
  mutate(
    first_row = first(row_number()),  # Get the first row number of each video
    rows_until_flight = if_else(
      flight_present == 1 & Behaviour == "Flight" & row_number() == min(which(Behaviour == "Flight")), 
      row_number() - first_row, 
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  group_by(file_name) %>%
  mutate(rows_until_flight = if_else(flight_present == 1, min(rows_until_flight, na.rm = TRUE), NA_integer_)) %>%
  ungroup()

#convert frames until seconds = latency by dividing by 30 bc 1s = 30 frames
Baboon_flight_latency <- Baboon_flight_latency %>%
  mutate(latency_to_flee_s = rows_until_flight / 30)

#Dataframe for latency to flee model - WHY ONLY 152 VIDEOS??
Baboon_flight_latency <- Baboon_flight_latency %>%
  group_by(file_name, Habitat, age_sex_class, site, predator_cue, group_number, offspring, year) %>%
  summarise(
    latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(latency_to_flee, predator_cue, Habitat, age_sex_class, group_number, offspring) %>% #need to drop one video where age_sex_class is NA for analysis
  mutate(log_latency_to_flee = log(latency_to_flee + 1)) 

# Join bounding boxes
Baboon_flight_latency <- Baboon_flight_latency %>%
  left_join(bbox, by = "file_name")

#export dataframe
saveRDS(Baboon_flight_latency, "data_derived/Baboon_flight_latency_df.rds")



# calculate mean latency to flee for predator-only cues
Baboon_flight_latency_predator <- Baboon_flight_latency %>%
  filter(predator_cue != "Control") 
mean(Baboon_flight_latency_predator$latency_to_flee, na.rm = TRUE)
sd(Baboon_flight_latency_predator$latency_to_flee, na.rm = TRUE)


