#Creation of dataframes for each response variable (proportion vigilance, latency to flee, flight frequency)

#load packages
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#Import 2024 data
Final_2024 <- readRDS("data_derived/Final_2024.rds")

#DATAFRAME FOR VIGILANCE ANALYSIS

#filter videos that have No_sound or sound.quality = poor as they will not be included in analysis
Baboon_vigilance_data_24 <- Final_2024 %>%
  filter(!(sound_quality %in% c("Poor", "None")))

#create new column where Walking_V, Staring, standing and staring, Scanning, Startling = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_vigilance_data_24 <- Baboon_vigilance_data_24 %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare","Startling") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))

#order frames in chronological order
Baboon_vigilance_data_24 <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#remove the number of frames before the audio cue is played
Baboon_vigilance_data_24 <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#exclude videos where baboon fled immediately as they display not proportion of vigilance
Baboon_vigilance_data_24 <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  filter(first(Behaviour) != "Flight") %>%  # Remove groups where the first row's Behaviour is "Flight"
  ungroup()

#calculate proportion time spent vigilant 
Baboon_vigilance_data_24 <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    proportion_vigilant = if_else(total_frames == occluded_frames, NA, vigilant_frames / (total_frames - occluded_frames))  # Compute proportion or set NA if occluded frames = total frames
  ) %>%
  ungroup()

#add column grouping predator cues by hunting mode
Baboon_vigilance_data_24 <- Baboon_vigilance_data_24 %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Cheetah") ~ "Ambush",
    predator_cue %in% c("Hyena", "WD", "Leopard") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for proportion vigilance model
Baboon_vigilance_stats_24 <- Baboon_vigilance_data_24 %>%
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
  group_by(file_name, Habitat, age_sex_class, site, predator_cue, group_number, offspring, year) %>%
  summarise(
    proportion_vigilant = first(na.omit(proportion_vigilant)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(proportion_vigilant, predator_cue, Habitat, age_sex_class, group_number, offspring) #need to drop NAs from proportion vigilant where total_frames = occluded_frames

saveRDS(Baboon_vigilance_stats_24, "data_derived/Baboon_vigilance_stats_24.rds")

#DATAFRAME FOR LATENCY TO FLEE

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_data_24 <- Final_2024 %>%
  filter(
    !(sound_quality %in% c("Poor", "None"))) # Exclude Poor and None Sound_quality

#order frames in chronological order
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#remove the number of frames before the audio cue is played
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#filter for videos where flight is present (flight_present = 1) 
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
  filter(flight_present == 1)

#calculate latency to flee
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
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
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
  mutate(latency_to_flee_s = rows_until_flight / 30)

#add column grouping predator cues by hunting mode
Baboon_flight_data_24 <- Baboon_flight_data_24 %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Cheetah") ~ "Ambush",
    predator_cue %in% c("Hyena", "WD", "Leopard") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for latency to flee model
Baboon_flight_stats_24 <- Baboon_flight_data_24 %>%
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
  group_by(file_name, Habitat, age_sex_class, site, predator_cue, group_number, offspring, year) %>%
  summarise(
    latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(latency_to_flee, predator_cue, Habitat, age_sex_class, group_number, offspring)%>% #need to drop one video where age_sex_class is NA for analysis
  mutate(log_latency_to_flee = log(latency_to_flee + 1)) 

saveRDS(Baboon_flight_stats_24, "data_derived/Baboon_flight_stats_24.rds")

#DATAFRAME FOR FLIGHT FREQUENCY

#filter videos that have No_sound or sound.quality = poor 
Baboon_frequency_data_24 <- Final_2024 %>%
  filter(
    !(sound_quality %in% c("Poor", "None")))  # Exclude Poor and None Sound_quality

#order frames in chronological order
Baboon_frequency_data_24 <- Baboon_frequency_data_24 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#remove the number of frames before the audio cue is played
Baboon_frequency_data_24 <- Baboon_frequency_data_24 %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_frequency_data_24 <- Baboon_frequency_data_24 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#add column grouping predator cues by hunting mode
Baboon_frequency_data_24 <- Baboon_frequency_data_24 %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Cheetah") ~ "Ambush",
    predator_cue %in% c("Hyena", "WD", "Leopard") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for flight frequency model
Baboon_frequency_stats_24 <- Baboon_frequency_data_24 %>%
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
  group_by(file_name, Habitat, age_sex_class, site, predator_cue, group_number, offspring, flight_present, year) %>%
  summarise(
    flight_present = first(na.omit(flight_present)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(predator_cue, Habitat, age_sex_class, group_number, offspring) #need to drop NAs from proportion vigilant where total_frames = occluded_frames

saveRDS(Baboon_frequency_stats_24, "data_derived/Baboon_frequency_stats_24.rds")

#2021 data
Final_2021 <- readRDS("data_derived/Final_2021.rds")

#DATAFRAME FOR VIGILANCE ANALYSIS

#filter videos that have No_sound or sound.quality = poor as they will not be included in analysis
Baboon_vigilance_data <- Final_2021 %>%
  filter(!(Sound_quality %in% c("Poor", "None")))

#create new column where Walking_V, Staring, standing and staring, Scanning, Startling = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare","Startling") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))

#exclude videos where baboon fled immediately as they display not proportion of vigilance
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  group_by(file_name) %>%
  filter(first(Behaviour) != "Flight") %>%  # Remove groups where the first row's Behaviour is "Flight"
  ungroup()

#calculate proportion time spent vigilant 
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    proportion_vigilant = ifelse(total_frames == occluded_frames, NA, vigilant_frames / (total_frames - occluded_frames))  # Compute proportion or set NA if occluded frames = total frames
  ) %>%
  ungroup()

#add column grouping predator cues by hunting mode
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  mutate(Hunting_mode = case_when(
    Predator.cue %in% c("Lion", "Cheetah") ~ "Ambush",
    Predator.cue %in% c("Hyena", "Wild_dog", "Leopard") ~ "Coursing",
    Predator.cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for proportion vigilance model
Baboon_vigilance_stats <- Baboon_vigilance_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" & Presence.of.offspring == 1 ~ "Female_Adult_with_offspring",
    Focal.individual.sex == "F" & Focal.individual.age == "A" & Presence.of.offspring == "None" ~ "Female_Adult_no_offspring",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  mutate(Presence_of_offspring = case_when(
    Presence.of.offspring == "None" ~ 0,   # Assign 0 if "None"
    TRUE ~ 1   # Assign 1 if anything else
  )) %>%
  group_by(file_name, Habitat, age_sex_class, Camera.trap.site, Predator.cue, Number.of.individuals, Presence_of_offspring, Year) %>%
  summarise(
    proportion_vigilant = first(na.omit(proportion_vigilant)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(proportion_vigilant, Predator.cue, Habitat, age_sex_class, Number.of.individuals, Presence_of_offspring) %>%
  # Rename columns to be same as 2024
  rename(
    site = Camera.trap.site,
    predator_cue = Predator.cue,
    group_number = Number.of.individuals,
    offspring = Presence_of_offspring,
    year = Year
  )

saveRDS(Baboon_vigilance_stats, "data_derived/Baboon_vigilance_stats.rds")

#DATAFRAME FOR LATENCY TO FLEE

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_data <- Final_2021 %>%
  filter(
    !(Sound_quality %in% c("Poor", "None")),  # Exclude Poor and None Sound_quality
    Sound_delay_s == "None"  # Keep only rows where Sound_delay_s is "None"
  )

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_data <- Baboon_flight_data %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#filter for videos where flight is present (flight_present = 1) 
Baboon_flight_data <- Baboon_flight_data %>%
  filter(flight_present == 1)

#calculate latency to flee
Baboon_flight_data <- Baboon_flight_data %>%
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
Baboon_flight_data <- Baboon_flight_data %>%
  mutate(latency_to_flee_s = rows_until_flight / 30)

#add column grouping predator cues by hunting mode
Baboon_flight_data <- Baboon_flight_data %>%
  mutate(Hunting_mode = case_when(
    Predator.cue %in% c("Lion", "Cheetah") ~ "Ambush",
    Predator.cue %in% c("Hyena", "Wild_dog", "Leopard") ~ "Coursing",
    Predator.cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for latency to flee model
Baboon_flight_stats <- Baboon_flight_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" & Presence.of.offspring == 1 ~ "Female_Adult_with_offspring",
    Focal.individual.sex == "F" & Focal.individual.age == "A" & Presence.of.offspring == "None" ~ "Female_Adult_no_offspring",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  mutate(Presence_of_offspring = case_when(
    Presence.of.offspring == "None" ~ 0,   # Assign 0 if "None"
    TRUE ~ 1   # Assign 1 if anything else
  )) %>%
  group_by(file_name, Habitat, age_sex_class, Camera.trap.site, Predator.cue, Number.of.individuals, Presence_of_offspring, Year) %>%
  summarise(
    latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(latency_to_flee, Predator.cue, Habitat, age_sex_class, Number.of.individuals, Presence_of_offspring)%>% #need to drop one video where age_sex_class is NA for analysis
  mutate(log_latency_to_flee = log(latency_to_flee + 1)) %>%
  # Rename columns to be same as 2024
  rename(
    site = Camera.trap.site,
    predator_cue = Predator.cue,
    group_number = Number.of.individuals,
    offspring = Presence_of_offspring,
    year = Year
  )

saveRDS(Baboon_flight_stats, "data_derived/Baboon_flight_stats.rds")

#DATAFRAME FOR FLIGHT FREQUENCY

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_frequency_data <- Final_2021 %>%
  filter(
    !(Sound_quality %in% c("Poor", "None")),  # Exclude Poor and None Sound_quality
    Sound_delay_s == "None"  # Keep only rows where Sound_delay_s is "None"
  )

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_frequency_data <- Baboon_frequency_data %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#add column grouping predator cues by hunting mode
Baboon_frequency_data <- Baboon_frequency_data %>%
  mutate(Hunting_mode = case_when(
    Predator.cue %in% c("Lion", "Cheetah") ~ "Ambush",
    Predator.cue %in% c("Hyena", "Wild_dog", "Leopard") ~ "Coursing",
    Predator.cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for flight frequency model
Baboon_frequency_stats <- Baboon_frequency_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" & Presence.of.offspring == 1 ~ "Female_Adult_with_offspring",
    Focal.individual.sex == "F" & Focal.individual.age == "A" & Presence.of.offspring == "None" ~ "Female_Adult_no_offspring",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  mutate(Presence_of_offspring = case_when(
    Presence.of.offspring == "None" ~ 0,   # Assign 0 if "None"
    TRUE ~ 1   # Assign 1 if anything else
  )) %>%
  group_by(file_name, Habitat, age_sex_class, Camera.trap.site, Predator.cue, Number.of.individuals, Presence_of_offspring, flight_present, Year) %>%
  summarise(
    flight_present = first(na.omit(flight_present)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(Predator.cue, Habitat, age_sex_class, Number.of.individuals, Presence_of_offspring) %>% #need to drop NAs from proportion vigilant where total_frames = occluded_frames
  # Rename columns to be same as 2024
  rename(
    site = Camera.trap.site,
    predator_cue = Predator.cue,
    group_number = Number.of.individuals,
    offspring = Presence_of_offspring,
    year = Year
  )

saveRDS(Baboon_frequency_stats, "data_derived/Baboon_frequency_stats.rds")

