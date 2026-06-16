# 2. Creation of dataframes for each response variable (proportion vigilance, flight, latency to flee)

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

# Create variable for predator vs control
Final_2021_2024 <- Final_2021_2024 %>% 
  mutate(cue_type = case_when(predator_cue == "Control" ~ "Control",
                              predator_cue %in% c("Leopard", "Lion", "Wild dog", "Cheetah", "Hyena") ~ "Predator"))

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

# Calculate sample size in each year
Final_2021_2024 %>% 
  select(file_name, year) %>% 
  unique() %>% 
  count(year)

# DATAFRAME FOR VIGILANCE ANALYSIS ----------------------------------------

# Create new column where Walking_V, Staring, standing and staring, Scanning, Startling = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_vigilance <- Final_2021_2024 %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare","Startling") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))

# Count number of videos that had vigilance
Vig_count <- Baboon_vigilance %>% 
  filter(cue_type == "Predator") %>% 
  count(file_name, behaviour_class) %>% 
  pivot_wider(names_from = behaviour_class, values_from = n, values_fill = 0) 
nrow(Vig_count %>% filter(Vigilant > 0)) / nrow(Vig_count) # 71%

# Exclude videos where baboon fled immediately as they display not proportion of vigilance
Baboon_vigilance <- Baboon_vigilance %>%
  group_by(file_name) %>%
  filter(first(Behaviour) != "Flight") %>%  # Remove groups where the first row's Behaviour is "Flight"
  ungroup()

# Calculate proportion time spent vigilant 
Baboon_vigilance <- Baboon_vigilance %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    nonoccluded_frames = total_frames - occluded_frames,
    
    # Calculate vigilance proportion
    proportion_vigilant = if_else(total_frames == occluded_frames, NA, vigilant_frames / nonoccluded_frames),  # Compute proportion or set NA if occluded frames = total frames

    # Transform data for beta distribution using Smithson & Verkuilen transformation
    proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n()
  ) %>%
  ungroup() 

# Dataframe for proportion vigilance model
Baboon_vigilance_by_video <- Baboon_vigilance %>%
  left_join(bbox, by = "file_name") %>%
  select(file_name, Habitat, age_sex_class, site, predator_cue, cue_type,
         group_number, offspring, year, day_number, initial_max_dimension,
         total_frames, nonoccluded_frames, occluded_frames,
         proportion_vigilant, proportion_vigilant_beta) %>%
  unique() %>% 
  drop_na(proportion_vigilant) #need to drop NAs from proportion vigilant where total_frames = occluded_frames%>%
  

# Join bounding boxes
Baboon_vigilance_by_video <- Baboon_vigilance_by_video 

# Remove any videos where the baboon was present for <2 seconds
Baboon_vigilance_by_video <- Baboon_vigilance_by_video %>% 
  filter(nonoccluded_frames > 60) 

#export dataframe 
saveRDS(Baboon_vigilance_by_video, "data_derived/Baboon_vigilance_df.rds")


# DATAFRAME FOR FLIGHT ANALYSIS ----------------------------------------

# Create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_binary <- Final_2021_2024 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup() %>%
  left_join(bbox, by = "file_name") %>% 
  select(file_name, flight_present, Habitat, age_sex_class, site, predator_cue, cue_type,
         group_number, offspring, year, day_number, initial_max_dimension) %>% 
  unique() 

# Export dataset
saveRDS(Baboon_flight_binary, "data_derived/Baboon_flight_binary_df.rds")



# DATAFRAME FOR LATENCY TO FLEE ----------------------------------------

Baboon_flight_latency <- Final_2021_2024 %>%
  arrange(file_name, frame) %>%
  group_by(file_name, Habitat, age_sex_class, site, predator_cue, 
           group_number, offspring, year) %>%
  mutate(frame_id = row_number()) %>%  # ✅ resets frame count per video
  summarise(
    flight_present = any(Behaviour == "Flight"),
    
    latency_frames = if (any(Behaviour == "Flight")) {
      min(frame_id[Behaviour == "Flight"]) - 1
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) %>%
  filter(flight_present) %>%
  mutate(
    latency_to_flee = latency_frames / 30,
    log_latency_to_flee = log(latency_to_flee + 1)
  )

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

# calculate mean latency to flee for control cues
Baboon_flight_latency_control <- Baboon_flight_latency %>%
  filter(predator_cue == "Control") 
mean(Baboon_flight_latency_control$latency_to_flee, na.rm = TRUE)
sd(Baboon_flight_latency_control$latency_to_flee, na.rm = TRUE)

# Calculate percentages that fled w/in 1, 2 seconds
Baboon_flight_latency_predator %>%
  summarise(
    n_total = sum(!is.na(latency_to_flee)),
    n_under_1s = sum(latency_to_flee < 1, na.rm = TRUE),
    percent_under_1s = (n_under_1s / n_total) * 100,
    n_under_2s = sum(latency_to_flee < 2, na.rm = TRUE),
    percent_under_2s = (n_under_2s / n_total) * 100
  )

