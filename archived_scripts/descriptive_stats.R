#vigilance descriptive stats

Baboon_vigilance_data_both <- bind_rows(Baboon_vigilance_data, Baboon_vigilance_data_24)

Baboon_vigilance_data_descriptive <- Baboon_vigilance_data_both %>%
  filter(predator_cue != "Control")

# Identify videos with any vigilance
vigilance_presence <- Baboon_vigilance_data_descriptive %>%
  group_by(file_name) %>%
  summarise(
    vigilance_present = any(behaviour_class == "Vigilant", na.rm = TRUE)
  )

# Calculate percentage of videos that show vigilance
vigilance_percentage <- vigilance_presence %>%
  summarise(
    total_videos = n(),
    videos_with_vigilance = sum(vigilance_present),
    percent_with_vigilance = (videos_with_vigilance / total_videos) * 100
  )

vigilance_percentage

videos_with_vigilance <- vigilance_presence %>%
  filter(vigilance_present) %>%
  pull(file_name)

Baboon_vigilance_data_vigilant <- Baboon_vigilance_data_both %>%
  filter(file_name %in% videos_with_vigilance)

vigilant_behaviour_summary <- Baboon_vigilance_data_vigilant %>%
  group_by(file_name) %>%
  summarise(
    shows_scanning = any(Behaviour == "Scanning", na.rm = TRUE),
    shows_staring = any(Behaviour %in% c("Staring", "Stand_stare"), na.rm = TRUE),
    shows_walking_v = any(Behaviour == "Walking_V", na.rm = TRUE)
  ) %>%
  summarise(
    total_videos = n(),
    percent_with_scanning = (sum(shows_scanning) / total_videos) * 100,
    percent_with_staring = (sum(shows_staring) / total_videos) * 100,
    percent_with_walking_v = (sum(shows_walking_v) / total_videos) * 100
  )

View(vigilant_behaviour_summary)


#Flight descriptive stats
Baboon_frequency_stats_descriptive <- Baboon_frequency_stats_both %>%
  filter(predator_cue != "Control")

flight_percentage_overall <- Baboon_frequency_stats_descriptive %>%
  summarise(
    total_videos = n(),
    videos_with_flight = sum(flight_present, na.rm = TRUE),
    percent_with_flight = (videos_with_flight / total_videos) * 100
  )

#latency to flee
View(Baboon_flight_stats_both)
latency_summary <- Baboon_flight_stats_both %>%
  summarise(
    mean_latency = mean(latency_to_flee, na.rm = TRUE),
    sd_latency = sd(latency_to_flee, na.rm = TRUE),
    n_fled = sum(!is.na(latency_to_flee))
  )
latency_summary

# % of trials where baboons fled
percent_fled <- mean(Baboon_flight_KM_all$flight_present == 1) * 100
percent_fled

# Summary stats for latency among videos with flight
latency_summary <- Baboon_flight_KM_all %>%
  filter(flight_present == 1) %>%
  summarise(
    mean_latency = mean(latency_to_flee_s, na.rm = TRUE),
    median_latency = median(latency_to_flee_s, na.rm = TRUE),
    sd_latency = sd(latency_to_flee_s, na.rm = TRUE),
    min_latency = min(latency_to_flee_s, na.rm = TRUE),
    max_latency = max(latency_to_flee_s, na.rm = TRUE)
  )

latency_summary

# Choose your threshold (e.g. 3 seconds)
threshold <- 2 

# % that fled within threshold
percent_within_threshold <- Baboon_flight_KM_all %>%
  filter(flight_present == 1) %>%
  summarise(percent = mean(latency_to_flee_s <= threshold) * 100)

percent_within_threshold
