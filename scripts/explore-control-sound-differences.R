library(dplyr)
library(ggplot2)

# ASKED ALVAR WHICH FILES ARE WHICH BIRDS

metadata_2024 <- read.csv("data/2024_baboon_second.csv") %>%
  mutate(file_name = paste(year, site, video_name, sep = "_")) %>% 
  select(file_name, predator_file) %>% 
  mutate(predator_file = as.factor(predator_file))

# Vigilance

Baboon_vigilance_df <- readRDS("data_derived/Baboon_vigilance_df.rds") %>% 
  filter(predator_cue == "Control", year == "2024") %>% 
  left_join(metadata_2024)

# Flight

Baboon_flight_df <- readRDS("data_derived/Baboon_flight_binary_df.rds") %>% 
  filter(predator_cue == "Control", year == "2024") %>% 
  left_join(metadata_2024) 

count(Baboon_flight_df, predator_file, flight_present)

Baboon_flight_df %>%
  count(predator_file, flight_present) %>%
  ggplot(aes(x = factor(predator_file),
             y = n,
             fill = factor(flight_present))) +
  geom_col(position = "fill") +
  labs(
    x = "Predator file",
    y = "Proportion of trials",
    fill = "Flight present"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()
