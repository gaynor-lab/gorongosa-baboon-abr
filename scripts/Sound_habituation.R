#Linear regression to test for sound habituation

#2021 - vigilance 
View(Baboon_vigilance_stats)

#make new columun with month and day
habituation_vigilance_21 <- Baboon_vigilance_stats %>%
  select(file_name, proportion_vigilant) %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))
View(habituation_vigilance_21)

#linear model to test correlation between day number and proportion of time spent vigilant
lm_vigilance_21 <- lm(proportion_vigilant ~ day_number, data = habituation_vigilance_21)
summary(lm_vigilance_21)

#2021 - latency to flee 

#make new columun with month and day
habituation_latency_21 <- Baboon_flight_stats %>%
  select(file_name, latency_to_flee) %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))
View(habituation_latency_21)

#linear model to test correlation between day number and latency to flee 
lm_latency_21 <- lm(latency_to_flee ~ day_number, data = habituation_latency_21)
summary(lm_latency_21)

#2021 - frequency of flight 
habituation_flight_21 <- Baboon_frequency_stats %>%
  select(file_name, flight_present) %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))
View(habituation_flight_21)

#glm to test correlation between day number and flight presence (binary)
glm_freq_21 <- glm(flight_present ~ day_number, data = habituation_flight_21, family = binomial)
summary(glm_freq_21)

#2024 - vigilance 
View(Baboon_vigilance_stats_24)

#make new columun with month and day
habituation_vigilance_24 <- Baboon_vigilance_stats_24 %>%
  select(file_name, proportion_vigilant) %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))
View(habituation_vigilance_24)

#linear model to test correlation between day number and proportion of time spent vigilant
lm_vigilance_24 <- lm(proportion_vigilant ~ day_number, data = habituation_vigilance_24)
summary(lm_vigilance_24)

#2024 - latency to flee 

#make new columun with month and day
habituation_latency_24 <- Baboon_flight_stats_24 %>%
  select(file_name, latency_to_flee) %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))
View(habituation_latency_24)

#linear model to test correlation between day number and latency to flee 
lm_latency_24 <- lm(latency_to_flee ~ day_number, data = habituation_latency_24)
summary(lm_latency_24)

#2024 - frequency of flight 
habituation_flight_24 <- Baboon_frequency_stats_24 %>%
  select(file_name, flight_present) %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name))) %>%
  mutate(day_number = case_when(
    month == 6  ~ day,
    month == 7  ~ 30 + day,
    month == 8  ~ 61 + day,
    month == 1  ~ day,   # in case June was labeled as January due to camera reset
    TRUE ~ NA_real_
  ))
View(habituation_flight_24)

#glm to test correlation between day number and flight presence (binary)
glm_freq_24 <- glm(flight_present ~ day_number, data = habituation_flight_24, family = binomial)
summary(glm_freq_24)
