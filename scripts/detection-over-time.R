library(dplyr)
library(ggplot2)
library(MASS)

# Bring in second watch data
B_21_second <- read.csv("data/2021_baboon_second.csv")
B_24_second <- read.csv("data/2024_baboon_second.csv")
all_videos <- bind_rows(B_21_second, B_24_second) %>% 
  dplyr::select(video_name, site, year) %>% 
  unique() %>% 
  mutate(year = as.factor(year))

# Add in habitat
counts_by_year_habitat <- all_videos %>% 
  count(site, year) %>%
  mutate(Habitat = case_when(
    site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed"
  ))

# Detections by year
ggplot(counts_by_year_habitat, aes(x = year, y = n)) +
  geom_boxplot() +
  theme_minimal()

# Account for the videos that had been removed
# F07 2021 = 485
# F07 2024 = 120
# L11 2024 = 109
counts_corrected <- counts_by_year_habitat %>%
  mutate(
    n = case_when(
      site == "F07" & year == "2021" ~ 485,
      site == "F07" & year == "2024" ~ 120,
      site == "L11" & year == "2024" ~ 109,
      TRUE ~ n
    )
  )

# TO DO - account for differences in sampling effort across years

# Habitat by year
ggplot(counts_corrected, aes(x = year, y = n, fill = Habitat)) +
  geom_boxplot() +
  theme_minimal()

# Run model
habitat_by_year <- glm.nb(n ~ year * Habitat,
                          data = counts_corrected)
summary(habitat_by_year)
