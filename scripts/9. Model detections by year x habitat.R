# Explore differences in ABR detections by year and habitat

library(dplyr)
library(ggplot2)
library(MASS)
library(glmmTMB)


# Bring in second watch data
B_21_second <- read.csv("data/second watch 2026 correction/2021_baboon_second.csv")
B_24_second <- read.csv("data/second watch 2026 correction/2024_baboon_second.csv")
all_videos <- bind_rows(B_21_second, B_24_second) %>% 
  dplyr::select(video_name, site, year) %>% 
  unique() %>% 
  mutate(year = as.factor(year))

# Count videos by year
counts_by_year <- all_videos %>% 
  count(site, year) 

# Account for the videos that had been removed
# F07 2021 = 485
# F07 2024 = 120
# L11 2024 = 109
counts_corrected <- counts_by_year %>%
  mutate(
    n = case_when(
      site == "F07" & year == "2021" ~ 485,
      site == "F07" & year == "2024" ~ 120,
      site == "L11" & year == "2024" ~ 109,
      TRUE ~ n
    )
  )


# Account for differences in sampling effort across years
operation_days <- read.csv("data/ABR_OperationalDates.csv") %>% 
  dplyr::select(site, year, days) %>% 
  mutate(year = as.factor(year))
counts_corrected <- operation_days %>%
  left_join(counts_corrected) %>%
  mutate(daily_n = n / days) %>% 
  mutate(daily_n = ifelse(is.na(daily_n), 0, daily_n))
  

# Add in habitat
ABR_habitat <- read.csv("data/ABR_locations.csv") %>% 
  dplyr::select(Site, Habitat) %>% 
  rename(site = Site)
counts_corrected <- counts_corrected %>% 
  left_join(ABR_habitat, by = "site")

# Remove two sites not in 2024
counts_corrected_filtered <- counts_corrected %>% 
  filter(!(site %in% c("D09", "N03")))

# Habitat by year
set.seed(12345)
(habitat_year_plot <- ggplot(counts_corrected, aes(x = year, y = daily_n, fill = Habitat)) +
  geom_point(
    aes(color = Habitat),
    position = position_jitterdodge(jitter.width = 0.4,
                                    dodge.width = 0.75),
    size = 2.5,
    alpha = 0.8
  ) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA,
    color = "gray30",
    position = position_dodge(width = 0.75)
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#A23B2A", "#0A0A52")) +
  scale_color_manual(values = c("#A23B2A", "#0A0A52")) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Baboon detections per day",
    fill = "Habitat",
    color = "Habitat"
  ) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.8, 0.7)
  )
)

ggsave("figures/detections-by-habitat-year.png",
       width = 5, height = 3, dpi = 300)

# Run model
habitat_by_year <- glmmTMB(
  n ~ year * Habitat + offset(log(days)) + (1 | site),
  family = nbinom2,
  data = counts_corrected
)

summary(habitat_by_year)

# summary stats by habitat-year
counts_corrected %>%
  group_by(year, Habitat) %>%
  summarise(
    mean_daily_n = mean(daily_n),
    sd_daily_n = sd(daily_n),
    n_sites = n()
  )

# summary stats by year
counts_corrected %>%
  group_by(year) %>%
  summarise(
    mean_daily_n = mean(daily_n),
    sd_daily_n = sd(daily_n),
    n_sites = n()
  )

# Calculate summary stats
counts_corrected %>% 
  group_by(year, Habitat) %>%
  summarise(
    mean_daily_n = mean(daily_n),
    sd_daily_n = sd(daily_n),
    n_sites = n()
  )

counts_corrected %>% 
  group_by(year) %>%
  summarise(
    mean_daily_n = mean(daily_n),
    sd_daily_n = sd(daily_n),
    n_sites = n()
  )
