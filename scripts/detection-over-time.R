library(dplyr)
library(ggplot2)
library(MASS)

# Bring in second watch data
B_21_second <- read.csv("data/second watch 2026 correction/2021_baboon_second.csv")
B_24_second <- read.csv("data/second watch 2026 correction/2024_baboon_second.csv")
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

# Account for differences in sampling effort across years
operation_days <- read.csv("data/ABR_OperationalDates.csv") %>% 
  dplyr::select(site, year, days) %>% 
  mutate(year = as.factor(year))
counts_corrected <- counts_corrected %>%
  left_join(operation_days) %>%
  mutate(daily_n = n / days)

# Habitat by year
set.seed(12345)
(habitat_year_plot <- ggplot(counts_corrected, aes(x = year, y = daily_n, fill = Habitat)) +
  geom_point(
    aes(color = Habitat),
    position = position_jitterdodge(jitter.width = 0.4,
                                    dodge.width = 0.75),
    size = 2.5,
    alpha = 0.9,
    shape = 21
  ) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA,
    color = "gray30",
    position = position_dodge(width = 0.75)
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
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
habitat_by_year <- glm.nb(n ~ year * Habitat + offset(log(days)),
                          data = counts_corrected)
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
