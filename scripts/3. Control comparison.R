# Compare the control and predator cues as a first pass

library(dplyr)
library(stringr)
library(glmmTMB)
library(lubridate)
library(ggplot2)

# Import and prep data ----------------------------------------------------

#Import derived dataframes
Baboon_vigilance_df <- readRDS("data_derived/Baboon_vigilance_df.rds")
Baboon_flight_df <- readRDS("data_derived/Baboon_flight_binary_df.rds")

# PROPORTION OF TIME SPENT VIGILANT-----


# Summary stats

#pred
nrow(Baboon_vigilance_df %>% filter(proportion_vigilant == 0, cue_type == "Predator")) # 44 videos (9%) no vigilance
nrow(Baboon_vigilance_df %>% filter(proportion_vigilant > 0, cue_type == "Predator")) # 431 videos (91%) vigilance

#control
nrow(Baboon_vigilance_df %>% filter(proportion_vigilant == 0, cue_type == "Control")) # 20 videos (22%) no vigilance
nrow(Baboon_vigilance_df %>% filter(proportion_vigilant > 0, cue_type == "Control")) # 73 videos (78%) vigilance

# mean and SD
Baboon_vigilance_df %>%
  filter(cue_type %in% c("Predator", "Control")) %>%
  group_by(cue_type) %>%
  summarise(
    mean_proportion_vigilant = mean(proportion_vigilant, na.rm = TRUE),
    sd_proportion_vigilant = sd(proportion_vigilant, na.rm = TRUE)
  )

# Model

vigilance_comparison <- glmmTMB(proportion_vigilant_beta ~ cue_type + (1|site),
                                data = Baboon_vigilance_df,
                                family = beta_family(),
                                na.action = na.fail) 

summary(vigilance_comparison)
# P = 0.00014

# FLIGHT-----

# Summary stats

#pred
nrow(Baboon_flight_df %>% filter(flight_present == 0, cue_type == "Predator")) # 636 videos (82%) no flight
nrow(Baboon_flight_df %>% filter(flight_present == 1, cue_type == "Predator")) # 140 videos (18%) flight

#control
nrow(Baboon_flight_df %>% filter(flight_present == 0, cue_type == "Control")) # 124 videos (90%) no flight
nrow(Baboon_flight_df %>% filter(flight_present == 1, cue_type == "Control")) # 14 videos (10%) flight

# Model

flight_comparison <- glmmTMB(flight_present ~ cue_type + (1|site),
                             data = Baboon_flight_df,
                             family = binomial(),
                             na.action = na.fail)

summary(flight_comparison)
# P = 0.0185

ggplot(Baboon_vigilance_df, aes(x = cue_type, y = proportion_vigilant_beta)) + 
  geom_boxplot() +
  theme_bw()

# Count up flight frequency across treatments
Baboon_flight_df %>%
  count(cue_type, flight_present) %>%
  group_by(cue_type) %>%
  mutate(prop = n / sum(n)) 
# 18% fled from predator, 10% fled from control

