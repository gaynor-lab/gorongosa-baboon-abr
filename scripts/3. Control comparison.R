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

vigilance_comparison <- glmmTMB(proportion_vigilant_beta ~ cue_type + (1|site),
                                data = Baboon_vigilance_df,
                                family = beta_family(),
                                na.action = na.fail) 

summary(vigilance_comparison)
# P = 0.00014

# FLIGHT-----

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

