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
# P = 0.0244

# FLIGHT-----

flight_comparison <- glmmTMB(flight_present ~ cue_type + (1|site),
                             data = Baboon_flight_df,
                             family = binomial(),
                             na.action = na.fail)

summary(flight_comparison)
# P = 0.030

ggplot(Baboon_vigilance_df, aes(x = cue_type, y = proportion_vigilant_beta)) + 
  geom_boxplot() +
  theme_bw()

# Count up flight frequency across treatments
count(Baboon_flight_df, cue_type, flight_present)
13/(113+13) # 10% fled from control
140/(650+140) # 18% fled from predator
