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
offsets <- read.csv("data/files_requiring_offset.csv")

# PROPORTION OF TIME SPENT VIGILANT-----

#Transform data for beta distribution using Smithson & Verkuilen transformation
#this is needed because beta distribution requires values to be 0<x<1 but in proportion_vigilance we have exact 0s and 1s
#this transformation compresses the scale of the data, taking values away from exactly 0 and 1
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n())

# Create variable for predator vs control
Baboon_vigilance_df <- Baboon_vigilance_df %>% 
  mutate(cue_type = case_when(predator_cue == "Control" ~ "Control",
                              predator_cue %in% c("Leopard", "Lion", "Wild dog", "Cheetah", "Hyena") ~ "Predator"))


# FLIGHT-----

#make new column with month and day to test for sound habituation
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day   = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)))

# Correction of month and day

# Create variable for predator vs control
Baboon_flight_df <- Baboon_flight_df %>% 
  mutate(cue_type = case_when(predator_cue == "Control" ~ "Control",
                              predator_cue %in% c("Leopard", "Lion", "Wild dog", "Cheetah", "Hyena") ~ "Predator"))


# Comparison --------------------------------------------------------------

vigilance_comparison <- glmmTMB(proportion_vigilant_beta ~ cue_type + (1|site),
                                data = Baboon_vigilance_df,
                                family = beta_family(),
                                na.action = na.fail) 

summary(vigilance_comparison)
# P = 0.0244

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

# Look into vigilance a bit more
pred_only <- Baboon_vigilance_df %>% filter(cue_type == "Predator")
pred_vigilant_only <- pred_only %>% filter(proportion_vigilant > 0)
nrow(pred_vigilant_only)/nrow(pred_only) # 82.2% of videos (excluding flight)
nrow(pred_vigilant_only)/744 # 71% of videos (including flight)
mean(pred_only$proportion_vigilant)
sd(pred_only$proportion_vigilant)

control_only <- Baboon_vigilance_df %>% filter(cue_type == "Control")
control_vigilant_only <- control_only %>% filter(proportion_vigilant > 0)
nrow(control_vigilant_only)/nrow(control_only) # 72.5% of videos
mean(control_only$proportion_vigilant)
sd(control_only$proportion_vigilant)