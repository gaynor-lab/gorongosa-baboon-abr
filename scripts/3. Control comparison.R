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

#Fix typo in file_name 
Baboon_vigilance_df$file_name <- gsub(
  "^2024_F07_7260057_Baboon\\.AVI$",
  "2024_F07_07260057_Baboon.AVI",
  Baboon_vigilance_df$file_name
)

Baboon_vigilance_df$file_name <- gsub(
  "^2024_D05_0790076_Baboon\\.AVI$",
  "2024_D05_07090076_Baboon.AVI",
  Baboon_vigilance_df$file_name
)

#make new column with month and day to test for sound habituation
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day   = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)))

# Correction of month and day

# Join offsets

baboon_vigilance_offset <- left_join(Baboon_vigilance_df, offsets, by = "file_name")

# Convert offset strings to numeric, and apply correction
baboon_vigilance_offset <- baboon_vigilance_offset %>%
  mutate(
    month_offset_num = as.numeric(month_offset),
    day_offset_num   = as.numeric(day_offset),
    raw_date = make_date(year = year, month = month, day = day),
    date_corrected = case_when(
      is.na(month_offset_num) & is.na(day_offset_num) ~ raw_date,
      TRUE ~ raw_date %m+% months(coalesce(month_offset_num, 0)) + days(coalesce(day_offset_num, 0))
    )
  )

# Days since first deployment, calculated separately per year
Baboon_vigilance_df <- baboon_vigilance_offset %>%
  group_by(year) %>%
  mutate(day_number = as.numeric(date_corrected - min(date_corrected, na.rm = TRUE)) + 1) %>%
  ungroup()

#change Wild dog name to match in both datasets
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix issue with spacing in predator cues
Baboon_vigilance_df <- Baboon_vigilance_df %>%
  mutate(predator_cue = str_trim(predator_cue))

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

#Fix typo in file_name 
Baboon_flight_df$file_name <- gsub(
  "^2024_F07_7260057_Baboon\\.AVI$",
  "2024_F07_07260057_Baboon.AVI",
  Baboon_flight_df$file_name
)

Baboon_flight_df$file_name <- gsub(
  "^2024_D05_0790076_Baboon\\.AVI$",
  "2024_D05_07090076_Baboon.AVI",
  Baboon_flight_df$file_name
)

#make new column with month and day to test for sound habituation
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(month = as.numeric(sub(".*?_(\\d{2}).*", "\\1", file_name)),
         day   = as.numeric(sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)))

# Correction of month and day

# Join offsets
baboon_flight_offset <- left_join(Baboon_flight_df, offsets, by = "file_name")

# Convert offset strings to numeric, and apply correction
baboon_flight_offset <- baboon_flight_offset %>%
  mutate(
    month_offset_num = as.numeric(month_offset),
    day_offset_num   = as.numeric(day_offset),
    raw_date = make_date(year = year, month = month, day = day),
    date_corrected = case_when(
      is.na(month_offset_num) & is.na(day_offset_num) ~ raw_date,
      TRUE ~ raw_date %m+% months(coalesce(month_offset_num, 0)) + days(coalesce(day_offset_num, 0))
    )
  )

# Days since first deployment, calculated separately per year
Baboon_flight_df <- baboon_flight_offset %>%
  group_by(year) %>%
  mutate(day_number = as.numeric(date_corrected - min(date_corrected, na.rm = TRUE)) + 1) %>%
  ungroup()

#change Wild dog name to match in both datasets
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix issue with spacing in predator cues
Baboon_flight_df <- Baboon_flight_df %>%
  mutate(predator_cue = str_trim(predator_cue))

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