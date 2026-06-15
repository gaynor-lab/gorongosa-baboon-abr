# Make flight latency figures

#load packages
library(survival)
library(survminer)
library(dplyr)
library(stringr)

#import dataframes
Final_2024 <- readRDS("data_derived/Final_2024.rds")
Final_2021 <- readRDS("data_derived/Final_2021.rds")

#Create dataframe for flight including non-flight "censored" data - 2024

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_KM_24 <- Final_2024 %>%
  filter(
    !(sound_quality %in% c("Poor", "None")),
    predator_cue != "No_sound"
  ) # Exclude Poor and None Sound_quality

#order frames in chronological order
Baboon_flight_KM_24 <- Baboon_flight_KM_24 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#remove the number of frames before the audio cue is played
Baboon_flight_KM_24 <- Baboon_flight_KM_24 %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_KM_24 <- Baboon_flight_KM_24 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

# Calculate latency to flee per file_name
Baboon_flight_KM_24 <- Baboon_flight_KM_24 %>%
  group_by(file_name) %>%
  arrange(frame) %>%
  mutate(
    total_rows = n(),
    first_frame = min(frame),
    first_flight_frame = if (any(Behaviour == "Flight")) min(frame[Behaviour == "Flight"]) else NA_integer_,
    rows_until_flight = if_else(
      flight_present == 1 & !is.na(first_flight_frame),
      first_flight_frame - first_frame,
      total_rows
    ),
    latency_to_flee_s = rows_until_flight / 30
  ) %>%
  # NOW collapse to 1 row per video
  summarise(
    latency_to_flee_s = first(latency_to_flee_s),
    flight_present = first(flight_present),
    year = first(year),
    predator_cue = first(predator_cue),
    sex = first(sex),
    age = first(age),
    group_number = first(group_number),
    offspring = first(offspring),
    site = first(site),
    .groups = "drop"
  )

#Create dataframe for flight including non-flight "censored" data - 2021 

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_KM_21 <- Final_2021 %>%
  filter(
    !(sound_quality %in% c("Poor", "None")),
    predator_cue != "No_sound"
  )

#order frames in chronological order
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  ungroup()

#remove the number of frames before the audio cue is played
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  mutate(sound_delay_s = ifelse(is.na(sound_delay_s), 0, sound_delay_s),
         sound_delay_s = as.numeric(sound_delay_s)) %>%
  group_by(file_name) %>%
  slice((sound_delay_s[1] * 30 + 1):n()) %>%
  ungroup()

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#Calculate latency to flee
Baboon_flight_KM_21 <- Baboon_flight_KM_21 %>%
  filter(file_name != "2021_E02_08030006_Baboon.AVI") %>% #video got duplicated
  group_by(file_name) %>%
  arrange(.data$frame) %>%
  mutate(
    first_row = first(row_number()),
    first_flight_row = match("Flight", Behaviour),
    
    # if no flight occurred, set rows_until_flight to total number of rows in the video
    rows_until_flight = if_else(
      flight_present == 1 & !is.na(first_flight_row),
      first_flight_row - first_row,
      n()  
    ),
    
    latency_to_flee_s = rows_until_flight / 30  # convert to seconds
  ) %>%
  summarise(
    latency_to_flee_s = first(latency_to_flee_s),
    flight_present = first(flight_present),
    year = first(year),
    predator_cue = first(predator_cue),
    sex = first(sex),
    age = first(age),
    group_number = first(group_number),
    offspring = first(offspring),
    site = first(site),
    .groups = "drop"
  )

#Merge 2021 and 2024 datasets 
Baboon_flight_KM_all <- bind_rows(Baboon_flight_KM_24, Baboon_flight_KM_21)

#change Wild dog name to match in both datasets
Baboon_flight_KM_all <- Baboon_flight_KM_all %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix spacing issue in predator names
Baboon_flight_KM_all <- Baboon_flight_KM_all %>%
  mutate(predator_cue = str_trim(predator_cue))

#add in habitat and age sex class columns
Baboon_flight_KM_all <- Baboon_flight_KM_all %>%
  mutate(
    Habitat = case_when(
      site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
      site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
      TRUE ~ NA_character_  # Ensure other values get NA if not listed
    ),
    age_sex_class = case_when(
      sex == "J" & age == "J" ~ "Juvenile",
      sex == "F" & age == "A" & offspring == 1 ~ "Female_Adult_with_offspring",
      sex == "F" & age == "A" & offspring == 0 ~ "Female_Adult_no_offspring",
      sex == "M" & age == "A" ~ "Male_Adult",
      TRUE ~ NA_character_  # Default if nothing matches
    )
  )

# Remove control cues
Baboon_flight_KM_all_nocontrol <- Baboon_flight_KM_all %>% 
  filter(predator_cue != "Control")

#View(Baboon_flight_KM_all)
# Create the survival object
surv_obj <- Surv(time = Baboon_flight_KM_all_nocontrol$latency_to_flee_s, 
                 event = Baboon_flight_KM_all_nocontrol$flight_present)

# Fit the Kaplan-Meier survival curve for all data combined
fit <- survfit(surv_obj ~ 1)

#Create the overall survival curve plot
km_plot <- ggsurvplot(
  fit, 
  data = Baboon_flight_KM_all_nocontrol,
  fun = "event",
  risk.table = FALSE,      
  conf.int = TRUE,
  xlab = "Time Since Predator Cue (seconds)",
  ylab = "Probability of Flight",
  censor = FALSE,
  ggtheme = theme_minimal()
)

# Modify plot
((km_plot$plot <- km_plot$plot +
  scale_color_manual(values = "black") +
  theme(
    panel.grid.minor = element_blank(),    
    panel.grid.major = element_line(color = "grey80"),  
    axis.text = element_text(size = 10),   
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0, 0.25))))

ggsave("figures/KM_plot_all.png", km_plot$plot, width = 5, height = 4)
ggsave("figures/publication/Figure5.png", km_plot$plot, width = 5, height = 4)

#KM plot for predator cue

#Set 'predator_cue' as factor
Baboon_flight_KM_all$predator_cue <- as.factor(Baboon_flight_KM_all$predator_cue)

# Create the survival object WITH control
surv_obj_all <- Surv(time = Baboon_flight_KM_all$latency_to_flee_s, 
                 event = Baboon_flight_KM_all$flight_present)

# Fit survival model
fit_predator <- survfit(surv_obj_all ~ predator_cue, data = Baboon_flight_KM_all)

#Create survival curve plot
predator_plot <- ggsurvplot(
  fit_predator,
  data = Baboon_flight_KM_all,
  fun = "event",
  conf.int = TRUE,
  conf.int.alpha = 0.10,
  risk.table = FALSE,
  xlab = "Latency to flee (seconds)",
  ylab = "Probability of flight",
  censor.shape = "|",
  ggtheme = theme_minimal(),
  legend.title = "Predator cue",    
  legend.labs = c("Cheetah", "Hyena", "Lion", "Control", "Leopard", "Wild dog") 
)
  
# Customize theme and zoom
predator_plot$plot <- predator_plot$plot +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(0, 0.40))

#pairwise analysis
pairwise_survdiff(Surv(latency_to_flee_s, flight_present) ~ predator_cue,
                  data = Baboon_flight_KM_all,
                  p.adjust.method = "holm")


#KM Plot for year

# Fit survival model
fit_year <- survfit(surv_obj ~ year, data = Baboon_flight_KM_all_nocontrol)

#Create survival curve plot
year_plot <- ggsurvplot(
  fit_year,
  data = Baboon_flight_KM_all_nocontrol,
  fun = "event",
  conf.int = TRUE,
  conf.int.alpha = 0.10,
  risk.table = FALSE,
  xlab = "Latency to flee (seconds)",
  ylab = "Probability of flight",
  censor.shape = "|",
  ggtheme = theme_minimal(),
  legend.title = "Year",    
  legend.labs = c("2021", "2024")
  
)
year_plot$plot <- year_plot$plot +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(0, 0.3))

#pairwise analysis
pairwise_survdiff(Surv(latency_to_flee_s, flight_present) ~ year,
                  data = Baboon_flight_KM_all_nocontrol,
                  p.adjust.method = "holm")

#KM plot for age_sex class

#filter out NAs for age_sex class
Baboon_flight_age_sex <- Baboon_flight_KM_all_nocontrol %>%
  filter(!is.na(age_sex_class))

#create survival object
surv_obj_age_sex <- Surv(
  time = Baboon_flight_age_sex$latency_to_flee_s,
  event = Baboon_flight_age_sex$flight_present
)

#fit survival curve
fit_sex <- survfit(surv_obj_age_sex ~ age_sex_class, data = Baboon_flight_age_sex)

#plot
sex_plot <- ggsurvplot(
  fit_sex,
  data = Baboon_flight_age_sex,
  conf.int = TRUE,
  conf.int.alpha = 0.10,
  fun = "event",
  risk.table = FALSE,
  xlab = "Latency to flee (seconds)",
  ylab = "Probability of flight",
  censor.shape = "|",
  ggtheme = theme_minimal(),
  legend.title = "Age sex class",    
  legend.labs = c("Female adult no offspring", "Female adult with offspring", "Juvenile", "Male")
)

sex_plot$plot <- sex_plot$plot +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(0, 0.4))

#pairwise analysis
pairwise_survdiff(Surv(latency_to_flee_s, flight_present) ~ age_sex_class,
                  data = Baboon_flight_KM_all_nocontrol,
                  p.adjust.method = "holm")

#KM plot for habitat

# Fit survival model
fit_habitat <- survfit(surv_obj ~ Habitat, data = Baboon_flight_KM_all_nocontrol)

#plot
habitat_plot <- ggsurvplot(
  fit_habitat,
  data = Baboon_flight_KM_all_nocontrol,
  fun = "event",
  conf.int = TRUE,
  conf.int.alpha = 0.10,
  risk.table = FALSE,
  xlab = "Latency to flee (seconds)",
  ylab = "Probability of flight",
  censor.shape = "|",
  ggtheme = theme_minimal(),
  legend.title = "Habitat",    
  legend.labs = c("Closed", "Open")
)
habitat_plot$plot <- habitat_plot$plot +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(0, 0.3))

#pairwise analysis
pairwise_survdiff(Surv(latency_to_flee_s, flight_present) ~ Habitat,
                  data = Baboon_flight_KM_all_nocontrol,
                  p.adjust.method = "holm")
    

# Save all plots

ggsave("figures/KM_predator.png", predator_plot$plot, width = 6, height = 4, dpi = 300)
ggsave("figures/KM_year.png", year_plot$plot, width = 6, height = 4, dpi = 300)
ggsave("figures/KM_age_sex.png", sex_plot$plot, width = 6, height = 4, dpi = 300)
ggsave("figures/KM_habitat.png", habitat_plot$plot, width = 6, height = 4, dpi = 300)

library(cowplot)
multipanel <- plot_grid(year_plot$plot, predator_plot$plot, habitat_plot$plot, sex_plot$plot,
          labels = c('A', 'B', 'C', 'D'), ncol = 2)
ggsave("figures/KM_supplement_multipanel.png", multipanel, width = 12, height = 10, dpi = 300)
ggsave("figures/publication/FigureS3.png", multipanel, width = 12, height = 10, dpi = 300)
