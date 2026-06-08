library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)


metadata_2024 <- read.csv("data/second watch 2026 correction/2024_baboon_second.csv") %>%
  mutate(file_name = paste(year, site, video_name, sep = "_")) %>% 
  select(file_name, predator_file) %>% 
  mutate(predator_file = as.factor(predator_file)) %>% 
  filter(predator_file %in% c("31", "32", "33", "34", "35", "36", "37")) %>%
  mutate(control_species = case_when(
    predator_file == "31" ~ "African scops owl",
    predator_file == "32" ~ "African barred owlet",
    predator_file %in% c("33", "34") ~ "Dove",
    predator_file == "35" ~ "African hoopoe",
    predator_file %in% c("36", "37") ~ "Southern ground hornbill"
  )) 

(counts <- count(metadata_2024, control_species))
mean(counts$n)

# Vigilance

Baboon_vigilance_df <- readRDS("data_derived/Baboon_vigilance_df.rds") %>% 
  filter(predator_cue == "Control", year == "2024") %>% 
  left_join(metadata_2024) %>% 
  drop_na()


# Calculate sample sizes
n_df <- Baboon_vigilance_df %>%
  group_by(control_species) %>%
  summarise(n = n(), .groups = "drop")
            
            
A <- Baboon_vigilance_df %>% 
  ggplot(aes(x = control_species,
             y = proportion_vigilant)) +
  geom_boxplot() +
  geom_text(
    data = n_df,
    aes(x = control_species, y = .95, label = paste0("n = ", n)),
    inherit.aes = FALSE
  ) +
  labs(
    x = "Control species",
    y = "Proportion vigilant"
  ) +
  theme_bw() +
  coord_flip()


# Flight

Baboon_flight_df <- readRDS("data_derived/Baboon_flight_binary_df.rds") %>% 
  filter(predator_cue == "Control", year == "2024") %>% 
  left_join(metadata_2024) %>% 
  drop_na()

count(Baboon_flight_df, control_species, flight_present)

n_df_flight <- Baboon_flight_df %>%
  group_by(control_species) %>%
  summarise(n = n(), .groups = "drop")

B <- Baboon_flight_df %>%
  count(control_species, flight_present) %>%
  ggplot(aes(x = factor(control_species),
             y = n,
             fill = factor(flight_present))) +
  geom_col(position = "fill") +
  geom_text(
    data = n_df_flight,
    aes(x = control_species, y = .9, label = paste0("n = ", n)),
    inherit.aes = FALSE
  ) +
  labs(
    x = "Control species",
    y = "Proportion of trials",
    fill = "Flight present"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  coord_flip()

(combined_plot <- (A / B) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14)))

ggsave("figures/control-sound-comparison.png",
       width = 7, height = 7, units = "in", dpi = 300)
