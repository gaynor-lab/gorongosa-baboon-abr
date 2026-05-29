library(dplyr)

Final_2021 <- readRDS("data_derived/Final_2021.rds")
Final_2024 <- readRDS("data_derived/Final_2024.rds")

Final_2021_2024 <- bind_rows(Final_2021, Final_2024)

# Summarize bounding boxes
bounding_box_summary <- Final_2021_2024 %>%
  group_by(file_name) %>%
  arrange(frame, .by_group = TRUE) %>%
  summarise(
    max_width = max(width, na.rm = TRUE),
    max_height = max(height, na.rm = TRUE),
    max_dimension = max(width, height, na.rm = TRUE)
  ) %>% 
  select(file_name, max_width, max_height, max_dimension)

# Take only starting bounding box
bounding_box_initial <- Final_2021_2024 %>%
  arrange(file_name, frame) %>%
  distinct(file_name, .keep_all = TRUE) %>% 
  group_by(file_name) %>% 
  mutate(initial_max_dimension = max(width, height, na.rm = TRUE)) %>% 
  rename(initial_width = width, 
         initial_height = height) %>% 
  select(file_name, initial_width, initial_height, initial_max_dimension)

# Join
bbox_summary <- left_join(bounding_box_summary, bounding_box_initial, by = "file_name")

# Export the bounding box summary
write.csv(bbox_summary, "data_derived/bounding_box_summary.csv", row.names = FALSE)
