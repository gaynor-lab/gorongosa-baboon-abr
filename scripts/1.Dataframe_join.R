# 1. Joining dataframes (CVAT annotations and metadata)

# Load packages
library(xml2)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

# Import metadata in CSV format 
B_21_second <- read.csv("data/second watch 2026 correction/2021_baboon_second.csv")

B_24_second <- read.csv("data/second watch 2026 correction/2024_baboon_second.csv")

# Make file name column in second watch data to join with file_name from CVAT annotations
B_21_second<- B_21_second %>%
  mutate(file_name = paste(year, site, video_name, sep = "_"))

B_24_second<- B_24_second %>%
  mutate(file_name = paste(year, site, video_name, sep = "_"))


# Join for 2024 -----------------------------------------------------------

# Convert XML to dataframe
xml_file_2024 <- read_xml("data/2024_baboon_CVAT.xml")

# Extract all tasks (task = 1 video)
tasks <- xml_find_all(xml_file_2024, ".//task")

# Create task and video ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})

# Create annotations with task ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})

# Extract all <track> nodes
tracks <- xml_find_all(xml_file_2024, ".//track")

# Convert to data frame with desired columns
frame_df <- map_df(tracks, ~{
  task_id <- xml_attr(.x, "task_id")
  label <- xml_attr(.x, "label")
  
  # Extract <box> elements within each <track>
  boxes <- xml_find_all(.x, ".//box")
  
  map_df(boxes, ~tibble(
    task_id = as.integer(task_id),
    label = label,
    frame = as.integer(xml_attr(.x, "frame")),
    xtl = xml_attr(.x, "xtl"), # x left
    ytl = xml_attr(.x, "ytl"), # y top left
    xbr = xml_attr(.x, "xbr"), # x bottom right
    ybr = xml_attr(.x, "ybr") # y bottom right
  ))
})

# Calculate width and height of bounding boxes
frame_df <- frame_df %>%
  mutate(
    width = as.numeric(xbr) - as.numeric(xtl),
    height = as.numeric(ybr) - as.numeric(ytl)
  )

# Join with file name
frame_df2 <- left_join(frame_df, task_df)
colnames(frame_df2)[colnames(frame_df2) == "task_name"] <- "file_name"

# Fix typo in L11 date file names
frame_df2 <- frame_df2 %>%
  mutate(
    file_name = if_else(
      str_detect(file_name, "L11") & str_detect(file_name, "2924"),
      str_replace(file_name, "2924", "2024"),
      file_name
    )
  )

# Join datasets based on filename
Final_2024 <- frame_df2 %>%
  left_join(B_24_second, by = "file_name")

# Rename columns
colnames(Final_2024)[colnames(Final_2024) == "task_id"] <- "Task_ID"
colnames(Final_2024)[colnames(Final_2024) == "label"] <- "Behaviour"

# Fix typo in file names
Final_2024 <- Final_2024 %>%
  mutate(
    file_name = if_else(
      str_detect(file_name, "L11"),
      str_replace(file_name, "2924", "2024"),
      file_name
    ),
    file_name = str_replace_all(file_name, c(
      "^2024_F07_7260057_Baboon\\.AVI$" = "2024_F07_07260057_Baboon.AVI",
      "^2024_D05_0790076_Baboon\\.AVI$" = "2024_D05_07090076_Baboon.AVI"
    ))
  )

# Ensure wild dog is consistently named
Final_2024 <- Final_2024 %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

# Fix issues with spacing in predator cues
Final_2024 <- Final_2024 %>%
  mutate(predator_cue = str_trim(predator_cue))

# Fix typo in Walking_V
Final_2024 <- Final_2024 %>%
  mutate(Behaviour = ifelse(Behaviour == "Waking_V", "Walking_V", Behaviour))


# Join for 2021 -----------------------------------------------------------

# Convert XML to dataframe
xml_file_2021 <- read_xml("data/2021_baboon_CVAT.xml")

# Extract all tasks (task = 1 video)
tasks <- xml_find_all(xml_file_2021, ".//task")

# Create task and video ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})

# Create annotations with task ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})

# Extract all <track> nodes
tracks <- xml_find_all(xml_file_2021, ".//track")

# Convert to data frame with desired columns
frame_df <- map_df(tracks, ~{
  task_id <- xml_attr(.x, "task_id")
  label <- xml_attr(.x, "label")
  
  # Extract <box> elements within each <track>
  boxes <- xml_find_all(.x, ".//box")
  
  map_df(boxes, ~tibble(
    task_id = as.integer(task_id),
    label = label,
    frame = as.integer(xml_attr(.x, "frame")),
    xtl = xml_attr(.x, "xtl"), # x left
    ytl = xml_attr(.x, "ytl"), # y top left
    xbr = xml_attr(.x, "xbr"), # x bottom right
    ybr = xml_attr(.x, "ybr") # y bottom right
  ))
})

# Calculate width and height of bounding boxes
frame_df <- frame_df %>%
  mutate(
    width = as.numeric(xbr) - as.numeric(xtl),
    height = as.numeric(ybr) - as.numeric(ytl)
  )

# Fix typo in 2021_D05_08120024_Baboon_AVI file name
frame_df2 <- frame_df2 %>%
  mutate(file_name = recode(
    file_name,
    "2021_D05_08120024_Baboon_AVI" = "2021_D05_08120024_Baboon_.AVI"
  ))

# Join with file name
frame_df2 <- left_join(frame_df, task_df)
colnames(frame_df2)[colnames(frame_df2) == "task_name"] <- "file_name"

# Join datasets based on filename
merged_clean_2021 <- frame_df2 %>%
  left_join(B_21_second, by = "file_name")

# Rename columns
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "task_id"] <- "Task_ID"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "label"] <- "Behaviour"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "Sound.quality...Good..Poor..None"] <- "Sound_quality"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "Sound.delay..s."] <- "Sound_delay_s"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "Other.species.present..list.w.commas."] <- "Other_species_present"

# Rename behaviour labels
Final_2021 <- merged_clean_2021 %>%
  mutate(Behaviour = case_when(
    Behaviour == "Walking (w/o vigilance)" ~ "Walking_NV",
    Behaviour == "Staring (not walking)" ~ "Staring",
    Behaviour == "Scanning (not walking)" ~ "Scanning",
    Behaviour == "Fleeing" ~ "Flight",
    Behaviour == "Social interactions" ~ "Social_interactions",
    Behaviour == "occluded" ~ "Occluded",
    Behaviour == "walking with vigilance" ~ "Walking_V",
    Behaviour == "startling" ~ "Startling",
    Behaviour == "standing and staring" ~ "Stand_stare",
    TRUE ~ Behaviour  # Keep all other values unchanged
  ))

# Fix typos
Final_2021 <- Final_2021 %>%
  mutate(predator_cue = recode(predator_cue,
                               "Leo[ard" = "Leopard",
                               "Cheeetah" = "Cheetah",
                               "Control " = "Control"
  ))

# Ensure wild dog is consistently named
Final_2024 <- Final_2024 %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

# Rename NA predator cues to No_sound
Final_2021 <- Final_2021 %>%
  mutate(predator_cue = if_else(is.na(predator_cue), "No_sound", predator_cue))

# Fix issues with spacing in predator cues
Final_2024 <- Final_2024 %>%
  mutate(predator_cue = str_trim(predator_cue))

# Save dataframes 
saveRDS(Final_2021, "data_derived/Final_2021.rds")
saveRDS(Final_2024, "data_derived/Final_2024.rds")
