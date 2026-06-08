alvar <- read.csv("baboon_second_check - 2021_baboon_second_check.csv") %>%
  mutate(file_name = paste(year, site, video_name, sep = "_")) 

`%notin%` <- Negate(`%in%`)

test <- frame_df2 %>% 
  select(file_name) %>% 
  unique()

test %>% filter(file_name %notin% alvar$file_name)

missing_annotation <- alvar %>% filter(file_name %notin% test$file_name) %>% 
  filter(sound_quality == "Good")

write.csv(missing_annotation, "missing_annotation.csv", row.names = FALSE)


# NO 2024 ISSUES

alvar2024 <- read.csv("baboon_second_check - 2024_baboon_second_check.csv") %>%
  mutate(file_name = paste(year, site, video_name, sep = "_")) 

test <- frame_df2 %>% 
  select(file_name) %>% 
  unique()

test %>% filter(file_name %notin% alvar2024$file_name)

missing_annotation <- alvar2024 %>% filter(file_name %notin% test$file_name) %>% 
  filter(sound_quality == "Good")

