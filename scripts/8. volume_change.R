#T-test to test pre and post volume change in 2024

#T-test dataframes
ttest_vigilance <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, proportion_vigilant, volume_change) %>%
  ungroup()

View(ttest_vigilance)
ttest_latency <- Baboon_flight_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, latency_to_flee_s, volume_change) %>%
  ungroup()

ttest_frequency <- Baboon_frequency_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, flight_present, volume_change) %>%
  ungroup()

#check normality for vigilance
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "pre"])
#non normally distributed
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "post"])
#not normally distributeed

#check normality for latency
shapiro.test(ttest_latency$latency_to_flee_s[ttest_latency$volume_change == "pre"])
#non normally distributed
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "post"])
#not normally distributeed

#does not fit assumptions for t-test so Mann-whitney U test
wilcox.test(proportion_vigilant ~ volume_change, data = ttest_vigilance)
#p=0.2672, no statistically significant difference :)

wilcox.test(latency_to_flee_s ~ volume_change, data = ttest_latency)
#p=0.1478, no statistically significant difference

wilcox.test(flight_present ~ volume_change, data = ttest_frequency)
#p=0.4112, no statistically significant difference


#Spearman's rank correlation test for response intensity and volume in 2021 

#read in ABR volume data 
ABR_volume_21 <- read.csv("C:/Users/sophi/OneDrive/Desktop/data-gorongosa-baboon-abr/ABR_volumes_2021.csv")

#join volume data to vigilance data
Baboon_vigilance_stats <- Baboon_vigilance_stats %>%
  left_join(
    ABR_volume_21 %>% select(Site, dB_avg),
    by = c("site" = "Site")
  )

#Spearman's rank correlation
cor.test(
  Baboon_vigilance_stats$dB_avg,
  Baboon_vigilance_stats$proportion_vigilant,
  method = "spearman",
  use = "complete.obs"
)
