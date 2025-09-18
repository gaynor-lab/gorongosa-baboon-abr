#T-test to test pre and post volume change in 2024

#import dataframes for 2024 
Baboon_vigilance_data_24 <- readRDS("data_derived/Baboon_vigilance_data_24.rds")
Baboon_flight_data_24 <- readRDS("data_derived/Baboon_flight_data_24.rds")
Baboon_frequency_data_24 <- readRDS("data_derived/Baboon_frequency_data_24.rds")

#T-test dataframe for vigilance
ttest_vigilance <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, proportion_vigilant, volume_change) %>%
  ungroup()

#T-test dataframe for latency
ttest_latency <- Baboon_flight_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, latency_to_flee_s, volume_change) %>%
  ungroup()

#T-test dataframe for frequency
ttest_frequency <- Baboon_frequency_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, flight_present, volume_change) %>%
  ungroup()

#check normality for vigilance
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "pre"])
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "post"])

#check normality for latency
shapiro.test(ttest_latency$latency_to_flee_s[ttest_latency$volume_change == "pre"])
shapiro.test(ttest_latency$latency_to_flee_s[ttest_latency$volume_change == "post"])

#check normality for frequency
shapiro.test(ttest_frequency$flight_present[ttest_frequency$volume_change == "pre"])
shapiro.test(ttest_frequency$flight_present[ttest_frequency$volume_change == "post"])
#does not fit assumptions of normality for t-test so Mann-whitney U test

#Mann-whitney U test for vigialnce
wilcox.test(proportion_vigilant ~ volume_change, data = ttest_vigilance)
#p=0.2672, no statistically significant difference 

#Mann-whitney U test for latency
wilcox.test(latency_to_flee_s ~ volume_change, data = ttest_latency)
#p=0.1478, no statistically significant difference

#Mann-whitney U test for frequency
wilcox.test(flight_present ~ volume_change, data = ttest_frequency)
#p=0.4112, no statistically significant difference


#Spearman's rank correlation test for response intensity and volume in 2021 

#import dataframes for 2021
Baboon_vigilance_stats <- readRDS("data_derived/Baboon_vigilance_stats.rds")
Baboon_frequency_stats <- readRDS("data_derived/Baboon_frequency_stats.rds")
Baboon_flight_stats <- readRDS("data_derived/Baboon_flight_stats.rds")

#read in ABR volume data 
ABR_volume_21 <- read.csv("data/ABR_volumes_2021.csv")

#join volume data to vigilance data
Baboon_vigilance_stats <- Baboon_vigilance_stats %>%
  left_join(
    ABR_volume_21 %>% select(Site, dB_avg),
    by = c("site" = "Site")
  )

#join volume data to flight data
Baboon_frequency_stats <- Baboon_frequency_stats %>%
  left_join(
    ABR_volume_21 %>% select(Site, dB_avg),
    by = c("site" = "Site")
  )

#join volume data to latency data
Baboon_flight_stats <- Baboon_flight_stats %>%
  left_join(
    ABR_volume_21 %>% select(Site, dB_avg),
    by = c("site" = "Site")
  )


#Spearman's rank correlation for vigilance
cor.test(
  Baboon_vigilance_stats$dB_avg,
  Baboon_vigilance_stats$proportion_vigilant,
  method = "spearman",
  use = "complete.obs"
)

#Spearman's rank correlation for flight frequency
cor.test(
  Baboon_frequency_stats$dB_avg,
  Baboon_frequency_stats$flight_present,
  method = "spearman",
  use = "complete.obs"
)

#Spearman's rank correlation for latency
cor.test(
  Baboon_flight_stats$dB_avg,
  Baboon_flight_stats$latency_to_flee,
  method = "spearman",
  use = "complete.obs"
)

