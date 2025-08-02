#T-test to test pre and post volume change in 2024

#T-test dataframes
ttest_vigilance <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, proportion_vigilant, volume_change) %>%
  ungroup()

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