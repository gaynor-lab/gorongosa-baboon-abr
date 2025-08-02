#Test differences in variances in 2021 vs 2024
library(car)
leveneTest(proportion_vigilant ~ year, data = Baboon_vigilance_stats_both)
leveneTest(latency_to_flee ~ year, data = Baboon_flight_stats_both)
leveneTest(flight_present ~ year, data = Baboon_frequency_stats_both)

var_2021 <- var(Baboon_frequency_stats_both$flight_present[Baboon_frequency_stats_both$year == 2021])
var_2024 <- var(Baboon_frequency_stats_both$flight_present[Baboon_frequency_stats_both$year == 2024])
var_2021
var_2024

table(Baboon_frequency_stats_both$flight_present, Baboon_frequency_stats_both$year)
chisq.test(table(Baboon_frequency_stats_both$flight_present, Baboon_frequency_stats_both$year))
