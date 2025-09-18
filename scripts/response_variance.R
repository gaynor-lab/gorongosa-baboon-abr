#Test differences in variances in 2021 vs 2024

#Load packages
library(car)

#Levene's test for variance 
leveneTest(proportion_vigilant ~ year, data = Baboon_vigilance_stats_both)
leveneTest(latency_to_flee ~ year, data = Baboon_flight_stats_both)
