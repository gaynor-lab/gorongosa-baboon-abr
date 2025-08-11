#Analysis excluding cheetahs

#Proportion vigilance

#remove cheetahs from predator cues
Baboon_vigilance_stats_no_cheetah <- Baboon_vigilance_stats_both %>%
  filter(predator_cue != "Cheetah")

view(Baboon_vigilance_stats_no_cheetah)

#Global GLMM using beta distribution
Vigilance_global_model_no_cheetah <- glmmTMB(proportion_vigilant_beta ~ predator_cue * year + Habitat + age_sex_class + group_number + (1|site),
                                       data = Baboon_vigilance_stats_no_cheetah,
                                       family = beta_family(),
                                       na.action = na.fail) 

#generate model set
Vigilance_models_no_cheetah <- dredge(Vigilance_global_model_no_cheetah)

#Model averaging based on AIC
Vigilance_model_avg_no_cheetah <- model.avg(Vigilance_models_no_cheetah)

#Get model-averaged results
summary(Vigilance_model_avg_no_cheetah)
print(Vigilance_model_avg_no_cheetah)
formula(Vigilance_models_no_cheetah)

#R-squared 
r.squaredGLMM(Vigilance_global_model_no_cheetah)

#Latency to flee

#remove cheetahs from predator cues
Baboon_flight_stats_no_cheetah <- Baboon_flight_stats_both %>%
  filter(predator_cue != "Cheetah")

View(Baboon_flight_stats_no_cheetah)

#Global GLMM with gaussian (normal) distribution
Latency_global_model_no_cheetah<- glmmTMB(log_latency_to_flee ~ predator_cue * year + Habitat + age_sex_class + group_number + (1|site),
                                     data = Baboon_flight_stats_no_cheetah,
                                     family = gaussian(),
                                     na.action = na.fail)

#generate model set
Latency_models_no_cheetah <- dredge(Latency_global_model_no_cheetah)

# Model averaging based on AIC
Latency_model_avg_no_cheetah <- model.avg(Latency_models_no_cheetah)

# Get model-averaged results
summary(Latency_model_avg_no_cheetah)
print(Latency_model_avg_no_cheetah)

#R-squared
r.squaredGLMM(Latency_global_model_no_cheetah)

#Frequency of flight

#remove cheetahs from predator cues
Baboon_frequency_stats_no_cheetah <- Baboon_frequency_stats_both %>%
  filter(predator_cue != "Cheetah")

#Global GLMM with binomial distribution
Frequency_global_model_no_cheetah <- glmmTMB(flight_present ~ predator_cue * year + Habitat + age_sex_class + group_number + (1|site),
                                       data = Baboon_frequency_stats_no_cheetah,
                                       family = binomial(),
                                       na.action = na.fail)

#generate model set
Frequency_models_no_cheetah <- dredge(Frequency_global_model_no_cheetah)

# Model averaging based on AIC
Frequency_model_avg_no_cheetah <- model.avg(Frequency_models_no_cheetah)

# Get model-averaged results
summary(Frequency_model_avg_no_cheetah)
print(Frequency_model_avg_both)

#R² squared
r2(Frequency_global_model_both)