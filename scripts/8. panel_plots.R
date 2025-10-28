#Script for creating panel plots of raw data and model predictions for each behavioural response variable

#load packages
library(ggplot2)
library(cowplot)

#PROPORTION VIGILANCE - combined plots 
plot_grid(
  combined_vigilance_year_plot,
  combined_vigilance_predator_plot,
  combined_vigilance_habitat_plot,
  combined_vigilance_prey_plot,
  labels = c("A", "B", "C", "D"), 
  label_size = 14,  
  ncol = 2,
  nrow = 2
)

#PROPORTION VIGILANCE - combined plots, averaged predictions
plot_grid(
  combined_avg_vigilance_year_plot,
  combined_avg_vigilance_predator_plot,
  combined_avg_vigilance_habitat_plot,
  combined_avg_vigilance_prey_plot,
  labels = c("A", "B", "C", "D"), 
  label_size = 14,  
  ncol = 2,
  nrow = 2
)

#PROPORTION VIGILANCE - combined plots, fixed predictions
plot_grid(
  combined_fixed_vigilance_year_plot,
  combined_fixed_vigilance_predator_plot,
  combined_fixed_vigilance_habitat_plot,
  combined_fixed_vigilance_prey_plot,
  labels = c("A", "B", "C", "D"), 
  label_size = 14,  
  ncol = 2,
  nrow = 2
)

#LATENCY TO FLEE - Kaplan-mier curves 
plot_grid(
  year_plot$plot,
  predator_plot$plot,
  habitat_plot$plot,
  sex_plot$plot,
  labels = c("A", "B", "C", "D"),  
  label_size = 14,
  ncol = 2,
  nrow = 2
)

#FREQUENCY OF FLIGHT - combined plots
plot_grid(
  combined_frequency_year_plot,
  combined_frequency_pred_plot,
  combined_frequency_habitat_plot,
  combined_frequency_prey_plot,
  labels = c("A", "B", "C", "D"), 
  label_size = 14,  
  ncol = 2,
  nrow = 2
)

#FREQUENCY OF FLIGHT - combined plots, averaged predictions
plot_grid(
  combined_avg_frequency_year_plot,
  combined_avg_frequency_pred_plot,
  combined_avg_frequency_habitat_plot,
  combined_avg_frequency_prey_plot,
  labels = c("A", "B", "C", "D"), 
  label_size = 14,  
  ncol = 2,
  nrow = 2
)

#FREQUENCY OF FLIGHT - combined plots, fixed predictions
plot_grid(
  combined_fixed_frequency_year_plot,
  combined_fixed_frequency_pred_plot,
  combined_fixed_frequency_habitat_plot,
  combined_fixed_frequency_prey_plot,
  labels = c("A", "B", "C", "D"), 
  label_size = 14,  
  ncol = 2,
  nrow = 2
)
