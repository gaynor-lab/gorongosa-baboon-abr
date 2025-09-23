#Script for creating panel plots of raw data and model predictions for each behavioural response variable

#load packages
install.packages("cowplot")
library(ggplot2)
library(cowplot)


#PROPORTION VIGILANCE - separate plots
plot_grid(
  vigilance_year_plot,
  predicted_vigilance_year_plot,
  vigilance_pred_plot,
  predicted_vigilance_pred_plot,
  vigilance_habitat_plot,
  predicted_vigilance_habitat_plot,
  vigilance_prey_plot,
  predicted_vigilance_prey_plot,
  labels = c("A", "B", "C", "D", "E", "F", "G", "H"),  # Your panel labels
  label_size = 14,  # You can customize the size
  ncol = 2,
  nrow = 4
)

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

#FREQUENCY OF FLIGHT - separate plots
plot_grid(
  frequency_year_plot,
  predicted_frequency_year_plot,
  frequency_pred_plot,
  predicted_frequency_pred_plot,
  frequency_habitat_plot,
  predicted_frequency_habitat_plot,
  frequency_prey_plot,
  predicted_frequency_prey_plot,
  labels = c("A", "B", "C", "D", "E", "F", "G", "H"),  # Your panel labels
  label_size = 14,  # You can customize the size
  ncol = 2,
  nrow = 4
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
