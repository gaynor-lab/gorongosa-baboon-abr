# gorongosa-baboon-abr
This repository contains R scripts to calculate, model, predict, and graph antipredator response variables (proportion of time spent vigilant and frequency of flight). 

# Script guide  
1. Dataframe_join - This script joins the annotated video data from CVAT (xml) to second watch data (CSV), such as predator audio cue species identity and demographic factors, into one dataframe, joining by the unique video file name.
2. Dataframe_creation - This script creates a dataframe with cleaned data (i.e. removal of poor sound quality videos, reorder frames in chronological order) for each behavioural response variable.
3. Model_averaging - This script creates a global model for each behavioural response variable and then uses the dredge function to perform model averaging.
4. Model_predictions - This script uses the predict function to generate model predictions of each behavioural response variable for each proposed mediator of antipredator behaviour (year, predator cue, prey age-sex class, and habitat).
5. Raw_data_figures - This script creates plots for each behavioural response variable by each mediator of antipredator behaviour.
6. Combined_plots - This script merges the box and whisker plots displaying the raw data (5. Raw_data_figures) with the model predictions (4. Model_predictions).
7. Kaplan_meier analysis - This script runs a Kaplan-Meier survival analysis for latency to flee and creates survival analysis curve plots. 
8. Panel_plots - This script creates paneled plots to display the plots for year, predator cue, age-sex class, and habitat for each behavioural response variable.
9. Volume change - This script tests the correlation between the volume of ABR predator cue and the strength of antipredator behaviour.
10. GNP_ABR_map - This script creates a figure of where Gorgonosa National Park is within Mozambique and where the ABR sites are within the park. 
