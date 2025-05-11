# gorongosa-baboon-abr
This repository contains R scripts to calculate antipredator response variables (proportion of time spent vigilant, latency to flee, and frequency of flight). 

#Script guide  
1. Dataframe_join - This script joins the annotated video data from CVAT (xml) to second watch data (CSV) such as predator audio cue species identity and demographic factors into one dataframe joining by the unique video file name.
2. Dataframe_creation - This script creates a dataframe with cleaned data (i.e. removal of poor sound quality videos, reorder frames in chronological order) for each behavioural response variable.
3. Model_averaging - This script creates a global model for each behavioural response variable and then uses the dredge function to perform model averaging.
4. Model_Predictions - This script uses the predict function to generate model predictions of each behavioural response variable for each proposed mediator of antipredator behaviour (year, predator cue, prey age-sex class, and habitat)
5. Additional_analysis - This script calculates additional analyses including: T-test for volume change of ABRs, levene's test for variance in antipredator behaviour between years, and linear regression to test habituation to audio cues. 
6. Raw_dara_figures - This script creates plots for each behavioural response variable by each mediator of antipredator behaviour.
7. Panel_plots - This script creates panelled plots to display the raw data side by side to the model predictions for each behavioural response variable.
