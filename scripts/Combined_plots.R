#Combining model predictions and box/whisker plots 

#PROPORTION VIGILANCE 

#Vigilance by predator cue 
#Merge plotting into one figure
combined_vigilance_predator_plot <- ggplot() +
  # Boxplot with jittered raw data
  geom_boxplot(
    data = Baboon_vigilance_graph_both,
    aes(x = predator_cue, y = proportion_vigilant, fill = "Observed"),
    alpha = 0.6, outlier.shape = NA, 
    position = position_dodge(width = 0.9), width = 0.6
  ) +
  geom_jitter(
    data = Baboon_vigilance_graph_both,
    aes(x = predator_cue, y = proportion_vigilant),
    size = 1.5, alpha = 0.5, color = "#023743",
    position = position_jitter(width = 0.15)
  ) +
  # Model predictions (dodged to the right)
  geom_point(
    data = vigilance_pred_only,
    aes(x = predator_cue, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    data = vigilance_pred_only,
    aes(x = predator_cue, ymin = lower, ymax = upper, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = "Predator Cue",
    y = "Proportion Vigilant",
    color = "",
    fill = ""
  ) +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  scale_fill_manual(values = c("Observed" = "#023743")) +
  scale_color_manual(values = c("Predicted" = "khaki2")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid = element_blank(),
    legend.position = "top"
  )

#vigilance by year
combined_vigilance_year_plot <- ggplot() +
  # Boxplot with jittered raw data
  geom_boxplot(
    data = Baboon_vigilance_stats_both,
    aes(x = year, y = proportion_vigilant, fill = "Observed"),
    alpha = 0.6, outlier.shape = NA,
    position = position_dodge(width = 0.9), width = 0.6
  ) +
  geom_jitter(
    data = Baboon_vigilance_stats_both,
    aes(x = year, y = proportion_vigilant),
    size = 1.5, alpha = 0.5, color = "#023743",
    position = position_jitter(width = 0.15)
  ) +
  # Model predictions (points + error bars)
  geom_point(
    data = vigilance_year_only,
    aes(x = year, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    data = vigilance_year_only,
    aes(x = year, ymin = lower, ymax = upper, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = "Year",
    y = "Proportion Vigilant",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743")) +
  scale_color_manual(values = c("Predicted" = "khaki2")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid = element_blank(),
    legend.position = "top"
  )

#vigilance by habitat
combined_vigilance_habitat_plot <- ggplot() +
  # Boxplot with jittered raw data
  geom_boxplot(
    data = Baboon_vigilance_stats_both,
    aes(x = Habitat, y = proportion_vigilant, fill = "Observed"),
    alpha = 0.6, outlier.shape = NA,
    position = position_dodge(width = 0.9), width = 0.6
  ) +
  geom_jitter(
    data = Baboon_vigilance_stats_both,
    aes(x = Habitat, y = proportion_vigilant),
    size = 1.5, alpha = 0.5, color = "#023743",
    position = position_jitter(width = 0.15)
  ) +
  # Model predictions (points + error bars)
  geom_point(
    data = vigilance_habitat_only,
    aes(x = Habitat, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    data = vigilance_habitat_only,
    aes(x = Habitat, ymin = lower, ymax = upper, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = "Habitat",
    y = "Proportion Vigilant",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743")) +
  scale_color_manual(values = c("Predicted" = "khaki2")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid = element_blank(),
    legend.position = "top"
  )

#vigilance by age-sex class
combined_vigilance_prey_plot <- ggplot() +
  # Boxplot with jittered raw data
  geom_boxplot(
    data = Baboon_vigilance_stats_both,
    aes(x = age_sex_class, y = proportion_vigilant, fill = "Observed"),
    alpha = 0.6, outlier.shape = NA,
    position = position_dodge(width = 0.9), width = 0.6
  ) +
  geom_jitter(
    data = Baboon_vigilance_stats_both,
    aes(x = age_sex_class, y = proportion_vigilant),
    size = 1.5, alpha = 0.5, color = "#023743",
    position = position_jitter(width = 0.15)
  ) +
  scale_x_discrete(labels = c(
    "Female no offspring" = "Female no \noffspring",
    "Female with offspring" = "Female w/ \noffspring",
    "Male" = "Male"
  )) +
  # Model predictions (points + error bars)
  geom_point(
    data = vigilance_prey_only,
    aes(x = age_sex_class, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    data = vigilance_prey_only,
    aes(x = age_sex_class, ymin = lower, ymax = upper, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = "Age and Sex Class",
    y = "Proportion Vigilant",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743")) +
  scale_color_manual(values = c("Predicted" = "khaki2")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid = element_blank(),
    legend.position = "top"
  )

#FREQUENCY OF FLIGHT 

# Flight by predator cue
combined_frequency_pred_plot <- ggplot() +
  # Observed bar plot (no error bars)
  geom_bar(
    data = flight_frequency_pred_only,
    aes(x = predator_cue, y = flight_frequency, fill = "Observed"),
    stat = "identity",
    position = position_dodge(width = 0.9),
    color = "black", alpha = 0.8
  ) +
  # Predicted values with CI (keep error bars)
  geom_point(
    data = frequency_pred_only,
    aes(x = predator_cue, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.9)
  ) +
  geom_errorbar(
    data = frequency_pred_only,
    aes(x = predator_cue, ymin = lower_resp, ymax = upper_resp, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  # Labels and theme
  labs(
    x = "Predator Cue",
    y = "Frequency of Flight",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743")) +
  scale_color_manual(values = c("Observed" = "#023743", "Predicted" = "khaki3")) +
  guides(
    fill = "none",  # remove duplicate observed fill legend
    color = guide_legend(
      override.aes = list(
        shape = c(16),          # only predicted points show up in legend
        linetype = c(1),
        color = c("khaki3")
      )
    )
  ) +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none",
    legend.text = element_text(size = 12),
    panel.grid = element_blank()
  )


# Flight by year
combined_frequency_year_plot <- ggplot() +
  # Observed bar plot (no error bars)
  geom_bar(
    data = flight_frequency_year,
    aes(x = year, y = flight_frequency, fill = "Observed"),
    stat = "identity",
    width = 0.6,
    color = "black", alpha = 0.8
  ) +
  # Predicted values with CI
  geom_point(
    data = frequency_year_only,
    aes(x = year, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    data = frequency_year_only,
    aes(x = year, ymin = lower_resp, ymax = upper_resp, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  # Labels and theme
  labs(
    x = "Year",
    y = "Flight Frequency",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743FF")) +
  scale_color_manual(values = c("Observed" = "#023743FF", "Predicted" = "khaki3")) +
  guides(
    fill = "none",  # remove duplicate observed fill legend
    color = guide_legend(
      override.aes = list(
        shape = c(16),        # only predicted point appears in legend
        linetype = c(1),
        color = c("khaki3")   # predicted = khaki3
      )
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none",
    legend.text = element_text(size = 12)
  )


# Flight by habitat
combined_frequency_habitat_plot <- ggplot() +
  # Observed bar plot (no error bars)
  geom_bar(
    data = flight_frequency_habitat,
    aes(x = Habitat, y = flight_frequency, fill = "Observed"),
    stat = "identity",
    width = 0.6,
    color = "black", alpha = 0.8
  ) +
  # Predicted values with CI
  geom_point(
    data = frequency_habitat_only,
    aes(x = Habitat, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    data = frequency_habitat_only,
    aes(x = Habitat, ymin = lower_resp, ymax = upper_resp, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  # Labels and theme
  labs(
    x = "Habitat",
    y = "Frequency of Flight",
    color = "",
    fill = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743FF")) +
  scale_color_manual(values = c("Observed" = "#023743FF", "Predicted" = "khaki3")) +
  guides(
    fill = "none",  
    color = guide_legend(
      override.aes = list(
        shape = c(16),        
        linetype = c(1),
        color = c("khaki3")   
      )
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none",
    legend.text = element_text(size = 12)
  )
                               

#flight by age-sex class
combined_frequency_prey_plot <- ggplot() +
  # Observed: bars only (no error bars)
  geom_bar(
    data = Baboon_frequency_graph_both,
    aes(x = age_sex_class, y = flight_frequency, fill = "Observed"),
    stat = "identity",
    width = 0.6,
    alpha = 0.8,
    color = "black"
  ) +
  # Predicted: points with CI
  geom_point(
    data = frequency_prey_only,
    aes(x = age_sex_class, y = predicted, color = "Predicted"),
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  geom_errorbar(
    data = frequency_prey_only,
    aes(x = age_sex_class, ymin = lower_resp, ymax = upper_resp, color = "Predicted"),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  # Labels and theme
  labs(
    x = "Age and Sex Class",
    y = "Flight Frequency",
    fill = "",
    color = ""
  ) +
  scale_fill_manual(values = c("Observed" = "#023743FF"), guide = "none") +  # remove duplicate
  scale_color_manual(values = c("Observed" = "#023743FF", "Predicted" = "khaki3")) +
  guides(
    fill = "none",  
    color = guide_legend(
      override.aes = list(
        shape = c(16),        
        linetype = c(1),
        color = c("khaki3")   
      )
    )
  ) +
  scale_x_discrete(labels = c(
    "Female no offspring" = "Female no \noffspring",
    "Female with offspring" = "Female w/ \noffspring",
    "Male" = "Male",
    "Juvenile" = "Juvenile"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none",
    legend.text = element_text(size = 12)
  )

