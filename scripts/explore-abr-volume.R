library(dplyr)
library(ggplot2)
library(ggeffects)
library(patchwork)

volume <- read.csv("data/ABR Volume Test.csv")

# Calculate means by Distance and Habitat
means_df <- volume %>%
  group_by(Habitat, Distance_m) %>%
  summarise(mean_volume = mean(Volume_db, na.rm = TRUE), .groups = "drop")

# Plot
(a <- ggplot(volume, aes(x = Distance_m, y = Volume_db, color = Habitat)) +
  geom_point(alpha = 0.4) +
  geom_point(data = means_df,
             aes(x = Distance_m, y = mean_volume),
             size = 3) +
  geom_line(data = means_df,
            aes(x = Distance_m, y = mean_volume, group = Habitat),
            linewidth = 1.2,
            alpha = 0.6) +
  labs(x = "Distance (m)",
       y = "Volume (dB)") +
  scale_color_manual(
    name = "Habitat",
    values = c(
      "Open" = "#0A0A52",
      "Closed" = "#A23B2A"
    )
  ) +
  xlim(0, 10) +
  ylim(50, 80) +
  theme_bw() +
  theme(legend.position = "top"))


model <- lm(Volume_db ~ Distance_m * Habitat, data = volume)
summary(model)


pred <- ggpredict(model, terms = c("Distance_m", "Habitat"))

(b <- ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  labs(x = "Distance (m)",
       y = "Predicted Volume (dB)",
       color = "Habitat",
       fill = "Habitat") +
  theme_bw() +
  scale_color_manual(
    name = "Habitat",
    values = c(
      "Open" = "#0A0A52",
      "Closed" = "#A23B2A"
    )
  ) +
  scale_fill_manual(
    name = "Habitat",
    values = c(
      "Open" = "#0A0A52",
      "Closed" = "#A23B2A"
    )
  ) +
    xlim(0, 10) +
    ylim(50, 80) +
  theme(legend.position = "top"))

combined_fig <- (a + b) +
  plot_annotation(
    tag_levels = 'A', 
    tag_suffix = ""
  ) &
  theme(plot.tag = element_text(size = 14))


ggsave("figures/volume-test.png", width = 10, height = 4, dpi = 300)

