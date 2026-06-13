# ===================================================================
# Marginal effects plots (final layout version)
# ===================================================================

library(glmmTMB)
library(ggeffects)
library(ggplot2)
library(dplyr)
library(patchwork)

theme_set(theme_bw(base_size = 12))

# Load models + data
source("scripts/4. Run_full_models.R")

# ===================================================================
# Scaling (correct extraction from scale())
# ===================================================================

get_scale_params <- function(x_scaled) {
  list(
    mean = attr(x_scaled, "scaled:center"),
    sd   = attr(x_scaled, "scaled:scale")
  )
}

backtransform <- function(x, scale) {
  as.numeric(x) * scale$sd + scale$mean
}


# ===================================================================
# VIGILANCE MODEL
# ===================================================================

# Scaling parameters
vig_group_scale <- get_scale_params(Baboon_vigilance_df_nocontrol$group_number)
vig_day_scale   <- get_scale_params(Baboon_vigilance_df_nocontrol$day_number)

# Marginal effects
vig_cue      <- as.data.frame(ggpredict(Vigilance_global_model, "predator_cue"))
vig_year     <- as.data.frame(ggpredict(Vigilance_global_model, "year"))
vig_habitat  <- as.data.frame(ggpredict(Vigilance_global_model, "Habitat"))
vig_age      <- as.data.frame(ggpredict(Vigilance_global_model, "age_sex_class"))
vig_group    <- as.data.frame(ggpredict(Vigilance_global_model, "group_number [all]"))
vig_day      <- as.data.frame(ggpredict(Vigilance_global_model, "day_number [all]"))

# Back-transform
vig_group$x <- backtransform(vig_group$x, vig_group_scale)
vig_day$x   <- backtransform(vig_day$x, vig_day_scale)

# Change labels
vig_age <- vig_age %>%
  mutate(x = recode(x,
                    "Female_Adult_no_offspring"   = "Adult F",
                    "Female_Adult_with_offspring" = "Adult F w/ offspring",
                    "Juvenile"                    = "Juvenile",
                    "Male_Adult"                  = "Adult M"
  ))

# Shared y limits
vig_ymin <- min(c(vig_cue$conf.low, vig_year$conf.low, vig_habitat$conf.low,
                  vig_age$conf.low, vig_group$conf.low, vig_day$conf.low), na.rm = TRUE)

vig_ymax <- max(c(vig_cue$conf.high, vig_year$conf.high, vig_habitat$conf.high,
                  vig_age$conf.high, vig_group$conf.high, vig_day$conf.high), na.rm = TRUE)

# Helper: remove axis
no_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
)
no_x <- theme(
  axis.title.x = element_blank()
)

# Row 1a
p1 <- ggplot(vig_cue, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#1e9b56", size = 3) +
  coord_cartesian(ylim = c(vig_ymin, vig_ymax)) +
  labs(y = "Proportion vigilant", title = "Predator cue") +
  no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p2 <- ggplot(vig_year, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#f8be1d", size = 3) +
  coord_cartesian(ylim = c(vig_ymin, vig_ymax)) +
  labs(title = "Year") +
  no_y + no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3 <- ggplot(vig_habitat, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#f27229", size = 3) +
  coord_cartesian(ylim = c(vig_ymin, vig_ymax)) +
  labs(title = "Habitat", y = "Proportion vigilant") +
  no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Row 2
p4 <- ggplot(vig_age, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#0077B6", size = 3) +
  coord_cartesian(ylim = c(vig_ymin, vig_ymax)) +
  labs(title = "Age-sex class") +
  no_y + no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p5 <- ggplot(vig_group, aes(x = x, y = predicted)) +
  geom_line(lwd = 1, col = "#ea3633") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  coord_cartesian(ylim = c(vig_ymin, vig_ymax)) +
  labs(y = "Proportion vigilant", title = "Number of neighbors") +
  no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p6 <- ggplot(vig_day, aes(x = x, y = predicted)) +
  geom_line(lwd = 1, col = "#4c00b0") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  coord_cartesian(ylim = c(vig_ymin, vig_ymax)) +
  labs(x = "Day of study", title = "Day of study") +
  no_y + no_x +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Combine 2×3
p_vig <- (p1 | p2 ) / (p3 | p4) / (p5 | p6)

# ===================================================================
# FLIGHT MODEL
# ===================================================================

flight_group_scale <- get_scale_params(Baboon_flight_df_nocontrol$group_number)
flight_day_scale   <- get_scale_params(Baboon_flight_df_nocontrol$day_number)

flight_cue      <- as.data.frame(ggpredict(Flight_global_model, "predator_cue"))
flight_year     <- as.data.frame(ggpredict(Flight_global_model, "year"))
flight_habitat  <- as.data.frame(ggpredict(Flight_global_model, "Habitat"))
flight_age      <- as.data.frame(ggpredict(Flight_global_model, "age_sex_class"))
flight_group    <- as.data.frame(ggpredict(Flight_global_model, "group_number [all]"))
flight_day      <- as.data.frame(ggpredict(Flight_global_model, "day_number [all]"))

# Back-transform
flight_group$x <- backtransform(flight_group$x, flight_group_scale)
flight_day$x   <- backtransform(flight_day$x, flight_day_scale)

# Change age-sex labels
flight_age <- flight_age %>%
  mutate(x = recode(x,
                    "Female_Adult_no_offspring"   = "Adult F",
                    "Female_Adult_with_offspring" = "Adult F w/ offspring",
                    "Juvenile"                    = "Juvenile",
                    "Male_Adult"                  = "Adult M"
  ))

# Shared y
flight_ymin <- min(c(flight_cue$conf.low, flight_year$conf.low, flight_habitat$conf.low,
                     flight_age$conf.low, flight_group$conf.low, flight_day$conf.low), na.rm = TRUE)

flight_ymax <- max(c(flight_cue$conf.high, flight_year$conf.high, flight_habitat$conf.high,
                     flight_age$conf.high, flight_group$conf.high, flight_day$conf.high), na.rm = TRUE)

fp1 <- ggplot(flight_cue, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#1e9b56", size = 3) +
  coord_cartesian(ylim = c(flight_ymin, flight_ymax)) +
  labs(y = "Flight probability", title = "Predator cue") +
  no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fp2 <- ggplot(flight_year, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#f8be1d", size = 3) +
  coord_cartesian(ylim = c(flight_ymin, flight_ymax)) +
  labs(title = "Year") +
  no_y + no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fp3 <- ggplot(flight_habitat, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#f27229", size = 3) +
  coord_cartesian(ylim = c(flight_ymin, flight_ymax)) +
  labs(title = "Habitat", y = "Flight probability") +
  no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fp4 <- ggplot(flight_age, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, width = 0) +
  geom_point(col = "#0077B6", size = 3) +
  coord_cartesian(ylim = c(flight_ymin, flight_ymax)) +
  labs(title = "Age-sex class") +
  no_y + no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fp5 <- ggplot(flight_group, aes(x = x, y = predicted)) +
  geom_line(lwd = 1, col = "#ea3633") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  coord_cartesian(ylim = c(flight_ymin, flight_ymax)) +
  labs(title ="Number of neighbors", y = "Flight probability") +
  no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fp6 <- ggplot(flight_day, aes(x = x, y = predicted)) +
  geom_line(lwd = 1, col = "#4c00b0") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  coord_cartesian(ylim = c(flight_ymin, flight_ymax)) +
  labs(title = "Day of study") +
  no_y + no_x + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Combine
p_flight <- (fp1 | fp2 ) / (fp3 | fp4) /
  (fp5 | fp6)

p_vig
p_flight
