# Script for creating study area map 

# Load packages
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(ggforce)
library(tidyr)


# Map of ABR locations ----------------------------------------------------

# Load shapefiles
GNP_boundary <- st_read("data/GNP_shp/GNP_Landscapes_New_boundary.shp")
Lake_urema <- st_read("data/GNP_shp/LakeUrema.shp")
Roads <- st_read("data/GNP_shp/roads_may2015.shp")
Africa <- st_read("data/GNP_shp/ne_10m_admin_0_countries.shp") %>%
  filter(REGION_UN == "Africa")
GNP_boundary_only <- st_read("data/GNP_shp/WDPA_WDOECM_Nov2025_Public_801_shp-polygons.shp")

# Load ABR data
ABR_data <- read.csv("data/ABR_locations.csv") %>% 
  mutate(
  X2021 = ifelse(is.na(X2021), 0, X2021),
  X2024 = ifelse(is.na(X2024), 0, X2024),
  alpha_val = ifelse(Habitat == "Open", 0.9, 0.4)  # transparency
)

# Convert ABR locations to sf object
ABR_data <- st_as_sf(
  ABR_data,
  coords = c("Long", "Lat"),   
  crs = 4326                   
)

#Match projections
# Use GNP_boundary as reference CRS
crs_ref <- st_crs(GNP_boundary)

Lake_urema  <- st_transform(Lake_urema, crs_ref)
Roads       <- st_transform(Roads, crs_ref)
ABR_data <- st_transform(ABR_data, crs_ref)
GNP_boundary_only <- st_transform(GNP_boundary_only, crs_ref)

# Create bounding box
bbox <- st_bbox(ABR_data)


# Convert sf to data frame with coordinates
points_df <- ABR_data %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(ABR_data)) %>%
  rename(lon = X, lat = Y) %>%
  mutate(r_scaled = sqrt(total) / max(sqrt(total)) * 800)

# Prepare pie chart data
pie_df <- points_df %>%
  mutate(
    X2021 = ifelse(is.na(X2021), 0, X2021),
    X2024 = ifelse(is.na(X2024), 0, X2024)
  ) %>%
  pivot_longer(
    cols = c(X2021, X2024),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = gsub("^X", "", year)) %>%   # remove "X"
  group_by(Site) %>%
  mutate(
    frac = value / sum(value),
    ymax = cumsum(frac),
    ymin = lag(ymax, default = 0)
  ) %>%
  ungroup()

# Plot
ggplot() +
  
  # Lake
  geom_sf(data = Lake_urema, fill = "steelblue3", alpha = 0.6) +
  
  # Roads
  geom_sf(
    data = Roads,
    aes(linetype = "Roads"),
    color = "grey40",
    size = 0.3
  ) +
  
  # Pie charts
  geom_arc_bar(
    data = pie_df,
    aes(
      x0 = lon,
      y0 = lat,
      r0 = 0,
      r = sqrt(total) / max(sqrt(points_df$total)) * 800,
      start = ymin * 2 * pi,
      end = ymax * 2 * pi,
      fill = Habitat,
      alpha = year   
    ),
    color = "black",   
    size = 0.3
  ) +
  
  # add points for legend purposes
  geom_point(
    data = points_df,
    aes(x = lon, y = lat, size = r_scaled),
    alpha = 0   # invisible on map
  ) +
  
  scale_size_continuous(
    name = "Detections",
    breaks = sqrt(c(2, 32, 176)) / max(sqrt(points_df$total)) * 800,
    labels = c(2, 32, 176),
    range = c(2, 10),
    guide = guide_legend(
      order = 3,
      override.aes = list(
        fill = "white",
        color = "white",
        alpha = 1
      )
    )
  ) +
  
  # Zoom extent
  coord_sf(
    xlim = c(bbox["xmin"] - 1000, bbox["xmax"] + 1000),
    ylim = c(bbox["ymin"] - 1000, bbox["ymax"] + 1000),
    expand = FALSE
  ) +
  
  # Habitat colors
  scale_fill_manual(
    name = "Habitat",
    values = c(
      "Open" = "#0A0A52",
      "Closed" = "#A23B2A"
    )
  ) +
  
  # Year transparency
  scale_alpha_manual(
    name = "Year",
    values = c(
      "2021" = 0.5,
      "2024" = 1
    )
  ) +
  
  # Roads legend
  scale_linetype_manual(
    values = c("Roads" = "solid"),
    name = " "
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3
  ) +
  
  # Clean map styling
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  
  # Set legend order
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      override.aes = list(fill = "black", color = "black")
    ),
    linetype = guide_legend(order = 4)
  )

ggsave("figures/ABR_map.png", width = 6, height = 4, dpi = 300)


# Map of Gorongosa National Park in Mozambique ----------------------------

# Match projects 
Africa_utm <- st_transform(Africa, st_crs(GNP_boundary))

# Filter for mozambique
moz <- Africa_utm %>%
  filter(ADMIN == "Mozambique")

# Filter for GNP core zone only
GNP_core <- GNP_boundary_only %>%
  filter(DESIG == "Parque Nacional")

# Plot
ggplot() +
  
  # Africa in UTM
  geom_sf(data = Africa_utm, fill = "#F0F0F0", color = "black") +
  
  #Mozambique in UTM
  geom_sf(data = moz, fill = "#D9D9D9", color = "black") +
  
  # Gorongosa National Park
  geom_sf(data = GNP_core, fill = "#556B2F", color = "black") +

  # Zoom to GNP
  coord_sf(
    xlim = c(bbox["xmin"] - 1000000, bbox["xmax"] + 1000000),
    ylim = c(bbox["ymin"] - 1000000, bbox["ymax"] + 1000000),
    expand = FALSE
  ) +

  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Manually joined and annotated with Google Slides (including fixing detection legend)
# https://docs.google.com/presentation/d/1lzlNSKpoXZXEYsodMYE6I1q24MOwJZqBOJQTCsScZfI/edit
