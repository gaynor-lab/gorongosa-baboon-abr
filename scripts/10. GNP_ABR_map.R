#Script for creating map of Gorongosa National Park and ABR sites

#load packages
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)

#Map of ABR sites 

#load shapefiles
GNP_boundary <- st_read("data/GNP_shp/GNP_Landscapes_New_boundary.shp")
Lake_urema <- st_read("data/GNP_shp/LakeUrema.shp")
Roads <- st_read("data/GNP_shp/roads_may2015.shp")
Africa <- st_read("data/GNP_shp/ne_10m_admin_0_countries.shp") %>%
  filter(REGION_UN == "Africa")
GNP_boundary_only <- st_read("data/GNP_shp/WDPA_WDOECM_Nov2025_Public_801_shp-polygons.shp")

#load ABR data
ABR_data <- read.csv("data/ABR_locations.csv")

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

#create bounding box
bbox <- st_bbox(ABR_data)

#plot
ggplot() +
  
  # Park background
  geom_sf(data = GNP_boundary_only, fill = "#A3B18A", color = "black") +
  
  # Lake Urema
  geom_sf(data = Lake_urema, fill = "steelblue3", alpha = 0.6) +
  
  # Roads
  geom_sf(data = Roads, aes(linetype = "Roads"), color = "grey40", size = 0.3) +
  
  # Camera trap points
  geom_sf(
    data = ABR_data,
    aes(color = Habitat, size = total),
    alpha = 0.9
  ) +
  
  # Zoom window (with buffer)
  coord_sf(
    xlim = c(bbox["xmin"] - 1000, bbox["xmax"] + 1000),
    ylim = c(bbox["ymin"] - 1000, bbox["ymax"] + 1000),
    expand = FALSE
  ) +
  
  # Habitats
  scale_color_manual(
    values = c(
      "Open" = "#0A0A52",   
      "Closed" = "#A23B2A"
    ),
    name = "Habitat"
  ) +
  
  # Roads legend using linetype
  scale_linetype_manual(
    values = c("Roads" = "solid"),
    name = " "
  ) +
  
  # Video count sizes
  scale_size_continuous(
    range = c(2, 10),
    name = "Baboon Observations",
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3
  ) +
  
  # Remove axes + grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
  )

#Map of Goronogosa National Park in Mozambique

#Match projects 
Africa_utm <- st_transform(Africa, st_crs(GNP_boundary))

#filter for mozambique
moz <- Africa_utm %>%
  filter(ADMIN == "Mozambique")

#filter for gnp core zone only
GNP_core <- GNP_boundary_only %>%
  filter(DESIG == "Parque Nacional")

#plot
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


