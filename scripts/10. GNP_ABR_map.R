#Script for creating map of Gorongosa National Park and ABR sites

#load packages
library(readxl)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(wdpar)
library(ggspatial)

#read in data here
ABR_data <- read.csv("data/ABR_locations.csv")

#Plot ABR sites
leaflet(ABR_data) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(
    lng = ~Long,
    lat = ~Lat,
    fillColor = ~ifelse(Habitat == "Open", "#DAA520", "#228B22"),  # light blue & forest green
    color = "black",
    fillOpacity = 0.8,
    weight = 1,
    radius = ~sqrt(total),  # scale marker size by number of videos
    stroke = TRUE
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#DAA520", "#228B22"),
    labels = c("Open", "Closed"),
    title = "Habitat Type",
    opacity = 0.8
  ) %>%
  addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE)) %>%
  addControl(
    html = "<div style='font-size:16px; font-weight:bold;'>↑<br>N</div>",
    position = "topright"
  )

#Plot map of Gorongosa National Park

# Create a clean directory for the shapefile download
temp_dir <- "C:/Users/sophi/Downloads/wdpa_temp"
dir.create(temp_dir, showWarnings = FALSE)

# Fetch Mozambique protected areas
moz <- wdpa_fetch("Mozambique", download_dir = temp_dir, wait = TRUE)

#Filter to just Gorongosa data
gorongosa <- moz[grepl("Gorongosa", moz$NAME, ignore.case = TRUE), ]

#Create leaflet map and overlay park outline
leaflet() %>%
  addProviderTiles("USGS.USTopo") %>%
  addPolygons(
    data = gorongosa,
    color = "darkgreen",  
    weight = 2,               
    opacity = 0.9,
    label = ~NAME
  ) %>%
  addScaleBar(position = "bottomright") %>%
  addControl(
    html = "<div style='font-size:16px; font-weight:bold;'>↑<br>N</div>",
    position = "topright"
  )

#Map of camera trap sites - with shapefiles

#install packages
library(sf)
library(ggplot2)
library(dplyr)

#load shapefiles
#Floodplain <- st_read("data/GNP_shp/Floodplain_only.shp")
GNP_boundary <- st_read("data/GNP_shp/GNP_Landscapes_New_boundary.shp")
Rivers <- st_read("data/GNP_shp/gp_permanent_rivers_utm.shp")
Grasslands <- st_read("data/GNP_shp/Grassland_only.shp")
Lake_urema <- st_read("data/GNP_shp/LakeUrema.shp")
Roads <- st_read("data/GNP_shp/roads_may2015.shp")
Africa <- st_read("data/GNP_shp/ne_10m_admin_0_countries.shp") %>%
  filter(REGION_UN == "Africa")

#load ABR data
ABR_data <- read.csv("data/ABR_locations.csv")

# Convert to sf object 
ABR_data <- st_as_sf(
  ABR_data,
  coords = c("Long", "Lat"),   
  crs = 4326                   
)
bbox <- st_bbox(ABR_data)

#Match projections
# Use GNP_boundary as reference CRS
crs_ref <- st_crs(GNP_boundary)

Floodplain  <- st_transform(Floodplain, crs_ref)
Rivers      <- st_transform(Rivers, crs_ref)
st_crs(Grasslands) <- 4326
Lake_urema  <- st_transform(Lake_urema, crs_ref)
Roads       <- st_transform(Roads, crs_ref)
ABR_data <- st_transform(ABR_data, st_crs(GNP_boundary)) 


#plot
ggplot() +
  
  # Park background
  geom_sf(data = GNP_boundary, fill = "#A3B18A", color = "black") +
  
  # Lake Urema
  geom_sf(data = Lake_urema, fill = "steelblue3", alpha = 0.6) +
  
  # Roads
  geom_sf(data = Roads, color = "grey40", size = 0.3) +
  
  # Camera trap points
  geom_sf(
    data = ABR_data,
    aes(color = Habitat, size = total),
    alpha = 0.9
  ) +
  
  # Zoom window (with buffer)
  coord_sf(
    xlim = c(bbox["xmin"] - 700, bbox["xmax"] + 700),
    ylim = c(bbox["ymin"] - 700, bbox["ymax"] + 700),
    expand = FALSE
  ) +
  
  # Habitats
  scale_color_manual(
    values = c(
      "Open" = "#0A0A52",   # Navy
      "Closed" = "#A23B2A"  # Rusty red
    ),
    name = "Habitat"
  ) +
  
  # Video count sizes
  scale_size_continuous(
    range = c(2, 10),
    name = "Total Videos"
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Remove axes + grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"   # REMOVE legend
  )

#Map of Goronogosa National Park in Mozambique
Africa_utm <- st_transform(Africa, st_crs(GNP_boundary))
moz <- Africa_utm %>%
  filter(ADMIN == "Mozambique")
st_crs(Africa)
st_crs(GNP_boundary)


ggplot() +
  
  # Africa in UTM
  geom_sf(data = Africa_utm, fill = "#F0F0F0", color = "black") +
  
  #Mozambique in UTM
  geom_sf(data = moz, fill = "#D9D9D9", color = "black") +
  
  # Gorongosa National Park
  geom_sf(data = GNP_boundary, fill = "#556B2F", color = "black") +
  
  # Zoom to GNP
  coord_sf(
    xlim = c(bbox["xmin"] - 1000000, bbox["xmax"] + 1000000),
    ylim = c(bbox["ymin"] - 1000000, bbox["ymax"] + 1000000),
    expand = FALSE
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +

  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

