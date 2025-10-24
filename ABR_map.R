#Script for creating map of Gorongosa National Park and ABR sites

#load packages
library(readxl)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

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
# Get Mozambique boundaries
moz <- ne_countries(country = "Mozambique", scale = "medium", returnclass = "sf")

# Create a data frame for Gorongosa National Park
gorongosa <- data.frame(
  name = "Gorongosa National Park",
  lon = 34.36,
  lat = -18.68
)

# Convert to sf object
gorongosa_sf <- st_as_sf(gorongosa, coords = c("lon", "lat"), crs = 4326)

#plot the map
ggplot() +
  geom_sf(data = moz, fill = "lightgray", color = "black") +
  geom_sf(data = gorongosa_sf, color = "darkred", size = 3) +
  geom_text(
    data = gorongosa,
    aes(x = lon, y = lat, label = "Gorongosa NP"),
    nudge_y = -1, color = "darkred", size = 3
  ) +
  coord_sf(xlim = c(30, 41), ylim = c(-27, -10), expand = FALSE) +
  labs(
    title = "Location of Gorongosa National Park within Mozambique",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()
