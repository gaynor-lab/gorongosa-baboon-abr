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

install.packages("geojsonio")
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

#Map 2 - with shapefiles

#install packages
install.packages(c("sf", "ggplot2", "dplyr"))
library(sf)
library(ggplot2)
library(dplyr)

#load shapefiles
#Floodplain <- st_read("data/GNP_shp/Floodplain_only.shp")
GNP_boundary <- st_read("data/GNP_shp/GNP_Landscapes_New_boundary.shp")
Rivers <- st_read("data/GNP_shp/gp_permanent_rivers_utm.shp")
#Grasslands <- st_read("data/GNP_shp/Grassland_only.shp")
Lake_urema <- st_read("data/GNP_shp/LakeUrema.shp")
Roads <- st_read("data/GNP_shp/roads_may2015.shp")

#visualize - delete later
plot(st_geometry(Floodplain))
plot(st_geometry(GNP_boundary))
plot(st_geometry(Rivers))
plot(st_geometry(Grasslands))
plot(st_geometry(Lake_urema))

#Match projections
# Use GNP_boundary as reference CRS
crs_ref <- st_crs(GNP_boundary)

Floodplain  <- st_transform(Floodplain, crs_ref)
Rivers      <- st_transform(Rivers, crs_ref)
st_crs(Grasslands) <- st_crs(GNP_boundary)
Lake_urema  <- st_transform(Lake_urema, crs_ref)
Roads       <- st_transform(Roads, crs_ref)


#plotting first attempt - CRS is off
ggplot() +
  
  # Landscape boundary
  geom_sf(data = GNP_boundary, fill = NA, color = "black", linewidth = 0.6) +
  
  # Floodplain
  geom_sf(data = Floodplain, fill = "lightblue", color = NA, alpha = 0.5) +
  
  # Grasslands
  geom_sf(data = Grasslands, fill = "tan", color = NA, alpha = 0.5) +
  
  # Lake Urema
  geom_sf(data = Lake_urema, fill = "blue", color = NA, alpha = 0.7) +
  
  # Rivers
  geom_sf(data = Rivers, color = "steelblue", linewidth = 0.4) +
  
  # Roads
  geom_sf(data = Roads, color = "black", linewidth = 0.6) +
  
  # Theme
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  )



