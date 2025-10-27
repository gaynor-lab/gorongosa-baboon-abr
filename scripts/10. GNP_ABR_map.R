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


