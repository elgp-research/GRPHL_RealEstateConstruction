##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sf)
library(waffle)
library(sfheaders)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--5a. GIS Data: Map of Wages by Metropolitan Regions in the U.S.--------------

# turning data frame into an 'sf' object for the leaflet package
GIS_data_sf <- st_as_sf(GIS_data, wkt = "geometry")

# Transform the data to the WGS84 SRS to remove the warning message from R
GIS_data_sf_transformed <- st_transform(GIS_data_sf, "+proj=longlat +datum=WGS84")

# Create color palettes for each variable
pal2 <- colorNumeric(palette = "YlOrRd", domain = GIS_data$H_MEDIAN)
pal4 <- colorNumeric(palette = "RdPu", domain = GIS_data$A_MEDIAN)

##--5c. GIS Data: Median Hourly Wage------------------------------------------------------
map <- leaflet(GIS_data_sf_transformed) %>%
  addTiles() %>%
  addPolygons(
    group = "H_MEDIAN",
    fillColor = ~pal2(H_MEDIAN),
    fillOpacity = 0.7,
    color = "#444444",
    weight = 1,
    popup = ~paste("Region: ", NAME, "<br>",
                   "Median Hourly Wage: $", round(H_MEDIAN,2))
  )
# Set the initial view of the map to the center of the United States
map <- map %>% setView(
  lng = -98.583333,
  lat = 39.833333,
  zoom = 4
) %>% addLegend(
  pal = pal2,
  values = ~H_MEDIAN,
  position = "bottomright",
  title = "Median Hourly Wage"
)
# Print map
map

##--5e. GIS Data: Median Annual Wage------------------------------------------------------
map <- leaflet(GIS_data_sf_transformed) %>%
  addTiles() %>%
  addPolygons(
    group = "A_MEDIAN",
    fillColor = ~pal4(A_MEDIAN),
    fillOpacity = 0.7,
    color = "#444444",
    weight = 1,
    popup = ~paste("Region: ", NAME, "<br>",
                   "Annual Median Wage: $", round(A_MEDIAN,2))
  )
# Set the initial view of the map to the center of the United States
map <- map %>% setView(
  lng = -98.583333,
  lat = 39.833333,
  zoom = 4
) %>% addLegend(
  pal = pal4,
  values = ~A_MEDIAN,
  position = "bottomright",
  title = "Median Annual Wage"
)
# Print map
map
