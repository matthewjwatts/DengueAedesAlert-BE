# Render the current date
output$current_date <- renderText({
  paste("Current Date:", format(Sys.Date(), "%Y-%m-%d"))
})

# Reactive expression to filter Aedes data for the current year
current_year_aedes <- reactive({
  aedes_data[aedes_data$Detection_date >= as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), ]
})

# Reactive expression to calculate centroids of another dataset (assuming dengue_data for this example)
current_period_centroids <- reactive({
  # Calculate the start date based on the selected weeks before
  start_date <- Sys.Date() - as.numeric(input$weeks_before) * 7
  end_date <- Sys.Date()
  
  # Filter the dengue_data for the selected period
  dengue_filtered <- dengue_data[dengue_data$Report_date >= start_date & 
                                   dengue_data$Report_date <= end_date, ]
  
  # Join with postaldistricts to get the spatial data
  joined_dengue <- merge(dengue_filtered, postaldistricts, by = "Postcode")
  
  # Convert to sf and calculate centroids
  st_centroid(st_as_sf(joined_dengue))
})

# Reactive expression to filter observations for the selected weeks before current date
filtered_aedes <- reactive({
  start_date <- Sys.Date() - as.numeric(input$weeks_before) * 7
  end_date <- Sys.Date()
  
  aedes_filtered <- aedes_data[aedes_data$Detection_date >= start_date & 
                                 aedes_data$Detection_date <= end_date, ]
  
  # Join with postaldistricts to get the spatial data
  joined_aedes <- merge(aedes_filtered, postaldistricts, by = "Postcode")
  
  st_as_sf(joined_aedes)
})

# Render the Leaflet map
output$realtime_map <- renderLeaflet({
  # Get the full year data and filtered data
  aedes_sf <- st_as_sf(merge(current_year_aedes(), postaldistricts, by = "Postcode"))
  filtered_aedes_sf <- filtered_aedes()
  dengue_centroids_sf <- current_period_centroids()
  
  # Extract coordinates
  aedes_coords <- st_coordinates(filtered_aedes_sf)
  dengue_coords <- st_coordinates(dengue_centroids_sf)
  
  # Create the leaflet map
  leaflet() %>%
    addTiles() %>%
    # Add polygons for Aedes distribution
    addPolygons(data = aedes_sf, fillColor = "red", weight = 2, opacity = 1, fillOpacity = 0.7,
                color = "black", group = "Aedes Distribution") %>%
    # Add points for filtered Aedes observations
    addCircleMarkers(lng = aedes_coords[,1], lat = aedes_coords[,2], radius = 5, 
                     fillColor = "green", fillOpacity = 1, color = "white", 
                     weight = 2, group = "Filtered Aedes Observations") %>%
    # Add centroids for another dataset (e.g., dengue_data)
    addCircleMarkers(lng = dengue_coords[,1], lat = dengue_coords[,2], radius = 5, 
                     fillColor = "blue", fillOpacity = 1, color = "white", 
                     weight = 2, group = "Dengue Centroids") %>%
    # Add layer control
    addLayersControl(
      overlayGroups = c("Aedes Distribution", "Filtered Aedes Observations", "Dengue Centroids"),
      options = layersControlOptions(collapsed = FALSE)
    )
})