library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.providers)
library(DT)
library(sf)
library(shinyalert)
library(tidyverse)
library(leaflet.extras)
library(markdown)

#################################################################################################################
# Ensure data / data frames are created / loaded if not in environment
#################################################################################################################


# Global variables to store the data frames
if (!exists("aedes_data")) {
  aedes_data <<- data.frame(
    Observation_ID = character(),
    Site_codes = character(),
    Municipality = character(),
    Postcode = numeric(),
    Latitude = numeric(),
    Longitude = numeric(),
    Site_type = character(),
    Status = character(),
    Detection_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

if (!exists("dengue_data")) {
  dengue_data <<- data.frame(
    Sample_ID = character(),
    Report_date  = as.Date(character()),
    Source_country = character(),
    Postcode = numeric(),
    stringsAsFactors = FALSE
  )
}


# Check if postaldistricts exists, if not, load it
if (!exists("postaldistricts")) {
  postaldistricts <<- st_read("data/postaldistricts.geojson")
}

# Check if postaldistricts exists, if not, load it
if (!exists("valid_postcodes")) {
  valid_postcodes <<-  unique(postaldistricts$Postcode)
}

#################################################################################################################
# Web layout etc
#################################################################################################################


ui <- dashboardPage(
  dashboardHeader(title = "DENVAedes-Alert-BE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About"),
      menuItem("Realtime risk", tabName = "RealtimeRisk"),
      menuItem("Historical risk", tabName = "HistoricalRisk"),
      menuItem("Cumulative DF cases (5Yr)", tabName = "CumulativeDengue"), # Counts 0 to 5, 5th year will be incomplete
      menuItem("Cumulative Aedes detections (5Yr)", tabName = "CumulativeAedes"),  # Counts 0 to 5, 5th year will be incomplete
      menuItem("Data management", tabName = "DataManagement")
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      tabItem(tabName = "About",
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "github-markdown.css")
              ),
              div(class = "markdown-body", includeMarkdown("README.md"))
      ),
      tabItem(tabName = "RealtimeRisk",
              h1("Realtime Overlap Analysis"),
              br(),
              h3(textOutput("current_date_display")),
              # Conditional Panel for Warning Message
              conditionalPanel(
                condition = "output.show_warning_message == true",  # Reactively show warning message
                div(style = "color: red; font-weight: bold;",
                    textOutput("warning_message")  # Display detailed warning message
                )
              ),
              # Place the slider input in the main page content
              sliderInput("weeks_before", 
                          "Select DF case range (weeks before current date):",
                          min = 1, max = 4, value = 1, step = 1),
              leafletOutput("realtime_map", height = "600px"),
              br(),
              h2("Aedes-dengue overlaps"),
              br(),
              DTOutput("aedes_dengue_table")  # Remove the comma here
                # Add this line
      ),  # Add the closing parenthesis for the tabItem
      tabItem(tabName = "HistoricalRisk",
              h1("Historical Overlap Analysis"),
              br(),
              # Year selection dropdown
              selectInput("year_select", "Select Year:", choices = NULL),
              
              # Month range slider with corrected animation
              sliderInput("month_range", "Select DF Report Month Range:",
                          min = 1, max = 12, value = c(1, 12), step = 1,
                          ticks = TRUE, animate = TRUE),  # Corrected animation option
              
              leafletOutput("historical_map", height = "600px"),
              br(),
              h2("Historical Aedes-dengue Overlaps"),
              br(),
              DTOutput("historical_aedes_dengue_table")
      ),
      tabItem(tabName = "CumulativeDengue",
              h1("Cumulative DF Cases (5Yr)"),
              leafletOutput("cumulative_dengue_map", height = "600px"),
              br(),
              h2("Cumulative DF Cases Summary Table (5Yr)"),
              DTOutput("cumulative_dengue_table")
      ), 
      tabItem(tabName = "CumulativeAedes",  # New tab content for Aedes data
              h1("Cumulative Aedes Detections (5Yr)"),
              leafletOutput("cumulative_aedes_map", height = "600px"),  # Leaflet map output
              br(),
              h2("Cumulative Aedes Detections (5Yr) Summary Table"),
              DTOutput("cumulative_aedes_table")  # Data table output
      ),
      tabItem(tabName = "DataManagement",
              h1("Upload data"),
              selectInput("dataset_select", "Select Dataset to Upload:",
                          choices = list("Aedes data" = "aedes", "Dengue data" = "dengue"),
                          selected = "aedes"),
              fileInput("file1", "Choose CSV File", accept = ".csv"),
              uiOutput("verification_panel"),
              verbatimTextOutput("message"),
              h1("Delete data"),
              br(),
              actionButton("delete_all_data", "Delete All Data")
      )
    )
  )
)

#################################################################################################################
# Back-end (server) functions
#################################################################################################################


server <- function(input, output, session) {

  
  #################################################################################################################
  # Check if data exists functions 
  #################################################################################################################
  
  
  file_data <- reactiveVal(NULL)
  show_verification <- reactiveVal(FALSE)
  
  # Create a reactive timer to poll the global data
  autoInvalidate <- reactiveTimer(2000) # Check every 2 seconds
  
  # Use reactiveValues to hold the data and trigger reactivity
  data_values <- reactiveValues(
    aedes_data = aedes_data,
    dengue_data = dengue_data
  )
  
  # Observe global data changes and update reactive values
  observe({
    autoInvalidate()
    data_values$aedes_data <- aedes_data
    data_values$dengue_data <- dengue_data
  })
  
  
  # Reactive expression to check if any dataset is missing or empty and specify which ones
  dataset_warning <- reactive({
    warnings <- c()
    
    if (nrow(data_values$aedes_data) == 0) {
      warnings <- c(warnings, "Aedes data is missing or empty.")
    }
    
    if (nrow(data_values$dengue_data) == 0) {
      warnings <- c(warnings, "Dengue data is missing or empty.")
    }
    
    if (is.null(postaldistricts) || nrow(postaldistricts) == 0) {
      warnings <- c(warnings, "Postal Districts data is missing or empty.")
    }
    
    if (length(warnings) > 0) {
      return(paste(warnings, collapse = "\n"))
    } else {
      return(NULL)
    }
  })
  
  # Observe and display a detailed warning message in the RealtimeRisk tab
  observe({
    warning_message <- dataset_warning()
    
    if (!is.null(warning_message)) {
      shinyalert(
        title = "Data Warning",
        text = warning_message,
        type = "warning",
        timer = 5000, # Optional: Auto close the alert after 5 seconds
        showConfirmButton = TRUE
      )
    }
  })
  
  
  #################################################################################################################
  # Real-time Risk (overlaps) functions
  #################################################################################################################
  
  
  current_date <- reactive({
    current_date <- Sys.Date()
    as.Date(current_date, format = "%Y-%m-%d")
  })
  
  
  # Render the current date for display
  output$current_date_display <- renderText({
    paste("Date today:", current_date())
  })
  
  # Reactive expression to calculate start_date based on slider input
  start_date <- reactive({
    current_date() - (input$weeks_before * 7)  # Changed to multiply by 7 for weeks
  })
  
  aedes_data_filtered <- reactive({
    current_year <- 2023
    data_values$aedes_data %>%
      mutate(Detection_date = as.Date(Detection_date, format = "%Y-%m-%d")) %>%
      filter(format(Detection_date, "%Y") == current_year)
  })
  
  dengue_data_filtered <- reactive({
    data_values$dengue_data %>%
      mutate(Report_date = as.Date(Report_date, format = "%Y-%m-%d")) %>%
      filter(Report_date >= start_date() & Report_date <= current_date())
  })
  
  
  # Reactive expression to join the filtered dataframes
  aedes_dengue_matches <- reactive({
    inner_join(aedes_data_filtered(), dengue_data_filtered(), by = 'Postcode')
  })
  
  # Reactive expression to join the filtered dataframes
  aedes_dengue_non_matches <- reactive({
    anti_join(aedes_data_filtered(), dengue_data_filtered(), by = 'Postcode')
  })
  
  # plot aedes postive sites with no overlap - Reactive expression to create an SF object for plotting
  aedes_sf <- reactive({
    # Join the matches with postaldistricts to get spatial geometry
    sf_data <- inner_join(postaldistricts, aedes_dengue_non_matches(), by = 'Postcode')
    return(sf_data)
  })
  
  
  # plot aedes postive sites Reactive expression to create an SF object for plotting
  aedes_dengue_sf <- reactive({
    # Join the matches with postaldistricts to get spatial geometry
    sf_data <- inner_join(postaldistricts, aedes_dengue_matches(), by = 'Postcode')
    return(sf_data)
  })
  
  
  # Calculate the number of cases per postcode
  dengue_case_count <- reactive({dengue_data_filtered() %>%
    group_by(Postcode) %>%
    summarize(case_count = n(), .groups = 'drop')})
  
  
  dengue_case_counts <-  reactive({
    # Join the matches with postaldistricts to get spatial geometry
    sf_data <- inner_join(postaldistricts, dengue_case_count(), by = 'Postcode')
    return(sf_data)
  })
  
  # Convert polygon to point using point on surface
  dengue_case_count_centroids <- reactive({
    sf_centroids <- st_centroid(dengue_case_counts())
    return(sf_centroids)})
  
  
  # Render the Leaflet map
  output$realtime_map <- renderLeaflet({
    req(aedes_dengue_sf(), aedes_sf(), dengue_case_count_centroids())
    
    # Define the initial view parameters
    initial_lat <- 50.50   # Latitude of the center of Belgium
    initial_lng <- 4.35    # Longitude of the center of Belgium
    initial_zoom <- 7.5      # Appropriate zoom level for Belgium
    
    custom_icons <- icons(
      iconUrl = "www/icons/68004_location_pin_icon.png",  # Use your image URL
      iconWidth = 10,  # Width of the icon
      iconHeight = 10  # Height of the icon
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add polygons with matching cases
      addPolygons(
        data = aedes_dengue_sf(), 
        color = "red", weight = 2, fillOpacity = 0.5, 
        group = "Matches",
        popup = ~paste("Municipality: ", Municipality, "; Post Code: ", Postcode)
      ) %>%
      
      # Add polygons without matching cases
      addPolygons(
        data = aedes_sf(), 
        color = "grey", weight = 2, fillOpacity = 0.5, 
        group = "No-overlap",
        popup = ~paste("Municipality: ", Municipality, "; Post Code: ", Postcode)
      ) %>%
      
      # Set the initial view
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      
      addMarkers(
        data = dengue_case_count_centroids(), 
        icon = custom_icons, 
        popup = ~paste("Post Code: ", Postcode, "; N.cases: ", case_count)
      ) %>%
      
      # Add legend to distinguish between the two categories of polygons
      addLegend(
        position = "bottomright", 
        colors = c("red", "grey"),   # Colors for the legend
        labels = c("Overlap with dengue case/s", "No-overlap with dengue case/s"),  # Corresponding labels
        title = "Aedes positive postcodes",
        opacity = 0.5
      ) %>%
      
      # Add a second custom legend for the marker icons
      addControl(
        html = "
        <div style='background: white; padding: 10px;'>
          <div style='display: flex; align-items: center;'>
            <img src='icons/68004_location_pin_icon.png' width='25' height='25' style='margin-right: 5px;'> 
            Dengue Cases
          </div>
        </div>", 
        position = "bottomleft"
      ) %>%   # Added the missing %>% here
      
      # Add the reset zoom button
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset View",
        onClick = JS(paste0("function(btn, map){ map.setView([", initial_lat, ", ", initial_lng, "], ", initial_zoom, "); }"))
      ))
  })
  

  # Reactive expression to filter and rename columns
  aedes_dengue_matches_table <- reactive({
    aedes_dengue_matches() %>%
      dplyr::select(-Latitude, -Longitude, -Observation_ID) %>%
      dplyr::rename(
        Aedes_site_codes = Site_codes, 
        Aedes_site_type = Site_type, 
        Aedes_detection_date = Detection_date,
        Aedes_Status = Status,
        DF_case_sample_ID = Sample_ID, 
        DF_case_report_date = Report_date,
        DF_case_source_country = Source_country
      )
  })
  
  # Render the aedes_dengue_matches dataframe in the DTOutput
  output$aedes_dengue_table <- renderDT({
    req(aedes_dengue_matches_table())  # Make sure the reactive expression is evaluated inside a reactive context
    datatable(
      aedes_dengue_matches_table(),
      options = list(pageLength = 50)
    )
  })
  
  
  
  #################################################################################################################
  # Historical overlaps function
  #################################################################################################################

  # Reactive expression to get common years between aedes_data and dengue_data
  common_years <- reactive({
    # Convert Detection_date and Report_date to Date type, handle NA values
    aedes_years <- data_values$aedes_data %>%
      mutate(Detection_date = as.Date(Detection_date, format = "%Y-%m-%d")) %>%
      filter(!is.na(Detection_date)) %>%  # Remove NA dates
      pull(Detection_date) %>%
      format("%Y") %>%
      unique() %>%
      as.numeric()
    
    dengue_years <- data_values$dengue_data %>%
      mutate(Report_date = as.Date(Report_date, format = "%Y-%m-%d")) %>%
      filter(!is.na(Report_date)) %>%  # Remove NA dates
      pull(Report_date) %>%
      format("%Y") %>%
      unique() %>%
      as.numeric()
    
    # Return common years in sorted order
    intersect(aedes_years, dengue_years) %>% sort()
  })
  
  
  # Update year_select dropdown with common years in reverse order and set a default
  observe({
    updateSelectInput(session, "year_select", 
                      choices = rev(common_years()), # Reverse the choices
                      selected = tail(common_years(), 1)) # Set default as the most recent year
  })
  
  

  
  
  # Reactive expression to filter aedes_data based on selected year
  aedes_data_filtered_historical <- reactive({
    req(input$year_select)
    
    data_values$aedes_data %>%
      mutate(Detection_date = as.Date(Detection_date, format = "%Y-%m-%d")) %>%
      filter(format(Detection_date, "%Y") == input$year_select)
  })
  
  # Reactive expression to filter dengue_data based on selected year and month range
  dengue_data_filtered_historical <- reactive({
    req(input$year_select, input$month_range)
    
    start_month <- input$month_range[1]
    end_month <- input$month_range[2]
    
    # Convert month range to a list of month numbers to include
    if (start_month <= end_month) {
      months_selected <- start_month:end_month
    } else {
      months_selected <- c(start_month:12, 1:end_month)  # Wrap around from December to January
    }
    
    data_values$dengue_data %>%
      mutate(Report_date = as.Date(Report_date, format = "%Y-%m-%d"),
             Report_month = as.numeric(format(Report_date, "%m"))) %>%
      filter(format(Report_date, "%Y") == input$year_select &
               Report_month %in% months_selected)
  })
  
  # Reactive expression to join the filtered dataframes for historical data
  aedes_dengue_matches_historical <- reactive({
    inner_join(aedes_data_filtered_historical(), dengue_data_filtered_historical(), by = 'Postcode')
  })
  
  # Reactive expression to create an SF object for plotting historical data
  aedes_dengue_non_matches_historical <- reactive({
    anti_join(aedes_data_filtered_historical(), dengue_data_filtered_historical(), by = 'Postcode')
  })
  
  
  
  
  
  # plot aedes postive sites with no overlap - Reactive expression to create an SF object for plotting
  aedes_historical_sf <- reactive({
    # Join the matches with postaldistricts to get spatial geometry
    sf_data <- inner_join(postaldistricts, aedes_dengue_non_matches_historical(), by = 'Postcode')
    return(sf_data)
  })
  
  
  # plot aedes postive sites Reactive expression to create an SF object for plotting
  aedes_dengue_matches_historical_sf <- reactive({
    # Join the matches with postaldistricts to get spatial geometry
    sf_data <- inner_join(postaldistricts, aedes_dengue_matches_historical(), by = 'Postcode')
    return(sf_data)
  })
  
  
  # Calculate the number of cases per postcode
  dengue_case_count_historical <- reactive({dengue_data_filtered_historical() %>%
      group_by(Postcode) %>%
      summarize(case_count = n(), .groups = 'drop')})
  
  
  dengue_case_count_historical_sf <-  reactive({
    # Join the matches with postaldistricts to get spatial geometry
    sf_data <- inner_join(postaldistricts, dengue_case_count_historical(), by = 'Postcode')
    return(sf_data)
  })
  
  # Convert polygon to point using point on surface
  dengue_case_count_historical_centroids <- reactive({
    sf_centroids <- st_centroid(dengue_case_count_historical_sf())
    return(sf_centroids)})
  
  
  
  
  
  # Render the Leaflet map for historical risk analysis
  output$historical_map <- renderLeaflet({
    req(aedes_dengue_matches_historical_sf(), aedes_historical_sf(), dengue_case_count_historical_centroids())
    
    initial_lat <- 50.50  # Latitude of the center of Belgium
    initial_lng <- 4.35   # Longitude of the center of Belgium
    initial_zoom <- 7.5   # Appropriate zoom level for Belgium
    
    custom_icons <- icons(
      iconUrl = "www/icons/68004_location_pin_icon.png",  # Use your image URL
      iconWidth = 10,  # Width of the icon
      iconHeight = 10  # Height of the icon
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add polygons with matching cases
      addPolygons(
        data = aedes_dengue_matches_historical_sf(), 
        color = "red", weight = 2, fillOpacity = 0.5, 
        group = "Matches",
        popup = ~paste("Municipality: ", Municipality, "; Post Code: ", Postcode)
      ) %>%
      
      # Add polygons without matching cases
      addPolygons(
        data = aedes_historical_sf(), 
        color = "grey", weight = 2, fillOpacity = 0.5, 
        group = "No-overlap",
        popup = ~paste("Municipality: ", Municipality, "; Post Code: ", Postcode)
      ) %>%
      
      # Set the initial view
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      
      addMarkers(
        data = dengue_case_count_historical_centroids(), 
        icon = custom_icons, 
        popup = ~paste("Post Code: ", Postcode, "; N.cases: ", case_count)
      ) %>%
      
      # Add legend to distinguish between the two categories of polygons
      addLegend(
        position = "bottomright", 
        colors = c("red", "grey"),   
        labels = c("Overlap with dengue case/s", "No-overlap with dengue case/s"), 
        title = "Aedes positive postcodes",
        opacity = 0.5
      ) %>%
      
      # Add a second custom legend for the marker icons
      addControl(
        html = "
      <div style='background: white; padding: 10px;'>
        <div style='display: flex; align-items: center;'>
          <img src='icons/68004_location_pin_icon.png' width='25' height='25' style='margin-right: 5px;'> 
          Dengue Cases
        </div>
      </div>", 
        position = "bottomleft"
      ) %>%
      
      # Add the reset zoom button
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset View",
        onClick = JS(paste0("function(btn, map){ map.setView([", initial_lat, ", ", initial_lng, "], ", initial_zoom, "); }"))
      ))
  })
  
  # Render the historical aedes_dengue_matches dataframe in the DTOutput
  output$historical_aedes_dengue_table <- renderDT({
    req(aedes_dengue_matches_historical())
    
    datatable(
      aedes_dengue_matches_historical() %>%
        select(-Latitude, -Longitude, -Observation_ID, -Report_month) %>%
        rename(
          Aedes_site_codes = Site_codes, 
          Aedes_site_type = Site_type, 
          Aedes_detection_date = Detection_date,
          Aedes_Status = Status,
          DF_case_sample_ID = Sample_ID, 
          DF_case_report_date = Report_date,
          DF_case_source_country = Source_country
        ),
      options = list(pageLength = 50)
    )
  })
  
  
  #################################################################################################################
  # Cumulative dengue cases over the last 5 years: - working with gradation of colours to indicate number of cases
  #################################################################################################################
  
  # Define a reactive expression to calculate cumulative dengue cases
  postcode_dengue_counts_5y <- reactive({
    # Define the target year as the current year
    target_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # Calculate the range of years
    start_year <- target_year - 4  # Last 5 years including the target year
    
    # Convert the 'Report_date' column to Date format
    dengue_data_cleaned <- data_values$dengue_data %>%
      mutate(Report_date = as.Date(Report_date, format = "%Y-%m-%d")) %>%
      filter(!is.na(Report_date)) # Remove NA dates
    
    # Filter dengue data for the last 5 years
    dengue_data_5y <- dengue_data_cleaned %>%
      mutate(dengue_year = as.numeric(format(Report_date, "%Y"))) %>%
      filter(dengue_year >= start_year & dengue_year <= target_year)
    
    # Counting observations per postcode over the last 5 years
    postcode_dengue_counts_5y <- dengue_data_5y %>%
      group_by(Postcode) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Create bins for categorizing dengue counts
    postcode_dengue_counts_5y$bins <- 
      cut(postcode_dengue_counts_5y$count,
          breaks = c(0, 2, 5, 10, 20, 30, 50, Inf),
          labels = c("1-2", "3-5", "6-10", "11-20", "21-30", "31-50", ">50"),
          right = FALSE)
    
    return(postcode_dengue_counts_5y)
  })
  
  # Reactive expression to join the cumulative counts with the postal district geometries
  postcode_dengue_counts_5y_sf <- reactive({
    # Join with spatial data (postaldistricts) to get the geometry for mapping
    sf_data <- inner_join(postaldistricts, postcode_dengue_counts_5y(), by = 'Postcode')
    return(sf_data)
  })
  
  # Create a color palette based on the bins within the reactive context
  output$cumulative_dengue_map <- renderLeaflet({
    # Access the reactive data within the renderLeaflet context
    sf_data <- postcode_dengue_counts_5y_sf()
    
    # Check if the data is available
    req(nrow(sf_data) > 0)
    
    # Create a color palette based on the bins
    pal <- colorFactor(palette = "YlOrRd", domain = sf_data$bins)
    
    # Create and render the leaflet map for cumulative dengue cases over the last 5 years
    leaflet(sf_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(bins),
        fillOpacity = 0.7,
        color = "#BDBDC3",
        weight = 1,
        popup = ~paste("Post Code:", Postcode, "; DF cases (5Yr):", count)
      ) %>%
      addLegend(
        pal = pal,
        values = ~bins,
        opacity = 0.7,
        title = "DF cases (5Yr)",
        position = "bottomright"
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset View",
        onClick = JS(paste0("function(btn, map){ map.setView([", 50.50, ", ", 4.35, "], ", 7.5, "); }"))
      ))
  })
  
  # Show the data table for the cumulative dengue cases
  output$cumulative_dengue_table <- renderDT({
    # Access the reactive data within the renderDT context
    sf_data <- postcode_dengue_counts_5y() # Correct reactive value
    
    # Check if the data is available
    req(nrow(sf_data) > 0)
    
    # Render the datatable
    datatable(
      sf_data %>%
        dplyr::select(Postcode, count) %>%
        dplyr::rename(
          "Post Code" = Postcode,
          "DF Cases (5Yr)" = count
        ),
      options = list(pageLength = 50)
    )
  })
  

  
  #######################################################################################################################
  # Cumulative Aedes Data over the last 5 years: - counting the total number of observations a postcode was positive 
  # Note since a whole 5y will be possible, it include 5 years back from current date not inlcuding current year, however current year will be included
  ########################################################################################################################
  
  # Reactive expression to calculate cumulative Aedes data over the last 5 years
  postcode_aedes_positive_5y <- reactive({
    # Define the target year as the current year
    target_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # Calculate the range of years
    start_year <- target_year - 5  # Last 5 years including the target year
    
    # Convert the 'Detection_date' column to Date format
    aedes_data_cleaned <- data_values$aedes_data %>%
      mutate(Detection_date = as.Date(Detection_date, format = "%Y-%m-%d")) %>%
      filter(!is.na(Detection_date)) # Remove NA dates
    
    # Filter Aedes data for the last 5 years
    aedes_data_5y <- aedes_data_cleaned %>%
      mutate(aedes_year = as.numeric(format(Detection_date, "%Y"))) %>%
      filter(aedes_year >= start_year & aedes_year <= target_year)
    
    
    # Sum all observations over the last 5 years for each postcode
    positive_aedes_counts_5y <-  aedes_data_5y %>%
      group_by(Postcode, Municipality) %>%
      summarise(total_count = n(), .groups = 'drop')
    
    return(positive_aedes_counts_5y)
  })
  
  # Reactive expression to join the total count with the postal district geometries
  postcode_aedes_positive_5y_sf <- reactive({
    # Join with spatial data (postaldistricts) to get the geometry for mapping
    sf_data <- inner_join(postaldistricts, postcode_aedes_positive_5y(), by = 'Postcode')
    # Replace NA values in total_count with 0 (if needed)
    sf_data$total_count[is.na(sf_data$total_count)] <- 0
    return(sf_data)
  })
  
  # Create a color palette based on the total number of observations
  output$cumulative_aedes_map <- renderLeaflet({
    # Access the reactive data within the renderLeaflet context
    sf_data <- postcode_aedes_positive_5y_sf()
    
    # Check if the data is available
    req(nrow(sf_data) > 0)
    
    # Define color palette from yellow to red based on total observations
    color_palette <- colorNumeric(
      palette = c("yellow", "#ff0000"),  # Light yellow to red
      domain = c(1, 5)  # Adjust domain based on max observations
    )
    
    #Custom labels for the legend
    legend_labels <- c("1", "2", "3", "4", "5")
    
    
    # Create and render the leaflet map for cumulative Aedes data over the last 5 years
    leaflet(sf_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~color_palette(total_count),  # Use white for 0
        fillOpacity = 1,  # Adjust fill opacity for better visibility
        weight = 0.3,
        opacity = 0.5,
        color = 'black',  # Black border for better contrast
        dashArray = '3',
        highlight = highlightOptions(
          weight = 5,
          color = "white",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        popup = ~paste("Municipality: ", Municipality, "; Postcode:", Postcode, "; Total detections (5Yr):", total_count),
        label = ~paste("Municipality: ", Municipality, "; Postcode:", Postcode, ";  Total detections (5Yr):", total_count),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        colors = c(color_palette(c(1, 2, 3, 4, 5))),  # Custom colors for legend
        labels = legend_labels,
        opacity = 1,
        title = "Cumulative years",
        position = "bottomright"
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset View",
        onClick = JS(paste0("function(btn, map){ map.setView([", 50.50, ", ", 4.35, "], ", 7.5, "); }"))
      ))
  })
  
  # Show the data table for the cumulative Aedes data
  output$cumulative_aedes_table <- renderDT({
    # Access the reactive data within the renderDT context
    sf_data <- postcode_aedes_positive_5y() # Correct reactive value
    
    # Check if the data is available
    req(nrow(sf_data) > 0)
    
    # Render the datatable
    datatable(
      sf_data %>%
        dplyr::select(Postcode, Municipality, total_count) %>%
        dplyr::rename(
          "Post Code" = Postcode,
          "Municipality" = Municipality,
          "Total Detections (Last 5 Yr)" = total_count
        ),
      options = list(pageLength = 50)
    )
  })
  
  
  
  
  
  
  
  
  
  
  
#######################################
  # Data management functions
#######################################

  # Observe file input changes and reset the message
  observeEvent(input$file1, {
    req(input$file1)  # Ensure a file is uploaded
    
    # Clear previous messages before processing the new file
    output$message <- renderText(NULL)
    
    ext <- tools::file_ext(input$file1$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    dataset_type <- input$dataset_select
    if (dataset_type == "aedes") {
      uploaded_file <- tryCatch({
        read.csv(input$file1$datapath, header = TRUE, colClasses = c("Observation_ID" = "character"))
      }, warning = function(w) {
        shinyalert("Error", "Warning during file read: not all columns named in 'colClasses' exist.", type = "error")
        return(NULL)
      }, error = function(e) {
        shinyalert("Error", "Error during file read: not all columns named in 'colClasses' exist.", type = "error")
        return(NULL)
      })
      
      if (is.null(uploaded_file)) {
        output$message <- renderText("Error: The uploaded file could not be read.")
        show_verification(FALSE)  # Hide verification panel in case of error
        return(NULL)
      }
      
      # Validate columns
      required_columns <- c("Observation_ID", "Site_codes", "Municipality", 
                            "Postcode", "Latitude", "Longitude", "Site_type", "Detection_date", "Status")
      
      if (!all(required_columns %in% colnames(uploaded_file))) {
        shinyalert("Error", "The uploaded file does not have the required columns.", type = "error")
        output$message <- renderText("Error: The uploaded file does not have the required columns.")
        show_verification(FALSE)  # Hide verification panel in case of error
        return(NULL)
      }
      
      #########################
      # Validate data for Aedes
      #########################
      
      validation_errors <- list()
      if (!is.character(uploaded_file$Observation_ID) || any(uploaded_file$Observation_ID == "")) validation_errors <- c(validation_errors, "Observation_ID must be a non-empty string")
      if (!is.character(uploaded_file$Site_codes)) validation_errors <- c(validation_errors, "Site_codes must be a string")
      if (!is.character(uploaded_file$Municipality)) validation_errors <- c(validation_errors, "Municipality must be a string.")
      if (!all(uploaded_file$Postcode %in% valid_postcodes)) validation_errors <- c(validation_errors, "Postcode must exist in the valid postcodes list.")
      if (!all(uploaded_file$Latitude >= -90 & uploaded_file$Latitude <= 90)) validation_errors <- c(validation_errors, "Latitude must be between -90 and 90 degrees.")
      if (!all(uploaded_file$Longitude >= -180 & uploaded_file$Longitude <= 180)) validation_errors <- c(validation_errors, "Longitude must be between -180 and 180 degrees.")
      if (!is.character(uploaded_file$Site_type)) validation_errors <- c(validation_errors, "Site_type must be a string.")
      if (!is.character(uploaded_file$Status)) validation_errors <- c(validation_errors, "Status must be a string.")
      if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(uploaded_file$Detection_date)))) validation_errors <- c(validation_errors, "Detection_date must be in YYYY-MM-DD format.")
      
      if (length(validation_errors) > 0) {
        shinyalert("Validation Errors", paste(validation_errors, collapse = "; "), type = "error")
        output$message <- renderText(paste("Validation errors:", paste(validation_errors, collapse = "; ")))
        show_verification(FALSE)  # Hide verification panel in case of error
        return(NULL)
      }
      
    } else if (dataset_type == "dengue") {
      uploaded_file <- read.csv(input$file1$datapath, header = TRUE)
      
      # Validate columns for dengue_data
      required_columns <- c("Sample_ID", "Source_country", "Postcode", 'Report_date')
      
      if (!all(required_columns %in% colnames(uploaded_file))) {
        shinyalert("Error", "The uploaded file does not have the required columns for Dengue Data.", type = "error")
        output$message <- renderText("Error: The uploaded file does not have the required columns for Dengue Data.")
        show_verification(FALSE)  # Hide verification panel in case of error
        return(NULL)
      }
      
      ###############################
      # Validate data for dengue_data
      ###############################
      
      validation_errors <- list()
      if (!is.numeric(uploaded_file$Sample_ID) || any(uploaded_file$Sample_ID == "")) validation_errors <- c(validation_errors, "Sample_ID must be a non-empty string")
      if (!is.character(uploaded_file$Source_country)) validation_errors <- c(validation_errors, "Source_country must be a string")
      if (!all(uploaded_file$Postcode %in% valid_postcodes)) validation_errors <- c(validation_errors, "Postcode must exist in the valid postcodes list.")
      if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(uploaded_file$Report_date)))) validation_errors <- c(validation_errors, "Report_date must be in YYYY-MM-DD format.")
      
      if (length(validation_errors) > 0) {
        shinyalert("Validation Errors", paste(validation_errors, collapse = "; "), type = "error")
        output$message <- renderText(paste("Validation errors:", paste(validation_errors, collapse = "; ")))
        show_verification(FALSE)  # Hide verification panel in case of error
        return(NULL)
      }
    }
    
    # If all checks pass, store the data
    file_data(uploaded_file)
    output$data_table <- renderDT(uploaded_file)
    show_verification(TRUE)  # Show verification panel only if no errors
    output$message <- renderText(NULL)  # Clear previous messages
  })
  
  # Handle confirmation of upload
  observeEvent(input$confirm_upload, {
    req(file_data())
    save_data <- file_data()
    
    dataset_type <- input$dataset_select
    if (dataset_type == "aedes") {
      aedes_data <<- rbind(aedes_data, save_data)
      aedes_data <<- aedes_data[!duplicated(aedes_data$Observation_ID), ]
      data_values$aedes_data <- aedes_data
      output$message <- renderText("File has been appended to the global 'aedes_data' data frame")
    } else if (dataset_type == "dengue") {
      dengue_data <<- rbind(dengue_data, save_data)
      dengue_data <<- dengue_data[!duplicated(dengue_data$Sample_ID), ]
      data_values$dengue_data <- dengue_data
      output$message <- renderText("File has been appended to the global 'dengue_data' data frame")
    }
    
    show_verification(FALSE)  # Hide verification panel
    output$message <- renderText(NULL)  # Clear messages
    
    # Show success message only once
    shinyalert("Success", "Upload successful!", type = "success")
  })
  
  # Handle cancellation of upload
  observeEvent(input$cancel_upload, {
    file_data(NULL)  # Clear the file data
    show_verification(FALSE)  # Hide verification panel
    output$message <- renderText(NULL)  # Clear error message
    output$message <- renderText("Upload cancelled.")  # Show cancelled message
  })
  
  
  output$verification_panel <- renderUI({
    if (show_verification()) {
      tagList(
        h3("Verify Uploaded Data"),
        DTOutput("data_table"),
        actionButton("confirm_upload", "Confirm Upload"),
        actionButton("cancel_upload", "Cancel Upload"),
        verbatimTextOutput("message")
      )
    }
  })
  
  observeEvent(input$delete_all_data, {
    # Show confirmation dialog
    shinyalert(
      title = "Are you sure?",
      text = "This will delete all data permanently. Do you want to proceed?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Yes, delete it!",
      cancelButtonText = "No, cancel!",
      callbackR = function(x) {
        if (x) {
          # User clicked on "Yes"
          # Clear global data frames
          aedes_data <<- data.frame(
            Observation_ID = character(),
            Site_codes = character(),
            Municipality = character(),
            Postcode = numeric(),
            Latitude = numeric(),
            Longitude = numeric(),
            Site_type = character(),
            Status = character(),
            Detection_date = as.Date(character()),
            stringsAsFactors = FALSE
          )
          
          dengue_data <<- data.frame(
            Sample_ID = character(),
            Report_date  = as.Date(character()),
            Source_country = character(),
            Postcode = numeric(),
            stringsAsFactors = FALSE
          )
          
          # Show success message
          shinyalert("Success", "All data has been deleted!", type = "success")
          
          # Update reactive values
          data_values$aedes_data <- aedes_data
          data_values$dengue_data <- dengue_data
          
          # Stay on the current tab
          updateTabItems(session, "tabs", selected = "DeleteData")
        }
      }
    )
  })
  
  
  # Output to control visibility of the warning message
  output$show_warning_message <- reactive({
    !is.null(dataset_warning())
  })
  outputOptions(output, "show_warning_message", suspendWhenHidden = FALSE)
  
  # Output to display the actual warning message
  output$warning_message <- renderText({
    dataset_warning()
  })
  
}






shinyApp(ui, server)

