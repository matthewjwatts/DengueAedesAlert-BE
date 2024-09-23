library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.providers)
library(DT)
library(sf)
library(shinyalert)
library(tidyverse)
library(leaflet.extras)


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




ui <- dashboardPage(
  dashboardHeader(title = "Dengue Risk Belgium"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Realtime risk", tabName = "RealtimeRisk"),
      menuItem("Historical risk", tabName = "HistoricalRisk"),
      menuItem("Data management", tabName = "DataManagement")
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      tabItem(tabName = "RealtimeRisk",
              h1("Realtime risk locations"),
              h3(textOutput("current_date_display")),
              # Place the slider input in the main page content
              sliderInput("weeks_before", 
                          "Select weeks before current date:",
                          min = 1, max = 4, value = 1, step = 1),
              leafletOutput("realtime_map", height = "600px"),
              DTOutput("aedes_dengue_table")  # Remove the comma here
                # Add this line
      ),  # Add the closing parenthesis for the tabItem
      tabItem(tabName = "HistoricalRisk",
              h1("Historical risk locations")),
      tabItem(tabName = "DataManagement",
              h1("Data management"),
              selectInput("dataset_select", "Select Dataset to Upload:",
                          choices = list("Aedes data" = "aedes", "Dengue data" = "dengue"),
                          selected = "aedes"),
              fileInput("file1", "Choose CSV File", accept = ".csv"),
              uiOutput("verification_panel"),
              verbatimTextOutput("message")
      )
    )
  )
)


server <- function(input, output, session) {

  
  file_data <- reactiveVal(NULL)
  show_verification <- reactiveVal(FALSE)
  
  current_date <- reactive({
    current_date <- '2023-12-28'
    as.Date(current_date, format = "%Y-%m-%d")
  })
  
  # Render the current date for display
  output$current_date_display <- renderText({
    paste("Date today:", current_date())
  })
  
  # Reactive expression to calculate start_date based on slider input
  start_date <- reactive({
    current_date() - (input$weeks_before * 60)  # Changed to multiply by 7 for weeks
  })
  
  # Reactive expression to filter aedes_data based on current year
  aedes_data_filtered <- reactive({
    current_year <- 2023
    aedes_data %>%
      mutate(Detection_date = as.Date(Detection_date, format = "%Y-%m-%d")) %>%
      filter(format(Detection_date, "%Y") == current_year)
  })
  
  # Reactive expression to filter dengue_data based on start_date
  dengue_data_filtered <- reactive({
    dengue_data %>%
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
        Denv_case_sample_ID = Sample_ID, 
        Denv_case_report_date = Report_date,
        Denv_case_source_country = Source_country
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

  

  observeEvent(input$file1, {
    req(input$file1)  # Ensure a file is uploaded
    
    
    # Clear previous messages
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
        return(NULL)
      }
      
      # Validate columns
      required_columns <- c("Observation_ID", "Site_codes", "Municipality", 
                            "Postcode", "Latitude", "Longitude", "Site_type", "Detection_date")
      
      if (!all(required_columns %in% colnames(uploaded_file))) {
        shinyalert("Error", "The uploaded file does not have the required columns.", type = "error")
        output$message <- renderText("Error: The uploaded file does not have the required columns.")
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
      if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(uploaded_file$Detection_date)))) validation_errors <- c(validation_errors, "Detection_date must be in YYYY-MM-DD format.")
      
      if (length(validation_errors) > 0) {
        shinyalert("Validation Errors", paste(validation_errors, collapse = "; "), type = "error")
        output$message <- renderText(paste("Validation errors:", paste(validation_errors, collapse = "; ")))
        return(NULL)
      }
      
      ############################
      # Controls for aedes uploads
      ############################
      
    } else if (dataset_type == "dengue") {
      uploaded_file <- read.csv(input$file1$datapath, header = TRUE)
      
      # Validate columns for dengue_data
      required_columns <- c("Sample_ID", "Source_country", "Postcode", 'Report_date') # Add appropriate required columns for dengue_data
      
      if (!all(required_columns %in% colnames(uploaded_file))) {
        shinyalert("Error", "The uploaded file does not have the required columns for Dengue Data.", type = "error")
        output$message <- renderText("Error: The uploaded file does not have the required columns for Dengue Data.")
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
        return(NULL)
      }
    }
    
    
    file_data(uploaded_file)
    output$data_table <- renderDT(uploaded_file)
    show_verification(TRUE)
  })
  
  
  
  
  observeEvent(input$confirm_upload, {
    req(file_data())
    save_data <- file_data()
    
    dataset_type <- input$dataset_select
    if (dataset_type == "aedes") {
      # Append the new data to the existing global data frame
      aedes_data <<- rbind(aedes_data, save_data)
      # Remove duplicates based on Observation_ID
      aedes_data <<- aedes_data[!duplicated(aedes_data$Observation_ID), ]
      
      # Assign the updated dataframe to the global environment
      assign("aedes_data", aedes_data, envir = .GlobalEnv)
      
      output$message <- renderText("File has been appended to the global 'aedes_data' data frame")
      
    } else if (dataset_type == "dengue") {
      # Append the new data to the existing global data frame
      dengue_data <<- rbind(dengue_data, save_data)
      # Remove duplicates based on Sample_ID
      dengue_data <<- dengue_data[!duplicated(dengue_data$Sample_ID), ]
      
      # Assign the updated dataframe to the global environment
      assign("dengue_data", dengue_data, envir = .GlobalEnv)
      
      output$message <- renderText("File has been appended to the global 'dengue_data' data frame")
    }
    
    # Hide the verification panel after confirmation
    show_verification(FALSE)
    
    # Clear messages
    output$message <- renderText(NULL)
    
    # Show success message
    shinyalert("Success", "Upload successful!", type = "success")
  })
  
  observeEvent(input$cancel_upload, {
    # Clear the file_data and hide the verification panel
    file_data(NULL)
    show_verification(FALSE)
    
    # Clear messages
    output$message <- renderText(NULL)
    
    output$message <- renderText("Upload cancelled.")
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
}

shinyApp(ui, server)

