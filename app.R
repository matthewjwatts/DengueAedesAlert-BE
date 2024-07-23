library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.providers)
library(DT)
library(shinyalert)

# Global variables to store the data frames
if (!exists("aedes_data")) {
  aedes_data <<- data.frame(
    Observation_ID = integer(),
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
    Observation_ID = integer(),
    Other_Columns = character(), # Add appropriate columns for dengue_data
    stringsAsFactors = FALSE
  )
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
              h1("Realtime risk locations")),
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
      
      # Validate data
      validation_errors <- list()
      if (!is.character(uploaded_file$Observation_ID)) validation_errors <- c(validation_errors, "Observation_ID must be a string and not empty")
      if (!is.character(uploaded_file$Site_codes)) validation_errors <- c(validation_errors, "Site_codes must be a string")
      if (!is.character(uploaded_file$Municipality)) validation_errors <- c(validation_errors, "Municipality must be a string.")
      if (!all(nchar(as.character(uploaded_file$Postcode)) == 4 & is.numeric(as.numeric(uploaded_file$Postcode)))) validation_errors <- c(validation_errors, "Postcode must be a four-digit numerical code.")
      if (!all(uploaded_file$Latitude >= -90 & uploaded_file$Latitude <= 90)) validation_errors <- c(validation_errors, "Latitude must be between -90 and 90 degrees.")
      if (!all(uploaded_file$Longitude >= -180 & uploaded_file$Longitude <= 180)) validation_errors <- c(validation_errors, "Longitude must be between -180 and 180 degrees.")
      if (!is.character(uploaded_file$Site_type)) validation_errors <- c(validation_errors, "Site_type must be a string.")
      if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(uploaded_file$Detection_date)))) validation_errors <- c(validation_errors, "Detection_date must be in YYYY-MM-DD format.")
      
      if (length(validation_errors) > 0) {
        shinyalert("Validation Errors", paste(validation_errors, collapse = "; "), type = "error")
        output$message <- renderText(paste("Validation errors:", paste(validation_errors, collapse = "; ")))
        return(NULL)
      }
      
    } else if (dataset_type == "dengue") {
      uploaded_file <- read.csv(input$file1$datapath, header = TRUE)
      
      # Validate columns for dengue_data
      required_columns <- c("Observation_ID", "Other_Columns") # Add appropriate required columns for dengue_data
      
      if (!all(required_columns %in% colnames(uploaded_file))) {
        shinyalert("Error", "The uploaded file does not have the required columns for Dengue Data.", type = "error")
        output$message <- renderText("Error: The uploaded file does not have the required columns for Dengue Data.")
        return(NULL)
      }
      
      # Validate data for dengue_data
      validation_errors <- list()
      if (!is.character(uploaded_file$Observation_ID)) validation_errors <- c(validation_errors, "Observation_ID must be a string and not empty")
      # Add other validation checks specific to dengue_data
      
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
      # Remove duplicates based on Observation_ID
      dengue_data <<- dengue_data[!duplicated(dengue_data$Observation_ID), ]
      
      # Assign the updated dataframe to the global environment
      assign("dengue_data", dengue_data, envir = .GlobalEnv)
      
      output$message <- renderText("File has been appended to the global 'dengue_data' data frame")
    }
    # Hide the verification panel after confirmation
    show_verification(FALSE)
    
    # Show success message
    shinyalert("Success", "Upload successful!", type = "success")
  })
  
  observeEvent(input$cancel_upload, {
    # Clear the file_data and hide the verification panel
    file_data(NULL)
    show_verification(FALSE)
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

