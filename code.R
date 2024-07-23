library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.providers)
library(DT)

# Global variable to store the data frame
aedes_data <- NULL

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
    tabItems(
      tabItem(tabName = "RealtimeRisk",
              h1("Realtime risk locations")),
      tabItem(tabName = "HistoricalRisk",
              h1("Historical risk locations")),
      tabItem(tabName = "DataManagement",
              h1("Data management"),
              fileInput("file1", "Choose CSV File", accept = ".csv"),
              tableOutput("contents"),
              verbatimTextOutput("message"),
              actionButton("save_file", "Save File to Environment"),
              actionButton("view_data", "View Data")
      )
    )
  )
)

server <- function(input, output, session) {
  file_data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)  # Ensure a file is uploaded
    
    ext <- tools::file_ext(input$file1$datapath)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    aedes_file <- read.csv(input$file1$datapath, header = TRUE)  # Hard-code header = TRUE
    
    # Validate columns
    required_columns <- c("Observation_ID", "Site_codes", "Municipality", 
                          "Postcode", "Latitude", "Longitude", "Site_type", "Detection_date")
    
    if (!all(required_columns %in% colnames(aedes_file))) {
      output$message <- renderText("Error: The uploaded file does not have the required columns.")
      return(NULL)
    }
    
    # Validate data
    validation_errors <- list()
    
    if (!is.character(aedes_file$Site_codes)) validation_errors <- c(validation_errors, "Site_codes must be numeric.")
    if (!is.character(aedes_file$Municipality)) validation_errors <- c(validation_errors, "Municipality must be a string.")
    if (!all(nchar(as.character(aedes_file$Postcode)) == 4 & is.numeric(as.numeric(aedes_file$Postcode)))) validation_errors <- c(validation_errors, "Postcode must be a four-digit numerical code.")
    if (!all(grepl("^\\d+\\.\\d{5}$", as.character(aedes_file$Latitude)))) validation_errors <- c(validation_errors, "Latitude must be in decimal (8,5) format.")
    if (!all(grepl("^\\d+\\.\\d{5}$", as.character(aedes_file$Longitude)))) validation_errors <- c(validation_errors, "Longitude must be in decimal (8,5) format.")
    if (!is.character(aedes_file$Site_type)) validation_errors <- c(validation_errors, "Site_type must be a string.")
    if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(aedes_file$Detection_date)))) validation_errors <- c(validation_errors, "Detection_date must be in YYYY-MM-DD format.")
    
    if (length(validation_errors) > 0) {
      output$message <- renderText(paste("Validation errors:", paste(validation_errors, collapse = "; ")))
      return(NULL)
    }
    
    file_data(aedes_file)
    output$message <- renderText("File successfully uploaded and validated.")
  })
  
  observeEvent(input$save_file, {
    req(file_data())
    save_data <- file_data()
    # Assign the data frame to a global variable
    assign("aedes_data", save_data, envir = .GlobalEnv)
    output$message <- renderText("File has been saved to the global environment as 'aedes_data'")
  })
  
  observeEvent(input$view_data, {
    output$contents <- renderTable({
      req(aedes_data)
      aedes_data
    })
  })
}

shinyApp(ui, server)
