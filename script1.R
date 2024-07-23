library(shiny)
library(readr)

# Define UI
ui <- fluidPage(
  titlePanel("CSV Upload with Strict Requirements"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      actionButton("upload", "Upload")
    ),
    mainPanel(
      tableOutput("contents"),
      verbatimTextOutput("message")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$upload, {
    req(input$file1)
    
    # Read the CSV file
    df <- read_csv(input$file1$datapath)
    
    # Define the required column names and formats
    required_columns <- c("Observation_ID", "Site_codes", "Municipality", 
                          "Postcode", "Latitude", "Longitude", "Site_type", "Detection_date")
    
    # Check if the uploaded file has the required columns
    if (!all(required_columns %in% colnames(df))) {
      output$message <- renderText("Error: The uploaded file does not have the required columns.")
      return()
    }
    
    # Validate the data formats
    validation_errors <- list()
    
    if (!is.numeric(df$Site_codes)) validation_errors <- c(validation_errors, "Site_codes must be numeric.")
    if (!is.character(df$Municipality)) validation_errors <- c(validation_errors, "Municipality must be a string.")
    if (!all(nchar(as.character(df$Postcode)) == 4 & is.numeric(as.numeric(df$Postcode)))) validation_errors <- c(validation_errors, "Postcode must be a four-digit numerical code.")
    if (!all(grepl("^\\d+\\.\\d{5}$", as.character(df$Latitude)))) validation_errors <- c(validation_errors, "Latitude must be in decimal (8,5) format.")
    if (!all(grepl("^\\d+\\.\\d{5}$", as.character(df$Longitude)))) validation_errors <- c(validation_errors, "Longitude must be in decimal (8,5) format.")
    if (!is.character(df$Site_type)) validation_errors <- c(validation_errors, "Site_type must be a string.")
    if (!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(df$Detectio_date)))) validation_errors <- c(validation_errors, "Detectio_date must be in YYYY-MM-DD format.")
    
    if (length(validation_errors) > 0) {
      output$message <- renderText(paste("Error:", paste(validation_errors, collapse = " ")))
    } else {
      output$message <- renderText("File uploaded successfully.")
      output$contents <- renderTable({
        head(df)
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
