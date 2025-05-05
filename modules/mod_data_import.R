# UI function for the data import module
dataImportUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Data Import",
           
           fluidRow(
             column(3,  # Sidebar width reduced to 3
                    # File selection section
                    wellPanel(
                      h4("File Selection"),
                      fileInput(ns("file"), "Select CSV file",
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                multiple = FALSE),
                      helpText("Maximum file size: 1 GB")
                    ),
                    
                    # Import options section
                    wellPanel(
                      h4("Import Options"),
                      checkboxInput(ns("header"), "Header present", TRUE),
                      radioButtons(ns("sep"), "Separator:",
                                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                   selected = ";"),
                      radioButtons(ns("dec"), "Decimal point:",
                                   choices = c(Period = ".", Comma = ","),
                                   selected = "."),
                      selectInput(ns("encoding"), "Character encoding:",
                                  choices = c("UTF-8", "Latin-1", "Windows-1252"),
                                  selected = "UTF-8")
                    ),
                    
                    # Action section
                    wellPanel(
                      h4("Actions"),
                      actionButton(ns("load_data"), "Load data", class = "btn-primary", 
                                   icon = icon("upload")),
                      # Status display instead of complex progress bar
                      conditionalPanel(
                        condition = "input.load_data > 0", 
                        ns = ns,
                        div(
                          style = "margin-top: 10px;",
                          textOutput(ns("loading_status"))
                        )
                      )
                    )
             ),
             column(9,  # Main panel
                    # Data preview panel
                    wellPanel(
                        h3("Data Preview"),
                        verbatimTextOutput(ns("data_structure")),
                        DT::dataTableOutput(ns("data_preview"))
                      )
                    
             )
           )
  )
}

# Server function for the data import module
dataImportServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Loading status text
    load_status <- reactiveVal("Ready to load")
    
    # Display loading status
    output$loading_status <- renderText({
      load_status()
    })
    
    # Reactive dataset
    data <- reactiveVal(NULL)
    
    # Flag to indicate if data is loaded
    data_loaded <- reactiveVal(FALSE)
    
    # Output for conditionalPanel
    output$data_loaded <- reactive({
      return(data_loaded())
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    # Load data with progress display
    observeEvent(input$load_data, {
      req(input$file)
      
      inFile <- input$file
      
      # Check file size (max 100 MB)
      if (inFile$size > 1000 * 1024 * 1024) {
        showNotification(
          "The file exceeds the maximum size of 1 GB.", 
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Simpler version with built-in Shiny withProgress function
      withProgress(message = 'Loading data', value = 0, {
        
        # Update status
        load_status("Loading data...")
        
        tryCatch({
          incProgress(0.1, detail = "Reading file")
          
          df <- read.csv(inFile$datapath,
                         header = input$header,
                         sep = input$sep,
                         dec = input$dec,
                         encoding = input$encoding,
                         stringsAsFactors = FALSE)
          
          incProgress(0.9, detail = "Processing data")
          
          # Process and save data
          data(df)
          
          # Set data loaded flag to true
          data_loaded(TRUE)
          
          incProgress(1, detail = "Completed")
          
          # Update status
          load_status("Data successfully loaded")
          
          # Success message
          showNotification(
            paste("File successfully loaded:", inFile$name),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          # Update status
          load_status(paste("Error loading:", e$message))
          
          # Error message
          showNotification(
            paste("Error loading file:", e$message),
            type = "error",
            duration = NULL
          )
        })
      })
    })
    
    # Data structure information
    output$data_structure <- renderPrint({
      req(data())
      cat("Dimensions: ", dim(data())[1], " rows, ", dim(data())[2], " columns\n\n")
      str(data())
    })
    
    # Data preview
    output$data_preview <- DT::renderDataTable({
      req(data())
      DT::datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Return module values (to make accessible to other modules)
    return(list(
      data = data
    ))
  })
}