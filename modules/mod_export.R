# modules/mod_export.R
# Export module for the GLM modeling tool

# Export Module UI
exportModuleUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Export",
    fluidRow(
      column(
        width = 12,
        h3("Export Model & Risk Factors"),
        hr()
      )
    ),
    fluidRow(
      column(
        width = 12,
        radioButtons(ns("model_selection"), "Select model:",
                     choices = c("Standard Model" = "standard", 
                                 "Relaxed Model" = "relaxed"),
                     selected = "standard",
                     inline = TRUE),
        hr()
      )
    ),
    fluidRow(
      # Panel 1: Risk Factors Export
      column(
        width = 4,
        wellPanel(
          h4("Export Risk Factors"),
          # Slider instead of selection field for S values
          sliderInput(ns("s_value"), "Select S value:", 
                      min = 1, max = 100, value = 50, step = 1),
          radioButtons(ns("export_type"), "Export type:", 
                       choices = c("Response" = "response", "Link" = "link"),
                       selected = "response"),
          textInput(ns("export_file_name"), "File name (optional):",
                    placeholder = "(e.g., riskfactors.xlsx)"),
          br(),
          actionButton(ns("export_risk_factors"), "Export risk factors", 
                       class = "btn-primary", style = "width:100%")
        )
      ),
      
      # Panel 2: Components Export
      column(
        width = 4,
        wellPanel(
          h4("Export Components"),
          selectInput(ns("component_selection"), "Select component:",
                      choices = c("Please select" = "")),
          
          # Conditional Panel for S value in prediction components
          conditionalPanel(
            condition = paste0(
              "input.component_selection == 'preds_full' || ",
              "input.component_selection == 'preds_train' || ",
              "input.component_selection == 'preds_test'"
            ),
            ns = ns,
            sliderInput(ns("components_s_value"), "Select S value:", 
                        min = 1, max = 100, value = 50, step = 1)
          ),
          
          # File name and export format
          conditionalPanel(
            condition = paste0(
              "input.component_selection == 'preds_full' || ",
              "input.component_selection == 'preds_train' || ",
              "input.component_selection == 'preds_test'"
            ),
            ns = ns,
            textInput(ns("component_file_name"), "File name (optional):",
                      placeholder = "(e.g., predictions.csv)")
          ),
          
          conditionalPanel(
            condition = paste0(
              "input.component_selection != 'preds_full' && ",
              "input.component_selection != 'preds_train' && ",
              "input.component_selection != 'preds_test' && ",
              "input.component_selection != ''"
            ),
            ns = ns,
            textInput(ns("component_rds_file_name"), "File name (optional):",
                      placeholder = "(e.g., contrasts.rds)")
          ),
          
          br(),
          actionButton(ns("export_component"), "Export component", 
                       class = "btn-primary", style = "width:100%")
        )
      ),
      
      # Panel 3: Complete Model Export
      column(
        width = 4,
        wellPanel(
          h4("Export Complete Model"),
          radioButtons(ns("model_export_format"), "Export format:", 
                       choices = c("RDS" = "rds", "RData" = "rdata"),
                       selected = "rds"),
          textInput(ns("model_export_file_name"), "File name (optional):",
                    placeholder = "(e.g., glm_model.rds)"),
          br(),
          actionButton(ns("export_full_model"), "Export complete model", 
                       class = "btn-primary", style = "width:100%")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Export History"),
          verbatimTextOutput(ns("export_log"))
        )
      )
    )
  )
}

# Export Module Server
exportModuleServer <- function(id, distribution, standard_model, relaxed_model) {
  moduleServer(id, function(input, output, session) {
    
    # Get currently active model based on user selection
    active_model <- reactive({
      if (input$model_selection == "standard") {
        standard_model$model_results()
      } else {
        relaxed_model$model_results()
      }
    })
    
    # Observe the active model to set the maximum S values
    observe({
      req(active_model())
      model <- active_model()
      
      if (!is.null(model$base_level)) {
        s_values <- gsub("s", "", colnames(model$base_level)) 
        max_s <- max(as.numeric(s_values), na.rm = TRUE) + 1 
        
        # Update sliders with the maximum available S value
        updateSliderInput(session, "s_value", 
                          max = max_s,
                          value = min(input$s_value, max_s))
        
        updateSliderInput(session, "components_s_value", 
                          max = max_s,
                          value = min(input$components_s_value, max_s))
      }
    })
    
    # Dynamic selection of components based on model type
    observe({
      if (input$model_selection == "standard") {
        updateSelectInput(session, "component_selection",
                          choices = c(
                            "Please select" = "",
                            "Predictions (All)" = "preds_full",
                            "Predictions (Training)" = "preds_train",
                            "Predictions (Test)" = "preds_test",
                            "Contrasts" = "contrasts",
                            "Fitted Model (Training)" = "fitted_model_train",
                            "Split" = "split"
                          ))
      } else {
        updateSelectInput(session, "component_selection",
                          choices = c(
                            "Please select" = "",
                            "Predictions (All)" = "preds_full",
                            "Predictions (Training)" = "preds_train",
                            "Predictions (Test)" = "preds_test",
                            "Contrasts" = "contrasts",
                            "Initial Model" = "initial_model",
                            "Relaxed Model" = "relaxed_model",
                            "Split" = "split"
                          ))
      }
    })
    
    # Update the filename placeholder based on the selected format
    observe({
      # Update placeholder and help text for complete model
      file_ext <- input$model_export_format
      placeholder <- paste0("complete_model.", file_ext, " (e.g., glm_model.", file_ext, ")")
      
      updateTextInput(session, "model_export_file_name", 
                      placeholder = placeholder)
      
      # Updating help text would be more complex in Shiny
      # and would require a custom JavaScript handler
      # Therefore, it's not directly implemented here
    })
    
    # Log for export activities
    export_log <- reactiveVal("")
    
    # Function to add to the log
    add_to_log <- function(message) {
      current <- export_log()
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      new_log <- paste0(current, timestamp, ": ", message, "\n")
      export_log(new_log)
    }
    
    # Display log
    output$export_log <- renderText({
      export_log()
    })
    
    # Export risk factors to Excel
    observeEvent(input$export_risk_factors, {
      req(active_model())
      model <- active_model()
      model_type <- input$model_selection
      
      # Validate the model has risk factors
      if (is.null(model$risk_factors)) {
        showNotification("No risk factors available in the model.", type = "error")
        add_to_log(paste0("Error: No risk factors available in the ", 
                          if(model_type == "standard") "standard model" else "relaxed model",
                          "."))
        return()
      }
      
      # File name - use input or default
      file_name <- if (input$export_file_name == "") {
        paste0(
          if(model_type == "standard") "standard_" else "relaxed_",
          "risk_factors_s", input$s_value, "_", input$export_type, ".xlsx"
        )
      } else {
        input$export_file_name
      }
      
      # Try to export
      tryCatch({
        s_value <- as.numeric(input$s_value) - 1
        result <- export_risk_factors_to_excel(
          model = model,
          distribution = distribution(),
          s = s_value,
          type = input$export_type,
          file_name = file_name
        )
        
        model_type_label <- if(model_type == "standard") "standard model" else "relaxed model"
        showNotification(paste0("Risk factors from ", model_type_label, " successfully exported: ", file_name), 
                         type = "default")
        add_to_log(paste0("Risk factors from ", model_type_label, " successfully exported: ", file_name))
      }, error = function(e) {
        showNotification(paste("Error exporting risk factors:", e$message), 
                         type = "error")
        add_to_log(paste("Error exporting risk factors:", e$message))
      })
    })
    
    # Export model component
    observeEvent(input$export_component, {
      req(active_model(), input$component_selection != "")
      model <- active_model()
      model_type <- input$model_selection
      component_type <- input$component_selection
      
      # Determine file type based on selected component
      is_prediction <- component_type %in% c("preds_full", "preds_train", "preds_test")
      file_extension <- if(is_prediction) ".csv" else ".rds"
      
      # File name - use input or default
      file_name <- if(is_prediction) {
        if (input$component_file_name == "") {
          paste0(
            if(model_type == "standard") "standard_" else "relaxed_",
            component_type, "_s", input$components_s_value, file_extension
          )
        } else {
          # Ensure the correct file extension is used
          if(!grepl("\\.csv$", input$component_file_name)) {
            paste0(tools::file_path_sans_ext(input$component_file_name), file_extension)
          } else {
            input$component_file_name
          }
        }
      } else {
        if (input$component_rds_file_name == "") {
          paste0(
            if(model_type == "standard") "standard_" else "relaxed_",
            component_type, file_extension
          )
        } else {
          # Ensure the correct file extension is used
          if(!grepl("\\.rds$", input$component_rds_file_name)) {
            paste0(tools::file_path_sans_ext(input$component_rds_file_name), file_extension)
          } else {
            input$component_rds_file_name
          }
        }
      }
      
      # Try to export
      tryCatch({
        # Extract component
        component_data <- NULL
        
        if (is_prediction) {
          # For predictions: Use S value
          s_value <- as.numeric(input$components_s_value) - 1
          s_col <- paste0("s", s_value)
          
          # Extract predictions
          component_data <- model[[component_type]]
          
          # If only one column (for the selected S value) is desired
          if (!is.null(s_col) && s_col %in% colnames(component_data)) {
            component_data <- data.frame(
              id = rownames(component_data),
              prediction = component_data[, s_col],
              stringsAsFactors = FALSE
            )
          }
          
          # Export as CSV
          write.csv(component_data, file = file_name, row.names = FALSE)
        } else {
          # For other components
          
          if (component_type == "contrasts") {
            component_data <- model$contrasts
          } else if (component_type == "fitted_model_train" && model_type == "standard") {
            component_data <- model$fitted_model_train
          } else if (component_type == "split") {
            component_data <- model$split
          } else if (component_type == "relaxed_model" && model_type == "relaxed") {
            component_data <- model$relaxed_model
          } else if (component_type == "initial_model" && model_type == "relaxed") {
            component_data <- model$initial_model
          }
          
          # Export as RDS
          saveRDS(component_data, file = file_name)
        }
        
        # Check if component was successfully extracted
        if (is.null(component_data)) {
          showNotification(paste0("Component '", component_type, "' is not available or empty."), 
                           type = "warning")
          add_to_log(paste0("Warning: Component '", component_type, "' is not available or empty."))
          return()
        }
        
        model_type_label <- if(model_type == "standard") "standard model" else "relaxed model"
        showNotification(paste0("Component '", component_type, "' from ", model_type_label, 
                                " successfully exported: ", file_name), 
                         type = "default")
        add_to_log(paste0("Component '", component_type, "' from ", model_type_label, 
                          " successfully exported: ", file_name))
      }, error = function(e) {
        showNotification(paste("Error exporting component:", e$message), 
                         type = "error")
        add_to_log(paste("Error exporting component:", e$message))
      })
    })
    
    # Export complete model
    observeEvent(input$export_full_model, {
      req(active_model())
      model <- active_model()
      model_type <- input$model_selection
      file_ext <- input$model_export_format
      
      # File name - use input or default
      file_name <- if (input$model_export_file_name == "") {
        paste0(
          if(model_type == "standard") "standard_" else "relaxed_",
          "complete_model.", file_ext
        )
      } else {
        # Ensure the correct file extension is used
        if(!grepl(paste0("\\.", file_ext, "$"), input$model_export_file_name)) {
          paste0(tools::file_path_sans_ext(input$model_export_file_name), ".", file_ext)
        } else {
          input$model_export_file_name
        }
      }
      
      tryCatch({
        if (file_ext == "rds") {
          saveRDS(model, file = file_name)
        } else if (file_ext == "rdata") {
          # Use a named variable based on the model type
          if (model_type == "standard") {
            standard_model_export <- model
            save(standard_model_export, file = file_name)
          } else {
            relaxed_model_export <- model
            save(relaxed_model_export, file = file_name)
          }
        }
        
        model_type_label <- if(model_type == "standard") "Standard model" else "Relaxed model"
        showNotification(paste0(model_type_label, " successfully exported: ", file_name), 
                         type = "default")
        add_to_log(paste0(model_type_label, " successfully exported: ", file_name))
      }, error = function(e) {
        showNotification(paste("Error exporting model:", e$message), 
                         type = "error")
        add_to_log(paste("Error exporting model:", e$message))
      })
    })
    
  })
}