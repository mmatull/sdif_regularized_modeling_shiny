# UI function for the variable selection module with data preparation
variableSelectionUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Variable Selection",
           fluidPage(
             
             sidebarLayout(
               sidebarPanel(width = 3, # Reduced width of the sidePanel
                            # Main variables
                            wellPanel(
                              h4("Main Variables", class = "text-primary"),
                              selectInput(ns("target_var"), "Target Variable:",
                                          choices = c("Please select" = ""), selected = ""),
                              selectInput(ns("weight_var"), "Weight Variable:",
                                          choices = c("None" = "", "Please select" = ""), selected = ""),
                              selectInput(ns("offset_var"), "Offset Variable:",
                                          choices = c("None" = "", "Please select" = ""), selected = "")
                            ),
                            
                            # Feature selection and type
                            conditionalPanel(
                              condition = paste0("input['", ns("target_var"), "'] !== ''"),
                              wellPanel(
                                h4("Select Feature", class = "text-primary"),
                                selectInput(ns("feature_to_analyze"), "Feature:",
                                            choices = c("Please select" = "")),
                                radioButtons(ns("feature_type"), "Feature Type:",
                                             choices = c("Numeric" = "numeric",
                                                         "Categorical" = "categorical",
                                                         "Do not use" = "exclude"),
                                             selected = "numeric")
                              )
                            ),
                            
                            # Buttons for apply and reset
                            conditionalPanel(
                              condition = paste0("input['", ns("feature_to_analyze"), "'] !== '' && input['", ns("feature_type"), "'] !== 'exclude'"),
                              wellPanel(
                                actionButton(ns("apply_changes"), "Apply Changes",
                                             class = "btn-primary btn-block"),
                                actionButton(ns("reset_feature"), "Reset Feature Selection",
                                             class = "btn-warning btn-block")
                              )
                            )
               ),
               
               mainPanel(width = 9, # Adjusted width of the mainPanel
                         fluidRow(
                           column(8, # Left column for missing values and preview
                                  # Missing value handling (conditionally displayed)
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== ''"),
                                    wellPanel(
                                      h4("Missing Value Handling", class = "text-primary"),
                                      verbatimTextOutput(ns("missing_summary")),
                                      textInput(ns("additional_missing"),
                                                "Additional Missing Values (comma-separated):", ""),
                                      checkboxInput(ns("missing_as_category"),
                                                    "Treat Missing Values as a Separate Category", TRUE)
                                    )
                                  ),
                                  
                                  # Options for numeric features (only show if numeric)
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== '' && input['", ns("feature_type"), "'] == 'numeric'"),
                                    wellPanel(
                                      h4("Numeric Options", class = "text-primary"),
                                      radioButtons(ns("binning_method"), "Binning Method:",
                                                   choices = c("Equal Width" = "equal_width",
                                                               "Equal Count" = "equal_count"),
                                                   selected = "equal_width"),
                                      sliderInput(ns("numeric_bins"), "Number of Bins:",
                                                  min = 2, max = 50, value = 10, step = 1)
                                    )
                                  ),
                                  
                                  # Preview of data preparation (conditionally displayed)
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== '' && input['", ns("feature_type"), "'] !== 'exclude'"),
                                    wellPanel(
                                      h4("Data Preparation Preview", class = "text-primary"),
                                      h6("Raw Data:"),
                                      verbatimTextOutput(ns("raw_data_preview")),
                                      h6("Processed Data:"),
                                      verbatimTextOutput(ns("processed_data_preview")),
                                      textInput(ns("output_column_name"), "Name of Generated Column:", "")
                                    )
                                  )
                           ),
                           column(4, # Right column for current selection
                                  # Current feature overview
                                  wellPanel(
                                    h4("Current Selection", class = "text-primary"),
                                    verbatimTextOutput(ns("selected_features_summary"))
                                  )
                           )
                         )
               )
             )
           )
  )
}

# Server function for the module (adapted for bin count and multiple selection)
variableSelectionServer <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    # Store current variable selection
    selected_features <- reactiveVal(list(
      numeric = list(), # Changed to list() - now stores name, output name, and other properties
      categorical = list(),
      exclude = character(0)
    ))
    
    # Processed data
    processed_data <- reactiveVal(NULL)
    
    # Update exclude list at start
    observe({
      req(imported_data$data())
      
      current_selected <- selected_features()
      
      # Set all features to exclude that are not in numeric or categorical
      # and not selected as target, weight or offset
      all_vars <- names(imported_data$data())
      used_vars <- c(
        input$target_var, 
        if(input$weight_var != "") input$weight_var else NULL,
        if(input$offset_var != "") input$offset_var else NULL,
        unlist(lapply(current_selected$numeric, function(x) x$name)),
        unlist(lapply(current_selected$categorical, function(x) x$name))
      )
      used_vars <- used_vars[used_vars != ""] # Remove empty values
      
      # Update exclude with all unused variables
      current_selected$exclude <- setdiff(all_vars, used_vars)
      selected_features(current_selected)
    })
    
    # Dynamically update variable selection lists when data is available
    observe({
      req(imported_data$data())
      var_choices <- c("Please select" = "", names(imported_data$data()))
      weight_offset_choices <- c("None" = "", names(imported_data$data()))
      
      updateSelectInput(session, "target_var", choices = var_choices)
      updateSelectInput(session, "weight_var", choices = weight_offset_choices)
      updateSelectInput(session, "offset_var", choices = weight_offset_choices)
    })
    
    # Update feature selection list for analysis
    observe({
      req(imported_data$data())
      req(input$target_var != "")
      
      # All variables except target, weight and offset
      excluded_vars <- c(input$target_var)
      if(input$weight_var != "") excluded_vars <- c(excluded_vars, input$weight_var)
      if(input$offset_var != "") excluded_vars <- c(excluded_vars, input$offset_var)
      
      all_vars <- setdiff(names(imported_data$data()), excluded_vars)
      
      updateSelectInput(session, "feature_to_analyze",
                        choices = c("Please select" = "", all_vars))
    })
    
    # When a feature is selected for analysis
    observeEvent(input$feature_to_analyze, {
      req(input$feature_to_analyze != "")
      
      # Default value based on data type
      feature_var <- input$feature_to_analyze
      if(is.numeric(imported_data$data()[[feature_var]])) {
        updateRadioButtons(session, "feature_type", selected = "numeric")
        # New naming convention
        suggested_name <- paste0(feature_var, "_num_", 
                                 ifelse(input$binning_method == "equal_width", 
                                        "eql_", "eqn_"), 
                                 input$numeric_bins)
      } else {
        updateRadioButtons(session, "feature_type", selected = "categorical")
        suggested_name <- paste0(feature_var, "_cat")
      }
      
      # Suggestion for output column name
      updateTextInput(session, "output_column_name", value = suggested_name)
    })
    
    # When binning method changes, update the suggested name
    observeEvent(input$binning_method, {
      req(input$feature_to_analyze != "")
      req(input$feature_type == "numeric")
      
      feature_var <- input$feature_to_analyze
      # New naming convention based on selected method
      suggested_name <- paste0(feature_var, "_num_", 
                               ifelse(input$binning_method == "equal_width", 
                                      "eql_", "eqn_"), 
                               input$numeric_bins)
      
      updateTextInput(session, "output_column_name", value = suggested_name)
    })
    
    # When bin count changes, update the name
    observeEvent(input$numeric_bins, {
      req(input$feature_to_analyze != "")
      req(input$feature_type == "numeric")
      
      feature_var <- input$feature_to_analyze
      # New naming convention with updated bin count
      suggested_name <- paste0(feature_var, "_num_", 
                               ifelse(input$binning_method == "equal_width", 
                                      "eql_", "eqn_"), 
                               input$numeric_bins)
      
      updateTextInput(session, "output_column_name", value = suggested_name)
    })
    
    # Monitor main variables (Target, Weight, Offset) and update exclude list
    observeEvent(list(input$target_var, input$weight_var, input$offset_var), {
      req(imported_data$data())
      
      # Save current state
      current_selected <- selected_features()
      
      # Remove from exclude list if selected as main variable
      used_vars <- c(
        input$target_var,
        if(input$weight_var != "") input$weight_var else NULL,
        if(input$offset_var != "") input$offset_var else NULL
      )
      used_vars <- used_vars[used_vars != ""] # Remove empty values
      
      # Update exclude by removing used variables
      current_selected$exclude <- setdiff(current_selected$exclude, used_vars)
      selected_features(current_selected)
    })
    
    # Summarize missing values
    output$missing_summary <- renderPrint({
      req(imported_data$data())
      req(input$feature_to_analyze != "")
      
      data <- imported_data$data()
      feature_var <- input$feature_to_analyze
      
      # Count NA values
      na_count <- sum(is.na(data[[feature_var]]))
      total_count <- nrow(data)
      na_percent <- round(na_count / total_count * 100, 2)
      
      cat("Missing Values (NA):", na_count, "of", total_count, "(", na_percent, "%)\n")
      
      # Most frequent values (for possible content-based missing values)
      if(!is.numeric(data[[feature_var]])) {
        top_values <- sort(table(data[[feature_var]]), decreasing = TRUE)[1:min(5, length(table(data[[feature_var]])))]
        cat("\nMost Frequent Values:\n")
        print(top_values)
      }
    })
    
    # Raw data preview
    output$raw_data_preview <- renderPrint({
      req(imported_data$data())
      req(input$feature_to_analyze != "")
      
      data <- imported_data$data()
      feature_var <- input$feature_to_analyze
      
      # Show first 10 values
      head(data[[feature_var]], 10)
    })
    
    # Preview of processed data
    output$processed_data_preview <- renderPrint({
      req(imported_data$data())
      req(input$feature_to_analyze != "")
      req(input$feature_type %in% c("numeric", "categorical"))
      
      data <- imported_data$data()
      feature_var <- input$feature_to_analyze
      
      # Additional missing values
      additional_missings <- NULL
      if(input$additional_missing != "") {
        additional_missings <- unlist(strsplit(input$additional_missing, ",\\s*"))
      }
      
      # Process data
      processed <- process_feature(
        data[[feature_var]],
        type = input$feature_type,
        additional_missings = additional_missings,
        missing_as_category = input$missing_as_category,
        numeric_bins = if(input$feature_type == "numeric") input$numeric_bins else 10 # Bin count only for numeric features
      )
      
      # Show first 10 processed values
      head(processed, 10)
    })
    
    # Extended function for processing features
    process_feature <- function(feature_data, type, additional_missings = NULL, 
                                missing_as_category = TRUE, numeric_bins = 10, 
                                binning_method = "equal_width") {
      # Create copy of data
      processed <- feature_data
      
      # Convert additional missing values to NA
      if(!is.null(additional_missings) && length(additional_missings) > 0) {
        for(missing_val in additional_missings) {
          processed[processed == missing_val] <- NA
        }
      }
      
      if(type == "numeric" & all(is.numeric(processed[!is.na(processed)]))) {
        # Numeric data: Binning according to method
        if(binning_method == "equal_width") {
          # Equal width with nice boundaries
          data_range <- range(processed, na.rm = TRUE)
          breaks <- pretty(data_range, n = numeric_bins)
          # Ensure all data is within boundaries
          if(min(processed, na.rm = TRUE) < min(breaks)) breaks[1] <- floor(min(processed, na.rm = TRUE))
          if(max(processed, na.rm = TRUE) > max(breaks)) breaks[length(breaks)] <- ceiling(max(processed, na.rm = TRUE))
        } else {
          # Equal count (quantiles) with nice boundaries
          # Calculate quantiles
          probs <- seq(0, 1, length.out = numeric_bins + 1)
          raw_breaks <- quantile(processed, probs = probs, na.rm = TRUE, type = 7)
          
          # Create nice boundaries
          # First round the raw boundaries
          magnitude <- 10^floor(log10(max(abs(raw_breaks), na.rm = TRUE)) - 1)
          rounded_breaks <- round(raw_breaks / magnitude) * magnitude
          
          # Remove duplicates and ensure all values are covered
          breaks <- unique(rounded_breaks)
          if(min(processed, na.rm = TRUE) < min(breaks)) breaks <- c(floor(min(processed, na.rm = TRUE)), breaks[-1])
          if(max(processed, na.rm = TRUE) > max(breaks)) breaks[length(breaks)] <- ceiling(max(processed, na.rm = TRUE))
          
          # If too few bins due to rounding, use original quantiles with minimal rounding
          if(length(breaks) < 3) {
            breaks <- pretty(raw_breaks, n = numeric_bins)
          }
        }
        
        # Categorization with calculated boundaries
        processed <- cut(processed, breaks = breaks, include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
        
        # Missing values as separate category
        if(missing_as_category) {
          levels(processed) <- c(levels(processed), "Missing")
          processed[is.na(processed)] <- "Missing"
        }
      } else if(type == "categorical") {
        # Categorical data: convert to factor
        if(is.character(processed) || is.factor(processed)) {
          # For text/factor data: sort alphabetically
          levels_sorted <- mixedsort(unique(processed[!is.na(processed)]))
          processed <- factor(processed, levels = levels_sorted)
          
          # Missing values as separate category
          if(missing_as_category) {
            levels(processed) <- c(levels(processed), "Missing")
            processed[is.na(processed)] <- "Missing"
          }
        } else if(is.numeric(processed)) {
          # For numeric data treated as categorical: sort numerically
          levels_sorted <- sort(unique(processed[!is.na(processed)]))
          processed <- factor(processed, levels = as.character(levels_sorted))
          
          # Missing values as separate category
          if(missing_as_category) {
            levels(processed) <- c(levels(processed), "Missing")
            processed[is.na(processed)] <- "Missing"
          }
        }
      }
      
      return(processed)
      
    }
    
    # Apply changes (MODIFIED FOR MULTIPLE SELECTION)
    observeEvent(input$apply_changes, {
      req(imported_data$data())
      req(input$feature_to_analyze != "")
      req(input$feature_type %in% c("numeric", "categorical", "exclude"))
      
      # Save current state
      current_data <- if(is.null(processed_data())) {
        imported_data$data()
      } else {
        processed_data()
      }
      
      feature_var <- input$feature_to_analyze
      
      # Set output column name
      output_name <- input$output_column_name
      if(output_name == "") {
        if(input$feature_type == "numeric") {
          output_name <- paste0(feature_var, "_num_", 
                                ifelse(input$binning_method == "equal_width", 
                                       "eql_", "eqn_"), 
                                input$numeric_bins)
        } else {
          output_name <- paste0(feature_var, "_cat")
        }
      }
      
      # Add or update feature to appropriate list
      old_lists <- selected_features()
      new_lists <- old_lists # Keep all existing entries
      
      # Remove feature only from the exclude list, if it's there
      new_lists$exclude <- setdiff(old_lists$exclude, feature_var)
      
      # Add feature to appropriate list (without removing from other lists)
      feature_info <- list(
        name = feature_var, 
        output_name = output_name,
        type = input$feature_type,
        bins = if(input$feature_type == "numeric") input$numeric_bins else NA,
        binning_method = if(input$feature_type == "numeric") input$binning_method else NA,
        missing_as_category = input$missing_as_category,
        additional_missing = input$additional_missing
      )
      
      if(input$feature_type == "numeric") {
        new_lists$numeric <- c(new_lists$numeric, list(feature_info))
      } else if(input$feature_type == "categorical") {
        new_lists$categorical <- c(new_lists$categorical, list(feature_info))
      } else {
        new_lists$exclude <- c(new_lists$exclude, feature_var)
      }
      
      selected_features(new_lists)
      
      # Process feature and add to data, unless excluded
      if(input$feature_type != "exclude") {
        # Additional missing values
        additional_missings <- NULL
        if(input$additional_missing != "") {
          additional_missings <- unlist(strsplit(input$additional_missing, ",\\s*"))
        }
        
        # Process data
        processed <- process_feature(
          current_data[[feature_var]],
          type = input$feature_type,
          additional_missings = additional_missings,
          missing_as_category = input$missing_as_category,
          numeric_bins = if(input$feature_type == "numeric") input$numeric_bins else 10,
          binning_method = if(input$feature_type == "numeric") input$binning_method else "equal_width"
        )
        
        # Add processed data
        current_data[[output_name]] <- processed
        processed_data(current_data)
      }
      
      # Feedback to user
      showNotification(
        paste("Feature", feature_var, "was successfully classified as",
              switch(input$feature_type,
                     numeric = "numeric",
                     categorical = "categorical",
                     exclude = "excluded"),
              "."),
        type = "message"
      )
      
      
    })
    
    # Reset feature selection
    observeEvent(input$reset_feature, {
      updateSelectInput(session, "feature_to_analyze", selected = "")
      updateRadioButtons(session, "feature_type", selected = "numeric")
      updateSliderInput(session, "numeric_bins", value = 10) # Reset slider
      updateTextInput(session, "additional_missing", value = "")
      updateCheckboxInput(session, "missing_as_category", value = TRUE)
      updateTextInput(session, "output_column_name", value = "")
    })
    
    # Feature summary
    output$selected_features_summary <- renderPrint({
      feature_lists <- selected_features()
      
      cat("Main Variables:\n")
      cat("- Target: ", if(input$target_var != "") input$target_var else "Not selected", "\n")
      cat("- Weight: ", if(input$weight_var != "") input$weight_var else "None", "\n")
      cat("- Offset: ", if(input$offset_var != "") input$offset_var else "None", "\n\n")
      
      cat("Numeric Features (", length(feature_lists$numeric), "):\n")
      if(length(feature_lists$numeric) > 0) {
        for(feature in feature_lists$numeric) {
          # Simplified output without additional information
          cat("- ", feature$name, " (", feature$output_name, ")\n", sep="")
        }
      } else {
        cat("None selected\n")
      }
      
      cat("\nCategorical Features (", length(feature_lists$categorical), "):\n")
      if(length(feature_lists$categorical) > 0) {
        for(feature in feature_lists$categorical) {
          # Simplified output without additional information
          cat("- ", feature$name, " (", feature$output_name, ")\n", sep="")
        }
      } else {
        cat("None selected\n")
      }
      
      cat("\nExcluded Features (", length(feature_lists$exclude), "):\n")
      if(length(feature_lists$exclude) > 0) {
        cat(paste("- ", feature_lists$exclude), sep = "\n")
      } else {
        cat("None excluded\n")
      }
    })
    
    # Return the reactive values of the module
    return(list(
      processed_data = reactive({
        req(processed_data())
        
        # Columns to keep
        keep_columns <- c()
        
        # Add main variables
        if(input$target_var != "") keep_columns <- c(keep_columns, input$target_var)
        if(input$weight_var != "") keep_columns <- c(keep_columns, input$weight_var)
        if(input$offset_var != "") keep_columns <- c(keep_columns, input$offset_var)
        
        # Add output names of selected features
        feature_lists <- selected_features()
        
        # Numeric features
        if(length(feature_lists$numeric) > 0) {
          numeric_outputs <- sapply(feature_lists$numeric, function(x) x$output_name)
          keep_columns <- c(keep_columns, numeric_outputs)
        }
        
        # Categorical features
        if(length(feature_lists$categorical) > 0) {
          categorical_outputs <- sapply(feature_lists$categorical, function(x) x$output_name)
          keep_columns <- c(keep_columns, categorical_outputs)
        }
        
        # Return only the selected columns
        processed_data()[, keep_columns, drop = FALSE]
      }),
      selected_features = selected_features,
      target_var = reactive(input$target_var),
      weight_var = reactive(input$weight_var),
      offset_var = reactive(input$offset_var)
    ))
  })
}