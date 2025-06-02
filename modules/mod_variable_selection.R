# UI function for the variable selection module with data preparation
variableSelectionUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Variable Selection & Preparation", # More descriptive title for the tab
           fluidPage(
             # Optional: A main title for the page
             # titlePanel("Variable Selection and Feature Engineering Tool"), 
             
             sidebarLayout(
               sidebarPanel(width = 3,
                            # --- Block 1: Define Main Variables ---
                            wellPanel(
                              h4(tags$strong("1. Define Main Variables")), # Numbering and emphasis for workflow
                              # helpText("Select the basic variables for your analysis here."), # Optional help text
                              selectInput(ns("target_var"), "Target Variable:",
                                          choices = c("Please select" = ""), selected = ""),
                              selectInput(ns("weight_var"), "Weight Variable (Optional):",
                                          choices = c("None" = "", "Please select" = ""), selected = ""), # "None" more explicit as the first option
                              selectInput(ns("offset_var"), "Offset Variable (Optional):",
                                          choices = c("None" = "", "Please select" = ""), selected = "")
                            ),
                            
                            # --- Block 2: Select Feature for Analysis and Set Type ---
                            conditionalPanel(
                              condition = paste0("input['", ns("target_var"), "'] !== ''"),
                              wellPanel(
                                h4(tags$strong("2. Select Feature for Analysis")),
                                selectInput(ns("feature_to_analyze"), "Feature to Analyze:",
                                            choices = c("Please select" = "")),
                                # Note: The feature type is updated server-side based on the selection.
                                # The UI initially shows "Numeric" until the server reports the correct type.
                                radioButtons(ns("feature_type"), "Feature Type:",
                                             choices = c("Numeric" = "numeric",
                                                         "Categorical" = "categorical"),
                                             selected = "numeric", # Default selection client-side
                                             # inline = TRUE # Optional for horizontal layout if space is tight
                                )
                              )
                            ),
                            
                            # --- Block 3: Actions for the selected feature ---
                            conditionalPanel(
                              condition = paste0("input['", ns("feature_to_analyze"), "'] !== ''"),
                              wellPanel(
                                # Often no extra heading needed for buttons
                                actionButton(ns("apply_changes"), "Apply Changes",
                                             icon = icon("check-circle"), # Icon for better visual guidance
                                             class = "btn-primary btn-block"),
                                actionButton(ns("reset_feature"), "Reset Feature Selection",
                                             icon = icon("undo"), # Icon
                                             class = "btn-warning btn-block")
                              )
                            )
               ), # End sidebarPanel
               
               mainPanel(width = 9,
                         fluidRow(
                           column(8, # Main area for configurations
                                  # --- Block 4: Missing Value Handling (appears when a feature is selected) ---
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== ''"),
                                    wellPanel(
                                      h4(tags$strong("A. Missing Value Handling")),
                                      fluidRow(
                                        column(4, 
                                               tags$b("Missing Value Summary:"), # Short, bold label
                                               verbatimTextOutput(ns("missing_summary"))),
                                        column(4, 
                                               textInput(ns("additional_missing"),
                                                         "Additional values to treat as Missing (comma-separated):", 
                                                         placeholder = "e.g., N/A, ?, 999")), # Placeholder as an example
                                        column(4, 
                                               checkboxInput(ns("missing_as_category"),
                                                             "Treat Missing Values as a Separate Category?", TRUE))
                                      )
                                    )
                                  ),
                                  
                                  # --- Block 5: Options for NUMERIC Features (with Tabs) ---
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== '' && input['", ns("feature_type"), "'] == 'numeric'"),
                                    wellPanel(
                                      h4(tags$strong("B. Options for Numeric Features")),
                                      tabsetPanel(id = ns("numeric_processing_mode"),
                                                  tabPanel("Standard Binning", value = "standard_numeric",
                                                           radioButtons(ns("binning_method"), "Binning Method:",
                                                                        choices = c("Equal Width" = "equal_width", # Explanatory text
                                                                                    "Equal Count (Quantile)" = "equal_count"),
                                                                        selected = "equal_width"),
                                                           sliderInput(ns("numeric_bins"), "Number of Bins:",
                                                                       min = 2, max = 50, value = 10, step = 1),
                                                           helpText("Note: 'Equal Count' may result in fewer bins than specified if there are many identical values.") # Help text
                                                  ),
                                                  tabPanel("Manual Binning", value = "manual_numeric",
                                                           # Use bslib::tooltip() to wrap the textInput
                                                           bslib::tooltip(
                                                             textInput(ns("manual_bins"), 
                                                                       "Manual Bin Boundaries (comma-separated, ascending):",
                                                                       placeholder = "-Inf, 0, 10, 20, 50, Inf"),
                                                             # The tooltip text comes after the element
                                                             "Use -Inf and Inf for open-ended intervals. Example: '-Inf, 0, 10, Inf' creates bins (-Inf,0], (0,10], (10,Inf).",
                                                             placement = "bottom" # Options: "top", "bottom", "left", "right"
                                                           )
                                                  )
                                      )
                                    )
                                  ),
                                  
                                  # --- Block 6: Options for CATEGORICAL Features (with Tabs) ---
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== '' && input['", ns("feature_type"), "'] == 'categorical'"),
                                    wellPanel(
                                      h4(tags$strong("B. Options for Categorical Features")), # Same hierarchy level as numeric options
                                      tabsetPanel(id = ns("categorical_processing_mode"),
                                                  tabPanel("Standard Grouping", value = "standard_categorical",
                                                           helpText("The feature will be treated as a standard factor. Original levels (after NA handling and sorting) will be used. Missing values are handled according to the setting above.")
                                                  ),
                                                  tabPanel("Manual Grouping", value = "manual_categorical",
                                                           bslib::tooltip(
                                                             textAreaInput(ns("manual_cat_groups"), "Manual Grouping Definition:", rows = 1, 
                                                                           placeholder = "A,B;C,NA;D"),
                                                             # The tooltip text comes after the element
                                                             "Define groups using semicolons (;). Within each group, separate levels with commas (,). Use 'NA' to explicitly assign missing values to a group. Levels containing commas or semicolons must be enclosed in \"quotes\".",
                                                             placement = "bottom" # Options: "top", "bottom", "left", "right"
                                                           ),
                                                           checkboxInput(ns("other_category"), "Group all unassigned levels into an 'Other' category?", TRUE)
                                                  )
                                      )
                                    )
                                  ),
                                  # --- Block 7: Data Preparation Preview and Output Column Name ---
                                  conditionalPanel(
                                    condition = paste0("input['", ns("feature_to_analyze"), "'] !== ''"),
                                    wellPanel(
                                      h4(tags$strong("C. Preview & Output Column Naming")),
                                      fluidRow(
                                        column(6,
                                               tags$b("Raw Data (Original Feature):"),
                                               div(style = "max-height: 200px; overflow-y: auto; padding: 5px; font-family: monospace;", 
                                                   verbatimTextOutput(ns("raw_data_preview"))
                                               )
                                        ),
                                        column(6,
                                               tags$b("Processed Data (Preview):"),
                                               div(style = "max-height: 200px; overflow-y: auto; padding: 5px; font-family: monospace;",
                                                   verbatimTextOutput(ns("processed_data_preview"))
                                               )
                                        )
                                      ),
                                      br(), 
                                      tags$b("Frequencies of the levels (number, %, (weighted) Avg.):"), 
                                      div(style = "max-height: 150px; overflow-y: auto; padding: 5px; font-family: monospace; margin-top: 5px; margin-bottom: 15px;", 
                                          verbatimTextOutput(ns("level_frequency_preview"))
                                      ),
                                      textInput(ns("output_column_name"), "Name of Generated Column:", 
                                                placeholder = "e.g., myFeature_binned") 
                                    )
                                  ),
 
                           ), # End column(8)
                           
                           column(4, # Sidebar in mainPanel for the summary
                                  wellPanel(
                                    style = "position: sticky; top: 20px;", # Makes the summary visible when scrolling
                                    h4(tags$strong("Selection Summary")),
                                    # helpText("Here you can see an overview of the already configured features."), # Optional
                                    div(style = "max-height: 500px; overflow-y: auto;", # Scrollbar for long summaries
                                        verbatimTextOutput(ns("selected_features_summary"))
                                    )
                                  )
                           ) # End column(4)
                         ) # End fluidRow
               ) # End mainPanel
             ) # End sidebarLayout
           ) # End fluidPage
  ) # End tabPanel
}

# Server function for the module
variableSelectionServer <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Get namespace for updateTabsetPanel
    
    selected_features <- reactiveVal(list(
      numeric = list(),  
      categorical = list()
      # exclude = character(0) # Removed exclude list
    ))
    
    processed_data <- reactiveVal(NULL)
    `%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && (is.logical(a) || nzchar(a))) a else b
    
    # Update main variable selectors
    observe({
      req(imported_data$data())
      var_choices <- c("Please select" = "", names(imported_data$data()))
      weight_offset_choices <- c("None" = "", names(imported_data$data()))
      
      current_target <- input$target_var
      current_weight <- input$weight_var
      current_offset <- input$offset_var
      
      updateSelectInput(session, "target_var", choices = var_choices, selected = if (is.null(current_target) || !current_target %in% var_choices) "" else current_target)
      updateSelectInput(session, "weight_var", choices = weight_offset_choices, selected = if (is.null(current_weight) || !current_weight %in% weight_offset_choices) "" else current_weight)
      updateSelectInput(session, "offset_var", choices = weight_offset_choices, selected = if (is.null(current_offset) || !current_offset %in% weight_offset_choices) "" else current_offset)
    })
    
    # Update feature_to_analyze selector
    observe({
      req(imported_data$data())
      
      excluded_vars <- c(input$target_var %||% character(0))
      if(!is.null(input$weight_var) && input$weight_var != "") excluded_vars <- c(excluded_vars, input$weight_var)
      if(!is.null(input$offset_var) && input$offset_var != "") excluded_vars <- c(excluded_vars, input$offset_var)
      
      all_vars <- setdiff(names(imported_data$data()), excluded_vars)
      current_feature_to_analyze <- input$feature_to_analyze
      
      updateSelectInput(session, "feature_to_analyze",
                        choices = c("Please select" = "", all_vars),  
                        selected = if (is.null(current_feature_to_analyze) || !current_feature_to_analyze %in% all_vars) "" else current_feature_to_analyze)
    })
    
    get_suggested_output_name <- function(feature_var, feature_type,  
                                          numeric_mode_val, manual_bins_val, binning_method_val, numeric_bins_val,
                                          categorical_mode_val, manual_cat_groups_val) {
      if (is.null(feature_var) || feature_var == "") return("") # Handle NULL or empty feature_var
      if (feature_type == "numeric") {
        if (numeric_mode_val == "manual_numeric" && nzchar(trimws(manual_bins_val %||% ""))) {
          paste0(feature_var, "_num_man")
        } else if (numeric_mode_val == "manual_numeric") {  
          paste0(feature_var, "_num_man")  
        } else {  
          paste0(feature_var, "_num_",  
                 ifelse(binning_method_val == "equal_width", "eqw_", "eqc_"), # Changed from eql_ and eqn_ for clarity
                 numeric_bins_val)
        }
      } else if (feature_type == "categorical") {
        if (categorical_mode_val == "manual_categorical" && nzchar(trimws(manual_cat_groups_val %||% ""))) {
          paste0(feature_var, "_cat_man")
        } else if (categorical_mode_val == "manual_categorical") {  
          paste0(feature_var, "_cat_man")
        }else {  
          paste0(feature_var, "_cat")
        }
      } else {  
        # This 'else' case should ideally not be reached if feature_type is always numeric or categorical
        paste0(feature_var, "_proc")  
      }
    }
    
    observeEvent(input$feature_to_analyze, {
      req(imported_data$data()) 
      feature_var <- input$feature_to_analyze
      
      if (is.null(feature_var) || feature_var == "") {
        updateRadioButtons(session, "feature_type", selected = "numeric") 
        return()
      }
      
      sf_lists <- selected_features()
      existing_feature_config <- NULL
      all_selected_configs <- c(sf_lists$numeric, sf_lists$categorical)
      if(length(all_selected_configs) > 0) { 
        idx <- vapply(all_selected_configs, function(f) !is.null(f$name) && f$name == feature_var, logical(1))
        if(any(idx)) existing_feature_config <- all_selected_configs[idx][[1]]
      }
      
      if (!is.null(existing_feature_config)) {  
        loaded_type <- existing_feature_config$type %||% "numeric"  
        updateRadioButtons(session, "feature_type", selected = loaded_type)
        updateTextInput(session, "additional_missing", value = existing_feature_config$additional_missing %||% "")
        updateCheckboxInput(session, "missing_as_category", value = existing_feature_config$missing_as_category %||% TRUE)
        updateTextInput(session, "output_column_name", value = existing_feature_config$output_name %||% "")
        
        if (loaded_type == "numeric") {
          num_mode <- existing_feature_config$numeric_processing_mode %||% "standard_numeric"
          updateTabsetPanel(session, ns("numeric_processing_mode"), selected = num_mode)
          if(num_mode == "manual_numeric") {
            updateTextInput(session, "manual_bins", value = existing_feature_config$manual_bin_definition %||% "")
          } else {  
            updateRadioButtons(session, "binning_method", selected = existing_feature_config$binning_method %||% "equal_width")
            updateSliderInput(session, "numeric_bins", value = existing_feature_config$bins %||% 10)
          }
        } else if (loaded_type == "categorical") {
          cat_mode <- existing_feature_config$categorical_processing_mode %||% "standard_categorical"
          updateTabsetPanel(session, ns("categorical_processing_mode"), selected = cat_mode)
          if(cat_mode == "manual_categorical"){
            updateTextAreaInput(session, "manual_cat_groups", value = existing_feature_config$manual_group_definition %||% "")
            updateCheckboxInput(session, "other_category", value = existing_feature_config$use_other_category %||% TRUE)
          }
        }
      } else {  
        feature_data_col <- imported_data$data()[[feature_var]]
        is_col_numeric <- is.numeric(feature_data_col)
        
        default_type <- if(is_col_numeric) "numeric" else "categorical"
        updateRadioButtons(session, "feature_type", selected = default_type)
        updateTextInput(session, "additional_missing", value = "")
        updateCheckboxInput(session, "missing_as_category", value = TRUE)
        updateTabsetPanel(session, ns("numeric_processing_mode"), selected = "standard_numeric")
        updateRadioButtons(session, "binning_method", selected = "equal_width")
        updateSliderInput(session, "numeric_bins", value = 10)
        updateTextInput(session, "manual_bins", value = "")
        updateTabsetPanel(session, ns("categorical_processing_mode"), selected = "standard_categorical")
        updateTextAreaInput(session, "manual_cat_groups", value = "")
        updateCheckboxInput(session, "other_category", value = TRUE)
        
        suggested_name <- get_suggested_output_name(feature_var, default_type,  
                                                    "standard_numeric", "", "equal_width", 10,    
                                                    "standard_categorical", "")  
        updateTextInput(session, "output_column_name", value = suggested_name)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE) 
    
    observe({
      req(imported_data$data()) 
      feature_var <- input$feature_to_analyze
      if(is.null(feature_var) || feature_var == "") {
        return()
      }
      
      current_numeric_mode <- input$numeric_processing_mode %||% "standard_numeric"
      current_categorical_mode <- input$categorical_processing_mode %||% "standard_categorical"
      
      suggested_name <- get_suggested_output_name(feature_var, input$feature_type,
                                                  current_numeric_mode, input$manual_bins,
                                                  input$binning_method, input$numeric_bins,
                                                  current_categorical_mode, input$manual_cat_groups)
      updateTextInput(session, "output_column_name", value = suggested_name)
    })
    
    # This observer was primarily for updating the exclude list when main vars change.
    # Since 'exclude' list is gone, this observer might not be strictly necessary
    # or could be simplified if other actions are needed when main vars change.
    # For now, commenting out the part that modifies 'exclude'.
    observeEvent(list(input$target_var, input$weight_var, input$offset_var), {
      req(imported_data$data())
      # current_selected <- selected_features()
      # used_vars <- c(input$target_var %||% character(0),
      #                input$weight_var%||% character(0),
      #                input$offset_var %||% character(0))
      # used_vars <- used_vars[used_vars != "" & !is.null(used_vars)]
      # 
      # current_selected$exclude <- setdiff(current_selected$exclude, used_vars) # Removed
      # selected_features(current_selected) # Removed
      # If other logic dependent on main var changes is needed, it can go here.
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$missing_summary <- renderPrint({
      req(imported_data$data())
      feature_var <- input$feature_to_analyze
      if (is.null(feature_var) || feature_var == "") {
        cat("Please select a feature to see missing value summary.")
        return()
      }
      data_col <- imported_data$data()[[feature_var]]
      na_initial_count <- sum(is.na(data_col))
      temp_col <- data_col
      additional_missings_vec <- NULL
      if (input$additional_missing != "") {
        additional_missings_vec <- trimws(unlist(strsplit(input$additional_missing, ",\\s*")))
        if(is.numeric(temp_col)) {
          num_additional_missings <- suppressWarnings(as.numeric(additional_missings_vec))
          temp_col[temp_col %in% num_additional_missings[!is.na(num_additional_missings)]] <- NA
        } else {  
          temp_col[as.character(temp_col) %in% additional_missings_vec] <- NA
        }
      }
      na_total_count <- sum(is.na(temp_col))
      total_rows <- length(data_col)
      na_percent <- round(na_total_count / total_rows * 100, 2)
      cat("Missing Values (after considering additional):\n")
      cat(na_total_count, "of", total_rows, "(", na_percent, "%)\n")
      if (!is.null(additional_missings_vec) && length(additional_missings_vec) > 0) {
        cat("Based on original NAs:", na_initial_count, "\n")
        cat("And user-defined:", paste(additional_missings_vec, collapse=", "), "\n")
      }
    })
    
    output$raw_data_preview <- renderPrint({
      req(imported_data$data())
      feature_var <- input$feature_to_analyze
      if (is.null(feature_var) || feature_var == "") {
        cat("Please select a feature to see raw data preview.")
        return()
      }
      head(imported_data$data()[[feature_var]], 10)
    })
    
    previewed_processed_column <- reactive({
      req(imported_data$data(), input$feature_to_analyze) 
      feature_var <- input$feature_to_analyze
      feature_type_val <- input$feature_type
      
      if (feature_var == "" || is.null(feature_type_val) || !feature_type_val %in% c("numeric", "categorical")) {
        return(NULL) 
      }
      
      additional_missings_vec <- NULL
      if (input$additional_missing != "") {
        additional_missings_vec <- trimws(unlist(strsplit(input$additional_missing, ",\\s*")))
      }
      
      current_processing_mode <- NA
      if (feature_type_val == "numeric") current_processing_mode <- input$numeric_processing_mode
      if (feature_type_val == "categorical") current_processing_mode <- input$categorical_processing_mode
      
      process_feature(
        feature_data = imported_data$data()[[feature_var]],
        type = feature_type_val,
        processing_mode = current_processing_mode,  
        additional_missings = additional_missings_vec,
        missing_as_category = input$missing_as_category,
        numeric_bins_standard = input$numeric_bins,
        binning_method_standard = input$binning_method,
        manual_bins_def_string = input$manual_bins,
        manual_cat_groups_def_string = input$manual_cat_groups,
        use_other_category_manual = input$other_category,
        original_feature_name = feature_var
      )
    })
    
    output$processed_data_preview <- renderPrint({
      processed_col <- previewed_processed_column()
      
      if(is.null(processed_col)){
        cat("Preview unavailable: Processing was blocked or resulted in an error. Check console for warnings.")
      } else {
        head(processed_col, 10)
      }
    })
    
    output$level_frequency_preview <- renderPrint({
      processed_col <- previewed_processed_column() # Nutze die neue reaktive Expression
      
      if (is.null(processed_col)) {
        cat("Frequencies not available.")
        return()
      }

      if (length(processed_col) == 0 || all(is.na(processed_col) & (!"Missing" %in% levels(factor(processed_col))))) {
        cat("No data available for frequency calculation.")
        return()
      }
      
      if (!is.factor(processed_col)) {
        processed_col_factor <- factor(processed_col)
      } else {
        processed_col_factor <- processed_col
      }
      
      counts <- table(processed_col_factor, useNA = "ifany") 
      
      if (sum(counts) == 0) { 
        cat("No valid levels for frequency calculation.")
        return()
      }
      
      percentages <- round(prop.table(counts) * 100, 2)
      
      if (length(percentages) == 0) {
        cat("Do not display frequencies.")
        return()
      }
      
      original_df <- imported_data$data()
      target_variable_name <- input$target_var
      weight_variable_name <- input$weight_var
      
      target_col_data <- NULL
      target_is_valid_and_numeric <- FALSE
      if (!is.null(target_variable_name) && nzchar(target_variable_name) && target_variable_name %in% names(original_df)) {
        target_col_data <- original_df[[target_variable_name]]
        if (is.numeric(target_col_data)) {
          target_is_valid_and_numeric <- TRUE
        } else {
          cat(paste0("Note: Target variable '", target_variable_name, "' is not numeric. Average is not calculated.\n"))
        }
      }
      
      weights_col_data <- NULL
      use_weights <- FALSE
      if (target_is_valid_and_numeric && !is.null(weight_variable_name) && nzchar(weight_variable_name) && weight_variable_name %in% names(original_df)) {
        weights_col_data <- original_df[[weight_variable_name]]
        if (is.numeric(weights_col_data)) {
          use_weights <- TRUE
        } else {
          cat(paste0("Note: Weighting variable '", weight_variable_name, "' is not numeric. Unweighted average is used.\n"))
          use_weights <- FALSE 
        }
      }
      
      output_lines <- character(length(percentages))
      level_names <- names(percentages)
      for (i in seq_along(percentages)) {
        
        is_na_level <- is.na(level_names[i]) 
        level_name_display <- if(is_na_level) "<NA>" else level_names[i]
        
        avg_target_str <- ""
        if (target_is_valid_and_numeric) {

          indices_in_level <- if (is_na_level) {
            which(is.na(processed_col)) 
          } else {
            which(as.character(processed_col) == level_name_display)
          }
          
          if (length(indices_in_level) > 0) {
            target_values_in_level <- target_col_data[indices_in_level]
            
            valid_target_mask <- !is.na(target_values_in_level)
            target_values_for_avg <- target_values_in_level[valid_target_mask]
            
            if (length(target_values_for_avg) > 0) {
              avg_target_val <- NA_real_
              if (use_weights) {
                weight_values_for_avg <- weights_col_data[indices_in_level][valid_target_mask] 
                
                valid_weights_mask <- !is.na(weight_values_for_avg) & weight_values_for_avg > 0
                
                if (sum(valid_weights_mask) > 0) {
                  target_subset_w <- target_values_for_avg[valid_weights_mask]
                  weights_subset_w <- weight_values_for_avg[valid_weights_mask]
                  
                  if (sum(weights_subset_w) > 0) { 
                    avg_target_val <- stats::weighted.mean(target_subset_w, weights_subset_w, na.rm = FALSE) 
                  } else { 
                    avg_target_val <- mean(target_subset_w, na.rm = TRUE) 
                  }
                } else { 
                  avg_target_val <- mean(target_values_for_avg, na.rm = TRUE) 
                }
              } else { 
                avg_target_val <- mean(target_values_for_avg, na.rm = TRUE)
              }
              
              if (!is.na(avg_target_val)) {
                avg_target_str <- paste0(" ",format(round(avg_target_val, 3), nsmall = 3))
              } else {
                avg_target_str <- " N/A"
              }
            } else { 
              avg_target_str <- " N/A (no data)"
            }
          } else { 
            avg_target_str <- " N/A (no obs)"
          }
        }
        output_lines[i] <- paste0("'", level_name_display, "': ", counts[i], " (", percentages[i], "%)", avg_target_str)
      }
      cat(paste(output_lines, collapse = "\n"))
    })
    
    parse_manual_cat_groups <- function(definition_string, original_feature_name) {
      if (is.null(definition_string) || nchar(trimws(definition_string)) == 0) {
        return(NULL)
      }
      group_definitions <- strsplit(definition_string, ";(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", perl = TRUE)[[1]]
      parsed_groups_list <- list()
      group_counter <- 1
      for (group_def_raw in group_definitions) {
        group_def <- trimws(group_def_raw)
        if (nchar(group_def) == 0) next
        level_definitions_raw <- strsplit(group_def, ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", perl = TRUE)[[1]]
        processed_levels <- c()  
        valid_group <- TRUE
        for (level_def_raw in level_definitions_raw) {
          level_def <- trimws(level_def_raw)
          if (nchar(level_def) == 0) { next }
          if (level_def == "NA") {
            processed_levels <- c(processed_levels, "NA_KEYWORD_FOR_PARSER")
          } else {
            unquoted_level <- level_def
            if (startsWith(level_def, "\"") && endsWith(level_def, "\"")) {
              unquoted_level <- substr(level_def, 2, nchar(level_def) - 1)
              unquoted_level <- gsub("\"\"", "\"", unquoted_level)  
            } else {
              if (grepl("[,;]", level_def)) {
                warning(paste0("Level '", level_def, "' in group '", group_def_raw, "' contains a comma or semicolon but is not enclosed in double quotes."))
              }
            }
            processed_levels <- c(processed_levels, unquoted_level)
          }
        }
        if (!valid_group || length(processed_levels) == 0) { next }
        group_name <- paste0(original_feature_name, "_g", group_counter)
        parsed_groups_list[[length(parsed_groups_list) + 1]] <- list(name = group_name, levels = processed_levels)
        group_counter <- group_counter + 1
      }
      if (length(parsed_groups_list) == 0) return(NULL)
      return(parsed_groups_list)
    }
    
    parse_manual_bins <- function(bins_string) {
      if (is.null(bins_string) || nchar(trimws(bins_string)) == 0) return(NULL)
      bins <- trimws(unlist(strsplit(bins_string, ",\\s*")))
      parsed_bins <- sapply(bins, function(b) {
        if (b == "-Inf") return(-Inf)
        if (b == "Inf") return(Inf)
        val <- suppressWarnings(as.numeric(b))
        return(val)
      }, USE.NAMES = FALSE)
      if (any(is.na(parsed_bins))) {
        warning("Invalid non-numeric values in manual bin boundaries.")
        return(NULL)
      }
      if (length(parsed_bins) < 2) {
        warning("Manual binning requires at least two boundary points.")
        return(NULL)
      }
      if (!all(diff(parsed_bins) > 0)) {
        warning("Manual bin boundaries must be unique and in strictly ascending order.")
        return(NULL)
      }
      return(parsed_bins)
    }
    
    process_feature <- function(feature_data, type, processing_mode,  
                                additional_missings = NULL,
                                missing_as_category = TRUE,  
                                numeric_bins_standard = 10,  
                                binning_method_standard = "equal_width",  
                                manual_bins_def_string = NULL,
                                manual_cat_groups_def_string = NULL,  
                                use_other_category_manual = FALSE,
                                original_feature_name = "feature") {
      
      processed_column <- feature_data
      if (is.character(processed_column) || is.factor(processed_column)) {
        if (!is.null(additional_missings) && "" %in% additional_missings) {
          processed_column[processed_column == ""] <- NA
          additional_missings <- additional_missings[additional_missings != ""]
        }
      }
      if (!is.null(additional_missings) && length(additional_missings) > 0) {
        if (is.numeric(processed_column)) {
          num_missing_vals <- suppressWarnings(as.numeric(additional_missings))
          num_missing_vals <- num_missing_vals[!is.na(num_missing_vals)]
          if(length(num_missing_vals) > 0) processed_column[processed_column %in% num_missing_vals] <- NA
        } else {  
          char_col <- as.character(processed_column)  
          char_col[char_col %in% additional_missings] <- NA
          if(is.factor(processed_column)) processed_column <- factor(char_col, levels=levels(processed_column))  
          else processed_column <- char_col
        }
      }
      is_missing_after_preprocessing <- is.na(processed_column)
      processed_column_char_for_cat <- as.character(processed_column)  
      processed_column_char_for_cat[is_missing_after_preprocessing] <- NA
      
      
      if (type == "numeric") {
        current_values_for_binning <- processed_column[!is_missing_after_preprocessing]
        if(length(current_values_for_binning) > 0 && !is.numeric(current_values_for_binning)){
          current_values_for_binning <- suppressWarnings(as.numeric(as.character(current_values_for_binning)))  
          if(all(is.na(current_values_for_binning))) {
            warning(paste0("Feature '", original_feature_name, "' (numeric) contains non-convertible data. Binning aborted."))
            return(NULL)  
          }
        }
        
        binned_factor <- NULL
        use_manual_num_binning_effective <- FALSE
        
        if (processing_mode == "manual_numeric") {
          if (!is.null(manual_bins_def_string) && nzchar(trimws(manual_bins_def_string))) {
            breaks <- parse_manual_bins(manual_bins_def_string)
            if (!is.null(breaks)) {
              use_manual_num_binning_effective <- TRUE
              if (length(current_values_for_binning) > 0) {
                binned_levels <- cut(current_values_for_binning, breaks = breaks, include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
                temp_full_col <- rep(NA_character_, length(processed_column))
                temp_full_col[!is_missing_after_preprocessing] <- as.character(binned_levels)
                binned_factor <- factor(temp_full_col, levels = levels(binned_levels), ordered = TRUE)
              } else {
                binned_factor <- factor(rep(NA, length(processed_column)), ordered = TRUE)
              }
            } else {
              warning("Manual bin definition was invalid. Falling back to automatic binning for numeric feature.")
            }
          } else {  
            warning("Manual numeric binning selected but no definition provided. Falling back to automatic binning.")
          }
        }
        
        if (processing_mode == "standard_numeric" || !use_manual_num_binning_effective) {
          if (length(current_values_for_binning) > 0 && sum(!is.na(current_values_for_binning)) > 0) {
            auto_breaks <- NULL
            if(binning_method_standard == "equal_width") {
              data_range <- range(current_values_for_binning, na.rm = TRUE)
              if(all(is.finite(data_range)) && diff(data_range) >=0){
                auto_breaks <- pretty(data_range, n = numeric_bins_standard)
                min_val <- min(current_values_for_binning, na.rm = TRUE); max_val <- max(current_values_for_binning, na.rm = TRUE)
                if(min_val < min(auto_breaks)) auto_breaks <- c(floor(min_val), auto_breaks)
                if(max_val > max(auto_breaks)) auto_breaks <- c(auto_breaks, ceiling(max_val))
                auto_breaks <- unique(sort(auto_breaks))
              }
            } else {  # equal_count
              probs <- seq(0, 1, length.out = numeric_bins_standard + 1)
              auto_breaks <- unique(quantile(current_values_for_binning, probs = probs, na.rm = TRUE, type = 7))
            }
            if (!is.null(auto_breaks) && length(auto_breaks) > 1) {
              binned_levels <- cut(current_values_for_binning, breaks = auto_breaks, include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
              temp_full_col <- rep(NA_character_, length(processed_column))
              temp_full_col[!is_missing_after_preprocessing] <- as.character(binned_levels)
              binned_factor <- factor(temp_full_col, levels = levels(binned_levels), ordered = TRUE)
            } else {
              warning(paste0("Could not create standard bins for '", original_feature_name,"'. Treating as unbinned factor."))
              binned_factor <- factor(processed_column, ordered = TRUE)  
            }
          } else {  
            binned_factor <- factor(rep(NA, length(processed_column)), ordered = TRUE)
          }
        }
        
        if (missing_as_category && any(is_missing_after_preprocessing)) {
          if (!is.factor(binned_factor) || is.null(binned_factor)) binned_factor <- factor(binned_factor, ordered = TRUE)  
          current_levels <- levels(binned_factor)
          missing_level_name <- "Missing"  
          if (!missing_level_name %in% current_levels) {
            levels(binned_factor) <- c(current_levels, missing_level_name)
          }
          binned_factor[is_missing_after_preprocessing] <- missing_level_name
        }
        return(binned_factor)
        
      } else if (type == "categorical") {
        final_grouped_factor <- NULL
        
        if (processing_mode == "manual_categorical") {
          if (!is.null(manual_cat_groups_def_string) && nzchar(trimws(manual_cat_groups_def_string))) {
            parsed_groups <- parse_manual_cat_groups(manual_cat_groups_def_string, original_feature_name)
            if (!is.null(parsed_groups) && length(parsed_groups) > 0) {
              temp_grouped_col <- rep(NA_character_, length(processed_column_char_for_cat))
              assigned_value <- rep(FALSE, length(processed_column_char_for_cat))
              defined_group_names_ordered <- sapply(parsed_groups, `[[`, "name")
              
              for (i in seq_along(parsed_groups)) {
                group_info <- parsed_groups[[i]]; group_name <- group_info$name; levels_in_group <- group_info$levels
                has_na_keyword <- "NA_KEYWORD_FOR_PARSER" %in% levels_in_group
                actual_levels_in_group <- levels_in_group[levels_in_group != "NA_KEYWORD_FOR_PARSER"]
                
                if (length(actual_levels_in_group) > 0) {
                  match_indices <- which(processed_column_char_for_cat %in% as.character(actual_levels_in_group) & !is_missing_after_preprocessing & !assigned_value)
                  temp_grouped_col[match_indices] <- group_name
                  assigned_value[match_indices] <- TRUE
                }
                if (has_na_keyword) {
                  na_indices <- which(is_missing_after_preprocessing & !assigned_value)
                  temp_grouped_col[na_indices] <- group_name
                  assigned_value[na_indices] <- TRUE
                }
              }
              unassigned_indices <- which(!assigned_value)
              if (length(unassigned_indices) > 0) {
                if (use_other_category_manual) {
                  other_category_name <- "Other"
                  temp_grouped_col[unassigned_indices] <- other_category_name
                  if (!other_category_name %in% defined_group_names_ordered) {
                    defined_group_names_ordered <- c(defined_group_names_ordered, other_category_name)
                  }
                } else {
                  original_values_unassigned <- processed_column_char_for_cat[unassigned_indices]
                  if(any(!is.na(original_values_unassigned)) || any(is.na(original_values_unassigned))){  
                    warning(paste0("Feature '", original_feature_name, "': Manual categorical grouping incomplete and 'Other' not used. Processing blocked."))
                    return(NULL)  
                  }
                }
              }
              final_grouped_factor <- factor(temp_grouped_col, levels = defined_group_names_ordered)
            } else {  
              warning("Manual categorical grouping definition was invalid. Falling back to standard treatment.")
              processing_mode <- "standard_categorical"  
            }
          } else {  
            warning("Manual categorical grouping selected but no definition provided. Falling back to standard treatment.")
            processing_mode <- "standard_categorical"  
          }
        }
        
        if (processing_mode == "standard_categorical") {  
          data_for_factor <- processed_column_char_for_cat[!is_missing_after_preprocessing]
          if (length(data_for_factor) > 0) {
            unique_levels <- unique(data_for_factor)  
            if (requireNamespace("gtools", quietly = TRUE)) {
              levels_sorted <- gtools::mixedsort(unique_levels)
            } else {
              levels_sorted <- sort(unique_levels)
            }
            final_grouped_factor <- factor(processed_column_char_for_cat, levels = levels_sorted)
          } else {  
            final_grouped_factor <- factor(processed_column_char_for_cat)  
          }
        }  
        
        na_handled_by_manual_group <- FALSE
        if (processing_mode == "manual_categorical" && !is.null(final_grouped_factor)) {
          if (any(is_missing_after_preprocessing & !is.na(final_grouped_factor))) {
            na_handled_by_manual_group <- TRUE
          }
        }
        
        if (missing_as_category && any(is_missing_after_preprocessing) && !na_handled_by_manual_group ) {
          if (!is.factor(final_grouped_factor) || is.null(final_grouped_factor)) final_grouped_factor <- factor(final_grouped_factor)
          current_levels <- levels(final_grouped_factor)
          missing_level_name <- "Missing"
          if (!missing_level_name %in% current_levels) {
            levels(final_grouped_factor) <- c(current_levels, missing_level_name)
          }
          final_grouped_factor[is.na(final_grouped_factor) & is_missing_after_preprocessing] <- missing_level_name
        }
        return(final_grouped_factor)
      }  
      return(processed_column)  
    }  
    
    observeEvent(input$apply_changes, {
      req(imported_data$data())
      feature_var <- input$feature_to_analyze
      if (is.null(feature_var) || feature_var == "") {
        showNotification("Please select a feature to apply changes.", type = "warning")
        return()
      }
      feature_type <- input$feature_type # Will be "numeric" or "categorical"
      
      output_name <- input$output_column_name
      if(is.null(output_name) || output_name == "") {  
        output_name <- get_suggested_output_name(feature_var, feature_type,
                                                 input$numeric_processing_mode, input$manual_bins,
                                                 input$binning_method, input$numeric_bins,
                                                 input$categorical_processing_mode, input$manual_cat_groups)
      }
      if(is.null(output_name) || output_name == "") { 
        showNotification("Could not determine output column name.", type = "error")
        return()
      }
      
      additional_missings_str <- input$additional_missing
      additional_missings_vec <- NULL
      if(nzchar(additional_missings_str)) {
        additional_missings_vec <- trimws(unlist(strsplit(additional_missings_str, ",\\s*")))
      }
      
      current_processing_mode_val <- NA
      if(feature_type == "numeric") current_processing_mode_val <- input$numeric_processing_mode
      if(feature_type == "categorical") current_processing_mode_val <- input$categorical_processing_mode
      
      processed_column_result <- process_feature(
        feature_data = imported_data$data()[[feature_var]],  
        type = feature_type,
        processing_mode = current_processing_mode_val,
        additional_missings = additional_missings_vec,
        missing_as_category = input$missing_as_category,
        numeric_bins_standard = input$numeric_bins,
        binning_method_standard = input$binning_method,
        manual_bins_def_string = input$manual_bins,
        manual_cat_groups_def_string = input$manual_cat_groups,
        use_other_category_manual = input$other_category,
        original_feature_name = feature_var
      )
      
      if (is.null(processed_column_result)) {
        showNotification(
          paste("Error processing feature", feature_var, ". Defective manual definition or unsuitable data. Check console for warnings."),
          type = "error", duration = 10
        )
      } else {
        sf_lists <- selected_features()
        # sf_lists$exclude <- setdiff(sf_lists$exclude, feature_var) # Removed
        sf_lists$numeric <- Filter(function(x) !is.null(x$name) && x$name != feature_var, sf_lists$numeric)
        sf_lists$categorical <- Filter(function(x) !is.null(x$name) && x$name != feature_var, sf_lists$categorical)
        
        feature_info <- list(
          name = feature_var,  
          output_name = output_name,
          type = feature_type,
          missing_as_category = input$missing_as_category,
          additional_missing = additional_missings_str
        )
        if (feature_type == "numeric") {
          feature_info$numeric_processing_mode = current_processing_mode_val
          if (current_processing_mode_val == "manual_numeric") {
            feature_info$manual_bin_definition = input$manual_bins
          } else {  
            feature_info$bins = input$numeric_bins
            feature_info$binning_method = input$binning_method
          }
        } else if (feature_type == "categorical") {
          feature_info$categorical_processing_mode = current_processing_mode_val
          if (current_processing_mode_val == "manual_categorical") {
            feature_info$manual_group_definition = input$manual_cat_groups
            feature_info$use_other_category = input$other_category
          }
        }
        
        if(feature_type == "numeric") {
          sf_lists$numeric[[length(sf_lists$numeric) + 1]] <- feature_info
        } else if(feature_type == "categorical") {
          sf_lists$categorical[[length(sf_lists$categorical) + 1]] <- feature_info
        }
        selected_features(sf_lists)
        
        current_processed_df <- processed_data()
        if(is.null(current_processed_df)){
          current_processed_df <- imported_data$data()  
        }
        current_processed_df[[output_name]] <- processed_column_result
        processed_data(current_processed_df)
        
        showNotification(
          paste("Feature", feature_var, "processed as", feature_type, "into column", output_name,"."),
          type = "message"
        )
      }
    })  
    
    observeEvent(input$reset_feature, {
      updateSelectInput(session, "feature_to_analyze", selected = "")
      updateRadioButtons(session, "feature_type", selected = "numeric")  
      updateTabsetPanel(session, ns("numeric_processing_mode"), selected = "standard_numeric")  
      updateRadioButtons(session, "binning_method", selected = "equal_width")
      updateSliderInput(session, "numeric_bins", value = 10)
      updateTextInput(session, "manual_bins", value = "")
      updateTabsetPanel(session, ns("categorical_processing_mode"), selected = "standard_categorical")  
      updateTextAreaInput(session, "manual_cat_groups", value = "")
      updateCheckboxInput(session, "other_category", value = TRUE)
      updateTextInput(session, "additional_missing", value = "")
      updateCheckboxInput(session, "missing_as_category", value = TRUE)
      updateTextInput(session, "output_column_name", value = "")
    })
    
    output$selected_features_summary <- renderPrint({
      feature_lists <- selected_features()
      cat("Main Variables:\n")
      cat("- Target: ", input$target_var %||% "Not selected", "\n")
      cat("- Weight: ", input$weight_var %||% "None", "\n")
      cat("- Offset: ", input$offset_var %||% "None", "\n\n")
      
      cat("Numeric Features (", length(feature_lists$numeric), "):\n")
      if(length(feature_lists$numeric) > 0) {
        for(f in feature_lists$numeric) {
          mode_desc <- if(!is.null(f$numeric_processing_mode) && f$numeric_processing_mode == "manual_numeric") "(Manual Bins)" else paste0("(", f$bins %||% "N/A"," bins, ",f$binning_method %||% "N/A" ,")")
          cat("- ", f$name %||% "N/A", " -> ", f$output_name %||% "N/A", mode_desc, "\n", sep="")
        }
      } else { cat("None selected\n") }
      
      cat("\nCategorical Features (", length(feature_lists$categorical), "):\n")
      if(length(feature_lists$categorical) > 0) {
        for(f in feature_lists$categorical) {
          mode_desc <- if(!is.null(f$categorical_processing_mode) && f$categorical_processing_mode == "manual_categorical") "(Manual Groups)" else "(Standard Grouping)"
          cat("- ", f$name %||% "N/A", " -> ", f$output_name %||% "N/A", mode_desc, "\n", sep="")
        }
      } else { cat("None selected\n") }
      
    })
    
    return(list(
      processed_data = reactive({
        current_Pdata_val <- processed_data() 
        original_data_val <- imported_data$data() 
        
        base_df_for_subset <- NULL
        if (!is.null(current_Pdata_val)) {
          base_df_for_subset <- current_Pdata_val
        } else if (!is.null(original_data_val)) {
          base_df_for_subset <- original_data_val 
        } else {
          return(data.frame()) 
        }
        
        if (!is.data.frame(base_df_for_subset)) {
          warning("base_df_for_subset is not a data.frame in processed_data reactive.")
          return(data.frame())
        }
        
        keep_these_columns <- character(0) 
        
        main_vars_selected <- c(input$target_var %||% "", input$weight_var %||% "", input$offset_var %||% "")
        main_vars_selected <- main_vars_selected[nzchar(main_vars_selected)] 
        if(length(main_vars_selected) > 0) {
          keep_these_columns <- c(keep_these_columns, main_vars_selected[main_vars_selected %in% names(base_df_for_subset)])
        }
        
        sf_lists_val <- selected_features() 
        
        num_feature_outputs <- character(0)
        if (length(sf_lists_val$numeric) > 0) {
          raw_num_outputs <- sapply(sf_lists_val$numeric, function(x) x$output_name %||% NA_character_)
          num_feature_outputs <- raw_num_outputs[!is.na(raw_num_outputs) & nzchar(raw_num_outputs)]
        }
        
        cat_feature_outputs <- character(0)
        if (length(sf_lists_val$categorical) > 0) {
          raw_cat_outputs <- sapply(sf_lists_val$categorical, function(x) x$output_name %||% NA_character_)
          cat_feature_outputs <- raw_cat_outputs[!is.na(raw_cat_outputs) & nzchar(raw_cat_outputs)]
        }
        
        all_feature_outputs <- c(num_feature_outputs, cat_feature_outputs)
        if(length(all_feature_outputs) > 0) {
          keep_these_columns <- c(keep_these_columns, all_feature_outputs[all_feature_outputs %in% names(base_df_for_subset)])
        }
        
        keep_these_columns <- unique(keep_these_columns)
        final_columns_to_keep <- intersect(keep_these_columns, names(base_df_for_subset))
        
        if (length(final_columns_to_keep) > 0) {
          return(base_df_for_subset[, final_columns_to_keep, drop = FALSE])
        } else {
          return(base_df_for_subset[, FALSE, drop = FALSE])  
        }
      }),
      selected_features = selected_features, 
      target_var = reactive(input$target_var),
      weight_var = reactive(input$weight_var),
      offset_var = reactive(input$offset_var)
    )) 
  }) 
}
