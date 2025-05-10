# modules/mod_model_pipeline.R

# UI function for the model pipeline module
modelPipelineUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Model Pipeline",
           fluidPage(
             
             fluidRow(
               # Left column - Control elements
               column(3,
                      # Display of current main variables (readonly)
                      wellPanel(
                        h4("Current Main Variables"),
                        tags$div(
                          tags$strong("Target Variable: "),
                          textOutput(ns("current_target_var"), inline = TRUE)
                        ),
                        tags$div(
                          tags$strong("Weight: "),
                          textOutput(ns("current_weight_var"), inline = TRUE)
                        ),
                        tags$div(
                          tags$strong("Offset: "),
                          textOutput(ns("current_offset_var"), inline = TRUE)
                        )
                      ),
                      
                      # Feature selection for the model
                      wellPanel(
                        h4("Features for the Model"),
                        uiOutput(ns("feature_selection_ui")),
                        hr(),
                        # Number of selected features
                        tags$div(
                          tags$strong("Number of selected features: "),
                          textOutput(ns("num_selected_features"), inline = TRUE)
                        )
                      ),
                      
                      # Model configuration
                      wellPanel(
                        h4("Model Configuration"),
                        # Distribution selection
                        selectInput(ns("distribution"), "Distribution:",
                                    choices = c("tweedie", "gaussian", "poisson", "gamma"),
                                    selected = "tweedie"),
                        
                        # Tweedie Power Parameter (only visible when tweedie is selected)
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'tweedie'", ns("distribution")),
                          sliderInput(ns("tweedie_power"), "Tweedie Power Parameter:",
                                      min = 1.1, max = 2.0, value = 1.5, step = 0.1)
                        ),
                        
                        # Sparse Matrix Option
                        checkboxInput(ns("sparse_matrix"), "Use Sparse Matrix", value = TRUE),
                        
                        # Test Set Size
                        sliderInput(ns("test_size"), "Test Set Size (%):",
                                    min = 10, max = 40, value = 20, step = 5),
                        
                        # Run Button
                        actionButton(ns("run_model"), "Run Model", 
                                     class = "btn-primary", 
                                     icon = icon("play"))
                      )
               ),
               
               # Right column - Results
               column(8,
                      # Results area
                      conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("model_run")),
                        tabsetPanel(
                          tabPanel("Model Visualization", 
                                   fluidRow(
                                     column(6, 
                                            plotlyOutput(ns("risk_factor_plotly"), height = "800px")
                                     ),
                                     column(6, 
                                            plotlyOutput(ns("deviance_plot"), height = "500px")
                                     )
                                   )
                          ),
                          # New tab for all risk factor plots
                          tabPanel("All Risk Factors",
                                   fluidRow(
                                     column(12,
                                            fluidRow(
                                              column(6,
                                                     sliderInput(ns("global_s_value"), "Select s:", 
                                                                 min = 1, max = 100, value = 1, step = 1)
                                              ),
                                              column(6,
                                                     sliderInput(ns("grid_columns"), "Number of columns:", 
                                                                 min = 1, max = 4, value = 2, step = 1)
                                              )
                                            ),
                                            # Dynamic UI output for all feature plots
                                            uiOutput(ns("all_risk_factor_plots"))
                                     )
                                   )
                          ),
                          tabPanel("Predictions",
                                   fluidRow(
                                     column(12,
                                            fluidRow(
                                              column(6,
                                                     sliderInput(ns("global_lambda_index"), "Select Lambda Index:", 
                                                                 min = 1, max = 100, value = 0, step = 1)
                                              ),
                                              column(6,
                                                     sliderInput(ns("grid_columns_2"), "Number of columns:", 
                                                                 min = 1, max = 4, value = 2, step = 1)
                                              )
                                            ),
                                            # Options for training or test data
                                            fluidRow(
                                              column(12,
                                                     radioButtons(ns("train_or_test"), "Dataset:",
                                                                  choices = c("Training" = "train", "Test" = "test"),
                                                                  selected = "train",
                                                                  inline = TRUE)
                                              )
                                            ),
                                            # Dynamic UI output for all feature prediction plots
                                            uiOutput(ns("all_feature_prediction_plots"))
                                     )
                                   )
                          )
                        )
                      ),
                      # Display when no results are available
                      conditionalPanel(
                        condition = sprintf("output['%s'] != true", ns("model_run")),
                        wellPanel(
                          style = "margin-top: 100px; padding: 40px; text-align: center;",
                          h4("Configure and run model parameters to see results.")
                        )
                      )
               )
             )
           )
  )
}

# Server function for the model pipeline module
modelPipelineServer <- function(id, imported_data, target_var, weight_var, offset_var, selected_features_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value for model results
    model_results <- reactiveVal(NULL)
    
    # --- Display of main variables ---
    output$current_target_var <- renderText({ req(target_var()); target_var() })
    output$current_weight_var <- renderText({ if(weight_var() == "") "None" else weight_var() })
    output$current_offset_var <- renderText({ if(offset_var() == "") "None" else offset_var() })
    
    # --- Feature selection UI ---
    output$feature_selection_ui <- renderUI({
      req(selected_features_input())
      feature_lists <- selected_features_input()
      
      # Extract features from the lists
      numeric_features <- if(length(feature_lists$numeric) > 0) {
        sapply(feature_lists$numeric, function(f) f$output_name)
      } else {
        character(0)
      }
      
      categorical_features <- if(length(feature_lists$categorical) > 0) {
        sapply(feature_lists$categorical, function(f) f$output_name)
      } else {
        character(0)
      }
      
      # All available features
      all_features <- c(numeric_features, categorical_features)
      
      # If no features are available
      if(length(all_features) == 0) {
        return(tags$div(
          tags$p("No features available. Please select features on the 'Variable Selection' tab.")
        ))
      }
      
      # Checkboxes for feature selection
      checkboxGroupInput(ns("selected_model_features"), 
                         "Select features for the model:",
                         choices = sort(all_features),
                         selected = all_features)
    })
    
    # Number of selected features
    output$num_selected_features <- renderText({
      req(input$selected_model_features)
      length(input$selected_model_features)
    })
    
    # Reactive function to run the model
    observeEvent(input$run_model, {
      req(imported_data(), target_var(), input$selected_model_features)
      
      # Check if enough features are selected
      if(length(input$selected_model_features) < 1) {
        showNotification("Please select at least one feature for the model.",
                         type = "warning")
        return()
      }
      
      # Progress indicator
      withProgress(message = "Running model...", {
        
        # Pipeline parameters
        features <- input$selected_model_features
        data <- imported_data()$processed_data
        distribution <- input$distribution
        tweedie_power <- if(input$distribution == "tweedie") input$tweedie_power else NULL
        test_size <- input$test_size / 100  # Convert from percentage to proportion
        
        if (offset_var() == "") {
          offset_col <- NULL
        } else {
          offset_col <- offset_var()
        }
        
        # Run the model pipeline
        tryCatch({
          model_results_data <- run_model_pipeline(
            features = features, 
            data = data, 
            distribution = distribution, 
            target_col = target_var(), 
            weight_col = weight_var(), 
            offset_col = offset_col, 
            sparse_matrix = input$sparse_matrix, 
            test_size = test_size, 
            tweedie_power = tweedie_power
          )
          
          # Save the results
          model_results(model_results_data)
          
          # Success message
          showNotification("Model successfully executed.", type = "message")
          
        }, error = function(e) {
          showNotification(
            paste("Error running the model pipeline:", e$message), 
            type = "error", 
            duration = 10
          )
          model_results(NULL)
        })
      })
    })
    
    # Indicator if model results are available
    output$model_run <- reactive({
      !is.null(model_results())
    })
    outputOptions(output, "model_run", suspendWhenHidden = FALSE)
    
    # Plot for Deviance
    output$deviance_plot <- renderPlotly({
      req(model_results())
      plot_deviance_train_test(model_results()$deviance_train, model_results()$deviance_test)
    })
    
    # Risk factor summary
    output$risk_factor_plotly <- renderPlotly({
      req(model_results())
      summary_df <- summarize_risk_factor_levels_per_s(model_results())
      
      # Determine maximum S values and feature count
      max_s <- ncol(summary_df)
      max_count <- max(summary_df, na.rm = TRUE)
      
      # Initial s value
      s_value <- 1
      
      # Create bar plot for the initial s value
      data_to_plot <- data.frame(
        Feature = rownames(summary_df),
        UniqueCounts = summary_df[, s_value]
      )
      
      # Sort feature names by count
      data_to_plot$Feature <- factor(data_to_plot$Feature, 
                                     levels = data_to_plot$Feature[order(data_to_plot$UniqueCounts)])
      
      # Create plotly plot
      p <- plot_ly(data_to_plot, 
                   x = ~UniqueCounts, 
                   y = ~Feature, 
                   type = 'bar', 
                   orientation = 'h',
                   marker = list(color = '#3498db')) %>%
        layout(
          #title = paste("Risk Factor Levels at s =", s_value),
          xaxis = list(
            title = "Level Counts", 
            range = c(0, max_count),
            dtick = 1  # Only whole numbers on X axis
          ),
          yaxis = list(title = ""), 
          #yaxis = list(title = "Feature"),
          margin = list(l = 100, r = 50, b = 150, t = 50, pad = 4)
        )
      
      # Add slider
      p <- p %>% layout(
        sliders = list(
          list(
            active = s_value - 1,  # 0-based index
            currentvalue = list(prefix = "s = "),
            steps = lapply(1:max_s, function(step_s) {
              # Data for this s value
              step_data <- data.frame(
                Feature = rownames(summary_df),
                UniqueCounts = summary_df[, step_s]
              )
              
              # Sort feature names by count
              step_data$Feature <- factor(step_data$Feature, 
                                          levels = step_data$Feature[order(step_data$UniqueCounts)])
              
              list(
                method = "update",
                args = list(
                  list(x = list(step_data$UniqueCounts))#,
                  #list(title = paste("Risk Factor Levels at s =", step_s))
                ),
                label = as.character(step_s)
              )
            })
          )
        )
      )
      
      p
    })
    
    # Observe the active model to set the maximum S values
    observe({
      req(model_results())
      model <- model_results()
      
      if (!is.null(model$base_level)) {
        s_values <- gsub("s", "", colnames(model$base_level)) 
        max_s <- max(as.numeric(s_values), na.rm = TRUE) + 1 
        
        # Update sliders with the maximum available S value
        updateSliderInput(session, "global_s_value", 
                          max = max_s,
                          value = min(input$global_s_value, max_s))
        
        updateSliderInput(session, "global_lambda_index", 
                          max = max_s,
                          value = min(input$global_lambda_index, max_s))
      }
    })
    
    
    
    
    output$all_risk_factor_plots <- renderUI({
      req(model_results(), input$global_s_value, input$grid_columns)
      
      # Check if there are risk factors
      if (length(model_results()$risk_factors) == 0) {
        return(tags$div(
          tags$p("No risk factors found.")
        ))
      }
      
      # List of all features with risk factors
      features_with_risk_factors <- names(model_results()$risk_factors)
      
      # Number of columns
      num_columns <- input$grid_columns
      col_width <- 12 / num_columns
      
      # Calculate optimal height based on column count
      # More columns means smaller plots
      base_height <- 400  # Base height for a plot with one column
      adjusted_height <- base_height * (1 / sqrt(num_columns))  # Scaling factor
      
      # Adjust font size
      base_font_size <- 14  # Base size for a plot with one column
      adjusted_font_size <- max(8, base_font_size * (1 / sqrt(num_columns)))  # Min. 8px
      
      # Group features into rows
      rows <- list()
      current_row <- list()
      col_count <- 0
      
      for (feature in features_with_risk_factors) {
        plot_id <- paste0("risk_factor_plot_", gsub("[^a-zA-Z0-9]", "_", feature))
        
        # Create plot container
        plot_container <- column(
          width = col_width,
          div(
            style = paste0("border: 1px solid #ddd; border-radius: 4px; padding: 5px; margin-bottom: 10px; font-size: ", 
                           adjusted_font_size, "px;"),
            h5(style = paste0("margin: 0 0 5px 0; font-weight: bold; font-size: ", 
                              adjusted_font_size + 2, "px;"), 
               paste0("Feature: ", feature)),
            div(
              style = "width: 100%;",
              plotlyOutput(ns(plot_id), height = paste0(round(adjusted_height), "px"))
            )
          )
        )
        
        # Add plot to current row
        current_row[[length(current_row) + 1]] <- plot_container
        col_count <- col_count + 1
        
        # If the row is full or it's the last feature, add the row to the list
        if (col_count == num_columns || feature == features_with_risk_factors[length(features_with_risk_factors)]) {
          rows[[length(rows) + 1]] <- fluidRow(do.call(tagList, current_row))
          current_row <- list()
          col_count <- 0
        }
      }
      
      # Combine all rows
      do.call(tagList, rows)
    })
    

    # Create all risk factor plots
    observe({
      req(model_results())
      
      # Create a plot for each feature
      for (feature in names(model_results()$risk_factors)) {
        # Unique plot ID
        plot_id <- paste0("risk_factor_plot_", gsub("[^a-zA-Z0-9]", "_", feature))
        
        # Local environment for renderPlotly
        local({
          local_feature <- feature
          local_plot_id <- plot_id
          
          # Create and update plot
          output[[local_plot_id]] <- renderPlotly({
            req(model_results(), input$grid_columns)
            
            # Determine weight column
            weight_column <- if(weight_var() == "") NULL else weight_var()
            
            # Get risk factor matrix
            risk_factor_matrix <- model_results()$risk_factors[[local_feature]]$risk_factor_link_dummy_encoded
            risk_factor_cols <- colnames(risk_factor_matrix)
            
            # Ensure s_value is valid
            s_value <- min(max(1, isolate(input$global_s_value)), length(risk_factor_cols))
            
            # Number of columns for layout adjustments
            num_columns <- input$grid_columns
            
            p <- plot_all_risk_factors_for_feature_no_slider(
              pipeline_output = model_results(),
              exposure_df = imported_data()$processed_data,
              feature_name = local_feature,
              exposure_col = weight_column,
              s_value = s_value
            )
            
            # Adjust font size based on column count
            base_font_size <- 12  # Base size
            adjusted_font_size <- max(8, base_font_size * (1 / sqrt(num_columns)))
            
            # Adjust layout - less space for labels with more columns
            margin_l <- max(20, 50 * (1 / sqrt(num_columns)))
            margin_b <- max(20, 40 * (1 / sqrt(num_columns)))
            
            # Plot with optimized layout
            p <- p %>% 
              layout(
                autosize = TRUE,
                margin = list(l = margin_l, r = 10, b = margin_b, t = 10, pad = 2),
                font = list(size = adjusted_font_size),
                xaxis = list(tickfont = list(size = adjusted_font_size)),
                yaxis = list(tickfont = list(size = adjusted_font_size))
              ) %>%
              config(responsive = TRUE, displayModeBar = FALSE) %>%
              plotly::event_register("plotly_restyle")
            
            p
          })
        })
      }
    })
    
    # Separate observer for slider
    observe({
      req(model_results())
      
      for (feature in names(model_results()$risk_factors)) {
        # Unique plot ID
        plot_id <- paste0("risk_factor_plot_", gsub("[^a-zA-Z0-9]", "_", feature))
        
        # Local environment
        local({
          local_feature <- feature
          local_plot_id <- plot_id
          
          # Separater Observer für Slider-Änderungen
          observeEvent(input$global_s_value, {
            req(model_results())
            
            # Get risk factor matrix
            risk_factor_matrix <- model_results()$risk_factors[[local_feature]]$risk_factor_link_dummy_encoded
            categories <- as.character(rownames(risk_factor_matrix))
            risk_factor_cols <- colnames(risk_factor_matrix)
            
            # Current s value 
            current_s_value <- min(max(1, input$global_s_value), length(risk_factor_cols))
            
            # Prepare data as proper lists, not named vectors
            new_y_values <- as.numeric(risk_factor_matrix[, current_s_value])
            new_hover_texts <- lapply(seq_along(categories), function(i) {
              paste("Category:", categories[i], 
                    "<br>Risk Factor:", format(new_y_values[i], digits = 5, nsmall = 5))
            })
            
            plotlyProxy(ns(local_plot_id), session) %>%
              plotlyProxyInvoke(
                "restyle",
                list(
                  y = list(new_y_values),
                  text = list(new_hover_texts)
                ),
                list(1)
              )
          })
        })
      }
    })
    
    output$all_feature_prediction_plots <- renderUI({
      req(model_results(), input$global_lambda_index, input$grid_columns_2)
      
      # List of all features
      features <- names(model_results()$risk_factors)
      
      # Check if features are available
      if (length(features) == 0) {
        return(tags$div(
          tags$p("No features found for prediction.")
        ))
      }
      
      # Layout configuration
      num_columns <- input$grid_columns_2
      col_width <- 12 / num_columns
      
      # Adjust height and font size
      base_height <- 400
      adjusted_height <- base_height * (1 / sqrt(num_columns))
      base_font_size <- 14
      adjusted_font_size <- max(8, base_font_size * (1 / sqrt(num_columns)))
      
      # Prepare rows and columns
      rows <- list()
      current_row <- list()
      col_count <- 0
      
      # Create plots for each feature
      for (feature in features) {
        plot_id <- paste0("feature_prediction_plot_", gsub("[^a-zA-Z0-9]", "_", feature))
        
        # Create plot container
        plot_container <- column(
          width = col_width,
          div(
            style = paste0("border: 1px solid #ddd; border-radius: 4px; padding: 5px; margin-bottom: 10px; font-size: ", 
                           adjusted_font_size, "px;"),
            # h5(style = paste0("margin: 0 0 5px 0; font-weight: bold; font-size: ", 
            #                   adjusted_font_size + 2, "px;"), 
            #    paste0("Feature: ", feature)),
            div(
              style = "width: 100%;",
              plotlyOutput(ns(plot_id), height = paste0(round(adjusted_height), "px"))
            )
          )
        )
        
        # Add plot to current row
        current_row[[length(current_row) + 1]] <- plot_container
        col_count <- col_count + 1
        
        # Finish row if full or last feature
        if (col_count == num_columns || feature == features[length(features)]) {
          rows[[length(rows) + 1]] <- fluidRow(do.call(tagList, current_row))
          current_row <- list()
          col_count <- 0
        }
      }
      
      # Combine all rows
      do.call(tagList, rows)
    })
    
    # Plot creation for each feature
    observe({
      req(model_results(), input$train_or_test)
      
      # Create a plot for each feature
      for (feature in names(model_results()$risk_factors)) {
        plot_id <- paste0("feature_prediction_plot_", gsub("[^a-zA-Z0-9]", "_", feature))
        
        local({
          local_feature <- feature
          local_plot_id <- plot_id

          # Create plot
          output[[local_plot_id]] <- renderPlotly({
            req(model_results(), input$grid_columns_2, input$train_or_test)
            
            # Weight column and target column
            weight_column <- weight_var()
            target_column <- target_var()
            
            # Validate lambda indexes
            lambda_index <- min(max(0, isolate(input$global_lambda_index)), 
                                ncol(model_results()$preds_train) - 1)
            
            # Create plot
            p <- plot_feature_predictions(
              pipeline = model_results(),
              data = imported_data()$processed_data,
              feature_name = local_feature,
              target_col = target_column,
              weight_col = weight_column,
              lambda_index = lambda_index,
              train_or_test = input$train_or_test
            )
            
            # Adjust layout
            num_columns <- input$grid_columns_2
            base_font_size <- 12
            adjusted_font_size <- max(8, base_font_size * (1 / sqrt(num_columns)))
            margin_l <- max(20, 50 * (1 / sqrt(num_columns)))
            margin_b <- max(20, 40 * (1 / sqrt(num_columns)))
            
            # Optimize plot layout
            p <- p %>% 
              layout(
                title = NULL,  # Remove title, as it's already in the container
                autosize = TRUE,
                margin = list(l = margin_l, r = 10, b = margin_b, t = 10, pad = 2),
                font = list(size = adjusted_font_size),
                xaxis = list(tickfont = list(size = adjusted_font_size)),
                yaxis = list(tickfont = list(size = adjusted_font_size)),
                legend = list(
                  orientation = "h",
                  x = 0.5,
                  y = 1.05,
                  xanchor = "center",
                  font = list(size = adjusted_font_size)
                )
              ) %>%
              config(responsive = TRUE, displayModeBar = FALSE) %>%
              plotly::event_register("plotly_restyle")
            
            p
          })
        })
      }
    })
    
    # Separate observer for lambda slider updates
    observe({
      req(model_results())
      
      for (feature in names(model_results()$risk_factors)) {
        plot_id <- paste0("feature_prediction_plot_", gsub("[^a-zA-Z0-9]", "_", feature))
        
        local({
          local_feature <- feature
          local_plot_id <- plot_id
          
          # Separate observer for lambda slider changes
          observeEvent(input$global_lambda_index, {
            req(model_results(), input$train_or_test)
            
            # Weight column and target column
            weight_column <- weight_var()
            target_column <- target_var()
            
            # Validate lambda indexes
            lambda_index <- min(max(0, input$global_lambda_index), 
                                ncol(model_results()$preds_train) - 1)
            
            # Set train_or_test-specific variables based on the train_or_test parameter
            train_or_test <- input$train_or_test
            train_or_test <- tolower(train_or_test)  # Convert to lowercase to match function parameters
            index_field <- paste0(train_or_test, "_index")
            preds_field <- paste0("preds_", train_or_test)
            
            tryCatch({
              # Select appropriate data based on train_or_test parameter
              selected_data <- imported_data()$processed_data[model_results()$split[[index_field]],]
              
              # Extract feature values and ensure they're factors for proper grouping
              feature_values <- selected_data[[local_feature]]
              if (!is.factor(feature_values)) {
                feature_values <- factor(feature_values)
              }
              
              # Calculate predicted values from pipeline model (only the part we need to update)
              column_index <- lambda_index 
              pred_values <- selected_data %>%
                mutate(
                  prediction = model_results()[[preds_field]][,column_index]
                ) %>%
                group_by(feature_level = feature_values) %>%
                summarise(
                  pred_rate = sum(prediction * !!sym(weight_column)) / sum(!!sym(weight_column)),
                  .groups = "drop"
                )
              
              # Get the feature levels in the correct order
              if (is.factor(pred_values$feature_level)) {
                levels_ordered <- levels(pred_values$feature_level)
              } else {
                levels_ordered <- sort(unique(pred_values$feature_level))
              }
              
              # Make sure data is in the correct order to match the plot
              pred_values <- pred_values %>%
                mutate(feature_level = factor(feature_level, levels = levels_ordered)) %>%
                arrange(feature_level)
              
              # Create hover text in same format as original plot
              hover_text <- paste(
                "Level:", pred_values$feature_level, 
                "<br>Prediction:", format(pred_values$pred_rate, digits = 5, nsmall = 5)
              )
              
              # Update trace 3 (index 2) with the new prediction values
              plotlyProxy(ns(local_plot_id), session) %>%
                plotlyProxyInvoke(
                  "restyle",
                  list(
                    y = list(pred_values$pred_rate),
                    text = list(hover_text)
                  ),
                  list(2)  # Update 3rd trace (index 2)
                )
            }, error = function(e) {
              # Log error for debugging but don't break app
              message(paste("Error updating predictions for feature", local_feature, ":", e$message))
            })
          })
        })
      }
    })
    # Return values of the module
    return(list(
      model_results = model_results,
      selected_features = reactive(input$selected_model_features),
      distribution = reactive(input$distribution),
      tweedie_power = reactive(if(input$distribution == "tweedie") input$tweedie_power else NULL),
      test_size = reactive(input$test_size / 100)
    ))
  })
}