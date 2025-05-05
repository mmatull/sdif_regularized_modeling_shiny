# UI Function - Focused on Visualization of a Feature
variableVisUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Variable Visualization",
           fluidPage(
             
             fluidRow(
               # Left Column - Control Elements
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
                      
                      # Feature Selection and Type Display
                      wellPanel(
                        h4("Select Feature"),
                        selectInput(ns("feature_to_analyze"), "Select feature to view:",
                                    choices = c("Please select" = "")),
                        # Info about automatically detected type of selected feature
                        tags$div(style="margin-bottom: 10px;",
                                 tags$strong("Detected Type: "),
                                 textOutput(ns("detected_feature_type"), inline = TRUE)
                        )
                        # No buttons, no status display anymore
                      ),
                      # Current Feature Overview (expanded)
                      wellPanel(
                        h4("Current Model Selection"),
                        uiOutput(ns("selected_features_summary_ui"))
                      )
               ),
               
               # Right Column - Visualizations
               column(9,
                      # Show visualization when a feature is selected
                      conditionalPanel(
                        condition = paste0("input['", ns("feature_to_analyze"), "'] !== ''"), # Simplified condition
                        fluidRow(
                          column(12,
                                 h4("Visualizations for ",
                                    tags$span(style="font-weight:bold;", textOutput(ns("current_feature_name"), inline = TRUE))),
                                 hr()
                          )
                        ),
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   h5("Distribution and Target Means"),
                                   plotOutput(ns("feature_histogram"), height = "250px")
                                 )
                          )
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h5("Scatterplot to Target"),
                                   plotOutput(ns("feature_scatter"), height = "250px") # Implementation needed
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h5("Top Correlations"),
                                   plotOutput(ns("feature_correlations"), height = "250px") # Implementation needed
                                 )
                          )
                        )
                      )
               )
             )
           )
  )
}


# Server Function - Focused on Visualization
variableVisServer <- function(id, imported_data, target_var, weight_var, offset_var, selected_features_input = NULL) { # No selected_features_input anymore
  moduleServer(id, function(input, output, session) {
    
    selected_features <- if(!is.null(selected_features_input) && inherits(selected_features_input, "reactiveVal")) { # <- CORRECTED
      selected_features_input # Directly use the reactiveVal
    } else if (!is.null(selected_features_input) && is.list(selected_features_input)){
      reactiveVal(selected_features_input) # Initialize from list
    } else {
      reactiveVal(list(
        numeric = list(),      # List of lists: list(name="var1", output_name="var1")
        categorical = list(),  # List of lists: list(name="varA", output_name="varA")
        exclude = character(0) # Vector of names
      ))
    }
    
    # --- Display of main variables (unchanged) ---
    output$current_target_var <- renderText({ req(target_var()); target_var() })
    output$current_weight_var <- renderText({ if(weight_var() == "") "None" else weight_var() })
    output$current_offset_var <- renderText({ if(offset_var() == "") "None" else offset_var() })
    
    # --- Feature Selection Dropdown Update ---
    observe({
      req(imported_data$processed_data(), target_var())
      excluded_vars <- c(target_var())
      if(weight_var() != "") excluded_vars <- c(excluded_vars, weight_var())
      if(offset_var() != "") excluded_vars <- c(excluded_vars, offset_var())
      all_vars <- setdiff(names(imported_data$processed_data()), excluded_vars)
      updateSelectInput(session, "feature_to_analyze",
                        choices = c("Please select" = "", all_vars),
                        selected = input$feature_to_analyze)
    })
    
    # --- Type Detection and Display ---
    get_feature_type <- function(feature_name) {
      req(imported_data$processed_data())
      data_col <- imported_data$processed_data()[[feature_name]]
      if (is.numeric(data_col)) {
        return("Numeric")
      } else if (is.factor(data_col) || is.character(data_col)) {
        return("Categorical")
      } else {
        return("Unknown")
      }
    }
    
    # Show automatically detected type of selected feature
    output$detected_feature_type <- renderText({
      req(input$feature_to_analyze != "")
      get_feature_type(input$feature_to_analyze)
    })
    
    # Current feature for visualizations (name)
    output$current_feature_name <- renderText({
      req(input$feature_to_analyze)
      input$feature_to_analyze
    })
    
    # Improved feature summary display
    output$selected_features_summary_ui <- renderUI({
      feature_lists <- selected_features()
      
      # Helper to create list items
      render_list <- function(items, type) {
        if(length(items) > 0) {
          # If items are lists (numeric/categorical), extract 'name'
          if(is.list(items[[1]])) {
            item_names <- sapply(items, function(f) f$output_name)
          } else {
            item_names <- items # exclude is just a vector
          }
          tagList(
            tags$strong(paste0(type, " (", length(item_names), "):")),
            tags$ul(lapply(sort(item_names), tags$li))
          )
        } else {
          tags$p(tags$strong(paste0(type, ":")), " none")
        }
      }
      
      tagList(
        render_list(feature_lists$numeric, "Numeric"),
        render_list(feature_lists$categorical, "Categorical"),
        render_list(feature_lists$exclude, "Excluded")
      )
    })
    
    # --- Visualizations ---
    
    # Current feature for visualizations (name)
    output$current_feature_name <- renderText({
      req(input$feature_to_analyze)
      input$feature_to_analyze
    })
    
    # Feature Histogram/Bar Chart
    output$feature_histogram <- renderPlot({
      req(input$feature_to_analyze != "") # Only plot when feature is selected
      req(target_var())
      
      # Prepare data
      data <- imported_data$processed_data()
      feature_var <- input$feature_to_analyze
      target_var_name <- target_var()
      weight_var_name <- if(weight_var() != "") weight_var() else NULL
      
      # Determine feature type for plot logic
      feature_type_plot <- get_feature_type(feature_var)
      req(feature_type_plot %in% c("Numeric", "Categorical")) # Only plot if type is clear
      
      # Standardize weights
      weights <- if(!is.null(weight_var_name)) data[[weight_var_name]] else rep(1, nrow(data))
      if(!is.numeric(weights)) {
        warning("Weight variable is not numeric. Using weight 1.")
        weights <- rep(1, nrow(data))
      }
      weights[is.na(weights)] <- 1
      
      # Target Variable
      target_vals <- data[[target_var_name]]
      if(!is.numeric(target_vals)) {
        warning("Target variable is not numeric.")
        return(NULL)
      }
      
      # Filter data for plot (only valid values)
      valid_idx <- !is.na(data[[feature_var]]) & !is.na(target_vals) & !is.na(weights) & weights > 0
      plot_data <- data[valid_idx, c(feature_var, target_var_name), drop = FALSE]
      plot_data$weights <- weights[valid_idx]
      plot_data$target <- target_vals[valid_idx]
      
      # Check dependencies
      if (!require(ggplot2)) { stop("Package ggplot2 is required.") }
      if (!require(dplyr)) { stop("Package dplyr is required.") }
      
      # Plot based on type (logic as before, but without checking for 'exclude')
      if(feature_type_plot == "Numeric") {
        # Binning for numeric features
        breaks <- graphics::hist(plot_data[[feature_var]], plot = FALSE, breaks="Sturges")$breaks
        plot_data$bin <- cut(plot_data[[feature_var]], breaks = breaks, include.lowest = TRUE, right = FALSE)
        
        # Aggregate with dplyr
        agg_data <- dplyr::summarise(
          dplyr::group_by(plot_data, bin),
          total_weight = sum(weights, na.rm = TRUE),
          avg_target = stats::weighted.mean(target, w = weights, na.rm = TRUE),
          .groups = 'drop'
        )
        agg_data <- na.omit(agg_data)
        
        # Bin midpoints
        bin_mids <- sapply(levels(agg_data$bin), function(b) {
          vals <- as.numeric(unlist(strsplit(gsub("\\[|\\)|\\]", "", b), ",")))
          mean(vals, na.rm = TRUE)
        })
        agg_data$bin_mid <- bin_mids[as.character(agg_data$bin)]
        
        # Scaling factor
        max_weight <- max(agg_data$total_weight, 0, na.rm=TRUE)
        max_avg_target <- max(agg_data$avg_target, 0, na.rm=TRUE)
        scaling_factor <- if (max_avg_target > 0) max_weight / max_avg_target else 1
        
        # Dual-axis plot
        p <- ggplot2::ggplot(agg_data) +
          ggplot2::geom_col(ggplot2::aes(x = bin_mid, y = total_weight, fill = "Exposure"), alpha = 0.7) +
          ggplot2::geom_line(ggplot2::aes(x = bin_mid, y = avg_target * scaling_factor, color = "Avg Target"), linewidth = 1.2) +
          ggplot2::geom_point(ggplot2::aes(x = bin_mid, y = avg_target * scaling_factor), color = "red", size = 3) +
          ggplot2::scale_y_continuous(
            name = "Exposure (Sum of Weights)",
            sec.axis = ggplot2::sec_axis(~. / scaling_factor, name = "Average Target Value")
          ) +
          ggplot2::scale_fill_manual(values = c("Exposure" = "steelblue")) +
          ggplot2::scale_color_manual(values = c("Avg Target" = "red")) +
          ggplot2::labs(x = feature_var, y = "Exposure (Sum of Weights)",
                        title = paste("Distribution and Average Target Value of", feature_var)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank())
        return(p)
        
      } else { # Categorical
        # Aggregate per category
        agg_data <- dplyr::summarise(
          dplyr::group_by(plot_data, category = .data[[feature_var]]),
          total_weight = sum(weights, na.rm = TRUE),
          avg_target = stats::weighted.mean(target, w = weights, na.rm = TRUE),
          .groups = 'drop'
        )
        agg_data <- na.omit(agg_data)
        
        # Scaling factor
        max_weight <- max(agg_data$total_weight, 0, na.rm=TRUE)
        max_avg_target <- max(agg_data$avg_target, 0, na.rm=TRUE)
        scaling_factor <- if (max_avg_target > 0) max_weight / max_avg_target else 1
        
        # Dual-axis plot
        p <- ggplot2::ggplot(agg_data) +
          ggplot2::geom_col(ggplot2::aes(x = category, y = total_weight, fill = "Exposure"), alpha = 0.7) +
          ggplot2::geom_line(ggplot2::aes(x = category, y = avg_target * scaling_factor, group = 1, color = "Avg Target"), linewidth = 1.2) +
          ggplot2::geom_point(ggplot2::aes(x = category, y = avg_target * scaling_factor), color = "red", size = 3) +
          ggplot2::scale_y_continuous(
            name = "Exposure (Sum of Weights)",
            sec.axis = ggplot2::sec_axis(~. / scaling_factor, name = "Average Target Value")
          ) +
          ggplot2::scale_fill_manual(values = c("Exposure" = "steelblue")) +
          ggplot2::scale_color_manual(values = c("Avg Target" = "red")) +
          ggplot2::labs(x = feature_var, y = "Exposure (Sum of Weights)",
                        title = paste("Distribution and Average Target Value of", feature_var)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        return(p)
      }
    })
    
    # Feature-Scatter Plot
    output$feature_scatter <- renderPlot({
      req(input$feature_to_analyze != "") # Only plot when feature is selected
      req(target_var())
      req(imported_data$processed_data())
      
      # Prepare data
      data <- imported_data$processed_data()
      feature_var <- input$feature_to_analyze
      target_var_name <- target_var()
      weight_var_name <- if(weight_var() != "") weight_var() else NULL
      
      # Determine feature type for plot logic
      feature_type_plot <- get_feature_type(feature_var)
      req(feature_type_plot %in% c("Numeric", "Categorical")) # Only plot if type is clear
      
      # Only rows with target != 0 and max 1000 random points
      filtered_data <- data[data[[target_var_name]] != 0, ]
      if(nrow(filtered_data) > 1000) {
        set.seed(42)  # For reproducibility
        sample_rows <- sample(1:nrow(filtered_data), 1000)
        filtered_data <- filtered_data[sample_rows, ]
      }
      
      # Plot based on type
      if(feature_type_plot == "Numeric") {
        plot_data <- data.frame(
          x = filtered_data[[feature_var]],
          y = filtered_data[[target_var_name]],
          weight = if(!is.null(weight_var_name)) filtered_data[[weight_var_name]] else rep(1, nrow(filtered_data))
        )
        
        # Scatterplot with ggplot2
        ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, size = weight)) +
          ggplot2::geom_point(alpha = 0.7, color = "steelblue") +
          ggplot2::geom_smooth(method = "loess", se = FALSE, color = "tomato") +
          ggplot2::labs(x = feature_var, y = target_var_name, size = "Weight",
                        title = paste("Scatterplot:", feature_var, "vs", target_var_name)) +
          ggplot2::theme_minimal()
      } else {
        # Categorical - Boxplot
        plot_data <- data.frame(
          x = as.factor(filtered_data[[feature_var]]),
          y = filtered_data[[target_var_name]],
          weight = if(!is.null(weight_var_name)) filtered_data[[weight_var_name]] else rep(1, nrow(filtered_data))
        )
        #browser()
        
        # Boxplot with ggplot2
        ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, weight = weight)) +
          ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
          ggplot2::geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue", height = 0.1 * diff(range(plot_data$y, na.rm = TRUE))) +
          ggplot2::labs(x = feature_var, y = target_var_name,
                        title = paste("Boxplot:", feature_var, "vs", target_var_name)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }
    })
    output$feature_correlations <- renderPlot({
      req(input$feature_to_analyze != "") # Only plot when feature is selected
      req(imported_data$processed_data())
      
      # Prepare data
      data <- imported_data$processed_data()
      feature_var <- input$feature_to_analyze
      target_var_name <- target_var()
      weight_var_name <- if(weight_var() != "") weight_var() else NULL
      
      # Exclude target variable, weight and offset
      excluded_vars <- c(target_var_name)
      if(!is.null(weight_var_name)) excluded_vars <- c(excluded_vars, weight_var_name)
      if(offset_var() != "") excluded_vars <- c(excluded_vars, offset_var())
      
      # Determine feature type for plot logic
      feature_type_plot <- get_feature_type(feature_var)
      req(feature_type_plot %in% c("Numeric", "Categorical")) # Only plot if type is clear
      
      # Function to calculate Cramer's V
      cramers_v <- function(x, y) {
        tbl <- table(x, y)
        tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0]
        n <- sum(tbl)
        chi_sq <- chisq.test(tbl, correct = FALSE)$statistic
        k <- min(nrow(tbl), ncol(tbl))
        sqrt(chi_sq / (n * (k - 1)))
      }
      
      # Function to calculate Eta-Squared
      eta_squared <- function(catVar, numVar) {
        # Perform ANOVA
        fit <- aov(numVar ~ catVar)
        # Calculate Eta-Squared
        summary_stats <- summary(fit)[[1]]
        eta_sq <- summary_stats["catVar", "Sum Sq"] / sum(summary_stats[, "Sum Sq"])
        return(eta_sq)
      }
      
      # Create variable lists (without excluded variables)
      all_vars <- setdiff(names(data), c(feature_var, excluded_vars))
      numeric_vars <- names(which(sapply(data, is.numeric)))
      categorical_vars <- setdiff(names(data), numeric_vars)
      
      # Initialize correlation values vector
      cor_values <- numeric()
      
      # Calculate correlation based on variable type
      if(feature_type_plot == "Numeric") {
        # The feature is numeric
        for(var in all_vars) {
          if(var %in% numeric_vars) {
            # Numeric vs. Numeric: Pearson correlation
            cor_values[var] <- tryCatch({
              cor(data[[feature_var]], data[[var]], 
                  use = "pairwise.complete.obs", method = "pearson")
            }, error = function(e) {
              NA
            })
          } else {
            # Numeric vs. Categorical: Eta-Squared
            cor_values[var] <- tryCatch({
              eta_squared(as.factor(data[[var]]), data[[feature_var]])
            }, error = function(e) {
              NA
            })
          }
        }
      } else {
        # The feature is categorical
        for(var in all_vars) {
          if(var %in% categorical_vars) {
            # Categorical vs. Categorical: Cramer's V
            cor_values[var] <- tryCatch({
              cramers_v(as.factor(data[[feature_var]]), as.factor(data[[var]]))
            }, error = function(e) {
              NA
            })
          } else {
            # Categorical vs. Numeric: Eta-Squared
            cor_values[var] <- tryCatch({
              eta_squared(as.factor(data[[feature_var]]), data[[var]])
            }, error = function(e) {
              NA
            })  
          }
        }
      }
      
      # Remove NA values
      cor_values <- cor_values[!is.na(cor_values)]
      
      # Top 10 correlations (or fewer if not enough available)
      top_n <- min(10, length(cor_values))
      if(top_n == 0) {
        return(ggplot2::ggplot() + 
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No valid correlations found") +
                 ggplot2::theme_void())
      }
      
      top_cors <- sort(abs(cor_values), decreasing = TRUE)[1:top_n]
      top_vars <- names(top_cors)
      
      # Color scheme and label based on correlation type
      if(feature_type_plot == "Numeric") {
        cor_type <- sapply(top_vars, function(var) {
          if(var %in% numeric_vars) "Pearson" else "Eta-Squared"
        })
      } else {
        cor_type <- sapply(top_vars, function(var) {
          if(var %in% categorical_vars) "Cramer's V" else "Eta-Squared"
        })
      }
      
      # Bar plot of correlations
      cor_df <- data.frame(
        Variable = top_vars,
        Correlation = cor_values[top_vars],
        Type = cor_type
      )
      
      # Custom color scheme for correlation types
      type_colors <- c("Pearson" = "steelblue", "Eta-Squared" = "darkgreen", "Cramer's V" = "purple")
      
      ggplot2::ggplot(cor_df, ggplot2::aes(x = reorder(Variable, abs(Correlation)), 
                                           y = Correlation, 
                                           fill = Type)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = type_colors, 
                                   name = "Correlation Type") +
        ggplot2::labs(x = "Variable", 
                      y = "Correlation Strength", 
                      title = paste("Top Feature Correlations with", feature_var)) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "top")
    })
    
    # --- Return Value ---
    # The module no longer returns reactive values that describe the state of feature selection.
    # It only serves for display.
    return(NULL) # Or return(list())
    
  })
}