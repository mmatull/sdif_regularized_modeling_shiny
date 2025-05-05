# server.R

server <- function(input, output, session) {
  
  # Data Import Modul 
  imported_data <- dataImportServer("import")
  
  # Variable Selection Modul
  variable_selection <- variableSelectionServer("variables", imported_data)
  
  # Debug
  # observe({
  #   cat("Target Var:", variable_selection$target_var(), "\n")
  #   cat("Weight Var:", variable_selection$weight_var(), "\n")
  #   cat("Offset Var:", variable_selection$offset_var(), "\n")
  #   cat("sel Var:", unlist(variable_selection$selected_features()), "\n")
  # })
  
  vis_output <- variableVisServer("visualization", 
                                  imported_data = list(
                                    processed_data = variable_selection$processed_data
                                  ),
                                  target_var = variable_selection$target_var,
                                  weight_var = variable_selection$weight_var,
                                  offset_var = variable_selection$offset_var,
                                  selected_features_input = variable_selection$selected_features)
  
  # Neues Modellpipeline-Modul initialisieren
  model_pipeline_output <- modelPipelineServer("model_pipeline",
                                               imported_data = reactive({
                                                 list(
                                                   processed_data = variable_selection$processed_data()
                                                 )
                                               }),
                                               target_var = variable_selection$target_var,
                                               weight_var = variable_selection$weight_var,
                                               offset_var = variable_selection$offset_var,
                                               selected_features_input = variable_selection$selected_features)
  
  # Relaxed-Modellpipeline-Modul, verwendet das Standard-Modell
  relaxed_model_results <- modelPipelineRelaxServer(
    "relaxed_model",
    imported_data = reactive({
      list(
        processed_data = variable_selection$processed_data()
      )
    }),
    target_var = variable_selection$target_var,
    weight_var = variable_selection$weight_var,
    offset_var = variable_selection$offset_var,
    distribution = model_pipeline_output$distribution,
    selected_features_input = variable_selection$selected_features,
    standard_model = reactive({ model_pipeline_output$model_results() })
  )
  
  # Export-Modul initialisieren
  exportModuleServer("export", 
                     distribution = model_pipeline_output$distribution,
                     standard_model = model_pipeline_output,
                     relaxed_model = relaxed_model_results)
  

  # Diese Werte können später für die GLM-Modellierung verwendet werden:
  # - model_vars$target_var()
  # - model_vars$weight_var()
  # - model_vars$offset_var()
  # - model_vars$numeric_features()
  # - model_vars$categorical_features()
  # - model_vars$formula() - Fertige Formel für glm()
}