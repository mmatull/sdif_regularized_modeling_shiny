# ui.R

ui <- fluidPage(
  
  # Theme
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Dark Mode Toggle
  input_dark_mode(id = "mode"),
  
  titlePanel("RegularizedGLM Studio"),
  
  tabsetPanel(id = "main_tabs",
              dataImportUI("import"),
              variableSelectionUI("variables"),
              variableVisUI("visualization"),
              modelPipelineUI("model_pipeline"),  
              modelPipelineRelaxUI("relaxed_model"),
              exportModuleUI("export")
  )
)