# global.R
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(RColorBrewer)
library(gtools)
library(bslib)

library(MASS)
library(Matrix)
library(dplyr)
library(purrr)
library(caret)
library(glmnet)
library(rlang)
library(tweedie)
library(plotly)
library(openxlsx)

options(shiny.maxRequestSize = 1000 * 1024^2) 

# Modul
source("modules/mod_data_import.R")
source("modules/mod_variable_selection.R")
source("modules/mod_plot.R")
source("modules/mod_model_pipeline.R")
source("modules/mod_model_pipeline_relax.R")
source("modules/mod_export.R")

# Core-
source("core/model_pipeline.R")
source("core/plot.R") 
source("core/prepare_data.R") 
source("core/util.R")

# dark <- bs_theme(version = 5, bootswatch = "darkly") %>%
#   bs_add_rules("
#     .nav-tabs {
#       --nav-tabs-active-bg: #343a40;
#       --nav-tabs-active-color: #fff;
#     }
#     
#     .nav-tabs .nav-link {
#       color: #adb5bd;
#       transition: all 0.2s ease;
#     }
#     
#     .nav-tabs .nav-link:hover {
#       color: #dee2e6;
#       border-color: #495057 #495057 transparent;
#     }
#     
#     .nav-tabs .nav-link.active {
#       color: var(--nav-tabs-active-color) !important;
#       background-color: var(--nav-tabs-active-bg) !important;
#       border-color: #495057 #495057 var(--nav-tabs-active-bg) !important;
#     }
#   ")
# 
# light <- bs_theme(version = 5, bootswatch = "flatly")
