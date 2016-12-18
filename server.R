source(file.path('..','core', 'R-views.R'), chdir = TRUE)
source('app_logic.R', chdir = TRUE)
source('shiny-view.R', chdir = TRUE)

library(shiny)
library(shinyRGL)
library(ggplot2)
library(psych)
library(magrittr)
library(dplyr)
library(reshape2)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 'max_mb_upload' MB.
options(shiny.maxRequestSize = max_mb_upload*1024^2)

server <- shinyServer(function(input, output){
  
  # core state/data
  source('reactives.R', local = TRUE)$value
  
  ##########################################################
  # Load/import stage
  source(file.path('components', 'load_save_comp.R'), local = TRUE)$value
  
  ##########################################################
  # Edit stage
  source(file.path('components','algorithms_comp.R'), local = TRUE)$value
  
  ##########################################################
  # Correct/select stage
  source(file.path('components','correction_stack_comp.R'), local = TRUE)$value
  source(file.path('components','cor_sel_sidebar_comp.R'), local = TRUE)$value
  
  ##########################################################
  # Visualization stage
  source(file.path('components','tools_choice_comp.R'), local = TRUE)$value
  source(file.path('components','time_serie_comp.R'), local = TRUE)$value
  source(file.path('components','profile_comp.R'), local = TRUE)$value
  source(file.path('components','versus_comp.R'), local = TRUE)$value
  source(file.path('components','categorical_comp.R'), local = TRUE)$value
  source(file.path('components','app_state_comp.R'), local = TRUE)$value
  
  init_app()
  
})